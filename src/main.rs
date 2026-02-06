use petgraph::{graph::NodeIndex, Graph};
use std::{
    collections::HashMap,
    default,
    fs::{self, File},
    ops::Index,
    path::{self, PathBuf},
    sync::Arc,
    time::UNIX_EPOCH,
};
use syntax::{
    ast::{
        self, edit::AstNodeEdit, BlockExpr, Enum, Expr, FieldList, GenericParamList, HasAttrs,
        HasDocComments, HasGenericParams, HasModuleItem, HasName, HasVisibility, IfExpr, Item,
        LetStmt, Module, Pat, Path, PathSegment, Stmt, Struct, Type, TypeAlias, Union, Use,
        Visibility,
    },
    AstNode, Edition, SourceFile, SyntaxToken,
};

mod validate;
mod def_resolver;

#[derive(Debug)]
enum Field {
    String(String),
    Bool(bool),
    Number(usize),
    Null,
}

#[derive(Debug, Default)]
struct Node {
    labels: Vec<String>,
    fields: HashMap<String, Field>,
}

#[derive(Debug, Default)]
struct Edge {
    labels: Vec<String>,
    fields: HashMap<String, Field>,
}

type UnitedGraph = Graph<Node, Edge>;

struct State {
    files_roots: Vec<NodeIndex>,
    graph: UnitedGraph,
}

impl State {
    fn new() -> Self {
        Self {
            graph: UnitedGraph::new(),
            files_roots: Vec::new(),
        }
    }
}

fn gen_token(graph: &mut UnitedGraph, ty: &str, tok: Option<SyntaxToken>, parent: NodeIndex) {
    let mut fields = HashMap::new();
    fields.insert("type".to_string(), Field::String(ty.to_string()));

    match tok {
        Some(keyword) => {
            let start = keyword.text_range().start();
            fields.insert("position_start".to_string(), Field::Number(start.into()));
            let end = keyword.text_range().end();
            fields.insert("position_end".to_string(), Field::Number(end.into()));
        }
        None => {
            fields.insert("position_start".to_string(), Field::Null);
            fields.insert("position_end".to_string(), Field::Null);
        }
    }

    let node = graph.add_node(Node {
        labels: vec![],
        fields,
    });

    let edge = Edge {
        labels: vec!["CST".to_string()],
        fields: HashMap::new(),
    };
    graph.add_edge(parent, node, edge);
}

fn gen_attrs(
    graph: &mut UnitedGraph,
    attrs: syntax::ast::AstChildren<syntax::ast::Attr>,
    parent: NodeIndex,
) {
    for attr in attrs.into_iter() {
        let meta = attr.meta().unwrap();

        let attr_id = graph.add_node(Node {
            labels: vec!["Attribute".to_string()],
            fields: HashMap::from([("meta".to_string(), Field::String(meta.to_string()))]),
            ..Default::default()
        });

        let edge = Edge {
            labels: vec!["AST".to_string(), "HAS_ATTR".to_string()],
            fields: HashMap::new(),
        };

        graph.add_edge(parent, attr_id, edge);
    }
}

fn gen_docs(graph: &mut UnitedGraph, docs: syntax::ast::DocCommentIter, parent: NodeIndex) {
    for comment in docs {
        let comm_id = graph.add_node(Node {
            labels: vec!["Comment".to_string()],
            fields: HashMap::from([("text".to_string(), Field::String(comment.to_string()))]),
            ..Default::default()
        });

        let edge = Edge {
            labels: vec!["CST".to_string(), "HAS_COMMENT".to_string()],
            fields: HashMap::new(),
        };

        graph.add_edge(parent, comm_id, edge);
    }
}

fn gen_path_segment(graph: &mut UnitedGraph, path: PathSegment, parent: NodeIndex) {
    //TODO
}

///
/// PathSegment --[NEXT] --> PathSegment
///
///
fn gen_path(graph: &mut UnitedGraph, path: Path, parent: NodeIndex) {
    let mut stack = std::collections::VecDeque::new();
    stack.push_back(path.clone());
    let mut cpath = path;
    loop {
        if let Some(npath) = cpath.qualifier() {
            stack.push_front(npath.clone());
            cpath = npath;
        } else {
            break;
        }
    }

    let mut start = None;
    let mut current = None;

    for i in stack {
        let path_segment = graph.add_node(Node {
            labels: vec!["PathSegment".to_string()],
            fields: Default::default(),
        });

        if let Some(segment) = i.segment() {
            gen_path_segment(graph, segment, path_segment)
        }

        gen_token(graph, "Coloncolon", i.coloncolon_token(), path_segment);

        let edge = Edge {
            labels: vec!["AST".to_string(), "CST".to_string(), "NEXT".to_string()],
            fields: HashMap::new(),
        };

        if let Some(current) = current {
            graph.add_edge(path_segment, current, edge);
        }

        current = Some(path_segment);
        if start == None {
            start = Some(path_segment);
        }
    }

    let edge = Edge {
        labels: vec!["AST".to_string(), "CST".to_string(), "HAS_PATH".to_string()],
        fields: HashMap::new(),
    };

    graph.add_edge(start.unwrap(), parent, edge);
}

/// Rust reference: https://doc.rust-lang.org/reference/visibility-and-privacy.html
///
///
/// parent <-- [AST, HASH_VIS] -- { label AST, Visibility}
///                                 |              |   |
///                                 [CST]          |   [AST, CST]
///                                 l_paren_token  |   |
///                                 r_paren_token  |   pub_token
///                                 in_token       |
///                                                [AST, CST]
///                                                |
///                                                path
///                                 
fn gen_visibility(graph: &mut UnitedGraph, vis: Visibility, parent: NodeIndex) {
    let visibility = graph.add_node(Node {
        labels: vec!["AST".to_string(), "Visibility".to_string()],
        fields: Default::default(),
    });

    let vis_edge = Edge {
        labels: vec!["AST".to_string(), "HAS_VIS".to_string()],
        fields: HashMap::new(),
    };

    use syntax::ast::Visibility;
    let items: Vec<(&str, Box<dyn Fn(&Visibility) -> Option<SyntaxToken>>)> = vec![
        ("LParenthesis", Box::new(Visibility::l_paren_token)),
        ("RParenthesis", Box::new(Visibility::r_paren_token)),
        ("in", Box::new(Visibility::in_token)),
        ("pub", Box::new(Visibility::pub_token)),
    ];
    for (name, toke) in items {
        gen_token(graph, name, toke(&vis), visibility);
    }

    if let Some(path) = vis.path() {
        gen_path(graph, path, visibility);
    }

    graph.add_edge(visibility, parent, vis_edge);
}

fn gen_generic_param_list(graph: &mut UnitedGraph, gp: GenericParamList, parent: NodeIndex) {
    let gp_node = graph.add_node(Node {
        labels: vec!["AST".to_string(), "GenericList".to_string()],
        fields: Default::default(),
    });

    use syntax::ast::GenericParamList;
    let items: Vec<(&str, Box<dyn Fn(&GenericParamList) -> Option<SyntaxToken>>)> = vec![
        ("LAngle", Box::new(GenericParamList::l_angle_token)),
        ("RAngle", Box::new(GenericParamList::r_angle_token)),
    ];
    for (name, toke) in items {
        gen_token(graph, name, toke(&gp), gp_node);
    }

    let mut prev = gp_node;
    for item in gp.generic_params() {
        match item {
            ast::GenericParam::ConstParam(const_param) => todo!(),
            ast::GenericParam::LifetimeParam(lifetime_param) => todo!(),
            ast::GenericParam::TypeParam(type_param) => todo!(),
        }
    }
}

fn gen_field_list(graph: &mut UnitedGraph, flist: syntax::ast::FieldList, parent: NodeIndex) {
    let field_list_ast = graph.add_node(Node {
        labels: vec!["AST".to_string(), "FieldList".to_string()],
        fields: Default::default(),
    });

    match flist {
        FieldList::RecordFieldList(rl) => {
            use syntax::ast::RecordFieldList;
            let items: Vec<(&str, Box<dyn Fn(&RecordFieldList) -> Option<SyntaxToken>>)> = vec![
                ("LCurly", Box::new(RecordFieldList::l_curly_token)),
                ("RCurly", Box::new(RecordFieldList::r_curly_token)),
            ];
            for (name, toke) in items {
                gen_token(graph, name, toke(&rl), field_list_ast)
            }

            let mut prev = field_list_ast;
            for item in rl.fields() {
                let field_ast = graph.add_node(Node {
                    labels: vec!["AST".to_string(), "Field".to_string()],
                    fields: Default::default(),
                });

                if let Some(ident) = item.name() {
                    gen_name(graph, ident, field_ast);
                }

                gen_attrs(graph, item.attrs(), field_ast);
                gen_docs(graph, item.doc_comments(), field_ast);

                if let Some(vis) = item.visibility() {
                    gen_visibility(graph, vis, field_ast);
                }

                if let Some(ty) = item.ty() {
                    gen_type(graph, ty, field_ast);
                }

                gen_token(graph, "Colon", item.colon_token(), field_ast);

                let edge = Edge {
                    labels: vec!["AST".to_string(), "CST".to_string(), "NEXT".to_string()],
                    fields: HashMap::new(),
                };
                graph.add_edge(prev, field_ast, edge);
                prev = field_ast;
            }
        }
        FieldList::TupleFieldList(tl) => {
            todo!()
        }
    }

    let edge = Edge {
        labels: vec!["AST".to_string(), "CST".to_string()],
        fields: HashMap::new(),
    };
    graph.add_edge(parent, field_list_ast, edge);
}

fn gen_name(graph: &mut UnitedGraph, ident: ast::Name, parent: NodeIndex) {
    let mut fields = HashMap::new();

    if ident.ident_token().is_some() {
        fields.insert("type".to_string(), Field::String("Identifier".to_string()));
        fields.insert("content".to_string(), Field::String(ident.to_string()));
    } else {
        fields.insert("type".to_string(), Field::String(ident.to_string()));
    }

    match ident.ident_token() {
        Some(keyword) => {
            let start = keyword.text_range().start();
            fields.insert("position_start".to_string(), Field::Number(start.into()));
            let end = keyword.text_range().end();
            fields.insert("position_end".to_string(), Field::Number(end.into()));
        }
        None => match ident.self_token() {
            Some(self_kw) => {
                let start = self_kw.text_range().start();
                fields.insert("position_start".to_string(), Field::Number(start.into()));
                let end = self_kw.text_range().end();
                fields.insert("position_end".to_string(), Field::Number(end.into()));
            },
            None => {},
        }
    }

    let node = graph.add_node(Node {
        labels: vec![],
        fields,
    });

    let edge = Edge {
        labels: vec!["AST".to_string(), "CST".to_string()],
        fields: HashMap::new(),
    };
    graph.add_edge(parent, node, edge);
}

// TODO: !!!!
fn gen_type(graph: &mut UnitedGraph, ty: Type, parent: NodeIndex) {
    // match ty {
    //     Type::PathType(path_type) => todo!(),
    //     _ => todo!(),
    // }
}

// TODO: !!!!
fn gen_ret_type(graph: &mut UnitedGraph, ty: ast::RetType, parent: NodeIndex) {
    todo!()
}

fn gen_abi(graph: &mut UnitedGraph, ty: ast::Abi, parent: NodeIndex) {
    todo!()
}

fn gen_block(graph: &mut UnitedGraph, ty: ast::BlockExpr, parent: NodeIndex) {
    // let struct_ast = graph.add_node(Node {
    //     labels: vec!["AST".to_string(), "BlockExpression".to_string()],
    //     fields: Default::default(),
    // });

    // let mut q_body = String::new();
    
    // q_body.push_str(&format!("CREATE (block: BlockExpression)\n"));

    // for attr in expr.attrs() {
    //     q_body.push_str(&format!(
    //         "CREATE (block)<-[:HAS_ATTR]-(:Attr {{meta: \"{}\" }})\n",
    //         attr
    //     ));
    // }

    // if let Some(stms) = expr.stmt_list() {
    //     for stmt in stms.statements() {
    //         let id = match stmt {
    //             Stmt::ExprStmt(expr) => gen_expr(graph, expr.expr().unwrap()).await,
    //             Stmt::Item(item) => gen_item(graph, item).await,
    //             Stmt::LetStmt(let_stmt) => gen_let_stmt(graph, let_stmt).await,
    //         };
    //     }
    // }

    // return "TODO_SOME_ID".to_string();
}


fn gen_param_list(graph: &mut UnitedGraph, ty: ast::ParamList, parent: NodeIndex) {

}

/// Example:
///
///  {                       --------[CST, HAS_COMMENT] --> {
///   label: AST, Struct                                      label: "Comment",
///                                                           text: ...,
///  }                                                      }
///  | | |
///  | | |- [AST, CST, HAS_IDENT] --- { type: "Identifier", content: ..., position: ... }
///  | | |
///  | | |-[AST, HAS_GEN_PARAM] --  
///  | |
///  | ---[AST, HAS_ATTR]--> {
///  |                        label: Attribute,
///  |                        meta: ... ,
///  |                       }
///  |--------\
///  |         -----------
///  {                   |   
///   type: "Keyword"    { type: "LBrace", position: ... }
///   content: "struct"
///   position: ...,
///  }
///  
fn gen_struct(graph: &mut UnitedGraph, item: Struct) -> NodeIndex {
    let struct_ast = graph.add_node(Node {
        labels: vec!["AST".to_string(), "Struct".to_string()],
        fields: Default::default(),
    });

    gen_attrs(graph, item.attrs(), struct_ast);
    gen_docs(graph, item.doc_comments(), struct_ast);

    if let Some(l) = item.generic_param_list() {
        gen_generic_param_list(graph, l, struct_ast);
    }

    if let Some(name) = item.name() {
        gen_name(graph, name, struct_ast);
    }

    if let Some(vis) = item.visibility() {
        gen_visibility(graph, vis, struct_ast);
    }

    if let Some(flist) = item.field_list() {
        gen_field_list(graph, flist, struct_ast);
    }

    gen_token(graph, "Semicolon", item.semicolon_token(), struct_ast);

    gen_token(graph, "Struct", item.struct_token(), struct_ast);

    struct_ast
}

fn gen_use(graph: &mut UnitedGraph, item: Use) -> NodeIndex {
    todo!()
}

fn gen_union(graph: &mut UnitedGraph, item: Union) -> NodeIndex {
    todo!()
}

fn gen_type_alias(graph: &mut UnitedGraph, item: TypeAlias) -> NodeIndex {
    todo!()
}

fn gen_fn(graph: &mut UnitedGraph, item: ast::Fn) -> NodeIndex {
    let func_ast = graph.add_node(Node {
        labels: vec!["AST".to_string(), "Function".to_string()],
        fields: Default::default(),
    });

    gen_attrs(graph, item.attrs(), func_ast);
    gen_docs(graph, item.doc_comments(), func_ast);

    if let Some(l) = item.generic_param_list() {
        gen_generic_param_list(graph, l, func_ast);
    }

    if let Some(name) = item.name() {
        gen_name(graph, name, func_ast);
    }

    if let Some(vis) = item.visibility() {
        gen_visibility(graph, vis, func_ast);
    }

    if let Some(abi) = item.abi() {
        gen_abi(graph, abi, func_ast);
    }

    if let Some(block) = item.body() {
        gen_block(graph, block, func_ast);
    }
    
    if let Some(plist) = item.param_list() {
        gen_param_list(graph, plist, func_ast)
    } 
    
    if let Some(rtype) = item.ret_type() {
        gen_ret_type(graph, rtype, func_ast);
    }

    let items: Vec<(&str, Box<dyn Fn(&ast::Fn) -> Option<SyntaxToken>>)> = vec![
        ("Semicolon", Box::new(ast::Fn::semicolon_token)),
        ("Async", Box::new(ast::Fn::async_token)),
        ("Const", Box::new(ast::Fn::const_token)),
        ("Default", Box::new(ast::Fn::default_token)),
        ("Fn", Box::new(ast::Fn::fn_token)),
        ("Unsafe", Box::new(ast::Fn::fn_token)),
    ];
    
    for (name, toke) in items {
        gen_token(graph, name, toke(&item), func_ast)
    }
 
    func_ast
}

fn gen_item(graph: &mut UnitedGraph, item: Item) -> NodeIndex {
    match item {
        Item::Const(item) => todo!(),
        Item::Enum(item) => todo!(),
        Item::Module(module) => todo!(),
        Item::Use(item) => todo!(),
        Item::ExternBlock(extern_block) => todo!(),
        Item::ExternCrate(extern_crate) => todo!(),
        Item::Fn(func) => gen_fn(graph, func),
        Item::Impl(_) => todo!(),
        Item::MacroCall(macro_call) => todo!(),
        Item::MacroDef(macro_def) => todo!(),
        Item::MacroRules(macro_rules) => todo!(),
        Item::Static(_) => todo!(),
        Item::Struct(item) => gen_struct(graph, item),
        Item::Trait(_) => todo!(),
        Item::TraitAlias(trait_alias) => todo!(),
        Item::TypeAlias(type_alias) => gen_type_alias(graph, type_alias),
        Item::Union(item) => gen_union(graph, item),
    }
}

/*

fn parse_file(state: &mut State, path: &path::Path) -> anyhow::Result<()> {
    let str_path = path.to_str().unwrap().to_string();
    let metadata = fs::metadata(path)?;
    let unix_timestamp = metadata.modified()?.duration_since(UNIX_EPOCH)?.as_secs();

    if let Some(result) = state.files_roots.into_iter()
        .map(|item| state.graph[item])
        .find(|item| item.fields["file"] == str_path) {

        if result.fields["last_modified_time"] == unix_timestamp {
            return Ok(());
        }
    }

    let payload = fs::read_to_string(path)?;

    let parse_result = SourceFile::parse(&payload);
    let tree = parse_result.tree();

    let root = state.graph.add_node(Node {
        name: "SourceFile".to_string(),
        fields: HashMap::from([
            ("file".to_string(), Field::String(str_path)),
            (
                "last_modified_time".to_string(),
                Field::Number(unix_timestamp as i128),
            ),
        ]),
        ..Default::default()
    });

    for attr in tree.attrs() {
        let meta = attr.meta().unwrap();

        let attr_id = graph.add_node(Node {
            name: "Attribute".to_string(),
            fields: HashMap::from([("meta".to_string(), Field::String(meta.to_string()))]),
            ..Default::default()
        });

        graph.add_edge(root, attr_id, "HAS_ATTR".to_string());
    }

    for comment in tree.doc_comments() {
        let comm_id = graph.add_node(Node {
            name: "Comment".to_string(),
            fields: HashMap::from([("text".to_string(), Field::String(comment.to_string()))]),
            ..Default::default()
        });

        graph.add_edge(root, comm_id, "HAS_COMMENT".to_string());
    }

    for item in tree.items() {
        let item_id = gen_item(&graph, item);
        graph.add_edge(root, item_id, "HAS_ITEM".to_string());
    }

    Ok(())
}

fn gen_module(graph: &UnitedGraph, module: Module) -> NodeIndex {
    let name = module.name().unwrap().to_string();
    let mod_id = graph.add_node(Node {
        name: "Module".to_string(),
        fields: HashMap::from([("name".to_string(), Field::String(name))]),
        ..Default::default()
    });

    if let Some(visibility) = module.visibility() {
        let vis_id = gen_visibility(graph, visibility);
        graph.add_edge(mod_id, vis_id, "HAS_VISIBILITY".to_string());
    }

    for attr in module.attrs() {
        let meta = attr.meta().unwrap();

        let attr_id = graph.add_node(Node {
            name: "Attribute".to_string(),
            fields: HashMap::from([("meta".to_string(), Field::String(meta.to_string()))]),
            ..Default::default()
        });

        graph.add_edge(mod_id, attr_id, "HAS_ATTR".to_string());
    }

    for comment in module.doc_comments() {
        let comm_id = graph.add_node(Node {
            name: "Comment".to_string(),
            fields: HashMap::from([("text".to_string(), Field::String(comment.to_string()))]),
            ..Default::default()
        });

        graph.add_edge(mod_id, comm_id, "HAS_COMMENT".to_string());
    }

    if let Some(item_list) = module.item_list() {
        for attr in item_list.attrs() {
            let meta = attr.meta().unwrap();

            let attr_id = graph.add_node(Node {
                name: "Attribute".to_string(),
                fields: HashMap::from([("meta".to_string(), Field::String(meta.to_string()))]),
                ..Default::default()
            });

            graph.add_edge(mod_id, attr_id, "HAS_ATTR".to_string());
        }

        for item in item_list.items() {
            let item_id = gen_item(&graph, item);

            graph.add_edge(mod_id, item_id, "HAS_ITEM".to_string());
        }

        mod_id
    }
}

async fn gen_expr(graph: &UnitedGraph, item: Expr) -> String {
    return match item {
        syntax::ast::Expr::IfExpr(item) => gen_if_expr(graph, item).await,
        _ => todo!(),
    };
}

async fn gen_if_expr(graph: &UnitedGraph, item: IfExpr) -> String {
    let mut q_body = String::new();

    q_body.push_str(&format!("CREATE (if_expr:IfExpr) RETURN ID(if_expr)\n"));

    for attr in item.attrs() {
        let meta = attr.meta().unwrap();
        q_body.push_str(&format!(
            "CREATE (if_expr)<-[:HAS_ATTR]-(:Attr {{meta: \"{}\" }})\n",
            meta
        ));
    }

    let id = gen_expr(graph, item.condition().unwrap()).await;
    q_body.push_str(&format!("CREATE (if_expr)<-HAS_CONDITION-(TODO)\n"));
    if let Some(expr) = item.else_branch() {
        let node = match expr {
            syntax::ast::ElseBranch::Block(expr) => gen_block(graph, expr).await,
            syntax::ast::ElseBranch::IfExpr(expr) => gen_if_expr(graph, expr).await,
        };

        q_body.push_str(&format!("CREATE (if_expr)<-HAS_ELSE-(TODO)\n"));
    }

    graph.execute(query(&q_body)).await.unwrap();
    return "TODO_SOME_ID".to_string();
}

///
/// let_stmt <--[HAS_ATTR]-- attr
///   ^  ^
///   |  |--[HAS_PATTERN]-- pattern <--[HAS_TYPE]-- type
///   |                       ^
///   |                       |--[HAS_INITIALIZER] -- initializer
///   |----[HAS_ELSE]-- block
///
async fn gen_let_stmt(graph: &Graph, let_stmt: LetStmt) -> String {
    let mut q_body = String::new();
    //TODO: resolve pattern (pattern_piece)-POINT_TO->(some_declaration_field)

    q_body.push_str(&format!(
        "CREATE (let_stmt: LetStatement) RETURN ID(let_stmt)\n"
    ));
    for attr in let_stmt.attrs() {
        let meta = attr.meta().unwrap();
        q_body.push_str(&format!(
            "CREATE (let_stmt)<-[:HAS_ATTR]-(:Attr {{meta: \"{}\" }})\n",
            meta
        ));
    }
    let pat_id = gen_pattern(graph, let_stmt.pat().unwrap()).await;
    q_body.push_str(&format!("CREATE (let_stmt)<-HAS_PATTERN-(pat: TODO)\n"));

    if let Some(ty) = let_stmt.ty() {
        let ty_id = gen_type(graph, ty).await;
        q_body.push_str(&format!("CREATE (pat)<-HAS_TYPE-(pat: TODO)\n"));
    }

    let initializer = let_stmt.initializer().unwrap();
    let init_id = gen_expr(graph, initializer).await;
    q_body.push_str(&format!("CREATE (pat)<-HAS_INITIALIZER-(TODO)\n"));

    let else_expr = let_stmt.let_else().unwrap().block_expr().unwrap();
    let else_id = gen_block(graph, else_expr).await;
    q_body.push_str(&format!("CREATE (let_stmt)<-HAS_ELSE-(TODO)\n"));

    println!("{}", q_body);
    graph.execute(query(&q_body)).await.unwrap();

    return "TODO_SOME_ID".to_string();
}

async fn gen_pattern(graph: &Graph, pat: Pat) -> String {
    match pat {
        //
        // pattern { name, has_ref, has_mut } <--[HAS_ATTR]-- attr
        //   ^
        //   |---[HAS_PATTERN] -- pattern
        //
        Pat::IdentPat(item) => {
            let mut q_body = String::new();

            let has_ref = if item.ref_token().is_some() {
                ", has_ref: true"
            } else {
                ""
            };

            let has_mut = if item.mut_token().is_some() {
                ", has_mut: true"
            } else {
                ""
            };

            q_body.push_str(&format!(
                "CREATE (pat: IdentifierPattern {{ name: \"{}\" {} {} }})\n",
                item.name().unwrap(),
                has_ref,
                has_mut
            ));

            for attr in item.attrs() {
                let meta = attr.meta().unwrap();
                q_body.push_str(&format!(
                    "CREATE (pat)<-[:HAS_ATTR]-(:Attr {{meta: \"{}\" }})\n",
                    meta
                ));
            }

            if let Some(pat) = item.pat() {
                let pat_id = gen_pattern(graph, pat).await;
                q_body.push_str(&format!("CREATE (pat)<-HAS_PATTERN-(pat: TODO)\n"));
            }

            println!("{}", q_body);
            graph.execute(query(&q_body)).await.unwrap();
        }
        Pat::RestPat(pat) => {}
        //
        // pattern
        //  ^ ^ ^ ...
        //  | | |
        //  | |  ----[HAS_PATTERN] -- pattern
        //  | -------[HAS_PATTERN] -- pattern
        //  ---------[HAS_PATTERN] -- pattern
        //
        Pat::OrPat(pat) => {
            let mut q_body = String::new();

            q_body.push_str(&format!("CREATE (pat: OrPattern )\n"));

            for pat in pat.pats() {
                let pat_id = gen_pattern(graph, pat).await;

                q_body.push_str(&format!("CREATE (pat)<-HAS_PATTERN-(pat: TODO)\n"));
            }
        }
        //
        // Example:
        //
        // ```
        // match s {
        //      Point {x: 10, y: 20} => (),
        //      Point {y: 10, x: 20} => (),
        //      Point {x: 10, ..} => (),
        //      Point {..} => (),
        //  }
        // ```
        //
        // pattern { has_rest }
        //  ^ ^ ^ ...
        //  | | |
        //  | |  ----[HAS_FIELD] -- field {name} <--[HAS_ATTR]-- attr
        //  | |                     ^   ^
        //  | |                     |   |--[HAS_PATTERN] -- pattern
        //  | |                     |
        //  | |                     |-- [HAS_PATH] -- path
        //  | |
        //  | -------[HAS_FIELD] -- field {name} <--[HAS_ATTR]-- attr
        //  ---------[HAS_FIELD] -- field {name} <--[HAS_ATTR]-- attr
        //
        Pat::RecordPat(pat) => {
            let mut q_body = String::new();

            let path_id = gen_path(graph, pat.path().unwrap()).await;
            let list = pat.record_pat_field_list().unwrap();

            let has_rest = if list.rest_pat().is_some() {
                "has_rest: true"
            } else {
                ""
            };

            q_body.push_str(&format!("CREATE (pat: StructPattern {} )\n", has_rest));

            for pat in list.fields() {
                q_body.push_str(&format!(
                    "CREATE (field: StructPatternField {{ name: \"{}\" }} )\n",
                    pat.name_ref().unwrap()
                ));
                q_body.push_str(&format!("CREATE (pat)<-HAS_FIELD-(field)\n"));

                for attr in pat.attrs() {
                    q_body.push_str(&format!(
                        "CREATE (field)<-[:HAS_ATTR]-(:Attr {{meta: \"{}\" }})\n",
                        attr
                    ));
                }

                let pat_id = gen_pattern(graph, pat.pat().unwrap()).await;
                q_body.push_str(&format!("CREATE (field)<-HAS_PATTERN-(pat: TODO)\n"));
            }
        }
        //
        // Example:
        //
        // pattern
        //   |
        //   --[NEXT]--> pattern --[NEXT]--> pattern
        //
        Pat::SlicePat(pat) => {
            let mut q_body = String::new();

            q_body.push_str(&format!("CREATE (pat: SlicePat )\n"));

            for pat in pat.pats() {
                q_body.push_str(&format!("CREATE (pat)-NEXT->(pat: TODO)\n"));
            }
        }
        //
        // Example:
        //
        // ```
        //  match (3, 4, 4) {
        //      (a, b @ 0..=10, c @ 0..=10) => println!("{} {} {}", a, b, c),
        //      _ => println!("another"),
        //  }
        // ```
        //
        // pattern
        //   |
        //   --[NEXT]--> pattern --[NEXT]--> pattern
        //
        Pat::TuplePat(pat) => {
            let mut q_body = String::new();

            q_body.push_str(&format!("CREATE (pat: TuplePat )\n"));

            for pat in pat.fields() {
                q_body.push_str(&format!("CREATE (pat)-NEXT->(pat: TODO)\n"));
            }
        }
        //
        // Example:
        //
        // ```
        // #![feature(inline_const_pat)]
        //
        // match u32::MAX - 1000 {
        //      0 ..= const { u32::MAX / 2 } => println!("low"),
        //      const { u32::MAX / 2 + 1 } ..= u32::MAX => println!("high"),
        //  }
        // ```
        //
        // pattern
        //   |
        //   --[HAS_EXPR]-> block
        //
        Pat::ConstBlockPat(pat) => {
            let mut q_body = String::new();

            q_body.push_str(&format!("CREATE (pat: SlicePat )\n"));
        }
        _ => todo!(),
    }

    todo!()
}

fn gen_path(graph: &Graph, path: Path) -> String {
    todo!()
}
*/

fn gen_enum(graph: &mut UnitedGraph, item: Enum, parent: NodeIndex) -> NodeIndex {
    let enum_ast = graph.add_node(Node {
        labels: vec!["AST".to_string(), "Enum".to_string()],
        fields: Default::default(),
    });

    gen_attrs(graph, item.attrs(), enum_ast);
    gen_docs(graph, item.doc_comments(), enum_ast);

    if let Some(vis) = item.visibility() {
        gen_visibility(graph, vis, enum_ast);
    }

    if let Some(generic_param_list) = item.generic_param_list() {
        gen_generic_param_list(graph, generic_param_list, enum_ast);
    }
    
    if let Some(name) = item.name() {
        gen_name(graph, name, enum_ast);
    }
    
    if let Some(variant_list) = item.variant_list() {
        use syntax::ast::RecordFieldList;
        let items: Vec<(&str, Box<dyn Fn(&VariantList) -> Option<SyntaxToken>>)> = vec![
            ("LCurly", Box::new(VariantList::l_curly_token)),
            ("RCurly", Box::new(VariantList::r_curly_token)),
        ];
        for (name, toke) in items {
            gen_token(graph, name, toke(&variant_list), enum_ast);
        }

        for variant in variant_list.variants() {
            
        }
    }

    gen_token(graph, "Enum", item.enum_token(), enum_ast);

    enum_ast
}

#[test]
fn test_struct_parse() {
    let mut state = State::new();

    let payload = r#"
        pub (in ::super::some) struct Some {
            item: u32,
        }
    "#;

    let parse_result = SourceFile::parse(payload, Edition::Edition2024)
        .ok()
        .unwrap();
    let istruct = parse_result.syntax().descendants().filter_map(Struct::cast);
    for item in istruct {
        gen_struct(&mut state.graph, item);
    }

    let data = format!("{:?}", petgraph::dot::Dot::with_config(&state.graph, &[]));
    fs::write("./foo.dot", data).expect("Unable to write file");
}

fn main() {
    let mut state = State::new();

    let payload = r#"
        #[attr1]
        enum Test1 {
            SOME_VALUE = 42,
        }

        enum Test2<> {
            Some(T),
            None
        }

        enum Test3 {
            Some1((T1, T2, T3)),
            Some((T4, T5, T6)),
        } 

        enum Test4 {
            Some1 {
                field1: T1,
                field2: T2,
            },
            Some2 {
                field3: T3,
                field4: T4,
            },
        }
    "#;

    let parse_result = SourceFile::parse(payload, Edition::Edition2024)
        .ok()
        .unwrap();
    let enums = parse_result.syntax().descendants().filter_map(Struct::cast);
    for item in enums {
        let id = gen_struct(&mut state.graph, item);
        dbg!(id);
    }
}
