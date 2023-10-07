use neo4rs::{query, Graph};
use std::{
    fs::{self, File},
    path::{self, Path, PathBuf},
    sync::Arc,
};
use syntax::{
    ast::{
        BlockExpr, Enum, Expr, FieldList, HasAttrs, HasDocComments, HasModuleItem, HasName,
        HasVisibility, IfExpr, Item, Stmt, LetStmt, Pat, Type,
    },
    AstNode, SourceFile,
};

async fn parse_file(graph: &mut Graph, path: &Path) -> anyhow::Result<()> {
    let payload = fs::read_to_string(path)?;

    let parse_result = SourceFile::parse(&payload);
    let tree = parse_result.tree();

    let mut q_body = String::new();

    q_body.push_str(&format!(
        "CREATE (enum:SourceFile {{path: \"{}\" }})\n",
        path.to_string_lossy()
    ));

    for attr in tree.attrs() {
        let meta = attr.meta().unwrap();

        q_body.push_str(&format!(
            "CREATE (enum)<-[:HAS_ATTR]-(:Attr {{meta: \"{}\" }})\n",
            meta
        ));
    }

    for comment in tree.doc_comments() {
        q_body.push_str(&format!(
            "CREATE (enum)<-[:HAS_COMMENT]-(:Comment {{text: \"{}\" }})\n",
            comment
        ));
    }

    println!("{}", q_body);
    graph.execute(query(&q_body)).await.unwrap();

    for item in tree.items() {
        gen_item(&graph, item);
    }

    Ok(())
}

async fn gen_item(graph: &Graph, item: Item) -> String {
    let id = match item {
        Item::Const(item) => {
            // Constant { name }
            // HAS_TYPE
            // HAS_ATTR
            // VISIBLE_FROM
            // TODO: Expr
            // TODO: default for specialization

            let expr = item.body().unwrap();
        }
        Item::Enum(item) => gen_enum(graph, item).await,
        Item::Module(i) => todo!(),
        _ => todo!(),
    };

    "TODO_SOME_ID".to_string()
}

async fn gen_expr(graph: &Graph, item: Expr) -> String {
    return match item {
        syntax::ast::Expr::IfExpr(item) => gen_if_expr(graph, item).await,
        _ => todo!(),
    };
}

async fn gen_if_expr(graph: &Graph, item: IfExpr) -> String {
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

async fn gen_block(graph: &Graph, expr: BlockExpr) -> String {
    let mut q_body = String::new();

    q_body.push_str(&format!("CREATE (block: BlockExpression)\n"));

    for attr in expr.attrs() {
        q_body.push_str(&format!(
            "CREATE (block)<-[:HAS_ATTR]-(:Attr {{meta: \"{}\" }})\n",
            attr
        ));
    }
  
    if let Some(stms) = expr.stmt_list() {
        for stmt in stms.statements() {
            let id = match stmt {
                Stmt::ExprStmt(expr) => gen_expr(graph, expr.expr().unwrap()).await,
                Stmt::Item(item) => gen_item(graph, item).await,
                Stmt::LetStmt(let_stmt) => gen_let_stmt(graph, let_stmt).await,
            };
        }
    }

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
    
    q_body.push_str(&format!("CREATE (let_stmt: LetStatement) RETURN ID(let_stmt)\n"));
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

            q_body.push_str(
                &format!("CREATE (pat: IdentifierPattern {{ name: \"{}\" {} {} }})\n", 
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
        },
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

            q_body.push_str(
                &format!("CREATE (pat: OrPattern )\n"
            ));

            for pat in pat.pats() {
                let pat_id = gen_pattern(graph, pat).await;
                
                q_body.push_str(&format!("CREATE (pat)<-HAS_PATTERN-(pat: TODO)\n"));
            }
        }
        _ => todo!(),
    }

    todo!()
}

async fn gen_type(graph: &Graph, ty: Type) -> String {
    todo!()
}

async fn gen_enum(graph: &Graph, item: Enum) -> String {
    //TODO: generic params ???
    //TODO: where clause ???
    //TODO: visibility - what is target node???
    //TODO: TupleField
    //TODO: RecordFieldList

    // CREATE (enum:Enum {name: $name}),
    // (enum)<-[:HAS_VARIANT]-(variant:Variant {name: $name})
    // (enum)<-[:VISIBLE_FROM]-(s:SourceFile {name: $name})
    // (enum)<-[:HAS_ATTR]-(:Attr {meta: $meta})
    //
    // (variant)<-[:VISIBLE_FROM]-(s:SourceFile {name: $name})
    // (variant)<-[:HAS_ATTR]-(:Attr {meta: $meta})
    //
    // Value form variant
    //(variant)<-[:HAS_VALUE]-(expr:Expr)
    //
    // Record form variant
    // (variant)<-[:HAS_VALUE]-(rec:Record)
    // (rec)<-[:HAS_MEMBER]-(f:Field {name: $name})
    //
    // Tuple form variant
    // (variant)<-[:HAS_VALUE]-(tuple:Tuple)
    // (tuple)<-[:HAS_MEMBER]-(f:Field {name: $name})

    let mut q_body = String::new();

    let name = item.name().unwrap();
    q_body.push_str(&format!("CREATE (enum:Enum {{name: \"{}\" }})\n", name));

    for attr in item.attrs() {
        q_body.push_str(&format!(
            "CREATE (enum)<-[:HAS_ATTR]-(:Attr {{meta: \"{}\" }})\n",
            attr
        ));
    }

    // TODO: now visibility point to source file
    q_body.push_str("CREATE (enum)<-[:VISIBLE_FROM]-(s:SourceFile {name: \"TEST.rs\"})\n");

    let mut glob_record_field_idx = 0;
    let mut glob_tuple_field_idx = 0;
    for (variant_idx, variant) in item.variant_list().unwrap().variants().enumerate() {
        let name = variant.name().unwrap();
        q_body.push_str(&format!(
            "CREATE (enum)<-[:HAS_VARIANT]-(variant_{}:Variant {{name: \"{}\" }})\n",
            variant_idx, name
        ));

        for attr in variant.attrs() {
            let meta = attr.meta().unwrap();
            q_body.push_str(&format!(
                "CREATE (variant_{})<-[:HAS_ATTR]-(:Attr {{meta: \"{}\" }})\n",
                variant_idx, meta
            ));
        }

        // TODO: now visibility point to source file
        q_body.push_str(&format!(
            "CREATE (variant_{})<-[:VISIBLE_FROM]-(s)\n",
            variant_idx
        ));

        // Value form variant
        if variant.eq_token().is_some() {
            let expr = variant.expr().unwrap();
            q_body.push_str(&format!(
                "CREATE (variant_{})<-[:HAS_VALUE]-(expr:Expr {{expr: \"{}\" }})\n",
                variant_idx, expr
            ));
        } else {
            if let Some(field_list) = variant.field_list() {
                match field_list {
                    // Record form variant
                    FieldList::RecordFieldList(rec_list) => {
                        q_body.push_str(&format!(
                            "CREATE (variant_{})<-[:HAS_VALUE]-(rec_{}:Record)\n",
                            variant_idx, variant_idx
                        ));

                        for field in rec_list.fields() {
                            let name = field.name().unwrap();
                            q_body.push_str(&format!(
                                            "CREATE (rec_{})<-[:HAS_MEMBER]-(filed_{}:Field {{name: \"{}\" }})\n",
                                            variant_idx, glob_record_field_idx, name
                                        ));
                            glob_record_field_idx += 1;
                        }
                    }
                    // Tuple form variant
                    FieldList::TupleFieldList(tup_list) => {
                        q_body.push_str(&format!(
                            "CREATE (variant_{})<-[:HAS_VALUE]-(tuple_{}:Tuple)\n",
                            variant_idx, variant_idx
                        ));
                        for field in tup_list.fields() {
                            q_body.push_str(&format!(
                                "CREATE (tuple_{})<-[:HAS_MEMBER]-(filed_{}:Field)\n",
                                variant_idx, glob_tuple_field_idx
                            ));
                            glob_tuple_field_idx += 1;
                        }
                    }
                }
            }
        }
    }
    println!("{}", q_body);
    graph.execute(query(&q_body)).await.unwrap();

    return "TODO_SOME_ID".to_string();
}

#[tokio::main]
async fn main() {
    let uri = "localhost:7687";
    let user = "neo4j";
    let pass = "password";
    let graph = Arc::new(Graph::new(uri, user, pass).await.unwrap());

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
    let parse_result = SourceFile::parse(payload).ok().unwrap();
    let enums = parse_result.syntax().descendants().filter_map(Enum::cast);
    for item in enums {
        let id = gen_enum(&graph, item).await;
        dbg!(id);
    }
}
