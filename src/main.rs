use neo4rs::{query, Graph};
use std::{
    fs::{self, File},
    path::{self, Path, PathBuf},
    sync::Arc,
};
use syntax::{
    ast::{FieldList, HasAttrs, HasDocComments, HasModuleItem, HasName, HasVisibility, Item, Enum},
    SourceFile, AstNode,
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

    for item in tree.items() {
        let items = match item {
            Item::Const(i) => {
                todo!()
            }
            Item::Enum(item) => gen_enum(graph, item).await,
            Item::Module(i) => {}
            _ => {}
        };
    }

    Ok(())
}

async fn gen_enum(graph: &Graph, item: Enum) {
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
        gen_enum(&graph, item);
    }
}
