// use petgraph::{data::Build, Graph};

// use crate::{Edge, Field, Node, State};

// type NodePattern = Box<dyn Fn(&Node) -> bool>;
// type EdgePattern = Box<dyn Fn(&Edge) -> bool>;

// struct Pattern {
//     on_match: Option<Box<dyn Fn()>>,
//     content: Graph<NodePattern, EdgePattern>,
// }

// impl Pattern {
//     fn new() -> Self {
//         Self { on_match: Option::None, content: Graph::new() }
//     }
// }

// impl State {
//     fn sg_match(pattern: Pattern) -> bool { 
//        todo!()
//     }
// }

// fn validate_ast() -> Pattern {
//     todo!()
// }

// // https://github.com/rust-lang/rust/blob/a1eceec00b2684f947481696ae2322e20d59db60/compiler/rustc_ast_passes/src/ast_validation.rs#L321
// fn deny_unnamed_field() -> Pattern {
//     let mut pat = Pattern::new();
//     let container = pat.content.add_node(Box::new(|n: &Node| {
//         !n.labels.contains(&"Struct".to_string()) ||
//         !n.labels.contains(&"Union".to_string()) 
//     }));
//     let unnamed_field_node = pat.content.add_node(Box::new(|n: &Node| {
//         n.labels.contains(&"Field".to_string()) &&
//         matches!(&n.fields["name"], Field::String(str) if str == "_")
//     }));
//     pat.content.add_edge(container, unnamed_field_node, Box::new(|n: &Edge| {
//         n == "HAS_MEMBER"
//     }));

//     pat.on_match = Some(Box::new(|| { 
//         panic!("unnamed fields can only have struct or union types")
//     }));
//     pat
// }

// #[test]
// fn test_deny_unnamed_field() {
//     let payload = r#"
        
//     "#;

//     let pat = deny_unnamed_field();
//     // TODO: 
// }

