use stack_graphs::arena::Handle;
use stack_graphs::graph::{File, Node, StackGraph};
use stack_graphs::partial::PartialPaths;
use stack_graphs::stitching::{ForwardPartialPathStitcher, GraphEdgeCandidates, StitcherConfig};
use stack_graphs::NoCancellation;

pub fn find_reference_at_position(
    graph: &StackGraph,
    file: Handle<File>,
    line: usize,
    column: usize,
) -> Option<Handle<Node>> {
    graph
        .nodes_for_file(file)
        .filter(|&node| graph[node].is_reference())
        .find(|&node| {
            // source_info содержит позицию в исходном коде
            if let Some(info) = graph.source_info(node) {
                info.span.contains(line, column)
            } else {
                false
            }
        })
}


pub fn goto_definition(
    graph: &StackGraph,
    reference_node: Handle<Node>, 
) -> Vec<Handle<Node>> {
    let mut partials = PartialPaths::new();
    let mut definitions = Vec::new();
    
    ForwardPartialPathStitcher::find_all_complete_partial_paths(
        &mut GraphEdgeCandidates::new(graph, &mut partials, None),
        std::iter::once(reference_node), 
        StitcherConfig::default(),
        &NoCancellation,
        |graph, _partials, path| {
            let end_node = path.end_node;
            if graph[end_node].is_definition() {
                definitions.push(end_node);
            }
        },
    )
    .expect("should not be cancelled");
    
    definitions
}
