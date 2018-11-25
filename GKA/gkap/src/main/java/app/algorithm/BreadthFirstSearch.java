package app.algorithm;

import org.graphstream.graph.Edge;
import org.graphstream.graph.Graph;
import org.graphstream.graph.Node;
import org.graphstream.graph.Path;
import org.graphstream.graph.implementations.MultiGraph;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

public class BreadthFirstSearch extends ShortestPathAlgorithm {

    public BreadthFirstSearch(MultiGraph graph) {
        super.graph = graph;
    }

    @Override
    public List<Path> getShortestPath(String from, String to) throws InvalidGraphException {
        checkNodes(from, to);

        // Check if source node and target node are the same
        if (from.equals(to)) return new LinkedList<>();

        List<Path> paths = new LinkedList<>();
        Node target = graph.getNode(to);
        graphTouched();

        Path path = new Path();
        // Start at target node
        path.setRoot(target);

        paths.addAll(addEdges(path, target));

        // TODO: reverse paths list

        return paths;
    }

    public boolean flagNodes(String from, String to) {
        List<Node> flaggedNodes = new LinkedList<>();

        Node startNode = graph.getNode(from);
        graphTouched();

        startNode.addAttribute("index", 0);

        if (from.equals(to)) return true;

        flaggedNodes.add(startNode);

        return flagNodes(flaggedNodes, to, 0);
    }

    private boolean flagNodes(List<Node> nodes, String to, int i) {
        Node node;
        List<Node> flaggedNodes = new LinkedList<>();

        for (Node currentNode : nodes) {
            for (Edge edge : currentNode.getEachLeavingEdge()) {

                if (edge.getTargetNode().getAttribute("index") == null || edge.getSourceNode().getAttribute("index") == null) {
                    node = getNextNode(edge, currentNode);
                    node.addAttribute("index", i + 1);
                    flaggedNodes.add(node);
                    if (node.getId().equals(to)) {
                        return true;
                    }
                }
            }
        }
        if (flaggedNodes.isEmpty()) { // Falls kein neuer Knoten erreicht wurde
            return false;
        } else {
            return flagNodes(flaggedNodes, to, i + 1); // Rekursiver Aufruf
        }
    }

    private Node getNextNode(Edge edge, Node from) {
        Node adjacent = null;

        if (edge.isDirected()) {

            adjacent = edge.getTargetNode();

            if (edge.getTargetNode().getId().equals(from.getId())) {
                adjacent = edge.getSourceNode();
            } else {
                adjacent = edge.getTargetNode();
            }
        }

        return adjacent;
    }

    private Node getPrevNode(Edge edge, Node source) {
        Node adjacent;

        if (edge.isDirected()) {
            adjacent = edge.getSourceNode();
        } else {
            if (edge.getSourceNode().getId().equals(source.getId())) {
                adjacent = edge.getTargetNode();
            } else {
                adjacent = edge.getSourceNode();
            }
        }
        return adjacent;
    }

    private List<Path> addEdges(Path path, Node target) {
        List<Path> paths = new LinkedList<>();
        Node adjacent;
        int i = target.getAttribute("index");

        for (Edge edge : target.getEachEnteringEdge()) {

            Object iSource = edge.getSourceNode().getAttribute("index");
            Object iTarget = edge.getTargetNode().getAttribute("index");

            if (iSource != null && iTarget != null && (((int) iSource == i - 1 || (int) iTarget == i - 1))) {

                adjacent = getPrevNode(edge, target);
                Path pathNew = path.getACopy();
                pathNew.add(edge);

                if (i <= 1) paths.add(pathNew);

                paths.addAll(addEdges(pathNew, adjacent));
            }
        }
        return paths;
    }

    @Override
    public int getDistance() {
        return 0;
    }
}
