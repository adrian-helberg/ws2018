package application.algorithm;

import java.util.LinkedList;
import java.util.List;
import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.Path;
import org.graphstream.graph.implementations.MultiGraph;
import application.exception.InvalidGraphException;

/**
 * GKAP BFS
 *
 * @author Adrian Helberg
 * @author Maxmilian Janzen
 */
public class BreadthFirstSearch extends ShortestPathAlgorithm {

	public BreadthFirstSearch(MultiGraph graph) {
		this.graph = graph;
	}

	@Override
	public List<Path> getShortestPath(String from, String to) throws InvalidGraphException {
		checkNodes(from, to);

		// Start time analysis
		startTimer();

		if (!flagNodes(from, to)) {
			return new LinkedList<>();
		}

		List<Path> paths = new LinkedList<>();

		Node target = graph.getNode(to);
		toughGraph();

		Path path = new Path();
		path.setRoot(target);

		if ((int) target.getAttribute(Graph.index) == 0) {
			paths.add(path);
			return paths;
		}

		paths.addAll(addEdges(path, target));
		paths = turnPaths(paths);

		// Stop time analysis
		stopTimer();

		return paths;
	}

	private List<Path> addEdges(Path path, Node target) {
		List<Path> paths = new LinkedList<>();
		Node adjacent;

		int i = target.getAttribute(Graph.index);
		for (Edge edge : target.getEachEnteringEdge()) {
			Object iSource = edge.getSourceNode().getAttribute(Graph.index);
			Object iTarget = edge.getTargetNode().getAttribute(Graph.index);

			if (iSource != null
					&& iTarget != null
					&& (((int) iSource == i - 1 || (int) iTarget == i - 1))) {

				adjacent = getPrevNode(edge, target);
				Path pathNew = path.getACopy();
				pathNew.add(edge);

				if (i <= 1) {
					paths.add(pathNew);
				}

				paths.addAll(addEdges(pathNew, adjacent));
			}
		}
		return paths;
	}

	@Override
	public int getTotalDistance() {
		return 0;
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

	private boolean flagNodes(String from, String to) {
		List<Node> flaggedNodes = new LinkedList<>();

		Node startNode = graph.getNode(from);
		toughGraph();

		startNode.addAttribute(Graph.index, 0);

		if (from.equals(to)) {
			return true;
		}

		flaggedNodes.add(startNode);
		return flagNodes(flaggedNodes, to, 0);
	}

	private boolean flagNodes(List<Node> nodes, String to, int i) {
		Node node;
		List<Node> flaggedNodes = new LinkedList<>();

		for (Node currentNode : nodes) {
			for (Edge edge : currentNode.getEachLeavingEdge()) {

				if (edge.getTargetNode().getAttribute(Graph.index) == null
						|| edge.getSourceNode().getAttribute(Graph.index) == null) {

					node = getNextNode(edge, currentNode);
					node.addAttribute(Graph.index, i + 1);

					flaggedNodes.add(node);

					if (node.getId().equals(to)) {
						return true;
					}
				}
			}
		}

		if (flaggedNodes.isEmpty()) {
			return false;
		} else {
			return flagNodes(flaggedNodes, to, i + 1);
		}
	}

	private Node getNextNode(Edge edge, Node from) {
		Node adjacent;

		if (edge.isDirected()) {
			adjacent = edge.getTargetNode();
		} else {
			if (edge.getTargetNode().getId().equals(from.getId())) {
				adjacent = edge.getSourceNode();
			} else {
				adjacent = edge.getTargetNode();
			}
		}

		return adjacent;
	}

}
