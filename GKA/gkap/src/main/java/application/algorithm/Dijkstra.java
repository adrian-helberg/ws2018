package application.algorithm;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.Path;
import org.graphstream.graph.implementations.MultiGraph;
import application.exception.InvalidGraphException;

/**
 * GKAP Dijkstra Shortest Path
 *
 * @author Adrian Helberg
 * @author Maximilian Janzen
 * @see <a href="https://de.wikipedia.org/wiki/Dijkstra-Algorithmus">Dijkstra Algorithm</a>
 */
public class Dijkstra extends ShortestPathAlgorithm {
	private HashSet<Node> unSettledNodes;
	private HashSet<Node> settledNodes;
	private int totalDistance = 0;

	public Dijkstra(MultiGraph graph) {
		this.graph = graph;
		unSettledNodes = new HashSet<>();
		settledNodes = new HashSet<>();
	}
	
	@Override
	public List<Path> getShortestPath(String from, String to) throws InvalidGraphException {
		List<Path> paths = new LinkedList<>();

		// Initial
		checkNodes(from, to);
		startTimer();
		flagNodes(from);

		// Add source node
		unSettledNodes.add(graph.getNode(from));
		toughGraph();

		Node evaluationNode;
		while (!(unSettledNodes.isEmpty())) {
			evaluationNode = getNodeWithLowestDistance(unSettledNodes);

			unSettledNodes.remove(evaluationNode);
			settledNodes.add(evaluationNode);

			evaluateNeighbors(evaluationNode);
		}

		Node nodeTo = graph.getNode(to);
		toughGraph();

		if ((Integer) nodeTo.getAttribute(Graph.distance) < Integer.MAX_VALUE) {
			Path path = new Path();
			path.setRoot(nodeTo);

			Node nodeFrom = graph.getNode(from);
			Node currentNode = null;

			while (!nodeFrom.equals(currentNode)) {
				currentNode = nodeTo.getAttribute(Graph.prevNode);
				Edge edge = currentNode.getEdgeBetween(nodeTo);
				totalDistance = totalDistance + (Integer) edge.getAttribute(Graph.distance);
				path.add(edge);
				nodeTo = currentNode;
			}
			
			paths.add(path);
		}

		paths = turnPaths(paths);

		// Stop time analysis
		stopTimer();

		return paths;
	}

	/**
	 * Set node distances to integer max value (infinity)
	 * @param from Source node
	 */
	private void flagNodes(String from) {
		for (Node node : graph) {
			// Graph access
			toughGraph();
			if (!(node.getId().equals(from))) {
				node.setAttribute(Graph.distance, Integer.MAX_VALUE);
			} else {
				node.setAttribute(Graph.distance, 0);
			}
		}
	}

	/**
	 * Return Node with lowest distance
	 * @param unSettledNodes Nodes to check
	 * @return Node with lowest distance
	 */
	private Node getNodeWithLowestDistance(HashSet<Node> unSettledNodes) {
		int lowestDistance = Integer.MAX_VALUE;
		Node lowestNode = null;

		for (Node node : unSettledNodes) {

			if (((int) node.getAttribute(Graph.distance)) < lowestDistance) {
				lowestDistance = node.getAttribute(Graph.distance);
				lowestNode = node;
			}
		}

		return lowestNode;
	}

	/**
	 * Process adjacent Nodes
	 * @param evaluationNode Current node to process from
	 */
	private void evaluateNeighbors(Node evaluationNode) {
		int newDistance;

		// Process adjacent edged
		for (Edge edge : evaluationNode.getLeavingEdgeSet()) {

			Node nodeTarget = edge.getOpposite(evaluationNode);

			if (!(settledNodes.contains(nodeTarget))) {

				newDistance = (int) evaluationNode.getAttribute(Graph.distance) +
						(int) edge.getAttribute(Graph.distance);

				if (newDistance < (int) nodeTarget.getAttribute(Graph.distance)) {

					nodeTarget.setAttribute(Graph.distance, newDistance);
					nodeTarget.setAttribute(Graph.prevNode, evaluationNode);
					unSettledNodes.add(nodeTarget);
				}
			}
		}
	}

	@Override
	public int getTotalDistance() {
		return totalDistance;
	}
}
