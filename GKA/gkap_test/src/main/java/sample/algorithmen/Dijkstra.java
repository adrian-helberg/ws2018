package sample.algorithmen;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.Path;
import org.graphstream.graph.implementations.MultiGraph;

import sample.exception.InvalidGraphException;

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
		checkNodes(from, to);

		// Start time analysis
		startTimer();

		flagNodes(from);

		List<Path> paths = new LinkedList<>();

		unSettledNodes.add(graph.getNode(from));
		toughGraph();

		Node evaluationNode;
		while (!(unSettledNodes.isEmpty())) {
			evaluationNode = getNodeWithLowestDistance(unSettledNodes);
			unSettledNodes.remove(evaluationNode);
			settledNodes.add(evaluationNode);
			unSettledNodes.addAll(evaluateNeighbors(evaluationNode));
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

	private void flagNodes(String from) {
		for (Node node : graph) {
			toughGraph();
			if (!(node.getId().equals(from))) {
				node.setAttribute(Graph.distance, Integer.MAX_VALUE);
			} else {
				node.setAttribute(Graph.distance, 0);
			}
		}
	}

	private Node getNodeWithLowestDistance(HashSet<Node> unSettledNodes) {
		int lowestDistance = Integer.MAX_VALUE;
		Node lowestNode = null;

		for (Node node : unSettledNodes) {
			if (((Integer) node.getAttribute(Graph.distance)) < lowestDistance) {
				lowestDistance = node.getAttribute(Graph.distance);
				lowestNode = node;
			}
		}

		return lowestNode;
	}

	private HashSet<Node> evaluateNeighbors(Node evaluationNode) {
		HashSet<Node> reachableNodes = new HashSet<>();
		int newDistance = 0;
		for (Edge edge : evaluationNode.getLeavingEdgeSet()) {
			Node nodeTarget = edge.getOpposite(evaluationNode);
			if (!(settledNodes.contains(nodeTarget))) {
				newDistance = (Integer) evaluationNode.getAttribute(Graph.distance) + 
						(Integer) edge.getAttribute(Graph.distance);
				if (newDistance < (Integer) nodeTarget.getAttribute(Graph.distance)) {
					nodeTarget.setAttribute(Graph.distance, newDistance);
					nodeTarget.setAttribute(Graph.prevNode, evaluationNode);
					unSettledNodes.add(nodeTarget);
				}
			}
		}
		return reachableNodes;
	}

	@Override
	public int getTotalDistance() {
		return totalDistance;
	}
}
