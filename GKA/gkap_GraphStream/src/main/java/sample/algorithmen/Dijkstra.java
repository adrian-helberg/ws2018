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
	// Dijkstra für gerichtete Graphen
	private HashSet<Node> _unSettledNodes;
	private HashSet<Node> _settledNodes;
	private int _totalDistance = 0;
	
	/**
	 * Initialisiert einen Dijkstra-Algorithmus
	 * @param graph Zu durchsuchender Graph
	 */
	public Dijkstra(MultiGraph graph) {
		_graph = graph;
		_unSettledNodes = new HashSet<Node>();
		_settledNodes = new HashSet<Node>();
	}
	
	@Override
	public List<Path> getShortestPath(String from, String to) throws InvalidGraphException {
		checkNodes(from, to);
		startTimer();
		flagNodes(from);
		List<Path> paths = new LinkedList<>();
		_unSettledNodes.add(_graph.getNode(from));
		increaseGraphAccess();
		Node evaluationNode;
		while (!(_unSettledNodes.isEmpty())) {
			evaluationNode = getNodeWithLowestDistance(_unSettledNodes);
			_unSettledNodes.remove(evaluationNode);
			_settledNodes.add(evaluationNode);
			 _unSettledNodes.addAll(evaluateNeighbors(evaluationNode));
		}
		Node nodeTo = _graph.getNode(to);
		increaseGraphAccess();
		if ((Integer) nodeTo.getAttribute(Graph.distance) < Integer.MAX_VALUE) {
			Path path = new Path();
			path.setRoot(nodeTo);
			Node nodeFrom = _graph.getNode(from);
			Node currentNode = null;
			while (!nodeFrom.equals(currentNode)) {
				currentNode = nodeTo.getAttribute(Graph.prevNode);
				Edge edge = currentNode.getEdgeBetween(nodeTo);
				_totalDistance = _totalDistance + (Integer) edge.getAttribute(Graph.distance);
				path.add(edge);
				nodeTo = currentNode;
			}
			
			paths.add(path);
		}
		paths = turnPaths(paths);
		stopTimer();
		return paths;
	}

	/**
	 * Setzt die Distanz fuer alle Knoten auf pseudo-unendlich
	 * @param from String Startknoten
	 */
	private void flagNodes(String from) {
		for (Node node : _graph) {
			increaseGraphAccess();
			if (!(node.getId().equals(from))) {
				node.setAttribute(Graph.distance, Integer.MAX_VALUE);
			} else {
				node.setAttribute(Graph.distance, 0);
			}
		}
	}
	
	/**
	 * Sucht den Knoten mit der kleinsten Distanz aus einem Set
	 * @param unSettledNodes HashSet<Node>
	 * @return ein Knoten
	 */
	private Node getNodeWithLowestDistance(HashSet<Node> unSettledNodes) {
		int lowestDistance = Integer.MAX_VALUE;
		Node lowestNode = null;
		for (Node node : unSettledNodes) {
			if (((Integer) node.getAttribute(Graph.distance)) < lowestDistance) {
				lowestDistance = (Integer) node.getAttribute(Graph.distance);
				lowestNode = node;
			}
		}
		return lowestNode;
	}
	
	/**
	 * Listet zu einem Knoten alle Nachbarknoten auf
	 * @param evaluationNode Ausgangsknoten
	 * @return HashSet<Node>
	 */
	private HashSet<Node> evaluateNeighbors(Node evaluationNode) {
		HashSet<Node> reachableNodes = new HashSet<Node>();
		int newDistance = 0;
		for (Edge edge : evaluationNode.getLeavingEdgeSet()) {
			Node nodeTarget = edge.getOpposite(evaluationNode);
			if (!(_settledNodes.contains(nodeTarget))) {
				newDistance = (Integer) evaluationNode.getAttribute(Graph.distance) + 
						(Integer) edge.getAttribute(Graph.distance);
				if (newDistance < (Integer) nodeTarget.getAttribute(Graph.distance)) {
					nodeTarget.setAttribute(Graph.distance, newDistance);
					nodeTarget.setAttribute(Graph.prevNode, evaluationNode);
					_unSettledNodes.add(nodeTarget);
				}
			}
		}
		return reachableNodes;
	}

	@Override
	public int getTotalDistance() {
		return _totalDistance;
	}
}
