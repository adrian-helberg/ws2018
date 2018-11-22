package sample.algorithmen;

import java.util.LinkedList;
import java.util.List;
import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.Path;
import org.graphstream.graph.implementations.MultiGraph;
import sample.exception.InvalidGraphException;

public class FloydWarschall extends ShortestPathAlgorithm {
	
    private int[][] _distances;
    private Node[] _nodeAry;
    private int[][] _transit;
    private int _nodeCount = 0;
    private int _totalDistance = 0;
    
	public FloydWarschall(MultiGraph graph) {
		_graph = graph;
		_nodeCount = graph.getNodeCount();
		_distances = new int[_nodeCount][_nodeCount];
		_nodeAry = new Node[_graph.getNodeCount()];
		_transit = new int[_nodeCount][_nodeCount];
		increaseGraphAccess();
		int n = 0;
		for (Node node : _graph) {
			increaseGraphAccess();
			_nodeAry[n] = node;
			n++;
		}
	}

	@Override
	public List<Path> getShortestPath(String from, String to) throws InvalidGraphException {
		checkNodes(from, to);
		startTimer();
		flagNodes();
		List<Path> paths = new LinkedList<>();
		for (int i = 0; i < _nodeCount; i++) {
			for (int j = 0; j < _nodeCount; j++) {
				if (!(i == j)) {
					for (int k = 0; k < _nodeCount; k++) {
						if (!(k == i)) {
							if (_distances[j][i] != Integer.MAX_VALUE && _distances[i][k] != Integer.MAX_VALUE &&
									_distances[j][k] > _distances[j][i] + _distances[i][k]) {
								_distances[j][k] = _distances[j][i] + _distances[i][k];
								_transit[j][k] = _transit[i][k];
							}
						}
					}
					if (_distances[j][j] < 0) {
						throw new InvalidGraphException("Es wurde ein Kreis mit negativer Laenge gefunden!");
					}
				}
			}
		}
		Node nodeFrom = _graph.getNode(from);
		increaseGraphAccess();
		Node nodeTo = _graph.getNode(to);
		increaseGraphAccess();
		int intI = 0;
		int intJ = 0;
		for (int i = 0; i < _nodeCount; i++) {
			if (_nodeAry[i].equals(nodeFrom)) {
				intI = i;
			}
			if (_nodeAry[i].equals(nodeTo)) {
				intJ = i;
			}
		}
		Path path = calculateMinPath(nodeFrom, nodeTo, intI, intJ);
		if (!path.empty()) {
			paths.add(path);
			paths = turnPaths(paths);
		}
		stopTimer();
		return paths;
	}
	
	@Override
	public int getTotalDistance() {
		return _totalDistance;
	}
	
	private void flagNodes() {
		for (int i = 0; i < _nodeCount; i++) {
			for (int j = 0; j < _nodeCount; j++) {
				if (_nodeAry[j].hasEdgeBetween(_nodeAry[i])) {
					Edge edge = _nodeAry[j].getEdgeBetween(_nodeAry[i]);
					_distances[i][j] = edge.getAttribute(Graph.distance);
					_transit[i][j] = i;
				} else {
					_distances[i][j] = Integer.MAX_VALUE;
				}
			}
		}
		
	}
	
	private Path calculateMinPath(Node from, Node to, int indexFrom, int indexTo) throws InvalidGraphException {
		Path path = new Path();
		path.setRoot(to);
		Node currentNode = to;
		int currentIndex = indexTo;
		if (_distances[indexFrom][currentIndex] == Integer.MAX_VALUE) {
			return path;
		}
		while (!currentNode.equals(from)) {
			currentIndex = _transit[indexFrom][currentIndex];
			currentNode = _nodeAry[currentIndex];
			Edge edge = currentNode.getEdgeBetween(to);
			_totalDistance = _totalDistance + (Integer) edge.getAttribute(Graph.distance);
			path.add(edge);
			to = currentNode;
			indexTo = currentIndex;
		}
		return path;
	}
}