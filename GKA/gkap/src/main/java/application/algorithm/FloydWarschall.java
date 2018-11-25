package application.algorithm;

import java.util.LinkedList;
import java.util.List;
import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.Path;
import org.graphstream.graph.implementations.MultiGraph;
import application.exception.InvalidGraphException;

/**
 * GKAP Floyd-Warschall Shortest Path
 *
 * @author Adrian Helberg
 * @author Maximilian Janzen
 * @see <a href="https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm">Floyd-Warschall Algorithm</a>
 */
public class FloydWarschall extends ShortestPathAlgorithm {
    private int[][] distances;
    private Node[] nodeAry;
    private int[][] _transit;
    private int nodeCount;
    private int _totalDistance = 0;
    
	public FloydWarschall(MultiGraph graph) {
		this.graph = graph;
		nodeCount = graph.getNodeCount();
		distances = new int[nodeCount][nodeCount];
		nodeAry = new Node[graph.getNodeCount()];
		_transit = new int[nodeCount][nodeCount];
		toughGraph();

		int n = 0;
		for (Node node : graph) {
			toughGraph();
			nodeAry[n] = node;
			n++;
		}
	}

	@Override
	public List<Path> getShortestPath(String from, String to) throws InvalidGraphException {
		List<Path> paths = new LinkedList<>();

		// Initial
		checkNodes(from, to);
		startTimer();
		flagNodes();

		for (int i = 0; i < nodeCount; i++) {
			for (int j = 0; j < nodeCount; j++) {

				if (!(i == j)) {

					for (int k = 0; k < nodeCount; k++) {

						if (!(k == i)) {
							if (distances[j][i] != Integer.MAX_VALUE && distances[i][k] != Integer.MAX_VALUE
									&& distances[j][k] > distances[j][i] + distances[i][k]) {

								distances[j][k] = distances[j][i] + distances[i][k];
								_transit[j][k] = _transit[i][k];

							}
						}
					}
					if (distances[j][j] < 0) {
						throw new InvalidGraphException("Circle detected");
					}
				}
			}
		}

		Node nodeFrom = graph.getNode(from);
		toughGraph();
		Node nodeTo = graph.getNode(to);
		toughGraph();
		int intI = 0;
		int intJ = 0;

		for (int i = 0; i < nodeCount; i++) {
			if (nodeAry[i].equals(nodeFrom)) {
				intI = i;
			}
			if (nodeAry[i].equals(nodeTo)) {
				intJ = i;
			}
		}

		Path path = calculateMinPath(nodeFrom, nodeTo, intI, intJ);

		if (!path.empty()) {
			paths.add(path);
			paths = turnPaths(paths);
		}

		// Stop time analysis
		stopTimer();

		return paths;
	}
	
	@Override
	public int getTotalDistance() {
		return _totalDistance;
	}
	
	private void flagNodes() {
		for (int i = 0; i < nodeCount; i++) {
			for (int j = 0; j < nodeCount; j++) {
				if (nodeAry[j].hasEdgeBetween(nodeAry[i])) {
					Edge edge = nodeAry[j].getEdgeBetween(nodeAry[i]);
					distances[i][j] = edge.getAttribute(Graph.distance);
					_transit[i][j] = i;
				} else {
					distances[i][j] = Integer.MAX_VALUE;
				}
			}
		}
	}
	
	private Path calculateMinPath(Node from, Node to, int indexFrom, int indexTo) {
		Path path = new Path();
		path.setRoot(to);
		Node currentNode = to;
		int currentIndex = indexTo;

		if (distances[indexFrom][currentIndex] == Integer.MAX_VALUE) {
			return path;
		}

		while (!currentNode.equals(from)) {
			currentIndex = _transit[indexFrom][currentIndex];
			currentNode = nodeAry[currentIndex];
			Edge edge = currentNode.getEdgeBetween(to);
			_totalDistance = _totalDistance + (Integer) edge.getAttribute(Graph.distance);
			path.add(edge);
			to = currentNode;
		}
		return path;
	}
}