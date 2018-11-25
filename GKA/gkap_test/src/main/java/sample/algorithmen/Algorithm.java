package sample.algorithmen;

import java.util.LinkedList;
import java.util.List;
import org.graphstream.algorithm.AStar;
import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.Path;
import org.graphstream.graph.implementations.MultiGraph;
import sample.exception.InvalidGraphException;

/**
 * GKAP Base algorithm
 *
 * @author Adrian Helberg
 * @author Maxmilian Janzen
 */
public abstract class Algorithm {
	MultiGraph graph;
	private long timeMilliseconds;
	private long timeNanoseconds;
	private int graphTouched = 0;

	/**
	 * Set current time
	 */
	void startTimer() {
		timeMilliseconds = System.currentTimeMillis();
		timeNanoseconds = System.nanoTime();
	}
	
	/**
	 * Set passed time
	 */
	void stopTimer() {
		timeMilliseconds = System.currentTimeMillis() - timeMilliseconds;
		timeNanoseconds = System.nanoTime() - timeNanoseconds;
	}
	
	/**
	 * Return milliseconds String
	 * @return Milliseconds as String
	 */
	public String getStringTimeMilliseconds() {
		return timeMilliseconds + " ms";
	}

	/**
	 * Return nanoseconds String
	 * @return Nanoseconds as String
	 */
	public String getStringTimeNanoseconds() {
		return timeNanoseconds + " ns";
	}
	
	/**
	 * Check given nodes and graph
	 * @param from Source node
	 * @param to Target node
	 * @throws InvalidGraphException If invalid nodes or graph
	 */
	boolean checkNodes(String from, String to) throws InvalidGraphException {
		int _graphHash = 0;
		if (graph != null && _graphHash != graph.hashCode()) {

			AStar aStar = new AStar(graph);
			aStar.compute(from, to);
			Path path = aStar.getShortestPath();

			if (path == null || path.size() <= 0) {
				throw new InvalidGraphException("There is no path from " + from + " to " + to);
			}
		}

		if (from == null || graph.getNode(from) == null) {
			throw new IllegalArgumentException("Node not found: " + from);
		}

		if (to == null || graph.getNode(to) == null) {
			throw new IllegalArgumentException("Node not found: " + to);
		}

		return true;
	}
	
	/**
	 * Reverse path list
	 * @param pathList List of Paths
	 * @return Reversed path list
	 */
	List<Path> turnPaths(List<Path> pathList) {
		List<Path> paths = new LinkedList<>();

		for (Path path : pathList) {
			List<Edge> pathEdgeList = path.getEdgePath();
			List<Node> pathNodeList = path.getNodePath();
			Path pathNeu = new Path();
			pathNeu.setRoot(pathNodeList.get(pathNodeList.size() - 1));

			for (int i = pathEdgeList.size() - 1; i >= 0; i--) {
				pathNeu.add(pathEdgeList.get(i));
			}

			paths.add(pathNeu);
		}

		return paths;
	}
	
	/**
	 * Increase number of graph accesses
	 */
	void toughGraph() {
		graphTouched++;
	}
	
	/**
	 * Return graph accesses
	 * @return Integer
	 */
	public int getGraphTouches() {
		return graphTouched;
	}
}
