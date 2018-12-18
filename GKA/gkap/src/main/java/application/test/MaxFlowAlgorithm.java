package application.test;

import java.util.ArrayList;
import java.util.LinkedList;
import application.algorithm.Algorithm;
import application.algorithm.Graph;
import application.exception.InvalidGraphException;
import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.implementations.MultiGraph;

/**
 * @author Maximilian Janzen & Adrian Helberg
 */
public abstract class MaxFlowAlgorithm extends Algorithm {
	
	int[][] _capacities;
	int[][] _flows;
	LinkedList<Integer> _path;
	
	MaxFlowAlgorithm(MultiGraph graph) {
		if (graph == null) {
			System.out.println("No graph passed");
			System.exit(-1);
		}

		this.graph = graph;
		// Prepare capacities and flows
		int nodeCount = graph.getNodeCount();
		_capacities = new int[nodeCount][nodeCount];
		_flows = new int[nodeCount][nodeCount];

		for (int i = 0; i < nodeCount; i++) {
			for (int j = 0; j < nodeCount; j++) {
				_flows[i][j] = 0;
				_capacities[i][j] = Integer.MIN_VALUE;
			}
		}

		int i, j;
		// Iterate edges
		for (Edge edge : graph.getEachEdge()) {
			i = edge.getSourceNode().getIndex();
			j = edge.getTargetNode().getIndex();
			// Set capacity
			_capacities[i][j] = edge.getAttribute(Graph.distance);

			if (!edge.isDirected()) {
				_capacities[j][i] = edge.getAttribute(Graph.distance);
			}
		}

		_path = new LinkedList<>();
	}

	/**
	 * Get maximum flow
	 * @param source Source identifier
	 * @param sink Sing identifier
	 * @return Maximum flow
	 * @throws InvalidGraphException If invalid nodes
	 */
	public int getMaxFlow(String source, String sink) throws InvalidGraphException {
		// Validate nodes
		checkNodes(source, sink);
		// Start time analysis
		startTimer();

		int maxFlow = 0;
		Node sourceNode = graph.getNode(source);
		Node sinkNode = graph.getNode(sink);

		int minCurrentFlow;
		int indexSource, indexTarget;

		while ((minCurrentFlow = findPath(sourceNode, sinkNode)) > 0) {
			for (int i = 1; i < _path.size(); i++) {
				indexSource = _path.get(i - 1);
				indexTarget = _path.get(i);
				_flows[indexTarget][indexSource] = (_flows[indexTarget][indexSource] + minCurrentFlow);
				_flows[indexSource][indexTarget] = (_flows[indexSource][indexTarget] - minCurrentFlow);
			}
			maxFlow += minCurrentFlow;
		}

		// Stop time analysis
		stopTimer();

		return maxFlow;
	}

	abstract protected int findPath(Node source, Node sink);
}
