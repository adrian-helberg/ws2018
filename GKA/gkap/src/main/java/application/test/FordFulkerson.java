package application.test;

import java.util.HashMap;
import java.util.HashSet;
import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.implementations.MultiGraph;

/**
 * Ford Fulkerson Algorithm
 * @author Maximilian Janzen & Adrian Helberg
 */
public class FordFulkerson extends MaxFlowAlgorithm {
	
	private HashSet<Integer> _unSettledNodes;
	private HashSet<Integer> _settledNodes;
	private HashMap<Integer, Integer> _previousNodes;
	
	public FordFulkerson(MultiGraph graph) {
		super(graph);
		_unSettledNodes = new HashSet<>();
		_settledNodes = new HashSet<>();
		_previousNodes = new HashMap<>();
	}

	/**
	 * Find path from source to sink node and return minimum flow
	 * @param source Source node
	 * @param sink Sink node
	 * @return Minimum flow
	 */
	public int findPath(Node source, Node sink) {
		// Prepare
		_path.clear();
		_unSettledNodes.clear();
		_settledNodes.clear();
		_previousNodes.clear();
		_unSettledNodes.add(source.getIndex());
		int evaluationNode;

		// Until all nodes are processed
		while (!_unSettledNodes.isEmpty()) {
			// Node to evaluate
			evaluationNode = getNode();

			evaluateNeighbors(graph.getNode(evaluationNode));

			_unSettledNodes.remove(evaluationNode);
			_settledNodes.add(evaluationNode);
		}
		
		// Calculate minimum flow
		int minFlow = Integer.MAX_VALUE;

		if (_previousNodes.containsKey(sink.getIndex())) {
			int indexCurrentNode = sink.getIndex();

			// Build up path backwards
			_path.addLast(indexCurrentNode);

			int indexPreviousNode;
			// Process until start node is reached
			while (indexCurrentNode != source.getIndex()) {
				indexPreviousNode = _previousNodes.get(indexCurrentNode);

				// Add previous node to path
				_path.addLast(indexPreviousNode);
				// Compare current flow with new flow
				minFlow = Math.min(minFlow, _capacities[indexPreviousNode][indexCurrentNode] - _flows[indexPreviousNode][indexCurrentNode]);
				indexCurrentNode = indexPreviousNode;
			}
		} else {
			minFlow = 0;
		}

		return minFlow;
	}

	/**
	 * Evaluate neighbor nodes
	 * @param evaluationNode Node to be evaluated
	 */
	private void evaluateNeighbors(Node evaluationNode) {
		int indexEvaluationNode = evaluationNode.getIndex();
		int indexOppositeNode;

		// Iterate outgoing edges
		for (Edge edge : evaluationNode.getLeavingEdgeSet()) {
			// Opposite node
			indexOppositeNode = edge.getOpposite(evaluationNode).getIndex();

			if (!_settledNodes.contains(indexOppositeNode)
					&& !_unSettledNodes.contains(indexOppositeNode)
					&&	_capacities[indexEvaluationNode][indexOppositeNode] - _flows[indexEvaluationNode][indexOppositeNode] > 0) {

				_previousNodes.put(indexOppositeNode, indexEvaluationNode);
				_unSettledNodes.add(indexOppositeNode);
			}
		}
	}

	/**
	 * Return a not inspected node index
	 * @return Index
	 */
	private int getNode() {
		return _unSettledNodes.isEmpty() ? 0 : _unSettledNodes.iterator().next();
	}
}
