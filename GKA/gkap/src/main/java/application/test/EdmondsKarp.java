package application.test;

import java.util.HashMap;
import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.implementations.MultiGraph;

/**
 * Edmonds Karp Algorithm
 * @author Adrian Helberg
 * @author Maximilian Janzen
 */
public class EdmondsKarp extends MaxFlowAlgorithm {
	// Marking nodes
	private HashMap<Integer, Integer> _nodeFlags;

	public EdmondsKarp(MultiGraph graph) {
		super(graph);
		_nodeFlags = new HashMap<>();
	}

	/**
	 * Find path from source to sink node and return minimum flow
	 * @param sourceNode Source node
	 * @param sinkNode Sink node
	 * @return Minimum flow
	 */
	protected int findPath(Node sourceNode, Node sinkNode) {
		// Prepare
		_path.clear();
		_nodeFlags.clear();
		
		flagNodes(sourceNode, sinkNode);

		// Set minimum flow to pseudo infinity initially
		int minFlow = Integer.MAX_VALUE;

		if (_nodeFlags.containsKey(sinkNode.getIndex())) {
			int indexSinkNode = sinkNode.getIndex();
			int flagCurrentNode = _nodeFlags.get(indexSinkNode);
			Node currentNode = sinkNode;
			int indexCurrentNode = indexSinkNode;
			int indexPreviousNode;
			Node previousNode;

			// Add sink node
			_path.addLast(indexSinkNode);

			// Process until source node is reached
			while (!currentNode.equals(sourceNode)) {
				// Iterate incoming edges
				for (Edge edge : currentNode.getEachEnteringEdge()) {

					previousNode = edge.getOpposite(currentNode);
					indexPreviousNode = previousNode.getIndex();

					// Is previous node flagged and is there free capacity?
					if (_nodeFlags.containsKey(indexPreviousNode)
							&& _nodeFlags.get(indexPreviousNode) == (flagCurrentNode - 1)
							&& (_capacities[indexPreviousNode][indexCurrentNode] - _flows[indexPreviousNode][indexCurrentNode]) > 0) {

						// Build up path
						_path.addLast(indexPreviousNode);

						// Calculate minimum flow
						minFlow = Math.min(minFlow, _capacities[indexPreviousNode][indexCurrentNode] - _flows[indexPreviousNode][indexCurrentNode]);

						indexCurrentNode = indexPreviousNode;
						currentNode = previousNode;
						flagCurrentNode = _nodeFlags.get(indexPreviousNode);

						break;
					}
				}
			}
		}
		if (minFlow == Integer.MAX_VALUE) {
			minFlow = 0;
		}
		return minFlow;
	}
	
	/**
	 * Set number of steps for source node to reach sink node
	 * @param sourceNode Source node
	 * @param sinkNode Sink node
	 */
	private void flagNodes(Node sourceNode, Node sinkNode) {
		int index = 0;
		_nodeFlags.put(sourceNode.getIndex(), index);
		flagNodes(sourceNode, sinkNode, index + 1);
	}
	
	/**
	 * Flag nodes
	 * @param currentNode Ausgangsknoten
	 * @param sinkNode Senke
	 * @param index Index des Ausgangsknoten
	 */
	private void flagNodes(Node currentNode, Node sinkNode, int index) {
		Node oppositeNode;
		int indexOppositeNode;
		// Node index to evaluate
		int indexEvaluationNode = currentNode.getIndex();

		// Iterate outgoing edges
		for (Edge edge : currentNode.getLeavingEdgeSet()) {
			oppositeNode = edge.getOpposite(currentNode);
			indexOppositeNode = oppositeNode.getIndex();

			// If there is capacity free and (node not flagged or flag greater index)
			if (_capacities[indexEvaluationNode][indexOppositeNode] - _flows[indexEvaluationNode][indexOppositeNode] > 0
					&& (!_nodeFlags.containsKey(indexOppositeNode)
						|| _nodeFlags.get(indexOppositeNode) > index)) {

				_nodeFlags.put(indexOppositeNode, index);

				// Sink node reached
				if (oppositeNode.equals(sinkNode)) break;

				flagNodes(oppositeNode, sinkNode, index + 1);
			}
		}
	}
}
