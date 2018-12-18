package application.algorithm;

import java.util.HashMap;
import application.exception.InvalidGraphException;
import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.implementations.MultiGraph;

/**
 * GKAP Edmond & Karp
 *
 * @author Adrian Helberg
 * @author Maximilian Janzen
 * @see <a href="https://en.wikipedia.org/wiki/Edmonds%E2%80%93Karp_algorithm">Edmond & Karp</a>
 */
public class EdmondsKarp extends MaxFlowAlgorithm {
/*	// Markers and associated nodes
	private HashMap<Integer, Integer> _nodeFlags;

	public EdmondsKarp(MultiGraph graph) {
		super(graph);
		_nodeFlags = new HashMap<>();
	}

	protected int findPath(Node sourceNode, Node sinkNode) {
		// Initial
		_path.clear();
		_nodeFlags.clear();
		
		flagNodes(sourceNode, sinkNode);
		
		int minFlow = Integer.MAX_VALUE;

		if (_nodeFlags.containsKey(sinkNode.getIndex())) {
			// There is a path to sink
			int indexSinkNode = sinkNode.getIndex();
			int flagCurrentNode = _nodeFlags.get(indexSinkNode);

			Node currentNode = sinkNode;
			int indexCurrentNode = indexSinkNode;

			// Add sink
			_path.addLast(indexSinkNode);
			int indexPreviousNode;

			Node previousNode;
			while (!currentNode.equals(sourceNode)) {
				// Source node not reached yet
				for (Edge edge : currentNode.getEachEnteringEdge()) {

					previousNode = edge.getOpposite(currentNode);
					indexPreviousNode = previousNode.getIndex();
					// Previous node marked and capacity free?
					if (_nodeFlags.containsKey(indexPreviousNode) && _nodeFlags.get(indexPreviousNode) == (flagCurrentNode - 1) &&
							(_capacities[indexPreviousNode][indexCurrentNode] - _flows[indexPreviousNode][indexCurrentNode]) > 0) {

						_path.addLast(indexPreviousNode);

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

	*//**
	 * Set the number of steps to reach the sink
	 *
	 * @param sourceNode Source node
	 * @param sinkNode Target node
	 *//*
	private void flagNodes(Node sourceNode, Node sinkNode) {
		int index = 0;
		_nodeFlags.put(sourceNode.getIndex(), index);
		flagNodes(sourceNode, sinkNode, index + 1);
	}

	*//**
	 * Recursive node flagging
	 *
	 * @param currentNode Current node
	 * @param sinkNode Target node
	 * @param index Node index
	 *//*
	private void flagNodes(Node currentNode, Node sinkNode, int index) {
		Node oppositeNode;
		int indexEvaluationNode = currentNode.getIndex();
		int indexOppositeNode;

		for (Edge edge : currentNode.getLeavingEdgeSet()) {

			oppositeNode = edge.getOpposite(currentNode);
			indexOppositeNode = oppositeNode.getIndex();
			// Check marks and capacities
			if (_capacities[indexEvaluationNode][indexOppositeNode] - _flows[indexEvaluationNode][indexOppositeNode] > 0
					&& (
							!_nodeFlags.containsKey(indexOppositeNode) || _nodeFlags.get(indexOppositeNode) > index)
					)
			{
				_nodeFlags.put(indexOppositeNode, index);

				// Recursion break
				if (oppositeNode.equals(sinkNode)) {
					break;
				}

				flagNodes(oppositeNode, sinkNode, index + 1);
			}
		}
	}*/
}
