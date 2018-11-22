package sample.algorithmen;

import java.util.HashMap;

import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.implementations.MultiGraph;

import sample.exception.InvalidGraphException;
/**
 * For a more high level description, see Ford�Fulkerson algorithm.
 * Quelle: https://en.wikipedia.org/wiki/Edmonds%E2%80%93Karp_algorithm
 *
 */
public class EdmondsKarp extends MaxFlowAlgorithm {
	
	private HashMap<Integer, Integer> _nodeFlags; // Speichert Markierungen und zugehoerige Knoten

	public EdmondsKarp(MultiGraph graph) throws InvalidGraphException {
		super(graph);
		_nodeFlags = new HashMap<>();
	}

	protected int findPath(Node sourceNode, Node sinkNode) {
		// Variablen vorbereiten
		_path.clear();
		_nodeFlags.clear();
		
		flagNodes(sourceNode, sinkNode);
		
		int minFlow = Integer.MAX_VALUE;
		if (_nodeFlags.containsKey(sinkNode.getIndex())) { // Gibt es einen freien Pfad zur Senke
			int indexSinkNode = sinkNode.getIndex();
			int flagCurrentNode = _nodeFlags.get(indexSinkNode);
			Node currentNode = sinkNode;
			int indexCurrentNode = indexSinkNode;
			_path.addLast(indexSinkNode); // Senke hinzufuegen
			int indexPreviousNode = 0;
			Node previousNode = null;
			while (!currentNode.equals(sourceNode)) { // Solange Quellknoten nicht erreicht
				for (Edge edge : currentNode.getEachEnteringEdge()) { // Jede Kante
					previousNode = edge.getOpposite(currentNode);
					indexPreviousNode = previousNode.getIndex();
					// pruefen ob Vorgaengerknoten geflagt und Kapazitaet frei
					if (_nodeFlags.containsKey(indexPreviousNode) && _nodeFlags.get(indexPreviousNode) == (flagCurrentNode - 1) &&
							(_capacities[indexPreviousNode][indexCurrentNode] - _flows[indexPreviousNode][indexCurrentNode]) > 0) {
						_path.addLast(indexPreviousNode); // Pfad weiter aufbauen
						minFlow = Math.min(minFlow, _capacities[indexPreviousNode][indexCurrentNode] - _flows[indexPreviousNode][indexCurrentNode]);
						indexCurrentNode = indexPreviousNode; // Werte neu setzen
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
	 * Speichert fuer Knoten den Index in wievielen Schritten der Knoten erreichbar ist
	 * @param sourceNode Quelle
	 * @param sinkNode Senke
	 */
	private void flagNodes(Node sourceNode, Node sinkNode) {
		int index = 0;
		_nodeFlags.put(sourceNode.getIndex(), index);
		flagNodes(sourceNode, sinkNode, index + 1);
	}
	
	/**
	 * Hilfsmethode zum speichern der Indizes fuer Knoten
	 * @param currentNode Ausgangsknoten
	 * @param sinkNode Senke
	 * @param index Index des Ausgangsknoten
	 */
	private void flagNodes(Node currentNode, Node sinkNode, int index) {
		Node oppositeNode = null;
		int indexEvaluationNode = currentNode.getIndex();
		int indexOppositeNode = 0;
		for (Edge edge : currentNode.getLeavingEdgeSet()) {
			oppositeNode = edge.getOpposite(currentNode);
			indexOppositeNode = oppositeNode.getIndex();
			// Falls Kapazitaet frei ist und (Knoten nicht geflaggt oder flag groesser als index)
			if (_capacities[indexEvaluationNode][indexOppositeNode] - _flows[indexEvaluationNode][indexOppositeNode] > 0 &&
					(!_nodeFlags.containsKey(indexOppositeNode) || _nodeFlags.get(indexOppositeNode) > index)) {
				_nodeFlags.put(indexOppositeNode, index);
				if (oppositeNode.equals(sinkNode)) {
					break;
				}
				flagNodes(oppositeNode, sinkNode, index + 1);
			}
		}
	}
}
