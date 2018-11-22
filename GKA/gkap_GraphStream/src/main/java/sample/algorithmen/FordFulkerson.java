package sample.algorithmen;

import java.util.HashMap;
import java.util.HashSet;

import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.implementations.MultiGraph;

import sample.exception.InvalidGraphException;

public class FordFulkerson extends MaxFlowAlgorithm {
	
	private HashSet<Integer> _unSettledNodes; // Markierte, noch nicht inspizierte Knoten
	private HashSet<Integer> _settledNodes; // Inspizierte Knoten
	private HashMap<Integer, Integer> _previousNodes; // Vorgaengerknoten von Knoten
	
	public FordFulkerson(MultiGraph graph) throws InvalidGraphException {
		super(graph);
		_unSettledNodes = new HashSet<>();
		_settledNodes = new HashSet<>();
		_previousNodes = new HashMap<>();
	}
	
	protected int findPath(Node source, Node sink) {
		// Variablen vorbereiten
		_path.clear();
		_unSettledNodes.clear();
		_settledNodes.clear();
		_previousNodes.clear();
		
		// Pfad finden
		_unSettledNodes.add(source.getIndex());
		int evaluationNode = 0;
		while (!_unSettledNodes.isEmpty()) { // Solange Knoten inspizieren,
			evaluationNode = getNode();
			evaluateNeighbors(_graph.getNode(evaluationNode)); // bis alle gefundenen
			_unSettledNodes.remove(evaluationNode); // inspiziert wurden
			_settledNodes.add(evaluationNode);
		}
		
		// Moegliche Flussstaerke ermitteln
		int minFlow = Integer.MAX_VALUE;
		if (_previousNodes.containsKey(sink.getIndex())) { // Wurde die Senke gefunden?
			int indexCurrentNode = sink.getIndex();
			_path.addLast(indexCurrentNode); // Pfad rueckwaerts aufbauen
			int indexPreviousNode = 0;
			while (indexCurrentNode != source.getIndex()) { // Bis der Startknoten erreicht ist
				indexPreviousNode = _previousNodes.get(indexCurrentNode); // Vorgaengerknoten
				_path.addLast(indexPreviousNode); // zum Pfad hinzufuegen
				// Bisherigen moeglichen Fluss mit aktuell moeglichem Fluss vergleichen
				minFlow = Math.min(minFlow, _capacities[indexPreviousNode][indexCurrentNode] - _flows[indexPreviousNode][indexCurrentNode]);
				indexCurrentNode = indexPreviousNode;
			}
		} else {
			minFlow = 0;
		}
		return minFlow;
	}

	/**
	 * Ermittelt erreichbare Knoten von einem Knoten
	 * @param evaluationNode
	 */
	private void evaluateNeighbors(Node evaluationNode) {
		int indexEvaluationNode = evaluationNode.getIndex();
		int indexOppositeNode = 0;
		for (Edge edge : evaluationNode.getLeavingEdgeSet()) {
			indexOppositeNode = edge.getOpposite(evaluationNode).getIndex();
			if (!_settledNodes.contains(indexOppositeNode) && !_unSettledNodes.contains(indexOppositeNode) &&
					_capacities[indexEvaluationNode][indexOppositeNode] - _flows[indexEvaluationNode][indexOppositeNode] > 0) {
				_previousNodes.put(indexOppositeNode, indexEvaluationNode);
				_unSettledNodes.add(indexOppositeNode);
			}
		}
	}

	/**
	 * Gibt einem noch nicht inspizierten Knoten zurueck
	 * @return int
	 */
	private int getNode() {
		for (int nodeIndex : _unSettledNodes) {
			return nodeIndex;
		}
		return 0;
	}
}
