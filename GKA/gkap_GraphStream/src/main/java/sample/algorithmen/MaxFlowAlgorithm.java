package sample.algorithmen;

import java.util.ArrayList;
import java.util.LinkedList;

import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.implementations.MultiGraph;

import sample.exception.InvalidGraphException;

public abstract class MaxFlowAlgorithm extends Algorithm {
	
	protected int[][] _capacities;
	protected int[][] _flows;
	protected ArrayList<Node> _inspectedNodes;
	protected LinkedList<Integer> _path;
	
	protected MaxFlowAlgorithm(MultiGraph graph) throws InvalidGraphException {
		if (graph == null) {
			throw new InvalidGraphException("Es wurde kein Graph uebergeben!");
		}
		_graph = graph;
		int nodeCount = graph.getNodeCount();
		_capacities = new int[nodeCount][nodeCount];
		_flows = new int[nodeCount][nodeCount];
//		for (int i = 0; i < nodeCount; i++) {
//			for (int j = 0; j < nodeCount; j++) {
//				_flows[i][j] = 0;
//				_capacities[i][j] = Integer.MIN_VALUE;
//			}
//		}
		int i = 0, j = 0;
		for (Edge edge : graph.getEachEdge()) {
			i = edge.getSourceNode().getIndex();
			j = edge.getTargetNode().getIndex();
			_capacities[i][j] = (int) edge.getAttribute(Graph.distance);
			if (!edge.isDirected()) {
				_capacities[j][i] = (int) edge.getAttribute(Graph.distance);
			}
		}
		_inspectedNodes = new ArrayList<>();
		_path = new LinkedList<>();
	}
	
	/**
	 * Der Algorithmus ermittelt den maximalen Durchfluss von einer Quelle zur Senke
	 * @param Source Quellknoten
	 * @param sink Senkeknoten
	 * @return Maximal moeglicher Durchfluss
	 */
	public int getMaxFlow(String source, String sink) throws InvalidGraphException {
		checkNodes(source, sink);
		startTimer();
		int maxFlow = 0;
		Node sourceNode = _graph.getNode(source);
		Node sinkNode = _graph.getNode(sink);
		int minCurrentFlow = 0;
		int indexSource = 0, indexTarget = 0;
		// Solange es einen freien Pfad zur Senke gibt
		while ((minCurrentFlow = findPath(sourceNode, sinkNode)) > 0) {
			for (int i = 1; i < _path.size(); i++) { // Pfad durchlaufen
				indexSource = _path.get(i - 1);
				indexTarget = _path.get(i);
				// Der Pfad l�uft von Senke zu Quelle, Flow entsprechend setzen
				_flows[indexTarget][indexSource] = (_flows[indexTarget][indexSource] + minCurrentFlow);
				_flows[indexSource][indexTarget] = (_flows[indexSource][indexTarget] - minCurrentFlow);
			}
			maxFlow += minCurrentFlow;
		}
		stopTimer();
		return maxFlow;
	}
	
	abstract protected int findPath(Node source, Node sink);
}
