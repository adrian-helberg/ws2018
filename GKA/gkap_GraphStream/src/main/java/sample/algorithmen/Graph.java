package sample.algorithmen;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.Path;
import org.graphstream.graph.implementations.MultiGraph;

public class Graph {

	// Attributbezeichner fuer den Stil eines Pfades
	public static final String uiStyle = "ui.style";

	// Markierung von Knoten und Pfaden
	public static final String uiColorRed = "fill-color: red;";
	public static final String uiColorblue = "fill-color: blue;";

	// Gewichte fuer Knoten und Kanten
	public static final String weight = "weight";

	// Name von Knoten und Kanten
	public static final String uiName = "ui.label";

	// Index von Knoten und Kanten
	public static final String index = "i";

	// Distanz zwischen beliebigen Knoten
	public static final String distance = "distance";
	
	// Vorgaengerknoten in einem Graphen
	public static final String prevNode = "prevNode";
	
	// Maximal moeglicher Durchfluss
	public static final String maxFlow = "maxFlow";
	
	// Aktueller Wert eines Durchflusses
	public static final String currentFlow = "currentFlow";
	
	// Boolean, ist die Flussrichtung des vorigen knotens positiv?
	public static final String prevNodePositive = "prevNodePositive";
	
	// Darf die Kante passiert werden?
	public static final String walkTrough = "walkTrough";

	private MultiGraph _graph;

	public Graph(String name) { //TODO SCHNITTSTELLE HIER WIRD DER GRAPH GELADEN
		_graph = new MultiGraph(name);
		_graph.setStrict(false); // Weist doppelte Eingabe von Knoten nicht ab
		_graph.setAutoCreate(true); // Knoten automatisch mit Kanten hinzufuegen
	}
	
	public Graph(String name, int nodes, int edges, boolean bigGraph) {
		this(name);
		if (!bigGraph) {
			if (nodes > 20) { nodes = 20; }
		}
		if (edges > (nodes * (nodes - 1)) / 2) { 
			edges = (nodes * (nodes - 1)) / 2;
		}
		String nodeName = "";
		ArrayList<Node> nodeList = new ArrayList<>();
		for (int i = 1; i <= nodes; i++) {
			nodeName = "" + i;
			_graph.addNode(nodeName);
			Node node = _graph.getNode(nodeName);
			node.addAttribute(Graph.uiName, nodeName);
			node.addAttribute(Graph.uiStyle, Graph.uiColorblue);
			nodeList.add(node);
		}
		Random randomWeight = new Random(); // Nicht zu gro�es Intervall f�r die Kantengewichtung
													// damit mehrere k�rzeste Pfade erstellt werden k�nnte.
		Random randomNode = new Random();
		Node from;
		Node to;
		final boolean directed = true;
		int edgeWeight = 0;
		String edgeWeightString = "";
		for (int i = 0; i < edges; i++) {
			from = nodeList.get(randomNode.nextInt(nodeList.size()));
			to = nodeList.get(randomNode.nextInt(nodeList.size()));
			if (!(from.hasEdgeBetween(to))) {
				edgeWeight = randomWeight.nextInt(edges * 5);
				edgeWeightString = "" + edgeWeight;
				_graph.addEdge(edgeWeightString, from, to, directed);
				Edge edge = _graph.getEdge(edgeWeightString);
				edge.addAttribute(Graph.distance, edgeWeight);
				edge.addAttribute(Graph.uiStyle, Graph.uiColorblue);
				edge.addAttribute(Graph.walkTrough, true);
			}
		}
	}

	public MultiGraph getMultiGraph() {
		return _graph;
	}
	
	public void setMultiGraph(MultiGraph graph) {
		_graph = graph;
	}

	public static void printPathEdges(Path path) {
		System.out.println("Pfadlaenge: " + path.getEdgeCount());
		for (Edge edge : path.getEdgePath()) {
			System.out.println(edge.getId());
		}
	}
//
	public void showGraph(List<Path> pathList) {
		for (Path path : pathList) {
			visualizeShortestPath(path);
			Graph.printPathEdges(path);
		}
		_graph.display();
	}
	public MultiGraph makeGraph(List<Path> pathList) {
		for (Path path : pathList) {
			visualizeShortestPath(path);
			Graph.printPathEdges(path);
		}
		//_graph.display();
		return _graph;
	}


	/**
	 * Markiert den kuerzesten Pfad durch den Graphen
	 * 
	 * @param path
	 *            Ein Pfad
	 */
	private void visualizeShortestPath(Path path) {
		List<Edge> edgePath = path.getEdgePath();
		List<Node> nodePath = path.getNodePath();
		if (nodePath.size() == 1) {
			nodePath.get(0).addAttribute(Graph.uiStyle, Graph.uiColorRed);
		}
		for (Edge edge : edgePath) {
			edge.addAttribute(Graph.uiStyle, Graph.uiColorRed);
			edge.getNode0().addAttribute(Graph.uiStyle, Graph.uiColorRed);
			edge.getNode1().addAttribute(Graph.uiStyle, Graph.uiColorRed);
		}
	}
}
