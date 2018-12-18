package application.algorithm;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.Path;
import org.graphstream.graph.implementations.MultiGraph;

/**
 * GKAP Graph
 * @author Maximilian Janzen & Adrian Helberg
 */
public class Graph {
	// Properties
	static final String uiStyle = "ui.style";
	private static final String uiColorRed = "fill-color: red;";
	static final String uiColorblue = "fill-color: blue;";
	static final String uiName = "ui.label";
	static final String index = "i";
	public static final String distance = "distance";
	static final String prevNode = "prevNode";
	static final String walkTrough = "walkTrough";
	private MultiGraph graph;

	public Graph(String name) {
		graph = new MultiGraph(name);
		graph.setStrict(false);
		graph.setAutoCreate(true);
	}

	public Graph(String name, int nodes, int edges, boolean bigGraph) {
		this(name);

		if (!bigGraph) {
			if (nodes > 20) { nodes = 20; }
		}

		if (edges > (nodes * (nodes - 1)) / 2) { 
			edges = (nodes * (nodes - 1)) / 2;
		}

		String nodeName;
		ArrayList<Node> nodeList = new ArrayList<>();

		for (int i = 1; i <= nodes; i++) {
			nodeName = "" + i;
			graph.addNode(nodeName);
			Node node = graph.getNode(nodeName);
			node.addAttribute(Graph.uiName, nodeName);
			node.addAttribute(Graph.uiStyle, Graph.uiColorblue);
			nodeList.add(node);
		}

		Random randomWeight = new Random();
		Random randomNode = new Random();
		Node from;
		Node to;
		final boolean directed = true;
		int edgeWeight;
		String edgeWeightString;

		for (int i = 0; i < edges; i++) {
			from = nodeList.get(randomNode.nextInt(nodeList.size()));
			to = nodeList.get(randomNode.nextInt(nodeList.size()));

			if (!(from.hasEdgeBetween(to))) {
				edgeWeight = randomWeight.nextInt(edges * 5);
				edgeWeightString = "" + edgeWeight;
				graph.addEdge(edgeWeightString, from, to, directed);
				Edge edge = graph.getEdge(edgeWeightString);
				edge.addAttribute(Graph.distance, edgeWeight);
				edge.addAttribute(Graph.uiStyle, Graph.uiColorblue);
				edge.addAttribute(Graph.walkTrough, true);
			}
		}
	}

	public MultiGraph getMultiGraph() {
		return graph;
	}

	public MultiGraph makeGraph(List<Path> pathList) {
		if (pathList == null) return graph;

		for (Path path : pathList) {
			visualizeShortestPath(path);
		}

		return graph;
	}

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
