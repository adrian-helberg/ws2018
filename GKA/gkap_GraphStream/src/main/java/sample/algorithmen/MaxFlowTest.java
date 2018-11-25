package sample.algorithmen;

import static org.junit.Assert.*;
import java.io.IOException;

import org.graphstream.graph.Edge;
import org.graphstream.graph.implementations.MultiGraph;
import org.junit.Test;
import sample.exception.InvalidGraphException;

/**
 * @author Maximilian Janzen & Adrian Helberg
 */
public class MaxFlowTest {

	@Test
	public void testSmallGraph() {
		Graph graph = new Graph("graph", 20, 150, false);
		String source = "2", sink = "18";
		MultiGraph multiGraph = graph.getMultiGraph();
		for (Edge edge : multiGraph.getNode(source).getEachEnteringEdge()) {
			multiGraph.removeEdge(edge);
		}
		for (Edge edge : multiGraph.getNode(sink).getEachLeavingEdge()) {
			multiGraph.removeEdge(edge);
		}
		try {
			FordFulkerson fordFulkerson = new FordFulkerson(multiGraph);
			EdmondsKarp edmondsKarp = new EdmondsKarp(multiGraph);
			int fordFulkersonFlow = 0;
			int edmondsKarpFlow = 0;
			System.out.println("fordfulkerson_small_graph");
			fordFulkersonFlow = fordFulkerson.getMaxFlow(source, sink);
			System.out.println("edmundskarp_small_graph");
			edmondsKarpFlow = edmondsKarp.getMaxFlow(source, sink);
			System.out.println("FordFulkerson-flow: " + fordFulkersonFlow + " in: " + fordFulkerson.getStringTimeMilliseconds() + ", " + fordFulkerson.getStringTimeNanoseconds() +
					"\nEdmondsKarp-flow: " + edmondsKarpFlow + " in: " + edmondsKarp.getStringTimeMilliseconds() + ", " + edmondsKarp.getStringTimeNanoseconds() + "\n");
			assertTrue(fordFulkersonFlow > 0);
			assertTrue(edmondsKarpFlow > 0);
			assertEquals(fordFulkersonFlow, edmondsKarpFlow);
		} catch (InvalidGraphException e) {
			System.out.println("Fehler im Algorithmus!" + e.getMessage());
			e.printStackTrace();
		}
		
	}

	@Test
	public void testGraph4() {
		Graph graph = new Graph("graph");
		Parser parser = new Parser(graph.getMultiGraph());
		MultiGraph multiGraph = null;
		try {
			multiGraph = parser.parseFromFile("graph04.gka");
		} catch (IOException e) {
			System.out.println("Die Datei graph04.gka existiert nicht!" + e.getMessage());
			e.printStackTrace();
		}
		String source = "q", sink = "s";
		
		try {
			FordFulkerson fordFulkerson = new FordFulkerson(multiGraph);
			EdmondsKarp edmondsKarp = new EdmondsKarp(multiGraph);
			int fordFulkersonFlow = 0;
			int edmondsKarpFlow = 0;
			System.out.println("fordfulkerson_graph04");
			fordFulkersonFlow = fordFulkerson.getMaxFlow(source, sink);
			System.out.println("edmondskarp_graph04");
			edmondsKarpFlow = edmondsKarp.getMaxFlow(source, sink);
			System.out.println("FordFulkerson-flow: " + fordFulkersonFlow + " in: " + fordFulkerson.getStringTimeMilliseconds() + ", " + fordFulkerson.getStringTimeNanoseconds() +
					"\nEdmondsKarp-flow: " + edmondsKarpFlow + " in: " + edmondsKarp.getStringTimeMilliseconds() + ", " + edmondsKarp.getStringTimeNanoseconds() + "\n");
			assertTrue(fordFulkersonFlow > 0);
			assertTrue(edmondsKarpFlow > 0);
			assertEquals(fordFulkersonFlow, edmondsKarpFlow);
		} catch (InvalidGraphException e) {
			System.out.println("Fehler im Algorithmus!" + e.getMessage());
			e.printStackTrace();
		}
		
	}
	
	@Test
	public void testNetworkGraph() {
		Graph graph = new Graph("graph", 50, 800, true);
		String source = "1", sink = "50";
		MultiGraph multiGraph = graph.getMultiGraph();
		for (Edge edge : multiGraph.getNode(source).getEachEnteringEdge()) {
			multiGraph.removeEdge(edge);
		}
		for (Edge edge : multiGraph.getNode(sink).getEachLeavingEdge()) {
			multiGraph.removeEdge(edge);
		}
		
		try {
			FordFulkerson fordFulkerson = new FordFulkerson(multiGraph);
			EdmondsKarp edmondsKarp = new EdmondsKarp(multiGraph);
			int fordFulkersonFlow = 0;
			int edmondsKarpFlow = 0;
			System.out.println("fordfulkerson_big_net");
			fordFulkersonFlow = fordFulkerson.getMaxFlow(source, sink);
			System.out.println("edmondskarp_big_net");
			edmondsKarpFlow = edmondsKarp.getMaxFlow(source, sink);
			System.out.println("FordFulkerson-flow: " + fordFulkersonFlow + " in: " + fordFulkerson.getStringTimeMilliseconds() + ", " + fordFulkerson.getStringTimeNanoseconds() +
					"\nEdmondsKarp-flow: " + edmondsKarpFlow + " in: " + edmondsKarp.getStringTimeMilliseconds() + ", " + edmondsKarp.getStringTimeNanoseconds() + "\n");
			assertTrue(fordFulkersonFlow > 0);
			assertTrue(edmondsKarpFlow > 0);
			assertEquals(fordFulkersonFlow, edmondsKarpFlow);
		} catch (InvalidGraphException e) {
			System.out.println("Fehler im Algorithmus!" + e.getMessage());
			e.printStackTrace();
		}
		
	}
	
	@Test
	public void testVeryBigGraph() {
		Graph graph = new Graph("graph", 2000, 150000, true);
		String source = "1", sink = "50";
		MultiGraph multiGraph = graph.getMultiGraph();
		for (Edge edge : multiGraph.getNode(source).getEachEnteringEdge()) {
			multiGraph.removeEdge(edge);
		}
		for (Edge edge : multiGraph.getNode(sink).getEachLeavingEdge()) {
			multiGraph.removeEdge(edge);
		}
		try {
			FordFulkerson fordFulkerson = new FordFulkerson(multiGraph);
			EdmondsKarp edmondsKarp = new EdmondsKarp(multiGraph);
			int fordFulkersonFlow = 0;
			int edmondsKarpFlow = 0;
			System.out.println("fordfulkerson_very_big");
			fordFulkersonFlow = fordFulkerson.getMaxFlow(source, sink);
//			System.out.println("edmondskarp_very_big");
//			edmondsKarpFlow = edmondsKarp.getMaxFlow(source, sink);
			System.out.println("FordFulkerson-flow: " + fordFulkersonFlow + " in: " + fordFulkerson.getStringTimeMilliseconds() + ", " + fordFulkerson.getStringTimeNanoseconds() +
					"\nEdmondsKarp-flow: " + edmondsKarpFlow + " in: " + edmondsKarp.getStringTimeMilliseconds() + ", " + edmondsKarp.getStringTimeNanoseconds() + "\n");
			assertTrue(fordFulkersonFlow > 0);
//			assertTrue(edmondsKarpFlow > 0);
//			assertEquals(fordFulkersonFlow, edmondsKarpFlow);
		} catch (InvalidGraphException e) {
			System.out.println("Fehler im Algorithmus!" + e.getMessage());
			e.printStackTrace();
		}
		
	}
	
	/**
	 * Erstellt einen kleinen Multigraph
	 * @return Graph
	 */
//	private MultiGraph getSmallMultiGraph() {
//		MultiGraph graph = new MultiGraph("Graph");
//		graph.addNode("q");
//		graph.addNode("b");
//		graph.addNode("s");
//		graph.addNode("c");
//		boolean directed = false;
//		graph.addEdge("qb", "q", "b", directed);
//		graph.addEdge("bs", "b", "s", directed);
//		graph.addEdge("qs", "q", "s", directed);
//		graph.addEdge("sq", "s", "q", directed);
//		graph.addEdge("qc", "q", "c", directed);
//		graph.addEdge("cs", "c", "s", directed);
//		
//		for (Edge edge : graph.getEdgeSet()) {
//			edge.setAttribute(Graph.distance, 2);
//		}
//		graph.getEdge("qs").setAttribute(Graph.distance, 5);
//		graph.getEdge("sq").setAttribute(Graph.distance, 5);
//		graph.getEdge("cs").setAttribute(Graph.distance, 1);
////		graph.getEdge("sc").setAttribute(Graph.distance, 1);
//		return graph;
//	}
}
