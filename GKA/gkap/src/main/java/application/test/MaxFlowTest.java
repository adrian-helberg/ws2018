package application.test;

import static org.junit.Assert.*;
import java.io.IOException;
import application.algorithm.Graph;
import application.algorithm.Parser;
import application.exception.InvalidGraphException;
import org.graphstream.graph.Edge;
import org.graphstream.graph.implementations.MultiGraph;
import org.junit.Test;

/**
 * Max flow algorithm test suite
 * @author Maximilian Janzen & Adrian Helberg
 */
@SuppressWarnings("Duplicates")
public class MaxFlowTest {

	@Test
	public void testSmallGraph() {
		// Prepare test graph
		Graph graph = new Graph("graph", 20, 150, false);
		String source = "2", sink = "18";
		MultiGraph multiGraph = graph.getMultiGraph();

		// Remove incoming edges from source node
		for (Edge edge : multiGraph.getNode(source).getEachEnteringEdge()) {
			multiGraph.removeEdge(edge);
		}
		// Remove outgoing edges from sink node
		for (Edge edge : multiGraph.getNode(sink).getEachLeavingEdge()) {
			multiGraph.removeEdge(edge);
		}

		try {
			FordFulkerson fordFulkerson = new FordFulkerson(multiGraph);
			EdmondsKarp edmondsKarp = new EdmondsKarp(multiGraph);

			int fordFulkersonFlow = fordFulkerson.getMaxFlow(source, sink);
			int edmondsKarpFlow = edmondsKarp.getMaxFlow(source, sink);

			assertTrue(fordFulkersonFlow > 0);
			assertTrue(edmondsKarpFlow > 0);
			assertEquals(fordFulkersonFlow, edmondsKarpFlow);

		} catch (InvalidGraphException e) {
			e.printStackTrace();
		}
	}

	@Test
	public void testGraph4() {
		// Prepare test graph
		Graph graph = new Graph("graph");
		Parser parser = new Parser(graph.getMultiGraph());
		MultiGraph multiGraph = null;

		try {
			multiGraph = parser.parseFromFile("graph04.gka");
		} catch (IOException e) {
			e.printStackTrace();
		}

		String source = "q", sink = "s";
		
		try {
			FordFulkerson fordFulkerson = new FordFulkerson(multiGraph);
			EdmondsKarp edmondsKarp = new EdmondsKarp(multiGraph);

			int fordFulkersonFlow = fordFulkerson.getMaxFlow(source, sink);
			int edmondsKarpFlow = edmondsKarp.getMaxFlow(source, sink);

			assertTrue(fordFulkersonFlow > 0);
			assertTrue(edmondsKarpFlow > 0);
			assertEquals(fordFulkersonFlow, edmondsKarpFlow);

		} catch (InvalidGraphException e) {
			e.printStackTrace();
		}
	}
	
	@Test
	public void testNetworkGraph() {
		// Prepare test graph
		Graph graph = new Graph("graph", 50, 800, true);
		String source = "1", sink = "50";
		MultiGraph multiGraph = graph.getMultiGraph();

		// Remove incoming edges from source node
		for (Edge edge : multiGraph.getNode(source).getEachEnteringEdge()) {
			multiGraph.removeEdge(edge);
		}
		// Remove outgoing edges from sink node
		for (Edge edge : multiGraph.getNode(sink).getEachLeavingEdge()) {
			multiGraph.removeEdge(edge);
		}
		
		try {
			FordFulkerson fordFulkerson = new FordFulkerson(multiGraph);
			EdmondsKarp edmondsKarp = new EdmondsKarp(multiGraph);

			int fordFulkersonFlow = fordFulkerson.getMaxFlow(source, sink);
			int edmondsKarpFlow = edmondsKarp.getMaxFlow(source, sink);

			assertTrue(fordFulkersonFlow > 0);
			assertTrue(edmondsKarpFlow > 0);
			assertEquals(fordFulkersonFlow, edmondsKarpFlow);

		} catch (InvalidGraphException e) {
			e.printStackTrace();
		}
	}
	
	@Test
	public void testVeryBigGraph() {
		// Prepare test graph
		Graph graph = new Graph("graph", 500, 20000, true);
		String source = "1", sink = "50";
		MultiGraph multiGraph = graph.getMultiGraph();

		// Remove incoming edges from source node
		for (Edge edge : multiGraph.getNode(source).getEachEnteringEdge()) {
			multiGraph.removeEdge(edge);
		}
		// Remove outgoing edges from sink node
		for (Edge edge : multiGraph.getNode(sink).getEachLeavingEdge()) {
			multiGraph.removeEdge(edge);
		}

		try {
			FordFulkerson fordFulkerson = new FordFulkerson(multiGraph);
			EdmondsKarp edmondsKarp = new EdmondsKarp(multiGraph);

			int fordFulkersonFlow = fordFulkerson.getMaxFlow(source, sink);
			int edmondsKarpFlow = edmondsKarp.getMaxFlow(source, sink);

			assertTrue(fordFulkersonFlow > 0);
			assertTrue(edmondsKarpFlow > 0);
			assertEquals(fordFulkersonFlow, edmondsKarpFlow);

		} catch (InvalidGraphException e) {
			e.printStackTrace();
		}
	}
}
