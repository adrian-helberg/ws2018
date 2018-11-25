package application.algorithm;

import static org.junit.Assert.*;
import java.io.IOException;
import java.util.regex.Pattern;
import org.graphstream.graph.Edge;
import org.graphstream.graph.implementations.MultiGraph;
import org.junit.Before;
import org.junit.Test;

/**
 * GKAP Graph Test
 * @author Maximilian Janzen & Adrian Helberg
 */
public class GraphTest {

	private Graph graph;
	private MultiGraph multiGraph;
	private Parser parser;
	@Before
	public void before() {
		graph = new Graph("graph");
		multiGraph = graph.getMultiGraph();
		parser = new Parser(multiGraph);
	}
	
	@Test
	public void testPattern() {
		assertTrue(Pattern.matches("([a-zA-Z_0-9]+"
				+ "(((--|->)[a-zA-Z_0-9]+){0,1}"
				+ "([(][a-zA-Z_0-9]+[)]){0,1}"
				+ "(:[0-9]+){0,1}))", "12abc123->asf(abc):1000"));
	}

	@Test
	public void testFileGraph01() {
		try {
			multiGraph = parser.parseFromFile("graph01.gka");
		} catch (IOException e) {
			e.printStackTrace();
		}
		assertEquals(multiGraph.getNodeCount(), 14);
		assertEquals(multiGraph.getEdgeCount(), 39);
		assertEquals(multiGraph.getNode("f").getEdgeSet().size(), 4);
		Edge edge = multiGraph.getEdge(0);
		assertTrue(edge.isDirected());
		assertEquals(edge.getNode0().toString(), "a");
		assertEquals(edge.getNode1().toString(), "b");
	}
	
	@Test
	public void testFileGraph03() {
		try {
			multiGraph = parser.parseFromFile("graph03.gka");
		} catch (IOException e) {
			e.printStackTrace();
		}
		assertEquals(multiGraph.getNodeCount(), 22);
		assertEquals(multiGraph.getEdgeCount(), 40);
		assertEquals(multiGraph.getNode("Hamburg").getEdgeSet().size(), 5);
		Edge edge = multiGraph.getEdge(0);
		assertFalse(edge.isDirected());
		assertEquals(edge.getNode0().toString(), "Paderborn");
		assertEquals(edge.getNode1().toString(), "Hamburg");
	}
	
	@Test
	public void testSmallGraph() {
		graph = new Graph("graph", 10, 20, false);
		multiGraph = graph.getMultiGraph();
		assertEquals(multiGraph.getNodeCount(), 10);
	}
	
	@Test
	public void testSmallGraphSize() {
		graph = new Graph("graph", 30, 210, false);
		multiGraph = graph.getMultiGraph();
		assertEquals(multiGraph.getNodeCount(), 20);
		assertTrue(multiGraph.getEdgeCount() <= 210);
	}
	
	@Test
	public void testBigGraphSize() {
		graph = new Graph("graph", 100, 2500, true);
		multiGraph = graph.getMultiGraph();
		assertEquals(multiGraph.getNodeCount(), 100);
		assertTrue(multiGraph.getEdgeCount() <= 2500);
	}
	
	@Test
	public void testTooBigGraph() {
		try {
			graph = new Graph("graph", 101, 2500, true);
		} catch (Exception e) {
			assertTrue(e instanceof IllegalArgumentException);
		}
		try {
			graph = new Graph("graph", 100, 2501, true);
		} catch (Exception e) {
			assertTrue(e instanceof IllegalArgumentException);
		}
	}
}
