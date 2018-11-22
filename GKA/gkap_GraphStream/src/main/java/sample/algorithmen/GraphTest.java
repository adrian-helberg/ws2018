package sample.algorithmen;

import static org.junit.Assert.*;

import java.io.IOException;
import java.util.regex.Pattern;

import org.graphstream.graph.Edge;
import org.graphstream.graph.implementations.MultiGraph;
import org.junit.Before;
import org.junit.Test;

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
	public void testPattern() {					// Testet das Pattern fuer eine Kante
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
			System.out.println("Fehler beim Lesen der Datei: " + e.getMessage());
			e.printStackTrace();
		}
		assertEquals(multiGraph.getNodeCount(), 12);  // Testet die korrekte Anzahl der Knoten
		assertEquals(multiGraph.getEdgeCount(), 39);	 // Testet die korrekte Anzahl der Kanten
		assertEquals(multiGraph.getNode("f").getEdgeSet().size(), 4);	// Testet die korrekte Anzahl an Kanten fuer Knoten f
		Edge edge = multiGraph.getEdge(0);
		assertTrue(edge.isDirected());
		assertEquals(edge.getNode0().toString(), "a");	// Testet den korrekten Startknoten der 1ten Kante
		assertEquals(edge.getNode1().toString(), "b");	// Testet den korrekten Zielknoten der 1ten Kante
	}
	
	@Test
	public void testFileGraph03() {
		try {
			multiGraph = parser.parseFromFile("graph03.gka");
		} catch (IOException e) {
			System.out.println("Fehler beim Lesen der Datei: " + e.getMessage());
			e.printStackTrace();
		}
		assertEquals(multiGraph.getNodeCount(), 22);			// Testet die korrekte Anzahl der Knoten
		assertEquals(multiGraph.getEdgeCount(), 40);			// Testet die korrekte Anzahl der Kanten
		assertEquals(multiGraph.getNode("Hamburg").getEdgeSet().size(), 5);	// Testet die korrekte Anzahl an Kanten fuer Knoten Hamburg
		Edge edge = multiGraph.getEdge(0);
		assertFalse(edge.isDirected());		// Testet ob die erste kante gerichtet ist
		assertEquals(edge.getNode0().toString(), "Paderborn");	// Testet den korrekten Startknoten der ersten Kante
		assertEquals(edge.getNode1().toString(), "Hamburg");	// Testen den korrekten Zielknoten der ersten Kante
	}
	
	@Test
	public void testSmallGraph() {
		graph = new Graph("graph", 10, 20, false);
		multiGraph = graph.getMultiGraph();
		assertEquals(multiGraph.getNodeCount(), 10);			// Testet die korrekte Anzahl der Knoten
	}
	
	@Test
	public void testSmallGraphSize() {
		graph = new Graph("graph", 30, 210, false);
		multiGraph = graph.getMultiGraph();
		assertEquals(multiGraph.getNodeCount(), 20);			// Testet die korrekte Anzahl der Knoten
		assertTrue(multiGraph.getEdgeCount() <= 210);			// Testet die korrekte Anzahl der Kanten
	}
	
	@Test
	public void testBigGraphSize() {
		graph = new Graph("graph", 100, 2500, true);
		multiGraph = graph.getMultiGraph();
		assertEquals(multiGraph.getNodeCount(), 100);			// Testet die korrekte Anzahl der Knoten
		assertTrue(multiGraph.getEdgeCount() <= 2500);			// Testet die korrekte Anzahl der Kanten
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
