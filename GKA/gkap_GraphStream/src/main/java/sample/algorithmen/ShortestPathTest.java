package sample.algorithmen;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.util.List;
import org.graphstream.graph.Edge;
import org.graphstream.graph.Path;
import org.graphstream.graph.implementations.MultiGraph;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import sample.exception.InvalidGraphException;

public class ShortestPathTest {
	Graph _graph;
	MultiGraph _multiGraph;
	Parser _parser;
	
	@Before
	public void start() {
		_graph = new Graph("graph");
		_parser = new Parser(_graph.getMultiGraph());
	}
	
	/**
	 * Testet, ob der Graph richtig deklariert.
	 */
	@Test
	public void testGraphBuild() {
		_multiGraph = _graph.getMultiGraph();
		Assert.assertNotSame(_multiGraph, null);
	}
	
	@Test
	public void testBreadthFirstSearchWithSmallGraph() {
		BreadthFirstSearch bfs = new BreadthFirstSearch(getSmallMultiGraph());
		String from = "a", to = "c";
		List<Path> paths;
		try {
			paths = bfs.getShortestPath(from, to);
			assertEquals(paths.size(), 1); // Gibt es genau einen kuerzesten Pfad?

			Path path = paths.get(0);
			assertEquals(path.getEdgeCount(), 1); // Hat der kuerzeste Pfad nur eine Kante?
		} catch (InvalidGraphException e) {
			e.printStackTrace();
		}
		
	}
	
	@Test
	public void testNotReachableNode() {
		MultiGraph graph = getSmallMultiGraph();
		graph.removeEdge("ab");
		graph.removeEdge("bc"); // Jetzt gibt es keine Kanten zu Knoten b
		BreadthFirstSearch bfs = new BreadthFirstSearch(graph);
		String from = "a", to = "b";
		List<Path> paths;
		try {
			paths = bfs.getShortestPath(from, to);
			assertEquals(paths.size(), 0); // Gibt es keinen kuerzesten Pfad?
		} catch (InvalidGraphException e) {
			assertTrue(e instanceof InvalidGraphException);
		}
		
	}
	
	@Test
	public void testBreadthFirstSearchWithGraph01() {
		try {
			_multiGraph = _parser.parseFromFile("graph01.gka");
		} catch (IOException e) {
			System.out.println("Fehler beim Lesen der Datei: " + e.getMessage());
			e.printStackTrace();
		}
		BreadthFirstSearch bfs = new BreadthFirstSearch(_multiGraph);
		String from = "a", to = "c";
		List<Path> paths;
		try {
			paths = bfs.getShortestPath(from, to);
			assertEquals(paths.size(), 1); // Gibt es genau einen kuerzesten Pfad?
			
			Path path = paths.get(0);
			assertEquals(path.getEdgeCount(), 1); // Hat der kuerzeste Pfad nur eine Kante?

			from = "c";
			to = "g";
			paths = bfs.getShortestPath(from, to);
			assertEquals(paths.size(), 1); // Gibt es genau einen kuerzesten Pfad?
			path = paths.get(0);
			assertEquals(path.getEdgeCount(), 3); // Hat der kuerzeste Pfad 3 Kanten?
		} catch (InvalidGraphException e) {
			e.printStackTrace();
		}
		
		
	}

	@Test
	public void testBreadthFirstSearchWithGraph08() {
		try {
			_multiGraph = _parser.parseFromFile("graph08.gka");
		} catch (IOException e) {
			System.out.println("Fehler beim Lesen der Datei: " + e.getMessage());
			e.printStackTrace();
		}
		BreadthFirstSearch bfs = new BreadthFirstSearch(_multiGraph);
		String from = "v1", to = "v13";
		List<Path> paths;
		try {
			paths = bfs.getShortestPath(from, to);
			assertEquals(paths.size(), 1); // Gibt es genau einen kuerzesten Pfad?
			
			Path path = paths.get(0);
			assertEquals(path.getEdgeCount(), 2); // Hat der kuerzeste Pfad 2 Kanten?
		} catch (InvalidGraphException e) {
			e.printStackTrace();
		}
		
	}
	
	@Test
	public void testDijkstraWithSmallGraph() {
		Dijkstra dijkstra = new Dijkstra(getSmallMultiGraph());
		String from = "a", to = "c";
		List<Path> paths = null;
		try {
			paths = dijkstra.getShortestPath(from, to);
			if (dijkstra.checkNodes(from, to)) {
				assertEquals(paths.size(), 1); // Gibt es genau einen kuerzesten Pfad?

				Path path = paths.get(0);
				assertEquals(path.getEdgeCount(), 1); // Hat der kuerzeste Pfad nur eine Kante?
			}
		} catch (InvalidGraphException e) {
			System.out.println("here dijkstra");
			e.printStackTrace();
		}
		
	}
	
	@Test
	public void testDijkstraWithGraph03() {
		try {
			_multiGraph = _parser.parseFromFile("graph03.gka");
		} catch (IOException e) {
			System.out.println("Fehler beim Lesen der Datei: " + e.getMessage());
			e.printStackTrace();
		}
		Dijkstra dijkstra = new Dijkstra(_multiGraph);
		String from = "Hamburg", to = "Minden";
		List<Path> paths;
		try {
			paths = dijkstra.getShortestPath(from, to);
			assertEquals(paths.size(), 1); // Gibt es genau einen kuerzesten Pfad?
			Path path = paths.get(0);
			assertEquals(path.getEdgeCount(), 2); // Hat der kuerzeste Pfad 2 Kanten?
			assertEquals(_multiGraph.getNode(from), path.getRoot()); // Ist der Wurzelknoten der Startknoten?
			List<Edge> edges = path.getEdgePath();
			Edge lastEdge = edges.get(edges.size() - 1);
			// Beinhaltet die letzte Kante den Zielknoten?
			assertTrue(lastEdge.getSourceNode().equals(_multiGraph.getNode(to)) || lastEdge.getTargetNode().equals(_multiGraph.getNode(to)));
		} catch (InvalidGraphException e) {
			e.printStackTrace();
		}
		
	}
	
	@Test
	public void testFloydWarschallWithSmallGraph() {
		FloydWarschall floydWarschall = new FloydWarschall(getSmallMultiGraph());
		String from = "a", to = "c";
		List<Path> paths = null;
		try {
			paths = floydWarschall.getShortestPath(from, to);
			if (floydWarschall.checkNodes(from, to)) {
				assertEquals(paths.size(), 1); // Gibt es genau einen kuerzesten Pfad?

				Path path = paths.get(0);
				assertEquals(path.getEdgeCount(), 1); // Hat der kuerzeste Pfad nur eine Kante?
			}
		} catch (InvalidGraphException e) {
			System.out.println("here floydSmallGraph");
			e.printStackTrace();
		}
		
	}
	
	@Test
	public void testFloydWarschallWithGraph03() {
		try {
			_multiGraph = _parser.parseFromFile("graph03.gka");
		} catch (IOException e) {
			System.out.println("Fehler beim Lesen der Datei: " + e.getMessage());
			e.printStackTrace();
		}
		FloydWarschall floydWarschall = new FloydWarschall(_multiGraph);
		String from = "Hamburg", to = "Minden";
		List<Path> paths = null;
		try {
			paths = floydWarschall.getShortestPath(from, to);
		} catch (InvalidGraphException e) {
			System.out.println("Fehler beim Ermitteln des kuerzesten Pfades: " + e.getMessage());
			e.printStackTrace();
		}
		assertEquals(paths.size(), 1); // Gibt es genau einen kuerzesten Pfad?
		Path path = paths.get(0);
		assertEquals(path.getEdgeCount(), 2); // Hat der kuerzeste Pfad 2 Kanten?
		assertEquals(_multiGraph.getNode(from), path.getRoot()); // Ist der Wurzelknoten der Startknoten?
		List<Edge> edges = path.getEdgePath();
		Edge lastEdge = edges.get(edges.size() - 1);
		// Beinhaltet die letzte Kante den Zielknoten?
		assertTrue(lastEdge.getSourceNode().equals(_multiGraph.getNode(to)) || lastEdge.getTargetNode().equals(_multiGraph.getNode(to)));
	}
	
	@Test
	public void testNormalGraph() {
		MultiGraph multiGraph = new Graph("graph", 20, 70, false).getMultiGraph();
		String from = "4", to = "20";
		BreadthFirstSearch bfs = new BreadthFirstSearch(multiGraph);
		List<Path> paths;
		try {
			paths = bfs.getShortestPath(from, to);
			int size = paths.size(); // Gibt es einen Pfad?
			if (size > 1) {
				size = 1;
			}
			// Falls es (mindestens) einen Pfad gibt
			int distance = 0;
			Dijkstra dijkstra = new Dijkstra(multiGraph);
			paths = dijkstra.getShortestPath(from, to);
			assertEquals(size, paths.size()); // findet der Dijkstra den Pfad?
			distance = dijkstra.getTotalDistance();
			
			FloydWarschall floydWarschall = new FloydWarschall(multiGraph);
			paths = floydWarschall.getShortestPath(from, to);
//			assertEquals(size, paths.size()); // findet der FolydWarschall den Pfad?
//			assertEquals(distance, floydWarschall.getTotalDistance());
		} catch (InvalidGraphException e1) {
			e1.printStackTrace();
		}
		
	}
	
	@Test
	public void testBigGraph() {
		MultiGraph multiGraph = new Graph("graph", 70, 200, true).getMultiGraph();
		String from = "1", to = "68";
		BreadthFirstSearch bfs = new BreadthFirstSearch(multiGraph);
		List<Path> paths;
		try {
			paths = bfs.getShortestPath(from, to);
			int size = paths.size(); // Gibt es einen Pfad?
			if (size > 1) {
				size = 1;
			}
			// Falls es (mindestens) einen Pfad gibt
			int distance = 0;
			Dijkstra dijkstra = new Dijkstra(multiGraph);
			paths = dijkstra.getShortestPath(from, to);
			assertEquals(size, paths.size()); // findet der Dijkstra den Pfad?
			distance = dijkstra.getTotalDistance();
			FloydWarschall floydWarschall = new FloydWarschall(multiGraph);
			paths = floydWarschall.getShortestPath(from, to);
			// assertEquals(size, paths.size()); // findet der FolydWarschall den Pfad?
			// assertEquals(distance, floydWarschall.getTotalDistance());
		} catch (InvalidGraphException e1) {
			System.out.println("here bigGraph");
			e1.printStackTrace();
		}
		
	}
	
	/**
	 * Erstellt einen kleinen Multigraph
	 * @return Graph
	 */
	private MultiGraph getSmallMultiGraph() {
		MultiGraph graph = new MultiGraph("Graph");
		graph.addNode("a");
		graph.addNode("b");
		graph.addNode("c");
		boolean directed = false;
		graph.addEdge("ab", "a", "b", directed);
		graph.addEdge("bc", "b", "c", directed);
		graph.addEdge("ac", "a", "c", directed);
		for (Edge edge : graph.getEdgeSet()) {
			edge.setAttribute(Graph.distance, 2);
		}
		return graph;
	}
}
