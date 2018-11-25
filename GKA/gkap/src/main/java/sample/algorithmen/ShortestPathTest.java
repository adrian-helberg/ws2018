package sample.algorithmen;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.graphstream.graph.Edge;
import org.graphstream.graph.Path;
import org.graphstream.graph.implementations.MultiGraph;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import org.junit.rules.Timeout;
import sample.MainApp;
import sample.exception.InvalidGraphException;

/**
 * GKAP Shortest Path Test
 *
 * @author Adrian Helberg
 * @author Maximilian Janzen
 */
public class ShortestPathTest {
	private Graph graph;
	private MultiGraph multiGraph;
	private Parser parser;
	
	@Before
	public void start() {
		graph = new Graph("graph");
		parser = new Parser(graph.getMultiGraph());
	}

	@Test
	public void testGraphBuild() {
		multiGraph = graph.getMultiGraph();
		Assert.assertNotSame(multiGraph, null);
	}
	
	@Test
	public void testBreadthFirstSearchWithSmallGraph() {
		BreadthFirstSearch bfs = new BreadthFirstSearch(getSmallMultiGraph());
		String from = "a", to = "c";
		List<Path> paths;
		try {
			paths = bfs.getShortestPath(from, to);
			assertEquals(paths.size(), 1);

			Path path = paths.get(0);
			assertEquals(path.getEdgeCount(), 1); // Hat der kuerzeste Pfad nur eine Kante?
		} catch (InvalidGraphException e) {
			e.printStackTrace();
		}
		
	}
	
	@Test
	public void testNotReachableNode() {
		MultiGraph graph = getSmallMultiGraph();
		// Isolate b
		graph.removeEdge("ab");
		graph.removeEdge("bc");

		BreadthFirstSearch bfs = new BreadthFirstSearch(graph);

		List<Path> paths;
		try {
			paths = bfs.getShortestPath("a", "b");
			assertEquals(paths.size(), 0);
		} catch (InvalidGraphException e) {
			e.printStackTrace();
		}
		
	}
	
	@Test
	public void testBreadthFirstSearchWithGraph01() {
		try {
			multiGraph = parser.parseFromFile("graph01.gka");
		} catch (IOException e) {
			e.printStackTrace();
		}
		assertNotNull(multiGraph);

		BreadthFirstSearch bfs = new BreadthFirstSearch(multiGraph);

		List<Path> paths;
		try {
			paths = bfs.getShortestPath("a", "c");
			assertEquals(paths.size(), 1);
			
			Path path = paths.get(0);
			assertEquals(path.getEdgeCount(), 1);
			paths = bfs.getShortestPath("c", "g");
			assertEquals(paths.size(), 1);
			path = paths.get(0);
			assertEquals(path.getEdgeCount(), 3);
		} catch (InvalidGraphException e) {
			e.printStackTrace();
		}
	}

	@Test
	public void testBreadthFirstSearchWithGraph08() {
		try {
			multiGraph = parser.parseFromFile("graph08.gka");
		} catch (IOException e) {
			e.printStackTrace();
		}
		assertNotNull(multiGraph);

		BreadthFirstSearch bfs = new BreadthFirstSearch(multiGraph);

		List<Path> paths;
		try {
			paths = bfs.getShortestPath("v1", "v13");
			assertEquals(paths.size(), 1);
			Path path = paths.get(0);
			assertEquals(path.getEdgeCount(), 2);
		} catch (InvalidGraphException e) {
			e.printStackTrace();
		}
	}
	
	@Test
	public void testDijkstraWithSmallGraph() {
		Dijkstra dijkstra = new Dijkstra(getSmallMultiGraph());
		List<Path> paths;

		try {
			paths = dijkstra.getShortestPath("a", "c");
			if (dijkstra.checkNodes("a", "c")) {
				assertEquals(paths.size(), 1);
				Path path = paths.get(0);
				assertEquals(path.getEdgeCount(), 1);
			}
		} catch (InvalidGraphException e) {
			e.printStackTrace();
		}
	}
	
	@Test
	public void testDijkstraWithGraph03() {
		try {
			multiGraph = parser.parseFromFile("graph03.gka");
		} catch (IOException e) {
			e.printStackTrace();
		}
		assertNotNull(multiGraph);

		Dijkstra dijkstra = new Dijkstra(multiGraph);
		List<Path> paths;
		try {
			paths = dijkstra.getShortestPath("Hamburg", "Minden");
			assertEquals(paths.size(), 1);
			Path path = paths.get(0);
			assertEquals(path.getEdgeCount(), 2);
			assertEquals(multiGraph.getNode("Hamburg"), path.getRoot());
			List<Edge> edges = path.getEdgePath();
			Edge lastEdge = edges.get(edges.size() - 1);

			assertTrue(lastEdge.getSourceNode().equals(multiGraph.getNode("Minden")) || lastEdge.getTargetNode().equals(multiGraph.getNode("Minden")));
		} catch (InvalidGraphException e) {
			e.printStackTrace();
		}
	}
	
	@SuppressWarnings("Duplicates")
	@Test
	public void testFloydWarschallWithSmallGraph() {
		FloydWarschall floydWarschall = new FloydWarschall(getSmallMultiGraph());
		List<Path> paths;

		try {
			paths = floydWarschall.getShortestPath("a", "c");

			if (floydWarschall.checkNodes("a", "c")) {
				assertEquals(paths.size(), 1);
				Path path = paths.get(0);

				assertEquals(path.getEdgeCount(), 1);
			}
		} catch (InvalidGraphException e) {
			e.printStackTrace();
		}
	}
	
	@Test
	public void testFloydWarschallWithGraph03() {
		try {
			multiGraph = parser.parseFromFile("graph03.gka");
		} catch (IOException e) {
			e.printStackTrace();
		}
		assertNotNull(multiGraph);

		FloydWarschall floydWarschall = new FloydWarschall(multiGraph);
		List<Path> paths = null;
		try {
			paths = floydWarschall.getShortestPath("Hamburg", "Minden");
		} catch (InvalidGraphException e) {
			e.printStackTrace();
		}

		assertEquals(paths.size(), 1);

		Path path = paths.get(0);

		assertEquals(path.getEdgeCount(), 2);
		assertEquals(multiGraph.getNode("Hamburg"), path.getRoot());

		List<Edge> edges = path.getEdgePath();
		Edge lastEdge = edges.get(edges.size() - 1);

		assertTrue(lastEdge.getSourceNode().equals(multiGraph.getNode("Minden")) || lastEdge.getTargetNode().equals(multiGraph.getNode("Minden")));
	}
	
	@Test
	public void testNormalGraph() {
		MultiGraph multiGraph = new Graph("graph", 20, 70, false).getMultiGraph();

		try {
			Dijkstra dijkstra = new Dijkstra(multiGraph);
			assertEquals(1, dijkstra.getShortestPath("4", "20").size());

			FloydWarschall floydWarschall = new FloydWarschall(multiGraph);
			assertEquals(1, floydWarschall.getShortestPath("4", "20").size());

			assertEquals(dijkstra.getTotalDistance(), floydWarschall.getTotalDistance());
		} catch (InvalidGraphException e1) {
			e1.printStackTrace();
		}
	}
	
	@Test
	public void testBigGraph() {

		MultiGraph big = new Graph("BIG", 100, 3500, true).getMultiGraph();
		BreadthFirstSearch bfs = new BreadthFirstSearch(big);
		List<Path> paths = new LinkedList<>();
		try {
			paths = bfs.getShortestPath("1", "80");
		} catch (InvalidGraphException e) {
			e.printStackTrace();
		}

		assertTrue(paths.size() > 0);

		Dijkstra dijkstra = new Dijkstra(big);
		FloydWarschall floydWarschall = new FloydWarschall(big);

		try {
			assertEquals(dijkstra.getTotalDistance(), floydWarschall.getTotalDistance());
			assertEquals(dijkstra.getShortestPath("1", "67"), floydWarschall.getShortestPath("1", "67"));
		} catch (InvalidGraphException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Return small multi-graph
	 * @return Multi-graph
	 */
	private MultiGraph getSmallMultiGraph() {
		MultiGraph graph = new MultiGraph("Graph");

		graph.addNode("a");
		graph.addNode("b");
		graph.addNode("c");
		graph.addEdge("ab", "a", "b", false);
		graph.addEdge("bc", "b", "c", false);
		graph.addEdge("ac", "a", "c", false);

		for (Edge edge : graph.getEdgeSet()) {
			edge.setAttribute(Graph.distance, 2);
		}

		return graph;
	}
}
