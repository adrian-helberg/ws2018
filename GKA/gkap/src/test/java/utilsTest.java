import org.jgrapht.graph.AbstractBaseGraph;
import org.junit.Before;
import org.junit.Test;
import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import static org.junit.Assert.*;

/**
 * Test suite for static utils
 *
 * TODO: Synchronize test methods
 *
 * @author Adrian Helberg, Maximilian Janzen
 */
public class utilsTest {
    private String invalidFileName;
    private static final String ALPHA_NUMERIC_STRING = "abcdefghijklmnopqrstuvwxyz0123456789";

    @Before // Execute before @Test methods
    public void initialize() {
        // Make sure to choose a random file name for invalid file name tests
        setInvalidFileName();
    }

    /**
     * Test if dot files are successfully created
     * <name node1>[ -> <name node2> [(<edge name>)][: <edgeweight>]]; as directed graph
     * <name node1>[ -- <name node2> [(<edge name>)][: <edgeweight>]]; as undirected graph
     */
    @Before // Execute before @Test methods
    public void readGKAFiles() {

        for (int i = 1; i < 12; i++) {
            if (i == 5) {
                // graph05.gka contains missing weight -> invalid file, tested in readInvalidGKAFile()
                continue;
            }

            utils.readGKAFile("graph" + (i < 10 ? "0" : "") + i + ".gka");
        }
    }

    /**
     * Test invalid file name input for reading GKA files
     * NullPointerException expected because does not exist
     */
    @Test(expected = NullPointerException.class)
    public void readInvalidGKAFile() {
        utils.readGKAFile(invalidFileName);
        utils.readGKAFile("graph05.gka");
    }

    /**
     * Test importing graphs
     * Note: Unable to use toString() representation of library graph to compare weights
     * since edge weight are not dumped
     * // REVIEW jvs 29-May-2006: dump weight somewhere?
     */
    @Test
    public void importGraph() {
        AbstractBaseGraph graph = utils.importGraph("graph08.dot");

        // Check vertices
        String expectedVertices = "[v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16]";
        assertEquals(expectedVertices, graph.vertexSet().toString());

        // Check edges
        String expectedEdges = "[(v1 : v2), (v1 : v3), (v1 : v4), " +
                "(v2 : v5), (v2 : v6), (v2 : v7), (v2 : v8), " +
                "(v3 : v9), (v3 : v10), (v3 : v11), (v3 : v12), " +
                "(v4 : v13), (v5 : v14), (v5 : v15), (v6 : v16)]";
        assertEquals(expectedEdges, graph.edgeSet().toString());

        // Check weights
        assertEquals(5.0, graph.getEdgeWeight(graph.getEdge("v1", "v2")), 0);
        assertEquals(7.0, graph.getEdgeWeight(graph.getEdge("v1", "v3")), 0);
        assertEquals(5.0, graph.getEdgeWeight(graph.getEdge("v1", "v4")), 0);

        assertEquals(4.0, graph.getEdgeWeight(graph.getEdge("v2", "v5")), 0);
        assertEquals(1.0, graph.getEdgeWeight(graph.getEdge("v2", "v6")), 0);
        assertEquals(8.0, graph.getEdgeWeight(graph.getEdge("v2", "v7")), 0);
        assertEquals(3.0, graph.getEdgeWeight(graph.getEdge("v2", "v8")), 0);

        assertEquals(5.0, graph.getEdgeWeight(graph.getEdge("v3", "v9")), 0);
        assertEquals(3.0, graph.getEdgeWeight(graph.getEdge("v3", "v10")), 0);
        assertEquals(4.0, graph.getEdgeWeight(graph.getEdge("v3", "v11")), 0);
        assertEquals(7.0, graph.getEdgeWeight(graph.getEdge("v3", "v12")), 0);
        assertEquals(4.0, graph.getEdgeWeight(graph.getEdge("v4", "v13")), 0);

        assertEquals(5.0, graph.getEdgeWeight(graph.getEdge("v5", "v14")), 0);
        assertEquals(3.0, graph.getEdgeWeight(graph.getEdge("v5", "v15")), 0);

        assertEquals(8.0, graph.getEdgeWeight(graph.getEdge("v6", "v16")), 0);
    }

    /**
     * Test exporting graphs
     */
    @Test
    public void exportGraph() {
        AbstractBaseGraph graph = utils.importGraph("graph08.dot");
        utils.exportGraph(graph, "test");
    }

    /**
     * Set private invalidFileName to test for invalid file name
     */
    private void setInvalidFileName() {
        StringBuilder sb = new StringBuilder();
        for (int i = 1; i <= 7; i++) {
            sb.append(
                    ALPHA_NUMERIC_STRING.charAt(
                            (int) (Math.random() * ALPHA_NUMERIC_STRING.length())
                    )
            );
        }
        invalidFileName = sb.toString();
        // Make sure file with generated name does not exist
        URL url = utils.class.getResource(invalidFileName + ".gka");
        if (url != null) {
            setInvalidFileName();
        }
    }

    /**
     * Test Dijkstra shortest path
     */
    @Test
    public void testDijkstra() {
        String name = "graph01";
        AbstractBaseGraph graph = utils.importGraph(name + ".dot");
        String expectedPath = "[(a : k), (k : g)]";

        assertEquals(
                expectedPath,
                utils.Dijkstra(graph, "a", "g").toString()
        );

        name = "graph02";
        graph = utils.importGraph(name + ".dot");
        expectedPath = "[(a : c), (k : c)]";

        assertEquals(
                expectedPath,
                utils.Dijkstra(graph, "a", "k").toString()
        );

        name = "graph03";
        graph = utils.importGraph(name + ".dot");
        expectedPath = "[(Lueneburg : Soltau), (Celle : Soltau), (Oldenburg : Celle)]";

        assertEquals(
                expectedPath,
                utils.Dijkstra(graph, "Lueneburg", "Oldenburg").toString()
        );
    }

    /**
     * Test Breadth-First Search (BFS)
     */
    @Test
    public void testBFS() {
        String name = "graph01";
        AbstractBaseGraph graph = utils.importGraph(name + ".dot");

        String source = "a";
        String target = "g";

        assertEquals(
                utils.Dijkstra(graph, source, target).toString(),
                utils.BFS(graph, source, target, new ArrayList<>()).toString()
        );

        name = "graph02";
        graph = utils.importGraph(name + ".dot");

        source = "a";
        target = "k";

        assertEquals(
                utils.Dijkstra(graph, source, target).toString(),
                utils.BFS(graph, source, target, new ArrayList<>()).toString()
        );

        name = "graph03";
        graph = utils.importGraph(name + ".dot");

        source = "Lueneburg";
        target = "Oldenburg";

        assertEquals(
                utils.Dijkstra(graph, source, target).toString(),
                utils.BFS(graph, source, target, new ArrayList<>()).toString()
        );
    }
}
