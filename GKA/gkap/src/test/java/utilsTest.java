import org.jgrapht.graph.AbstractBaseGraph;
import org.jgrapht.graph.DefaultWeightedEdge;
import org.junit.Before;
import org.junit.Test;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Scanner;

import static org.junit.Assert.*;

public class utilsTest {
    private String invalidFileName;
    private static final String ALPHA_NUMERIC_STRING = "abcdefghijklmnopqrstuvwxyz0123456789";
    private static final String testFileName = "graph08";

    @Before
    public void initialize() {
        // Make sure to choose a random file name for invalid file name tests
        setInvalidFileName();
    }

    /**
     * Test invalid file name input for reading GKA files
     * NullPointerException expected because does not exist
     */
    @Test(expected = NullPointerException.class)
    public void readInvalidGKAFile() {
        utils.readGKAFile(invalidFileName);
    }

    /**
     * Test if dot files are successfully created
     * <name node1>[ -> <name node2> [(<edge name>)][: <edgeweight>]]; as directed graph
     * <name node1>[ -- <name node2> [(<edge name>)][: <edgeweight>]]; as undirected graph
     */
    @Test
    public void readGKAFile() {
        utils.readGKAFile(testFileName + ".gka");

        File generatedFile  = new File("src/main/graphviz/" + testFileName + ".dot");
        if (!generatedFile.exists()) {
            fail();
        }
    }

    /**
     * Test importing graphs
     * Note: Unable to use toString() representation of library graph to compare weights
     * since edge weight are not dumped
     * // REVIEW jvs 29-May-2006: dump weight somewhere?
     */
    @Test
    public void importGraph() {
        AbstractBaseGraph graph = utils.importGraph(testFileName + ".dot");

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

    @Test
    public void exportGraph() {

        // Import unweighted graph
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
}
