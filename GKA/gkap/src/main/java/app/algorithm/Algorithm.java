package app.algorithm;

import org.graphstream.algorithm.AStar;
import org.graphstream.graph.implementations.MultiGraph;

/**
 * GKAP base algorithm
 *
 * @author Adrian Helberg, Maximilian Janzen
 */
public abstract class Algorithm {
    MultiGraph graph;
    int graphTouched = 0;

    /**
     * Check nodes for invalidity and if a path exists between them
     * @param from Source node
     * @param to Target node
     * @return True if check successful; False otherwise
     * @throws InvalidGraphException for invalid input
     * @see <a href="https://de.wikipedia.org/wiki/A*-Algorithmus">A-Star-Algorithm</a>
     */
    protected boolean checkNodes(String from, String to) throws InvalidGraphException {
        // Handle invalid input
        if (graph == null) throw new InvalidGraphException("Graph is null");
        if (graph.hashCode() == 0) throw new InvalidGraphException("Invalid graph hash");
        if (from == null) throw new InvalidGraphException("Source node is null");
        if (to == null) throw new InvalidGraphException("Target node is null");

        // Initialize A* Algorithm
        AStar aStar = new AStar(graph);
        aStar.compute(from, to);

        if (aStar.noPathFound()) throw new InvalidGraphException("No path from " + from + " to " + to);

        return true;
    }

    /**
     * Increase the number of graph accesses
     */
    public void graphTouched() { graphTouched++; }

    /**
     * Return the number of graph touches
     * @return Number of graph accesses
     */
    public int getGraphTouched() { return graphTouched; }
}
