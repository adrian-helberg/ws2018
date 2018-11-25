package app.algorithm;

import org.graphstream.graph.Path;

import java.util.List;

/**
 * GKAP shortest path algorithm
 *
 * @author Adrian Helberg, Maximilian Janzen
 */
public abstract class ShortestPathAlgorithm extends Algorithm {

    /**
     * Get the shortest path between two nodes
     * @param from source node
     * @param to target node
     * @return List of shortest paths
     * @throws InvalidGraphException for invalid graph
     */
    public abstract List<Path> getShortestPath(String from, String to) throws InvalidGraphException;

    /**
     * Get the distance TODO: Better explanation here
     * @return Number of used edges
     */
    public abstract int getDistance();
}
