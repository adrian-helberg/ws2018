package sample.algorithmen;

import java.util.List;
import org.graphstream.graph.Path;
import sample.exception.InvalidGraphException;

/**
 * @author Maximilian Janzen & Adrian Helberg
 */
public abstract class ShortestPathAlgorithm extends Algorithm {

	/**
	 * Findet den oder die kuerzesten Pfade in einem Graphen
	 * @param from Startknoten
	 * @param to Zielknoten
	 * @return Liste mit kuerzesten Pfaden
	 * @throws InvalidGraphException 
	 */
	abstract public List<Path> getShortestPath(String from, String to) throws InvalidGraphException;
	
	abstract public int getTotalDistance();
}
