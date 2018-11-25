package sample.algorithmen;

import java.util.LinkedList;
import java.util.List;

import org.graphstream.algorithm.AStar;
import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.Path;
import org.graphstream.graph.implementations.MultiGraph;

import sample.exception.InvalidGraphException;

/**
 * Adstrakte Klasse mit den Basisfunktionen fuer Algorithmen
 * @author Maxmilian Janzen & Adrian Helberg
 *
 */
public abstract class Algorithm {

	protected MultiGraph _graph;
	private long _timeMilliseconds;
	private long _timeNanoseconds;
	private int _graphAccess = 0;
	private int _graphHash = 0;
	
	/**
	 * Startet die Laufzeitmessung
	 */
	protected void startTimer() {
		_timeMilliseconds = System.currentTimeMillis();
		_timeNanoseconds = System.nanoTime();
	}
	
	/**
	 * Stoppt die Laufzeitmessung
	 */
	protected void stopTimer() {
		_timeMilliseconds = System.currentTimeMillis() - _timeMilliseconds;
		_timeNanoseconds = System.nanoTime() - _timeNanoseconds;
	}
	
	/**
	 * Gibt die Dauer in Millisekunden aus
	 * @return x Millisekunden.
	 */
	public String getStringTimeMilliseconds() {
		return _timeMilliseconds + " Millisekunden";
	}
	
	/**
	 * Gibt die Dauer in Nanosekunden aus
	 * @return x Nanosekunden.
	 */
	public String getStringTimeNanoseconds() {
		return _timeNanoseconds + " Nanosekunden";
	}
	
	/**
	 * Prueft die Knoten auf null und ob im Graphen vorhanden und ob ein Pfad existiert
	 * @param from Startknoten
	 * @param to Zielknoten
	 * @throws InvalidGraphException 
	 */
	protected boolean checkNodes(String from, String to) throws InvalidGraphException {
		if (_graph != null && _graphHash != _graph.hashCode()) {
			AStar aStar = new AStar(_graph);
			aStar.compute(from, to);
			Path path = aStar.getShortestPath();
			if (path == null || path.size() <= 0) {
				throw new InvalidGraphException("Der Graph besitzt keinen Pfad von " + from + " zu " + to);
			}
		}
		if (from == null || _graph.getNode(from) == null) { throw new IllegalArgumentException("Knoten 'from' nicht gefunden: " + from); }
		if (to == null || _graph.getNode(to) == null) { throw new IllegalArgumentException("Knoten 'to' nicht gefunden: " + to); }
		return true;
	}
	
	/**
	 * Sortiert Pfade in die Reihenfolge von dem Startknoten aus
	 * @param pathList Pfadliste
	 * @return Sortierte Pfade
	 */
	protected List<Path> turnPaths(List<Path> pathList) {
		List<Path> paths = new LinkedList<>();
		for (Path path : pathList) { // Jeden Pfad
			List<Edge> pathEdgeList = path.getEdgePath(); // Kantenliste erstellen
			List<Node> pathNodeList = path.getNodePath(); // Knotenliste erstellen
			Path pathNeu = new Path();
			pathNeu.setRoot(pathNodeList.get(pathNodeList.size() - 1)); // Wurzelknoten = Startknoten
			for (int i = pathEdgeList.size() - 1; i >= 0; i--) { // jede Kante
				pathNeu.add(pathEdgeList.get(i)); // Zum neuen Pfad hinzufuegen
			}
			paths.add(pathNeu); // Neuen Pfad hinzufuegen
		}
		return paths;
	}
	
	/**
	 * Erhoeht den Counter fuer die Zugriffe auf einen Graphen um 1
	 */
	protected void increaseGraphAccess() {
		_graphAccess++;
	}
	
	/**
	 * Gibt die Anzahl der Zugriffe auf einen Graphen aus
	 * @return Integer
	 */
	public int getGraphAccess() {
		return _graphAccess;
	}
}
