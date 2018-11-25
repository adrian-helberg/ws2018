package sample.algorithmen;

import java.util.LinkedList;
import java.util.List;

import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.Path;
import org.graphstream.graph.implementations.MultiGraph;

import sample.exception.InvalidGraphException;
/**
 * BFS
 * @author Maxmilian Janzen & Adrian Helberg
 *
 */
public class BreadthFirstSearch extends ShortestPathAlgorithm {

	public BreadthFirstSearch(MultiGraph graph) {
		_graph = graph;
	}

	@Override
	public List<Path> getShortestPath(String from, String to) throws InvalidGraphException {
		checkNodes(from, to);
		startTimer();
		if (!flagNodes(from, to)) { // Falls der Zielknoten nicht erreicht wurde
			return new LinkedList<>();
		}
		List<Path> paths = new LinkedList<>();
		Node target = _graph.getNode(to); // Gebe Zielknoten
		increaseGraphAccess();
		Path path = new Path();
		path.setRoot(target); // Pfad beginnt bei Zielknoten
		if ((int) target.getAttribute(Graph.index) == 0) { // Falls Start- =
															// Zielknoten ist
			paths.add(path); // Pfad komplett
			return paths;
		}
		paths.addAll(addEdges(path, target)); // Ermittelte Pfade hinzufuegen
		paths = turnPaths(paths);
		stopTimer();
		return paths;
	}

	/**
	 * Setzt rekursiv Pfade vom Zielknoten ausgehend zusammen
	 * 
	 * @param path
	 *            Bisheriger Pfad
	 * @param target
	 *            Zielknoten
	 * @return Ermittelte Pfade
	 */
	private List<Path> addEdges(Path path, Node target) {
		List<Path> paths = new LinkedList<>();
		Node adjazentKnot;
		int i = target.getAttribute(Graph.index); // Index des Zielknotens
		for (Edge edge : target.getEachEnteringEdge()) { // Alle ankommenden
															// Kanten
			Object iSource = edge.getSourceNode().getAttribute(Graph.index); // Index
																				// Sourceknoten
			Object iTarget = edge.getTargetNode().getAttribute(Graph.index); // Index
																				// Targetknoten
			if (iSource != null && iTarget != null && (((int) iSource == i - 1 || (int) iTarget == i - 1))) {
				// Falls ein verbundener Knoten index = Zielindex -1 hat
				adjazentKnot = getPrevNode(edge, target);
				Path pathNew = path.getACopy(); // Bisherigen Pfad kopieren
				pathNew.add(edge); // Kante hinzufuegen
				if (i <= 1) {
					paths.add(pathNew); // Neuen Pfad hinzufuegen
				}
				paths.addAll(addEdges(pathNew, adjazentKnot)); // Rekursiver
																// Aufruf mit
																// neuem
																// Ausgangsknoten
			}
		}
		return paths;
	}

	@Override
	public int getTotalDistance() {
		return 0;
	}

	/**
	 * Ermittelt den Vorgaenger des Sourceknoten
	 * 
	 * @param edge
	 *            Kante
	 * @param source
	 *            Sourceknoten
	 * @return Vorgaenger des Sourceknoten
	 */
	private Node getPrevNode(Edge edge, Node source) {
		Node adjazentNode; //
		if (edge.isDirected()) { // Falls Kante gerichtet
			adjazentNode = edge.getSourceNode(); // Ausgehender Knoten ist der
													// Adjazentknoten
		} else {
			if (edge.getSourceNode().getId().equals(source.getId())) { // Falls
																		// Sourceknoten
																		// =
																		// Kantensource
																		// ist
				adjazentNode = edge.getTargetNode(); // Waehle Kantentarget
			} else { // Sonst
				adjazentNode = edge.getSourceNode(); // Waehle Kantensource
			}
		}
		return adjazentNode;
	}

	/**
	 * Prueft ob Start- und Zielknoten aequivalent sind
	 * 
	 * @param source:
	 *            Startknoten
	 * @param target:
	 *            Zielknoten
	 * @return Existiert ein Weg?
	 */
	public boolean flagNodes(String from, String to) {
		int i = 0;
		List<Node> flaggedNodes = new LinkedList<>();
		Node startNode = _graph.getNode(from); // Startknoten
		increaseGraphAccess();
		startNode.addAttribute(Graph.index, i); // Index des Startknotens
												// speichern
		if (from.equals(to)) { // Sind Start- und Zielknoten gleich
			return true;
		}
		flaggedNodes.add(startNode); // Startknoten ist markiert
		return flagNodes(flaggedNodes, to, i);
	}

	/**
	 * Rekursive Methode um Knoten zu markieren und den Zielknoten zu suchen
	 * 
	 * @param nodes
	 *            Liste Knoten
	 * @param to
	 *            Zielknoten
	 * @param i
	 *            Index
	 * @return Wurde der Zielknoten erreicht
	 */
	private boolean flagNodes(List<Node> nodes, String to, int i) {
		Node node;
		List<Node> flaggedNodes = new LinkedList<>();
		for (Node currentNode : nodes) { // Durchsuche bisherige Knoten
			for (Edge edge : currentNode.getEachLeavingEdge()) { // Alle
																	// ausgehenden
																	// Kanten
				if (edge.getTargetNode().getAttribute(Graph.index) == null
						|| edge.getSourceNode().getAttribute(Graph.index) == null) {
					node = getNextNode(edge, currentNode); // Hat ein
															// verbundener
															// Knoten keinen
															// Index
					node.addAttribute(Graph.index, i + 1); // Speichere Index
					flaggedNodes.add(node); // Speichere Knoten als markiert
					if (node.getId().equals(to)) { // Falls Knoten = Zielknoten
						return true;
					}
				}
			}
		}
		if (flaggedNodes.isEmpty()) { // Falls kein neuer Knoten erreicht wurde
			return false;
		} else {
			return flagNodes(flaggedNodes, to, i + 1); // Rekursiver Aufruf
		}
	}

	/**
	 * Liefert den Knoten am Ende einer Kante
	 * 
	 * @param edge
	 *            Kante
	 * @param from
	 *            Startknoten
	 * @return verbundener Knoten
	 */
	private Node getNextNode(Edge edge, Node from) {
		Node adjazentNode; // Naechster Knoten
		if (edge.isDirected()) { // Falls gerichtet
			adjazentNode = edge.getTargetNode(); // = Targetknoten
		} else {
			if (edge.getTargetNode().getId().equals(from.getId())) { // Falls
																		// Targetknoten
																		// =
																		// Sourceknoten
				adjazentNode = edge.getSourceNode(); // = Sourceknoten
			} else { // Sonst
				adjazentNode = edge.getTargetNode(); // = Targetknoten
			}
		}
		return adjazentNode;
	}

}
