package sample.algorithmen;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import org.graphstream.graph.Path;

import sample.exception.InvalidGraphException;

public class Comunicator {
	private Graph _graph;
	private Parser _parser;
	private ArrayList<ShortestPathAlgorithm> _shortestPathAlgorithm;
	private String _from = "";
	private String _to = "";
	private boolean _graphLoaded = false;
	
	private final String _hilfe = "Hilfe:\nKante mit Knoten:Startknoten -- (oder) -> Zielknoten : Kantengewicht(Ganzzahl)"
			+ "\nOperatorsymbol: '/' vor folgenden Befehlen angeben:\nStart- und Zielknoten:from <Name> to <name>f <Name> t <name>\nAlgorithmen aufrufen:\n"
			+ "Breadth First Search: breadthfirstsearch (oder) breadth (oder) bfs\nDijkstra: dijkstra (oder) dk\n"
			+ "Floyd-Warschall: floydwarschall (oder) floyd (oder) fw\nDatei einlesen: (Pfad zur Datei) Dateiname\n"
			+ "Neuer kleiner Graph: graph (oder) newgraph Knotenanzahl(Ganzzahl) Kantenanzahl(Ganzzahl)\n"
			+ "Neuer Big-graph: big (oder) biggraph (oder) graphbig Knotenanzahl(Ganzzahl) Kantenanzahl(Ganzzahl)";

	public Comunicator() {
		_graph = new Graph("graph");
		_parser = new Parser(_graph.getMultiGraph());
		_shortestPathAlgorithm = new ArrayList<>();
	}
	
	public void comunicate() {
		String input = "";
		BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
		while (true) {
			try {
				input = br.readLine();
			} catch (IOException e) {
				System.out.println("Fehler bein Einlesen des Input. " + e.getMessage());
				e.printStackTrace();
			}
			decodeInput(input);
		}
		
	}
	
	private void showGraphs() {
		if (!(_from == null || _from.isEmpty() || _to == null || _to.isEmpty())) {
			for (ShortestPathAlgorithm algorithm : _shortestPathAlgorithm) {
				
				try {
					List<Path> paths = algorithm.getShortestPath(_from, _to);
					System.out.println(algorithm.getClass() + " fand " + paths.size() + " Pfade:");
					_graph.showGraph(paths);
					System.out.println("In " + algorithm.getStringTimeMilliseconds() + ", " +
					algorithm.getStringTimeNanoseconds() + "\nZugriffe: " + algorithm.getGraphAccess());
				} catch (InvalidGraphException e) {
					System.out.println("Fehler im Graphen:" + e.getMessage());
					e.printStackTrace();
				}
			}
			_shortestPathAlgorithm.clear();
			_from = "";
			_to = "";
			_graphLoaded = false;
		} else {
			System.out.println("Fehler: Start und/oder Ziel nicht angegeben!");
		}
	}
	
	private void decodeInput(String input) {
		if (_parser.matchesRegEx(input) && !(input.charAt(0) == '/')) {
			_parser.addElement(input);
		} else {
			input = input.substring(1, input.length());
			String[] inputParts = input.split("([, ;:])");
			int i = 0;
			String part = "";
			while (i < inputParts.length) {
				try {
					part = inputParts[i];
					part.toLowerCase();
					part.trim();
					if (part == null || part.isEmpty()) {
						i++;
					} else if (part.equals("from") || part.equals("f")) {
						_from = inputParts[i + 1];
						i += 2;
					} else if (part.equals("to") || part.equals("t")) {
						_to = inputParts[i + 1];
						i += 2;
					} else if (part.equals("dijkstra") || part.equals("dk")) {
						_shortestPathAlgorithm.add(new Dijkstra(_graph.getMultiGraph()));
						i++;
					} else if (part.equals("floydwarschall") || part.equals("floyd") || part.equals("fw")) {
						_shortestPathAlgorithm.add(new FloydWarschall(_graph.getMultiGraph()));
						i++;
					} else if (part.equals("breadthfirstsearch") || part.equals("breadth") || part.equals("bfs")) {
						_shortestPathAlgorithm.add(new BreadthFirstSearch(_graph.getMultiGraph()));
						i++;
					} else if (part.substring(part.length() - 4, part.length()).equals(".gka")) {
						try {
							_parser.parseFromFile(part);
						} catch (IOException e) {
							System.out.println("Fehler beim Lesen der Datei: " + e.getMessage());
							e.printStackTrace();
						}
						_graphLoaded = true;
						i++;
					} else if (part.equals("graph") || part.equals("newgraph")) {
						try {
							_graph = new Graph(part, Integer.parseInt(inputParts[i + 1]), Integer.parseInt(inputParts[i + 2]), false);
						} catch (NumberFormatException e) {
							System.out.println("Die Werte für Knoten und Kanten müssen Ganzzahlen sein! " + e.getMessage());
							e.printStackTrace();
						}
						_graphLoaded = true;
						i += 3;
					} else if (part.equals("big") || part.equals("biggraph") || part.equals("graphbig")) {
						try {
							_graph = new Graph(part, Integer.parseInt(inputParts[i + 1]), Integer.parseInt(inputParts[i + 2]), true);
						} catch (NumberFormatException e) {
							System.out.println("Die Werte für Knoten und Kanten müssen Ganzzahlen sein! " + e.getMessage());
							e.printStackTrace();
						}
						_graphLoaded = true;
						i += 3;
					} else if (part.equals("hilfe") || part.equals("help")) {
						System.out.println(_hilfe);
						i++;
					} else {
						System.out.println(_hilfe);
						i++;
					}
				} catch (Exception e) {
					System.out.println("Die Eingabe konnte nicht erkannt werden, Hilfstext mit \"hilfe\" oder \"help\"\n" + e.getMessage());
					i = inputParts.length;
				}
			}
			if (hasAllValues()) {
				showGraphs();
			}
		}
	}
	
	private boolean hasAllValues() {
		return !(_from.isEmpty()) && !(_to.isEmpty()) && _graphLoaded && !(_shortestPathAlgorithm.isEmpty());
	}
}
