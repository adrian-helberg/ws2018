package sample.algorithmen;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.implementations.MultiGraph;

public class Parser {

	private final static String _source = "source";
	private final static String _edge = "edge";
	private final static String _target = "target";
	private final static String _edgeName = "edgeName";
	private final static String _edgeDistance = "edgeDistance";
	private static final Pattern REGEX_PATTERN = Pattern
			.compile(
					"((?<" + _source + ">\\w+)((((?<" + _edge + ">--|->)" + "(?<" + _target + ">\\w+)){1}(?<"
							+ _edgeName + ">[(]\\w+[)])?(?<" + _edgeDistance + ">:\\d+(\\.\\d+)?)?))?)",
					Pattern.UNICODE_CHARACTER_CLASS);

	private MultiGraph _graph;

	/**
	 * Ein Parser erstellt einen Graphen aus einer Datei
	 */
	public Parser(MultiGraph graph) {
		_graph = graph;
	}

	/**
	 * Erstellt einen Graphen aus den Daten der Datei
	 * 
	 * @param fileName
	 *            Pfad zur Datei
	 * @return MultiGraph
	 * @throws IOException
	 */
	public MultiGraph parseFromFile(String fileName) throws IOException {
		File file = new File(fileName); // Einzulesende Datei
		List<String> input;
		input = Files.lines(file.toPath(), Charset.forName("iso_8859_1")).map(s -> s.replaceAll("\\s+", ""))
				.filter(s -> (!s.isEmpty())).flatMap(s -> Stream.of(s.split(";"))).collect(Collectors.toList());
		for (String line : input) {
			addElement(line);
		}
		System.out.println("Graph erfolgreich aus '" + fileName + "' erstellt.\n");
		return _graph;
	}
	/**
	 * Erstellt einen Graphen aus den Daten der Datei
	 *
	 * @param file
	 *            Pfad zur Datei
	 * @return MultiGraph
	 * @throws IOException
	 */
	public MultiGraph parseFromFileUi(File file) throws IOException {
		List<String> input;
		input = Files.lines(file.toPath(), Charset.forName("iso_8859_1")).map(s -> s.replaceAll("\\s+", ""))
				.filter(s -> (!s.isEmpty())).flatMap(s -> Stream.of(s.split(";"))).collect(Collectors.toList());
		for (String line : input) {
			addElement(line);
		}
		return _graph;
	}
	/**
	 * Zerlegt die Line nach der RegEx in ihre Bestandteile und fuegt diese dem
	 * Graphen hinzu
	 *
	 * @param line
	 *            String
	 */
	public void addElement(String line) {
		if (matchesRegEx(line)) {
			Matcher matcher = REGEX_PATTERN.matcher(line);
			matcher.find(); // Suche nach den Bestandteilen
			String source = matcher.group(_source); // Knoten muss angegeben
													// sein
			String edge = matcher.group(_edge);
			if (edge != null) { // wenn Kante vorhanden
				String target = matcher.group(_target);
				String edgeName = matcher.group(_edgeName);
				String edgeWeight = matcher.group(_edgeDistance); // Kantengewicht
				if (edgeWeight == null) {
					edgeWeight = "1";
				} // Default = 1, steht fuer die Kante
				edgeWeight = edgeWeight.replaceAll(":", "");
				int edgeWeightInt = Integer.parseInt(edgeWeight);
				// Default false, nur true wenn direction explizit angegeben.
				boolean directed = edge.equals("->");
				if (edgeName == null) { // Falls kein Name fuer eine Kante
										// festgelegt ist
					edgeName = source + edge + target; // Name zusammenstellen
				}
				Edge currentEdge = _graph.addEdge(edgeName, source, target, directed); // Kante
																						// einfuegen
				currentEdge.addAttribute(Graph.uiName, edgeWeight); // Kantennamen
																	// angeben
				currentEdge.addAttribute(Graph.uiStyle, Graph.uiColorblue);
				currentEdge.addAttribute(Graph.distance, edgeWeightInt); // Kantengewicht
				currentEdge.addAttribute(Graph.walkTrough, true);	// angeben
				Node sourceNode = currentEdge.getSourceNode();
				sourceNode.addAttribute(Graph.uiName, sourceNode.getId());
				sourceNode.addAttribute(Graph.uiStyle, Graph.uiColorblue);
				Node targetNode = currentEdge.getTargetNode();
				targetNode.addAttribute(Graph.uiName, targetNode.getId());
				targetNode.addAttribute(Graph.uiStyle, Graph.uiColorblue);
			} else {
				if (_graph.getNode(source) == null) { // Sonst nur Knoten
														// hinzufuegen
					_graph.addNode(source);
					Node node = _graph.getNode(source);
					node.addAttribute(Graph.uiName, node.getId());
				}
			}
		}
	}

	/**
	 * Prueft ob der Input auf die Regular Expression passt
	 * 
	 * @param line
	 *            Stringliste
	 * @return entspricht der String der RegEx?
	 */
	public boolean matchesRegEx(String line) {
		return REGEX_PATTERN.matcher(line).matches();
	}
}