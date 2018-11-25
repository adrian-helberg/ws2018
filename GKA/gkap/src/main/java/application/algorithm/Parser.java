package application.algorithm;

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

/**
 * GKAP Parser
 * @author Maximilian Janzen & Adrian Helberg
 */
public class Parser {

	private final static String _source = "source";
	private final static String _edge = "edge";
	private final static String _target = "target";
	private final static String _edgeName = "edgeName";
	private final static String _edgeDistance = "edgeDistance";
	private static final Pattern REGEX_PATTERN = Pattern.compile(
		"((?<" + _source + ">\\w+)((((?<" + _edge + ">--|->)" + "(?<" + _target + ">\\w+)){1}(?<"
				+ _edgeName + ">[(]\\w+[)])?(?<" + _edgeDistance + ">:\\d+(\\.\\d+)?)?))?)",
		Pattern.UNICODE_CHARACTER_CLASS);

	private MultiGraph _graph;

	public Parser(MultiGraph graph) {
		_graph = graph;
	}

	MultiGraph parseFromFile(String fileName) throws IOException {
		File file = new File(fileName);
		List<String> input;
		input = Files.lines(file.toPath(), Charset.forName("iso_8859_1")).map(s -> s.replaceAll("\\s+", ""))
				.filter(s -> (!s.isEmpty())).flatMap(s -> Stream.of(s.split(";"))).collect(Collectors.toList());
		for (String line : input) {
			addElement(line);
		}
		return _graph;
	}

	public MultiGraph parseFromFileUi(File file) throws IOException {
		List<String> input;
		input = Files.lines(file.toPath(), Charset.forName("iso_8859_1")).map(s -> s.replaceAll("\\s+", ""))
				.filter(s -> (!s.isEmpty())).flatMap(s -> Stream.of(s.split(";"))).collect(Collectors.toList());
		for (String line : input) {
			addElement(line);
		}
		return _graph;
	}

	private void addElement(String line) {
		if (matchesRegEx(line)) {
			Matcher matcher = REGEX_PATTERN.matcher(line);
			matcher.find();
			String source = matcher.group(_source);
			String edge = matcher.group(_edge);
			if (edge != null) {
				String target = matcher.group(_target);
				String edgeName = matcher.group(_edgeName);
				String edgeWeight = matcher.group(_edgeDistance);
				if (edgeWeight == null) {
					edgeWeight = "1";
				}
				edgeWeight = edgeWeight.replaceAll(":", "");
				int edgeWeightInt = Integer.parseInt(edgeWeight);
				boolean directed = edge.equals("->");
				if (edgeName == null) {
					edgeName = source + edge + target;
				}
				Edge currentEdge = _graph.addEdge(edgeName, source, target, directed);
				currentEdge.addAttribute(Graph.uiName, edgeWeight);
				currentEdge.addAttribute(Graph.uiStyle, Graph.uiColorblue);
				currentEdge.addAttribute(Graph.distance, edgeWeightInt);
				currentEdge.addAttribute(Graph.walkTrough, true);
				Node sourceNode = currentEdge.getSourceNode();
				sourceNode.addAttribute(Graph.uiName, sourceNode.getId());
				sourceNode.addAttribute(Graph.uiStyle, Graph.uiColorblue);
				Node targetNode = currentEdge.getTargetNode();
				targetNode.addAttribute(Graph.uiName, targetNode.getId());
				targetNode.addAttribute(Graph.uiStyle, Graph.uiColorblue);
			} else {
				if (_graph.getNode(source) == null) {
					_graph.addNode(source);
					Node node = _graph.getNode(source);
					node.addAttribute(Graph.uiName, node.getId());
				}
			}
		}
	}

	private boolean matchesRegEx(String line) {
		return REGEX_PATTERN.matcher(line).matches();
	}
}