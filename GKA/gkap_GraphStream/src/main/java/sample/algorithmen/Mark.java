package sample.algorithmen;

import org.graphstream.graph.Node;

/**
 * @author Maximilian Janzen & Adrian Helberg
 */
public class Mark {
	protected Node previousNode;
	protected boolean markPositive = true;
	public Mark(Node _previousNode, boolean _markPositive) {
		previousNode = _previousNode;
		markPositive = _markPositive;
	}
}
