package application.algorithm;

import java.util.LinkedList;
import application.exception.InvalidGraphException;
import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.implementations.MultiGraph;

/**
 * GKAP Ford Fulkerson Algorithm
 *
 * @author Adrian Helberg
 * @author Maximilian Janzen
 * @see <a href="https://en.wikipedia.org/wiki/Ford%E2%80%93Fulkerson_algorithm">Ford Fulkerson AAlgorithm</a>
 */
public class FordFulkerson extends MaxFlowAlgorithm {

	public FordFulkerson(MultiGraph graph, String source, String target) {
		this.graph = graph;
		this.source = source;
		this.target = target;
	}

	/**
	 * Compute and set max flow
	 * @throws InvalidGraphException If
	 */
	@Override
	public void compute() throws InvalidGraphException {
		// Get nodes from string identifier
		Node source = graph.getNode(this.source);
		Node sink = graph.getNode(this.target);

		// Validate nodes
		if (source == null)
			throw new InvalidGraphException("Source node not found");

		if (sink == null)
			throw new InvalidGraphException("Sink node not found");

		// Validate capacity
		checkArrays();
		// Prepare capacities
		loadCapacitiesFromAttribute();

		// Iterate edges
		for (int i = 0; i < graph.getEdgeCount(); i++) {
			Edge e = graph.getEdge(i);

			// Set all capacities to zero
			setFlow((Node) e.getNode0(), e.getNode1(), 0);
			setFlow((Node) e.getNode1(), e.getNode0(), 0);
		}

		// Minimum capacity flow
		int minCf;
		LinkedList<Node> path = new LinkedList<>();

		// Process until every capacity is set
		while ((minCf = findPath(path, source, sink)) > 0) {
			for (int i = 1; i < path.size(); i++) {
				Node u = path.get(i - 1);
				Node v = path.get(i);

				setFlow(u, v, getFlow(u, v) + minCf);
				setFlow(v, u, getFlow(v, u) - minCf);
			}

			path.clear();
		}

		int flow = 0;

		for (int i = 0; i < source.getDegree(); i++) {
			flow += getFlow(source, source.getEdge(i).getOpposite(source));
		}

		maximumFlow = flow;
	}

	/**
	 * Find path and return minimum capacity flow
	 * @param path Path
	 * @param source Source node
	 * @param target Sink node
	 * @return Minimum capacity flow
	 */
	int findPath(LinkedList<Node> path, Node source, Node target) {
		path.addLast(source);

		if (source == target)
			return Integer.MAX_VALUE;

		int minCf;

		// Iterate connected edges
		for (int i = 0; i < source.getDegree(); i++) {
			Edge e = source.getEdge(i);
			Node o = e.getOpposite(source);

			// Get minimum capacity flow
			try {
				if (getCapacity(source, o) - getFlow(source, o) > 0	&& !path.contains(o)) {
					if ((minCf = findPath(path, o, target)) > 0)
						return Math.min(minCf, getCapacity(source, o) - getFlow(source, o));
				}
			} catch (InvalidGraphException e1) {
				e1.printStackTrace();
			}
		}

		path.removeLast();
		return 0;
	}
}
