package application.algorithm;

import application.exception.InvalidGraphException;
import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;

import java.util.List;

/**
 * Basic Max FLow Algorithm
 *
 * @author Adrian Helberg
 * @author Maximilian Janzen
 */
public abstract class MaxFlowAlgorithm extends Algorithm {
	// Capacities of edges
	int[] capacities;
	// Flows of edges
	int[] flows;
	// Maximum flow
	int maximumFlow;
	//Edge count
	int n;
	// Capacity attribute
	String capacityAttribute;
	// Source and Sink
	String source, target;

	/**
	 * Get maximum flow
	 * @return Maximum flow
	 */
	public int getMaximumFlow() {
		return maximumFlow;
	}

	/**
	 * Get current flow by source and sink string identifier
	 * @param source Source identifier
	 * @param target Sink identifier
	 * @return Current flow value
	 */
	int getFlow(String source, String target) {
		Node s = graph.getNode(source);
		Node t = graph.getNode(target);

		return getFlow(s, t);
	}

	/**
	 * Get current flow by source and sink node
	 * @param source Source node
	 * @param target Sink node
	 * @return Current flow value
	 */
	int getFlow(Node source, Node target) {
		Edge e = source.getEdgeBetween(target);

		if (e.getSourceNode() == source) {
			return flows[e.getIndex()];
		} else {
			return flows[e.getIndex() + n];
		}
	}

	/**
	 * Set flow by source and sink string identifier
	 * @param source Source identifier
	 * @param target Sink identifier
	 * @param flow Flow value
	 */
	void setFlow(String source, String target, int flow) {
		Node s = graph.getNode(source);
		Node t = graph.getNode(target);

		setFlow(s, t, flow);
	}

	/**
	 * Set flow by source and sink node
	 * @param source Source node
	 * @param target Sink node
	 * @param flow Flow value
	 */
	void setFlow(Node source, Node target, int flow) {
		Edge e = source.getEdgeBetween(target);

		if (e.getSourceNode() == source)
			flows[e.getIndex()] = flow;
		else
			flows[e.getIndex() + n] = flow;
	}

	/**
	 * Get capacity by source and sink string identifier
	 * @param source Source identifier
	 * @param target Sink identifier
	 * @return Capacity value
	 * @throws InvalidGraphException If no edge between source and sink
	 */
	int getCapacity(String source, String target) throws InvalidGraphException {
		Node s = graph.getNode(source);
		Node t = graph.getNode(target);

		return getCapacity(s, t);
	}

	/**
	 * Get capacity by source and sink node
	 * @param source Source node
	 * @param target Sink node
	 * @return Capacity value
	 * @throws InvalidGraphException If no edge between source and sink
	 */
	int getCapacity(Node source, Node target) throws InvalidGraphException {
		Edge e = source.getEdgeBetween(target);

		if (e == null) throw new InvalidGraphException("No edge between " + source + " and " + target);

		if (e.getSourceNode() == source)
			return capacities[e.getIndex()];
		else
			return capacities[e.getIndex() + n];
	}

	/**
	 * Set capacity by source and sink string identifier
	 * @param source Source identifier
	 * @param target Sink identifier
	 * @param capacity Capacity value
	 */
	void setCapacity(String source, String target, int capacity) {
		Node s = graph.getNode(source);
		Node t = graph.getNode(target);

		setCapacity(s, t, capacity);
	}

	/**
	 * Set capacity by souce and sink node
	 * @param source Source node
	 * @param target Sink node
	 * @param capacity Capacity value
	 */
	void setCapacity(Node source, Node target, int capacity) {
		Edge e = source.getEdgeBetween(target);

		if (e.getSourceNode() == source)
			capacities[e.getIndex()] = capacity;
		else
			capacities[e.getIndex() + n] = capacity;
	}

	/**
	 * Set all capacities
	 * @param value Capacity value
	 */
	void setAllCapacities(int value) {
		for (int i = 0; i < 2 * n; i++)
			capacities[i] = value;
	}

	/**
	 * Check if capacities array is too large to compute
	 * @throws InvalidGraphException If capacity or flow array too large
	 */
	void checkArrays() throws InvalidGraphException {
		n = graph.getEdgeCount();

		if (capacities == null || capacities.length < 2 * n) {
			capacities = new int[2 * n];
			flows = new int[2 * n];
		} else {
			throw new InvalidGraphException("Capacity array too large to compute");
		}
	}

	/**
	 * Load capacities from attribute
	 * @throws InvalidGraphException If unknown attribute
	 */
	void loadCapacitiesFromAttribute() throws InvalidGraphException {
		// No capacity attribute
		if (capacityAttribute == null) return;

		Edge e;
		// Iterate capacities
		for (int i = 0; i < n; i++) {
			// Reset capacity
			capacities[i] = 0;
			capacities[i + n] = 0;

			e = graph.getEdge(i);

			if (e.hasNumber(capacityAttribute)) {
				capacities[i] = (int)e.getNumber(capacityAttribute);
			} else if (e.hasVector(capacityAttribute)) {
				List<? extends Number> capVect = graph.getEdge(i)
						.getVector(capacityAttribute);

				if (capVect.size() > 0)
					capacities[i] = capVect.get(0).intValue();
				if (capVect.size() > 1)
					capacities[i + n] = capVect.get(1).intValue();

			} else if (e.hasArray(capacityAttribute)) {
				Object[] capArray = e.getArray(capacityAttribute);

				if (capArray.length > 0)
					capacities[i] = ((Number) capArray[0]).intValue();
				if (capArray.length > 1)
					capacities[i + n] = ((Number) capArray[1]).intValue();
			} else if (e.hasAttribute(capacityAttribute)) {
				throw new InvalidGraphException("Unknown capacity attribute");
			}
		}
	}

	/**
	 * Compute max flow; Should be overwritten
	 * @throws InvalidGraphException
	 */
	void compute() throws InvalidGraphException {
		throw new InvalidGraphException("Method compute should be overwritten");
	}
}
