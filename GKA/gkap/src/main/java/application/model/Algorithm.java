package application.model;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import application.algorithm.Graph;
import org.graphstream.graph.Path;
import java.util.List;

/**
 * GKAP Algorithm model
 *
 * @author Adrian Helberg
 * @author Maximilian Janzen
 */
public class Algorithm {
    // Properties
    private final SimpleStringProperty algorithm;
    private final SimpleStringProperty source;
    private final SimpleStringProperty target;
    private final SimpleStringProperty time;
    private final SimpleStringProperty access;
    private final SimpleStringProperty distance;
    private final SimpleStringProperty maxFlow;
    private final SimpleObjectProperty<Graph> graph;
    private final SimpleObjectProperty<List<Path>> path;

    public Algorithm(String algorithm){
        this.algorithm = new SimpleStringProperty(algorithm);
        this.source = new SimpleStringProperty("n/a");
        this.target = new SimpleStringProperty("n/a");
        this.time = new SimpleStringProperty("n/a");
        this.access = new SimpleStringProperty("n/a");
        this.distance = new SimpleStringProperty("n/a");
        this.maxFlow = new SimpleStringProperty("n/a");
        this.graph = new SimpleObjectProperty<>(null);
        this.path = new SimpleObjectProperty<>(null);
    }

    // --- Algorithm
    public String getAlgorithm() {
        return algorithm.get();
    }

    public void setAlgorithm(String algorithm) {
        this.algorithm.set(algorithm);
    }

    public SimpleStringProperty algorithmProperty() {
        return algorithm;
    }

    // --- Source
    public String getSource() {
        return source.get();
    }

    public void setSource(String source) {
        this.source.set(source);
    }

    // --- Access
    public String getAccesses() {
        return access.get();
    }

    public void setAccess(String access) {
        this.access.set(access);
    }

    // --- Target
    public String getTarget() {
        return target.get();
    }

    public void setTarget(String target) {
        this.target.set(target);
    }

    // --- Time
    public String getTime() {
        return time.get();
    }

    public void setTime(String time) {
        this.time.set(time);
    }

    // --- Distance
    public String getDistance() {
        return distance.get();
    }

    public void setDistance(String distance) {
        this.distance.set(distance);
    }

    // --- Max Flow
    public String getMaxFlow() {
        return maxFlow.get();
    }

    public void setMaxFlow(String maxFlow) {
        this.maxFlow.set(maxFlow);
    }

    // --- Graph
    public Graph getGraph() {
        return graph.get();
    }

    public void setGraph(Graph graph) {
        this.graph.set(graph);
    }

    // --- Path
    public List<Path> getPath() {
        return path.get();
    }

    public void setPath(List<Path> path) {
        this.path.set(path);
    }
}
