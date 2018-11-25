package sample.model;

import javafx.beans.property.SimpleListProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.ObservableList;
import sample.algorithmen.Graph;
import org.graphstream.graph.Path;
import java.util.List;

/**
 * @author Maximilian Janzen & Adrian Helberg
 */
public class Algorithmus {

    private final SimpleStringProperty algorithmus;
    private final SimpleListProperty nodes;
    private final SimpleStringProperty start;
    private final SimpleStringProperty ende;
    private final SimpleStringProperty graphpath;
    private final SimpleStringProperty time;
    private final SimpleStringProperty access;
    private final SimpleObjectProperty<Graph> graph;
    private final SimpleObjectProperty<List<Path>> path;


    /**
     * Default constructor.
     */
    public Algorithmus() {
        this(null);
    }

    /**
     * Constructor
     *
     * @param algorithmus Name als String
     */
    public Algorithmus(String algorithmus){
        this.algorithmus = new SimpleStringProperty(algorithmus);
        this.nodes = new SimpleListProperty<>();
        this.start = new SimpleStringProperty("n/a");
        this.ende = new SimpleStringProperty("n/a");
        this.graphpath = new SimpleStringProperty("n/a");
        this.time = new SimpleStringProperty("n/a");
        this.access = new SimpleStringProperty("n/a");
        this.graph = new SimpleObjectProperty<>();
        this.path = new SimpleObjectProperty<>();
    }

    public String getAlgorithmus() {
        return algorithmus.get();
    }

    public SimpleStringProperty algorithmusProperty() {
        return algorithmus;
    }

    public void setAlgorithmus(String algorithmus) {
        this.algorithmus.set(algorithmus);
    }

    public Object getNodes() {
        return nodes.get();
    }

    public SimpleListProperty nodesProperty() {
        return nodes;
    }

    public void setNodes(Object nodes) {
        this.nodes.set((ObservableList) nodes);
    }

    public String getStart() {
        return start.get();
    }

    public String getAccesses() {
        return access.get();
    }

    public SimpleStringProperty startProperty() {
        return start;
    }

    public void setStart(String start) {
        this.start.set(start);
    }

    public void setAccess(String access) {
        this.access.set(access);
    }

    public String getEnde() {
        return ende.get();
    }

    public SimpleStringProperty endeProperty() {
        return ende;
    }

    public void setEnde(String ende) {
        this.ende.set(ende);
    }

    public String getGraphpath() {
        return graphpath.get();
    }

    public SimpleStringProperty graphpathProperty() {
        return graphpath;
    }

    public void setGraphpath(String graphpath) {
        this.graphpath.set(graphpath);
    }

    public String getTime() {
        return time.get();
    }

    public SimpleStringProperty timeProperty() {
        return time;
    }

    public void setTime(String time) {
        this.time.set(time);
    }

    public Graph getGraph() {
        return graph.get();
    }

    public SimpleObjectProperty<Graph> graphProperty() {
        return graph;
    }

    public void setGraph(Graph graph) {
        this.graph.set(graph);
    }

    public List<Path> getPath() {
        return path.get();
    }

    public SimpleObjectProperty<List<Path>> pathProperty() {
        return path;
    }

    public void setPath(List<Path> path) {
        this.path.set(path);
    }
}
