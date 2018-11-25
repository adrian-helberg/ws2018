package sample.model;

import javafx.beans.property.SimpleListProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.ObservableList;
import sample.algorithmen.Graph;
import org.graphstream.graph.Path;
import java.util.List;

/**
 * Algorithm model
 * @author Maximilian Janzen & Adrian Helberg
 */
public class Algorithm {
    // Properties
    private final SimpleStringProperty algorithmus;
    private final SimpleListProperty nodes;
    private final SimpleStringProperty source;
    private final SimpleStringProperty target;
    private final SimpleStringProperty graphpath;
    private final SimpleObjectProperty<Graph> graph;
    private final SimpleObjectProperty<List<Path>> path;

    public Algorithm(String algorithmus){
        this.algorithmus = new SimpleStringProperty(algorithmus);
        this.nodes = new SimpleListProperty<>();
        this.source = new SimpleStringProperty("n/a");
        this.target = new SimpleStringProperty("n/a");
        this.graphpath = new SimpleStringProperty();
        this.graph = new SimpleObjectProperty<>();
        this.path = new SimpleObjectProperty<>();
    }

    public String getAlgorithmus() {
        return algorithmus.get();
    }

    public SimpleStringProperty algorithmusProperty() {
        return algorithmus;
    }

    public String getSource() {
        return source.get();
    }

    public void setSource(String start) {
        this.source.set(start);
    }

    public String getTarget() {
        return target.get();
    }

    public void setTarget(String ende) {
        this.target.set(ende);
    }

    public Graph getGraph() {
        return graph.get();
    }

    public void setGraph(Graph graph) {
        this.graph.set(graph);
    }

    public List<Path> getPath() {
        return path.get();
    }

    public void setPath(List<Path> path) {
        this.path.set(path);
    }
}
