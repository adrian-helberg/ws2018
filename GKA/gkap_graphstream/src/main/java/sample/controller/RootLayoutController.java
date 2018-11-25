package sample.controller;

import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.stage.FileChooser;
import org.graphstream.graph.Path;
import sample.App;
import sample.algorithmen.*;
import sample.exception.InvalidGraphException;
import sample.model.Algorithm;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Root layout controller
 * @author Maximilian Janzen & Adrian Helberg
 */
public class RootLayoutController {
    @FXML
    private TextField sourceNode;
    @FXML
    private TextField targetNode;

    // Application instance
    private App app;

    private Graph graph = new Graph("graph");
    private Parser parser = new Parser(graph.getMultiGraph());
    private ArrayList<ShortestPathAlgorithm> shortestPathAlgorithmList = new ArrayList<>();
    // Flag for existing file
    private boolean fileLoaded = false;

    /**
     * Set reference to application instance
     *
     * @param instance Application instance
     */
    public void setInstance(App instance) {
        app = instance;
    }

    /**
     * Open FileChooser
     */
    @FXML
    private void handleOpen() {
        FileChooser fileChooser = new FileChooser();

        // Filter for GKA files
        FileChooser.ExtensionFilter extFilter = new FileChooser.ExtensionFilter(
                "Graph files (*.gka)", "*.gka"
        );
        fileChooser.getExtensionFilters().add(extFilter);

        // Show file chooser
        File file = fileChooser.showOpenDialog(app.getStage());

        if (file != null) {
            loadGraphFromFile(file);
        }
    }

    /**
     * Load graph from specified file.
     *
     * @param file File to be loaded from
     */
    private void loadGraphFromFile(File file) {
        try {
            parser.parseFromFileUi(file);
            fileLoaded = true;
        } catch (Exception e) {
            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setTitle("Error");
            alert.setHeaderText("Unable to load graph");
            alert.setContentText("Unable to read graph from file:\n" + file.getPath());
            alert.showAndWait();
        }
    }

    /**
     * Run algorithm
     */
    @FXML
    private void handleRun() throws InvalidGraphException {
        if (!fileLoaded){
            handleOpen();
        }

        if (isInputValid()) {
            for (Algorithm a : app.getAlgorithmList()) {

                String name = a.getAlgorithmus();

                if (name.contains("dijkstra")) shortestPathAlgorithmList.add(new Dijkstra(graph.getMultiGraph()));
                if (name.contains("floyd")) shortestPathAlgorithmList.add(new FloydWarschall(graph.getMultiGraph()));
                if (name.contains("Breadth")) shortestPathAlgorithmList.add(new BreadthFirstSearch(graph.getMultiGraph()));

                for (ShortestPathAlgorithm algorithm : shortestPathAlgorithmList) {
                    List<Path> paths = algorithm.getShortestPath(sourceNode.getText(), targetNode.getText());
                    a.setGraph(graph);
                    a.setPath(paths);
                    a.setSource(sourceNode.getText());
                    a.setTarget(targetNode.getText());
                }

                shortestPathAlgorithmList.clear();
            }
        }
    }

    /**
     * Closes the application.
     */
    @FXML
    private void handleExit() {
        System.exit(0);
    }

    /**
     * Validates the user input in the text fields.
     *
     * @return true if the input is valid
     */
    private boolean isInputValid() {
        String errorMessage = "";

        if (sourceNode.getText() == null || sourceNode.getText().length() == 0) {
            errorMessage += "Source node is required\n";
        }

        if (targetNode.getText() == null || targetNode.getText().length() == 0) {
            errorMessage += "Target node is required\n";
        }

        if (errorMessage.equals("")) {
            return true;
        }

        Alert alert = new Alert(Alert.AlertType.ERROR);
        alert.setTitle("IO Error");
        alert.setContentText(errorMessage);
        alert.showAndWait();

        return false;
    }
}
