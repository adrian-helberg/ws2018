package application;

import application.test.MaxFlowAlgorithm;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.stage.FileChooser;
import org.graphstream.graph.Path;
import application.algorithm.*;
import application.exception.InvalidGraphException;
import application.model.Algorithm;
import java.io.File;
import java.util.List;

/**
 * GKAP Root layout controller
 *
 * @author Adrian Helberg
 * @author Maximilian Janzen
 */
@SuppressWarnings("Duplicates")
public class RootLayoutController {
    private App app;
    private Graph graph = new Graph("graph");
    private Parser parser = new Parser(graph.getMultiGraph());

    void setInstance(App instance) {
        this.app = instance;
    }

    @FXML
    private TextField startField;
    @FXML
    private TextField endField;
    @FXML
    private void handleOpen() {
        FileChooser fileChooser = new FileChooser();
        FileChooser.ExtensionFilter extFilter = new FileChooser.ExtensionFilter(
                "Graph files (*.gka)", "*.gka");
        fileChooser.getExtensionFilters().add(extFilter);
        File file = fileChooser.showOpenDialog(app.getPrimaryStage());

        if (file != null) {
            loadGraphfromFile(file);
        }
    }

    /**
     * Parse file to get graph
     *
     * @param file File to be parsed
     */
    private void loadGraphfromFile(File file) {
        try {
            parser.parseFromFileUi(file);
            app.setFileLoaded(true);
        } catch (Exception e) {
            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setTitle("Error");
            alert.setHeaderText("Unable to read graph");
            alert.setContentText("Unable to read graph from file:\n" + file.getPath());
            alert.showAndWait();
        }
    }

    @FXML
    private void handleRun() {

        if(!app.isFileLoaded()){
            handleOpen();
        }

        String name;
        application.algorithm.Algorithm currentAlgorithm = null;

        if (isInputValid()) {
            // Process algorithms
            for (Algorithm o : app.getAlgorithmData()) {
                name = o.getAlgorithm();

                if (name.contains("Breadth")) {
                    currentAlgorithm = new BreadthFirstSearch(graph.getMultiGraph());
                } else if (name.contains("Dijkstra")) {
                    currentAlgorithm = new Dijkstra(graph.getMultiGraph());
                } else if (name.contains("Floyd")) {
                    currentAlgorithm = new FloydWarschall(graph.getMultiGraph());
                } else if (name.contains("Ford")) {
                    currentAlgorithm = new application.test.FordFulkerson(graph.getMultiGraph());
                } else if (name.contains("Edmonds")) {
                    currentAlgorithm = new application.test.EdmondsKarp(graph.getMultiGraph());
                }

                if (currentAlgorithm == null) {
                    System.out.println("Could not identify algorithm: " + name);
                }

                if (currentAlgorithm instanceof ShortestPathAlgorithm)
                {
                    List<Path> paths = null;

                    try {
                        paths = ((ShortestPathAlgorithm)currentAlgorithm).getShortestPath(startField.getText(), endField.getText());
                    } catch (InvalidGraphException e) {
                        e.printStackTrace();
                    }

                    // Set properties
                    o.setGraph(graph);
                    o.setPath(paths);
                    o.setSource(startField.getText());
                    o.setTarget(endField.getText());
                    o.setTime(currentAlgorithm.getStringTimeMilliseconds() + ", " +
                            currentAlgorithm.getStringTimeNanoseconds());
                    o.setAccess(Integer.toString(currentAlgorithm.getGraphTouches()));

                    int distance = ((ShortestPathAlgorithm) currentAlgorithm).getTotalDistance();
                    o.setDistance("" + distance);
                }
                else if (currentAlgorithm instanceof application.test.MaxFlowAlgorithm)
                {
                    try {
                        o.setMaxFlow("" + ((application.test.MaxFlowAlgorithm)currentAlgorithm).getMaxFlow(startField.getText(), endField.getText()));
                    } catch (InvalidGraphException e) {
                        e.printStackTrace();
                    }

                    // Set properties
                    o.setGraph(graph);
                    o.setSource(startField.getText());
                    o.setTarget(endField.getText());
                    o.setTime(currentAlgorithm.getStringTimeMilliseconds() + ", " +
                            currentAlgorithm.getStringTimeNanoseconds());
                    o.setAccess(Integer.toString(currentAlgorithm.getGraphTouches()));
                }
            }
        }
    }

    private void handleTextArea(Exception e, Alert alert) {
        TextArea textArea = new TextArea(e.getStackTrace().toString());
        textArea.setEditable(false);
        textArea.setWrapText(true);
        textArea.setMaxWidth(Double.MAX_VALUE);
        textArea.setMaxHeight(Double.MAX_VALUE);
        GridPane.setVgrow(textArea, Priority.ALWAYS);
        GridPane.setHgrow(textArea, Priority.ALWAYS);

        GridPane expContent = new GridPane();
        expContent.setMaxWidth(Double.MAX_VALUE);
        expContent.add(textArea, 0, 1);

        alert.getDialogPane().setExpandableContent(expContent);
        alert.showAndWait();
    }

    /**
     * Close application.
     */
    @FXML
    private void handleExit() {
        System.exit(0);
    }

    /**
     * Validate user input
     *
     * @return True if input is valid; False otherwise
     */
    private boolean isInputValid() {
        String errorMessage = "";

        if (startField.getText() == null || startField.getText().length() == 0) {
            errorMessage += "Source node is required\n";
        }
        if (endField.getText() == null || endField.getText().length() == 0) {
            errorMessage += "Target node is required\n";
        }
        if (errorMessage.length() == 0) {
            return true;
        } else {
            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setTitle("Invalid Input");
            alert.setHeaderText("Required input missing");
            alert.setContentText(errorMessage);
            alert.showAndWait();

            return false;
        }
    }
}
