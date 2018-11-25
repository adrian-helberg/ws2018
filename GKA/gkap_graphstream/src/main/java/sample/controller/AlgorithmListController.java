package sample.controller;

import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextArea;
import org.graphstream.graph.Path;
import sample.App;
import sample.model.Algorithm;

import java.io.IOException;
import java.util.List;

/**
 * Algorithm list controller
 * @author Maximilian Janzen & Adrian Helberg
 */
public class AlgorithmListController {
    @FXML
    private TableView<Algorithm> algorithmList;
    @FXML
    private TableColumn<Algorithm, String> algorithmListColumn;
    @FXML
    private Label source;
    @FXML
    private Label target;
    @FXML
    private TextArea path;
    private App app;

    /**
     * Initializes the controller class. This method is automatically called
     * after the fxml file has been loaded.
     */
    @FXML
    private void initialize() {
        algorithmListColumn.setCellValueFactory(cellData -> cellData.getValue().algorithmusProperty());

        // Clear Labels
        setLables(null);

        algorithmList.getSelectionModel().selectedItemProperty().addListener(
                (observable, oldValue, newValue) -> setLables(newValue)
        );
    }

    public void setInstance(App instance) {
        this.app = instance;
        algorithmList.setItems(app.getAlgorithmList());
    }

    /**
     * Fills all text fields to show details about the Algorithmus.
     * If the specified Algorithmus is null, all text fields are cleared.
     *
     * @param algorithm Algorithmus or null
     */
    private void setLables(Algorithm algorithm) {
        if (algorithm != null) {
            source.setText(algorithm.getSource());
            target.setText(algorithm.getTarget());
            path.setText(pathToString(algorithm.getPath()));
        } else {
            source.setText("n/a");
            target.setText("n/a");
            path.setText("n/a");
        }
    }

    /**
     * Convert Path to String
     */
    private String pathToString(List<Path> algoList){
        int max = algoList.size();
        int i = 0;
        String concat = "";
        if (max==0){
            return "n/a";
        } else {
            while (i < max) {
                concat += algoList.get(i);
                System.out.println(concat);
                System.out.println(i);
                i += 1;
            }
        }

        return concat;
    }

    /**
     * Show graph
     * @throws IOException If no graph selected
     */
    @FXML
    private void handleShowGraph() throws IOException {
        Algorithm selected = algorithmList.getSelectionModel().getSelectedItem();
        if (selected == null) throw new IOException("No graph selected");
        app.showGraphEditDialog(selected);
    }
}
