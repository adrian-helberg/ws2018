package application;

import javafx.fxml.FXML;
import javafx.scene.control.*;
import org.graphstream.graph.Path;
import application.model.Algorithm;
import java.util.List;

/**
 * GKAP Overview layout controller
 *
 * @author Adrian Helberg
 * @author Maximilian Janzen
 */
public class OverviewLayoutController {
    @FXML
    private TableView<Algorithm> algorithmusTableView;
    @FXML
    private TableColumn<Algorithm, String> algorithmusStringTableColumn;
    @FXML
    private Label startLabel;
    @FXML
    private Label endLabel;
    @FXML
    private Label timeLabel;
    @FXML
    private TextArea pathTextArea;
    @FXML
    private Label accessLabel;
    @FXML
    private Label distanceLabel;

    private App app;

    @FXML
    private void initialize() {
        algorithmusStringTableColumn.setCellValueFactory(cellData -> cellData.getValue().algorithmProperty());

        setLabels(null);

        algorithmusTableView.getSelectionModel().selectedItemProperty().addListener(
                (observable, oldValue, newValue) -> {
                    if (app.isFileLoaded()) setLabels(newValue);
                });
    }

    void setInstance(App instance) {
        this.app = instance;
        algorithmusTableView.setItems(instance.getAlgorithmData());
    }

    private void setLabels(Algorithm algo) {
        if (algo != null) {
            startLabel.setText(algo.getSource());
            endLabel.setText(algo.getTarget());
            timeLabel.setText(algo.getTime());
            accessLabel.setText(algo.getAccesses());
            pathTextArea.setText(pathToString(algo.getPath()));
            distanceLabel.setText(algo.getDistance());
        } else {
            startLabel.setText("n/a");
            endLabel.setText("n/a");
            timeLabel.setText("n/a");
            accessLabel.setText("n/a");
            pathTextArea.setText("n/a");
            distanceLabel.setText("n/a");
        }
    }

    private String pathToString(List<Path> algoList){
        int max = algoList.size();
        int i = 0;
        String concat = "";

        if (max==0){
            return "n/a";
        } else {
            while (i < max) {
                concat += algoList.get(i);
                i += 1;
            }
        }

        return concat;
    }

    @FXML
    private void handleShowGraph() {
        Algorithm selectedalgo = algorithmusTableView.getSelectionModel().getSelectedItem();
        if (selectedalgo != null) {
            app.showGraphEditDialog(selectedalgo);
        } else {
            Alert alert = new Alert(Alert.AlertType.WARNING);
            alert.initOwner(app.getPrimaryStage());
            alert.setTitle("No algorithm");
            alert.setHeaderText("No algorithm chosen");
            alert.setContentText("Please select algorithm");

            alert.showAndWait();
        }
    }
}
