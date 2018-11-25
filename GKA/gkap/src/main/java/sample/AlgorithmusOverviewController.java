package sample;

import javafx.fxml.FXML;
import javafx.scene.control.*;
import org.graphstream.graph.Path;
import sample.model.Algorithmus;

import java.util.List;

public class AlgorithmusOverviewController {
    @FXML
    private TableView<Algorithmus> algorithmusTableView;
    @FXML
    private TableColumn<Algorithmus, String> algorithmusStringTableColumn;
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

    private MainApp app;

    @FXML
    private void initialize() {
        algorithmusStringTableColumn.setCellValueFactory(cellData -> cellData.getValue().algorithmusProperty());

        setLabels(null);

        algorithmusTableView.getSelectionModel().selectedItemProperty().addListener(
                (observable, oldValue, newValue) -> setLabels(newValue));
    }

    void setMainApp(MainApp instance) {
        this.app = instance;
        algorithmusTableView.setItems(instance.getAlgorithmData());
    }

    private void setLabels(Algorithmus algo) {
        if (algo != null) {
            startLabel.setText(algo.getStart());
            endLabel.setText(algo.getEnde());
            timeLabel.setText(algo.getTime());
            accessLabel.setText(algo.getAccesses());
            pathTextArea.setText(pathToString(algo.getPath()));
        } else {
            startLabel.setText("n/a");
            endLabel.setText("n/a");
            timeLabel.setText("n/a");
            accessLabel.setText("n/a");
            pathTextArea.setText("n/a");
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
        Algorithmus selectedalgo = algorithmusTableView.getSelectionModel().getSelectedItem();
        if (selectedalgo != null) {
            app.showGraphEditDialog(selectedalgo);
        } else {
            Alert alert = new Alert(Alert.AlertType.WARNING);
            alert.initOwner(app.getPrimaryStage());
            alert.setTitle("No algorithm!");
            alert.setHeaderText("No algorithm chosen");
            alert.setContentText("Please select an algorithm");

            alert.showAndWait();
        }
    }
}
