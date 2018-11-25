package sample;

import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.layout.Pane;
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

    private MainApp mainApp;

    @FXML
    private void initialize() {
        algorithmusStringTableColumn.setCellValueFactory(cellData -> cellData.getValue().algorithmusProperty());

        showAlgorithmusDetails(null);

        algorithmusTableView.getSelectionModel().selectedItemProperty().addListener(
                (observable, oldValue, newValue) -> showAlgorithmusDetails(newValue));
    }

    void setMainApp(MainApp mainApp) {
        this.mainApp = mainApp;
        algorithmusTableView.setItems(mainApp.getAlgorithmData());
    }

    private void showAlgorithmusDetails(Algorithmus algo) {
        if (algo != null) {
            startLabel.setText(algo.getStart());
            endLabel.setText(algo.getEnde());
            timeLabel.setText(algo.getTime());
            pathTextArea.setText(pathToString(algo.getPath()));
        } else {
            startLabel.setText("n/a");
           endLabel.setText("n/a");
           timeLabel.setText("n/a");
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
                System.out.println(concat);
                System.out.println(i);
                i += 1;
            }
        }

        return concat;
    }

    @FXML
    private void handleShowGraph() {
        Algorithmus selectedalgo = algorithmusTableView.getSelectionModel().getSelectedItem();
        if (selectedalgo != null) {
            mainApp.showGraphEditDialog(selectedalgo);
        } else {
            Alert alert = new Alert(Alert.AlertType.WARNING);
            alert.initOwner(mainApp.getPrimaryStage());
            alert.setTitle("No algorithm!");
            alert.setHeaderText("No algorithm chosen");
            alert.setContentText("Please select an algorithm");

            alert.showAndWait();
        }
    }
}
