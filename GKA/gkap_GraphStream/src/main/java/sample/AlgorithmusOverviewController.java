package sample;

import javafx.fxml.FXML;
import javafx.scene.control.Alert;
import javafx.scene.control.Label;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Pane;
import org.graphstream.graph.Path;
import sample.model.Algorithmus;

import java.util.List;

/**
 * Created by Work on 11.05.2018.
 */
public class AlgorithmusOverviewController {
    @FXML
    private TableView<Algorithmus> algorithmusTableView;
    @FXML
    private TableColumn<Algorithmus, String> algorithmusStringTableColumn;

    @FXML
    private Label graphLabel;
    @FXML
    private Label startLabel;
    @FXML
    private Label endLabel;
    @FXML
    private Label timeLabel;
    @FXML
    private Label pathLabel;
    @FXML
    private Pane pane;

    // Reference to the main application.
    private MainApp mainApp;

    /**
     * The constructor.
     * The constructor is called before the initialize() method.
     */
    public AlgorithmusOverviewController() {
    }

    /**
     * Initializes the controller class. This method is automatically called
     * after the fxml file has been loaded.
     */
    @FXML
    private void initialize() {
        // Initialize the Algorithmus table with the one column.
        algorithmusStringTableColumn.setCellValueFactory(cellData -> cellData.getValue().algorithmusProperty());

        // Clear algo details.
        showAlgorithmusDetails(null);

        // Listen for selection changes and show the Algo details when changed.
        algorithmusTableView.getSelectionModel().selectedItemProperty().addListener(
                (observable, oldValue, newValue) -> showAlgorithmusDetails(newValue));
    }


    /**
     * Is called by the main application to give a reference back to itself.
     *
     * @param mainApp
     */
    public void setMainApp(MainApp mainApp) {
        this.mainApp = mainApp;

        // Add observable list data to the table
        algorithmusTableView.setItems(mainApp.getAlgorithmData());
    }


    /**
     * Fills all text fields to show details about the Algorithmus.
     * If the specified Algorithmus is null, all text fields are cleared.
     *
     * @param algo the Algorithmus or null
     */
    private void showAlgorithmusDetails(Algorithmus algo) {
        if (algo != null) {
            // Fill the labels with info from the Algorithmus object.

            startLabel.setText(algo.getStart());
            endLabel.setText(algo.getEnde());
            timeLabel.setText(algo.getTime());
            pathLabel.setText(pathToString(algo.getPath()));
        } else {
            startLabel.setText("");
           endLabel.setText("");
           timeLabel.setText("");
           pathLabel.setText("Hier würde der Pfad stehen");
        }
    }
/**
 * Convert Path to String
 */
public String pathToString(List<Path> algoList){
    int max = algoList.size();
    int i = 0;
    String concat = "";
    if (max==0){
        return "Hier würde der Pfad stehen";
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
     * Called when the user clicks the edit button. Opens a dialog to edit
     * details for the selected person.
     */
    @FXML
    private void handleShowGraph() {
        Algorithmus selectedalgo = algorithmusTableView.getSelectionModel().getSelectedItem();
        if (selectedalgo != null) {
            mainApp.showGraphEditDialog(selectedalgo);

        } else {
            // Nothing selected.
            Alert alert = new Alert(Alert.AlertType.WARNING);
            alert.initOwner(mainApp.getPrimaryStage());
            alert.setTitle("Keine Auswahl getroffen!");
            alert.setHeaderText("Es wurde kein Algorithmus ausgewählt");
            alert.setContentText("Bitte wähle einen aus der Liste");

            alert.showAndWait();
        }
    }
}
