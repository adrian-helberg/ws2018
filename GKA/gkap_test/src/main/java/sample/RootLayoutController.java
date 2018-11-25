package sample;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.stage.FileChooser;
import org.graphstream.graph.Path;
import sample.algorithmen.*;
import sample.exception.InvalidGraphException;
import sample.model.Algorithmus;
import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

public class RootLayoutController {
    @FXML
    private TextField startField;
    @FXML
    private TextField endField;
    @FXML
    private ProgressBar progressBar = new ProgressBar();
    private MainApp mainApp;
    private Graph _graph = new Graph("graph");
    private Parser _parser = new Parser(_graph.getMultiGraph());
    private ArrayList<ShortestPathAlgorithm> _shortestPathAlgorithm = new ArrayList<>();
    private ObservableList<Algorithmus> algo = FXCollections.observableArrayList();
    private boolean fileloaded = false;

    void setMainApp(MainApp mainApp) {
        this.mainApp = mainApp;
    }

    @FXML
    private void handleOpen() {
        FileChooser fileChooser = new FileChooser();

        // Set extension filter
        FileChooser.ExtensionFilter extFilter = new FileChooser.ExtensionFilter(
                "Graph files (*.gka)", "*.gka");
        fileChooser.getExtensionFilters().add(extFilter);

        // Show open file dialog
        File file = fileChooser.showOpenDialog(mainApp.getPrimaryStage());

        if (file != null) {
            loadGraphfromFile(file);
        }
    }

    private void loadGraphfromFile(File file) {
        try {
            _parser.parseFromFileUi(file);
            fileloaded = true;
        } catch (Exception e) { // catches ANY exception
            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setTitle("Error");
            alert.setHeaderText("Unable to read graph");
            alert.setContentText("Unable to read graph from file:\n" + file.getPath());
            alert.showAndWait();
        }
    }

    @FXML
    private void handleStart() {

        if(!fileloaded){
            handleOpen();
        }

        int increment= mainApp.getAlgorithmData().size();

        // Calculation to increment the Progressbar correct
        double prow=100/increment;
        double schritt= prow/100;
        String errorMessage="";
        double i=schritt;
        if (isInputValid()) {

            for (Algorithmus o : mainApp.getAlgorithmData()) {
                String algorithmusname = o.getAlgorithmus();
                String algorithmusnamenew = algorithmusname;
                String[] inputParts = algorithmusnamenew.split("([, ;:])");
                String part = inputParts[0].toLowerCase().trim();

                if (part.equals("dijkstra") || part.equals("dk")) {
                    _shortestPathAlgorithm.add(new Dijkstra(_graph.getMultiGraph()));
                } else if (part.equals("floydwarschall") || part.equals("floyd") || part.equals("fw")) {
                    _shortestPathAlgorithm.add(new FloydWarschall(_graph.getMultiGraph()));
                } else if (part.equals("breadthfirstsearch") || part.equals("breadth") || part.equals("bfs")) {
                    _shortestPathAlgorithm.add(new BreadthFirstSearch(_graph.getMultiGraph()));
                } else {
                    errorMessage += algorithmusname + "\n";
                }

                for (ShortestPathAlgorithm algorithm : _shortestPathAlgorithm) {
                    try {
                        List<Path> paths = algorithm.getShortestPath(startField.getText(), endField.getText());
                        o.setGraph(_graph);
                        o.setPath(paths);
                        o.setStart(startField.getText());
                        o.setEnde(endField.getText());
                        o.setTime(algorithm.getStringTimeMilliseconds() + ", " +
                                algorithm.getStringTimeNanoseconds());
                        if (i<=1){
                            progressBar.setProgress(i);
                            i+=schritt;
                        }else{
                            progressBar.setProgress(1);
                        }
                        // _graph.showGraph(paths);
                    } catch (InvalidGraphException e) {
                        Alert alert = new Alert(Alert.AlertType.ERROR);
                        alert.setTitle("Error");
                        alert.setHeaderText("Graph konnte nicht bearbeitet werden");
                        alert.setContentText("Fehler im Graphen:\n" + e.getMessage());
                        // StackTrace ausgeben
                        // Create expandable Exception.
                        StringWriter sw = new StringWriter();
                        PrintWriter pw = new PrintWriter(sw);
                        e.printStackTrace(pw);
                        String exceptionText = sw.toString();

                        Label label = new Label("Der exception Stack trace ist:");

                        TextArea textArea = new TextArea(exceptionText);
                        textArea.setEditable(false);
                        textArea.setWrapText(true);

                        textArea.setMaxWidth(Double.MAX_VALUE);
                        textArea.setMaxHeight(Double.MAX_VALUE);
                        GridPane.setVgrow(textArea, Priority.ALWAYS);
                        GridPane.setHgrow(textArea, Priority.ALWAYS);

                        GridPane expContent = new GridPane();
                        expContent.setMaxWidth(Double.MAX_VALUE);
                        expContent.add(label, 0, 0);
                        expContent.add(textArea, 0, 1);

                        alert.getDialogPane().setExpandableContent(expContent);
                        alert.showAndWait();
                    }
                 catch (Exception e){
                     Alert alert = new Alert(Alert.AlertType.ERROR);
                     alert.setTitle("Error");
                     alert.setHeaderText("Fehler in der Ausführung");
                     alert.setContentText("Da ist irgendetwas schiefgegeangen. Für details klappe die Fehlermeldung aus!");
                     // StackTrace ausgeben
                     // Create expandable Exception.
                     StringWriter sw = new StringWriter();
                     PrintWriter pw = new PrintWriter(sw);
                     e.printStackTrace(pw);
                     String exceptionText = sw.toString();

                     Label label = new Label("Der exception Stack trace ist:");

                     TextArea textArea = new TextArea(exceptionText);
                     textArea.setEditable(false);
                     textArea.setWrapText(true);

                     textArea.setMaxWidth(Double.MAX_VALUE);
                     textArea.setMaxHeight(Double.MAX_VALUE);
                     GridPane.setVgrow(textArea, Priority.ALWAYS);
                     GridPane.setHgrow(textArea, Priority.ALWAYS);

                     GridPane expContent = new GridPane();
                     expContent.setMaxWidth(Double.MAX_VALUE);
                     expContent.add(label, 0, 0);
                     expContent.add(textArea, 0, 1);
                     // Set expandable Exception into the dialog pane.
                     alert.getDialogPane().setExpandableContent(expContent);

                     alert.showAndWait();
                }
            }
            _shortestPathAlgorithm.clear();
        }
            progressBar.setProgress(1);
            if(errorMessage.length()!=0) {
            // Show the error message.
            Alert alert = new Alert(Alert.AlertType.WARNING);
            alert.setTitle("Achtung: Algorithmus fehlt!");
            alert.setHeaderText("Warnung: Es konnten nicht alle Algorithmen geladen und ausgeführt werden.");
            alert.setContentText(errorMessage);
            alert.showAndWait();
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
     * Shows a progress bar
     */
    @FXML
    private void handleProgressbar(){

    }

        /**
         * Validates the user input in the text fields.
         *
         * @return true if the input is valid
         */
        private boolean isInputValid() {
            String errorMessage = "";

            if (startField.getText() == null || startField.getText().length() == 0) {
                errorMessage += "Start Punkt benötigt!\n";
            }
            if (endField.getText() == null || endField.getText().length() == 0) {
                errorMessage += "End Punkt benötigt!\n";
            }
            if (errorMessage.length() == 0) {
                return true;
            } else {
                // Show the error message.
                Alert alert = new Alert(Alert.AlertType.ERROR);
//                alert.initOwner(dialogStage);
                alert.setTitle("Ungültige Werte");
                alert.setHeaderText("Bitte die fehlenden Werte eingeben");
                alert.setContentText(errorMessage);

                alert.showAndWait();

                return false;
            }
        }
}
