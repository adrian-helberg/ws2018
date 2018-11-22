package sample;

import javafx.embed.swing.SwingNode;
import javafx.fxml.FXML;
import javafx.scene.Scene;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;
import sample.algorithmen.Algorithm;
import sample.model.Algorithmus;

import javax.swing.*;

/**
 * Created by Work on 11.05.2018.
 * TODO nicht mehr benötigt neue Lösung in der Main
 * Könnte aber aufgrund der schönheit hier hin ausgegliedert werden
 */
public class GraphController {

    private Stage dialogStage;
    private Algorithmus algorithmus;
    private boolean okClicked = false;

    /**
     * Initializes the controller class. This method is automatically called
     * after the fxml file has been loaded.
     */
    @FXML
    private void initialize() {
    }

    /**
     * Sets the stage of this dialog.
     *
     * @param dialogStage
     */
    public void setDialogStage(Stage dialogStage) {
        this.dialogStage = dialogStage;
    }
    /**
     * Sets the Algorithmus to be shown in the dialog.
     *
     * @param algorithmus
     */
    public void setGraph(Algorithmus algorithmus) {
        this.algorithmus = algorithmus;

//TODO
    }

    /**
     * Returns true if the user clicked OK, false otherwise.
     *
     * @return
     */
    public boolean isOkClicked() {
        return okClicked;
    }

    /**
     * Called when the user clicks ok.
     */
    @FXML
    private void handleOk() {
               okClicked = true;
              dialogStage.close();
    }

}
