package sample.controller;

import javafx.fxml.FXML;
import javafx.stage.Stage;
import sample.model.Algorithm;

public class GraphController {

    private Stage dialogStage;
    private Algorithm algorithmus;
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
     * @param dialogStage Stage to be set
     */
    public void setDialogStage(Stage dialogStage) {
        this.dialogStage = dialogStage;
    }

    /**
     * Sets the Algorithmus to be shown in the dialog.
     *
     * @param algorithmus
     */
    public void setGraph(Algorithm algorithmus) {
        this.algorithmus = algorithmus;
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
