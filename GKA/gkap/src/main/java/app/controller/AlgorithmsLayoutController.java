package app.controller;

import app.App;
import app.algorithm.Algorithm;
import javafx.fxml.FXML;
import javafx.scene.control.TableColumn;

import javax.swing.text.TableView;

/**
 * GKAP layout controller
 *
 * @author Adrian Helberg, Maximilian Janzen
 */
public class AlgorithmsLayoutController {

    @FXML
    private TableView TableView;
    @FXML
    private TableColumn TableColumn;

    private App app;

    public void init(App instance) {
        app = instance;
        TableView.;
    }

    @FXML
    private void initialize() {

    }
}
