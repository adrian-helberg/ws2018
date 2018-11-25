package app;

import app.controller.AlgorithmsLayoutController;
import app.controller.LayoutController;
import javafx.application.Application;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;

import java.io.IOException;
import java.net.URL;

/**
 * GKAP project entry point
 *
 * @author Adrian Helberg, Maximilian Janzen
 */
public class App extends Application {

    private Stage stage;
    private BorderPane layout;
    // TODO: Change Object type to some algorithm type
    private ObservableList<Object> algorithms = FXCollections.observableArrayList();
    private final String layoutFile = "Layout.fxml";
    private final String algorithmsLayoutFile = "AlgorithmsLayout.fxml";

    // Application entry point
    public static void main(String[] args) {
        launch(args);
    }

    public App() {
        // TODO: Add algorithms
        algorithms.add("Breadth-first search");
    }

    @Override
    public void start(Stage primaryStage) throws Exception {
        // Add javaFX render properties
        System.setProperty("org.graphstream.ui.renderer", "org.graphstream.ui.j2dviewer.J2DGraphRenderer");

        stage = primaryStage;
        stage.setTitle("GKAP by Adrian Helberg & Maximilian Janzen");

        initLayout();
        initAlgorithmsLayout();
    }

    private void initLayout() throws java.io.IOException {
        // Get layout from file
        FXMLLoader loader = new FXMLLoader();
        URL layoutURL = getClass().getResource("/view/" + layoutFile);
        if (layoutURL == null) {
            System.out.println("Could not load fxml file: " + layoutFile);
        }

        loader.setLocation(layoutURL);
        layout = loader.load();

        // Prepare stage to be shown
        Scene scene = new Scene(layout);
        stage.setScene(scene);

        // Manage controller
        LayoutController controller = loader.getController();
        controller.init(this);

        // Render
        stage.show();
    }

    private void initAlgorithmsLayout() throws IOException {
        // Get layout from file
        FXMLLoader loader = new FXMLLoader();
        URL layoutURL = getClass().getResource("/view/" + algorithmsLayoutFile);
        if (layoutURL == null) {
            System.out.println("Could not load fxml file: " + algorithmsLayoutFile);
        }

        loader.setLocation(layoutURL);
        AnchorPane content = loader.load();

        // Manage controller
        AlgorithmsLayoutController controller = loader.getController();
        controller.init(this);

        // Place layout
        layout.setCenter(content);
    }

    // TODO: Add getter and setter for observable list algorithms?
}
