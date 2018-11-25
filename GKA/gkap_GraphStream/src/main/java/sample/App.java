package sample;

import java.awt.*;
import java.io.IOException;
import java.net.URL;
import java.util.Arrays;

import javafx.scene.control.*;
import javafx.scene.control.TextArea;
import javafx.scene.layout.*;
import org.graphstream.ui.layout.Layout;
import org.graphstream.ui.layout.Layouts;
import javafx.application.Application;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.embed.swing.SwingNode;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.graphstream.ui.swingViewer.ViewPanel;
import org.graphstream.ui.view.Viewer;
import sample.controller.AlgorithmListController;
import sample.controller.RootLayoutController;
import sample.model.Algorithm;
import javax.swing.*;

/**
 * GKAP Project entry point
 *
 * @author Adrian Helberg
 * @author Maximilian Janzen
 */
public class App extends Application {

    // Stage to be embed
    private Stage stage;
    private BorderPane layout;
    private ObservableList<Algorithm> algorithmList = FXCollections.observableArrayList();

    public App() {
        // Initial
        algorithmList.add(new Algorithm("Breadth first search"));
        algorithmList.add(new Algorithm("Dijkstra"));
        algorithmList.add(new Algorithm("Floyd Warschall"));
    }

    /**
     * Return added algorithms
     * @return List of algorithms
     */
    public ObservableList<Algorithm> getAlgorithmList() {
        return algorithmList;
    }

    /**
     * Start the javaFX application. Called by javaFX after initialization
     * @param primaryStage State to be embed in the application
     */
    @Override
    public void start(Stage primaryStage) {
        // Register GraphStream renderer as J2DGraphRenderer
        System.setProperty("org.graphstream.ui.renderer", "org.graphstream.ui.j2dviewer.J2DGraphRenderer");

        this.stage = primaryStage;
        this.stage.setTitle("GKAP project by Adrian Helberg & Maximilian Janzen");

        try {
            initRootLayout();
            initAlgorithmList();
        } catch (IOException e) {

            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setTitle("Error");
            alert.setHeaderText("An error occurred");
            alert.setContentText(e.getClass().toString());

            StringBuilder sb = new StringBuilder();
            sb.append(Arrays.toString(e.getStackTrace()));

            TextArea textArea = new TextArea(sb.toString());
            textArea.setEditable(false);
            textArea.setWrapText(true);
            // Additionally print stack trace
            e.printStackTrace();

            alert.getDialogPane().setExpandableContent(textArea);

            alert.showAndWait();
        }
    }

    /**
     * Initialize root layout.
     */
    private void initRootLayout() throws IOException {
        FXMLLoader loader = new FXMLLoader();
        URL layoutURL = App.class.getResource("/view/RootLayout.fxml");
        if (layoutURL == null) throw new IOException("Could not load layout file");

        loader.setLocation(layoutURL);
        layout = loader.load();

        // Scene to be rendered
        Scene scene = new Scene(layout);
        stage.setScene(scene);

        // FXML view model
        RootLayoutController controller = loader.getController();
        controller.setInstance(this);

        // Render
        stage.show();
    }

    /**
     * Initialize algorithm list layout.
     */
    private void initAlgorithmList() throws IOException {
        FXMLLoader loader = new FXMLLoader();
        URL layoutURL = App.class.getResource("/view/AlgorithmListLayout.fxml");
        if (layoutURL == null) throw new IOException("Could not load layout file");

        loader.setLocation(layoutURL);
        AnchorPane algorithmList = loader.load();
        layout.setCenter(algorithmList);

        // FXML view model
        AlgorithmListController controller = loader.getController();
        controller.setInstance(this);
    }

    // TODO: JavaDoc and naming
    public void showGraphEditDialog(Algorithm algorithmus) {
        // Load the fxml file and create a new stage for the popup dialog.
        try {
            FXMLLoader loader = new FXMLLoader();
            final SwingNode swingNode = new SwingNode();

            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    JPanel panel;
                    swingNode.setContent(panel = new JPanel(new GridLayout()) {
                        @Override
                        public Dimension getPreferredSize() {
                            return new Dimension(840, 680);
                        }
                    });

                    //panel.setBorder(BorderFactory.createLineBorder(Color.blue, 1));
                    Viewer viewer = new Viewer(algorithmus.getGraph().makeGraph(algorithmus.getPath()), Viewer.ThreadingModel.GRAPH_IN_GUI_THREAD);
                    Layout layout = Layouts.newLayoutAlgorithm();
                    viewer.enableAutoLayout(layout);
                    ViewPanel viewPanel = viewer.addDefaultView(false);
                    panel.add(viewPanel);

                }
            });

            loader.setLocation(App.class.getResource("/view/Graph.fxml"));
            // AnchorPane page = (AnchorPane) loader.load();
            StackPane pane = new StackPane();
            pane.getChildren().add(swingNode);

            // Create the dialog Stage.
            Stage dialogStage = new Stage();
            dialogStage.setTitle("Graph anzeigen für: " + algorithmus.getAlgorithmus());
            dialogStage.initModality(Modality.WINDOW_MODAL);
            dialogStage.initOwner(stage);
            // Scene scene = new Scene(page);
            dialogStage.setScene(new Scene(pane, 250, 150));
            dialogStage.showAndWait();
        }catch (Exception e){

        }
    }

    /**
     * Return main stage.
     * @return Main stage
     */
    public Stage getStage() {
        return stage;
    }

    /**
     * Application entry point
     * @param args Arguments
     */
    public static void main(String[] args) {
        launch(args);
    }
}