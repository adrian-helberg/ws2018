package application;

import java.awt.*;
import java.io.IOException;
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
import application.model.Algorithm;
import javax.swing.*;

/**
 * GKAP Project entry point
 *
 * @author Adrian Helberg
 * @author Maximilian Janzen
 */
public class App extends Application {
    private Stage primaryStage;
    private BorderPane rootLayout;
    private ObservableList<Algorithm> algorithmList = FXCollections.observableArrayList();
    private boolean fileLoaded = false;

    public App() {
        algorithmList.add(new Algorithm("Breadth First Search"));
        algorithmList.add(new Algorithm("Dijkstra"));
        algorithmList.add(new Algorithm("Floyd Warschall"));
        algorithmList.add(new Algorithm("Ford Fulkerson"));
        algorithmList.add(new Algorithm("Edmonds Karp"));
    }

    @Override
    public void start(Stage primaryStage) {
        System.setProperty("org.graphstream.ui.renderer", "org.graphstream.ui.j2dviewer.J2DGraphRenderer");

        this.primaryStage = primaryStage;
        this.primaryStage.setTitle("GKAP by Adrian Helberg & Maximilian Janzen");

        try {
            initRootLayout();
            initOverviewLayout();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Initialize root layout
     *
     * @throws IOException If layout could not be loaded
     */
    private void initRootLayout() throws IOException {
        FXMLLoader loader = new FXMLLoader();
        loader.setLocation(App.class.getResource("/view/RootLayout.fxml"));
        rootLayout = loader.load();
        Scene scene = new Scene(rootLayout);
        primaryStage.setScene(scene);
        RootLayoutController controller = loader.getController();
        controller.setInstance(this);
        primaryStage.show();
    }

    /**
     * Show overview inside root layout.
     *
     * @throws IOException If layout could not be loaded
     */
    private void initOverviewLayout() throws IOException {
        FXMLLoader loader = new FXMLLoader();
        loader.setLocation(App.class.getResource("/view/OverviewLayout.fxml"));
        AnchorPane overviewLayout = loader.load();
        rootLayout.setCenter(overviewLayout);
        OverviewLayoutController controller = loader.getController();
        controller.setInstance(this);
    }

    /**
     * Show graph in modal
     *
     * @param algorithm Algorithm to be shown
     */
    void showGraphEditDialog(Algorithm algorithm) {
        final SwingNode swingNode = new SwingNode();

        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                JPanel panel;
                swingNode.setContent(panel = new JPanel(new GridLayout()) {
                    @Override
                    public Dimension getPreferredSize() {
                        return new Dimension(800, 600);
                    }
                });

                Viewer viewer = new Viewer(algorithm.getGraph().makeGraph(algorithm.getPath()), Viewer.ThreadingModel.GRAPH_IN_GUI_THREAD);
                Layout layout = Layouts.newLayoutAlgorithm();
                viewer.enableAutoLayout(layout);
                ViewPanel viewPanel = viewer.addDefaultView(false);
                panel.add(viewPanel);
            }
        });

        StackPane pane = new StackPane();
        pane.getChildren().add(swingNode);
        Stage dialogStage = new Stage();
        dialogStage.setTitle(algorithm.getAlgorithm());
        dialogStage.initModality(Modality.WINDOW_MODAL);
        dialogStage.initOwner(primaryStage);
        dialogStage.setScene(new Scene(pane, 250, 150));
        dialogStage.showAndWait();
    }

    Stage getPrimaryStage() {
        return primaryStage;
    }

    ObservableList<Algorithm> getAlgorithmData() {
        return algorithmList;
    }

    public boolean isFileLoaded() {
        return fileLoaded;
    }

    public void setFileLoaded(boolean fileLoaded) {
        this.fileLoaded = fileLoaded;
    }

    /**
     * Main entry point
     * @param args Arguments
     */
    public static void main(String[] args) {
        launch(args);
    }
}