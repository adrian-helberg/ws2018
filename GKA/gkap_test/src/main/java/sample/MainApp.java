package sample;

import java.awt.*;
import java.awt.Label;
import java.awt.TextArea;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;

import javafx.scene.control.*;
import javafx.scene.layout.*;
import org.graphstream.graph.*;
import org.graphstream.graph.implementations.*;
import org.graphstream.ui.layout.Layout;
import org.graphstream.ui.layout.Layouts;
import org.graphstream.ui.swingViewer.*;
import org.graphstream.ui.view.*;
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
import sample.model.Algorithmus;
import javax.swing.*;

/**
 * @author Maximilian Janzen & Adrian Helberg
 */
public class MainApp extends Application {

    private Stage primaryStage;
    private BorderPane rootLayout;
    private ObservableList<Algorithmus> algorithm = FXCollections.observableArrayList();

    public MainApp() {
        algorithm.add(new Algorithmus("Breadth first search"));
        algorithm.add(new Algorithmus("Dijkstra"));
        algorithm.add(new Algorithmus("Floyd Warschall"));
    }

    ObservableList<Algorithmus> getAlgorithmData() {
        return algorithm;
    }

    @Override
    public void start(Stage primaryStage) {
        System.setProperty("org.graphstream.ui.renderer", "org.graphstream.ui.j2dviewer.J2DGraphRenderer");
        this.primaryStage = primaryStage;
        this.primaryStage.setTitle("GKA Praktikum finde den Optimalen Weg");

        try {
            initRootLayout();
            showAlgorithmusOverview();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void initRootLayout() throws IOException {
        FXMLLoader loader = new FXMLLoader();
        loader.setLocation(MainApp.class.getResource("/view/RootLayout.fxml"));
        rootLayout = (BorderPane) loader.load();

        Scene scene = new Scene(rootLayout);
        primaryStage.setScene(scene);

        RootLayoutController controller = loader.getController();
        controller.setMainApp(this);
        primaryStage.show();
    }

    /**
     * Shows the Algorithmus overview inside the root layout.
     */
    private void showAlgorithmusOverview() throws IOException {
        FXMLLoader loader = new FXMLLoader();
        loader.setLocation(MainApp.class.getResource("/view/AlgorithmusOverview.fxml"));
        AnchorPane algorithmusOverview = (AnchorPane) loader.load();

        rootLayout.setCenter(algorithmusOverview);

        AlgorithmusOverviewController controller = loader.getController();
        controller.setMainApp(this);
    }

    void showGraphEditDialog(Algorithmus algorithmus) {
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

                Viewer viewer = new Viewer(algorithmus.getGraph().makeGraph(algorithmus.getPath()), Viewer.ThreadingModel.GRAPH_IN_GUI_THREAD);
                Layout layout = Layouts.newLayoutAlgorithm();
                viewer.enableAutoLayout(layout);
                ViewPanel viewPanel = viewer.addDefaultView(false);
                panel.add(viewPanel);

            }
        });

        loader.setLocation(MainApp.class.getResource("/view/Graph.fxml"));
        StackPane pane = new StackPane();
        pane.getChildren().add(swingNode);

        Stage dialogStage = new Stage();
        dialogStage.setTitle("Graph anzeigen für: " + algorithmus.getAlgorithmus());
        dialogStage.initModality(Modality.WINDOW_MODAL);
        dialogStage.initOwner(primaryStage);
        dialogStage.setScene(new Scene(pane, 250, 150));
        dialogStage.showAndWait();
    }

    Stage getPrimaryStage() {
        return primaryStage;
    }

    public static void main(String[] args) {
        launch(args);
    }
}