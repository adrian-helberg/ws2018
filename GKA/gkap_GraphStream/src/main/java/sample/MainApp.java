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

public class MainApp extends Application {

    private Stage primaryStage;
    private BorderPane rootLayout;
    private ObservableList<Algorithmus> algorithm = FXCollections.observableArrayList();


    /**
     * Constructor
     */
    public MainApp() {
        algorithm.add(new Algorithmus("Breadth first search"));
        algorithm.add(new Algorithmus("Dijkstra"));
        algorithm.add(new Algorithmus("Edmonds Karp"));
        algorithm.add(new Algorithmus("Floyd Warschall"));
        algorithm.add(new Algorithmus("Ford Fulkerson"));
        algorithm.add(new Algorithmus("Max Flow Algorithm"));
        algorithm.add(new Algorithmus("Shortest Path Algorithm"));
    }

    /**
     * Returns the data as an observable list of Algorithms.
     * @return
     */
    public ObservableList<Algorithmus> getAlgorithmData() {
        return algorithm;
    }
    /**
     * Set the Algo data as an observable list of Algorithms.
     * @return
     */
    public void setAlgorithmData(ObservableList<Algorithmus> algorithmusneu) {
        algorithm.clear();
        algorithm.addAll(algorithmusneu);
    }


    @Override
    public void start(Stage primaryStage) {
        System.setProperty("org.graphstream.ui.renderer", "org.graphstream.ui.j2dviewer.J2DGraphRenderer");
        this.primaryStage = primaryStage;
        this.primaryStage.setTitle("GKA Praktikum finde den Optimalen Weg");

        initRootLayout();

        showAlgorithmusOverview();
    }

    /**
     * Initializes the root layout.
     */
    public void initRootLayout() {
        try {
            // Load root layout from fxml file.
            FXMLLoader loader = new FXMLLoader();
            loader.setLocation(MainApp.class.getResource("view/RootLayout.fxml"));
            rootLayout = (BorderPane) loader.load();

            // Show the scene containing the root layout.
            Scene scene = new Scene(rootLayout);
            primaryStage.setScene(scene);
            // Give the controller access to the main app.
            RootLayoutController controller = loader.getController();
            controller.setMainApp(this);
            primaryStage.show();

        } catch (IOException e) {
            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setTitle("Error");
            alert.setHeaderText("Layout Load Excpetion");
            alert.setContentText("Pech gehabt. Da kannst du nichts dran machen. Wende dich an den Autor");
            // StackTrace ausgeben
            // Create expandable Exception.
            StringWriter sw = new StringWriter();
            PrintWriter pw = new PrintWriter(sw);
            e.printStackTrace(pw);
            String exceptionText = sw.toString();

            javafx.scene.control.Label label = new javafx.scene.control.Label("Der exception Stack trace ist:");

            javafx.scene.control.TextArea textArea = new javafx.scene.control.TextArea(exceptionText);
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

    /**
     * Shows the Algorithmus overview inside the root layout.
     */
    public void showAlgorithmusOverview() {
        try {
            // Load Algorithmus overview.
            FXMLLoader loader = new FXMLLoader();
            loader.setLocation(MainApp.class.getResource("view/AlgorithmusOverview.fxml"));
            AnchorPane algorithmusOverview = (AnchorPane) loader.load();

            // Set Algorithmus overview into the center of root layout.
            rootLayout.setCenter(algorithmusOverview);

            // Give the controller access to the main app.
            AlgorithmusOverviewController controller = loader.getController();
            controller.setMainApp(this);
        } catch (IOException e) {
            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setTitle("Error");
            alert.setHeaderText("Graph kann nicht angezeigt werden");
            alert.setContentText("Zu diesem Algorithmus, lässt sich kein Graph Visuell erzeugen");
            // StackTrace ausgeben
            // Create expandable Exception.
            StringWriter sw = new StringWriter();
            PrintWriter pw = new PrintWriter(sw);
            e.printStackTrace(pw);
            String exceptionText = sw.toString();

            javafx.scene.control.Label label = new javafx.scene.control.Label("Der exception Stack trace ist:");

            javafx.scene.control.TextArea textArea = new javafx.scene.control.TextArea(exceptionText);
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

            alert.showAndWait();        }
    }


    /**
     * Opens a dialog to show a graph for specified Algorithm. If the user
     * clicks OK, the dialog close and true
     * is returned.
     *
     * @param algorithmus the Algorithmus object to be shown
   //  * @return true if the user clicked OK, false otherwise.
     */
    public void showGraphEditDialog(Algorithmus algorithmus) {
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

            loader.setLocation(MainApp.class.getResource("view/Graph.fxml"));
            // AnchorPane page = (AnchorPane) loader.load();
            StackPane pane = new StackPane();
            pane.getChildren().add(swingNode);

            // Create the dialog Stage.
            Stage dialogStage = new Stage();
            dialogStage.setTitle("Graph anzeigen für: " + algorithmus.getAlgorithmus());
            dialogStage.initModality(Modality.WINDOW_MODAL);
            dialogStage.initOwner(primaryStage);
            // Scene scene = new Scene(page);
            dialogStage.setScene(new Scene(pane, 250, 150));
            dialogStage.showAndWait();
        }catch (Exception e){

        }
    }

    /**
     * Returns the main stage.
     * @return
     */
    public Stage getPrimaryStage() {
        return primaryStage;
    }

    public static void main(String[] args) {
        launch(args);
    }
}