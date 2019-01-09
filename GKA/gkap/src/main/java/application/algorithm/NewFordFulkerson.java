package application.algorithm;

import org.graphstream.graph.Node;
import org.graphstream.graph.Edge;
import org.graphstream.graph.implementations.MultiGraph;
import org.graphstream.stream.file.*;

import java.io.IOException;
import java.util.*;

/**
 * File: NewFordFulkerson
 * Created by Maximilian Janzen & Adrian Helberg
 *
 * @author Maximilian Janzen & Adrian Helberg
 * Created in the Package application.algorithm of the Project: gkap
 * Created: 24.12.2018
 * Time: 13:35
 */

public class NewFordFulkerson extends MaxFlowAlgorithm {

    boolean finish=false;
    //Main just for debugging
    public static void main(String [] args){
        // Prepare test graph
        Graph graphparsed = new Graph("graph");
        Parser parser = new Parser(graphparsed.getMultiGraph());
        MultiGraph multiGraph = null;
        try {
            multiGraph = parser.parseFromFile("graph12.gka");

            FileSinkImages pic = new FileSinkImages(FileSinkImages.OutputType.PNG, FileSinkImages.Resolutions.VGA);
            pic.setLayoutPolicy(FileSinkImages.LayoutPolicy.COMPUTED_FULLY_AT_NEW_IMAGE);
            pic.writeAll(multiGraph, "graph12.png");

        NewFordFulkerson fordFulkerson= new NewFordFulkerson(multiGraph,"q","s");
        System.out.println("Ergebnis der Berechnung");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    public NewFordFulkerson(MultiGraph graph, String source, String target) {
        super(graph);
        this.graph = graph;
        this.source = source;
        this.target = target;
        try {
            init();
        } catch (Exception e) { //TODO Invalid Graph Exceptions entfernen
            e.printStackTrace();
        }
    }
    public NewFordFulkerson(MultiGraph graph) {
        super(graph);
    }

    @Override
    Node getAnyMarkedNode() {
        //Wähle alle markierten aus
        ArrayList<Node> nodes = new ArrayList<>();
        for (Node o : graph.getNodeSet()) {
            if (o.getAttribute("vorg") != null && o.getAttribute("visited") == null) {
                nodes.add(o);
            }
        }
        return nodes.get(0);
    }


}
