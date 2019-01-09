package application.algorithm;

import org.graphstream.graph.Node;
import org.graphstream.graph.implementations.MultiGraph;
import org.graphstream.stream.file.FileSinkImages;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Queue;

/**
 * File: NewEdmondKarp
 * Created by Maximilian Janzen
 *
 * @author Maximilian Janzen
 * Created in the Package application.algorithm of the Project: gkap
 * Created: 06.01.2019
 * Time: 19:51
 */

public class NewEdmondKarp extends MaxFlowAlgorithm{
    public static void main(String [] args){
        // Prepare test graph
        Graph graphparsed = new Graph("graph");
        Parser parser = new Parser(graphparsed.getMultiGraph());
        MultiGraph multiGraph = null;
        try {
            multiGraph = parser.parseFromFile("graph12.gka");

            /*FileSinkImages pic = new FileSinkImages(FileSinkImages.OutputType.PNG, FileSinkImages.Resolutions.VGA);
            pic.setLayoutPolicy(FileSinkImages.LayoutPolicy.COMPUTED_FULLY_AT_NEW_IMAGE);
            pic.writeAll(multiGraph, "graph13.png");*/

            new NewEdmondKarp(multiGraph,"q","s");
            System.out.println("Ergebnis der Berechnung");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    public NewEdmondKarp(MultiGraph graph, String source, String target) {
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
    public NewEdmondKarp(MultiGraph graph) {
        super(graph);
    }

    @Override
    Node getAnyMarkedNode() {
        //Wähle alle markierten aus
        Iterator<Node> iterator=graph.getNode(source).getBreadthFirstIterator();
        int i=0;
        while (iterator.hasNext()) {
            Node o = iterator.next();
            System.out.println("Knoten nach BFS ("+i+"): "+o);
            if ((o.getAttribute("vorg") != null && o.getAttribute("visited") == null)) {
                System.out.println("Knoten: gewählt" +o);
                return o;
            }
            i++;
        }

        return null;
    }

}
