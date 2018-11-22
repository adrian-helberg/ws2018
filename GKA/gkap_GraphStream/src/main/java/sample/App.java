package sample;

import org.graphstream.graph.implementations.MultiGraph;
import sample.algorithmen.Parser;

import java.io.File;
import java.io.IOException;
import java.util.Objects;

public class App {

    public static void main(String[] args) {

        System.setProperty("gs.ui.renderer", "org.graphstream.ui.j2dviewer.J2DGraphRenderer");

        MultiGraph graph = new MultiGraph("graph03");
        MultiGraph result;
        Parser parser = new Parser(graph);

        try {

            result = parser.parseFromFile(App.class.getResource("../graph03.gka").getFile());
            result.display();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
