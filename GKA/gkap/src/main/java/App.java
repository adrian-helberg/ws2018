import org.jgrapht.graph.AbstractBaseGraph;

/**
 * Application
 *
 * @author Adrian Helberg
 */
public class App {

    public static void main(String args[]) {
        utils.readGKAFile("graph01.gka");
        AbstractBaseGraph graph = utils.importGraph("graph01.dot");
        utils.BFS(graph, "a", "g");
    }
}