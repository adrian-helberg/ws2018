import org.jgrapht.graph.AbstractBaseGraph;

public class App {

    public static void main(String args[]) {
        AbstractBaseGraph graph = utils.importGraph("graph01");
        utils.exportGraph(graph, "test");
    }
}
