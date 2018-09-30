import org.jgrapht.graph.AbstractGraph;
import org.jgrapht.graph.DefaultEdge;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Queue;

public class math {

    static boolean BFS(String start, String end, AbstractGraph graph) {
        if (start.equals(end)) return true;

        Queue<String> queue = new LinkedList<>();
        ArrayList<String> visited = new ArrayList<>();
        queue.add(start);
        visited.add(start);

        while (!queue.isEmpty()) {
            String current = queue.remove();

            if (current.equals(end)) {
                return true;
            } else {
                if (graph.getAllEdges(start, end).isEmpty()) {
                    return false;
                } else {
                    for (Object e : graph.getAllEdges(start, end)) {
                        queue.add(e.toString());
                    }
                }
            }
            visited.add(current);
        }

        return false;
    }
}