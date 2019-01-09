package application.algorithm;

import application.algorithm.Algorithm;
import application.exception.InvalidGraphException;
import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.implementations.MultiGraph;
import org.graphstream.stream.file.FileSinkImages;

import java.util.*;

/**
 * Basic Max FLow Algorithm
 *
 * @author Adrian Helberg
 * @author Maximilian Janzen
 */
public abstract class MaxFlowAlgorithm extends Algorithm {
    // Capacities of edges
    int[] capacities;
    // Flows of edges
    int[] flows;
    // Maximum flow
    int maximumFlow;
    //Edge count
    int n;
    // Capacity attribute
    String capacityAttribute;
    // Source and Sink
    String source, target;

    MaxFlowAlgorithm(MultiGraph graph) {
        if (graph == null) {
            System.out.println("No graph passed");
            System.exit(-1);
        }

        this.graph = graph;
}

   MaxFlowAlgorithm(MultiGraph graph, String source, String target) {
      if (graph == null) {
          System.out.println("No graph passed");
          System.exit(-1);
      }
        this.graph = graph;
        this.source = source;
        this.target = target;
    }
    /**
     * Get maximum flow
     *
     * @return Maximum flow
     */
    public int getMaximumFlow() {
        return maximumFlow;
    }
    /**
     * Set Maximum flow
     * @param flow   Flow value
     */
    void setMaximumFlow( int flow) {
        maximumFlow=flow;
    }
    /**
     * Get current flow by source and sink string identifier
     *
     * @param source Source identifier
     * @param target Sink identifier
     * @return Current flow value
     */
    int getFlow(String source, String target) {
        Node s = graph.getNode(source);
        Node t = graph.getNode(target);

        return getFlow(s, t);
    }

    /**
     * Get current flow by source and sink node
     *
     * @param source Source node
     * @param target Sink node
     * @return Current flow value
     */
    int getFlow(Node source, Node target) {
        Edge e = source.getEdgeBetween(target);

        if (e.getSourceNode() == source) {
            return flows[e.getIndex()];
        } else {
            return flows[e.getIndex() + n];
        }
    }

    /**
     * Set flow by source and sink string identifier
     *
     * @param source Source identifier
     * @param target Sink identifier
     * @param flow   Flow value
     */
    void setFlow(String source, String target, int flow) {
        Node s = graph.getNode(source);
        Node t = graph.getNode(target);

        setFlow(s, t, flow);
    }

    /**
     * Set flow by source and sink node
     *
     * @param source Source node
     * @param target Sink node
     * @param flow   Flow value
     */
    void setFlow(Node source, Node target, int flow) {
        Edge e = source.getEdgeBetween(target);

        if (e.getSourceNode() == source)
            flows[e.getIndex()] = flow;
        else
            flows[e.getIndex() + n] = flow;
    }

    /**
     * Get capacity by source and sink string identifier
     *
     * @param source Source identifier
     * @param target Sink identifier
     * @return Capacity value
     * @throws InvalidGraphException If no edge between source and sink
     */
    int getCapacity(String source, String target) throws InvalidGraphException {
        Node s = graph.getNode(source);
        Node t = graph.getNode(target);

        return getCapacity(s, t);
    }

    /**
     * Get capacity by source and sink node
     *
     * @param source Source node
     * @param target Sink node
     * @return Capacity value
     * @throws InvalidGraphException If no edge between source and sink
     */
    int getCapacity(Node source, Node target) throws InvalidGraphException {
        Edge e = source.getEdgeBetween(target);

        if (e == null) throw new InvalidGraphException("No edge between " + source + " and " + target);

        if (e.getSourceNode() == source)
            return capacities[e.getIndex()];
        else
            return capacities[e.getIndex() + n];
    }

    /**
     * Set capacity by source and sink string identifier
     *
     * @param source   Source identifier
     * @param target   Sink identifier
     * @param capacity Capacity value
     */
    void setCapacity(String source, String target, int capacity) {
        Node s = graph.getNode(source);
        Node t = graph.getNode(target);

        setCapacity(s, t, capacity);
    }

    /**
     * Set capacity by souce and sink node
     *
     * @param source   Source node
     * @param target   Sink node
     * @param capacity Capacity value
     */
    void setCapacity(Node source, Node target, int capacity) {
        Edge e = source.getEdgeBetween(target);

        if (e.getSourceNode() == source)
            capacities[e.getIndex()] = capacity;
        else
            capacities[e.getIndex() + n] = capacity;
    }

    /**
     * Set all capacities
     *
     * @param value Capacity value
     */
    void setAllCapacities(int value) {
        for (int i = 0; i < 2 * n; i++)
            capacities[i] = value;
    }

    /**
     * Check if capacities array is too large to compute
     *
     * @throws InvalidGraphException If capacity or flow array too large
     */
    void checkArrays() throws InvalidGraphException {
        n = graph.getEdgeCount();

        if (capacities == null || capacities.length < 2 * n) {
            capacities = new int[2 * n];
            flows = new int[2 * n];
        } else {
            throw new InvalidGraphException("Capacity array too large to compute");
        }
    }

    /**
     * Load capacities from attribute
     *
     * @throws InvalidGraphException If unknown attribute
     */
    void loadCapacitiesFromAttribute() throws InvalidGraphException {
        // No capacity attribute
        if (capacityAttribute == null) return;

        Edge e;
        // Iterate capacities
        for (int i = 0; i < n; i++) {
            // Reset capacity
            capacities[i] = 0;
            capacities[i + n] = 0;

            e = graph.getEdge(i);

            if (e.hasNumber(capacityAttribute)) {
                capacities[i] = (int) e.getNumber(capacityAttribute);
            } else if (e.hasVector(capacityAttribute)) {
                List<? extends Number> capVect = graph.getEdge(i)
                        .getVector(capacityAttribute);

                if (capVect.size() > 0)
                    capacities[i] = capVect.get(0).intValue();
                if (capVect.size() > 1)
                    capacities[i + n] = capVect.get(1).intValue();

            } else if (e.hasArray(capacityAttribute)) {
                Object[] capArray = e.getArray(capacityAttribute);

                if (capArray.length > 0)
                    capacities[i] = ((Number) capArray[0]).intValue();
                if (capArray.length > 1)
                    capacities[i + n] = ((Number) capArray[1]).intValue();
            } else if (e.hasAttribute(capacityAttribute)) {
                throw new InvalidGraphException("Unknown capacity attribute");
            }
        }
    }

    /**
     * Compute max flow; Should be overwritten
     *
     * @throws InvalidGraphException
     */
    void compute() throws InvalidGraphException {
        throw new InvalidGraphException("Method compute should be overwritten");
    }


    /**
     * Compute max flow; Should be overwritten
     *
     * @throws InvalidGraphException
     */
    void init() throws Exception{
        // Get source and target Node
        Node source = graph.getNode(this.source);
        Node sink = graph.getNode(this.target);
        System.out.println("Starte Initialisierung");

        // are they exists?
        if (source == null)
            throw new Exception("Source node not found");

        if (sink == null)
            throw new Exception("Sink node not found");

        source.setAttribute("delta", Integer.MAX_VALUE);
        source.setAttribute("vorg", source);
        source.setAttribute("sign","");


        // all edges 0 flow at start
        for (Edge e : graph.getEachEdge())
            e.setAttribute("flow", (int) 0);

        // get Augmentierenden path
        System.out.print("Nach der Initialisierung gilt folgende Tabelle, wobei inspizierte Ecken mit* markiert werden, nur markierte, aber noch nicht inspizierte Ecken stehen inder Tabelle ohne * und nicht markierte Ecken sind dort nicht aufgeführt: \n");
        inspektion(source); // TODO In den Konstruktor
    }

    void inspektion( Node vi) throws Exception {
        Node source = graph.getNode(this.source); //TODO evtl. als node speichern
        FileSinkImages pic = new FileSinkImages(FileSinkImages.OutputType.PNG, FileSinkImages.Resolutions.VGA);
        //FileSinkDGSFiltered pic = new FileSinkDGSFiltered();

        // graph.addSink(pic);

        // Wenn allInspected true dann 4 ->
//Es gibt keinen vergrößernden Weg. Der jetzige Wert von d ist optimal.
//Ein Schnitt A(X,X) mit c(X,X) = d wird gebildet von genau denjenigen
//Kanten, bei denen entweder die Anfangsecke oder die Endecke inspiziert
//ist.
 /*       for( Node n: graph.getNodeSet()) {
            System.out.println("\nNode: "+n);
            System.out.println("Delta: "+(n.getAttribute("delta")!=null?n.getAttribute("delta").toString():"KeinDelta"));
            System.out.println("Vorg: "+(n.getAttribute("vorg")!=null?n.getAttribute("vorg").toString():"Keinvorg"));
            System.out.println("visited:"+(n.getAttribute("visited")!=null?n.getAttribute("visited").toString():"Keinvisited"));
        }*/

        //setze vi auf inspiziert und starte inspizierung
        vi.setAttribute("visited", true);
        //  System.out.println("VI: "+vi);
        //markiere Vorwärtskante
        for (Edge eij : vi.getEachLeavingEdge()) {
            Node vj = eij.getTargetNode();
            //Noch nicht markiert. Also weiter...
            // System.out.println("Vorwärtskante");

            //TODO Alle Attribute in Graph hinzufügen
            int floweij = eij.getAttribute("flow");
            int capeij = eij.getAttribute(Graph.distance);
            //int capeij = Integer.parseInt(capeijs.replaceAll("[\\D]", ""));
            //System.out.println("floweij: "+floweij);
            // System.out.println("capeijs: "+capeij);
            // System.out.println("vj: "+vj);

            //Noch nicht markiert. Also weiter...
            if (vj.getAttribute("vorg") == null && floweij < capeij) {
                //Plus==1; Minus == 0
                vj.setAttribute("sign", "+");
                vj.setAttribute("vorg", vi);
                Integer deltaj = Math.min((capeij - floweij), vi.getAttribute("delta"));
                vj.setAttribute("delta", deltaj);
                //System.out.println("deltaj: "+deltaj);
            }
        }

        //markiere Rückwärtskante
        for (Edge eji : vi.getEachEnteringEdge()) {
            Node vj = eji.getTargetNode();
            //Noch nicht markiert. Also weiter...
            //System.out.println("Rückwärtskante");

            int floweji = eji.getAttribute("flow");
            //System.out.println("floweji: "+floweji);
            //System.out.println("vj: "+vj);
            if (vj.getAttribute("vorg") == null && floweji > 0) {
                //Plus==1; Minus == 0
                vj.setAttribute("sign", "-");
                vj.setAttribute("vorg", vi);
                Integer deltaj = Math.min(floweji, vi.getAttribute("delta"));
                vj.setAttribute("delta", deltaj);
                //   System.out.println("deltaj: "+deltaj);

            }
        }
        hautdentextraus(vi);

        Node sinkNode = graph.getNode(target);
//Falls s Markiert 3 sonst 2
        if (sinkNode.getAttribute("vorg") != null) {
            changeFlow(sinkNode, sinkNode.getAttribute("delta"));
            //Markierungen Entfernen
            //System.out.println("Markierungen Entfernen");
            for (Node n : graph.getNodeSet()) {
                n.removeAttribute("vorg");
                n.removeAttribute("delta");
                // TODO Leer String statt 2 bzw. dann gleich + und - nehmen
                //TODO gleiches wie vorg auch für alle anderen
                n.removeAttribute("sign");
                n.removeAttribute("visited");
            }
            source.setAttribute("sign", "");
            source.setAttribute("vorg", source);
            source.setAttribute("delta", Integer.MAX_VALUE);
            System.out.println("\n==========================================================================================================================");
            System.out.println("Beginne neue Runde");
            //  try {
            // FileSinkImages pic = new FileSinkImages(FileSinkImages.OutputType.PNG, FileSinkImages.Resolutions.VGA);
            // pic.setLayoutPolicy(FileSinkImages.LayoutPolicy.COMPUTED_FULLY_AT_NEW_IMAGE);
            // pic.writeAll(graph, "graph03editedschritt3.png");
            // } catch (IOException e) {
            //     e.printStackTrace();
            // }
        }
        if (allMarkedAreInspected()) {
            int sumQ = 0;
            int sumQKomplement = 0;

            for (Edge e : source.getEachLeavingEdge()) {
                sumQ += (int) e.getAttribute("flow");
            }
            for (Edge e : source.getEachEnteringEdge()) {
                sumQKomplement += (int) e.getAttribute("flow");
            }
            maximumFlow = sumQ - sumQKomplement;
            System.out.println("Flow: " + maximumFlow);
            return;
           /* int flow = 0;
            for (int i = 0; i < source.getDegree(); i++) {
                flow += getFlow(source, source.getEdge(i).getOpposite(source));
            }
            setMaximumFlow(flow);
           // System.out.println("Flow: "+flow);
            //TODO Rückgabe prüfen und anpassen
            return getMaximumFlow();*/
        }

        //else {
        //try {
        // pic.setLayoutPolicy(FileSinkImages.LayoutPolicy.COMPUTED_ONCE_AT_NEW_IMAGE);
        //pic.writeAll(graph, "graph03edited.png");
        //  } catch (IOException e) {
        //      e.printStackTrace();
        //  }
        //Wähle beliebige markierte nicht inspizierte Ecke

        inspektion(getAnyMarkedNode()); //FEHLER GEFUNDEN TODO hier ist vorbei weil ich als Bedingung habe das der noch nicht inzpiziert sein darf. Ganz schön blöd! Nohcmal den Algorithmus durchgehen!
        //}    }
    }
        boolean allMarkedAreInspected() {
            HashSet<Node> markedNodes = new HashSet<>();
            for (Node o : graph.getNodeSet()) {
                if (o.getAttribute("vorg") != null)
                    markedNodes.add(o);
            }
            return markedNodes.stream().allMatch(x -> x.getAttribute("visited") != null);
        }

        Node getAnyMarkedNode() throws Exception{
        throw new Exception("Method should be overwritten");
        }

    void changeFlow(Node node, Integer sinkDelta) {
        if (!(node.toString().equals(source))) {
            Node vorgName = node.getAttribute("vorg");
            Edge edge = node.getEdgeBetween(vorgName);
            Integer fEij = edge.getAttribute("flow");
            String sign = node.getAttribute("sign");
            if (sign.equals("+")) {
                edge.setAttribute("flow", fEij + sinkDelta);
            } else if (sign.equals("-")) {
                edge.setAttribute("flow", fEij-sinkDelta);
            } else{
                System.err.println("SIGN nicht 0 oder 1!");//TODO delete
            }
            changeFlow(vorgName, sinkDelta);
        }
    }

    void hautdentextraus(Node vi){
        ArrayList<String> markednodes= getmarkedinspectedNodes();
        System.out.println("\nGewählt wurde (beliebig) " + vi);
        System.out.print("gekennzeichnete Ecke || \t"+markednodes.get(0)+"\n");
        System.out.println("=====================||============================================================================");
        System.out.print("       Kennzeichnung || \t "+markednodes.get(1)+"\n");
        System.out.println("=====================||============================================================================");

    }
    ArrayList<String> getmarkedinspectedNodes(){
        ArrayList<String> rueck = new ArrayList<>();
        StringBuilder allemarkednodes = new StringBuilder();
        StringBuilder kennzeichnung = new StringBuilder();
        Node vorg;
        for (Node o : graph.getEachNode()) {
            vorg =o.getAttribute("vorg");
            if (vorg != null){
                allemarkednodes.append(" \t \t").append(o);
                kennzeichnung.append(" \t ").append("("+o.getAttribute("sign")+" "+ vorg +", "+o.getAttribute("delta")+")");

            }
            if (o.getAttribute("visited")!=null){
                allemarkednodes.append("* ");
            }
        }
        rueck.add(0,allemarkednodes.toString());
        rueck.add(1,kennzeichnung.toString());

        return rueck;
    }
}
