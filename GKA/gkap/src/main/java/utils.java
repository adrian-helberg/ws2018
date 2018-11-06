import org.jgrapht.GraphPath;
import org.jgrapht.alg.shortestpath.DijkstraShortestPath;
import org.jgrapht.graph.*;
import org.jgrapht.io.*;
import org.jgrapht.traverse.BreadthFirstIterator;
import java.io.*;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Utils for managing graph import, export and files
 *
 * TODO: Access edge labels through EdgeLabelProvider
 *
 * @author Adrian Helberg, Maximilian Janzen
 */
class utils {
    /**
     * Reads gka file with given name and converts it into a dot file
     *
     * @param fileName File to import
     */
    static void readGKAFile(String fileName) {
        try {
            // Input
            File file = new File(Objects.requireNonNull(utils.class.getResource(fileName)).getFile());

            // Output
            String dir = "src/main/graphviz/";
            File dotDirectory = new File(dir);
            // Replace input extension with output extension
            String dotFileName = file.getName().replaceFirst("[.][^.]+$", "") + ".dot";
            File outputFile;
            createDirectory(dotDirectory);

            outputFile = new File(dir + File.separator + dotFileName);
            // Override existing file
            deleteFile(outputFile);
            createFile(outputFile);

            FileWriter writer = new FileWriter(outputFile, true);

            // Process graph orientation
            writer.write(isGraphDirected(file) ? "digraph {" : "graph {");
            writer.write(System.lineSeparator());

            // Process attributes as notes, edges, labels and weights
            processAttributes(file, outputFile, writer);

            writer.write("}");
            writer.close();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Imports a graph from given dot file into JGraphT data-structure
     *
     * @param fileName Dot file to be imported from
     * @return Successfully generated JGraphT graph or null otherwise
     */
    static AbstractBaseGraph importGraph(String fileName) {
        // Relative path because outside of resource folder due to graphviz visualization
        File file = new File("src/main/graphviz/" + fileName);

        // Graph object to be passed to DOTImporter
        AbstractBaseGraph graph;
        DOTImporter importer;
        boolean isWeighted = isDOTGraphWeighted(file);
        boolean isDirected = isDOTGraphDirected(file);

        if (isWeighted) {
            if (isDirected) {
                graph = new DirectedWeightedPseudograph<String, DefaultWeightedEdge>(DefaultWeightedEdge.class);
            } else {
                graph = new WeightedPseudograph<String, DefaultWeightedEdge>(DefaultWeightedEdge.class);
            }

            // Provider for DOTImporter, Access to vertices, edges and attributes
            importer = new DOTImporter<>(
                    (vertex, a) -> vertex,
                    (from, to, weight, obj) -> {
                        DefaultWeightedEdge edge = new DefaultWeightedEdge();
                        if (obj.containsKey("weight")) {
                            graph.setEdgeWeight(edge, Double.parseDouble(weight));
                        } else {
                            System.out.println("[ERROR]: Weight missing");
                        }
                        return edge;
                    });
        } else {
            if (isDirected) {
                graph = new DirectedPseudograph<String, DefaultEdge>(DefaultEdge.class);
            } else {
                graph = new Pseudograph<String, DefaultEdge>(DefaultEdge.class);
            }

            // Provider for DOTImporter, Access to vertices, edges and attributes
            importer = new DOTImporter<>(
                    (vertex, a) -> vertex,
                    (from, to, weight, obj) -> {
                        DefaultEdge edge = new DefaultEdge();
                        return edge;
                    });
        }

        try {
            // JGraphT import writes into graph object
            importer.importGraph(graph, file);

        } catch (ImportException e) {
            e.printStackTrace();
        }

        return graph;
    }

    /**
     * Exports a JGraphT graph to a file with given file name
     *
     * @param graph    JGraphT graph
     * @param fileName Given file name
     */
    static void exportGraph(AbstractBaseGraph graph, String fileName) {
        String dir = "src/main/graphviz/";
        File dotDirectory = new File(dir);
        String dotFileName = fileName + ".dot";
        File outputFile;

        try {
            createDirectory(dotDirectory);

            outputFile = new File(dir + File.separator + dotFileName);
            if (outputFile.exists()) {
                outputFile.delete();
                System.out.println("Deleted existing file");
            }

            boolean isCreated = outputFile.createNewFile();
            if (isCreated) {
                System.out.println("Created new File: " + outputFile.getPath());
            } else {
                throw new IOException("Unable to create new file");
            }

            // File writer for writing output file while processing input file
            FileWriter writer = new FileWriter(outputFile, true);

            DOTExporter exporter;
            boolean isWeighted = graph.getType().isWeighted();

            if (isWeighted) {
                exporter = new DOTExporter<String, DefaultWeightedEdge>(
                        new StringComponentNameProvider<>(),
                        null,
                        null,
                        null,
                        (e) -> {
                            Map<String, Attribute> map = new LinkedHashMap<>();
                            Double weight = graph.getEdgeWeight(e);
                            // Check whether weight has decimal place so we can generate integers OR doubles
                            if (weight == Math.floor(weight)) {
                                map.put("weight", DefaultAttribute.createAttribute(Integer.toString((int) graph.getEdgeWeight(e))));
                                map.put("label", DefaultAttribute.createAttribute(Integer.toString((int) graph.getEdgeWeight(e))));
                            } else {
                                map.put("weight", DefaultAttribute.createAttribute(Double.toString(graph.getEdgeWeight(e))));
                                map.put("label", DefaultAttribute.createAttribute(Double.toString(graph.getEdgeWeight(e))));
                            }
                            return map;
                        }
                );
            } else {
                exporter = new DOTExporter<String, DefaultEdge>(
                        new StringComponentNameProvider<>(),
                        null,
                        null,
                        null,
                        null
                );
            }

            exporter.exportGraph(graph, writer);

            writer.close();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Use Breadth-First Search (BFS) to return path from source to target node
     *
     * @param graph Graph to be searched
     * @param start Source node
     * @param end   Target node
     * @return Path from source to target node
     */
    static List<Object> BFS(AbstractBaseGraph graph, String start, String end, List<Object> resultingPath) {
        if (start.equals(end)) {
            Collections.reverse(resultingPath);
            return resultingPath;
        }

        BreadthFirstIterator graphIterator = new BreadthFirstIterator(graph, start);
        Object current;
        // Traverse the graph until target node is found
        while (graphIterator.hasNext()) {
            Object vertex = graphIterator.next();
            if (vertex.equals(end)) {
                current = graphIterator.getParent(vertex);
                resultingPath.add(graph.getEdge(current, vertex));
                BFS(graph, start, current.toString(), resultingPath);
            }
        }

        return resultingPath;
    }

    /**
     * Dijkstra shortest path
     * @param graph Graph to be processed
     * @param start Source node
     * @param end Target node
     * @return Shortest path
     */
    static GraphPath Dijkstra(AbstractBaseGraph graph, String start, String end) {
        return DijkstraShortestPath.findPathBetween(graph, start, end);
    }

    /**
     * Creates directory
     *
     * @param directory Directory to be created
     * @throws IOException If creation fails
     */
    private static void createDirectory(File directory) throws IOException {
        if (directory.mkdir()) {
            System.out.println("Create directory");
        } else if (directory.exists()) {
            System.out.println("Directory exists");
        } else {
            throw new IOException("Unable to create directory");
        }
    }

    /**
     * Creates new given file
     *
     * @param file File to be created
     * @throws IOException If creation not successful
     */
    private static void createFile(File file) throws IOException {
        boolean isCreated = file.createNewFile();
        if (isCreated) {
            System.out.println("Create new File: " + file.getPath());
        } else {
            throw new IOException("Unable to create new file");
        }
    }

    /**
     * Deletes a given file
     *
     * @param file File to be deleted
     * @throws IOException If deletion not successful
     */
    private static void deleteFile(File file) throws IOException {
        if (file.exists()) {
            if (file.delete()) {
                System.out.println("Delete existing file");
            } else {
                throw new IOException("Unable to delete existing file: " + file.getAbsolutePath());
            }
        }
    }

    /**
     * Process input file sign by sign to build up graph by specific pattern
     * <name node1>[ -> <name node2> [(<edge name>)][: <edgeweight>]]; as directed graph
     * <name node1>[ -- <name node2> [(<edge name>)][: <edgeweight>]]; as undirected graph
     * Examples (every example works for undirected graphs as well):
     * node
     * node1 -> node2
     * node1 -> node2 (name)
     * node1 -> node2 : weight
     * node1 -> node2 (name) : weight
     *
     * @param inputFile  Input *.gka file
     * @param outputFile Output *.dot file
     */
    private static void processAttributes(File inputFile, File outputFile, FileWriter writer) {
        // Directed or undirected
        String edge;
        String directedEdge = "->";
        String undirectedEdge = "--";
        String nameBracketOpen = "\\(";
        String nameBracketClosed = "\\)";
        String weightSign = ":";

        // RegEx
        Pattern pattern;
        Matcher matcher;

        // Result to be written in output file
        StringBuilder sb = new StringBuilder();

        // Use auto-closable scanner for processing file
        try (Scanner scanner = new Scanner(inputFile, "ISO-8859-1")) {

            while (scanner.hasNext()) {
                String line = scanner.nextLine();
                // Remove white spaces
                line = line.replaceAll("\\s+", "");
                if (line.isEmpty()) continue;

                // --- EDGE
                boolean containsEdge = line.contains(directedEdge) || line.contains(undirectedEdge);
                if (containsEdge) {
                    // Set graph orientation
                    edge = line.contains(directedEdge)
                            ? directedEdge
                            : undirectedEdge;

                    // --- NODE 1
                    pattern = regexAllUpToLiteral(edge);
                    matcher = pattern.matcher(line);
                    if (matcher.find()) {
                        sb.append(matcher.group());
                    } else {
                        throw new IOException("Unable to find content before edge");
                    }

                    // --- EDGE
                    sb.append(edge);

                    // --- NAME, WEIGHT
                    boolean containsName = line.contains("(") && line.contains(")");
                    boolean containsWeight = line.contains(weightSign);

                    if (containsName && containsWeight) {
                        // --- NODE 2
                        pattern = regexAllBetweenLiterals(edge, nameBracketOpen);
                        matcher = pattern.matcher(line);
                        if (matcher.find()) {
                            sb.append(matcher.group());
                        } else {
                            throw new IOException("Unable to find content between edge and name bracket open");
                        }

                        // --- NAME
                        pattern = regexAllBetweenLiterals(nameBracketOpen, nameBracketClosed);
                        matcher = pattern.matcher(line);
                        if (matcher.find()) {
                            sb.append("[taillabel=\"(");
                            sb.append(matcher.group());
                            sb.append(")");
                        } else {
                            throw new IOException("Unable to find content between name brackets");
                        }
                        // --- WEIGHT
                        pattern = regexAllAfterLiteral(weightSign);
                        matcher = pattern.matcher(line);
                        if (matcher.find()) {
                            String weight = matcher.group(1).replace(";", "");
                            sb.append("\", headlabel=\"");
                            sb.append(weight);
                            sb.append("\", weight=\"");
                            sb.append(weight);
                            sb.append("\"]");
                        } else {
                            throw new IOException("Unable to find content between name bracket closed and weight sign");
                        }
                    } else if (containsName) {
                        // --- NODE 2
                        pattern = regexAllBetweenLiterals(edge, nameBracketOpen);
                        matcher = pattern.matcher(line);
                        if (matcher.find()) {
                            sb.append(matcher.group());
                        } else {
                            throw new IOException("Unable to find content between edge and name bracket open");
                        }

                        // --- NAME
                        pattern = regexAllBetweenLiterals(nameBracketOpen, nameBracketClosed);
                        matcher = pattern.matcher(line);
                        if (matcher.find()) {
                            sb.append("[label=\"(");
                            sb.append(matcher.group());
                            sb.append(")\"]");
                        } else {
                            throw new IOException("Unable to find content between name brackets");
                        }
                    } else if (containsWeight) {
                        // --- NODE 2
                        pattern = regexAllBetweenLiterals(edge, weightSign);
                        matcher = pattern.matcher(line);
                        if (matcher.find()) {
                            sb.append(matcher.group());
                        } else {
                            throw new IOException("Unable to find content between edge and weight sign");
                        }

                        // --- WEIGHT
                        pattern = regexAllAfterLiteral(weightSign);
                        matcher = pattern.matcher(line);
                        if (matcher.find()) {
                            String weight = matcher.group(1).replace(";", "");

                            if (weight.equals("")) throw new IOException("Missing weight: " + inputFile);

                            sb.append("[label=\"");
                            sb.append(weight);
                            sb.append("\", weight=\"");
                            sb.append(weight);
                            sb.append("\"]");
                        } else {
                            throw new IOException("Unable to find content between name bracket closed and weight sign");
                        }
                    } else {
                        // --- NODE 2
                        pattern = regexAllAfterLiteral(edge);
                        matcher = pattern.matcher(line);
                        if (matcher.find()) {
                            sb.append(matcher.group(1).replace(";", ""));
                        } else {
                            throw new IOException("Unable to find content after edge");
                        }
                    }
                } else {
                    // --- NODE 1
                    sb.append(line.replaceAll(";", ""));
                }

                // End of line
                sb.append(";");
                sb.append(System.lineSeparator());

                // Finally write line into output file
                writer.write(replaceUmlauts(sb.toString()));
                sb.setLength(0);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Return a compiled regex pattern that matches 'all up to a specific given literal'
     *
     * @param literal Literal
     * @return Compiled regex pattern
     */
    private static Pattern regexAllUpToLiteral(String literal) {
        return Pattern.compile(".+?(?=" + literal + ")");
    }

    /**
     * Return a compiled regex pattern that matches 'all after specific given literal'
     *
     * @param literal Literal
     * @return Compiled regex pattern
     */
    private static Pattern regexAllAfterLiteral(String literal) {
        return Pattern.compile(literal + "(.*)");
    }

    /**
     * Return a compiled regex pattern that matches 'all between two specific given literals'
     *
     * @param literal1 First literal
     * @param literal2 Second literal
     * @return Compiled regex pattern
     */
    private static Pattern regexAllBetweenLiterals(String literal1, String literal2) {
        return Pattern.compile("(?<=" + literal1 + ")(.*)(?=" + literal2 + ")");
    }

    /**
     * Check whether graph in given file is directed or undirected
     *
     * @param inputFile Input graph
     * @return True if directed, false otherwise
     */
    private static boolean isGraphDirected(File inputFile) {
        // Use auto-closable scanner for processing file
        try (Scanner scanner = new Scanner(inputFile, "ISO-8859-1")) {

            while (scanner.hasNext()) {
                String line = scanner.nextLine();
                if (line.contains("->") || line.contains("--")) {
                    return line.contains("->");
                }
            }

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        return false;
    }

    /**
     * Check whether graph in given dot file is directed or undirected
     *
     * @param inputFile Input graph
     * @return True if directed, false otherwise
     */
    private static boolean isDOTGraphDirected(File inputFile) {
        // Use auto-closable scanner for processing file
        try (Scanner scanner = new Scanner(inputFile, "ISO-8859-1")) {

            if (scanner.hasNext()) {
                String line = scanner.nextLine();
                return line.contains("digraph");
            }

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        return false;
    }

    /**
     * Check whether graph in given file is weighted or not
     *
     * @param inputFile Input graph
     * @return True if weighted, false otherwise
     */
    private static boolean isDOTGraphWeighted(File inputFile) {
        // Use auto-closable scanner for processing file
        try (Scanner scanner = new Scanner(inputFile, "ISO-8859-1")) {

            while (scanner.hasNext()) {
                String line = scanner.nextLine();
                if (line.contains("->") || line.contains("--")) {
                    return line.contains("weight");
                }
            }

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        return false;
    }

    /**
     * Replaces German umlauts with corresponding usages
     *
     * @param orig Original string
     * @return String after replacing all umlauts
     */
    private static String replaceUmlauts(String orig) {
        // All umlauts
        String[][] umlauts = {
                {"Ä", "Ae"},
                {"Ü", "Ue"},
                {"Ö", "Oe"},
                {"ä", "ae"},
                {"ü", "ue"},
                {"ö", "oe"},
                {"ß", "ss"}
        };

        String result = orig;

        for (String[] replacement : umlauts) {
            result = result.replace(replacement[0], replacement[1]);
        }

        return result;
    }
}