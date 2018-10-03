import org.jgrapht.graph.*;
import org.jgrapht.io.*;
import java.io.*;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Utils for managing import, export and files
 *
 * @author Adrian Helberg
 */
final class utils {
    /**
     * Reads gka file with given name and converts it into a dot file
     * @param fileName File to import
     */
    static void readGKAFile(String fileName) {
        try {
            // Use ClassLoader for accessing resource files
            Class cls = Class.forName("utils");
            ClassLoader cl = cls.getClassLoader();
            File file = new File(Objects.requireNonNull(cl.getResource(fileName + ".gka")).getFile());

            convertToDot(file);

        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

    /**
     * Imports a graph from given dot file into JGraphT data-structure
     * @param fileName Dot file to be imported from
     * @return Successfully generated JGraphT graph or null otherwise
     */
    static AbstractBaseGraph importGraph(String fileName) {
        // Relative path because outside of resource folder due to graphviz visualization
        File file = new File("src/main/graphviz/" + fileName + ".dot");

        // Graph object to be passed to DOTImporter
        AbstractBaseGraph<String, DefaultWeightedEdge> graph = isGraphDirected(file)
                ? new DirectedWeightedPseudograph<>(DefaultWeightedEdge.class)
                : new WeightedPseudograph<>(DefaultWeightedEdge.class);

        // Provider for DOTImporter, Access to vertices, edges and attributes
        DOTImporter<String, DefaultWeightedEdge> importer = new DOTImporter<>(
                (l, a) -> l,
                (f, t, l, a) -> {
                    DefaultWeightedEdge edge = new DefaultWeightedEdge();
                    if (a.containsKey("weight")) graph.setEdgeWeight(edge, Double.parseDouble(l));
                    return edge;
                });

        try {
            // JGraphT import writes into graph object
            importer.importGraph(graph, file);

        } catch (ImportException e) {
            e.printStackTrace();
        }

        return graph;
    }

    static boolean exportGraph(AbstractBaseGraph graph, String fileName) {
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

            ComponentAttributeProvider<String> edgeAttributeProvider =
                    e -> {
                        Map<String, Attribute> map = new LinkedHashMap<>();
                        map.put("label", DefaultAttribute.createAttribute(e));
                        return map;
                    };

            GraphExporter<String, DefaultWeightedEdge> exporter = new DOTExporter<String, DefaultWeightedEdge>(
                    new IntegerComponentNameProvider<>(),
                    null,
                    null,
                    edgeAttributeProvider,
                    null
            );
            exporter.exportGraph(graph, writer);

            writer.close();

        } catch (IOException | ExportException e) {
            e.printStackTrace();
        }
        return true;
    }

    /**
     * Converts given gka file to dot file
     * @param inputFile File to be converted
     */
    private static void convertToDot(File inputFile) {
        String dir = "src/main/graphviz/";
        File dotDirectory = new File(dir);
        String dotFileName = inputFile.getName().replaceFirst("[.][^.]+$", "") + ".dot";
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

            // Process graph orientation
            writer.write(isGraphDirected(inputFile) ? "digraph {" : "graph {");
            writer.write(System.lineSeparator());

            // Process attributes as notes, edges, labels and weights
            processAttributes(inputFile, outputFile, writer);

            writer.write("}");
            writer.close();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Creates directory for graphviz files
     * @param dotDirectory Directory to be created
     * @throws IOException If creation fails
     */
    private static void createDirectory(File dotDirectory) throws IOException {
        if (dotDirectory.mkdir()) {
            System.out.println("Created 'graphviz' directory");
        } else if (dotDirectory.exists()) {
            System.out.println("'graphviz' directory exists");
        } else {
            throw new IOException("Unable to create 'graphviz' directory");
        }
    }

    /**
     * Process input file sign by sign to build up graph by specific pattern
     * <name node1>[ -> <name node2>][(edge name)][: <edgeweight>]; as directed graph
     * <name node1>[ -- <name node2>][(edge name)][: <edgeweight>]; as undirected graph
     * Examples:
     * node
     * node (name)
     * node1 -- node2
     * node1 -> node2
     * node1 -- node2 (name)
     * node1 -- node2 :weight
     *
     * @param inputFile  Input *.gka file
     * @param outputFile Output *.dot file
     */
    private static void processAttributes(File inputFile, File outputFile, FileWriter writer) {
        String node1 = null;
        String node2 = null;
        String edge = null;
        String name = null;
        String weight = null;

        Pattern pattern;
        Matcher matcher;

        // Final string to be written in output file
        StringBuilder sb = new StringBuilder();

        // Use auto-closable scanner for processing file
        try (Scanner scanner = new Scanner(inputFile, "ISO-8859-1")) {

            while (scanner.hasNext()) {
                String line = scanner.nextLine();
                // Remove white spaces
                line = line.replaceAll("\\s+", "");
                if (line.isEmpty()) continue;

                boolean containsEdge = line.contains("->") || line.contains("--");
                if (containsEdge) {
                    // If there is an edge, there must be two nodes
                    edge = line.contains("->") ? "->" : "--";
                    pattern = regexAllUpToLiteral(edge);

                    matcher = pattern.matcher(line);
                    if (matcher.find()) {
                        // Write node1
                        sb.append(matcher.group());
                        sb.append(edge);
                    }

                    boolean containsWeightOrName = line.contains("(") || line.contains(":");
                    if (containsWeightOrName) {
                        if (line.contains(":")) {
                            // Contains weight
                            pattern = regexAllBetweenLiterals(edge, ":");

                            matcher = pattern.matcher(line);
                            if (matcher.find()) {
                                // Write node2
                                sb.append(matcher.group(1));
                            }

                            pattern = regexAllAfterLiteral(":");

                            matcher = pattern.matcher(line);
                            applyLabeling(matcher, sb, true);
                        } else {
                            // Contains name
                            pattern = regexAllBetweenLiterals(edge, "\\(");

                            matcher = pattern.matcher(line);
                            if (matcher.find()) {
                                // Write node2
                                sb.append(matcher.group(1));
                            }

                            pattern = regexAllBetweenLiterals("\\(", "\\)");

                            matcher = pattern.matcher(line);
                            applyLabeling(matcher, sb, false);
                        }
                    } else {
                        pattern = regexAllAfterLiteral(edge);

                        matcher = pattern.matcher(line);
                        if (matcher.find()) {
                            // Write node2
                            sb.append(matcher.group(1).replaceAll(";", ""));
                        }
                    }

                } else {
                    // No edge -> single node
                    if (line.contains("(")) {
                        pattern = regexAllUpToLiteral("\\(");
                        matcher = pattern.matcher(line);
                        if (matcher.find()) {
                            // Write node1
                            sb.append(matcher.group());
                        }
                        // Labeled node
                        pattern = regexAllBetweenLiterals("\\(", "\\)");
                        matcher = pattern.matcher(line);
                        applyLabeling(matcher, sb, false);
                    } else {
                        // Write node1
                        sb.append(line.replaceAll(";", ""));
                    }
                }

                // End of line
                sb.append(";");
                sb.append(System.lineSeparator());

                // Finally write line into output file
                writer.write(sb.toString());
                sb.setLength(0);
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Handles labeling
     * @param matcher    Regex matcher
     * @param sb         StringBuilder with appended labeling
     * @param isWeighted True for weight labeling false otherwise
     */
    private static void applyLabeling(Matcher matcher, StringBuilder sb, boolean isWeighted) {
        if (matcher.find()) {
            // Remove semicolon (line ending in dot file)
            String value = matcher.group(1).replaceAll(";", "");
            if (!value.isEmpty()) {
                // Write attributes
                sb.append("[label=");
                sb.append(value);


                if (isWeighted) {
                    sb.append(", weight=");
                    sb.append(value);
                }

                sb.append("]");
            }
        }
    }

    /**
     * Return a compiled regex pattern that matches 'all up to a specific given literal'
     * @param literal Literal
     * @return Compiled regex pattern
     */
    private static Pattern regexAllUpToLiteral(String literal) {
        return Pattern.compile(".+?(?=" + literal + ")");
    }

    /**
     * Return a compiled regex pattern that matches 'all after specific given literal'
     * @param literal Literal
     * @return Compiled regex pattern
     */
    private static Pattern regexAllAfterLiteral(String literal) {
        return Pattern.compile(literal + "(.*)");
    }

    /**
     * Return a compiled regex pattern that matches 'all between two specific given literals'
     * @param literal1 First literal
     * @param literal2 Second literal
     * @return Compiled regex pattern
     */
    private static Pattern regexAllBetweenLiterals(String literal1, String literal2) {
        return Pattern.compile("(?<=" + literal1 + ")(.*)(?=" + literal2 + ")");
    }

    /**
     * Check whether graph in given file is directed or undirected
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
     * Replaces German umlauts with corresponding usages
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