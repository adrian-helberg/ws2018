import java.io.*;
import java.util.Objects;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Utils for managing files
 * @author Adrian Helberg
 */
final class utils {
    /**
     * Imports a file with given name and converts it into a dot file
     * @param fileName File to import
     */
    static void importGraph(String fileName) {
        try {
            // Use ClassLoader for accessing files
            Class cls = Class.forName("utils");
            ClassLoader cl = cls.getClassLoader();
            File file = new File(Objects.requireNonNull(cl.getResource(fileName + ".gka")).getFile());

            convertToDot(file);

        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

    /**
     * Converts *.gka file to *.dot file
     * @param file File to convert
     */
    private static void convertToDot(File file) {
        String dir = "I:\\git\\ws2018\\GKA\\gkap\\src\\main\\graphviz\\";
        File dotDirectory = new File(dir);
        String dotFileName = file.getName().replaceFirst("[.][^.]+$", "") + ".dot";

        // We need two scanners here; One for detection whether graph is directed or undirected,
        // one for processing each line (first scanner would skip first line on using nextLine())
        try (Scanner scanner = new Scanner(file, "ISO-8859-1")) {

            boolean isCreated = dotDirectory.mkdir();
            if (isCreated) {
                System.out.println("Created 'graphviz' directory");
            } else if (dotDirectory.exists()) {
                System.out.println("'graphviz' directory exists");
            } else {
                throw new IOException("Unable to create 'graphviz' directory");
            }

            File dotFile = new File(dir + File.separator + dotFileName);
            if (dotFile.exists()) {
                dotFile.delete();
                System.out.println("Deleted existing file");
            }

            isCreated = dotFile.createNewFile();
            if (isCreated) {
                System.out.println("Created new File: " + dotFile.getPath());
            } else {
                throw new IOException("Unable to create new file");
            }

            FileWriter writer = new FileWriter(dotFile);
            Matcher matcher;
            // Search for numbers
            String regex = "\\d+";
            Pattern pattern = Pattern.compile(regex);

            // Initially check whether input graph is directed or undirected
            boolean directed;
            if (scanner.hasNext()) {
                String firstLine = scanner.nextLine();
                directed = firstLine.contains("->");

                writer.write(directed ? "digraph {" : "graph {");
                writer.write(System.lineSeparator());

                processAttributes(writer, pattern, firstLine);

                writer.write(System.lineSeparator());
            } else {
                throw new IOException(file.getName() + " is empty.");
            }

            // Process input file line by line
            while (scanner.hasNext()) {
                String line = scanner.nextLine();

                processAttributes(writer, pattern, line);

                writer.write(System.lineSeparator());
            }

            writer.write("}");
            writer.close();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static void processAttributes(FileWriter writer, Pattern pattern, String firstLine) throws IOException {
        Matcher matcher;
        if (firstLine.contains(":")) {
            String[] lineParts = firstLine.split(":");

            if (lineParts.length != 2) {
                throw new IOException("input file has wrong formatted line");
            }

            writer.write(lineParts[0]);

            matcher = pattern.matcher(lineParts[1]);
            if (matcher.find()) {
                writer.write("[label=\"" + matcher.group() + "\"]");
                writer.write(";");
            }
        } else {
            writer.write(firstLine);
        }
    }
}