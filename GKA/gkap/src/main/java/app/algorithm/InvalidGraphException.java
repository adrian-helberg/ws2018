package app.algorithm;

/**
 * GKAP invalid graph exception
 *
 * @author Adrian Helberg, Maximilian Janzen
 */
public class InvalidGraphException extends Exception {

    public InvalidGraphException() {
        System.out.println("Invalid graph");
    }

    public InvalidGraphException(String text) {
        super(text);
    }
}
