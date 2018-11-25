package sample.exception;

/**
 * @author Maximilian Janzen & Adrian Helberg
 */
public class InvalidGraphException extends Exception {

	/**
	 * Fehlerausgabe fuer Graphen
	 */
	private static final long serialVersionUID = 1893751709182730979L;

	public InvalidGraphException() {
		System.out.println("Fehler im Graphen!");
	}

	public InvalidGraphException(String arg0) {
		super(arg0);
	}

	public InvalidGraphException(Throwable arg0) {
		super(arg0);
	}

	public InvalidGraphException(String arg0, Throwable arg1) {
		super(arg0, arg1);
	}

	public InvalidGraphException(String arg0, Throwable arg1, boolean arg2, boolean arg3) {
		super(arg0, arg1, arg2, arg3);
	}

}
