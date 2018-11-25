package sample.algorithmen;

/**
 * @author Maximilian Janzen & Adrian Helberg
 */
public class Start {

	public static void main(String[] args) {
		// Verlangt Graph Stream
				System.setProperty("org.graphstream.ui.renderer", "org.graphstream.ui.j2dviewer.J2DGraphRenderer");
				
		Comunicator comunicator = new Comunicator();
		comunicator.comunicate();
	}

	

}
