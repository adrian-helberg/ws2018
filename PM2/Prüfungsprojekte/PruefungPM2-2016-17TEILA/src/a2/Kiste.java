package a2;

public class Kiste {

	
	private double gewichtProEinheit;
	private String bezeichnung;
	private double wertProEinheit;
	private WarenTyp warenTyp;
	private int anzahl;
	
	/**
	 * Konstruktor
	 * @param bezeichnung
	 * @param warenTyp  @see a3.WarenTyp (enum)
	 * @param gewichtProEinheit Gewicht als double in kg
	 * @param wertProEinheit, Wert pro Einheit in Euro als double
	 * @param anzahl, int anzahl der enthaltenen Einheiten
	 */
	public Kiste(String bezeichnung,WarenTyp warenTyp, double gewichtProEinheit, double wertProEinheit, int anzahl) {
		this.bezeichnung = bezeichnung;
		this.warenTyp = warenTyp;
		this.wertProEinheit = wertProEinheit;
		this.gewichtProEinheit = gewichtProEinheit;
		this.anzahl = anzahl;
	}

	/**
	 * Getter für das Gewicht
	 * @return Gewicht (int)
	 */
	public double gewicht() {
		return gewichtProEinheit*anzahl;
	}
	
	/**
	 * Getter für die Bezeichnung
	 * @return  Bezeichnung (String)
	 */
	public String getBezeichnung() {
		return bezeichnung;
	}

	/**
	 * Gesamtwarenwert in der Kiste
	 * @return Wert in Euro als double
	 */
	public double wert() {
		return wertProEinheit*anzahl;
	}

	/**
	 * Getter für den Warentyp 
	 * @return den WarenTyp
	 */
	public WarenTyp getWarenTyp() {
		return warenTyp;
	}
	
	/**
	 * Aufbereitung als Zeichenkette
	 */
	@Override
	public String toString() {
		return String.format("%s %2.2f€/%4.2fkg", bezeichnung,wert(),gewicht());
	}

}
