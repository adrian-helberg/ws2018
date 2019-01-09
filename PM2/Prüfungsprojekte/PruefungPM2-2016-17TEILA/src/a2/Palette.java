package a2;

import java.util.ArrayList;
import java.util.List;

/**
 * Klasse zur ReprÃ¤sentation von Paletten. Eine Palette kann nur Waren desselben Typs aufnehmen. Der Typ wird bei der Erzeugung der Palette mit übergeben. 
 * Gesamtpunktzahl: 7 Pkt
 * @author Birgit Wendholt
 *
 */
public class Palette {
	
	private int nettoGewicht;
	private List<Kiste> waren = new ArrayList<>();
	private WarenTyp warenTyp;
	private int id;
	private static int counter = 0;

	/**
	 * Konstruktor
	 * @param nettoGewicht Nettogewicht der Palette in kg (ganzzahlig).
	 * @param warenTyp Warentyp, der auf diese Palette gepackt werden kann.
	 */
	public Palette(int nettoGewicht,WarenTyp warenTyp){
		this.nettoGewicht = nettoGewicht;
		this.warenTyp = warenTyp;
		this.id = ++counter;
	}
	
	
	public String getId(){
		return "P"+id;
	}
	/**
	 * Anzahl der Kisten der Palette
	 */
	
	public int size(){
		return waren.size();
	}
	/**
	 * Beladen der Palette
	 * @param ware: eine Ware, die auf die Palette gepackt wird. Der Warentyp der Ware muss mit dem der Palette übereinstimmen.
	 */
	public void packen(Kiste ware) {
		if (ware.getWarenTyp() == warenTyp)
		waren.add(ware);
	}
	
	/**
	 * Getter für den Warentyp
	 * @return WarenTyp der Waren der Palette
	 */
	public WarenTyp getWarenTyp() {
		return warenTyp;
	}
	
	/**
	 * Aufbereitung als Zeichenkette.
	 */
	public String toString(){
		return String.format("%s-%s %4.2f€/%4.2fkg--%s\n ",getId(),warenTyp,wert(),gewicht(),waren);
	}
	
	
	/////!!!!!!!!!!!!TODOS!!!!!!!!!!!!!!!!!!!/////
	/**
	 * TODO 
	 * Bestimmt das Gewicht der Palette als Summe des Nettogewichtes und der Gewichte aller Waren mit Hilfe des Streaming API's
	 * @return Gewicht in (kg) als double.
	 */
	public double gewicht() {
		return 0.0;
	}
	
	/**
	 * TODO 
	 * Bestimmt den Wert der Palette als Summe der Werte aller Waren.
	 * @return Gewicht in (kg) ganzzahlig.
	 */
	public double wert() {
		return 0.0;
	}

}
