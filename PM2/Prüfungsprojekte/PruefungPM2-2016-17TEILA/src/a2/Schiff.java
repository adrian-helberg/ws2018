package a2;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Klasse für Schiffe
 * Gesamtpunktzahl: 10 Pkt 
 * 		a.) und b.) 5 Pkt
 * 		c.) 5 Pkt
 * @author Birgit Wendholt
 *
 * @param <T> ein zu Container kompatibler Typ
 */
public class Schiff<T extends Container> {

	private List<T> ladung = new ArrayList<>();
	private String name;
		
	public Schiff(String string){
		this.name = string;
	}
	
	@Override
	public String toString() {
		return String.format("%s:%s", name, ladung );
	}
	
	/**
	 * Lädt einen Container auf das Schiff
	 * @param c der zu ladende Container
	 */
	public void beladen(T c) {
		ladung.add(c);
	}	

	public Container containerMitId(String id){
		for(Container c: ladung) {
			if (c.getId().equals(id)) return c;
		}
		return null;
	}
	
	public int indexOf(String containerId){
		int index = 0;
		for(Container c: ladung) {
			if (c.getId().equals(containerId)) return index;
			index++;
		}
		return -1;
	}
	
	
	public Palette paletteMitId(String id){
		for(Container c : ladung){
			if (c.paletteMitId(id) != null) {
				return c.paletteMitId(id);
			}
		}
		return null;
	}
	
	/////!!!!!!!!!!!!TODOS!!!!!!!!!!!!!!!!!!!/////
	/**
	 * Liefert eine Liste von Containern, die nach Wert sortiert sind. Lösung mittels Streaming API.
	 * @return  Liste von Containern
	 */
	public List<T> ladungNachWert(){
		 return null;
	}
				
	/**
	 * TODO
	 * Liefert eine Liste von Containern, die nach Gewicht sortiert sind. Lösung mittels Streaming API.
	 * @return  Liste von Containern
	 */
	public List<T> ladungNachGewicht() {
		return null;
	}
	
	/**
	 * TODO
	 * Liefert eine Liste von Containern, die nach GewichtsKlasse sortiert sind. Lösung mittels einer anonymen inneren Klasse.
	 * Instanzvariable ladung darf dabei dabei nicht modifiziert werden.
	 * 
	 * Hinweis: Enums (GewichtsKlasse ist ein Enum) sind Comparable
	 * @return  Liste von Containern
	 */
	public List<T> ladungNachGewichtsKlasse() {
		return null;
	}

}
