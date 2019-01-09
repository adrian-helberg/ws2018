package a2.donottouch;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;

public class Studie {
	
	private String matnr;
	private Map<Kurs,Integer> kurse = new HashMap<>();

	public Studie(String matnr) {
		this.matnr = matnr;
	}
	
	public void put(Kurs kurs, Integer punkte) {
		kurse.put(kurs, punkte);
	}

	public Integer get(Kurs kurs) {
		return kurse.get(kurs);
	}
	
	public boolean hasKurs(Kurs kurs) {
		return kurse.containsKey(kurs);
	}
	
	public Set<Kurs> getKurse(){
		return kurse.keySet();
	}
	
	public Collection<Integer> getPunkte(){
		return kurse.values();
	}

	public Set<Entry<Kurs,Integer>> getKurseUndPunkte(){
		return kurse.entrySet();
	}
	
	public String getMatnr() {
		return matnr;
	}
	@Override
	public String toString() {
		return String.format("Studie %s %s",matnr,kurse);
	}
	
	public String toSeparatedString(String sep1, String sep2) {
		String kurseSeparatedString = kurse.entrySet().stream().map(entry -> entry.getKey().getNummer() + sep2 + entry.getValue()).collect(Collectors.joining(sep1));
		return matnr + (kurseSeparatedString.isEmpty() ? "": sep1 + kurseSeparatedString);
	}
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((kurse == null) ? 0 : kurse.hashCode());
		result = prime * result + ((matnr == null) ? 0 : matnr.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Studie other = (Studie) obj;
		if (kurse == null) {
			if (other.kurse != null)
				return false;
		} else if (!kurse.equals(other.kurse))
			return false;
		if (matnr == null) {
			if (other.matnr != null)
				return false;
		} else if (!matnr.equals(other.matnr))
			return false;
		return true;
	}
}
