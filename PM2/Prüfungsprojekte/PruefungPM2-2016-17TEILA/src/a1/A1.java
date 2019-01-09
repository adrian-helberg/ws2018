package a1;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

public class A1 {

	/**
	 * TODO 5 Pkt
	 * 
	 * Berechnet die Formel summe k = 3..n 8*k/(k**2-1)**2 Wirft eine
	 * IllegalArgumentException, wenn n ausserhalb des gueltigen Wertebereichs,
	 * d.h. n <3.
	 * 
	 * @param n
	 *            Schrittweite der Naeherung
	 * @return double berechneter Wert der Naeherungsformel
	 */
	public static double sum(int n) {
		return 0.0;
	}

	/**
	 * TODO 5 Pkt Berechnet das erste n, fuer die der exakte Wert von der
	 * Naeherungsformel um < eps abweicht. Fuer dieses n gilt dann: exakt-sum(n) <
	 * eps. Der Wert von exakt ist 13.0/18 = 0.7222222222222222
	 * 
	 * @param eps
	 *            (double) maximaler Fehler der Naeherung < 1
	 * @return int das erste n, fuer das gilt: exakt-sum(n) < eps
	 */
	public static int fehlerSumKleinerEps(double eps) {
		return 0;
	}

	/**
	 * TODO (10 Pkt)
	 * 
	 * osterWelt ist ein unregelmaessiges 3-dimensionales Array, das an zufaelligen
	 * Positionen "easter egg" Eintraege enthaelt. Die Methode soll die Positionen
	 * der "easter egg" Eintraege als Liste von 3-elementigen Listen zurueckgeben.
	 * Das erste Element der 3-elem Liste ist die x, das zweite die y, das
	 * dritte die z Position des jeweiligen "easter egg" Eintrages. Nutzen Sie
	 * die Konstante EASTER_EGG, um Tippfehler zu vermeiden.
	 * 
	 * Beispieleingabe: 
	 * 
	 * [ [[null, null, null, null], [null], [null, null]], 
	 *   [[null]], 
	 *   [[easter egg, null, null, null, null, null], [null, null, null], [null, null, null, null], [null], [easter egg, null]], 
	 *   [[easter egg, null, null], [null]], 
	 *   [[null, null, null, null, null, easter egg]], 
	 *   [[easter egg, null, easter egg, null]]
	 * ]
	 * 
	 * Beispielergebnis: [[2, 0, 0], [2, 4, 0], [3, 0, 0], [4, 0, 5], [5, 0, 0], [5, 0, 2]]
	 * 
	 * @param osterWelt
	 *            3-dimensionales unregelmaessiges Arrays
	 * @return Liste mit 3 elementigen Listen, die die x,y,z Position der
	 *         "easter egg" Eintraege enthaelt.
	 */
	private static final String EASTER_EGG = "easter egg";

	public static List<List<Integer>> sucheOsterEier(String[][][] osterWelt) {
		return null;
	}

	/**
	 * TODO (10Pkt)
	 * 
	 * Gegeben eine beliebig geschachtelte Collection von Collections. Bestimmen
	 * Sie die maximale Schachtelungstiefe.
	 * 
	 * @param col
	 *            die Collection von Collection
	 * @return int die maximale Schachtelungstiefe. Die aesseere Collection wird
	 *         nicht mitgezaehlt
	 */
	public static int maxDepth(Collection<?> col) {
		return 0;
	}
}
