package a2;

import static a2.ContainerSchiffDatenGenerator.ANZAHL_CONTAINER_PRO_SCHIFF;
import static a2.ContainerSchiffDatenGenerator.FERTIGWARE;
import static a2.ContainerSchiffDatenGenerator.FERTIGWARE_GEWICHT_PRO_EINHEIT;
import static a2.ContainerSchiffDatenGenerator.FERTIGWARE_PREIS_PRO_EINHEIT;
import static a2.ContainerSchiffDatenGenerator.FERTIGWARE_ANZAHL_PRO_EINHEITS_KISTE;



import static a2.GewichtsKlasse.LEICHT;
import static a2.GewichtsKlasse.MITTEL;
import static a2.GewichtsKlasse.SCHWER;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Before;
import org.junit.Test;


public class ContainerSchiffDatenTest {

	private Kiste k1;
	private Kiste k2;
	private static final Schiff<Container> sc = ContainerSchiffDatenGenerator.beladeSchiff();
	private static final List<String> ladungNachWertNurContainerId = Arrays.asList("C10", "C1", "C6", "C8", "C5", "C3",
			"C2", "C4", "C9", "C7");

	private static final List<String> ladungNachGewichtNurContainerId = Arrays.asList("C5", "C3", "C8", "C1", "C6",
			"C9", "C10", "C4", "C2", "C7");

	private static final List<Object> ladungNachGewichtsKlasseKurz = Arrays.asList("C3", LEICHT, "C5", LEICHT, "C1",
			MITTEL, "C4", MITTEL, "C6", MITTEL, "C8", MITTEL, "C9", MITTEL, "C10", MITTEL, "C2", SCHWER, "C7", SCHWER);

	@Before
	public void init() {
		 k1 = new Kiste(FERTIGWARE.get(0), WarenTyp.Fertigwaren, FERTIGWARE_GEWICHT_PRO_EINHEIT.get(0),
					FERTIGWARE_PREIS_PRO_EINHEIT.get(0), FERTIGWARE_ANZAHL_PRO_EINHEITS_KISTE.get(0));
		 k2 = new Kiste(FERTIGWARE.get(1), WarenTyp.Fertigwaren, FERTIGWARE_GEWICHT_PRO_EINHEIT.get(1),
					FERTIGWARE_PREIS_PRO_EINHEIT.get(1), FERTIGWARE_ANZAHL_PRO_EINHEITS_KISTE.get(1));
	}
	@Test
	public void testKistenWert() {
		assertTrue(Double.compare(FERTIGWARE_PREIS_PRO_EINHEIT.get(0) * FERTIGWARE_ANZAHL_PRO_EINHEITS_KISTE.get(0),
				k1.wert()) == 0);
	}

	@Test
	public void testKistenGewicht() {
		assertTrue(Double.compare(FERTIGWARE_GEWICHT_PRO_EINHEIT.get(0) * FERTIGWARE_ANZAHL_PRO_EINHEITS_KISTE.get(0),
				k1.gewicht()) == 0);
	}

	@Test
	public void testPalettePacken() {
		Palette pal = new Palette(10, WarenTyp.Fertigwaren);
		pal.packen(k1);
		pal.packen(k2);
		// System.out.println(pal);
		assertTrue(2 == pal.size());
	}

	@Test
	public void testPaletteWertLeer() {
		Palette pal = new Palette(10, WarenTyp.Fertigwaren);
		assertTrue(Double.compare(0.0, pal.wert()) == 0);
	}

	@Test
	public void testPaletteWert2Kisten() {
		Palette pal = new Palette(10, WarenTyp.Fertigwaren);
		pal.packen(k1);
		pal.packen(k2);
		assertTrue(Double.compare(13400.0, pal.wert()) == 0);
	}

	@Test
	public void testPaletteWertPalette13() {
		Palette pal13 = sc.paletteMitId("P13");
		// System.out.println(pal13);
		assertTrue(Double.compare(28547.0, pal13.wert()) == 0);
	}

	@Test
	public void testPaletteGewichtLeer() {
		Palette pal = new Palette(10, WarenTyp.Fertigwaren);
		assertTrue(Double.compare(10.0, pal.gewicht()) == 0);
	}

	@Test
	public void testPaletteGewicht2Kisten() {
		Palette pal = new Palette(10, WarenTyp.Fertigwaren);
		pal.packen(k1);
		pal.packen(k2);
		assertTrue(Double.compare(61.0, pal.gewicht()) == 0);
	}

	@Test
	public void testPaletteGewichtPalette13Und27() {
		Palette pal13 = sc.paletteMitId("P13");
		// System.out.println(pal);
		assertTrue(Double.compare(495.0, pal13.gewicht()) == 0);
		Palette pal27 = sc.paletteMitId("P27");
		// System.out.println(pal27);
		assertTrue(Double.compare(760.0, pal27.gewicht()) == 0);
	}

	@Test
	public void testContainerGewichtLeer() {
		Container c = new Container(200);
		// System.out.println(c);
		assertTrue(Double.compare(200.0, c.gewicht()) == 0);
	}

	@Test
	public void testContainerGewichtMit2Paletten() {
		Palette pal13 = sc.paletteMitId("P13");
		Palette pal27 = sc.paletteMitId("P27");
		Container c = new Container(200);
		c.beladen(pal13);
		c.beladen(pal27);
		// System.out.println(c);
		assertTrue(Double.compare(pal13.gewicht() + pal27.gewicht() + 200, c.gewicht()) == 0);
	}

	@Test
	public void testContainer8Gewicht() {
		Container c8 = sc.containerMitId("C8");
		// System.out.println(c8);
		assertTrue(Double.compare(6074.0, c8.gewicht()) == 0);
	}

	@Test
	public void testContainerWertLeer() {
		Container c = new Container(200);
		// System.out.println(c);
		assertTrue(Double.compare(0.0, c.wert()) == 0);
	}

	@Test
	public void testContainerWertMit2Paletten() {
		Palette pal13 = sc.paletteMitId("P13");
		Palette pal27 = sc.paletteMitId("P27");
		Container c = new Container(200);
		c.beladen(pal13);
		c.beladen(pal27);
		// System.out.println(c);
		assertTrue(Double.compare(pal13.wert() + pal27.wert(), c.wert()) == 0);
	}

	@Test
	public void testContainer8Wert() {
		Container c8 = sc.containerMitId("C8");
		//System.out.println(c8);
		assertTrue(Double.compare(308971.20, c8.wert()) == 0);
	}

	@Test
	public void testContainerGewichtsKlasse() {
		assertEquals(GewichtsKlasse.MITTEL, sc.containerMitId("C1").gewichtsKlasse());
		assertEquals(GewichtsKlasse.SCHWER, sc.containerMitId("C2").gewichtsKlasse());
		assertEquals(GewichtsKlasse.LEICHT, sc.containerMitId("C3").gewichtsKlasse());
		assertEquals(GewichtsKlasse.MITTEL, sc.containerMitId("C4").gewichtsKlasse());
		assertEquals(GewichtsKlasse.LEICHT, sc.containerMitId("C5").gewichtsKlasse());
		assertEquals(GewichtsKlasse.MITTEL, sc.containerMitId("C6").gewichtsKlasse());
		assertEquals(GewichtsKlasse.SCHWER, sc.containerMitId("C7").gewichtsKlasse());
		assertEquals(GewichtsKlasse.MITTEL, sc.containerMitId("C8").gewichtsKlasse());
		assertEquals(GewichtsKlasse.MITTEL, sc.containerMitId("C9").gewichtsKlasse());
		assertEquals(GewichtsKlasse.MITTEL, sc.containerMitId("C10").gewichtsKlasse());
	}

	@Test
	public void testLadungNachWertFuerBeladenesSchiff() {
		List<Container> lc = sc.ladungNachWert();
		ArrayList<Object> llo = new ArrayList<>();
		for (Container c : lc) {
			llo.add(c.getId());
		}
		assertEquals(ladungNachWertNurContainerId, llo);
	}

	@Test
	public void testLadungNachWertFuerLeeresSchiff() {
		List<Container> lc = new Schiff<Container>("Schlechte Zeiten").ladungNachWert();
		assertTrue(lc.isEmpty());
	}
	
	@Test
	public void testLadungNachGewichtFuerBeladenesSchiff() {
		List<Container> lc = sc.ladungNachGewicht();
		ArrayList<String> llo = new ArrayList<>();
		for (Container c : lc) {
			llo.add(c.getId());
		}
		assertEquals(ladungNachGewichtNurContainerId, llo);
	}

	@Test
	public void testLadungNachGewichtFuerLeeresSchiff() {
		List<Container> lc = new Schiff<Container>("Schlechte Zeiten").ladungNachGewicht();
		assertTrue(lc.isEmpty());
}


	@Test
	public void testLadungNachGewichtsKlasseFuerBeladenesSchiff() {
		List<Container> lc = sc.ladungNachGewichtsKlasse();
		ArrayList<Object> llo = new ArrayList<>();
		for (Container c : lc) {
			llo.add(c.getId());
			llo.add(c.gewichtsKlasse());
		}
		assertEquals(ladungNachGewichtsKlasseKurz, llo);
		
		for(int i = 1; i <= ANZAHL_CONTAINER_PRO_SCHIFF; i ++){
			if (!(sc.indexOf("C"+i) == (i-1))) {
				fail("Urspruengliche Ordnung der Container veraendert");
			}
		}
	}

	@Test
	public void testLadungNachGewichtsKlasseFuerLeeresSchiff() {
		List<Container> lc = new Schiff<Container>("Schlechte Zeiten").ladungNachGewichtsKlasse();
		assertTrue(lc.isEmpty());
	}

}
