package a2;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

public class ContainerSchiffDatenGenerator {

	private static Random rand = new Random(111111);

	private static final int anzahlKistenProPalette = 8;
	private static final int anzahlPalettenProContainer = 10;
	public static final int ANZAHL_CONTAINER_PRO_SCHIFF = 10;

	public static final List<String> FERTIGWARE = Arrays.asList("Computer", "Maus", "Schuhe", "Netzteil");
	public static final List<Double> FERTIGWARE_PREIS_PRO_EINHEIT = Arrays.asList(1500.0, 59.0, 219.0, 39.0);
	public static final List<Double> FERTIGWARE_GEWICHT_PRO_EINHEIT = Arrays.asList(10.0, 0.01, 0.5, 0.3);
	public static final List<Integer> FERTIGWARE_ANZAHL_PRO_EINHEITS_KISTE = Arrays.asList(5, 100, 30, 50);

	private static List<String> lebensmittel = Arrays.asList("Milch", "Kaese", "Brokkoli", "Saft", "Nuesse",
			"Energy Drink");
	private static List<Double> lebensmittelPreisProEinheit = Arrays.asList(0.99, 2.19, 1.59, 1.39, 5.19, 1.19);
	private static List<Double> lebensmittelGewichtProEinheit = Arrays.asList(1.0, 1.0, 0.5, 1.0, 2.0, 0.3);
	private static List<Integer> lebensMittelAnzahlProEinheitsKiste = Arrays.asList(50, 150, 70, 50, 100, 200);

	private static List<String> genussmittel = Arrays.asList("Kaffee", "Tee", "Tabak", "Brandwein");
	private static List<Double> genussmittelPreisProEinheit = Arrays.asList(6.99, 5.00, 20.00, 30.00);
	private static List<Double> genussmittelGewichtProEinheit = Arrays.asList(0.5, 0.1, 0.2, 1.0);
	private static List<Integer> genussmittelAnzahlProEinheitsKiste = Arrays.asList(100, 400, 1000, 15);

	private static List<Kiste> generiereWarenKisten(WarenTyp wt) {
		switch (wt) {
		case Fertigwaren:
			return generiereWarenKisten(wt, FERTIGWARE, FERTIGWARE_PREIS_PRO_EINHEIT,
					FERTIGWARE_GEWICHT_PRO_EINHEIT, FERTIGWARE_ANZAHL_PRO_EINHEITS_KISTE);
		case Lebensmittel:
			return generiereWarenKisten(wt, lebensmittel, lebensmittelPreisProEinheit,
					lebensmittelGewichtProEinheit, lebensMittelAnzahlProEinheitsKiste);
		case Genussmittel:
			return generiereWarenKisten(wt, genussmittel, genussmittelPreisProEinheit,
					genussmittelGewichtProEinheit, genussmittelAnzahlProEinheitsKiste);
		default:
			return new ArrayList<>();
		}
	}
	
	private static List<Kiste> generiereWarenKisten(WarenTyp wt, List<String> ware, List<Double> warePreisProEinheit,
			List<Double> wareGewichtProEinheit, List<Integer> wareAnzahlProEinheitsKiste) {
		List<Kiste> kisten = new ArrayList<>();
		for (int i = 0; i < anzahlKistenProPalette; i++) {
			int auswahl = rand.nextInt(ware.size());
			Kiste kiste = new Kiste(ware.get(auswahl), wt, wareGewichtProEinheit.get(auswahl),
					warePreisProEinheit.get(auswahl), wareAnzahlProEinheitsKiste.get(auswahl));
			kisten.add(kiste);
		}
		return kisten;
	}

	private static List<Palette> beladePaletten() {
		List<Palette> lp = new ArrayList<>();
		for (int i = 0; i < anzahlPalettenProContainer; i++) {
			Palette pal = new Palette(10, WarenTyp.values()[(rand.nextInt(WarenTyp.values().length))]);
			generiereWarenKisten(pal.getWarenTyp()).stream().forEach(ware -> pal.packen(ware));
			lp.add(pal);
		}
		return lp;
	}

	private static List<Container> beladeContainer() {
		List<Container> lc = new ArrayList<>();
		for (int i = 0; i < ANZAHL_CONTAINER_PRO_SCHIFF; i++) {
			Container c = new Container(2000);
			beladePaletten().stream().forEach(pal -> c.beladen(pal));
			lc.add(c);
		}
		return lc;
	}

	public static Schiff<Container> beladeSchiff() {
		Schiff<Container> schiff = new Schiff<>("Riese");
		beladeContainer().stream().forEach(c -> schiff.beladen(c));
		return schiff;
	}
}
