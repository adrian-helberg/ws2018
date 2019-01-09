package a2;

import static a2.donottouch.helpers.StudieGeneratorConstants.AMOUNT;
import static a2.donottouch.helpers.StudieGeneratorConstants.MAT_START;
import static a2.donottouch.helpers.StudieGeneratorConstants.MAX_KURSE;
import static a2.donottouch.helpers.StudieGeneratorConstants.MAX_PT;
import static a2.donottouch.helpers.StudieGeneratorConstants.MIN_PT;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.FileNotFoundException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.junit.Before;
import org.junit.Test;

import a2.donottouch.Kurs;
import a2.donottouch.Studie;
import a2.donottouch.helpers.StudieGenerator;

public class A2Test {
	
	// Die Liste der Studies für den Test.
	private List<Studie> lstd; 
	private StudieDataReader reader = new StudieDataReader("studiedata.ddsv", "#", "::");
	private Studie studie1;
	private Studie clever;
	private final Integer m76 = 76;
	private final Integer m43 = 43;
	private final int m132 = 132;
	private Set<Kurs> kurseGesamt = new HashSet<>();
	
	@Before
	public void init() {
		lstd = StudieGenerator.generateList(AMOUNT, MAT_START, MAX_KURSE, MIN_PT, MAX_PT);
		studie1 = lstd.stream().filter(st -> st.getMatnr().equals("M8")).findFirst().orElse(null);
		clever = lstd.stream().filter(st -> st.getMatnr().equals("M16")).findFirst().orElse(null);
		kurseGesamt = IntStream.range(0, 10).mapToObj(n -> new Kurs(n)).collect(Collectors.toSet());
	}
	
	@Test
	public void testStudieDataReader() {
		try {
			assertEquals(lstd, reader.readStudieList());
		} catch (FileNotFoundException fnfe) {
			fail();
		}
	}
	
//	@Test
//	public void testFleissigsterStudie() {
//		assertEquals(fleiss, StudieDataStatistics.fleissigsterStudie(lstd));
//	}

	@Test
	public void testClevesterStudie() {
		assertEquals(clever, StudieDataStatistics.cleversterStudie(lstd));
	}

	@Test
	public void testPunkteProStudie() {
		assertEquals(m76, StudieDataStatistics.punkteProStudie(clever));
		assertEquals(m43, StudieDataStatistics.punkteProStudie(studie1));
	}
	
	@Test
	public void testAnzahlKurseGesamt() {
		assertEquals(m132, StudieDataStatistics.anzahlKurseGesamt(lstd));
	}

	@Test
	public void testBelegteKurse() {
		assertEquals(kurseGesamt,StudieDataStatistics.belegteKurse(lstd));
	}
}
