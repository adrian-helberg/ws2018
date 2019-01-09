package a1;

import static org.junit.Assert.*;
import static a1.A1.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import org.junit.Before;
import org.junit.Test;

public class A1Test {

	private double expected = 13.0 / 18;
	private int kneg1;
	private int kneg2;
	private int kgrenze3;
	private int k4;
	private int k100;
	private int k10000;

	private double eps3;
	private double eps4;
	private double eps100;
	private double eps10000;

	private final String EASTER_EGG = "easter egg";
	private String[][][] osterLeer;
	private String[][][] oster1MitLeer;
	private String[][][] oster2MitLeer;
	private String[][][] osterGefuellt;

	private List<List<Integer>> osterEier;

	private final int MAX_LENGTH = 7;
	private int seed = 11111;
	private Random generator = new Random(seed);

	private Collection<?> collGrenze1;
	private Collection<?> collGrenze2;

	private final int EASTER_GRENZE = 100;
	private final int EASTER_THRESHOLD = 80;

	@Before
	public void init() {
		kneg1 = 2;
		kneg2 = -4;
		kgrenze3 = 3;
		k4 = 4;
		k100 = 100;
		k10000 = 10000;

		eps3 = 0.3472222222222223;
		eps4 = 0.20499999999999997;
		eps100 = 3.9605920988107e-4;
		eps10000 = 3.9996002287878697E-8;

		osterEier = new ArrayList<List<Integer>>();
		osterEier.add(Arrays.asList(2, 0, 0));
		osterEier.add(Arrays.asList(2, 4, 0));
		osterEier.add(Arrays.asList(3, 0, 0));
		osterEier.add(Arrays.asList(4, 0, 5));
		osterEier.add(Arrays.asList(5, 0, 0));
		osterEier.add(Arrays.asList(5, 0, 2));

		osterLeer = new String[][][] {};
		oster1MitLeer = new String[][][] { new String[][] {}, new String[][] {} };
		oster2MitLeer = new String[][][] { new String[][] { new String[] {}, new String[] {} }, new String[][] {} };
		int xLength = generator.nextInt(MAX_LENGTH);
		osterGefuellt = new String[xLength][][];
		int yLength;
		int zLength;

		for (int i = 0; i < xLength; i++) {
			seed += 2;
			yLength = new Random(seed).nextInt(MAX_LENGTH);
			yLength = (yLength == 0 ? 1 : yLength);
			osterGefuellt[i] = new String[yLength][];
			for (int j = 0; j < yLength; j++) {
				seed += 2;
				zLength = new Random(seed).nextInt(MAX_LENGTH);
				zLength = (zLength == 0 ? 1 : zLength);
				osterGefuellt[i][j] = new String[zLength];
				for (int k = 0; k < zLength; k++) {
					if (generator.nextInt(EASTER_GRENZE) > EASTER_THRESHOLD) {
						osterGefuellt[i][j][k] = EASTER_EGG;
					}
				}
			}
		}

		System.out.println(Arrays.deepToString(osterGefuellt));
		System.out.println(osterEier);
		collGrenze1 = new ArrayList<>();
		collGrenze2 = new HashSet<>(Arrays.asList(new Object(), 1, 3, 4.0, "hi", new int[][] {}));
	}

	@Test
	public void sumGrenzeTest() {
		assertEquals(expected, sum(kgrenze3), eps3);
	}

	@Test(expected = IllegalArgumentException.class)
	public void sumNegTest1() {
		sum(kneg1);
	}

	@Test(expected = IllegalArgumentException.class)
	public void sumNegTest2() {
		sum(kneg2);
	}

	@Test
	public void sumTest() {
		assertEquals(expected, sum(k4), eps4);
		assertEquals(expected, sum(k100), eps100);
		assertEquals(expected, sum(k10000), eps10000);
	}

	@Test
	public void fehlerSumTest() {
		assertEquals(4, fehlerSumKleinerEps(eps4));
		assertEquals(3, fehlerSumKleinerEps(eps3));
		assertEquals(100, fehlerSumKleinerEps(eps100));
		assertEquals(10000, fehlerSumKleinerEps(eps10000));
	}

	@Test
	public void sucheOsterEierGrenzeTest() {
		assertEquals(new ArrayList<>(), sucheOsterEier(osterLeer));
		assertEquals(new ArrayList<>(), sucheOsterEier(oster1MitLeer));
		assertEquals(new ArrayList<>(), sucheOsterEier(oster2MitLeer));

	}

	@Test
	public void sucheOsterEierTest() {
		assertEquals(osterEier, sucheOsterEier(osterGefuellt));
	}

	@Test
	public void maxDepthGrenzeTest() {
		assertEquals(0, maxDepth(collGrenze1));
		assertEquals(0, maxDepth(collGrenze2));
	}

	@Test
	public void maxDepthTest() {
		Collection<?> colli = createNestedCollection(6);
		assertEquals(6, maxDepth(colli));
		colli = createNestedCollection(4);
		assertEquals(4,  maxDepth(colli));
	}

	private Collection<?> createNestedCollection(int depth) {
		Collection<Object> co;
		int type = generator.nextInt(100);
		if (type % 3 == 0) {
			co = new ArrayList<>();
		} else if (type % 3 == 1) {
			co = new HashSet<>();
		} else {
			co = new LinkedList<>();
		}
		if (depth == 0) {
			return co;
		}
		int bound = generator.nextInt(8);
		for (int i = 0; i < bound; i++) {
			if (generator.nextBoolean()) {
				co.add(generator.nextInt(100));
			} else {
				co.add(createNestedCollection(depth - 1));
			}
		}
		return co;
	}
}
