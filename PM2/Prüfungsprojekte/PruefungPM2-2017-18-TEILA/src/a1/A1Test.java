package a1;

import static a1.A1.collectLeafs;
import static a1.A1.reverse;
import static a1.A1.sumNIter;
import static a1.A1.sumNRek;
import static a1.A1.toRegular;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.HashSet;

import org.junit.Before;
import org.junit.Test;

import a1.donottouch.Node;

public class A1Test {

	private int n1 = 1;
	private int n45 = 45;
	private int n1000 = 1000;
	private int n0 = 0;
	private int n_14 = -14;
	private Integer[] emptyAry;
	private Integer[] intAry;
	private Integer[] intAryReverse;
	private double[][] double2dAry0;
	private double[][] double2dAryReg0;
	private double[][] double2dAry1;
	private double[][] double2dAryReg1;
	private double[][] double2dAry2;
	private double[][] double2dAryReg2;
	private double[][] double2dAry3;
	private double[][] double2dAryReg3;
	private Node oneNode = new Node(1);
	private Node multiNodes = new Node(1);
	Node n12;
	Node n13;
	Node n112; 
	Node n1111;

	@Before
	public void init() {
		emptyAry = new Integer[] {};
		intAry = new Integer[] { 1, 2, 3, 4, 5, 6, 7, 8 };
		intAryReverse = new Integer[] { 8, 7, 6, 5, 4, 3, 2, 1 };
		create2dArys();
		createNodes();		
	}

	private void create2dArys() {
		double2dAry0 = new double[][] {};
		double2dAryReg0 = new double[][] {};
		double2dAry1 = new double[][] { { 0.0 }, {}, { 1.0, 0.0 }, { 1.0 }, {}, { 0.0 }, {} };
		double2dAryReg1 = new double[][] { { 0.0, 0.0 }, { 0.0, 0.0 }, { 1.0, 0.0 }, { 1.0, 0.0 }, { 0.0, 0.0 },
				{ 0.0, 0.0 }, { 0.0, 0.0 } };
		double2dAry2 = new double[][] { { 1.0, 1.0, 1.0, 0.0, 0.0 }, {}, { 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0 }, {},
				{ 1.0, 0.0, 0.0 }, {}, { 1.0, 1.0, 0.0 } };
		double2dAryReg2 = new double[][] { { 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 },
				{ 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 },
				{ 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 },
				{ 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0 } };
		double2dAry3 = new double[][] { {}, { 0.0 }, { 0.0, 0.0 }, { 1.0 }, { 0.0, 0.0, 0.0, 0.0 } };
		double2dAryReg3 = new double[][] { { 0.0, 0.0, 0.0, 0.0 }, { 0.0, 0.0, 0.0, 0.0 }, { 0.0, 0.0, 0.0, 0.0 },
				{ 1.0, 0.0, 0.0, 0.0 }, { 0.0, 0.0, 0.0, 0.0 } };
	}

	private void createNodes() {
		oneNode = new Node(1);
		Node n11 = new Node(11);
		n12 = new Node(12);
		n13 = new Node(13);
		multiNodes.add(n11);
		multiNodes.add(n12);
		multiNodes.add(n13);
		Node n111 = new Node(111);
		n112 = new Node(112);
		n11.add(n111);
		n11.add(n112);
		n1111 = new Node(1111);
		n111.add(n1111);
	}

	@Test
	public void testSumIter() {
		assertEquals(n1 * (n1 + 1) / 2, sumNIter(n1));
		assertEquals(n45 * (n45 + 1) / 2, sumNIter(45));
		assertEquals(n1000 * (n1000 + 1) / 2, sumNIter(n1000));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSumIterNeg() {
		try {
			sumNIter(n0);
		} catch (IllegalArgumentException iae) {
			sumNIter(n_14);
		}
	}

	@Test
	public void testSumRek() {
		assertEquals(n1 * (n1 + 1) / 2, sumNRek(n1));
		assertEquals(n45 * (n45 + 1) / 2, sumNRek(45));
		assertEquals(n1000 * (n1000 + 1) / 2, sumNRek(n1000));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSumRekNeg() {
		try {
			sumNRek(n0);
		} catch (IllegalArgumentException iae) {
			sumNRek(n_14);
		}
	}

	@Test
	public void testToRegular() {
		assertArrayEquals(double2dAryReg0, toRegular(double2dAry0));
		assertArrayEquals(double2dAryReg1, toRegular(double2dAry1));
		assertArrayEquals(double2dAryReg2, toRegular(double2dAry2));
		assertArrayEquals(double2dAryReg3, toRegular(double2dAry3));
		assertArrayEquals(double2dAryReg1, toRegular(double2dAryReg1));
	}

	@Test
	public void testCollectLeafs() {
		assertEquals(Arrays.asList(oneNode), collectLeafs(oneNode));
		assertEquals(new HashSet<>(Arrays.asList(n12,n13,n112,n1111)), new HashSet<>(collectLeafs(multiNodes)));
	}

	@Test
	public void testReverse() {
		assertArrayEquals(intAryReverse, reverse(intAry));
		// reverse darf intAry nicht verändert haben
		assertArrayEquals(intAry, reverse(intAryReverse));
		assertArrayEquals(new Integer[] {}, reverse(emptyAry));
	}
}
