package a2.donottouch.helpers;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.function.Supplier;
import java.util.stream.IntStream;

import a2.donottouch.Studie;

public class StudieGenerator implements Supplier<Studie> {

	private int matStart;
	private int maxKurse;
	private final KursGenerator kg;
	private final Supplier<Integer> ptGenerator; 

	public StudieGenerator(int matStart, int maxKurse, int minPt, int maxPt) {
		this.matStart = matStart;
		this.maxKurse = maxKurse;
		kg = new KursGenerator(maxKurse);
		ptGenerator= new Supplier<Integer>() {
			private Random reproPt = new Random(1876998);
			@Override
			public Integer get() {
				return reproPt.nextInt(maxPt-minPt+1) + minPt;
			}
		};
	}
	
	@Override
	public Studie get() {
		matStart += 1;
		Studie studie = new Studie("M"+matStart);
		IntStream.range(0,maxKurse).forEach(_num -> studie.put(kg.get(), ptGenerator.get()));
		return studie;
	}
	
	public static List<Studie> generateList(int cap, int matStart, int maxKurse, int minPt, int maxPt){
		StudieGenerator sdg = new StudieGenerator(matStart, maxKurse, minPt, maxPt);
		List<Studie> lstd = new ArrayList<>();
		IntStream.range(0,cap).forEach(i_ -> {Studie std = sdg.get(); lstd.add(std); return;});
		return lstd;
	}
}
