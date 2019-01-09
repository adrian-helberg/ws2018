package a2.donottouch.helpers;

import java.util.Random;
import java.util.function.Supplier;

import a2.donottouch.Kurs;

public class KursGenerator implements Supplier<Kurs> {

	private int maxKurse;
	private Random repro = new Random(1001998);
	
	public KursGenerator(int maxKurse) {
		this.maxKurse = maxKurse; 
	}
	
	@Override
	public Kurs get() {
		return new Kurs(repro.nextInt(maxKurse));
	}
}
