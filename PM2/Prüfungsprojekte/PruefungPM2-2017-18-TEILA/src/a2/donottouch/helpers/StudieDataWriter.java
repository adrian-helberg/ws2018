package a2.donottouch.helpers;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import static a2.donottouch.helpers.StudieGeneratorConstants.*;

import a2.donottouch.Studie;

public class StudieDataWriter {
	
	public static void main(String[] args) throws IOException{

		List<Studie> lstd1 = StudieGenerator.generateList(AMOUNT, MAT_START, MAX_KURSE, MIN_PT, MAX_PT);
//		List<Studie> lstd2 = StudieGenerator.generateList(20, 0, 10, 0, 15);
//		
//		System.out.println(lstd1.equals(lstd2));
		
		System.out.println("Write to studiedata.ddsv");
		try (PrintWriter pw = new PrintWriter(new FileWriter("studiedata.ddsv"))) {
			lstd1.forEach(std -> pw.println(std.toSeparatedString("#", "::")));
		};
	}

}
