package a2;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

import a2.donottouch.Kurs;
import a2.donottouch.Studie;

public class StudieDataReader {
	
	private String filename;
	private String sep1;
	private String sep2;

	public StudieDataReader(String filename,String sep1, String sep2) {
		this.filename = filename;	
		this.sep1 = sep1;
		this.sep2 = sep2;
	}

	// 20 Pkt Gesamt
	public List<Studie> readStudieList() throws FileNotFoundException{
		// TODO
		try(Scanner scan = new Scanner(new File(filename))){
		};
		return null; 
	}
}
