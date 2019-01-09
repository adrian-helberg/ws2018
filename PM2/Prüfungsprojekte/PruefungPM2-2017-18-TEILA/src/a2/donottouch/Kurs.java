package a2.donottouch;

public class Kurs {
	
	private int nummer;

	public Kurs(int nummer) {
		this.nummer = nummer;
	}

	@Override
	public String toString() {
		return "k"+nummer;
	}

	public int getNummer() {
		return nummer;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + nummer;
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Kurs other = (Kurs) obj;
		if (nummer != other.nummer)
			return false;
		return true;
	}

	
	
}
