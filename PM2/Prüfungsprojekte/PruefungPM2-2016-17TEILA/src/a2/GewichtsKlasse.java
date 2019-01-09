package a2;

/**
 * 
 * @author Birgit Wendholt
 *
 * Gewichtsklassen zur Klassifikation von Gewichtsgrößen. Einordnen in die Gewichtsklassen unterstützt die  Methode @see #enthaltenIn(double)
 */
public enum GewichtsKlasse {
	
	LEICHT {
		@Override
		public boolean enthaltenIn(double gewicht) {
			return 0<= gewicht&& gewicht < L1;
		}
	}, MITTEL {
		@Override
		public boolean enthaltenIn(double gewicht) {
			return L1<= gewicht&& gewicht < L2;
		}
	}, SCHWER {
		@Override
		public boolean enthaltenIn(double gewicht) {
			return gewicht >= L2;
		}
	};
	public abstract boolean enthaltenIn(double gewicht);		
	private static final int L1 = 6000;
	private static final int L2 = 8000;
}
