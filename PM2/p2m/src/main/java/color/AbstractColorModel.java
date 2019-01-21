package color;

public abstract class AbstractColorModel implements ColorModel {
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + Double.hashCode(getRed());
        result = prime * result + Double.hashCode(getGreen());
        result = prime * result + Double.hashCode(getBlue());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;

        if (!(obj instanceof AbstractColorModel))
            return false;

        AbstractColorModel other = (AbstractColorModel) obj;

        return (Double.compare(getRed(), other.getRed()) == 0)
                && (Double.compare(getGreen(), other.getGreen()) == 0)
                && (Double.compare(getBlue(), other.getBlue())) == 0;
    }
}
