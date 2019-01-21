package color;

public class RGBColor extends AbstractColorModel implements ColorModel {

    private double red;
    private double green;
    private double blue;

    public RGBColor(double red, double green, double blue) {
        this.red = red;
        this.green = green;
        this.blue = blue;
    }

    public RGBColor(AbstractColorModel c) {
        this.red = c.getRed();
        this.green = c.getGreen();
        this.blue = c.getBlue();
    }

    @Override
    public double getRed() {
        return red;
    }

    @Override
    public double getGreen() {
        return green;
    }

    @Override
    public double getBlue() {
        return blue;
    }

    @Override
    public double getHue() {
        return red * 2;
    }

    @Override
    public double getSaturation() {
        return blue * 2;
    }

    @Override
    public double getBrightness() {
        return green * 2;
    }
}
