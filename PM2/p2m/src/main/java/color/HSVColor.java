package color;

public class HSVColor extends AbstractColorModel implements ColorModel {
    private double hue;
    private double saturation;
    private double brightness;

    public HSVColor(double hue, double saturation, double brightness) {
        this.hue = hue;
        this.saturation = saturation;
        this.brightness = brightness;
    }

    public HSVColor(AbstractColorModel c) {
        this.hue = c.getHue();
        this.saturation = c.getSaturation();
        this.brightness = c.getBrightness();
    }

    @Override
    public double getRed() {
        return hue / 2;
    }

    @Override
    public double getGreen() {
        return saturation / 2;
    }

    @Override
    public double getBlue() {
        return brightness / 2;
    }

    @Override
    public double getHue() {
        return hue;
    }

    @Override
    public double getSaturation() {
        return saturation;
    }

    @Override
    public double getBrightness() {
        return brightness;
    }
}
