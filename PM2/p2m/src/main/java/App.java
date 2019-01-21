import color.Container;
import color.HSVColor;
import color.RGBColor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

class App {

    public static void main(String[] args) {
        ArrayList<Integer> a = new ArrayList<>();
        a.add(1);
        a.add(2);
        a.add(3);

        ArrayList<Number> b = new ArrayList<>();
        b.add(4);
        b.add(5);
        b.add(6);

        copyAIntoB(a, b);
        System.out.println(b);
    }

    static int countElems(Collection<?> aCol) {
        return aCol.size();
    }

    static <T> void copyAIntoB(List<? extends T> a, List<T> b) {
        b.addAll(a);
    }

}
