import java.lang.Math;

public class SphereCalc {
	public static void main(String[] args) {
		Out.print("Geben Sie den Radius der Kugel ein: ");
		double radius = In.readDouble();

		double volume = (4.0 / 3.0) * Math.PI * Math.pow(radius, 3);
		Out.format("Volumen der Kugel: %.13f%n", volume);

		double surface = 4 * Math.PI * Math.pow(radius, 2);
		Out.format("Oberflaeche der Kugel: %.13f%n", surface);

		double ratio = surface / volume;
		Out.format("Verhaeltnis Oberflaeche/Volumen: %.1f%n", ratio);
	}
}