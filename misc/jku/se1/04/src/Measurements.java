import java.util.Locale;

class Measurements {
	static final int VALUES_SIZE = 10;

	public static void main(String[] args) {
		In.open(args[0]);
		Out.open(args[1]);

		int[] hist = new int[4];
		double[] values = new double[VALUES_SIZE];

		Out.println("Window means:");

		int index = 0;
		while (!In.isEof()) {
			double cur = In.readDouble();

			if (cur < 10.0) {
				hist[0]++;
			} else if (cur < 20.0) {
				hist[1]++;
			} else if (cur < 30.0) {
				hist[2]++;
			} else hist[3]++;

			for (int i = 0; i < values.length - 1; i++)
				values[i] = values[i + 1];

			values[values.length - 1] = cur;

			if (index++ < 10)
				continue;

			double mean = 0;
			for (double val : values)
				mean += val;

			mean /= values.length;

			Out.print(String.format(Locale.US, "%.1f ", mean));
		}

		Out.format("%nHistogram:%n[ 0.0, 10.0):%6d%n[10.0, 20.0):%6d"
				+ "%n[20.0, 30.0):%6d%n[30.0, 50.0]:%6d", hist[0], hist[1],
				hist[2], hist[3]);

		In.close();
		Out.close();
	}
}
