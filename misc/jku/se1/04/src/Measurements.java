import java.util.Locale;

class Measurements {
	static final int VALUES_SIZE = 10;

	public static void main(String[] args) {
		In.open(args[0]);
		Out.open(args[1]);

		double[] values = new double[VALUES_SIZE];

		int index = 0;
		while (!In.isEof()) {
			double cur = In.readDouble();

			if (index < VALUES_SIZE) {
				values[index++] = cur;

				continue;
			} else {
				for (int i = 0; i < values.length - 1; i++)
					values[i] = values[i + 1];

				values[values.length - 1] = cur;
			}

			double mean = 0;
			for (double val : values)
				mean += val;

			mean /= values.length;

			Out.println(String.format(Locale.US, "%.1f", mean));
		}

		In.close();
		Out.close();
	}
}
