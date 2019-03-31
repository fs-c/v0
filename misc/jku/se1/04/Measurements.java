class Measurements {
	static final int VALUES_SIZE = 10;

	static void main(String[] args) {
		In.open(args[0]);
		Out.open("./results.txt");

		double[] values = new double[VALUES_SIZE];

		int index = 0;
		while (!In.done() && ++index) {
			double cur = In.readDouble();

			if (values.length < VALUES_SIZE) {
				values[index] = cur;
			} else {
				// Push back items from [1, VALUES_SIZE] by one
				for (int i = 0; i < VALUES_SIZE - 1; i++)
					values[i + 1] = values[i];

				values[VALUES_SIZE - 1] = cur;
			}

			index %= VALUES_SIZE;
		}

		In.close();
		Out.close();
	}
}
