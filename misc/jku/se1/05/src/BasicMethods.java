import java.util.Arrays; // For Arrays.toString() 

class BasicMethods {
	public static void main(String[] args) {
		int value = -5;
		int[] values = { 1, 2, 3, 4, -5 };
		double[] floatingValues = { 1.1, 2.2, 3.3, 4.4 };

		Out.format("|%d| = %d%n", value, abs(value));

		value = 50;

		Out.format("|%d| = %d%n", value, abs(value));

		Out.format("min%s = %d%n", Arrays.toString(values), min(values));
		Out.format("avg%s = %f%n", Arrays.toString(floatingValues),
			average(floatingValues));
		
		value = -5;

		Out.format("%d is %sin %s%n", value,
			exists(values, value) ? "" : "not ", Arrays.toString(values));

		value = 10;
		
		Out.format("%d is %sin %s%n", value,
			exists(values, value) ? "" : "not ", Arrays.toString(values));
	}

	static int abs(int x) {
		return x < 0 ? x * -1 : x;
	}

	static int min(int[] values) {
		int smallest = Integer.MAX_VALUE;

		for (int val : values)
			if (val < smallest)
				smallest = val;

		return smallest;
	}

	static double average(double[] values) {
		double sum = 0;

		for (double val : values)
			sum += val;

		return values.length == 0 ? 0 : sum / values.length;
	}

	static boolean exists(int[] values, int x) {
		for (int val : values)
			if (val == x)
				return true;

		return false;
	}
}