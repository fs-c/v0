class RecursiveTaylor {
	public static void main(String[] args) {
		Out.print("x: ");
		int x = In.readInt();

		Out.print("i_max: ");
		int i = In.readInt();

		double libTaylor = Math.exp(x);
		double customTaylor = expTaylor(x, i);

		Out.format("%nx: %f%nexpTaylor: %f%nMath.exp: %f%ndiff: %f%n", (double) x, customTaylor, libTaylor,
				Math.abs(libTaylor - customTaylor));
	}

	static double expTaylor(double x, int i) {
		if (i == 0)
			return 1;

		return (1 / factorial(i)) * Math.pow(x, i) + expTaylor(x, i - 1);
	}

	static double factorial(double n) {
		if (n <= 1)
			return 1;

		return n * factorial(n - 1);
	}
}