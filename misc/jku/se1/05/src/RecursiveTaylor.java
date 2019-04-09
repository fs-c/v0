class RecursiveTaylor {
    public static void main(String[] args) {

    }

    static double expTaylor(double x, int i) {
        if (i == 1)
            return 1;

        return x + expTaylor((1 / factorial(i)) * Math.pow(x, i), i - 1);
    }

    static double factorial(double n) {
        if (n == 1)
            return 1;

        return n * factorial(n - 1);
    }
}