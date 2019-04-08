class RecursiveDrawLine {
	public static void main(String args[]) {
		Window.open(800, 600);

		drawLine(0, 0, 10, 10, 2, 1);

		//drawLine(30, 400, 600, 80, 10, 1);
		//drawLine(300, 50, 350, 500, 10, 1);
		//drawLine(20, 60, 700, 500, 10, 1);
	}

	public static void drawLine(int x1, int y1, int x2, int y2, int radius,
		int distance)
	{
		int pointDist = (int)Math.sqrt(Math.pow(x2 - x1, 2) + Math.pow(y2 - y1, 2));

		Out.format("[x1:%d/y1:%d, x2:%d/y2:%d] pd: %d, gd: %d%n", x1, y1, x2, y2,
			pointDist, distance);

		if (x1 == x2 && y1 == y2) {
			Out.println("drawing circles");

			Window.drawCircle(x1, y1, radius);
			Window.drawCircle(x2, y2, radius);

			return;
		} else {
			int mx = ((x2 + x1) / 2);
			int my = ((y2 + y1) / 2);

			drawLine(x1, y1, mx, my, radius, distance);
			drawLine(mx, my, x2, y2, radius, distance);
		}
	}
}