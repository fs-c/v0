class RecursiveDrawLine {
    public static void main(String[] args) {
        Window.open(800, 600);

        drawLine(30, 400, 600, 80, 10, 5);
        drawLine(300, 50, 350, 500, 10, 5);
        drawLine(20, 60, 700, 500, 10, 5);
    }

    static void drawLine(int x1, int y1, int x2, int y2, int radius,
        int distance)
    {
        double dist = Math.sqrt(Math.pow(x2 - x1, 2) + Math.pow(y2 - y1, 2));

        if (dist < distance) {
            Window.drawCircle(x1, y1, radius);
            Window.drawCircle(x2, y2, radius);

            return;
        }

        double midx = x1 + ((x2 - x1) / 2);
        double midy = y1 + ((y2 - y1) / 2);

        drawLine(x1, y1, (int)midx, (int)midy, radius, distance);
        drawLine((int)midx, (int)midy, x2, y2, radius, distance);
    }
}