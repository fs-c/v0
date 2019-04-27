class BasicShapes {
	static String inputFile = "./shapes.txt";

	public static void main(String[] args) {
		if (args.length != 0)
			inputFile = args[0];

		In.open(inputFile);

		Window.open(In.readInt(), In.readInt());

		while (!In.isEof()) {
			switch (In.readChar()) {
			case 'P':
				(new Point()).draw();
			break;
			case 'L':
				(new Line()).draw();
			break;
			case 'R':
				(new Rectangle()).draw();
			break;
			case 'C':
				(new Circle()).draw();
			break;
			default:
			}
		}
	}
}

class Point {
	int x;
	int y;

	Point() {
		x = In.readInt();
		y = In.readInt();
	}

	void draw() {
		Window.drawPoint(x, y);
	}
}

class Line {
	Point begin;
	Point end;

	Line() {
		begin = new Point();
		end = new Point();
	}

	void draw() {
		Window.drawLine(begin.x, begin.y, end.x, end.y);
	}
}

class Rectangle {
	Point start;
	int width;
	int height;

	Rectangle() {
		start = new Point();
		width = In.readInt();
		height = In.readInt();
	}

	void draw() {
		Window.drawRectangle(start.x, start.y, width, height);
	}
}

class Circle {
	Point start;
	int radius;

	Circle() {
		start = new Point();
		radius = In.readInt();
	}

	void draw() {
		Window.drawCircle(start.x, start.y, radius);
	}
}
