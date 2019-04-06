import java.awt.Color;
import java.awt.Component;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;

/**
 * This class supports output of graphical objects like points, lines, circles
 * and rectangles in as static window. The class provides a set of methods as
 * follows:
 * <ul>
 * <li>The method open initializes the class, creates a window and opens it.
 * </li>
 * <li>Methods drawPoint, drawLine, drawCircle and drawRectangle will draw
 * points, lines, circles and rectangles respectively. Further, there are
 * overloaded methods which also take a further color parameter to draw the
 * objects in the given color.</li>
 * <li>Then methoden fillCircle und fillRectangle will draw rectangles and
 * circles filled with the given color.</li>
 * </ul>
 *
 * @author Herbert Praehofer
 * @date
 */
public class Window {

// width and height

	/** Constant for the default width */
	public static final int DEFAULT_WIDTH = 800;

	/** Constant for the default height */
	public static final int DEFAULT_HEIGHT = 600;

	/** variable holding the width of the window */
	public static int width;

	/** variable holding the height of the window */
	public static int height;

// open

	/**
	 * Creates the output window with default width and height and opens it.
	 */
	public static void open() {
		open(DEFAULT_WIDTH, DEFAULT_HEIGHT);
	}

	/**
	 * Creates the output window with given width and height and opens it.
	 * 
	 * @param w the width of the window
	 * @param w the height of the window
	 */
	public static void open(int w, int h) {
		width = w;
		height = h;
		windowO = new Frame("WindowO");
		contentPane = new WindowOPanel();
		windowO.add(contentPane);
		image = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB);
		image.getGraphics().fillRect(0, 0, w, h);
		windowO.setSize(w + 12, h + headerHeight + 12);
		windowO.addWindowListener(new Window.WindowClosingAdapter(true));
		windowO.setVisible(true);
	}

	/** Clears the content of the window */
	public static void clear() {
		image.getGraphics().fillRect(0, 0, width, height);
		contentPane.repaint();
	}

// Methods for drawing 

	/**
	 * Draws a point at the given position (x, y).
	 * 
	 * @param x the x-coordinate for the point
	 * @param y the y-coordinate for the point
	 */
	public static void drawPoint(int x, int y) {
		Graphics g = image.getGraphics();
		g.setColor(Color.black);
		g.fillRect(x - 1, y - 1, 3, 3);
		contentPane.repaint();
	}

	/**
	 * Draws a line from a given coordinates x1/y1 to coordinates x2/y2
	 * 
	 * @param x1 the x-coordinate for the first point
	 * @param y1 the y-coordinate for the first point
	 * @param x2 the x-coordinate for the second point
	 * @param y2 the y-coordinate for the second point
	 */
	public static void drawLine(int x1, int y1, int x2, int y2) {
		Graphics g = image.getGraphics();
		g.setColor(Color.black);
		g.drawLine(x1, y1, x2, y2);
		contentPane.repaint();
	}

	/**
	 * Draws a rectangle at position x/y with width w and height h.
	 * 
	 * @param x the x-coordinate for the rectangle
	 * @param y the y-coordinate for the rectangle
	 * @param w the width of the rectangle
	 * @param h the height of the rectangle
	 */
	public static void drawRectangle(int x, int y, int w, int h) {
		Graphics g = image.getGraphics();
		g.setColor(Color.black);
		g.drawRect(x, y, w, h);
		contentPane.repaint();
	}

	/**
	 * Draws a circle at position x/y with radius r.
	 * 
	 * @param x the x-coordinate for the circle
	 * @param y the y-coordinate for the circle
	 * @param r the radius of the circle
	 */
	public static void drawCircle(int x, int y, int r) {
		Graphics g = image.getGraphics();
		g.setColor(Color.black);
		g.drawOval(x - r, y - r, 2 * r, 2 * r);
		contentPane.repaint();
	}

	/**
	 * Draws the text at the given position
	 * 
	 * @param text the text to draw
	 * @param x    the x-coordinate for the text
	 * @param y    the y-coordinate for the text
	 */
	public static void drawText(String text, int x, int y) {
		Graphics g = image.getGraphics();
		g.setColor(Color.black);
		g.drawString(text, x, y);
		contentPane.repaint();
	}

	/**
	 * Draws a point at the given position (x, y) with given color.
	 * 
	 * @param x     the x-coordinate for the point
	 * @param y     the y-coordinate for the point
	 * @param color the color
	 */
	public static void drawPoint(int x, int y, Color color) {
		Graphics g = image.getGraphics();
		g.setColor(color);
		g.fillRect(x - 1, y - 1, 3, 3);
		contentPane.repaint();
	}

	/**
	 * Draws a line from a given coordinates x1/y1 to coordinates x2/y2 with given
	 * color.
	 * 
	 * @param x1    the x-coordinate for the first point
	 * @param y1    the y-coordinate for the first point
	 * @param x2    the x-coordinate for the second point
	 * @param y2    the y-coordinate for the second point
	 * @param color the color
	 */
	public static void drawLine(int x1, int y1, int x2, int y2, Color color) {
		Graphics g = image.getGraphics();
		g.setColor(new Color(color.getRed(), color.getGreen(), color.getBlue()));
		g.drawLine(x1, y1, x2, y2);
		contentPane.repaint();
	}

	/**
	 * Draws a rectangle at position x/y with width w and height h with given color.
	 * 
	 * @param x     the x-coordinate for the rectangle
	 * @param y     the y-coordinate for the rectangle
	 * @param w     the width of the rectangle
	 * @param h     the height of the rectangle
	 * @param color the color
	 */
	public static void drawRectangle(int x, int y, int w, int h, Color color) {
		Graphics g = image.getGraphics();
		g.setColor(color);
		g.drawRect(x, y, w, h);
		contentPane.repaint();
	}

	/**
	 * Draws a circle at position x/y with radius r with given color.
	 * 
	 * @param x     the x-coordinate for the circle
	 * @param y     the y-coordinate for the circle
	 * @param r     the radius of the circle
	 * @param color the color
	 */
	public static void drawCircle(int x, int y, int r, Color color) {
		Graphics g = image.getGraphics();
		g.setColor(new Color(color.getRed(), color.getGreen(), color.getBlue()));
		g.drawOval(x - r, y - r, 2 * r, 2 * r);
		contentPane.repaint();
	}

	/**
	 * Draws the text at the given position with given color.
	 * 
	 * @param text  the text to draw
	 * @param x     the x-coordinate for the text
	 * @param y     the y-coordinate for the text
	 * @param color the color
	 */
	public static void drawText(String text, int x, int y, Color color) {
		Graphics g = image.getGraphics();
		g.setColor(color);
		g.drawString(text, x, y);
		contentPane.repaint();
	}

	/**
	 * Fills a rectangle at position x/y with width w and height h.
	 * 
	 * @param x     the x-coordinate for the rectangle
	 * @param y     the y-coordinate for the rectangle
	 * @param w     the width of the rectangle
	 * @param h     the height of the rectangle
	 * @param color the color
	 */
	public static void fillRectangle(int x, int y, int w, int h, Color color) {
		Graphics g = image.getGraphics();
		g.setColor(color);
		g.fillRect(x, y, w, h);
		contentPane.repaint();
	}

	/**
	 * Fills a circle at position x/y with radius r.
	 * 
	 * @param x     the x-coordinate for the circle
	 * @param y     the y-coordinate for the circle
	 * @param r     the radius of the circle
	 * @param color the color
	 */
	public static void fillCircle(int x, int y, int r, Color color) {
		Graphics g = image.getGraphics();
		g.setColor(color);
		g.fillOval(x - r, y - r, 2 * r, 2 * r);
		contentPane.repaint();
	}

	private static java.awt.Point p = null;

	/**
	 * Waits for mouse clicks in the window and returns the position of the event.
	 * Blocks until the mouse click is done by the user.
	 * 
	 * @return the position the mouse click
	 */
	public static java.awt.Point getMouseClick() {

		contentPane.addMouseListener(new MouseAdapter() {
			/**
			 * Invoked when the mouse has been clicked on a component.
			 */
			public void mouseClicked(MouseEvent e) {
				p = e.getPoint();
				synchronized (contentPane) {
					contentPane.notifyAll();
				}
			}
		});

		// block until mouse click has been done
		synchronized (contentPane) {
			try {
				contentPane.wait();
			} catch (InterruptedException e1) {
				e1.printStackTrace();
			}
		}
		return p;
	}

// private section --------------------------------------------------------------------------------

	/** Main frame */
	private static Frame windowO;

	/**
	 * Panel with content; is of type WindowOPanel, which draws into a BufferedImage
	 * image
	 */
	private static WindowOPanel contentPane;

	/**
	 * BufferedImage is the image where all the draw operations are done and which
	 * is displayed by WindowOPanel
	 */
	private static Image image;

	/** Variable defining the height of the header of the frame */
	private static int headerHeight = 24;

	/** Inner class for closing the frame */
	static class WindowClosingAdapter extends WindowAdapter {

		private boolean exitSystem;

		public WindowClosingAdapter(boolean exitSystem) {
			this.exitSystem = exitSystem;
		}

		public WindowClosingAdapter() {
			this(true);
		}

		public void windowClosing(WindowEvent event) {
			event.getWindow().setVisible(false);
			event.getWindow().dispose();
			if (exitSystem) {
				System.exit(0);
			}
		}
	}

	/**
	 * Inner class for doing the painting. Method paint is overridden and draws the
	 * content of the BufferedImage.
	 */
	@SuppressWarnings("serial")
	static class WindowOPanel extends Component {

		@Override
		public void paint(Graphics g) {
			g.drawImage(image, 0, 0, null);
		}
	}

}
