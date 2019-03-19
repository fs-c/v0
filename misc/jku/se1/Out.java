
import java.io.*;

/**
 * Simple output to the console and to files.
 * <p>
 * This class allows printing formatted data either to the console or to a file.
 * It is intended to be used in an introductory programming course when classes,
 * packages and exceptions are unknown at the beginning. To use it, simply copy
 * Out.class into the current directory.
 * </p>
 * <p>
 * All output goes to the current output file, which is initially the console.
 * Opening a file with open() makes it the new current output file. Closing a
 * file with close() switches back to the previous output file.
 * </p>
 */

public class Out {

	private static PrintStream out;
	private static PrintStream[] stack;
	private static int sp;
	private static boolean done;

	/**
	 * Returns true if the previous Out operation was successful, otherwise return
	 * false.
	 * 
	 * @return the Boolean value signaling if the last input operation was successful  
	 */
	public static boolean done() {
		return done && !out.checkError();
	}

	/** Prints the boolean value b either as "true" or "false". 
	 * 
	 * @param b the Boolean value to print
	 */
	public static void print(boolean b) {
		out.print(b);
	}

	/** Prints the character value c. 
	 * 
	 * @param s the character to print
	 */
	public static void print(char s) {
		out.print(s);
	}

	/** Prints the integer value i. 
	 * 
	 * @param i the integer value to print
	 */
	public static void print(int i) {
		out.print(i);
	}

	/** Prints the long value l. 
	 * 
	 * @param l the long value to print
	 */
	public static void print(long l) {
		out.print(l);
	}

	/** Prints the float value f. 
	 * 
	 * @param f the float value to print
	 */
	public static void print(float f) {
		out.print(f);
	}

	/** Prints the double value d. 
	 * 
	 * @param d the double value to print
	 */
	public static void print(double d) {
		out.print(d);
	}

	/** Prints the character array a. 
	 * 
	 * @param a the character array to print
	 */
	public static void print(char[] a) {
		out.print(a);
	}

	/** Prints the String s. 
	 * 
	 * @param s the string value to print
	 */
	public static void print(String s) {
		out.print(s);
	}

	/** Prints the Object o as resulting from String.valueOf(o). 
	 * 
	 * @param o the object value to print
	 */
	public static void print(Object o) {
		out.print(o);
	}

	/**
	 * Terminates the current line by writing a line separator string. On windows
	 * this is the character sequence '\r' and '\n'
	 */
	public static void println() {
		out.println();
	}

	/** Prints the boolean value b and terminate the line. 
	 * 
	 * @param b the boolean value to print
	 */
	public static void println(boolean b) {
		out.println(b);
	}

	/** Prints the character value c and terminate the line. 
	 * 
	 * @param c the character value to print
	 */
	public static void println(char c) {
		out.println(c);
	}

	/** Prints the integer value i and terminate the line. 
	 * 
	 * @param i the integer value to print
	 */
	public static void println(int i) {
		out.println(i);
	}

	/** Prints the long value l and terminate the line. 
	 * 
	 * @param l	 the long value to print
	 */
	public static void println(long l) {
		out.println(l);
	}

	/** Prints the float value f and terminate the line. 
	 * 
	 * @param f the float value to print
	 */
	public static void println(float f) {
		out.println(f);
	}

	/** Prints the double value d and terminate the line. 
	 * 
	 * @param d the double value to print
	 */
	public static void println(double d) {
		out.println(d);
	}

	/** Prints the character array a and terminate the line. 
	 * 
	 * @param a the character array to print
	 */
	public static void println(char[] a) {
		out.println(a);
	}

	/** Prints the String s and terminate the line. 
	 * 
	 * @param s the string value to print
	 */
	public static void println(String s) {
		out.println(s);
	}

	/**
	 * Prints the Object o as resulting from String.valueOf(o) and terminate the
	 * line.
	 * 
	 * @param o  the object value to print
	 */
	public static void println(Object o) {
		out.println(o);
	}

	/**
	 * Writes a formatted string to this output stream using the specified
     * format string and arguments. Uses {@link java.io.PrintStream#format(String, Object...)}
	 * 
	 * @param format 
     *         A format string as described in {@link java.util.Formatter}
     *
     * @param  args
     *         Arguments referenced by the format specifiers in the format
     *         string.  
     *         
     * @see java.io.PrintStream#format(String, Object...)
	 */	 
	public static void format(String format, Object... args) {
		out.format(format, args);
	}

	/**
	 * Writes a formatted string to this output stream using the specified
     * format string and arguments and starts a new line. 
     * Uses {@link java.io.PrintStream#format(String, Object...)}
	 * 
	 * @param format 
     *         A format string as described in {@link java.util.Formatter}
     *
     * @param  args
     *         Arguments referenced by the format specifiers in the format
     *         string.  
     *         
     * @see java.io.PrintStream#format(String, Object...)
	 */	 
	public static void formatln(String format, Object... args) {
		format(format + "%n", args);
	}

	/**
	 * Opens the file with the name fn as the current output file. All subsequent
	 * output goes to this file until it is closed. The old output file will be
	 * restored when the new output file is closed.
	 * 
	 * @param fn the file name to open
	 */
	public static void open(String fn) {
		try {
			PrintStream s = new PrintStream(new FileOutputStream(fn));
			stack[sp++] = out;
			out = s;
		} catch (Exception e) {
			done = false;
		}
	}

	/**
	 * Closes the current output file. The previous output file is restored and
	 * becomes the current output file.
	 */
	public static void close() {
		out.flush();
		out.close();
		if (sp > 0)
			out = stack[--sp];
	}

	static { // initializer
		done = true;
		out = System.out;
		stack = new PrintStream[8];
		sp = 0;
	}

}