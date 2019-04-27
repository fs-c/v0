public class CaesarCipher {
	static final int ALPHABET_SIZE = 26;

	public static void main(String[] args) {
		char ch;
		do {
			Out.print("'e' for encrypt, 'd' for decrypt, 'q' to quit: ");
			ch = In.readChar();

			if (ch == 'e' || ch == 'd') {
				Out.print("Filename: ");
				String filename = In.readWord();
				In.readLine();

				Out.print("Key: ");
				int key = In.readInt() % ALPHABET_SIZE;
				In.readLine();

				Out.print("Text: ");
				String text = getText(ch == 'd' ? filename : null);

				if (ch == 'e') {
					Out.open(filename);
				}

				String cipher = transformText(text, ch == 'e' ? key : -key);

				Out.print(cipher);

				if (ch == 'e') {
					Out.close();
				} else {
					Out.println();
				}
			}
		} while (In.done() && ch != 'q');
	}

	public static String transformText(String text, int key) {
		StringBuilder cipher = new StringBuilder(text);

		for (int i = 0; i < cipher.length(); i++) {
			char curChar = cipher.charAt(i);

			if (Character.isLetter(curChar)) {
				cipher.setCharAt(i, transformChar(curChar, key));
			}
		}

		return cipher.toString();
	}

	public static char transformChar(char ch, int key) {
		int lbound, ubound;

		if (Character.isLowerCase(ch)) {
			lbound = 'a';
			ubound = 'z';
		} else {
			lbound = 'A';
			ubound = 'Z';
		}

		char newChar = (char) (ch + key);
		if (newChar > ubound)
			newChar -= ALPHABET_SIZE;
		if (newChar < lbound)
			newChar += ALPHABET_SIZE;

		return newChar;
	}

	public static String getText(String filename) {
		String text;

		if (filename != null) {
			In.open(filename);
			text = In.readFile();
			In.close();
		} else text = In.readLine();

		return text;
	}
}
