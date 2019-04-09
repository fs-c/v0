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

				String text = null;
				if (ch == 'e') {
					Out.print("Text: ");
					text = In.readLine();
					Out.open(filename);
				} else {
					In.open(filename);
					text = In.readFile();
					In.close();
					key = -key;
					Out.print("Text: ");
				}

				StringBuilder cipher = new StringBuilder(text);
				for (int i = 0; i < cipher.length(); i++) {
					int lbound, ubound;
					char oldChar = cipher.charAt(i);

					if (Character.isLetter(oldChar)) {
						if (Character.isLowerCase(oldChar)) {
							lbound = 'a';
							ubound = 'z';
						} else {
							lbound = 'A';
							ubound = 'Z';
						}
						char newChar = (char) (oldChar + key);
						if (newChar > ubound)
							newChar -= ALPHABET_SIZE;
						if (newChar < lbound)
							newChar += ALPHABET_SIZE;
						cipher.setCharAt(i, newChar);
					}
				}
				Out.print(cipher);

				if (ch == 'e') {
					Out.close();
				} else {
					Out.println();
				}
			}
		} while (In.done() && ch != 'q');
	}
}
