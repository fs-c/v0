class CaesarCipher {
    public static final int UPCASE_BEGIN = 65;
    public static final int LOCASE_BEGIN = 97;

    public static void main(String[] args) {
        while (true) {
            Out.print("'e' for encrypt, 'd' for decrypt, 'q' to quit: ");
            char mode = In.read();

            if (mode == 'q' || (mode != 'd' && mode != 'e'))
                return;

            Out.print("Filename: ");
            String output = In.readWord();

            Out.print("Key: ");
            int shift = (In.readInt() % 26) * (mode == 'd' ? -1 : 1);

            Out.print("Text: ");
            String text = "";

            In.read();

            while ((int)In.peekRaw() != 10)
                text += In.read();

            StringBuilder cipher = new StringBuilder(text);

            for (int i = 0; i < cipher.length(); i++) {
                char old = cipher.charAt(i);

                if (!Character.isLetter(old)) {
                    cipher.setCharAt(i, old);
                    continue;
                }

                int offset = Character.isUpperCase(old) ? UPCASE_BEGIN :
                    LOCASE_BEGIN;

                cipher.setCharAt(i, (char)(
                    (((int)old - offset + shift) % 26) + offset
                ));
            }

            Out.open(output);
            Out.print(cipher);
            Out.close();
        }
    }
}
