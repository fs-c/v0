class TimeFormatter {
	public static void main(String[] args) {
		int hour = In.readInt();

		if (!In.done() || hour < 0 || hour > 23) {
			Out.println("error: hh has to be an integer and >-1 and <24");

			return;
		}

		char ch = In.readChar();
		if (!In.done() || ch != ':') {
			Out.println("error: missing colon");
		}

		int minute = In.readInt();

		if (!In.done() || minute < 0 || minute > 59) {
			Out.println("error: mm has to be an integer and >-1 and <60");

			return;
		}

		ch = In.peek();

		int second = 0;
		if (ch == ':') {
			ch = In.readChar();
			second = In.readInt();
		}

		if (!In.done() || second < 0 || second > 59) {
			Out.println("error: ss has to be an integer and >-1 and <60");

			return;
		}

		ch = In.peek();

		String period = "";
		if (ch == ',') {
			ch = In.readChar();

			period += In.readChar();
			period += In.readChar();

			if (!period.matches("am|pm")) {
				Out.println("error: period has to be one of 'am' or 'pm'");

				return;
			}

			if (hour < 1 || hour > 12) {
				Out.println("error: period was given but hh is not >0 and <13");

				return;
			}

			if (period.equals("pm") && hour != 12) {
				hour += 12;
			}

			if (period.equals("am") && hour == 12) {
				hour = 0;
			}
		}

		ch = In.readChar();

		if (ch == '.') {
			Out.format("%02d:%02d:%02d%n", hour, minute, second);
		}
	}
}
