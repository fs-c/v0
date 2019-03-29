class PositionTracker {
	public static void main(String[] args) {
		In.open(args[0]);

		int trackIndex = 0;

		while (!In.isEof()) {
			int length = In.readInt(), time = (length - 1) * 5, dist = 0;

			int prevX = In.readInt(), prevY = In.readInt();

			for (int i = 0; i < length - 1; i++) {
				int curX = In.readInt(), curY = In.readInt();

				int deltaX = (curX - prevX) * 100,
					deltaY = (curY - prevY) * 100;

				dist += (int)Math.sqrt(
					Math.pow(deltaX, 2) + Math.pow(deltaY, 2)
				);

				prevX = curX;
				prevY = curY;
			}

			Out.format("Track %d: %8d m %8d sec %8d cm/sec%n", trackIndex + 1,
				dist / 100, time, dist / time);

			trackIndex++;
		}

		In.close();
	}
}
