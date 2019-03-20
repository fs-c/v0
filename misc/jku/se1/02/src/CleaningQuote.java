public class CleaningQuote {
	public static void main(String[] args) {
		Out.println("----------------------------------------------");
		Out.println("Firma Rein u. Fein - Eingabe");
		Out.println("----------------------------------------------");
		Out.println("Kundendaten");
		Out.println("----------------------------------------------");

		Out.print("Name: ");
		String name = In.readWord() + " " + In.readWord();
		Out.print("E-Mail: ");
		String email = In.readWord();

		Out.println("----------------------------------------------");

		Out.print("m2 Boden: ");
		int floor = In.readInt();
		Out.print("Anzahl Räume bis 15 m2: ");
		int numSmall = In.readInt();
		Out.print("Anzahl Räume bis 25 m2: ");
		int numMed = In.readInt();
		Out.print("Anzahl Räume bis 40 m2: ");
		int numLarge = In.readInt();

		Out.println("----------------------------------------------");
		Out.println("Firma Rein u. Fein - Angebot");
		Out.println("----------------------------------------------");
		Out.format("Name: %s%n", name);
		Out.format("E-Mail: %s%n", email);
		Out.println("----------------------------------------------");

		int priceFloor = floor * 420;
		int priceSmall = numSmall * 1740;
		int priceMed = numMed * 1140;
		int priceLarge = numLarge * 740;

		Out.format("Boden%18d%18d,%02d €%n", floor, priceFloor / 100,
			priceFloor % 100);
		Out.format("Räume bis 15m2%9d%18d,%02d €%n", numSmall,
			priceSmall / 100, priceSmall % 100);
		Out.format("Räume bis 25m2%9d%18d,%02d €%n", numMed,
			priceMed / 100, priceMed % 100);
		Out.format("Räume bis 40m2%9d%18d,%02d €%n", numLarge,
			priceLarge / 100, priceLarge % 100);

		Out.println("----------------------------------------------");

		int net = priceFloor + priceSmall + priceMed + priceLarge;
		int taxes = net / 5;
		int gross = net + taxes;

		Out.format("Netto%36d,%02d €%n", net / 100, net % 100);
		Out.format(" +20%% Umsatzsteuer%23d,%02d €%n", taxes / 100,
			taxes % 100);
		Out.format("Brutto%35d,%02d €%n", gross / 100, gross % 100);

		Out.println("==============================================");
		Out.println("Hochachtungsvoll");
		Out.println("Rein & Fein");
	}
}