public class MyFirstProgram {
	public static void main(String[] args) {
		int number;
		Out.print("Please enter a number: ");
		number = In.readInt();

		Out.print("You have entered: ");
		Out.println(number);
	}
}