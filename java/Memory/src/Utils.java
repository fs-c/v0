import java.util.Random;

public class Utils
{
    /**
     * Efficiently shuffle an array of integers.
     *
     * @param array The array to shuffle.
     * @return The shuffled array.
     */
    public static int[] shuffle(int[] array)
    {
        int index, temp;
        Random random = new Random();

        for (int i = array.length - 1; i > 0; i--)
        {
            index = random.nextInt(i + 1);
            temp = array[index];
            array[index] = array[i];
            array[i] = temp;
        }

        return array;
    }

    private Utils()
    {
    }
}
