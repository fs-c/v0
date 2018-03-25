import javax.swing.*;

public class ImageProvider
{
    private static String[] images = {
            "pics/birne.png",
            "pics/burger.png",
            "pics/cupcake.png",
            "pics/erdbeere.png",
            "pics/kaffee.png",
            "pics/marienkaefer.png",
            "pics/pfirsich.png",
            "pics/zitrone.png"
    };

    private static ImageIcon backside;
    private static ImageIcon[] motives = new ImageIcon[images.length];

    /**
     * Loads all images into their respective variables (and therefore in memory).
     */
    public static void load()
    {
        backside = new ImageIcon("./pics/memory.png");

        for (int i = 0; i < images.length; i++) {
            motives[i] = new ImageIcon(images[i]);
        }
    }

    /**
     * @return The backside motive.
     */
    public static ImageIcon getBackside()
    {
        return backside;
    }

    /**
     * @param mNum The index of the motive to return.
     * @return The chosen image.
     */
    public static ImageIcon getMotive(int mNum)
    {
        return motives[mNum];
    }

    /**
     * Private constructor; this is an utility class.
     */
    private ImageProvider()
    {
    }
}
