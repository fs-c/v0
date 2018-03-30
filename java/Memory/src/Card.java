import javax.swing.*;
import java.awt.*;

class Card extends JButton
{
    public int motive;
    public boolean locked = false;
    public boolean flipped = false;

    /**
     * Construct a card with the given motive.
     *
     * @param motiveNum The index of the motive to load.
     */
    Card(int motiveNum)
    {
        motive = motiveNum;

        setBorder(null);
        setIcon(ImageProvider.getBackside());
        setMinimumSize(new Dimension(120, 120));
    }

    public void flip()
    {
        if (flipped) {
            setIcon(ImageProvider.getBackside());
        } else setIcon(ImageProvider.getMotive(motive));

        flipped = !flipped;
    }

    public void cover()
    {
        flipped = false;

        setIcon(ImageProvider.getBackside());
    }

    public void uncover()
    {
        flipped = true;

        setIcon(ImageProvider.getMotive(motive));
    }

    public void lock()
    {
        locked = true;
        setEnabled(false);
        // setIcon(new ImageIcon(GrayFilter.createDisabledImage(ImageProvider.getMotive(motive).getImage())));
    }
}
