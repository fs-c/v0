import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Random;

public class GUIPanel extends JPanel implements ActionListener
{
    private int[] motives = {0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7};
    private Card[] cards = new Card[motives.length];

    private Card active = null;

    /**
     * Constructs the GUI, handles all input.
     */
    GUIPanel()
    {
        // motives = Utils.shuffle(motives);

        ImageProvider.load();

        for (int i = 0; i < motives.length; i++) {
            cards[i] = new Card(motives[i]);

            cards[i].addActionListener(this);

            add(cards[i]);
        }

        setVisible(true);
    }

    @Override
    public void actionPerformed(ActionEvent e)
    {
        Card card = (Card) e.getSource();

        if (card.locked) { // Part of an already uncovered pair.
            return;
        }

        card.uncover();

        if (active != null) { // There's already a flipped card.
            if (active == card) { // Clicked the same card twice.
                active = null;
                card.cover();
                return;
            }

            if (active.motive == card.motive) { // Uncovered a pair.
                card.lock();
                active.lock();
            } else {
                card.cover();
                active.cover();
            }

            active = null;
        } else active = card;
    }
}
