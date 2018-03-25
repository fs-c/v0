import javax.swing.*;
import java.awt.*;

public class Memory extends JFrame
{
    /**
     * Construct the application.
     */
    public Memory()
    {
        setTitle("Memory");

        add(new GUIPanel());

        // setLayout(new FlowLayout(FlowLayout.LEFT));

        setSize((4 * 120) + 45, (4 * 120) + 60);

        // pack();
        setVisible(true);
    }

    public static void main(String[] args)
    {
        new Memory();
    }
}
