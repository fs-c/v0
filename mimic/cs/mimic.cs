using System;
using System.IO;

public class Mimic
{
    static public void Main(string[] args)
    {
        if (args.Length == 0)
        {
            Console.WriteLine("usage: <executable> <path to .osr file>");
        }

        try
        {
            using (BinaryReader stream = new BinaryReader(File.Open(args[0], FileMode.Open)))
            {
                byte mode = stream.ReadByte();
                stream.ReadByte(); // Why?
                int version = stream.ReadInt32();

                string beatmap = stream.ReadString();
                Console.WriteLine(beatmap);

                string player = stream.ReadString();
                Console.WriteLine(player);

                string local = stream.ReadString();
                Console.WriteLine(local);
            }
        }
        catch (Exception e)
        {
            Console.WriteLine("error reading file: ");
            Console.WriteLine(e.Message);
        }
    }
}