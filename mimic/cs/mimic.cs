using System;
using System.IO;

public class Mimic
{
    static public void Main(string[] args)
    {
        if (args.Length == 0)
        {
            Console.WriteLine("usage: <executable> <path to .osr file>");

	    	System.Environment.Exit(1);
        }

        try
        {
            using (BinaryReader stream = new BinaryReader(File.Open(args[0],
	    		FileMode.Open)))
            {
                var mode = stream.ReadByte();
				Console.WriteLine("mode:        {0:D}", mode);
                var version = stream.ReadInt32();
				Console.WriteLine("version:     {0:D}", version);

                var beatmap = stream.ReadString();
                Console.WriteLine(beatmap);

                var player = stream.ReadString();
                Console.WriteLine(player);

                var local = stream.ReadString();
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
