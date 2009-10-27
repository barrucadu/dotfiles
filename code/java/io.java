import java.io.*;

public class io
{
	public static void main(String args[])
    {
		System.out.println("This is a demonstration of output.");
		System.out.print("Now, please enter your name: ");

		String name = null;
		name = readln();

		System.out.println("Hello, " + name);
	}

	private static String readln()
	{
		BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
		String input = null;

		try
			{
				input = br.readLine();
			}
		catch(IOException ioe)
			{
				System.out.println("IO error.");
				System.exit(1);
			}

		return input;
	}
}