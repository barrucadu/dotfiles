public class problem2
{
  public static void main (String args[])
  {
    int a   = 0;
    int b   = 1;
    int c   = 0;
    int sum = 0;

    while (b < 4000000)
      {
        c = a + b;
        a = b;
        b = c;

        if (a % 2 == 0)
          sum += a;
      }

    System.out.println (sum);
  }
}