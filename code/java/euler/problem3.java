public class problem3
{
  private static long _target = 600851475143L;

  public static void main (String args[])
  {
    long i       = 0;
    long working = _target;

    for (i = 2; i < working; i ++)
      {
        if (isPrime (i) && working % i == 0)
          {
            working = working / i;
          }
      }

    System.out.println (working);
  }

  public static boolean isPrime (long num)
  {
    long i = 0;

    if (i == 2)
      return true;

    for (i = 2; i < Math.sqrt (num); i ++)
      {
        if (num % i == 0)
          return false;
      }

    return true;
  }
}