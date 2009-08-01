#include <math.h>
#include <stdlib.h>
#include <stdio.h>

// 1 = next prime
// 0 = previous prime
int findprime(int start, int direction)
{
  int prime     = start;
  int searching = 1;
  int iterator  = 2;
  int isprime   = 0;

  if(direction == 1)
    {
      prime ++;
    }
  else
    {
      prime --;
    }

  while(searching == 1)
    {
      isprime = 1;
      for(iterator = 2; iterator <= sqrt(prime); iterator ++)
        {
          if(prime % iterator == 0)
            {
              isprime = 0;
            }
        }
      if(isprime == 0)
        {
          if(direction == 1)
            {
              prime ++;
            }
          else
            {
              prime --;
            }
        }
      else
        {
          searching = 0;
        }
    }

  return prime;
}

int main(int argc, char* argv[])
{
  (void) argc;
  (void) argv;

  int ways      = 0;
  int current   = 4;
  int prime     = 0;
  int direction = 0;

  for(; current > 0; current --)
    {
      if(current > 2)
        {
          prime = findprime(prime, 1);
          ways  = ways + prime - 1;
        }
      else if(current == 2)
        {
          ways = ways + prime - 1;
        }
      else
        {
          prime = findprime(prime, 0) - 1;
          ways  = ways + prime;
        }
      printf("%d: %d ways.\n", current, prime - 1);
    }

  printf("100 can be written in %d ways as a sum of positive integers.\n", ways);
  return 0;
}
