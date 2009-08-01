#include <math.h>
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char* argv[])
{
  (void) argc;
  (void) argv;

  int triangle = 0;
  int current  = 1;
  int divisors = 0;
  int iterator = 0;

  while(divisors <= 500)
    {
      divisors = 0;
      triangle = triangle + current;
      current  ++;

      for(iterator = 1; iterator <= sqrt(triangle); iterator ++)
        {
          if (triangle % iterator == 0)
            {
              if (iterator * iterator == triangle)
                {
                  divisors = divisors + 1;
                }
              else
                {
                  divisors = divisors + 2;
                }
            }
        }
    }

  printf("Triangle number with %d divisors is %d\n.", divisors, triangle);
  return 0;
}
