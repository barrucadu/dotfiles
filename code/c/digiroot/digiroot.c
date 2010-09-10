#include <math.h>
#include <stdlib.h>
#include <stdio.h>

int listval (int list[9]) {
  int val = list[8] + 10 * list[7] + 100 * list[6] + 1000 * list[5] + 1000 * list[4] + 10000 * list[3] + 100000 * list[2] + 1000000 * list[1] + 10000000 * list[0];
  return val;
}

int listsum (int list[9]) {
  int sum = list[0] + list[1] + list[2] + list[3] + list[4] + list[5] + list[6] + list[7] + list[8];
  return sum;
}

int main (int argc, char* argv[]) {
  int found      = 0;
  int root       = atoi (argv[1]); // First arg = root
  int digits     = atoi (argv[2]); // Second arg = number of digits
  int current[9] = {0, 0, 0, 0, 0, 0, 0, 0, 0};
  int end[9]     = {0, 0, 0, 0, 0, 0, 0, 0, 0};

  for (int i = 0; i <= digits; i ++) {
    end[9 - i] = 9;
  }

  current[9 - digits] = 1;
  
  while (listval (current) < listval (end)) {
    if (listsum (current) == root) {
      found ++;
      printf ("%i\n", listval (current));
    }

    current[8] ++;
    if (current[8] == 10) {
      current[8] = 0;
      current[7] ++;
      if (current[7] == 10) {
        current[7] = 0;
        current[6] ++;
        if (current[6] == 10) {
          current[6] = 0;
          current[5] ++;
          if (current[5] == 10) {
            current[5] = 0;
            current[4] ++;
            if (current[4] == 10) {
              current[4] = 0;
              current[3] ++;
              if (current[3] == 10) {
                current[3] = 0;
                current[2] ++;
                if (current[2] == 10) {
                  current[2] = 0;
                  current[1] ++;
                  if (current[1] == 10) {
                    current[1] = 0;
                    current[0] ++;
                  }}}}}}}}}

  printf ("\nRoot: %i\nDigits: %i\nFound: %i\n", root, digits, found);
  return 0;
}
