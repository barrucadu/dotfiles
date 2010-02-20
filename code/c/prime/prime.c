#include <stdio.h>
#include <stdlib.h>

#define MAXPRIME 200000000

int next(char *sieve, int current)
{
    int p = current;
    while(sieve[p] == 1) ++p;
    return p;
}

void mark(char** sieve, int prime)
{
    int p = prime;
    int i;
    char* s = sieve[0];

    for(i = 1; i * p < MAXPRIME; ++i) s[i * p] = 1;
}

int main()
{
    int n = 0;
    int p = 0;

    char *sieve = (char*)malloc(sizeof(char) * MAXPRIME);
    sieve[0] = 1;
    sieve[1] = 1;

    while(n <= 10000000)
    {
	p = next(sieve, p);
	mark(&sieve, p);
	++n;
	
	if(n == 1000)     printf("The 1000th     prime number is %i\n", p);
	if(n == 10000)    printf("The 10000th    prime number is %i\n", p);
	if(n == 100000)   printf("The 100000th   prime number is %i\n", p);
	if(n == 1000000)  printf("The 1000000th  prime number is %i\n", p);
	if(n == 10000000) printf("The 10000000th prime number is %i\n", p);
    }

    return 0;
}
