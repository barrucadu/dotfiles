#include <kernel.h>
#include <string.h>

size_t strlen(const u8int* str)
{
    /* A function to return the number of bytes in a string. */
    const u8int *p = str;

    while (*p != '\0')
    {
	p++;
    }

    return (size_t) (p - str);
}

u8int* strrev(u8int* string)
{
    u8int* str;
    u32int i, len;

    len = strlen((const u8int*) string);

    for(i = 0; i < len; i ++)
    {
	str[i] = string[len - i - 1];
    }
    str[i] = '\0';
    
    return str;
}

u8int* itos(u32int number, u32int base)
{
    u8int num[64];
    u32int i = 0;
    u32int remainder;
    u8int add;

    do
    {
	add = '0';
	remainder = number % base;
	if(remainder >= 10)
	{
	    add = 'A';
	    remainder -= 10;
	}
	num[i] = remainder + add;
	i ++;
    } while(number /= base);
    
    if(base == 16)
    {
	num[i] = 'x';
	num[i + 1] = '0';
	i += 2;
    }

    num[i] = '\0';

    return strrev(num);
}
