#include <kernel.h>
#include <string.h>
#include <mm.h>

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
    u8int* str = kmalloc(strlen(string) * sizeof(u8int*));
    u32int i, len;

    len = strlen((const u8int*) string);

    for(i = 0; i < len; i ++)
    {
	str[i] = string[len - i - 1];
    }
    str[i] = '\0';
    
    return str;
}

u8int* ksprintf(u8int* format, ...)
{
    /* Very simple sprintf: supports %s, returns the final string */
    va_list args;
    va_start(args, format);

    u8int *strings[256];
    u32int size = strlen(format);
    u32int i, j, k, l;
    u8int *s;

    l = 0;
    while((s = va_arg(args, u8int*)))
    {
	strings[l] = s;
	size += strlen(s);
	l ++;
    }
    
    u8int *output = kmalloc(size);

    j = 0;
    l = 0;
    for(i = 0; i < (u32int) strlen(format); i ++)
    {
	if(format[i] == '%') /* Start of control character */
	{
	    if(format[i + 1] == 's') /* Embed the next string */
	    {
		for(k = 0; k < (u32int) strlen(strings[l]); k ++)
		{
		    output[j] = strings[l][k];
		    j ++;
		}
		j --;
		l ++;
	    }
	    i ++;
	} else {
	    output[j] = format[i];
	}
	j ++;
    }
    output[j] = '\0';

    return output;
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
