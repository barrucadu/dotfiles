#include <kernel.h>
#include <string.h>
#include <mm.h>

size_t strlen(const u8int* str)
{
    /* A function to return the number of bytes in a string. */
    const u8int *p = str;

    while (*p != NULL)
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

    str[i] = NULL;
    
    return str;
}

u8int* substr(u8int* string, u32int start, u32int length)
{
    u8int* out = kmalloc(length + 1);
    
    u32int i;
    for(i = 0; i < length; i ++)
    {
	out[i] = string[start + i];
    }

    out[i] = '\0';

    return out;
}

u32int strfindnext(u8int* string, u8int search, u32int offset)
{
    u32int out = offset + 1;

    while(string[out] != search)
    {
	out ++;

	if(out > (u32int) strlen(string))
	{
	    return 0;
	}
    }
    
    return out;
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
    output[j] = NULL;

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

    num[i] = NULL;

    return strrev(num);
}

u32int strcmp(u8int* s1, u8int* s2) /* Return 1 if strings are identical, 0 otherwise */
{
    if(strlen(s1) != strlen(s2))
    {
	return FALSE;
    }

    u32int i;
    
    for(i = 0; i < strlen(s1); i ++)
    {
	if(s1[i] != s2[i])
	{
	    return FALSE;
	}
    }

    return TRUE;
}
