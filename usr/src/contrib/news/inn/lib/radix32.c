/*  $Revision: 1.3 $
**
**  Radix-32 strings divide a number into five-bit nibbles and use the
**  alphabet 0..9a..v to represent 0..32.
*/
#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include "configdata.h"
#include "clibrary.h"


static char	ALPHABET[] =
    "0123456789abcdefghijklmnopqrstuv";


/*
**  Turn a number into a Radix-32 string.  Assume the number fits into
**  32 bits.
*/
void
Radix32(l, buff)
    register unsigned long	l;
    register char		*buff;
{
    register char		*p;
    register int		i;
    char			temp[10];

    /* Simple sanity checks. */
    if ((l &= 0xFFFFFFFFL) == 0) {
	*buff++ = ALPHABET[0];
	*buff = '\0';
	return;
    }

    /* Format the string, in reverse. */
    for (p = temp; l; l >>= 5)
	*p++ = ALPHABET[(int)(l & 037)];

    /* Reverse it. */
    for (i = p - temp; --i >= 0; )
	*buff++ = *--p;
    *buff = '\0';
}


#if	0
/*
**  Return a Radix-32 string as a number, or ~0 on error.
*/
unsigned long
Decode32(p)
    register char		*p;
{
    register unsigned long	l;
    register char		*cp;

    for (l = 0; *p; p++) {
	if ((cp = strchr(ALPHABET, *p)) == NULL)
	    return ~0;
	l = (l << 6) + cp - ALPHABET;
    }
    return l;
}
#endif	/* 0 */
