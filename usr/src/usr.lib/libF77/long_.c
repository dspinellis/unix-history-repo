/*
 *	"@(#)long_.c	1.1"
 */

/*
 * convert short ints to long.
 * Needed for literals in -I2 compiles.
 * used as follows:
 *	integer*4 long
 *	...
 *	call ftell(long(11))
 */

long long_(i)
short *i;
{	return((long)*i);	}
