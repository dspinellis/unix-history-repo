/*
 *	"@(#)short_.c	1.1"
 */

/*
 * convert long ints to short.
 *
 * used as follows:
 *	integer*2 short
 *	...
 *	call mysub(short(ivar))
 * where:
 *	mysub expects to receive an integer*2 arg and ivar is integer*4
 */

short short_(i)
long *i;
{	return((short)*i);	}
