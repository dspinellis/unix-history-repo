/*
 *	"@(#)bit.c	1.1"
 *
 * bit set, clear, test routines
 *
 * calling sequences:
 *	logical l, bit, state
 *	call bis (bitnum, word)
 *	call bic (bitnum, word)
 *	call setbit (bitnum, word, state)
 *	l = bit (bitnum, word)
 * where:
 *	bis(bic) sets(clears) bitnum in word
 *	setbit sets bitnum in word to 1 if state is .true.
 *	bit tests bitnum in word and returns a logical (t/f) value
 */

long bis_(n, w)
long *n, *w;
{
	if (*n >= 0 && *n <= 31)
		*w |= (1L << (*n));
}

long bic_(n, w)
long *n, *w;
{
	if (*n >= 0 && *n <= 31)
		*w &= ~(1L << (*n));
}

long bit_(n, w)
long *n, *w;
{
	if (*n < 0 || *n > 31)
		return(0);
	return((*w & (1L << (*n))) != 0);
}

setbit_(n, w, s)
long *n, *w, *s;
{
	if (*s)
		bis_(n, w);
	else
		bic_(n, w);
}
