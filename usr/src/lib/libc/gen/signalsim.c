/*	signalsim.c	4.1	83/06/04	*/

/*
 * Backwards compatible signal.
 */

signal(s, a)
{

	return (sigvec(s, a, 0));
}
