/*	kon.c	4.1	83/03/09	*/
/*
 * Turn on keypad, so it sends codes instead of doing them in local.
 */

#include "2648.h"

kon()
{
	escseq(NONE);
	outstr("\33&s1A");
}

koff()
{
	escseq(NONE);
	outstr("\33&s0A");
}
