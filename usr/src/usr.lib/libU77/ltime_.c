/*
char id_ltime[] = "@(#)ltime_.c	1.1";
 *
 * return broken down time
 *
 * calling sequence:
 *	integer time, t[9]
 *	call ltime(time, t)
 * where:
 *	time is a  system time. (see time(3F))
 *	t will receive the broken down time corrected for local timezone.
 *	(see ctime(3))
 */

int *localtime();

ltime_(clock, t)
long *clock; long *t;
{
	int i;
	int *l;

	l = localtime(clock);
	for (i=0; i<9; i++)
		*t++ = *l++;
}
