/*
char id_sleep[] = "@(#)sleep_.c	1.1";
 *
 * sleep for awhile
 *
 * calling sequence:
 *	call sleep(seconds)
 * where:
 *	seconds is an integer number of seconds to sleep (see sleep(3))
 */

sleep_(sec)
long *sec;
{
	sleep((unsigned int)*sec);
}
