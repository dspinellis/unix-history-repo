/*
char id_time[] = "@(#)time_.c	1.1";
 *
 * return the current time as an integer
 *
 * calling sequence:
 *	integer time
 *	i = time()
 * where:
 *	i will receive the current GMT in seconds.
 */

long time();

long time_()
{
	return(time(0));
}
