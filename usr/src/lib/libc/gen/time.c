/*	time.c	4.1	83/02/24	*/

/*
 * Backwards compatible time call.
 */
#include <sys/types.h>
#include <sys/time.h>

time_t
time(t)
	time_t *t;
{
	struct timeval tt;

	if (getimeofday(&tt, (struct timezone *)0) < 0)
		return (-1);
	if (t)
		*t = tt.tv_sec;
	return (tt.tv_sec);
}
