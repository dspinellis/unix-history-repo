/*
 * tzset.c
 *
 * Quick and dirty emulation of tzset(), tzname[], and daylight
 * for old BSD systems without it.
 *
 * Thanks to Rick Adams, rick@uunet.uu.net, for the basics.
 *
 * BUGS:
 *	Totally ignores the value of the TZ environment variable.
 */

#if 0
#include <sys/time.h>
#endif

static char tz1[1024];
static char tz2[1024];

/* external variables */
char *tzname[2] = {
	tz1, tz2
};
int daylight;

extern char *timezone();

void
tzset()
{
	struct timeval tp;
	struct timezone tz;

	(void) gettimeofday(&tp, &tz);
	(void) strcpy(tz1, timezone(tz.tz_minuteswest, 0));
	(void) strcpy(tz2, timezone(tz.tz_minuteswest, 1));
	daylight = tz.tz_dsttime;
}
