#ifdef SCCSID
static char	*SccsId = "@(#)ftime.c	2.5	4/26/85";
#endif /* SCSCID */

#include <sys/types.h>
struct timeb
{
	time_t	time;
	unsigned short millitm;
	short	timezone;
	short	dstflag;
};

extern long timezone;
extern int  daylight;

ftime(tp)
struct timeb *tp;
{
	long t;

	time(&t);
	tp->time = t;
	tp->millitm = 0;
	tp->timezone = timezone/60;
	tp->dstflag = daylight;
}
