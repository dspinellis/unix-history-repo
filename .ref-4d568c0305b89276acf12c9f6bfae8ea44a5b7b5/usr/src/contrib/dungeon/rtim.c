/*
 * Interface routines for dungeon.
 * These routines are for functions expected by the game
 * that are not available in the Unix/f77 library.
 */

#ifdef SYSV
#include <stdio.h>
#endif

#include <sys/types.h>

#ifdef SYSV
#include <time.h>
#else
#include <sys/timeb.h>
#include <sys/time.h>
#endif

/* routine to get time in hours minutes and seconds */

long time();
struct tm *localtime();
struct tm *tmptr;
long timebuf;

itime_(hrptr,minptr,secptr)

int *hrptr,*minptr,*secptr;
{

	time(&timebuf);
	tmptr = localtime(&timebuf);
	
	*hrptr  = tmptr->tm_hour;
	*minptr = tmptr->tm_min;
	*secptr = tmptr->tm_sec;

	return;
}

#ifdef SYSV
/* idate - return day (1-31), month (1-12) and year (AD) */
/*	by Dave Newkirk, ihnp4!ihlpm!dcn */

idate_( date )
long date[];
{
	struct tm *t, *localtime();
	long time(), *tloc, loc;

	tloc = &loc;			/* get pointer to time in seconds */
	time(tloc);
	t = localtime(tloc);		/* get time structure filled in */
	date[0] = t->tm_mday;
	date[1] = t->tm_mon + 1;
	date[2] = t->tm_year + 1900;

} /* end idate */
#endif

/* random number initializer */
inirnd_(seedptr)

int *seedptr;
{
int seed;

	seed = *seedptr;
	srand(seed);
	return;
}

/*  random number generator */
rnd_(maxval)

int *maxval;
{
/* note: returned random number ranges from 0 to maxval */

int rndval;

	rndval = rand();

	rndval = rndval % *maxval;

	return(rndval);
}

#ifdef SYSV
/* thanks to Dave Newkirk, ihnp4!ihlpm!dcn for the following routines */

/* getuid - fortran callable getuid */

int
getuid_()
{
	return (int)getuid();
}

/* unbuf - make output completely unbuffered */

unbuf_()
{
	void setbuf();

	setbuf(stdout, NULL);
}
#endif
