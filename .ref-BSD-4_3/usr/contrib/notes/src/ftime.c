#include "parms.h"
#include "structs.h"

#ifdef	USG
#ifdef	RCSIDENT
static char *SccsId = "@(#)ftime.c	2.4	4/20/84";
static char *RCSid = "$Header: ftime.c,v 1.7.0.1 85/02/06 08:44:29 notes Rel $";
#endif	RCSIDENT

/*
 *	This file performs the "ftime" call for systems which
 *	no longer support that system call.
 *
 *	Currently, this is only the USG systems: System III, System V,
 *	and UNIX 4.0 (btl-internal)
 *	This code is only compiled for these systems; the entire file
 *	is surrounded by an #ifdef USG
 *
 *	this code taken from news 2.10
 */

#include <sys/types.h>
struct timeb
{
    time_t time;
    unsigned short  millitm;
    short   timezone;
    short   dstflag;
};

extern long timezone;
extern int  daylight;

ftime (tp)
struct timeb   *tp;
{
    long    t;

    time (&t);
    tp -> time = t;
    tp -> millitm = 0;
    tp -> timezone = timezone / 60;
    tp -> dstflag = daylight;
}
#endif	USG
