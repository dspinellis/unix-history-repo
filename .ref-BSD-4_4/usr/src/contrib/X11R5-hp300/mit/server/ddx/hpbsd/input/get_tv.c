static char *RCSID = "$Header: /host/kaukau/disk2/X11R5/R5-hp300/mit/server/ddx/hpbsd/input/RCS/get_tv.c,v 1.1 1992/09/30 03:14:10 root Exp $";
/*
 * An alternative to gettimeofday(2) that is about 50 times faster
 * if used frequently.  HP-PA specific.
 *                     Dave Holt,  GSY Performance, dah@cup.hp.com
 */

#include <time.h>
#include <assert.h>

#ifdef NOTDEFINED /* The following is not currently used.  But 	
		   * I will leave it for possible future use.
		   * --kam 4/22/92
		   */

#ifdef DEBUG
#include <stdio.h>
#else /* Not DEBUG.  Don't bother with printf's. */
#define printf(s, a)
#endif /* DEBUG */

#define MILLION 1000000
#define BUNCH 8192		/* must be < 32767 */

unsigned int cr16();		/* in cr16.s (assembly code) */
static struct timeval slow_get_tv();
void *malloc();
static set_tix2usec();

/* This initial value for last_tv forces initialization of tix2usec[]. */
static struct timeval last_tv = {0, 2000000};
static unsigned int last_tix;

/*
 * tix2usec[BUNCH] is a table built by set_tix2usec() which converts
 * from CR16 ticks to microseconds:
 *     microseconds = tix2usec[ticks >> shift]
 */
static unsigned short *tix2usec;
static int shift;

struct timeval get_tv()		/* the fast path */
{
    unsigned int tix, delta_tix;
    long delta_usec;

    tix = cr16();
    delta_tix = tix - last_tix;
    last_tix = tix;
    if (delta_tix < BUNCH) {	/* we can use the array */
	delta_usec = tix2usec[delta_tix >> shift];
	last_tv.tv_usec += delta_usec;
	if (last_tv.tv_usec < MILLION) {
	    return(last_tv);
	}
    }
    return(slow_get_tv());
}

static struct timeval slow_get_tv()	/* the slow path */
{
    struct timezone tz;
    static int first_time = 1;

    if (first_time) {
	set_tix2usec();
	first_time = 0;
    }
    last_tix = cr16();
    gettimeofday(&last_tv, &tz);
    return(last_tv);
}

/* 
 * set_tix2usec() builds it by observing gettimeofday and CR16 over a
 * reasonable period (~50ms).
 */
static set_tix2usec()
{
    struct timeval tv1, tv2;
    struct timezone tz;
    int i, tix_per_entry, delta_us;
    unsigned int tix1, tix2, cr16(), delta_tix;
    double tix_per_usec, usec_per_tix;

    /* make sure the code is in memory before we time it */
    tix1 = cr16();
    gettimeofday(&tv1, &tz);

    do {
	tix1 = cr16();
	gettimeofday(&tv1, &tz);
	do {
	    tix2 = cr16();
	    gettimeofday(&tv2, &tz);
	    delta_us = (tv2.tv_sec - tv1.tv_sec) * MILLION +
	      (tv2.tv_usec - tv1.tv_usec);
	    delta_tix = tix2 - tix1;
	} while (delta_us < 50000); /* loop for at least 50ms */
    } while (tv2.tv_sec - tv1.tv_sec > 2); /* retry if delta is too big */

    tix_per_usec = 1.0 * delta_tix / delta_us;
    usec_per_tix = 1 / tix_per_usec;

    /* Table should convert at least 10ms deltas. */
    tix_per_entry = tix_per_usec * 10000 / BUNCH;
    for (shift = 0;
	 (1 << shift) < tix_per_entry;
	 shift++) {
    }

    tix2usec = (unsigned short *) malloc(BUNCH * sizeof(unsigned short));
    assert(tix2usec != NULL);

    for (i = 0; i < BUNCH; i++) {
	tix2usec[i] = i * (1 << shift) * usec_per_tix;
    }

    printf("tix_per_usec = %lf\n", tix_per_usec);
    printf("shift = %d\n", shift);
}

#endif /* NOTDEFINED */


void
calibrate_cr16(TicksPerMilli)

unsigned long *TicksPerMilli;

{

    struct timeval start, end;
    register unsigned long cr_start, cr_end;
    unsigned long total_ticks = 0, total_millis = 0;
    int i, j;

    for(i = 0; i < 6; i++)
    {
        cr_start = cr16();
        gettimeofday(&start, NULL);

	/* waste some time */
	for(j=0; j< 200000; j++)
		;
	cr_end = cr16();
	gettimeofday(&end, NULL);

	if(start.tv_usec > end.tv_usec)
	{
	    end.tv_usec += 1000000;
	    end.tv_sec--;
	}

	/* if we have a rollover during all of this, toss this one. */

	if(cr_end < cr_start)
	{
	    i--;
	}
	else
	{
	    total_millis += (end.tv_sec - start.tv_sec) * 1000 +
			    (end.tv_usec - start.tv_usec) / 1000;
	    total_ticks += cr_end - cr_start;
	}
    }

    *TicksPerMilli = total_ticks/total_millis;

}
