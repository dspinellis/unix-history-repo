#ifndef lint
static char *RCSid = "$Header: /f/osi/others/ntp/RCS/ntp_adjust.c,v 7.1 91/02/22 09:33:46 mrose Interim $";
#endif

/*
 * This module implemenets the logical Local Clock, as described in section
 * 5. of the NTP specification.
 * based on the ntp 3.4 code, but modified for OSI etc.
 * 
 * $Log:	ntp_adjust.c,v $
 * Revision 7.1  91/02/22  09:33:46  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/12/10  17:21:27  mrose
 * *** empty log message ***
 * 
 * Revision 1.1  89/06/15  20:36:55  jpo
 * Initial revision
 * 
 * 
 */

#include "ntp.h"

#ifdef	DEBUG
extern int debug;
#endif

extern int doset;
extern int kern_tickadj;
extern char *ntoa();
extern struct sysdata sys;
extern	LLog *pgm_log;

double	drift_comp = 0.0,
	compliance,
	clock_adjust;
long	update_timer = 0;

int	adj_precision;
double	adj_residual;
int	firstpass = 1;

#define	abs(x)	((x) < 0 ? -(x) : (x))

void
init_logical_clock()
{
	if (kern_tickadj)
		adj_precision = kern_tickadj;
	else
		adj_precision = 1;
	/*
	 *  If you have the "fix" for adjtime() installed in you kernel, you'll
	 *  have to make sure that adj_precision is set to 1 here.
	 */
}


/*
 *  5.0 Logical clock procedure
 *
 *  Only paramter is an offset to vary the clock by, in seconds.  We'll either
 *  arrange for the clock to slew to accomodate the adjustment, or just preform
 *  a step adjustment if the offset is too large.
 *
 *  The update which is to be performed is left in the external
 *  clock_adjust. 
 *
 *  Returns non-zero if clock was reset rather than slewed.
 *
 *  Many thanks for Dennis Ferguson <dennis@gw.ccie.utoronto.ca> for his
 *  corrections to my code.
 */

int
adj_logical(offset)
	double offset;
{
	struct timeval tv1, tv2;
#ifdef	XADJTIME2
	struct timeval delta, olddelta;
#endif
	
	/*
	 *  Now adjust the logical clock
	 */
	if (!doset)
		return 0;

	adj_residual = 0.0;
	if (offset > CLOCK_MAX || offset < -CLOCK_MAX) {
		double steptime = offset;

		(void) gettimeofday(&tv2, (struct timezone *) 0);
		steptime += tv2.tv_sec;
		steptime += tv2.tv_usec / 1000000.0;
		tv1.tv_sec = steptime;
		tv1.tv_usec = (steptime - tv1.tv_sec) * 1000000;
#ifdef	DEBUG
		if (debug > 2) {
			steptime = (tv1.tv_sec + tv1.tv_usec/1000000.0) -
				(tv2.tv_sec + tv2.tv_usec/1000000.0);
			TRACE (2, ("adj_logical: %f %f", offset, steptime));
		}
#endif
		if (settimeofday(&tv1, (struct timezone *) 0) < 0) {
			advise (LLOG_EXCEPTIONS, NULLCP, "Can't set time: %m");
			return(-1);
		}
		else {
			TRACE (1, ("set time of day"));
		}
		clock_adjust = 0.0;
		firstpass = 1;
		update_timer = 0;
		return (1);	  /* indicate that step adjustment was done */
	} else 	{
		double ai;

		/*
		 * If this is our very first adjustment, don't touch
		 * the drift compensation (this is f in the spec
		 * equations), else update using the *old* value
		 * of the compliance.
		 */
		clock_adjust = offset;
		if (firstpass)
			firstpass = 0;
		else if (update_timer > 0) {
			ai = abs(compliance);
			ai = (double)(1<<CLOCK_COMP) - 
				(double)(1<<CLOCK_FACTOR) * ai;
			if (ai < 1.0)		/* max(... , 1.0) */
				ai = 1.0;
			drift_comp += offset / (ai * (double)update_timer);
		}

		/*
		 * Set the timer to zero.  adj_host_clock() increments it
		 * so we can tell the period between updates.
		 */
		update_timer = 0;

		/*
		 * Now update the compliance.  The compliance is h in the
		 * equations.
		 */
		compliance += (offset - compliance)/(double)(1<<CLOCK_TRACK);

#ifdef XADJTIME2
		delta.tv_sec = offset;
		delta.tv_usec = (offset - delta.tv_sec) * 1000;
		(void) adjtime2(&delta, &olddelta);
#endif
		return(0);
	}
}

#ifndef	XADJTIME2
extern int adjtime();

/*
 *  This is that routine that performs the periodic clock adjustment.
 *  The procedure is best described in the the NTP document.  In a
 *  nutshell, we prefer to do lots of small evenly spaced adjustments.
 *  The alternative, one large adjustment, creates two much of a
 *  clock disruption and as a result oscillation.
 *
 *  This function is called every 2**CLOCK_ADJ seconds.
 *
 */

/*
 * global for debugging?
 */
double adjustment;

void
adj_host_clock(n)
int	n;
{

	struct timeval delta, olddelta;

	if (!doset)
		return;

	/*
	 * Add update period into timer so we know how long it
	 * took between the last update and the next one.
	 */
	update_timer += n;
	/*
	 * Should check to see if update_timer > 1 day here?
	 */

	/*
	 * Compute phase part of adjustment here and update clock_adjust.
	 * Note that the equations used here are implicit in the last
	 * two equations in the spec (in particular, look at the equation
	 * for g and figure out how to  find the k==1 term given the k==0 term.)
	 */
	adjustment = clock_adjust / (double)(1<<CLOCK_PHASE);
	clock_adjust -= adjustment;

	/*
	 * Now add in the frequency component.  Be careful to note that
	 * the ni occurs in the last equation since those equations take
	 * you from 64 second update to 64 second update (ei is the total
	 * adjustment done over 64 seconds) and we're only deal in the
	 * little 4 second adjustment interval here.
	 */
	adjustment += drift_comp / (double)(1<<CLOCK_FREQ);

	/*
	 * Add in old adjustment residual
	 */
	adjustment += adj_residual;

	/*
	 * Simplify.  Adjustment shouldn't be bigger than 2 ms.  Hope
	 * writer of spec was truth telling.
	 */
#ifdef	DEBUG
	delta.tv_sec = adjustment;
	if (debug && delta.tv_sec) abort();
#else
	delta.tv_sec = 0;
#endif
	delta.tv_usec = ((long)(adjustment * 1000000.0) / adj_precision)
		   * adj_precision;

	adj_residual = adjustment - (double) delta.tv_usec / 1000000.0;

	if (delta.tv_usec == 0)
		return;

	if (adjtime(&delta, &olddelta) < 0)
		advise (LLOG_EXCEPTIONS, NULLCP, "Can't adjust time: %m");

	TRACE (2, ("adj: %ld us  %f %f",
		   delta.tv_usec, drift_comp, clock_adjust));
}
#endif
