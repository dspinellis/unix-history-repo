/******************************************************************************
 *                                                                            *
 * Copyright (c) David L. Mills 1993, 1994                                    *
 *                                                                            *
 * Permission to use, copy, modify, and distribute this software and its      *
 * documentation for any purpose and without fee is hereby granted, provided  *
 * that the above copyright notice appears in all copies and that both the    *
 * copyright notice and this permission notice appear in supporting           *
 * documentation, and that the name University of Delaware not be used in     *
 * advertising or publicity pertaining to distribution of the software        *
 * without specific, written prior permission.  The University of Delaware    *
 * makes no representations about the suitability this software for any       *
 * purpose.  It is provided "as is" without express or implied warranty.      *
 *                                                                            *
 ******************************************************************************/

/*
 * $Id$
 */

/*
 * Modification history kern_ntptime.c
 *
 * 14 Feb 94	David L. Mills
 *	Added code for external clock
 *
 * 28 Nov 93	David L. Mills
 *	Revised frequency scaling to conform with adjusted parameters
 *
 * 17 Sep 93	David L. Mills
 *	Created file
 */
/*
 * ntp_gettime(), ntp_adjtime() - precision time interface
 *
 * These routines consitute the Network Time Protocol (NTP) interfaces
 * for user and daemon application programs. The ntp_gettime() routine
 * provides the time, maximum error (synch distance) and estimated error
 * (dispersion) to client user application programs. The ntp_adjtime()
 * routine is used by the NTP daemon to adjust the system clock to an
 * externally derived time. The time offset and related variables set by
 * this routine are used by hardclock() to adjust the phase and
 * frequency of the phase-lock loop which controls the system clock.
 */
#include "param.h"
#include "systm.h"
#include "kernel.h"
#include "timex.h"
#include "proc.h"

struct timex ntp_pll;		/* has to be declared somewhere... */


/*
 * ntp_gettime() - NTP user application interface
 */

struct ntp_gettime_args {
	struct ntptimeval *tp;
};

int
ntp_gettime(struct proc *p, struct ntp_gettime_args *uap, int *retval)
{
	struct timeval atv;
	struct ntptimeval ntv;
	int error = 0;
	int s;

	if (uap->tp) {
		s = splclock();
		microtime(&ntv.time);
		ntv.maxerror = ntp_pll.maxerror;
		ntv.esterror = ntp_pll.esterror;
		(void) splx(s);

		error = copyout((caddr_t)&ntv, (caddr_t)uap->tp, sizeof (ntv));
	}
	if (!error)
		retval[0] = ntp_pll.status;
	return error;
}

struct ntp_adjtime_args {
	struct timex *tp;
};

extern void hardupdate(long);

/*
 * ntp_adjtime() - NTP daemon application interface
 */
int
ntp_adjtime(struct proc *p, struct ntp_adjtime_args *uap, int *retval) {
	struct timex ntv;
	int s, error;

	error = copyin((caddr_t)uap->tp, (caddr_t)&ntv, sizeof(ntv));
	if (error)
		return error;

	/*
	 * Update selected clock variables - only the superuser can
	 * change anything. Note that there is no error checking here on
	 * the assumption the superuser should know what it is doing.
	 */
	if(ntv.mode != 0 && !(error = suser(p->p_ucred, &p->p_acflag)))
	  return error ? error : EPERM;

	s = splclock();
	if (ntv.mode & ADJ_OFFSET)
		hardupdate(ntv.offset);
	if (ntv.mode & ADJ_FREQUENCY)
#ifdef PPS_SYNC
		ntp_pll.frequency = ntv.frequency - ntp_pll.ybar;
#else /* PPS_SYNC */
		ntp_pll.frequency = ntv.frequency;
#endif /* PPS_SYNC */
	if (ntv.mode & ADJ_MAXERROR)
		ntp_pll.maxerror = ntv.maxerror;
	if (ntv.mode & ADJ_ESTERROR)
		ntp_pll.esterror = ntv.esterror;
	if (ntv.mode & ADJ_STATUS)
		if (ntp_pll.status == TIME_OK || ntv.status == TIME_BAD)
			ntp_pll.status = ntv.status;
	if (ntv.mode & ADJ_TIMECONST)
		ntp_pll.time_constant = ntv.time_constant;

	/*
	 * Retrieve all clock variables
	 */
	if (ntp_pll.offset < 0)
		ntv.offset = -(-ntp_pll.offset >> SHIFT_UPDATE);
	else
		ntv.offset = ntp_pll.offset >> SHIFT_UPDATE;
#ifdef PPS_SYNC
	ntv.frequency = ntp_pll.frequency + ntp_pll.ybar;
#else /* PPS_SYNC */
	ntv.frequency = ntp_pll.frequency;
#endif /* PPS_SYNC */
	ntv.maxerror = ntp_pll.maxerror;
	ntv.esterror = ntp_pll.esterror;
	ntv.status = ntp_pll.status;
	ntv.time_constant = ntp_pll.time_constant;
	ntv.precision = ntp_pll.precision;
	ntv.tolerance = ntp_pll.tolerance;
#ifdef PPS_SYNC
	ntv.ybar = ntp_pll.ybar;
	ntv.disp = ntp_pll.disp;
	ntv.shift = ntp_pll.shift;
	ntv.calcnt = ntp_pll.calcnt;
	ntv.jitcnt = ntp_pll.jitcnt;
	ntv.discnt = ntp_pll.discnt;
#endif /* PPS_SYNC */
	(void)splx(s);

	error = copyout((caddr_t)&ntv, (caddr_t)uap->tp, sizeof(ntv));
	retval[0] = ntp_pll.status;
	return error;
}

