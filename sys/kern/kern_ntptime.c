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
#include <sys/param.h>
#include <sys/user.h>
#include <sys/kernel.h>
#include <sys/proc.h>
#include <sys/timex.h>

/*
 * The following variables are used by the hardclock() routine in the
 * kern_clock.c module and are described in that module. 
 */
extern long time_offset;	/* time adjustment (us) */
extern long time_freq;		/* frequency offset (scaled ppm) */
extern long time_maxerror;	/* maximum error (us) */
extern long time_esterror;	/* estimated error (us) */
extern int time_status;		/* clock synchronization status */
extern long time_constant;	/* pll time constant */
extern long time_precision;	/* clock precision (us) */
extern long time_tolerance;	/* frequency tolerance (scaled ppm) */

#ifdef PPS_SYNC
/*
 * The following variables are used only if the PPS signal discipline
 * is configured in the kernel.
 */
extern long pps_ybar;		/* frequency estimate (scaled ppm) */
extern long pps_disp;		/* dispersion estimate (scaled ppm) */
extern int pps_shift;		/* interval duration (s) (shift) */
extern long pps_calcnt;		/* calibration intervals */
extern long pps_jitcnt;		/* jitter limit exceeded */
extern long pps_discnt;		/* dispersion limit exceeded */
#endif /* PPS_SYNC */

/*
 * ntp_gettime() - NTP user application interface
 */
ntp_gettime()
{
	register struct a {
		struct ntptimeval *tp;
	} *uap = (struct a *)u.u_ap;
	struct timeval atv;
	struct ntptimeval ntv;
	int s;

	if (uap->tp) {

		s = splclock();
#ifdef EXT_CLOCK
		/*
		 * The microtime() external clock routine returns a
		 * status code. If less than zero, we declare a bummer.
		 * While there are other places that call microtime(),
		 * this is the only place that matters from an
		 * application point of view.
		 */
		if (microtime(&atv) < 0)
			time_status = TIME_ERR;
		else if (time_status == TIME_ERR)
			time_status = TIME_BAD;
#else /* EXT_CLOCK */
		microtime(&atv);
#endif /* EXT_CLOCK */
		ntv.time = atv;
		ntv.maxerror = time_maxerror;
		ntv.esterror = time_esterror;
		(void) splx(s);

		u.u_error = copyout((caddr_t)&ntv, (caddr_t)uap->tp,
			sizeof (ntv));
	}
	if (!u.u_error)
		u.u_r.r_val1 = time_status;
}

/*
 * ntp_adjtime() - NTP daemon application interface
 */
ntp_adjtime()
{
	register struct a {
		struct timex *tp;
	} *uap = (struct a *)u.u_ap;
	struct timex ntv;
	int s;

	u.u_error = copyin((caddr_t)uap->tp, (caddr_t)&ntv,
	    sizeof(ntv));
	if (u.u_error)
		return;

	/*
	 * Update selected clock variables - only the superuser can
	 * change anything. Note that there is no error checking here on
	 * the assumption the superuser should know what it is doing.
	 */
	if (!suser() && ntv.mode != 0)
		return;

	s = splclock();
	if (ntv.mode & ADJ_OFFSET)
		hardupdate(ntv.offset);
	if (ntv.mode & ADJ_FREQUENCY)
#ifdef PPS_SYNC
		time_freq = ntv.frequency - pps_ybar;
#else /* PPS_SYNC */
		time_freq = ntv.frequency;
#endif /* PPS_SYNC */
	if (ntv.mode & ADJ_MAXERROR)
		time_maxerror = ntv.maxerror;
	if (ntv.mode & ADJ_ESTERROR)
		time_esterror = ntv.esterror;
	if (ntv.mode & ADJ_STATUS)
		if (time_status == TIME_OK || ntv.status == TIME_BAD)
			time_status = ntv.status;
	if (ntv.mode & ADJ_TIMECONST)
		time_constant = ntv.time_constant;

	/*
	 * Retrieve all clock variables
	 */
	if (time_offset < 0)
		ntv.offset = -(-time_offset >> SHIFT_UPDATE);
	else
		ntv.offset = time_offset >> SHIFT_UPDATE;
#ifdef PPS_SYNC
	ntv.frequency = time_freq + pps_ybar;
#else /* PPS_SYNC */
	ntv.frequency = time_freq;
#endif /* PPS_SYNC */
	ntv.maxerror = time_maxerror;
	ntv.esterror = time_esterror;
	ntv.status = time_status;
	ntv.time_constant = time_constant;
	ntv.precision = time_precision;
	ntv.tolerance = time_tolerance;
#ifdef PPS_SYNC
	ntv.ybar = pps_ybar;
	ntv.disp = pps_disp;
	ntv.shift = pps_shift;
	ntv.calcnt = pps_calcnt;
	ntv.jitcnt = pps_jitcnt;
	ntv.discnt = pps_discnt;
#endif /* PPS_SYNC */
	(void)splx(s);

	u.u_error = copyout((caddr_t)&ntv, (caddr_t)uap->tp,
	    sizeof(ntv));
	if (!u.u_error)
		u.u_r.r_val1 = time_status;
}
