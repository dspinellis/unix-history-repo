/* rtfs.c - RT-file transfer utility -- common subroutines */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/rtf/RCS/rtfsbr.c,v 7.1 91/02/22 09:34:22 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/rtf/RCS/rtfsbr.c,v 7.1 91/02/22 09:34:22 mrose Interim $
 *
 *
 * $Log:	rtfsbr.c,v $
 * Revision 7.1  91/02/22  09:34:22  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:10:50  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include "rtf.h"
#include <varargs.h>
#if	defined(SYS5) && !defined(HPUX)
#include <sys/times.h>
#define	TMS
#endif

/*    DATA */

static LLog _pgm_log = {
    "rtf.log", NULLCP, NULLCP, LLOG_FATAL | LLOG_EXCEPTIONS | LLOG_NOTICE,
    LLOG_FATAL, -1, LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
LLog *pgm_log = &_pgm_log;

/*  */

#define	RC_BASE	0x80


static char *reason_err0[] = {
    "no specific reason stated",
    "user receiving ability jeopardized",
    "reserved(1)",
    "user sequence error",
    "reserved(2)",
    "local SS-user error",
    "unreceoverable procedural error"
};

static int reason_err0_cnt = sizeof reason_err0 / sizeof reason_err0[0];


static char *reason_err8[] = {
    "demand data token"
};

static int reason_err8_cnt = sizeof reason_err8 / sizeof reason_err8[0];


char   *SReportString (code)
int	code;
{
    register int    fcode;
    static char buffer[BUFSIZ];

    if (code == SP_PROTOCOL)
	return "SS-provider protocol error";

    code &= 0xff;
    if (code & RC_BASE) {
	if ((fcode = code & ~RC_BASE) < reason_err8_cnt)
	    return reason_err8[fcode];
    }
    else
	if (code < reason_err0_cnt)
	    return reason_err0[code];

    (void) sprintf (buffer, "unknown reason code 0x%x", code);
    return buffer;
}

/*  */

void	rts_adios (rta, event)
register struct RtSAPabort *rta;
char   *event;
{
    rts_advise (rta, event);

    _exit (1);
}


void	rts_advise (rta, event)
register struct RtSAPabort *rta;
char   *event;
{
    char    buffer[BUFSIZ];

    if (rta -> rta_cc > 0)
	(void) sprintf (buffer, "[%s] %*.*s", RtErrString (rta -> rta_reason),
		rta -> rta_cc, rta -> rta_cc, rta -> rta_data);
    else
	(void) sprintf (buffer, "[%s]", RtErrString (rta -> rta_reason));

    advise (LLOG_NOTICE, NULLCP, "%s: %s", event, buffer);
}

/*  */

#ifndef	lint
void	adios (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);
    
    _ll_log (pgm_log, LLOG_FATAL, ap);

    va_end (ap);

    _exit (1);
}
#else
/* VARARGS */

void	adios (what, fmt)
char   *what,
       *fmt;
{
    adios (what, fmt);
}
#endif


#ifndef	lint
void	advise (va_alist)
va_dcl
{
    int	    code;
    va_list ap;

    va_start (ap);
    
    code = va_arg (ap, int);

    _ll_log (pgm_log, code, ap);

    va_end (ap);
}
#else
/* VARARGS */

void	advise (code, what, fmt)
char   *what,
       *fmt;
int	code;
{
    advise (code, what, fmt);
}
#endif


#ifndef	lint
void	ryr_advise (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _ll_log (pgm_log, LLOG_NOTICE, ap);

    va_end (ap);
}
#else
/* VARARGS */

void	ryr_advise (what, fmt)
char   *what,
       *fmt;
{
    ryr_advise (what, fmt);
}
#endif

/*  */

#ifdef	lint
/* VARARGS4 */

int	rtsaplose (rti, reason, what, fmt)
struct RtSAPindication *rti;
int	reason;
char   *what,
       *fmt;
{
    return rtsaplose (rti, reason, what, fmt);
}
#endif

/*  */

#ifndef	NBBY
#define	NBBY	8
#endif


#ifndef	TMS
timer (cc)
int     cc;
{
    long    ms;
    float   bs;
    struct timeval  stop,
                    td;
    static struct timeval   start;

    if (cc == 0) {
	(void) gettimeofday (&start, (struct timezone *) 0);
	return;
    }
    else
	(void) gettimeofday (&stop, (struct timezone  *) 0);

    tvsub (&td, &stop, &start);
    ms = (td.tv_sec * 1000) + (td.tv_usec / 1000);
    bs = (((float) cc * NBBY * 1000) / (float) (ms ? ms : 1)) / NBBY;

    advise (LLOG_NOTICE, NULLCP,
	    "transfer complete: %d bytes in %d.%02d seconds (%.2f Kbytes/s)",
	    cc, td.tv_sec, td.tv_usec / 10000, bs / 1024);
}


static  tvsub (tdiff, t1, t0)
register struct timeval *tdiff,
			*t1,
			*t0;
{

    tdiff -> tv_sec = t1 -> tv_sec - t0 -> tv_sec;
    tdiff -> tv_usec = t1 -> tv_usec - t0 -> tv_usec;
    if (tdiff -> tv_usec < 0)
	tdiff -> tv_sec--, tdiff -> tv_usec += 1000000;
}

#else
long	times ();


static	timer (cc)
int	cc;
{
    long    ms;
    float   bs;
    long    stop,
	    td,
	    secs,
	    msecs;
    struct tms tm;
    static long start;

    if (cc == 0) {
	start = times (&tm);
	return;
    }
    else
	stop = times (&tm);

    td = stop - start;
    secs = td / 60, msecs = (td % 60) * 1000 / 60;
    ms = (secs * 1000) +  msecs;
    bs = (((float) cc * NBBY * 1000) / (float) (ms ? ms : 1)) / NBBY;
    
    advise (LLOG_NOTICE, NULLCP,
	    "transfer complete: %d bytes in %d.%02d seconds (%.2f Kbytes/s)",
	    cc, secs, msecs / 10, bs / 1024);
}
#endif
