#include "util.h"
#include "mmdf.h"
#include "ch.h"
#include <signal.h>

/*
 *			C H _ U U C P . C
 *
 *		Take message and feed a request to UUX
 *
 *  qu2uu_send does the interesting work.  This interface was developed
 *  for MMDF by Doug Kingston at the US Army Ballistics Research Lab,
 *  Aberdeen, Maryland.    <dpk@brl>
 *
 *		    Original Version 21 Oct 81
 *
 *			Revision History
 *			================
 *
 *    27-Oct-82	Marshall T. Rose <mrose%uci@rand-relay>
 *		Support proper munging by using the UCI mail filtering
 *		routines (enabled by #ifdef MF)
 *
 *    17-Oct-83 Marshall T. Rose <mrose%uci@rand-relay>
 *		Major re-organization and some new interfacing.
 *
 */

/*
 *     MULTI-CHANNEL MEMO DISTRIBUTION FACILITY  (MMDF)
 *     
 *
 *     Department of Electrical Engineering
 *     University of Delaware
 *     Newark, Delaware  19711
 *
 *     Phone:  (302) 738-1163
 *     
 *
 */

/*  */

extern char logdfldir[];

extern struct ll_struct chanlog;
struct ll_struct   *logptr = &chanlog;

char   *dupfpath ();

/*  */

main (argc, argv)
int     argc;
char  **argv;
{
    short   retval;
    Chan * chanptr;

    ll_hdinit (logptr, "UU");
    logptr -> ll_file = dupfpath (logptr -> ll_file, logdfldir);

    siginit ();
    signal (SIGINT, SIG_IGN);

    if ((chanptr = ch_nm2struct (*argv)) == (Chan *) NOTOK)
	err_abrt (RP_PARM, "unknown channel name '%s'", *argv);

    retval = ch_uucp (argc, argv, chanptr);
    ll_close (logptr);

    exit (retval);
}

/*  */

ch_uucp (argc, argv, chanptr)
int     argc;
char  **argv;
Chan * chanptr;
{
#ifdef DEBUG
    logptr -> ll_level = LLOGBTR;
    ll_log (logptr, LLOGBTR, "ch_uucp(argc=%d,*argv='%s')", argc, *argv);
#endif

    if (rp_isbad (qu_init (argc, argv)))
	return RP_NO;
    if (rp_isbad (uu_init (chanptr)))
	return RP_NO;

    if (rp_isbad (qu2uu_send ()))
	return RP_NO;

    qu_end (OK);
    uu_end (OK);

    return RP_OK;
}

/*  */

err_abrt (code, fmt, b, c, d)
short   code;
char    fmt[],
        b[],
        c[],
        d[];
{
    char    linebuf[LINESIZE];

    qu_end (NOTOK);
    uu_end (NOTOK);

    sprintf (linebuf, "%s%s", "[ABEND:  %s]", fmt);
    ll_log (logptr, LLOGFAT, linebuf, rp_valstr (code), b, c, d);
    ll_close (logptr);

    exit (code);
}
