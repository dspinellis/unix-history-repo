#include "util.h"
#include "mmdf.h"
#include "ch.h"
#include <signal.h>

/* 
 *
 *			C H _ B B O A R D S . C
 *
 *			the new BBoards channel
 *
 *
 *	This is the channel that is used to handle Internet BBoard
 *	distribution in an intelligent fashion.  In order to run it, you
 *	need the UCI BBoards facility installed.  This requires the
 *	establishment of a special login called ``bboards'', and the
 *	getbbent() package.
 *
 *	The idea is simple.  Distribution lists get aliased to go through
 *	this channel.  Suppose that the relay (or site) using ch_bboards
 *	subscribes to UNIX-WIZARDS.  The maintainer of the list is given
 *	the address ``dist-unix-wizards'' to send to for this relay and all
 *	sites that it serves.  The site manager then defines the following
 *	alias in the aliases file:
 *
 *		dist-unix-wizards:	unix-wizards@dist-bboards
 *
 *	This channel (and this channel alone) is then defined to serve the
 *	``dist-bboards'' host.  When it gets invoked, the channel does two
 *	things:  First, if the relay itself subscribes to the BBoard (the
 *	bb_file entry in the BBoards file is non-empty), then it delivers
 *	the message to the file.  Second, if other sites subscribe to the
 *	BBoard, then ch_bboards will enter the message back into the queue
 *	system using the ``bboards'' login as the sender.
 *
 *	This achieves two goals:  first, the incoming bandwidth of relays
 *	is not degraded by many sites subscribing to the same BBoard;
 *	second, if an address goes bad down the line, the relay's
 *	``bboards'' login gets the message back (not the originator).  Since
 *	the relay's PostMaster is assumed to monitor this mailbox, problems
 *	can be found and corrected.
 *
 *	Finally, ch_bboards can be run by a site that does not relay for
 *	other sites.  In this case, the bb_dist field is empty.
 *
 */

/* 	Unlike previous versions of ch_bboards, this version does not change
 *	the contents of the headers of the message being re-distributed.
 *	The following changes are made:
 *
 *	    Envelope:	The failure address is changed to bboards@locname
 *	    Headers:	Another Received: is added
 *
 *
 *	The local copy going to the BBoard has two entries prepended to the
 *	headers:
 *
 *	    BBoard-ID: n
 *	    BB-Posted: date/time
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

    ll_hdinit (logptr, "BB");
    logptr -> ll_file = dupfpath (logptr -> ll_file, logdfldir);

    siginit ();
    signal (SIGINT, SIG_IGN);

    if ((chanptr = ch_nm2struct (*argv)) == (Chan *) NOTOK)
	err_abrt (RP_PARM, "unknown channel name '%s'", *argv);

    retval = ch_bboards (argc, argv, chanptr);
    ll_close (logptr);

    exit (retval);
}

/*  */

ch_bboards (argc, argv, chanptr)
int     argc;
char  **argv;
Chan * chanptr;
{
#ifdef	DEBUG
    ll_log (logptr, LLOGBTR, "ch_bboards(argc=%d,*argv='%s')", argc, *argv);
#endif

    if (rp_isbad (qu_init (argc, argv)))
	return RP_NO;
    if (rp_isbad (bb_init (chanptr)))
	return RP_NO;

    if (rp_isbad (qu2bb_send ()))
	return RP_NO;

    qu_end (OK);
    bb_end (OK);

    return RP_OK;
}

/*  */

err_abrt (code, fmt, b, c, d)
short   code;
char   *fmt,
       *b,
       *c,
       *d;
{
    char    linebuf[LINESIZE];

    qu_end (NOTOK);
    bb_end (NOTOK);

    sprintf (linebuf, "%s%s", "[abend:  %s]", fmt);
    ll_log (logptr, LLOGFAT, linebuf, rp_valstr (code), b, c, d);
    ll_close (logptr);

    exit (code);
}
