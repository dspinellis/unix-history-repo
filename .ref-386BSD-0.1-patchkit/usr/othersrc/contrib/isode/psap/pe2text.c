/* pe2text.c - write a PE thru a debug filter */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/pe2text.c,v 7.2 91/02/22 09:36:07 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/pe2text.c,v 7.2 91/02/22 09:36:07 mrose Interim $
 *
 *
 * $Log:	pe2text.c,v $
 * Revision 7.2  91/02/22  09:36:07  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/16  14:35:13  mrose
 * stupid-typo
 * 
 * Revision 7.0  89/11/23  22:12:58  mrose
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


/* LINTLIBRARY */

#include <stdio.h>
#include "psap.h"
#include "logger.h"

/*  */

/* logfile-backed abstract for PStreams */

/* ARGSUSED */

static int  ll_pswrite (ps, data, n, in_line)
PS	ps;
PElementData data;
PElementLen n;
int	in_line;
{
    register LLog    *lp = (LLog *) ps -> ps_addr;

    if (lp -> ll_stat & LLOGTTY) {
	(void) fflush (stdout);

	(void) fwrite ((char *) data, sizeof *data, (int) n, stderr);
	(void) fflush (stderr);
    }

    if (lp -> ll_fd == NOTOK) {
	if ((lp -> ll_stat & (LLOGERR | LLOGTTY)) == (LLOGERR | LLOGTTY))
	    return ((int) n);
	if (ll_open (lp) == NOTOK)
	    return NOTOK;
    }
    else
	if (ll_check (lp) == NOTOK)
	    return NOTOK;

    return write (lp -> ll_fd, (char *) data, (int) n);
}

/*  */

static int  ll_psopen (ps)
register PS ps;
{
    ps -> ps_writeP = ll_pswrite;

    return OK;
}

#define	ll_psetup(ps, lp)	((ps) -> ps_addr = (caddr_t) (lp), OK)

/*  */

void	pe2text (lp, pe, rw, cc)
register LLog *lp;
register PE pe;
int	rw,
	cc;
{
    register char   *bp;
    char   buffer[BUFSIZ];
    register PS ps;

    bp = buffer;
    (void) sprintf (bp, "%s PE", rw ? "read" : "wrote");
    bp += strlen (bp);
    if (pe -> pe_context != PE_DFLT_CTX) {
	(void) sprintf (bp, ", context %d", pe -> pe_context);
	bp += strlen (bp);
    }
    if (cc != NOTOK) {
	(void) sprintf (bp, ", length %d", cc);
	bp += strlen (bp);
    }
    LLOG (lp, LLOG_ALL, ("%s", buffer));

    if ((ps = ps_alloc (ll_psopen)) != NULLPS) {
	if (ll_psetup (ps, lp) != NOTOK)
	    (void) pe2pl (ps, pe);

	ps_free (ps);
    }

    (void) ll_printf (lp, "-------\n");

    (void) ll_sync (lp);
}
