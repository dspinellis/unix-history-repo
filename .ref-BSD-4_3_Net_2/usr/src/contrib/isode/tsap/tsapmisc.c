/* tsapmisc.c - miscellany tsap functions */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/tsap/RCS/tsapmisc.c,v 7.3 91/02/22 09:47:39 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/tsap/RCS/tsapmisc.c,v 7.3 91/02/22 09:47:39 mrose Interim $
 *
 *
 * $Log:	tsapmisc.c,v $
 * Revision 7.3  91/02/22  09:47:39  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/03/23  17:31:47  mrose
 * 8
 * 
 * Revision 7.1  90/03/06  09:34:18  mrose
 * update
 * 
 * Revision 7.0  89/11/23  22:30:54  mrose
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
#include <signal.h>
#include "tpkt.h"
#include <sys/ioctl.h>

/*    estimate of octets that might be returned */

int	TSelectOctets (sd, nbytes, td)
int	sd;
long   *nbytes;
register struct TSAPdisconnect *td;
{
    int	    result;
    long    value;
    SBV	    smask;
    register struct tsapblk *tb;

    missingP (nbytes);
    missingP (td);

    smask = sigioblock ();

    tsapPsig (tb, sd);

    result = OK;
    if (tb -> tb_nreadfnx) {
	if ((result = (*tb -> tb_nreadfnx) (tb, &value)) == NOTOK)
	    value = 0L;
    }
    else {
#ifdef	FIONREAD
	if (ioctl (tb -> tb_fd, FIONREAD, (char *) &value) == NOTOK)
	    value = 0L;
#endif

	switch (tb -> tb_flags & (TB_TP0 | TB_TP4)) {
	    case TB_TCP:
	    case TB_X25:
	    case TB_BRG:
	        if (value > DT_MAGIC && tb -> tb_len == 0)
		    value -= DT_MAGIC;
	        break;

	    default:
		break;
	}
    }

    if (result == OK)
	value += (long) tb -> tb_len;
    *nbytes = value;

    (void) sigiomask (smask);

    return result;
}

/*    get TSAPs */

int	TGetAddresses (sd, initiating, responding, td)
int	sd;
struct TSAPaddr *initiating,
		*responding;
register struct TSAPdisconnect *td;
{
    SBV	    smask;
    register struct tsapblk *tb;

    missingP (td);

    smask = sigioblock ();

    tsapPsig (tb, sd);

    if (initiating)
	copyTSAPaddrX (&tb -> tb_initiating, initiating);
    if (responding)
	copyTSAPaddrX (&tb -> tb_responding, responding);

    (void) sigiomask (smask);

    return OK;
}

/*    define transport manager */

#ifdef	MGMT
int	TSetManager (sd, fnx, td)
int	sd;
IFP	fnx;
register struct TSAPdisconnect *td;
{
    SBV	    smask;
    register struct tsapblk *tb;

    missingP (td);

    smask = sigioblock ();

    tsapPsig (tb, sd);

    tb -> tb_manfnx = fnx;

    (void) sigiomask (smask);

    return OK;
}
#endif
