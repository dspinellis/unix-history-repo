/* acserver2.c - generic server dispatch */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/acsap/RCS/acserver2.c,v 7.4 91/02/22 09:14:24 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/acsap/RCS/acserver2.c,v 7.4 91/02/22 09:14:24 mrose Interim $
 *
 *
 * $Log:	acserver2.c,v $
 * Revision 7.4  91/02/22  09:14:24  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/10/29  18:37:51  mrose
 * updates
 * 
 * Revision 7.2  90/07/09  14:30:46  mrose
 * sync
 * 
 * Revision 7.1  90/02/19  13:07:07  mrose
 * update
 * 
 * Revision 7.0  89/11/23  21:22:03  mrose
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

#include <signal.h>
#include "psap.h"
#include "tsap.h"
#include <sys/ioctl.h>
#ifdef	BSD42
#include <sys/file.h>
#endif
#ifdef	SYS5
#include <fcntl.h>
#endif
#include "tailor.h"

/*  */

static	int	is_nfds;
static fd_set	is_mask;
static char	is_single;
    		
/*  */

int	iserver_init (argc, argv, aei, initfnx, td)
int	argc;
char	**argv;
AEI	aei;
IFP	initfnx;
struct TSAPdisconnect *td;
{
    int	fd;

    isodetailor (NULLCP, 0);

    is_nfds = 0;
    FD_ZERO (&is_mask);

    if (argc > 1) {
	is_single = 1;
	if ((fd = (*initfnx) (argc, argv)) == NOTOK)
	    return tsaplose (td, DR_NETWORK, NULLCP, "initialization failed");

	is_nfds = fd + 1;
	FD_SET (fd, &is_mask);
    }
    else {
	struct PSAPaddr *pa;
	is_single = 0;

	if ((pa = aei2addr (aei)) == NULLPA)
	    return tsaplose (td, DR_ADDRESS, NULLCP,
		    "address translation failed");

	if (TNetListen (&pa -> pa_addr.sa_addr, td) == NOTOK)
	    return NOTOK;

	if (!isatty (2)) {
	    int     i;

	    for (i = 0; i < 5; i++) {
		switch (fork ()) {
		    case NOTOK: 
			sleep (5);
			continue;

		    case OK: 
			break;

		    default: 
			_exit (0);
		}
		break;
	    }

	    (void) chdir ("/");

	    if ((fd = open ("/dev/null", O_RDWR)) != NOTOK) {
		if (fd != 0)
		    (void) dup2 (fd, 0), (void) close (fd);
		(void) dup2 (0, 1);
		(void) dup2 (0, 2);
	    }

#ifdef	SETSID
	    (void) setsid ();
#endif
#ifdef	TIOCNOTTY
	    if ((fd = open ("/dev/tty", O_RDWR)) != NOTOK) {
		(void) ioctl (fd, TIOCNOTTY, NULLCP);
		(void) close (fd);
	    }
#else
#ifdef	SYS5
	    (void) setpgrp ();
	    (void) signal (SIGINT, SIG_IGN);
	    (void) signal (SIGQUIT, SIG_IGN);
#endif
#endif
	    isodexport (NULLCP);	/* re-initialize logfiles */
	}
    }

    return OK;
}

/*  */

int	iserver_wait (initfnx, workfnx, losefnx, nfds, rfds, wfds, efds, secs,
		      td)
IFP	initfnx,
    	workfnx,
    	losefnx;
int	nfds;
fd_set *rfds,
       *wfds,
       *efds;
int	secs;
struct TSAPdisconnect *td;
{
    int	    fd,
	    vecp;
    fd_set  ifds,
	    ofds,
    	    xfds;
    char   *vec[4];

    ifds = is_mask;	/* struct copy */
    FD_ZERO (&ofds);
    FD_ZERO (&xfds);
    if (is_nfds > nfds)
	nfds = is_nfds + 1;

    if (rfds)
	for (fd = 0; fd < nfds; fd++)
	    if (FD_ISSET (fd, rfds))
		FD_SET (fd, &ifds);
    if (wfds)
	ofds = *wfds;
    if (efds)
	xfds = *efds;

    if (TNetAccept (&vecp, vec, nfds, &ifds, &ofds, &xfds, secs, td)
	    == NOTOK) {
	(void) (*losefnx) (td);

	return OK;
    }
    if (wfds)
	*wfds = ofds;
    if (efds)
	*efds = xfds;

    if (vecp > 0 && (fd = (*initfnx) (vecp, vec)) != NOTOK) {
	if (fd >= is_nfds)
	    is_nfds = fd + 1;
	FD_SET (fd, &is_mask);
    }

    for (fd = 0; fd < nfds; fd++)
	if (FD_ISSET (fd, &is_mask) && FD_ISSET (fd, &ifds)) {
	    if (workfnx == NULLIFP) {
		(void) TNetClose (NULLTA, td);
		return tsaplose (td, DR_OPERATION, NULLCP,
				 "no worker routine for connected fd");
	    }
	    FD_CLR (fd, &ifds);
	    if ((*workfnx) (fd) == NOTOK) {
		FD_CLR (fd, &is_mask);
		if (is_nfds == fd + 1)
		    is_nfds--;

		if (is_single) {
		    int	    xd;

		    for (xd = 0; xd < nfds; xd++)
			if (FD_ISSET (xd, &is_mask))
			    break;
		    if (rfds)
			FD_ZERO (rfds);
		    if (xd >= is_nfds)
			return DONE;
		}
	    }
	}
    if (rfds)
	*rfds = ifds;
    return OK;
}

/*  */

fd_set	iserver_mask ()
{
    return is_mask;
}
