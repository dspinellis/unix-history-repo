/* select.c - select() abstractions */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/select.c,v 7.6 91/02/22 09:15:45 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/select.c,v 7.6 91/02/22 09:15:45 mrose Interim $
 *
 *
 * $Log:	select.c,v $
 * Revision 7.6  91/02/22  09:15:45  mrose
 * Interim 6.8
 * 
 * Revision 7.5  91/01/07  12:39:56  mrose
 * update
 * 
 * Revision 7.4  90/11/21  11:35:31  mrose
 * update
 * 
 * Revision 7.3  90/05/22  19:33:29  mrose
 * update
 * 
 * Revision 7.2  89/12/05  22:04:41  mrose
 * touch-up
 * 
 * Revision 7.1  89/11/30  23:53:31  mrose
 * touch-up
 * 
 * Revision 7.0  89/11/23  21:23:24  mrose
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

#include <errno.h>
#include <stdio.h>
#include "manifest.h"
#include "tailor.h"
#include <sys/stat.h>


extern int errno;


int	xselect_blocking_on_intr = 0;

/*  */

#ifdef	SOCKETS

#include <sys/time.h>


/* Synchronous multiplexing:
	< 0 :	block indefinately
	= 0 :	poll
	> 0 :	wait
 */

int	selsocket (nfds, rfds, wfds, efds, secs)
int	nfds;
fd_set *rfds,
       *wfds,
       *efds;
int	secs;
{
    int     n;
    fd_set  ifds,
            ofds,
	    xfds;
    struct timeval  tvs;
    register struct timeval *tv = &tvs;

    if (secs != NOTOK)
	tv -> tv_sec = secs, tv -> tv_usec = 0;
    else
	tv = NULL;

    if (rfds)
	ifds = *rfds;
    if (wfds)
	ofds = *wfds;
    if (efds)
	xfds = *efds;
    for (;;) {
	switch (n = select (nfds, rfds, wfds, efds, tv)) {
	    case OK: 
		if (secs == NOTOK)
		    break;
		return OK;

	    case NOTOK:
		if (xselect_blocking_on_intr
			&& errno == EINTR
		        && secs == NOTOK)
		    continue;
		/* else fall... */
		    
	    default: 
		return n;
	}

	if (rfds)
	    *rfds = ifds;
	if (wfds)
	    *wfds = ofds;
	if (efds)
	    *efds = xfds;
    }
}
#endif

/*  */

#ifdef	EXOS

#ifdef	SYS5

/* There seems to be a bug in the SYS5 EXOS select() routine when a socket can
   be read immediately (don't know about write).  The bug is that select()
   returns ZERO, and the mask is zero'd as well.  The code below explicitly
   checks for this case.
*/

#include "sys/soioctl.h"


int	selsocket (nfds, rfds, wfds, efds, secs)
int	nfds;
fd_set *rfds,
       *wfds,
       *efds;
int	secs;
{
    register int    fd;
    int     n;
    fd_set  ifds,
            ofds;
    long    nbytes,
	    usecs;

    if (secs != NOTOK)
	usecs = secs * 1000;
    else
	usecs = 0xffff; /* used to be ~(1L << (8 * sizeof usecs - 1)) */

    if (rfds)
	ifds = *rfds;
    if (wfds)
	ofds = *wfds;
    if (efds)
	FD_ZERO (efds);

    for (;;) {
	switch (n = select (nfds + 1, rfds, wfds, usecs)) {  /* +1 for UNISYS */
	    case OK: 
		if (rfds)
		    for (fd = 0; fd < nfds; fd++)
			if (FD_ISSET (fd, &ifds)
				&& ioctl (fd, FIONREAD, (char *) &nbytes)
					    != NOTOK
				&& nbytes > 0) {
			    FD_SET (fd, rfds);
			    n++;
			}
		if (n == 0 && secs == NOTOK)
		    break;
		return n;

	    case NOTOK: 
	    default: 
		return n;
	}

	if (rfds)
	    *rfds = ifds;
	if (wfds)
	    *wfds = ofds;
    }
}
#endif
#endif

/*  */

/* This routine used to be used for devices that didn't support real select.
   Those devices are no longer supported.

   Instead the routine is used to check if an I/O abstraction has some data
   buffered in user-space for reading...
 */

static IFP	sfnx[FD_SETSIZE] = { NULL };
static caddr_t	sdata[FD_SETSIZE] = { NULL };


IFP	set_check_fd (fd, fnx, data)
int	fd;
IFP	fnx;
caddr_t	data;
{
    IFP	    ofnx;

    if (fd < 0 || fd >= FD_SETSIZE)
	return NULLIFP;

    ofnx = sfnx[fd];
    sfnx[fd] = fnx, sdata[fd] = data;

    return ofnx;
}

/*  */

int	xselect (nfds, rfds, wfds, efds, secs)
int	nfds;
fd_set *rfds,
       *wfds,
       *efds;
int	secs;
{
    register int    fd;
    int	    n;
    fd_set  ifds,
	    ofds,
	    xfds;
    static int nsysfds = NOTOK;

    if (nsysfds == NOTOK)
	nsysfds = getdtablesize ();
    if (nfds > FD_SETSIZE)
	nfds = FD_SETSIZE;
    if (nfds > nsysfds + 1)
	nfds = nsysfds + 1;

    FD_ZERO (&ifds);
    n = 0;

    for (fd = 0; fd < nfds; fd++)
	if (sfnx[fd] != NULLIFP
	        && rfds
	        && FD_ISSET (fd, rfds)
		&& (*sfnx[fd]) (fd, sdata[fd]) == DONE) {
	    FD_SET (fd, &ifds);
	    n++;
	}

    if (n > 0) {
	*rfds = ifds;	/* struct copy */
	if (wfds)
	    FD_ZERO (wfds);
	if (efds)
	    FD_ZERO (efds);
	
	return n;
    }

    if (rfds)
	ifds = *rfds;	/* struct copy */
    if (wfds)
	ofds = *wfds;	/* struct copy */
    if (efds)
	xfds = *efds;	/* struct copy */

    if ((n = selsocket (nfds, rfds, wfds, efds, secs)) != NOTOK)
	return n;

    if (errno == EBADF) {
	struct stat st;

	if (rfds)
	    FD_ZERO (rfds);
	if (wfds)
	    FD_ZERO (wfds);
	if (efds)
	    FD_ZERO (efds);

	n = 0;
	for (fd = 0; fd < nfds; fd++)
	    if (((rfds && FD_ISSET (fd, &ifds))
			|| (wfds && FD_ISSET (fd, &ofds))
			|| (efds && FD_ISSET (fd, &xfds)))
		    && fstat (fd, &st) == NOTOK) {
		if (rfds && FD_ISSET (fd, &ifds))
		    FD_SET (fd, rfds);
		if (wfds && FD_ISSET (fd, &ofds))
		    FD_SET (fd, wfds);
		if (efds && FD_ISSET (fd, &xfds))
		    FD_SET (fd, efds);

		SLOG (compat_log, LLOG_EXCEPTIONS, "",
		      ("fd %d has gone bad", fd));
		n++;
	    }

	if (n)
	    return n;

	errno = EBADF;
    }

    return NOTOK;
}
