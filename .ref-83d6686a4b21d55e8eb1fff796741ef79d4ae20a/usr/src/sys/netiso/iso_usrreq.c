/***********************************************************
		Copyright IBM Corporation 1987

                      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of IBM not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * ARGO Project, Computer Sciences Dept., University of Wisconsin - Madison
 */
/*
 * $Header: iso_usrreq.c,v 4.2 88/06/29 15:00:06 hagens Exp $ 
 * $Source: /usr/argo/sys/netiso/RCS/iso_usrreq.c,v $ 
 *
 * iso_usrreq.c : support for iso address family ioctls only.
 * (which support ifconfig, for example)
 */

#ifndef lint
static char *rcsid = "$Header: iso_usrreq.c,v 4.2 88/06/29 15:00:06 hagens Exp $";
#endif

#ifdef ISO

#include "types.h"
#include "param.h"
#include "mbuf.h"
#include "domain.h"
#include "protosw.h"
#include "socket.h"
#include "socketvar.h"
#include "errno.h"

#include "../net/if.h"
#include "../net/route.h"

#include "iso.h"
#include "clnp.h"
#include "clnp_stat.h"
#include "argo_debug.h"


/*
 * FUNCTION:		iso_usrreq
 *
 * PURPOSE:			Provide a means of getting from soo_ioctl to iso_control
 *
 * RETURNS:			unix error code
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			Only PRU_CONTROL causes anything to occur. PRU_ATTACH
 *					and PRU_DETACH are noops so socket and close don't return
 *					an error code.
 */
iso_usrreq(so, req, m, nam, rights, control)
struct socket	*so;		/* socket: used only to get to this code */
int				req;		/* request */
struct mbuf		*m;			/* data for request */
struct mbuf		*nam;		/* optional name */
struct mbuf		*rights;	/* optional rights */
struct mbuf		*control;	/* optional control info */
{
	int		error;

	switch (req) {
		case PRU_CONTROL:
			error = iso_control(so, (int)m, (caddr_t)nam, 
				(struct ifnet *)rights);
			break;
		
		case PRU_ATTACH:
		case PRU_DETACH:
			error = 0;
			break;
		
		default:
			error = EOPNOTSUPP;
	}

	return error;
}

#endif	ISO
