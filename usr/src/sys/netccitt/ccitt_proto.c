/*
 * Copyright (c) University of British Columbia, 1984
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Laboratory for Computation Vision and the Computer Science Department
 * of the University of British Columbia.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ccitt_proto.c	7.2 (Berkeley) %G%
 */

#include "../h/param.h"
#include "../h/socket.h"
#include "../h/protosw.h"
#include "../h/domain.h"
#include "../netccitt/x25.h"

/*
 *	Definitions of protocols supported in the CCITT domain.
 */

#ifdef BSD4_3
extern	struct domain ccittdomain;
#endif

#ifdef XE
int	xe_output (), xe_ctlinput (), xe_init(), xe_timer();
#endif
#ifdef HDLC
int	hd_output (), hd_ctlinput (), hd_init (), hd_timer ();
#endif
int	pk_usrreq (), pk_timer ();

struct protosw ccittsw[] = {
#ifdef XE
#ifdef BSD4_3
 {	0,		&ccittdomain,	IEEEPROTO_802LLC,0,
#else
 {	0,		PF_CCITT,	IEEEPROTO_802LLC,0,
#endif
	0,		xe_output,	xe_ctlinput,	0,
	0,
	xe_init,	0,	 	xe_timer,	0,
 },
#endif
#ifdef HDLC
#ifdef BSD4_3
 {	0,		&ccittdomain,	CCITTPROTO_HDLC,0,
#else
 {	0,		PF_CCITT,	CCITTPROTO_HDLC,0,
#endif
	0,		hd_output,	hd_ctlinput,	0,
	0,
	hd_init,	0,	 	hd_timer,	0,
 },
#endif
#ifdef BSD4_3
 {	SOCK_STREAM,	&ccittdomain,	CCITTPROTO_X25,	PR_CONNREQUIRED|PR_ATOMIC|PR_WANTRCVD,
#else
 {	SOCK_STREAM,	PF_CCITT,	CCITTPROTO_X25,	PR_CONNREQUIRED|PR_ATOMIC|PR_WANTRCVD,
#endif
	0,		0,		0,		0,
	pk_usrreq,
	0,		0,		pk_timer,	0,
 }
};

struct domain ccittdomain =
#ifdef BSD4_3
	{ AF_CCITT, "ccitt", 0, 0, 0, ccittsw,
		&ccittsw[sizeof(ccittsw)/sizeof(ccittsw[0])] };
#else
	{ AF_CCITT, "ccitt", ccittsw, &ccittsw[sizeof(ccittsw)/sizeof(ccittsw[0])] };
#endif

