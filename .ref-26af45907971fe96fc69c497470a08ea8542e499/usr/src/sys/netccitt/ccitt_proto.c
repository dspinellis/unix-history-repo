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
 *	@(#)ccitt_proto.c	7.9 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/socket.h>
#include <sys/protosw.h>
#include <sys/domain.h>

#include <netccitt/x25.h>

#include <net/radix.h>

/*
 * Definitions of protocols supported in the CCITT domain.
 */

extern	struct domain ccittdomain;
#define DOMAIN &ccittdomain

#ifdef LLC
int	llc_output();
void	llc_ctlinput(), llc_init(), llc_timer();
#endif
#ifdef HDLC
int	hd_output();
void	hd_ctlinput(), hd_init(), hd_timer();
#endif
int	pk_usrreq(), pk_ctloutput();
void	pk_timer(), pk_init(), pk_input(), pk_ctlinput();

struct protosw ccittsw[] = {
#ifdef LLC
 {	0,		DOMAIN,		IEEEPROTO_802LLC,0,
	0,		llc_output,	llc_ctlinput,	0,
	0,
	llc_init,	0,	 	llc_timer,	0,
 },
#endif
#ifdef HDLC
 {	0,		DOMAIN,		CCITTPROTO_HDLC,0,
	0,		hd_output,	hd_ctlinput,	0,
	0,
	hd_init,	0,	 	hd_timer,	0,
 },
#endif
 {	SOCK_STREAM,	DOMAIN,		CCITTPROTO_X25,	PR_CONNREQUIRED|PR_ATOMIC|PR_WANTRCVD,
	pk_input,	0,		pk_ctlinput,	pk_ctloutput,
	pk_usrreq,
	pk_init,	0,		pk_timer,	0,
 }
};

struct domain ccittdomain =
	{ AF_CCITT, "ccitt", 0, 0, 0, ccittsw,
		&ccittsw[sizeof(ccittsw)/sizeof(ccittsw[0])], 0,
		rn_inithead, 32, sizeof (struct sockaddr_x25) };
