/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)uipc_pipe.c	6.3 (Berkeley) %G%
 */

#include "param.h"
#include "mbuf.h"
#include "protosw.h"
#include "socket.h"
#include "socketvar.h"
#include "unpcb.h"

#define	PIPSIZ	4096

/*
 * Sneakily connect a pipe from wso to rso.
 * This will get cleaned up when socketpair is added.
 */
piconnect(wso, rso)
	struct socket *wso, *rso;
{

	/* when we reserve memory this routine may fail */
	sotounpcb(wso)->unp_conn = sotounpcb(rso);
	sotounpcb(rso)->unp_conn = sotounpcb(wso);
	wso->so_snd.sb_hiwat = PIPSIZ;
	wso->so_snd.sb_mbmax = 2*PIPSIZ;
	wso->so_state |= SS_ISCONNECTED|SS_CANTRCVMORE;
	rso->so_rcv.sb_hiwat = 0;
	rso->so_rcv.sb_mbmax = 0;
	rso->so_state |= SS_ISCONNECTED|SS_CANTSENDMORE;
	return (1);
}
