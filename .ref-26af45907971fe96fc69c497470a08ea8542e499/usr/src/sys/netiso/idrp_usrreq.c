/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)idrp_usrreq.c	7.1 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/systm.h>
#include <sys/malloc.h>
#include <sys/mbuf.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <sys/protosw.h>
#include <sys/errno.h>

#include <net/route.h>
#include <net/if.h>

#include <netiso/argo_debug.h>
#include <netiso/iso.h>
#include <netiso/clnp.h>
#include <netiso/clnl.h>
#include <netiso/iso_pcb.h>
#include <netiso/iso_var.h>

int idrp_input();
struct	isopcb	idrp_isop;
static	struct	sockaddr_iso idrp_addrs[2] =
{  { sizeof(idrp_addrs), AF_ISO, }, { sizeof(idrp_addrs[1]), AF_ISO, } };
/*
 * IDRP initialization
 */
idrp_init()
{
	extern struct clnl_protosw clnl_protox[256];

	idrp_isop.isop_next = idrp_isop.isop_prev = &idrp_isop;
	idrp_isop.isop_faddr = &idrp_isop.isop_sfaddr;
	idrp_isop.isop_laddr = &idrp_isop.isop_sladdr;
	idrp_isop.isop_sladdr = idrp_addrs[1];
	idrp_isop.isop_sfaddr = idrp_addrs[1];
	clnl_protox[ISO10747_IDRP].clnl_input = idrp_input;
}

/*
 * CALLED FROM:
 * 	tpclnp_input().
 * FUNCTION and ARGUMENTS:
 * Take a packet (m) from clnp, strip off the clnp header
 * and mke suitable for the idrp socket.
 * No return value.  
 */
idrp_input(m, src, dst)
	register struct mbuf *m;
	struct sockaddr_iso *src, *dst;
{
	if (idrp_isop.isop_socket == 0) {
	bad:	m_freem(m);
		return 0;
	}
	bzero(idrp_addrs[0].siso_data, sizeof(idrp_addrs[0].siso_data));
	bcopy((caddr_t)&(src->siso_addr), (caddr_t)&idrp_addrs[0].siso_addr,
		1 + src->siso_nlen);
	bzero(idrp_addrs[1].siso_data, sizeof(idrp_addrs[1].siso_data));
	bcopy((caddr_t)&(dst->siso_addr), (caddr_t)&idrp_addrs[1].siso_addr,
		1 + dst->siso_nlen);
	if (sbappendaddr(&idrp_isop.isop_socket->so_rcv,
		(struct sockaddr *)idrp_addrs, m, (struct mbuf *)0) == 0)
		goto bad;
	sorwakeup(idrp_isop.isop_socket);
	return 0;
}

idrp_output(m, addr)
	struct mbuf *m, *addr;
{
	register struct sockaddr_iso *siso = mtod(addr, struct sockaddr_iso *);
	int s = splnet(), i;

	bcopy((caddr_t)&(siso->siso_addr),
	      (caddr_t)&idrp_isop.isop_sfaddr.siso_addr, 1 + siso->siso_nlen);
	siso++;
	bcopy((caddr_t)&(siso->siso_addr),
	      (caddr_t)&idrp_isop.isop_sladdr.siso_addr, 1 + siso->siso_nlen);
	i = clnp_output(m, idrp_isop, m->m_pkthdr.len, 0);
	splx(s);
	return (i);
}

u_long	idrp_sendspace = 3072;		/* really max datagram size */
u_long	idrp_recvspace = 40 * 1024;	/* 40 1K datagrams */

/*ARGSUSED*/
idrp_usrreq(so, req, m, addr, control)
	struct socket *so;
	int req;
	struct mbuf *m, *addr, *control;
{
	int error = 0;

	 /* Note: need to block idrp_input while changing
	 * the udp pcb queue and/or pcb addresses.
	 */
	switch (req) {

	case PRU_ATTACH:
		if (idrp_isop.isop_socket != NULL) {
			error = ENXIO;
			break;
		}
		idrp_isop.isop_socket = so;
		error = soreserve(so, idrp_sendspace, idrp_recvspace);
		break;

	case PRU_SHUTDOWN:
		socantsendmore(so);
		break;

	case PRU_SEND:
		return (idrp_output(m, addr));

	case PRU_ABORT:
		soisdisconnected(so);
	case PRU_DETACH:
		idrp_isop.isop_socket = 0;
		break;


	case PRU_SENSE:
		/*
		 * stat: don't bother with a blocksize.
		 */
		return (0);

	default:
		return (EOPNOTSUPP);	/* do not free mbuf's */
	}

release:
	if (control) {
		printf("idrp control data unexpectedly retained\n");
		m_freem(control);
	}
	if (m)
		m_freem(m);
	return (error);
}
