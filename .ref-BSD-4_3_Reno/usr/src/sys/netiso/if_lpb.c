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
 * $Header: if_lpb.c,v 4.2 88/06/29 14:59:38 hagens Exp $
 * $Source: /usr/argo/sys/netiso/RCS/if_lpb.c,v $
 *
 *	LOOPBACK driver that mimics the
 *	Eicon x.25 board for use by CONS
 */

#ifndef lint
static char *rcsid = "$Header: if_lpb.c,v 4.2 88/06/29 14:59:38 hagens Exp $";
#endif lint


#include "param.h"
#include "systm.h"
#include "types.h"
#include "mbuf.h"
#include "buf.h"
#include "protosw.h"
#include "socket.h"
#include "ioctl.h"
#include "errno.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"
#include "machine/io.h"
#include "../machineio/ioccvar.h"

#include "ecn.h"
#include "iso.h"
#include "argo_debug.h"
#include "../caif/eicon.h"
#include "iso_errno.h"

#define LPB_DEBUG
#ifdef LPB_DEBUG
#define MT_LPB_OPEN	0x55
#define MT_LPB_ACK 	0x56
#else LPB_DEBUG
#define MT_LPB_DATA MT_DATA
#define MT_LPB_ACK MT_DATA
#endif LPB_DEBUG

extern struct ifqueue 	consintrq;	
int						lpboutput();

/* These next 2 declarations are for Logical Unit Numbers - i.e. VC # -
 * the 2I assigns and frees them; we have to fake it
 */

static u_char 			free_luns[32];
static u_char 			*next_lun = free_luns;

/*
 * Initialize all LUNs as available for use.
 */
init_lpb()
{
	register int i;

	for (i = 0; i < 32; i++) {
		free_luns[i] = i+1;
	}
	next_lun = free_luns;
}

/*
 *	Allocate new logical unit number.
 *  Allocating number n means that both n and -n are allocated & used.
 *  NOTE: next_lun always points to an UNALLOCATED lun, hence
 *	 take a lun then increment, or decrement then stash the lun.
 */

u_char 
getlun()
{
	if( ((next_lun) - free_luns) > 32 ) {
		printf("PANIC: if_lpb: too many channels! \n");
		return 0;
	}
	IFDEBUG(D_CCONN)
		printf("getlun: returns 0x%x\n", *next_lun);
	ENDDEBUG
	ASSERT( *next_lun != 0 );
	if( *next_lun == 0 ) {
		register int i;

		printf(
		"PANIC IN lpb: free_luns 0x%x next_len 0x%x *next_lun 0x%x\n",
			free_luns, next_lun, *next_lun);
		
		for(i=0; i<32; i++) {
			printf("free_luns[ 0x%x ] = 0x%x\n", i, free_luns[i] );
		}
	}
	return *(next_lun++);

}

/* 
 * When you free one you free its neg 
 */

freelun(lun)
u_char	lun;
{
	IFDEBUG(D_CCONN)
		printf("freelun(0x%x)\n", lun);
	ENDDEBUG
	if( lun > 32 )
		return;

	ASSERT( (lun & 0xc0) == 0 );
	ASSERT( lun <= 32 );

	/* check for lun already in the list */
	{
		register u_char *r = next_lun;

		while( (int)(r - free_luns) <= 32 ) {
			if( *r == lun ) {
				return;
			}
			r++;
		}
	}

	next_lun --;
	*next_lun = lun;
}


/*
 * FUNCTION:		lpboutput
 *
 * PURPOSE:			Process an eicon x.25 request from a higher layer
 *					protocol.
 * ARGUMENTS:	 	(ifp) is points to the ifnet structure for this unit/device
 *					(m)  is an mbuf *, *m is an eicon request structure
 *
 * RETURNS:			unix error code
 *
 * NOTES:			Mimics the eicon driver.
 *
 */
lpboutput(ifp,m)
	register struct ifnet 	*ifp;
	register struct mbuf	*m;			/* request */
{
	int							s;
	struct eicon_request		*req;
	int							error = 0;

	/* Do this even if (ifp->if_flags & IFF_LOOPBACK) == 0
	 * because whether or not a vc is on loopback is determined
	 * at the time of connection establishement.
	 */
	s = splnet();
	req = mtod(m, struct eicon_request *);
	IFDEBUG(D_CDUMP_REQ)
		dump_buf(req, sizeof(struct eicon_request));
	ENDDEBUG
	switch (req->e_cmd) {
		case ECN_CALL: {
			/*
			 *	ECN_CALL	->	ECN_ACCEPT (for orig CONNECT)
			 *				->	ECN_CONNECT	(other side's connect indication)
			 */
			struct mbuf *mdata;
			struct mbuf *mopen;
			struct eicon_request *open;

			MGET(mopen, M_DONTWAIT, MT_LPB_OPEN);
			if (mopen == NULL) {
				printf("No mbufs for copy\n");
				error = ENOBUFS;
				break;
			}
			mopen->m_len = sizeof(struct eicon_request);

			open = mtod(mopen, struct eicon_request *);
			bcopy( req, open, sizeof(struct eicon_request) );

			/* get mbuf for the connect data */
			MGET(mdata, M_DONTWAIT, MT_LPB_OPEN);
			if (mdata == NULL) {
				printf("No mbufs for copy\n");
				error = ENOBUFS;
				break;
			}
			mdata->m_len = (e_data(req))->m_len;
			e_data(open) = mdata; /* e_data is really mtod(open)->m_next */
			/* make a copy of the connect data */
			IFDEBUG(D_CCONN)
				printf("bcopy( 0x%x, 0x%x, 0x%x)\n", mtod(e_data(req), caddr_t),
						mtod(mdata, caddr_t), 
						(e_data(req))->m_len);
			ENDDEBUG
			bcopy( mtod(e_data(req), caddr_t), mtod(mdata, caddr_t), 
						(e_data(req))->m_len);
			/* setup call */
			open->e_cmd = ECN_CONNECT;
			open->e_vc = getlun();

			/* setup call confirm */
			req->e_cmd = ECN_ACCEPT;
			req->e_vc = -(open->e_vc);

			IFDEBUG(D_CDUMP_REQ)
				printf("lpboutput CALL middle \n");
			ENDDEBUG

			if (IF_QFULL(&consintrq)) {
				IF_DROP(&consintrq);
				m_freem(mopen);
				printf("lpboutput: response dropped\n");
				error = ENOBUFS;
				break;
			} else {
				/* connect */
				IFDEBUG(D_CCONS);
					printf("CONNECT 0x%x --> X25INTRQ\n", mopen);
				ENDDEBUG
				IF_ENQUEUE(&consintrq, mopen);
				IFDEBUG(D_CDUMP_REQ);
					dump_buf(open, sizeof(struct eicon_request));
				ENDDEBUG

				/* confirm */
				IFDEBUG(D_CCONS);
					printf("CONFIRM 0x%x (data 0x%x =?= 0x%x) --> X25INTRQ\n", 
						m, m->m_next, e_data(req));
				ENDDEBUG
				IF_ENQUEUE(&consintrq, m);
				IFDEBUG(D_CDUMP_REQ);
					dump_buf(req, sizeof(struct eicon_request));
				ENDDEBUG
			}
		} break;
		
		case ECN_RESET: 
		case ECN_CLEAR: {
			/*
			 *	ECN_RESET	->	ECN_RESET	(other side's reset indication)
			 *	ECN_CLEAR	->	ECN_CLEAR	(other side's close indication)
			 * TODO: MAY HAVE DATA PACKET! 
			 * TODO: Have to be able to handle a 2nd CLEAR on on vc!
			 */
			freelun(req->e_vc);
			freelun((-req->e_vc)&0xff);
			req->e_vc = -req->e_vc; /* other side */
			req->e_reason = E_CO_PDNCLRESET;
			if (IF_QFULL(&consintrq)) {
				IF_DROP(&consintrq);
				printf("lpboutput: respose dropped\n");
				error = ENOBUFS;
			} else {
				IFDEBUG(D_CCONS);
					printf("CLOSE 0x%x --> X25INTRQ\n", m);
				ENDDEBUG
				IF_ENQUEUE(&consintrq, m);
				IFDEBUG(D_CDUMP_REQ);
					dump_buf(req, sizeof(struct eicon_request));
				ENDDEBUG
			}
		} break;
		
		case ECN_SEND: {
			/*
			 *	ECN_SEND 	->	ECN_RECEIVE	(data send becomes data recvd)
			 */
			struct mbuf *m0;
			struct eicon_request *ack;

			MGET(m0, M_DONTWAIT, MT_LPB_ACK); /* sets type, next, off */
			if (m0 == NULL) {
				printf("PANIC No mbufs for copy\n");
				error = ENOBUFS;
				break;
			}
			m0->m_len = sizeof(struct eicon_request);

			ack = mtod(m0, struct eicon_request *);
			/* setup ack */
			ack->e_cmd = ECN_ACK;
			ack->e_vc = req->e_vc;
			req->e_vc = -req->e_vc;
			req->e_cmd = ECN_RECEIVE;

			if (IF_QFULL(&consintrq)) {
				IF_DROP(&consintrq);
				printf("lpboutput: ADR_ACK DROPPED\n");
				m_freem(m0);
				error = ECONNABORTED;
			} else {
				IF_ENQUEUE(&consintrq, m);
				IFDEBUG(D_CCONS);
					printf("DATA 0x%x --> X25INTRQ\n", m);
				ENDDEBUG
				IFDEBUG(D_CDUMP_REQ);
					dump_buf(req, sizeof(struct eicon_request));
				ENDDEBUG
				IFDEBUG(D_CCONS);
					printf("ACK 0x%x --> X25INTRQ\n", m0);
				ENDDEBUG
				IF_ENQUEUE(&consintrq, m0);
				IFDEBUG(D_CDUMP_REQ);
					dump_buf(ack, sizeof(struct eicon_request));
				ENDDEBUG
			}
		} break;
		
		default:
			printf("Bad loopback request 0x%x\n", req->e_cmd);
			error = EINVAL;
	}

	if( error ) {
		m_freem(m);
	} else
		schednetisr(NETISR_X25);

	splx(s);
	return error;
}

#if NECN>0
	/* nothing */
#else

/* KLUDGE: when no ecn board config-ed in, we need a routing
 * ecnifp to return null.  We want to be able to configure with
 * sw loopback only.
 */
struct ifnet  *
ecnifp(unit)
int unit;
{
	return (struct ifnet *)NULL;
}

int
ecnoutput(ifp, m)
	struct ifnet *ifp;
	struct mbuf *m;
{
	printf("ecnoutput: ecn not configured\n");
	(void) m_freem(m);
	return ENETDOWN;
		
}

ecnshutdown(ifp)
{
	printf("ecnshutdown: ecn not configured\n");
}

ecnrestart(ifp)
{
	printf("ecnrestart: ecn not configured\n");
}
#endif NECN>0
