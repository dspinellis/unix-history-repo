
/*
 * $Header: te.c,v 1.1 88/06/29 15:00:09 hagens Exp $ 
 * $Source: /usr/argo/sys/netiso/RCS/te.c,v $ 
 *
 *	EON rfc 
 *  Layer between IP and CLNL
 *
 * TODO:
 * Put together a current rfc986 address format and get the right offset
 * for the nsel
 */
#define RFC986_NSEL_OFFSET 5

#ifndef lint
static char *rcsid = "$Header: te.c,v 1.1 88/06/29 15:00:09 hagens Exp $";
#endif lint

#include "eon.h"
#if NEON>0

#include <stdio.h>

#include "param.h"
#include "systm.h"
#include "types.h"
#include "mbuf.h"
#include "buf.h"
#include "protosw.h"
#include "socket.h"
#include "ioctl.h"
#include "errno.h"
#include "types.h"

#include "machine/io.h"
#include "../machineio/ioccvar.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"

#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/ip.h"
#include "../netinet/ip_var.h"
#include "../netinet/if_ether.h"

#include "../netiso/iso.h"
#include "../netiso/argo_debug.h"
#include "../netiso/iso_errno.h"
#include "../netiso/eonvar.h"

#define EOK 0

#undef insque
#define	insque(p,q)	_insque((queue_t)q,(queue_t)p)
#define	remque(q)	_remque((queue_t)q)


struct eon_centry {
	struct qhdr eonc_q_LINK;
#define eonc_nextLINK eonc_q_LINK.link
#define eonc_prevLINK eonc_q_LINK.flink

	struct qhdr eonc_q_IS;
#define eonc_nextIS eonc_q_IS.link
#define eonc_prevIS eonc_q_IS.flink

	struct qhdr eonc_q_ES;
#define eonc_nextES eonc_q_ES.link
#define eonc_prevES eonc_q_ES.flink

	struct in_addr	eonc_addr;
	u_short		eonc_status;
};

/* kinda like mtod() but for eon_centries */
#define qtocentry(q, off)  ((struct eon_centry *)  (((caddr_t)(q)) - off))
#define centrytoq(c, off)  ((struct qhdr *)  (((caddr_t)(c)) + off))

struct qhdr 			eon_LINK_hdr = {
	(struct qhdr *)0,
	(struct qhdr *)0,
};
static struct qhdr 		eon_IS_hdr = {
	(struct qhdr *)0,
	(struct qhdr *)0,
};
static struct qhdr 		eon_ES_hdr = {
	(struct qhdr *)0,
	(struct qhdr *)0,
};
static struct qhdr 		eon_FREE_hdr = {
	(struct qhdr *)0,
	(struct qhdr *)0,
};

eon_dumpcache(which)
	int 						which;
{
	register int 				off;
	register struct eon_centry 	*ent;
	struct	qhdr				*hdr;

	switch (which) {
		case E_FREE:
			printf("FREE LIST\n");
			off = _offsetof( struct eon_centry, eonc_q_LINK);
			hdr = &eon_FREE_hdr;
			ent = qtocentry( hdr->link, 
				_offsetof( struct eon_centry, eonc_q_LINK));
			break;
		case E_ES:
			printf("ES LIST\n");
			off = _offsetof( struct eon_centry, eonc_q_ES);
			hdr = &eon_ES_hdr;
			ent = qtocentry( hdr->link, 
				_offsetof( struct eon_centry, eonc_q_ES));
			break;
		case E_IS:
			printf("IS LIST\n");
			off = _offsetof( struct eon_centry, eonc_q_IS);
			hdr = &eon_IS_hdr;
			ent = qtocentry( hdr->link, 
				_offsetof( struct eon_centry, eonc_q_IS));
			break;
		case E_LINK:
			printf("LINK LIST\n");
			off = _offsetof( struct eon_centry, eonc_q_LINK);
			hdr = &eon_LINK_hdr;
			ent = qtocentry( hdr->link, 
				_offsetof( struct eon_centry, eonc_q_LINK));
			break;
	}
	if(hdr == centrytoq(ent, off)->link )
		printf("EMPTY\n");
	else while(1) {
		printf("0x%x: %d.%d.%d.%d, %s %s\n", ent,
			(ent->eonc_addr.s_addr>>24)&0xff,
			(ent->eonc_addr.s_addr>>16)&0xff,
			(ent->eonc_addr.s_addr>>8)&0xff,
			(ent->eonc_addr.s_addr)&0xff,
			((ent->eonc_status & EON_ESLINK_UP)?"ES^":
				(ent->eonc_status & EON_ESLINK_DOWN)?"es*": "   "),
			((ent->eonc_status & EON_ISLINK_UP)?"IS^":
				(ent->eonc_status & EON_ISLINK_DOWN)?"is*": "   ")
			);
		dump_buf(ent, sizeof(struct eon_centry) );

		{ 	/* ent = ent.next: */
			register struct qhdr 	*q;

			q = centrytoq(ent, off)->link;
			if( q == hdr)
				break;
			if( q == (struct qhdr *)0) /* panic */ {
				printf("eon0: BAD Q HDR or CENTRY! q 0x%x ent 0x%x off 0x%x\n",
					q, ent, off);
				break;
			}
			ent = qtocentry( q,  off );
		}
	}
}

initq(q) 
	struct qhdr *q;
{
	q->rlink = q->link = q;
}
main()
{
	static struct eon_centry	eoncache[EON_CACHESIZE];
	register int 				i;
	register struct eon_centry 	*ent;

	initq( &eon_FREE_hdr );
	initq( &eon_LINK_hdr );
	initq( &eon_ES_hdr );
	initq( &eon_IS_hdr );

	bzero( eoncache, EON_CACHESIZE*sizeof(struct eon_centry));
	ent = eoncache;

	for(i=0; i< EON_CACHESIZE; i++,ent++) {
		insque(&eon_FREE_hdr, 
			centrytoq(ent, _offsetof( struct eon_centry, eonc_q_LINK)));
	}

	eon_dumpcache(E_FREE);
	eon_dumpcache(E_ES);
}
#endif NEON>0

#define MAX_COLUMNS 8
dump_buf(buf, len)
char	*buf;
int		len;
{
	int		i,j;

	printf("Dump buf 0x%x len 0x%x\n", buf, len);
	for (i = 0; i < len; i += MAX_COLUMNS) {
		printf("+%d:\t", i);
		for (j = 0; j < MAX_COLUMNS; j++) {
			if (i + j < len) {
				printf("%x/%d\t", buf[i+j], buf[i+j]);
			} else {
				printf("	");
			}
		}

		for (j = 0; j < MAX_COLUMNS; j++) {
			if (i + j < len) {
				if (((buf[i+j]) > 31) && ((buf[i+j]) < 128))
					printf("%c", buf[i+j]);
				else
					printf(".");
			}
		}
		printf("\n");
	}
}

_insque(new, header)
	register struct qhdr *new, *header;
{
	(*new).link = (*header).link;
	(*new).rlink = header;
	(*(*header).link).rlink = new;
	(*header).link = new;
}
