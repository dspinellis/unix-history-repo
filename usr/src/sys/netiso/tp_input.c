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
 * ARGO TP
 *
 * $Header: tp_input.c,v 5.6 88/11/18 17:27:38 nhall Exp $
 * $Source: /usr/argo/sys/netiso/RCS/tp_input.c,v $
 *
 * tp_input() gets an mbuf chain from ip.  Actually, not directly
 * from ip, because ip calls a net-level routine that strips off
 * the net header and then calls tp_input(), passing the proper type
 * of addresses for the address family in use (how it figures out
 * which AF is not yet determined.
 *
 * Decomposing the tpdu is some of the most laughable code.  The variable-length
 * parameters and the problem of non-aligned memory references
 * necessitates such abominations as the macros WHILE_OPTIONS (q.v. below)
 * to loop through the header and decompose it.
 *
 * The routine tp_newsocket() is called when a CR comes in for a listening
 * socket.  tp_input calls sonewconn() and tp_newsocket() to set up the
 * "child" socket.  Most tpcb values are copied from the parent tpcb into
 * the child.
 * 
 * Also in here is tp_headersize() (grot) which tells the expected size
 * of a tp header, to be used by other layers.  It's in here because it
 * uses the static structure tpdu_info.
 */

#ifndef lint
static char *rcsid = "$Header: tp_input.c,v 5.6 88/11/18 17:27:38 nhall Exp $";
#endif lint

#include "argoxtwentyfive.h"
#include "param.h"
#include "mbuf.h"
#include "socket.h"
#include "socketvar.h"
#include "domain.h"
#include "protosw.h"
#include "errno.h"
#include "time.h"
#include "kernel.h"
#include "types.h"
#include "../netiso/iso_errno.h"
#include "../netiso/tp_param.h"
#include "../netiso/tp_timer.h"
#include "../netiso/tp_stat.h"
#include "../netiso/tp_pcb.h"
#include "../netiso/argo_debug.h"
#include "../netiso/tp_trace.h"
#include "../netiso/tp_tpdu.h"
#include "../netiso/iso.h"
#include "../netiso/cons.h"

int 	iso_check_csum(), tp_driver(), tp_headersize(), tp_error_emit();

#ifdef lint
#undef ATTR
#define ATTR(X)ev_number
#endif lint

struct mbuf *
tp_inputprep(m) 
	struct mbuf *m;
{
	struct tpdu *hdr;

	IFDEBUG(D_TPINPUT)
		printf("tp_inputprep: m 0x%x\n") ;
	ENDDEBUG

	while(  m->m_len < 1 ) {
		if( (m = m_free(m)) == MNULL ) {
			return (struct mbuf *)0;
		}
	}

	if(m->m_off & 0x3) {
		/* align to a 4-byte boundary - sigh */
		register struct mbuf *n;

		MGET(n, M_DONTWAIT, m->m_type);
		if( n == MNULL ) {
			m_freem(m);
			return (struct mbuf *)0;
		}
		n->m_act = MNULL;
		n->m_len = m->m_len;
		n->m_next = m->m_next;
		bcopy( mtod(m, caddr_t), mtod(n, caddr_t), m->m_len );
		m->m_next = 0;
		m_free(m);
		m = n;
	}
	CHANGE_MTYPE(m, TPMT_DATA);

	/* we KNOW that there is at least 1 byte in this mbuf */

	hdr = mtod( m, struct tpdu *);

	/*
	 * now pull up the whole tp header 
	 */
	if ( m->m_len < hdr->tpdu_li + 1 ) {
		if ((m = m_pullup(m, (int)(hdr->tpdu_li) + 1)) == MNULL ) {
			IncStat(ts_recv_drop);
			return (struct mbuf *)0;
		}
	}
	IFDEBUG(D_INPUT)
	printf(
	" at end: m 0x%x hdr->tpdu_li 0x%x m_len 0x%x\n",m,
		hdr->tpdu_li,m->m_len);
	ENDDEBUG
	return m;
}

/* begin groan
 * -- this array and the following macros allow you to step through the
 * parameters of the variable part of a header
 * note that if for any reason the values of the **_TPDU macros (in tp_events.h)
 * should change, this array has to be rearranged
 */

#define TP_LEN_CLASS_0_INDEX	2
#define TP_MAX_DATA_INDEX 3

static u_char tpdu_info[][4] =
{
/*								length						 max data len */
/*								reg fmt 	xtd fmt  class 0  		 	  */
 	/* UNUSED		0x0 */		0x0 ,		0x0,	0x0,		0x0,
 	/* XPD_TPDU_type 0x1 */		0x5,		0x8,	0x0,		TP_MAX_XPD_DATA,
 	/* XAK_TPDU_type 0x2 */		0x5 ,		0x8,	0x0,		0x0,
 	/* GR_TPDU_type	0x3 */		0x0 ,		0x0,	0x0,		0x0,
 	/* UNUSED		0x4 */		0x0 ,		0x0,	0x0,		0x0,
 	/* UNUSED		0x5 */		0x0 ,		0x0,	0x0,		0x0,
 	/* AK_TPDU_type 0x6 */		0x5,		0xa,	0x0,		0x0,
	/* ER_TPDU_type 0x7 */		0x5,		0x5,	0x0,		0x0,
 	/* DR_TPDU_type 0x8 */		0x7,		0x7,	0x7,		TP_MAX_DR_DATA,
 	/* UNUSED		0x9 */		0x0 ,		0x0,	0x0,		0x0,
 	/* UNUSED		0xa */		0x0 ,		0x0,	0x0,		0x0,
 	/* UNUSED		0xb */		0x0 ,		0x0,	0x0,		0x0,
 	/* DC_TPDU_type 0xc */		0x6,		0x6,	0x0,		0x0,
 	/* CC_TPDU_type 0xd */		0x7,		0x7,	0x7,		TP_MAX_CC_DATA,
 	/* CR_TPDU_type 0xe */		0x7,		0x7,	0x7,		TP_MAX_CR_DATA,
 	/* DT_TPDU_type 0xf */		0x5,		0x8,	0x3,		0x0,
};

/* 
 * WHENEVER YOU USE THE FOLLOWING MACRO,
 * BE SURE THE TPDUTYPE IS A LEGIT VALUE FIRST! 
 */

#define WHILE_OPTIONS(P, hdr,format)\
{	register  caddr_t		P;\
	P = (caddr_t)(hdr) +\
	tpdu_info[(hdr)->tpdu_type][(format)];\
	while( P < (caddr_t)(hdr) + (int)((hdr)->tpdu_li) ) {

#define END_WHILE_OPTIONS(P)\
	P = P  + 2 + (int)((struct tp_vbp *)P)->tpv_len ;\
} }

#define CHECK(Phrase, Erval, Stat, Whattodo, Loc)\
	if(Phrase) { error = (Erval); errloc = (caddr_t)(Loc); IncStat(Stat); \
	goto Whattodo; }

/* end groan */

/*
 * NAME:  tp_newsocket()
 *
 * CALLED FROM:
 *  tp_input() on incoming CR, when a socket w/ the called suffix
 * is awaiting a  connection request
 *
 * FUNCTION and ARGUMENTS:
 *  Create a new socket structure, attach to it a new transport pcb,
 *  using a copy of the net level pcb for the parent socket.
 *  (so) is the parent socket.
 *  (fname) is the foreign address (all that's used is the nsap portion)
 *
 * RETURN VALUE:
 *  a new socket structure, being this end of the newly formed connection.
 *
 * SIDE EFFECTS:
 *  Sets a few things in the tpcb and net level pcb
 *
 * NOTES:
 */
static struct socket *
tp_newsocket(so, fname, cons_channel, class_to_use, netservice)
	struct socket				*so;
	struct sockaddr				*fname;
	u_int						cons_channel;
	u_char						class_to_use;
	u_int						netservice;
{
	register struct tp_pcb	*tpcb = sototpcb(so); /* old tpcb, needed below */
	struct tp_pcb *			 newtpcb;
	struct proc *			selproc = so->so_rcv.sb_sel; /* kludge for select */

	/* 
	 * sonewconn() gets a new socket structure,
	 * a new lower layer pcb and a new tpcb,
	 * but the pcbs are unnamed (not bound)
	 */
	IFTRACE(D_NEWSOCK)
		tptraceTPCB(TPPTmisc, "newsock: listg_so,_tpcb selproc, so_head",
			so, tpcb, selproc, so->so_head);
	ENDTRACE	

	if ((so = sonewconn(so)) == (struct socket *)0)
		return so;
	IFTRACE(D_NEWSOCK)
		tptraceTPCB(TPPTmisc, "newsock: after newconn so, selproc, so_head",
			so, selproc, so->so_head, 0);
	ENDTRACE	

	so->so_rcv.sb_sel = selproc; /* so that soisconnected() after receipt
		* of the ack will wake this guy up if he's selecting on the
		* listening socket
		*/
	IFDEBUG(D_NEWSOCK)
		printf("tp_newsocket(channel 0x%x)  after sonewconn so 0x%x \n", so);
		dump_isoaddr(fname);
		{ 
			struct socket *t, *head ;

			head = so->so_head;
			t = so;
			printf("so 0x%x so_head 0x%x so_q0 0x%x, q0len %d\n",
					t, t->so_head, t->so_q0, t->so_q0len);
			while( (t=t->so_q0)  && t!= so  && t!= head)
				printf("so 0x%x so_head 0x%x so_q0 0x%x, q0len %d\n",
					t, t->so_head, t->so_q0, t->so_q0len);
		}
	ENDDEBUG

	/* 
	 * before we clobber the old tpcb ptr, get these items from the parent pcb 
	 */
	newtpcb = sototpcb(so);
	newtpcb->_tp_param = tpcb->_tp_param;
	newtpcb->tp_flags = tpcb->tp_flags;
	newtpcb->tp_lcredit = tpcb->tp_lcredit;
	newtpcb->tp_l_tpdusize = tpcb->tp_l_tpdusize;
	newtpcb->tp_lsuffixlen = tpcb->tp_lsuffixlen;
	bcopy( tpcb->tp_lsuffix, newtpcb->tp_lsuffix, newtpcb->tp_lsuffixlen);
	soreserve(so, tpcb->tp_winsize, tpcb->tp_winsize);

	if( /* old */ tpcb->tp_flags & (TPF_CONN_DATA_OUT | TPF_DISC_DATA_OUT )) {
		/* 
		 * Flags has already been copied, now copy the data 
		 * -- these data are the connect- or disconnect- data.
		 */
		struct mbuf *conndata;

		ASSERT(tpcb->tp_sock->so_snd.sb_mb != MNULL);
		ASSERT(tpcb->tp_sock->so_snd.sb_cc != 0);
		conndata = m_copy( tpcb->tp_sock->so_snd.sb_mb, 0,
									tpcb->tp_sock->so_snd.sb_cc);
		IFDEBUG(D_CONN)
			dump_mbuf(conndata, "conndata after mcopy");
			dump_mbuf(tpcb->tp_sock->so_snd.sb_mb, "old sosnd after mcopy");
			dump_mbuf(so->so_snd.sb_mb, "new (so->)sosnd before sbapndrec");
			dump_mbuf(conndata, "conndata before sbappendrec");
		ENDDEBUG
		sbappendrecord( &so->so_snd, conndata );
	}

	tpcb = newtpcb;
	tpcb->tp_state = TP_LISTENING;
	tpcb->tp_class = class_to_use;
	tpcb->tp_netservice = netservice;


	ASSERT( fname != 0 ) ; /* just checking */
	if ( fname ) {
		/*
		 *	tp_route_to takes its address argument in the form of an mbuf.
		 */
		struct mbuf	*m;
		int			err;

		MGET(m, M_DONTWAIT, MT_SONAME);	/* mbuf type used is confusing */
		if (m) {
			/*
			 * this seems a bit grotesque, but tp_route_to expects
			 * an mbuf * instead of simply a sockaddr; it calls the ll
			 * pcb_connect, which expects the name/addr in an mbuf as well.
			 * sigh.
			 */
			bcopy((caddr_t)fname, mtod(m, caddr_t), sizeof (struct sockaddr));
			m->m_act = MNULL;
			m->m_len = (fname->sa_family == AF_INET) ? 
				sizeof(struct sockaddr_in) : sizeof(struct sockaddr_iso);

			/* grot  : have to say the kernel can override params in
			 * the passive open case
			 */
			tpcb->tp_dont_change_params = 0;
			err = tp_route_to( m, tpcb, cons_channel);
			m_free(m);

			if (!err)
				goto ok;
		}
		IFDEBUG(D_CONN)
			printf("tp_route_to FAILED! detaching tpcb 0x%x, so 0x%x\n",
				tpcb, so);
		ENDDEBUG
		(void) tp_detach(tpcb); 
		return 0;
	}
ok:
	IFDEBUG(D_TPINPUT)
		printf("tp_newsocket returning so 0x%x, sototpcb(so) 0x%x\n",
			so, sototpcb(so));
	ENDDEBUG
	return so;
}

#ifndef CONS
tpcons_output()
{
	return(0);
}
#endif !CONS

/* 
 * NAME: 	tp_input()
 *
 * CALLED FROM:
 *  net layer input routine
 *
 * FUNCTION and ARGUMENTS:
 *  Process an incoming TPDU (m), finding the associated tpcb if there
 *  is one. Create the appropriate type of event and call the driver.
 *  (faddr) and (laddr) are the foreign and local addresses.
 * 
 * 	When tp_input() is called we KNOW that the ENTIRE TP HEADER
 * 	has been m_pullup-ed.
 *
 * RETURN VALUE: Nada
 *  
 * SIDE EFFECTS:
 *	When using COSNS it may affect the state of the net-level pcb
 *
 * NOTE:
 *  The initial value of acktime is 2 so that we will never
 *  have a 0 value for tp_peer_acktime.  It gets used in the
 *  computation of the retransmission timer value, and so it
 *  mustn't be zero.
 *  2 seems like a reasonable minimum.
 */
ProtoHook
tp_input(m, faddr, laddr, cons_channel, dgout_routine)
	register	struct mbuf 	*m;
	struct sockaddr 			*faddr, *laddr; /* NSAP addresses */
	u_int 						cons_channel;
	int 						(*dgout_routine)();

{
	register struct tp_pcb 	*tpcb = (struct tp_pcb *)0;
	register struct tpdu 	*hdr = mtod(m, struct tpdu *);
	struct socket 			*so;
	struct tp_event 		e;
	int 					error = 0;
	unsigned 				dutype;
	u_short 				dref, sref, acktime, subseq; /*VAX*/
	u_char 					preferred_class=0, class_to_use=0;
	u_char					opt, dusize, addlopt;
#ifdef TP_PERF_MEAS
	u_char					perf_meas=0;
#endif TP_PERF_MEAS
	u_char					fsufxlen;
	u_char					lsufxlen;
	caddr_t					fsufxloc=0, lsufxloc=0;
	int						tpdu_len;
	u_int 					takes_data;
	u_int					fcc_present; 
	caddr_t					errloc=0;
	struct tp_conn_param 	tpp;
	int						tpcons_output();

#ifdef TP_PERF_MEAS
	GET_CUR_TIME( &e.e_time );
#endif TP_PERF_MEAS
	
	IFDEBUG(D_TPINPUT)
		printf("tp_input(0x%x, ... 0x%x)\n", m, cons_channel);
	ENDDEBUG

again:

	tpdu_len = 0;
	tpcb = (struct tp_pcb *)0;
	fsufxlen = 0;
	lsufxlen = 0;
	addlopt = 0;
	acktime = 2;
	dusize = TP_DFL_TPDUSIZE;
	sref = 0;
	subseq = 0;
	takes_data = FALSE;
	fcc_present = FALSE;

	/* 
	 * get the actual tpdu length - necessary for monitoring
	 * and for checksumming
	 * 
	 * Also, maybe measure the mbuf chain lengths and sizes.
	 */

	{ 	register struct mbuf *n=m;
#	ifdef ARGO_DEBUG
		int chain_length = 0;
#	endif ARGO_DEBUG

		for(;;) {
			tpdu_len += n->m_len;
			IFDEBUG(D_MBUF_MEAS)
				if( n->m_off > MMAXOFF) {
					IncStat(ts_mb_cluster);
				} else {
					IncStat(ts_mb_small);
				}
				chain_length ++;
			ENDDEBUG
			if (n->m_next == MNULL ) {
				break;
			}
			n = n->m_next;
		}
		IFDEBUG(D_MBUF_MEAS)
			if(chain_length > 16)
				chain_length = 0; /* zero used for anything > 16 */
			tp_stat.ts_mb_len_distr[chain_length] ++;
		ENDDEBUG
	}
	IFTRACE(D_TPINPUT)
		tptraceTPCB(TPPTtpduin, hdr->tpdu_type, hdr, hdr->tpdu_li+1, tpdu_len, 
			0);
	ENDTRACE

	dref = ntohs((short)hdr->tpdu_dref);
	sref = ntohs((short)hdr->tpdu_sref);
	dutype = (int)hdr->tpdu_type;

	IFDEBUG(D_TPINPUT)
		printf("input: dutype 0x%x cons_channel 0x%x dref 0x%x\n", dutype,
			cons_channel, dref);
		printf("input: dref 0x%x sref 0x%x\n", dref, sref);
	ENDDEBUG
	IFTRACE(D_TPINPUT)
		tptrace(TPPTmisc, "channel dutype dref ", 
			cons_channel, dutype, dref, 0);
	ENDTRACE


#ifdef ARGO_DEBUG
	if( (dutype < TP_MIN_TPDUTYPE) || (dutype > TP_MAX_TPDUTYPE)) {
		printf("BAD dutype! 0x%x, channel 0x%x dref 0x%x\n",
			dutype, cons_channel, dref);
		dump_buf (m, sizeof( struct mbuf ));

		IncStat(ts_inv_dutype);
		goto discard;
	}
#endif ARGO_DEBUG

	CHECK( (dutype < TP_MIN_TPDUTYPE || dutype > TP_MAX_TPDUTYPE),
		E_TP_INV_TPDU, ts_inv_dutype, respond, 
		2 );
		/* unfortunately we can't take the address of the tpdu_type field,
		 * since it's a bit field - so we just use the constant offset 2
		 */

	/* Now this isn't very neat but since you locate a pcb one way
	 * at the beginning of connection establishment, and by
	 * the dref for each tpdu after that, we have to treat CRs differently
	 */
	if ( dutype == CR_TPDU_type ) {
		u_char alt_classes = 0;

#ifdef notdef  /* This is done up above */
		sref = hdr->tpdu_CRsref;
#endif notdef
		preferred_class = (1 << hdr->tpdu_CRclass);
		opt = hdr->tpdu_CRoptions;

		WHILE_OPTIONS(P, hdr, 1 ) /* { */

			switch( vbptr(P)->tpv_code ) {

			case	TPP_tpdu_size: 		
				vb_getval(P, u_char, dusize);
				IFDEBUG(D_TPINPUT)
					printf("CR dusize 0x%x\n", dusize);
				ENDDEBUG
				CHECK( (dusize < TP_MIN_TPDUSIZE || dusize > TP_MAX_TPDUSIZE),
						E_TP_INV_PVAL, ts_inv_pval, respond,
						(1 + (caddr_t)&vbptr(P)->tpv_val - P) )
				break;
			case	TPP_addl_opt:
				vb_getval(P, u_char, addlopt);
				break;
			case	TPP_calling_sufx:
				/* could use vb_getval, but we want to save the loc & len
				 * for later use
				 */
				fsufxloc = (caddr_t) &vbptr(P)->tpv_val;
				fsufxlen = vbptr(P)->tpv_len;
				IFDEBUG(D_TPINPUT)
					printf("CR fsufx:");
					{ register int j;
						for(j=0; j<fsufxlen; j++ ) {
							printf(" 0x%x. ", *((caddr_t)(fsufxloc+j)) );
						}
						printf("\n");
					}
				ENDDEBUG
				break;
			case	TPP_called_sufx:
				/* could use vb_getval, but we want to save the loc & len
				 * for later use
				 */
				lsufxloc = (caddr_t) &vbptr(P)->tpv_val;
				lsufxlen = vbptr(P)->tpv_len;
				IFDEBUG(D_TPINPUT)
					printf("CR lsufx:");
					{ register int j;
						for(j=0; j<lsufxlen; j++ ) {
							printf(" 0x%x. ", *((caddr_t)(lsufxloc+j)) );
						}
						printf("\n");
					}
				ENDDEBUG
				break;

#ifdef TP_PERF_MEAS
			case	TPP_perf_meas:
				vb_getval(P, u_char, perf_meas);
				break;
#endif TP_PERF_MEAS

			case	TPP_vers:
				/* not in class 0; 1 octet; in CR_TPDU only */
				CHECK( (vbval(P, u_char) != TP_VERSION ), 
					E_TP_INV_PVAL, ts_inv_pval, respond,
					(1 + (caddr_t)&vbptr(P)->tpv_val - P) )
				break;
			case	TPP_acktime:
				vb_getval(P, u_short, acktime);
				acktime = ntohs(acktime);
				acktime = acktime/500; /* convert to slowtimo ticks */
				if((short)acktime <=0 )
					acktime = 2; /* don't allow a bad peer to screw us up */
				IFDEBUG(D_TPINPUT)
					printf("CR acktime 0x%x\n", acktime);
				ENDDEBUG
				break;

			case	TPP_alt_class:
				{
					u_char *aclass = 0;
					register int i;

					for (i = ((struct tp_vbp *)P)->tpv_len; i>0; i--) {
						aclass = 
							(u_char *) &(((struct tp_vbp *)P)->tpv_val);
						alt_classes |= (1<<(*aclass));
					}
					IFDEBUG(D_TPINPUT)
						printf("alt_classes 0x%x\n", alt_classes);
					ENDDEBUG
				}
				break;

			case	TPP_security:
			case	TPP_residER:
			case	TPP_priority:
			case	TPP_transdelay:
			case	TPP_throughput: 
			case	TPP_addl_info: 
			case	TPP_subseq:
				IFDEBUG(D_TPINPUT)
					printf("param ignored CR_TPDU code= 0x%x\n",
						 vbptr(P)->tpv_code);
				ENDDEBUG
				IncStat(ts_param_ignored);
				break;

			case	TPP_checksum:		
				IFDEBUG(D_TPINPUT)
					printf("CR before cksum\n");
				ENDDEBUG

				CHECK( iso_check_csum(m, tpdu_len), 
					E_TP_INV_PVAL, ts_bad_csum, discard, 0)

				IFDEBUG(D_TPINPUT)
					printf("CR before cksum\n");
				ENDDEBUG
				break;

			default:
				IncStat(ts_inv_pcode);
				error = E_TP_INV_PCODE;
				goto discard;

			}

		/* } */ END_WHILE_OPTIONS(P)

		if( lsufxlen == 0) {
			/* can't look for a tpcb w/o any called sufx */
			error =  E_TP_LENGTH_INVAL;
			IncStat(ts_inv_sufx);
			goto respond;
		} else {
			register	struct tp_ref 	*rp;
			register	int			r;
			extern		int			tp_maxrefopen;

			rp = &tp_ref[1]; /* zero-th one is never open */
			for(  r=1 ; (r <= tp_maxrefopen) ; r++,rp++ ) {
				if (rp->tpr_state!=REF_OPENING) 
					continue;
				if ( bcmp(lsufxloc, rp->tpr_pcb->tp_lsuffix, lsufxlen)==0 ) {
					tpcb =  rp->tpr_pcb;
					if( laddr->sa_family !=
							tpcb->tp_sock->so_proto->pr_domain->dom_family ) {
						IFDEBUG(D_CONN)
						 	printf(
					"MISMATCHED AF on CR! laddr.family 0x%x expected 0x%x\n",
							laddr->sa_family, 
							tpcb->tp_sock->so_proto->pr_domain->dom_family );
						ENDDEBUG
						continue;
					}
					IFTRACE(D_TPINPUT)
						tptrace(TPPTmisc, "tp_input: ref *lsufxloc refstate", 
							r, *lsufxloc, rp->tpr_state, 0);
					ENDTRACE
					/* found it */
					break;
				}
			}

			CHECK( (r > tp_maxrefopen), E_TP_NO_SESSION, ts_inv_sufx, respond,
				(1 + 2 + (caddr_t)&hdr->_tpduf - (caddr_t)hdr))
				/* _tpduf is the fixed part; add 2 to get the dref bits of 
				 * the fixed part (can't take the address of a bit field) 
				 */
		}

		/* 
		 * WE HAVE A TPCB 
		 * already know that the classes in the CR match at least
		 * one class implemented, but we don't know yet if they
		 * include any classes permitted by this server.
		 */

		IFDEBUG(D_TPINPUT)
			printf("HAVE A TPCB 1: 0x%x\n", tpcb);
		ENDDEBUG
		IFDEBUG(D_CONN)
			printf(
"CR: bef CHKS: flags 0x%x class_to_use 0x%x alt 0x%x opt 0x%x tp_class 0x%x\n", 
				tpcb->tp_flags, class_to_use, alt_classes, opt, tpcb->tp_class);
		ENDDEBUG
		/* tpcb->tp_class doesn't include any classes not implemented  */
		class_to_use = (preferred_class & tpcb->tp_class);
		if( (class_to_use = preferred_class & tpcb->tp_class) == 0 )
			class_to_use = alt_classes & tpcb->tp_class;

		class_to_use = 1 << tp_mask_to_num(class_to_use);

		{
			tpp = tpcb->_tp_param;
			tpp.p_class = class_to_use;
			tpp.p_tpdusize = dusize;
			tpp.p_xtd_format = (opt & TPO_XTD_FMT) == TPO_XTD_FMT;
			tpp.p_xpd_service = (addlopt & TPAO_USE_TXPD) == TPAO_USE_TXPD;
			tpp.p_use_checksum = (tpp.p_class == TP_CLASS_0)?0:
				(addlopt & TPAO_NO_CSUM) == 0;
#ifdef notdef
			tpp.p_use_efc = (opt & TPO_USE_EFC) == TPO_USE_EFC;
			tpp.p_use_nxpd = (addlopt & TPAO_USE_NXPD) == TPAO_USE_NXPD;
			tpp.p_use_rcc = (addlopt & TPAO_USE_RCC) == TPAO_USE_RCC;
#endif notdef

		CHECK(
			tp_consistency(tpcb, 0 /* not force or strict */, &tpp) != 0, 
			E_TP_NEGOT_FAILED, ts_negotfailed, respond,
			(1 + 2 + (caddr_t)&hdr->_tpdufr.CRCC - (caddr_t)hdr) 
				/* ^ more or less the location of class */
			)
		}
		IFTRACE(D_CONN)
			tptrace(TPPTmisc, 
				"after 1 consist class_to_use class, out, tpconsout",
				class_to_use, 
				tpcb->tp_class, dgout_routine, tpcons_output
				);
		ENDTRACE
		CHECK(
			((class_to_use == TP_CLASS_0)&&(dgout_routine != tpcons_output)),
			E_TP_NEGOT_FAILED, ts_negotfailed, respond,
			(1 + 2 + (caddr_t)&hdr->_tpdufr.CRCC - (caddr_t)hdr) 
				/* ^ more or less the location of class */
			)
		IFDEBUG(D_CONN)
			printf("CR: after CRCCCHECKS: tpcb 0x%x, flags 0x%x\n", 
				tpcb, tpcb->tp_flags);
		ENDDEBUG
		takes_data = TRUE;
		e.ATTR(CR_TPDU).e_cdt  =  hdr->tpdu_CRcdt;
		e.ev_number = CR_TPDU;

		so = tpcb->tp_sock;
		if (so->so_options & SO_ACCEPTCONN) {
			/* 
			 * Create a socket, tpcb, ll pcb, etc. 
			 * for this newborn connection, and fill in all the values. 
			 */
			IFDEBUG(D_CONN)
				printf("abt to call tp_newsocket(0x%x, 0x%x, 0x%x, 0x%x)\n",
					so, laddr, faddr, cons_channel);
			ENDDEBUG
			if( (so = 
				tp_newsocket(so, faddr, cons_channel, 
					class_to_use, 
					(dgout_routine == tpcons_output)?ISO_CONS:ISO_CLNS)
					) == (struct socket *)0 ) {
				/* note - even if netservice is IN_CLNS, as far as
				 * the tp entity is concerned, the only differences
				 * are CO vs CL 
				 */
				IFDEBUG(D_CONN)
					printf("tp_newsocket returns 0\n");
				ENDDEBUG
				goto discard;
			}
			tpcb = sototpcb(so);

			/* stash the f suffix in the new tpcb */
			/* l suffix is already there */

			bcopy( fsufxloc, tpcb->tp_fsuffix, fsufxlen);
			if( (tpcb->tp_fsuffixlen = fsufxlen) == sizeof(short) ) {
				/* even if it's AF_ISO */
				bcopy (fsufxloc, &(satosin(faddr)->sin_port), sizeof(short));
				(tpcb->tp_nlproto->nlp_putsufx)(so->so_pcb, faddr, TP_FOREIGN);
			}

			/*
			 * stash the addresses in the net level pcb 
			 * kind of like a pcbconnect() but don't need
			 * or want all those checks.
			 */
			(tpcb->tp_nlproto->nlp_putnetaddr)(so->so_pcb, faddr, TP_FOREIGN);
			(tpcb->tp_nlproto->nlp_putnetaddr)(so->so_pcb, laddr, TP_LOCAL);

			/*
			 * in the AF_INET case, we need the l,f addrs to contain the ports
			 */
			if( tpcb->tp_domain == AF_INET) {
				CHECK((fsufxlen != sizeof(short))||(lsufxlen != sizeof(short)),
					E_TP_ADDR_UNK, ts_inv_dref, respond, 
					(fsufxloc - (caddr_t)hdr))
				bcopy (lsufxloc, &(satosin(laddr)->sin_port), sizeof(short));
				(tpcb->tp_nlproto->nlp_putsufx)(so->so_pcb, laddr, TP_LOCAL);
				/*
					this has already been done 'cause the fsufxlen is
					sizeof(short):
					bcopy (fsufxloc, &(satosin(faddr)->sin_port), 
						sizeof(short));
					(tpcb->tp_nlproto->nlp_putsufx)(so->so_pcb, faddr, 
						TP_FOREIGN);
				*/
			}

#ifdef TP_PERF_MEAS
			if( tpcb->tp_perf_on = perf_meas ) { /* assignment */
				/* ok, let's create an mbuf for stashing the
				 * statistics if one doesn't already exist 
				 */
				(void) tp_setup_perf(tpcb);
			}
#endif TP_PERF_MEAS
			tpcb->tp_fref = sref;

			/* We've already checked for consistency with the options 
			 * set in tpp,  but we couldn't set them earlier because 
			 * we didn't want to change options in the LISTENING tpcb.
			 * Now we set the options in the new socket's tpcb.
			 */
			(void) tp_consistency( tpcb, TP_FORCE, &tpp);

			if(!tpcb->tp_use_checksum)
				IncStat(ts_csum_off);
			if(tpcb->tp_xpd_service)
				IncStat(ts_use_txpd);
			if(tpcb->tp_xtd_format)
				IncStat(ts_xtd_fmt);

			/*
			 * Get the maximum transmission unit from the lower layer(s)
			 * so we can negotiate a reasonable max TPDU size.
			 */
			(tpcb->tp_nlproto->nlp_mtu)(so, so->so_pcb,
						&tpcb->tp_l_tpdusize, &tpcb->tp_tpdusize, 0);
			tpcb->tp_peer_acktime = acktime;

			/* 
			 * The following kludge is used to test retransmissions and 
			 * timeout during connection establishment.
			 */
			IFDEBUG(D_ZDREF)
				IncStat(ts_zdebug);
				tpcb->tp_fref = 0;
			ENDDEBUG
		}
		IncStat(ts_CR_rcvd);
	} else if ( dutype == ER_TPDU_type ) {
		/* 
		 * ER TPDUs have to be recognized separately
		 * because they don't necessarily have a tpcb
		 * with them and we don't want err out looking for such
		 * a beast.
		 * We could put a bunch of little kludges in the 
		 * next section of code so it would avoid references to tpcb
		 * if dutype == ER_TPDU_type but we don't want code for ERs to
		 * mess up code for data transfer.
		 */
		IncStat(ts_ER_rcvd);
		e.ev_number = ER_TPDU;
		e.ATTR(ER_TPDU).e_reason =  (u_char)hdr->tpdu_ERreason;
		takes_data = 1;
	} else {
		/* tpdu type is CC, XPD, XAK, GR, AK, DR, DC, or DT */

		/* In the next 4 checks,
		 * _tpduf is the fixed part; add 2 to get the dref bits of 
		 * the fixed part (can't take the address of a bit field) 
		 */
		if(cons_channel) {
#if NARGOXTWENTYFIVE > 0
			extern struct tp_pcb *cons_chan_to_tpcb();

			tpcb = cons_chan_to_tpcb( cons_channel );
			/* Problem:  We may have a legit
			 * error situation yet we may or may not have 
			 * a correspondence between the tpcb and the vc,
			 * e.g., TP4cr--> <no dice, respond w/ DR on vc>
			 *          <---  DR
			 * Now it's up to TP to look at the tpdu and do one of:
			 * confirm(dgm)(cr),  confirm(circuit)(cr), reject(cr), or
			 * nothing, if the circuit is already open (any other tpdu). 
			 * Sigh.
			 */

			/* I don't know about this error value */
			CHECK( (tpcb == (struct tp_pcb *)0) ,
				E_TP_NO_CR_ON_NC, ts_inv_dref, respond, 
				(1 + 2 + (caddr_t)&hdr->_tpduf - (caddr_t)hdr))
#else
			printf("tp_input(): X25 NOT CONFIGURED!!\n");
#endif NARGOXTWENTYFIVE > 0
			
		} else {

			CHECK( ((int)dref <= 0 || dref >= N_TPREF) ,
				E_TP_MISM_REFS,ts_inv_dref, respond,
				(1 + 2 + (caddr_t)&hdr->_tpduf - (caddr_t)hdr))
			CHECK( ((tpcb = tp_ref[dref].tpr_pcb ) == (struct tp_pcb *) 0 ), 
				E_TP_MISM_REFS,ts_inv_dref, respond,
				(1 + 2 + (caddr_t)&hdr->_tpduf - (caddr_t)hdr))
			CHECK( (tpcb->tp_refp->tpr_state == REF_FREE), 
				E_TP_MISM_REFS,ts_inv_dref, respond,
				(1 + 2 + (caddr_t)&hdr->_tpduf - (caddr_t)hdr))
		}

		IFDEBUG(D_TPINPUT)
			printf("HAVE A TPCB 2: 0x%x\n", tpcb);
		ENDDEBUG

		/* causes a DR to be sent for CC; ER for all else */
		CHECK( (tpcb->tp_refp->tpr_state == REF_FROZEN),
			(dutype == CC_TPDU_type?E_TP_NO_SESSION:E_TP_MISM_REFS),
			ts_inv_dref, respond,
			(1 + 2 + (caddr_t)&hdr->_tpduf - (caddr_t)hdr))

		IFDEBUG(D_TPINPUT)
			printf("state of dref %d ok, tpcb 0x%x\n", dref,tpcb);
		ENDDEBUG
		/* 
		 * At this point the state of the dref could be
		 * FROZEN: tpr_pcb == NULL,  has ( reference only) timers
		 *		   for example, DC may arrive after the close() has detached
		 *         the tpcb (e.g., if user turned off SO_LISTEN option)
		 * OPENING : a tpcb exists but no timers yet
		 * OPEN  : tpcb exists & timers are outstanding
		 */

		dusize = tpcb->tp_tpdusize;

		dutype = hdr->tpdu_type << 8; /* for the switch below */ 

		WHILE_OPTIONS(P, hdr, tpcb->tp_xtd_format) /* { */

#		define caseof(x,y) case (((x)<<8)+(y))
		switch( dutype | vbptr(P)->tpv_code ) {

			caseof( CC_TPDU_type, TPP_addl_opt ): 
					/* not in class 0; 1 octet */
					vb_getval(P, u_char, addlopt);
					break;
			caseof( CC_TPDU_type, TPP_tpdu_size ): 
					vb_getval(P, u_char, dusize);
					CHECK( (dusize < TP_MIN_TPDUSIZE || dusize > 
						TP_MAX_TPDUSIZE), E_TP_INV_PVAL, ts_inv_pval, respond,
						(1 + (caddr_t)&vbptr(P)->tpv_val - P) )
					IFDEBUG(D_TPINPUT)
						printf("CC dusize 0x%x\n", dusize);
					ENDDEBUG
					break;
			caseof( CC_TPDU_type, TPP_calling_sufx):
					IFDEBUG(D_TPINPUT)
						printf("CC calling (local) sufxlen 0x%x\n", lsufxlen);
					ENDDEBUG
					lsufxloc = (caddr_t) &vbptr(P)->tpv_val;
					lsufxlen = vbptr(P)->tpv_len;
					break;
			caseof(	CC_TPDU_type, TPP_acktime ):
					/* class 4 only, 2 octets */
					vb_getval(P, u_short, acktime);
					acktime = acktime/500; /* convert to slowtimo ticks */
					if( (short)acktime <=0 )
						acktime = 2;
					break;
			caseof(	CC_TPDU_type, TPP_called_sufx):
					fsufxloc = (caddr_t) &vbptr(P)->tpv_val;
					fsufxlen = vbptr(P)->tpv_len;
					IFDEBUG(D_TPINPUT)
						printf("CC called (foreign) sufx len %d\n", fsufxlen);
					ENDDEBUG
					break;

			caseof( CC_TPDU_type,	TPP_checksum):		
			caseof( DR_TPDU_type,	TPP_checksum):		
			caseof( DT_TPDU_type,	TPP_checksum):		
			caseof( XPD_TPDU_type,	TPP_checksum):		
					if( tpcb->tp_use_checksum )  {
						CHECK( iso_check_csum(m, tpdu_len), 
							E_TP_INV_PVAL, ts_bad_csum, discard, 0)
					}
					break;

			/*  this is different from the above because in the context
			 *  of concat/ sep tpdu_len might not be the same as hdr len 
			 */
			caseof( AK_TPDU_type,	TPP_checksum):		
			caseof( XAK_TPDU_type,	TPP_checksum):		
			caseof( DC_TPDU_type,	TPP_checksum):		
					if( tpcb->tp_use_checksum )  {
						CHECK( iso_check_csum(m, hdr->tpdu_li + 1), 
							E_TP_INV_PVAL, ts_bad_csum, discard, 0)
					}
					break;
#ifdef notdef
			caseof( DR_TPDU_type, TPP_addl_info ):
				/* ignore - its length and meaning are
				 * user defined and there's no way
				 * to pass this info to the user anyway
				 */
				break;
#endif notdef

			caseof( AK_TPDU_type, TPP_subseq ):
				/* used after reduction of window */
				vb_getval(P, u_short, subseq);
				subseq = ntohs(subseq);
				IFDEBUG(D_ACKRECV)
					printf("AK Subsequence # 0x%x\n", subseq);
				ENDDEBUG
				break;

			caseof( AK_TPDU_type, TPP_flow_cntl_conf ):
				{
					u_int 	ylwe;
					u_short ysubseq, ycredit;

					fcc_present = TRUE;
					vb_getval(P, u_int,	 	ylwe);
					vb_getval(P, u_short, 	ysubseq);
					vb_getval(P, u_short, 	ycredit);
					ylwe = ntohl(ylwe);
					ysubseq = ntohs(ysubseq);
					ycredit = ntohs(ycredit);
					IFDEBUG(D_ACKRECV)
						printf("AK FCC lwe 0x%x, subseq 0x%x, cdt 0x%x\n", 
							ylwe, ysubseq, ycredit);
					ENDDEBUG
				}
				break;

			default: 
				IFDEBUG(D_TPINPUT)
					printf("param ignored dutype 0x%x, code  0x%x\n",
						dutype, vbptr(P)->tpv_code);
				ENDDEBUG
				IFTRACE(D_TPINPUT)
					tptrace(TPPTmisc, "param ignored dutype code ",
						dutype, vbptr(P)->tpv_code ,0,0);
				ENDTRACE
				IncStat(ts_param_ignored);
				break;
#undef caseof
		}
		/* } */ END_WHILE_OPTIONS(P)

		/* NOTE: the variable dutype has been shifted left! */

		switch( hdr->tpdu_type ) {
		case CC_TPDU_type: 
			/* If CC comes back with an unacceptable class
			 * respond with a DR or ER
			 */

			opt = hdr->tpdu_CCoptions; /* 1 byte */

			{
				tpp = tpcb->_tp_param;
				tpp.p_class = (1<<hdr->tpdu_CCclass);
				tpp.p_tpdusize = dusize;
				tpp.p_dont_change_params = 0;
				tpp.p_xtd_format = (opt & TPO_XTD_FMT) == TPO_XTD_FMT;
				tpp.p_xpd_service = (addlopt & TPAO_USE_TXPD) == TPAO_USE_TXPD;
				tpp.p_use_checksum = (addlopt & TPAO_NO_CSUM) == 0;
#ifdef notdef
				tpp.p_use_efc = (opt & TPO_USE_EFC) == TPO_USE_EFC;
				tpp.p_use_nxpd = (addlopt & TPAO_USE_NXPD) == TPAO_USE_NXPD;
				tpp.p_use_rcc = (addlopt & TPAO_USE_RCC) == TPAO_USE_RCC;
#endif notdef

			CHECK(
				tp_consistency(tpcb, TP_FORCE, &tpp) != 0, 
				E_TP_NEGOT_FAILED, ts_negotfailed, respond,
				(1 + 2 + (caddr_t)&hdr->_tpdufr.CRCC - (caddr_t)hdr) 
					/* ^ more or less the location of class */
				)
			IFTRACE(D_CONN)
				tptrace(TPPTmisc, 
					"after 1 consist class, out, tpconsout",
					tpcb->tp_class, dgout_routine, tpcons_output, 0
					);
			ENDTRACE
			CHECK(
				((class_to_use == TP_CLASS_0)&&
					(dgout_routine != tpcons_output)),
				E_TP_NEGOT_FAILED, ts_negotfailed, respond,
				(1 + 2 + (caddr_t)&hdr->_tpdufr.CRCC - (caddr_t)hdr) 
					/* ^ more or less the location of class */
				)
			}
			if( ! tpcb->tp_use_checksum)
				IncStat(ts_csum_off);
			if(tpcb->tp_xpd_service)
				IncStat(ts_use_txpd);
			if(tpcb->tp_xtd_format)
				IncStat(ts_xtd_fmt);

			IFTRACE(D_CONN)
				tptrace(TPPTmisc, "after CC class flags dusize CCclass",
					tpcb->tp_class, tpcb->tp_flags, tpcb->tp_tpdusize, 
					hdr->tpdu_CCclass);
			ENDTRACE

			/* 
			 * Get the maximum transmission unit from the lower layer(s)
			 * so we can decide how large a TPDU size to negotiate.
			 * It would be nice if the arguments to this
			 * were more reasonable.
			 */
			(tpcb->tp_nlproto->nlp_mtu)(tpcb->tp_sock, tpcb->tp_sock->so_pcb,
						&tpcb->tp_l_tpdusize, &tpcb->tp_tpdusize, 0);

#ifdef	CONS
			/* Could be that this CC came in on a NEW vc, in which case
			 * we have to confirm it.
			 */
			if( cons_channel )
				cons_netcmd( CONN_CONFIRM, tpcb->tp_npcb, cons_channel, 
						tpcb->tp_class == TP_CLASS_4);
#endif	CONS

			tpcb->tp_peer_acktime = acktime;

			/* if called or calling suffices appeared on the CC, 
			 * they'd better jive with what's in the pcb
			 */
			if( fsufxlen ) {
				CHECK( ((tpcb->tp_fsuffixlen != fsufxlen) ||
					bcmp(fsufxloc, tpcb->tp_fsuffix, fsufxlen)),
					E_TP_INV_PVAL,ts_inv_sufx, respond, 
					(1+fsufxloc - (caddr_t)hdr))
			}
			if( lsufxlen ) {
				CHECK( ((tpcb->tp_lsuffixlen != lsufxlen) ||
					bcmp(lsufxloc, tpcb->tp_lsuffix, lsufxlen)),
					E_TP_INV_PVAL,ts_inv_sufx, respond, 
					(1+lsufxloc - (caddr_t)hdr))
			}

#ifdef notdef
			e.ATTR(CC_TPDU).e_sref =  (u_short)hdr->tpdu_CCsref;
#else
			e.ATTR(CC_TPDU).e_sref =  sref;
#endif notdef
			e.ATTR(CC_TPDU).e_cdt  =  hdr->tpdu_CCcdt;
			takes_data = TRUE;
			e.ev_number = CC_TPDU;
			IncStat(ts_CC_rcvd);
			break;

		case DC_TPDU_type:
#ifdef notdef
			if (hdr->tpdu_DCsref != tpcb->tp_fref)
				printf("INPUT: inv sufx DCsref 0x%x, tp_fref 0x%x\n",
					hdr->tpdu_DCsref, tpcb->tp_fref);
#else
			if (sref != tpcb->tp_fref)
				printf("INPUT: inv sufx DCsref 0x%x, tp_fref 0x%x\n",
					sref, tpcb->tp_fref);
#endif notdef
					
#ifdef notdef
			CHECK( (hdr->tpdu_DCsref != tpcb->tp_fref), 
				E_TP_MISM_REFS, ts_inv_sufx, respond,
				(1 + (caddr_t)&hdr->tpdu_DCsref - (caddr_t)hdr))
#else
			CHECK( (sref != tpcb->tp_fref), 
				E_TP_MISM_REFS, ts_inv_sufx, respond,
				(1 + (caddr_t)&hdr->tpdu_DCsref - (caddr_t)hdr))
#endif notdef
			e.ev_number = DC_TPDU;
			IncStat(ts_DC_rcvd);
			break;

		case DR_TPDU_type: 
			IFTRACE(D_TPINPUT)
				tptrace(TPPTmisc, "DR recvd", hdr->tpdu_DRreason, 0, 0, 0);
			ENDTRACE
#ifdef vax
			if(sref != tpcb->tp_fref)
				printf("INPUT: inv sufx DRsref 0x%x tp_fref 0x%x\n",
					sref, tpcb->tp_fref);
					
			CHECK( (sref != tpcb->tp_fref), 
				E_TP_MISM_REFS,ts_inv_sufx, respond,
				(1 + (caddr_t)&hdr->tpdu_DRsref - (caddr_t)hdr))

			e.ATTR(DR_TPDU).e_reason = hdr->tpdu_DRreason;
			e.ATTR(DR_TPDU).e_sref =  (u_short)sref;
#else
			if(hdr->tpdu_DRsref != tpcb->tp_fref)
				printf("INPUT: inv sufx DRsref 0x%x tp_fref 0x%x\n",
					hdr->tpdu_DRsref, tpcb->tp_fref);
					
			CHECK( (hdr->tpdu_DRsref != tpcb->tp_fref), 
				E_TP_MISM_REFS,ts_inv_sufx, respond,
				(1 + (caddr_t)&hdr->tpdu_DRsref - (caddr_t)hdr))

			e.ATTR(DR_TPDU).e_reason = 
				hdr->tpdu_DRreason;
			e.ATTR(DR_TPDU).e_sref =  (u_short)hdr->tpdu_DRsref;
#endif vax
			takes_data = TRUE;
			e.ev_number = DR_TPDU;
			IncStat(ts_DR_rcvd);
			break;

		case ER_TPDU_type:
			IFTRACE(D_TPINPUT)
				tptrace(TPPTmisc, "ER recvd", hdr->tpdu_ERreason,0,0,0);
			ENDTRACE
			e.ev_number = ER_TPDU;
			e.ATTR(ER_TPDU).e_reason = hdr->tpdu_ERreason;
			IncStat(ts_ER_rcvd);
			break;

		case AK_TPDU_type: 

			e.ATTR(AK_TPDU).e_subseq = subseq;
			e.ATTR(AK_TPDU).e_fcc_present = fcc_present;

			if (tpcb->tp_xtd_format) {
#ifdef BYTE_ORDER
				union seq_type seqeotX;

				seqeotX.s_seqeot = ntohl(hdr->tpdu_seqeotX);
				e.ATTR(AK_TPDU).e_seq = seqeotX.s_seq;
				e.ATTR(AK_TPDU).e_cdt = ntohs(hdr->tpdu_AKcdtX);
#else
				e.ATTR(AK_TPDU).e_cdt = hdr->tpdu_AKcdtX;
				e.ATTR(AK_TPDU).e_seq = hdr->tpdu_AKseqX;
#endif BYTE_ORDER
			} else {
				e.ATTR(AK_TPDU).e_cdt = hdr->tpdu_AKcdt;
				e.ATTR(AK_TPDU).e_seq = hdr->tpdu_AKseq;
			}
			IFTRACE(D_TPINPUT)
				tptrace(TPPTmisc, "AK recvd seq cdt subseq fcc_pres", 
					e.ATTR(AK_TPDU).e_seq, e.ATTR(AK_TPDU).e_cdt,
					subseq, fcc_present);
			ENDTRACE

			e.ev_number = AK_TPDU;
			IncStat(ts_AK_rcvd);
			IncPStat(tpcb, tps_AK_rcvd);
			break;

		case XAK_TPDU_type: 
			if (tpcb->tp_xtd_format) {
#ifdef BYTE_ORDER
				union seq_type seqeotX;

				seqeotX.s_seqeot = ntohl(hdr->tpdu_seqeotX);
				e.ATTR(XAK_TPDU).e_seq = seqeotX.s_seq;
#else
				e.ATTR(XAK_TPDU).e_seq = hdr->tpdu_XAKseqX;
#endif BYTE_ORDER
			} else {
				e.ATTR(XAK_TPDU).e_seq = hdr->tpdu_XAKseq;
			}
			e.ev_number = XAK_TPDU;
			IncStat(ts_XAK_rcvd);
			IncPStat(tpcb, tps_XAK_rcvd);
			break;

		case XPD_TPDU_type: 
			if (tpcb->tp_xtd_format) {
#ifdef BYTE_ORDER
				union seq_type seqeotX;

				seqeotX.s_seqeot = ntohl(hdr->tpdu_seqeotX);
				e.ATTR(XPD_TPDU).e_seq = seqeotX.s_seq;
#else
				e.ATTR(XPD_TPDU).e_seq = hdr->tpdu_XPDseqX;
#endif BYTE_ORDER
			} else {
				e.ATTR(XPD_TPDU).e_seq = hdr->tpdu_XPDseq;
			}
			takes_data = TRUE;
			e.ev_number = XPD_TPDU;
			IncStat(ts_XPD_rcvd);
			IncPStat(tpcb, tps_XPD_rcvd);
			break;

		case DT_TPDU_type:
			{ /* the y option will cause occasional packets to be dropped.
			   * A little crude but it works.
			   */

				IFDEBUG(D_DROP)
					if(time.tv_usec & 0x4 && hdr->tpdu_DTseq & 0x1) {
						IncStat(ts_ydebug);
						goto discard;
					}
				ENDDEBUG
			}
			if (tpcb->tp_class == TP_CLASS_0) {
				e.ATTR(DT_TPDU).e_seq = 0; /* actually don't care */
				e.ATTR(DT_TPDU).e_eot = (((struct tp0du *)hdr)->tp0du_eot);
			} else if (tpcb->tp_xtd_format) {
#ifdef BYTE_ORDER
				union seq_type seqeotX;

				seqeotX.s_seqeot = ntohl(hdr->tpdu_seqeotX);
				e.ATTR(DT_TPDU).e_seq = seqeotX.s_seq;
				e.ATTR(DT_TPDU).e_eot = seqeotX.s_eot;
#else
				e.ATTR(DT_TPDU).e_seq = hdr->tpdu_DTseqX;
				e.ATTR(DT_TPDU).e_eot = hdr->tpdu_DTeotX;
#endif BYTE_ORDER
			} else {
				e.ATTR(DT_TPDU).e_seq = hdr->tpdu_DTseq;
				e.ATTR(DT_TPDU).e_eot = hdr->tpdu_DTeot;
			}
			if(e.ATTR(DT_TPDU).e_eot)
				IncStat(ts_eot_input);
			takes_data = TRUE;
			e.ev_number = DT_TPDU;
			IncStat(ts_DT_rcvd);
			IncPStat(tpcb, tps_DT_rcvd);
			break;

		case GR_TPDU_type: 
			tp_indicate(T_DISCONNECT, tpcb, ECONNABORTED);
			/* drop through */
		default:
			/* this should NEVER happen because there is a
			 * check for dutype well above here
			 */
			error = E_TP_INV_TPDU; /* causes an ER  */
			IFDEBUG(D_TPINPUT)
				printf("INVALID dutype 0x%x\n", hdr->tpdu_type);
			ENDDEBUG
			IncStat(ts_inv_dutype);
			goto respond;
		}
	}

	/* peel off the tp header; 
	 * remember that the du_li doesn't count itself.
	 * This may leave us w/ an empty mbuf at the front of a chain.
	 * We can't just throw away the empty mbuf because hdr still points
	 * into the mbuf's data area and we're still using hdr (the tpdu header)
	 */
	m->m_len -= ((int)hdr->tpdu_li + 1);
	m->m_off += ((int)hdr->tpdu_li + 1);

	if(takes_data) {
		register int max;

		switch( hdr->tpdu_type ) {
		case CR_TPDU_type:
		case CC_TPDU_type:
		case DR_TPDU_type:
		case XPD_TPDU_type:
		case DT_TPDU_type:
			e.ATTR(DT_TPDU).e_datalen = tpdu_len - hdr->tpdu_li - 1;
			max = tpdu_info[ hdr->tpdu_type ] [TP_MAX_DATA_INDEX];
			CHECK( (max && e.ATTR(DT_TPDU).e_datalen > max),
				 E_TP_LENGTH_INVAL,ts_inv_length, respond, (max + hdr->tpdu_li + 1))

			e.ATTR(DT_TPDU).e_data =  m;
			break;

		default:
			printf(
				"ERROR in tp_input! hdr->tpdu_type 0x%x takes_data 0x%x m 0x%x\n",
				hdr->tpdu_type, takes_data, m);
			break;
		}
		/* prevent m_freem() after tp_driver() from throwing it all away */
		m = MNULL;
	}

	IncStat(ts_tpdu_rcvd);

	IFDEBUG(D_TPINPUT)
		printf( "tp_input: before driver, state 0x%x event 0x%x m 0x%x",
			tpcb->tp_state, e.ev_number, m );
		printf(" e.e_data 0x%x\n", e.ATTR(DT_TPDU).e_data);
		printf("takes_data 0x%x m_len 0x%x, tpdu_len 0x%x\n",
			takes_data, (m==MNULL)?0:m->m_len,  tpdu_len);
	ENDDEBUG

	if( tpcb->tp_decbit != 0 ) /* unsigned 4 bits */
		tpcb->tp_decbit --;

	error = tp_driver(tpcb, &e);

	ASSERT(tpcb != (struct tp_pcb *)0);
	ASSERT(tpcb->tp_sock != (struct socket *)0);
	if( tpcb->tp_sock->so_error == 0 )
		tpcb->tp_sock->so_error = error;

	/* Kludge to keep the state tables under control (adding
	 * data on connect & disconnect & freeing the mbuf containing
	 * the data would have exploded the tables and made a big mess ).
	 */
	switch(e.ev_number) {
		case CC_TPDU:
		case DR_TPDU:
		case CR_TPDU:
			m = e.ATTR(CC_TPDU).e_data; /* same field for all three dutypes */
			IFDEBUG(D_TPINPUT)
				printf("after driver, restoring m to 0x%x, takes_data 0x%x\n", 
				m, takes_data);
			ENDDEBUG
			break;
		default:
			break;
	}
	/* Concatenated sequences are terminated by any tpdu that 
	 * carries data: CR, CC, DT, XPD, DR.
	 * All other tpdu types may be concatenated: AK, XAK, DC, ER.
	 */

separate:
	if ( takes_data == 0 )  {
		ASSERT( m != MNULL );
		/* 
		 * we already peeled off the prev. tp header so 
		 * we can just pull up some more and repeat
		 */

		IFDEBUG(D_TPINPUT)
			hdr = mtod(m, struct tpdu *);
			printf("tp_input @ separate: hdr 0x%x size %d m 0x%x\n", 
			hdr, (int) hdr->tpdu_li + 1, m);
			dump_mbuf(m, "tp_input after driver, at separate");
		ENDDEBUG

		if( m = tp_inputprep(m) ) {
			IncStat(ts_concat_rcvd);
			goto again;
		}
	}
	if ( m != MNULL ) {
		IFDEBUG(D_TPINPUT)
			printf("tp_input : m_freem(0x%x)\n", m);
		ENDDEBUG
		m_freem(m);
		IFDEBUG(D_TPINPUT)
			printf("tp_input : after m_freem 0x%x\n", m);
		ENDDEBUG
	}
	return (ProtoHook) tpcb;

discard:
	/* class 4: drop the tpdu */
	/* class 2,0: Should drop the net connection, if you can figure out
	 * to which connection it applies
	 */
	IFDEBUG(D_TPINPUT)
		printf("tp_input DISCARD\n");
	ENDDEBUG
	IFTRACE(D_TPINPUT)
		tptrace(TPPTmisc, "tp_input DISCARD m",  m,0,0,0);
	ENDTRACE
	m_freem(m);
	IncStat(ts_recv_drop);
	return (ProtoHook)0;

respond:
	IFDEBUG(D_ERROR_EMIT)
		printf("RESPOND: error 0x%x, errloc 0x%x\n", error, errloc);
	ENDDEBUG
	IFTRACE(D_TPINPUT)
		tptrace(TPPTmisc, "tp_input RESPOND m error sref",  m,error,sref,0);
	ENDTRACE
	if( sref == 0 )
		goto discard;
	(void) tp_error_emit(error, sref, faddr, laddr, 
		m, errloc, tpcb, cons_channel, dgout_routine
		);
	IFDEBUG(D_ERROR_EMIT)
		printf("tp_input after error_emit\n");
	ENDDEBUG

#ifdef lint
	printf("",sref,opt);
#endif lint
	IncStat(ts_recv_drop);
	return (ProtoHook)0;
}


/*
 * NAME: tp_headersize()
 *
 * CALLED FROM:
 *  tp_emit() and tp_sbsend()
 *  TP needs to know the header size so it can figure out how
 *  much data to put in each tpdu.
 *
 * FUNCTION, ARGUMENTS, and RETURN VALUE:
 *  For a given connection, represented by (tpcb), and 
 *  tpdu type (dutype), return the size of a tp header.
 *
 * RETURNS:	  the expected size of the heade in bytesr
 *
 * SIDE EFFECTS:	
 *
 * NOTES:	 It would be nice if it got the network header size as well.
 */
int
tp_headersize(dutype, tpcb) 
	int 			dutype;
	struct tp_pcb 	*tpcb;
{
	register int size = 0;

	IFTRACE(D_CONN)
		tptrace(TPPTmisc, "tp_headersize dutype class xtd_format",
			dutype, tpcb->tp_class, tpcb->tp_xtd_format, 0);
	ENDTRACE
	if( !( (tpcb->tp_class == TP_CLASS_0) || 
			(tpcb->tp_class == TP_CLASS_4) || 
			(dutype == DR_TPDU_type) || 
			(dutype == CR_TPDU_type) )) {
				printf("tp_headersize:dutype 0x%x, class 0x%x", 
			dutype, tpcb->tp_class);
	/* TODO: identify this and GET RID OF IT */
	}
	ASSERT( (tpcb->tp_class == TP_CLASS_0) || 
			(tpcb->tp_class == TP_CLASS_4) || 
			(dutype == DR_TPDU_type) || 
			(dutype == CR_TPDU_type) );

	if( tpcb->tp_class == TP_CLASS_0 ) {
		size =  tpdu_info[ dutype ] [TP_LEN_CLASS_0_INDEX];
	} else  {
		size = tpdu_info[ dutype ] [tpcb->tp_xtd_format];
	} 
	return size;
	/* caller must get network level header size separately */
}
