/*	@(#)egp.c	1.5 (Berkeley) 12/14/88 */

/* EGP User Process, ISI 23-Jun-84 */

#include	"include.h"


extern  u_short	mysystem;		/* autonomous system number */
extern	int	maxacq;			/* maximum number neighbors to be 
						acquired */
extern	int	egpsleep;	/* No. seconds between egpjob() wakeups.
				   Time computed when neigh. (re)acquired 
				   or dropped */
extern struct egpngh *egpngh;	/* start neighbor state info tables */


extern	u_short	egprid_h;	/* sequence number of received egp packet
				   in host byte order - all ids in internal
				   tables are in host byte order */


/* The major EGP state changes and initializations occur when the
 gateway first comes up, init_egp() (in init.c), when a neighbor goes into
 state NEIGHBOR, egpstngh(), and when a neighbor goes into state UNACQUIRED,
 egpstunacq().
*/


/* egpjob() periodically sends EGP packets.
 * At most maxacq neighbors will be acquired. The number of new acq. requests
 * sent this interrupt does not exceed the number of neighbors yet to be
 * acquired. Any number of retransmitted acq. requests may be sent to 
 * non-responding neighbors.
 */

egpjob()
{
	reg 	egpng	*ngp;
	reg	int	shiftreg;
		long	time;
	static	long	chkcmdtime = 0;
		char	*strtime;
		int	n_new_acqsnt = 0,	/* # new acquisition requests
						   sent this interrupt */
			i,
			nresponses;

	getod(&time);
	strtime = ctime(&time);

/* commence periodic processing for each egp neighbor */

	for(ngp=egpngh; ngp != NULL; ngp = ngp->ng_next) {

	    switch(ngp->ng_state) {
	    case UNACQUIRED:
		if(terminate) break;
					/* only send as many new acq. requests
					   as # neighbors yet to be acquired*/
		if( n_new_acqsnt >= maxacq - n_acquired) break;
		if( time < ngp->ng_htime) break;
		ngp->ng_state = ACQUIRE_SENT;
		ngp->ng_htime = time + ngp->ng_hint;
		egpsacq( ngp->ng_addr, ngp->ng_myaddr,NAREQ, ACTIVE,
								 ngp->ng_sid);
		n_new_acqsnt++;
		break;

	    case ACQUIRE_SENT:
		if (time < ngp->ng_htime) break;
		if (++ngp->ng_retry >= NACQ_RETRY) {
			ngp->ng_hint = LONGACQINT;
			egpstime();
			ngp->ng_retry = 0;
		}
		ngp->ng_htime = time + ngp->ng_hint;
		egpsacq( ngp->ng_addr, ngp->ng_myaddr, NAREQ, ACTIVE,
								 ngp->ng_sid);
		break;

	    case NEIGHBOR:

	    /* determine reachability */

		if( time >= ngp->ng_htime) {
		    shiftreg = ngp->ng_responses;
		    nresponses = 0;

		    for( i = 0; i < NCOMMANDS; i++) {	/* count responses */
			if( shiftreg & 1) nresponses++;
			shiftreg >>= 1;
		    }
		    if( ngp->ng_reach & NG_DOWN) {
			if( nresponses > NRESPONSES) {	/* down -> up */
				ngp->ng_reach &= ~NG_DOWN;
				ngp->ng_retry = 0;
				TRACE_EXT("egpjob: %s reachable %s\n",
					inet_ntoa(ngp->ng_addr), strtime);
			}
			else 				/* down -> down */
				if(++ngp->ng_retry >= NUNREACH) {
					egpcease( ngp, UNSPEC);
					break;
				}
		    }
		    else
			if( nresponses < NRESPONSES) {	/* up -> down */
				ngp->ng_reach |= NG_DOWN;
				TRACE_EXT("egpjob: %s unreachable %s\n",
					inet_ntoa(ngp->ng_addr), strtime);
				rt_unreach( ngp->ng_addr); /* delete routes
							for down gateway */

						/* if other unacq. neigh.
						cease and try to acq. */
				if( maxacq < nneigh ) {
					egpcease( ngp, UNSPEC);
					break;
				}
				ngp->ng_retry = 1;
			}	
		}

	    /* Send NR poll */

		if( time >= ngp->ng_stime) {	/* time for poll */
		    if( ngp->ng_reach == BOTH_UP) {
			if( ngp->ng_snpoll < NPOLL) {
				if( ngp->ng_snpoll++ == 0) {  /* not repoll */
					ngp->ng_sid++;
					ngp->ng_runsol = 0;
				}
					/* repoll interval is hello int */
				ngp->ng_stime = time + ngp->ng_hint;
				ngp->ng_responses <<= 1;
				egpspoll(ngp);
				break;
			}
			else {		/* no more repolls */
				ngp->ng_stime = time + ngp->ng_spint
							- ngp->ng_hint;
				ngp->ng_snpoll = 0;
			}
		    }
		    else
			ngp->ng_snpoll = 0;	/* reachability may have
						changed when poll outstanding
						*/
		}

	    /* Send hello */

		if (time >= ngp->ng_htime) {
			ngp->ng_htime = time + ngp->ng_hint;
			ngp->ng_responses <<= 1;
			egpshello(ngp, HELLO, ngp->ng_sid);
		}
		break;

	    case CEASE_SENT:
		if (time < ngp->ng_htime) break;
		if (++ngp->ng_retry > NCEASE_RETRY) {
			egpstunacq(ngp);
			break;
		}
		ngp->ng_htime = time + ngp->ng_hint;
		egpsacq( ngp->ng_addr, ngp->ng_myaddr, NACEASE,
						ngp->ng_status, ngp->ng_sid);
		break;

	    default:
		break;
	    }
	}

	if( time >= chkcmdtime) {
		if( chkcmdtime)			/* ignore initialization */
			egpchkcmd( time - chkcmdtime);
		chkcmdtime = time + CHKCMDTIME;
	}

	return;
}

/* egpchkcmd() check command reception rate for neighbors and if too high
 * cease and go to state BAD so no further peering.
 */

egpchkcmd( extratime)
	u_long	extratime;
{
	reg egpng *ngp;
	u_long	cmdtime;		/* time since last check
						# commands recvd */
	u_long	nmaxcmd;		/* max # commands allowed */

					/* compute max. allowed cmds */
	cmdtime = extratime + CHKCMDTIME;
	nmaxcmd = (NMAXCMD * cmdtime)/CHKCMDTIME;
/**printf("nmaxcmd = %d, cmdtime = %d\n", nmaxcmd, cmdtime);**/


			/* For each neighbor check if too many cmds recvd */
	for( ngp = egpngh; ngp != NULL; ngp = ngp->ng_next) {

/**printf("ng_rcmd = %d\n", ngp->ng_rcmd);**/
		if( ngp->ng_rcmd > nmaxcmd) {
			ngp->ng_flags |= NG_BAD;
			TRACE_EXT("egpjob: recv too many commands (%d) in ",
					ngp->ng_rcmd);
			TRACE_EXT("%d sec from %s -> state BAD\n", cmdtime,
					inet_ntoa( ngp->ng_addr));
			egpcease( ngp, PARAMPROB);
		}
		ngp->ng_rcmd = 0;
	}
}


/* Process egp pkt which just arrived */

egpin(pkt)
	reg  struct  ip  *pkt;
{
	reg	egpng		*ngp;
	reg	egpkt		*egp;
		int		iphlen,
				egplen;
					/* Unix 4.2 BSD passes offset in vax
					format, to raw socket => dont swap */
	if (pkt->ip_off & ~IP_DF) {
		TRACE_EXT("egpin: recv fragmanted pkt from %s\n",
				inet_ntoa( pkt->ip_src.s_addr));
		return;
	}
	iphlen = pkt->ip_hl;
	iphlen <<=2;

	egp = (egpkt *) (((char *) pkt) + iphlen);
	egplen = pkt->ip_len;		/* Unix 4.2 BSD passes data length in
					Vax format, not total length, to raw 
					socket */

	if (egp_cksum(egp, egplen) != 0) {
		TRACE_EXT("egpin: bad checksum from %s\n",
					inet_ntoa( pkt->ip_src.s_addr));
		return;
	}
	if( egp->egp_ver != EGPVER) {
		TRACE_EXT("egpin: bad version %d from %s\n", egp->egp_ver,
					inet_ntoa( pkt->ip_src.s_addr));
		return;
	}
	if( egplen < EGPLEN) {
		TRACE_EXT("egpin: bad pkt length %d from %s\n", egplen,
					inet_ntoa( pkt->ip_src.s_addr));
		return;
	}
	egprid_h = ntohs(egp->egp_id);	/* save sequence number in host byte
						order */

/* check if in legitimate neighbor list */

	for( ngp=egpngh; ngp != NULL; ngp = ngp->ng_next) {       /* all legit
								      neigh */
		if (ngp->ng_state == EMPTY) continue;

		if (ngp->ng_addr == pkt->ip_src.s_addr) {    /* legit neigh */
			if (ngp->ng_myaddr != pkt->ip_dst.s_addr
			    && egp->egp_type != EGPERR ) {

				egpserr(pkt->ip_src.s_addr,pkt->ip_dst.s_addr,
						     egp, egplen, EUNSPEC, 0);
				return;
			}
			if (ngp->ng_state == BAD)
				return;
			switch(egp->egp_type) {
			case EGPACQ:
				egpacq( ngp, egp, egplen);
				return;
			case EGPHELLO:
				egphello( ngp, egp);
				return;
			case EGPNR:
				egpnr( ngp, egp, egplen);
				return;
			case EGPPOLL:
				egppoll( ngp, egp);
				return;
			case EGPERR:
				if( ngp->ng_state == NEIGHBOR) {
					if( egp->egp_status == DOWN)
						ngp->ng_reach |= ME_DOWN;
					else
						ngp->ng_reach &= ~ME_DOWN;
					ngp->ng_responses |= 1;
				}
				break;
			default:
				egpserr(pkt->ip_src.s_addr,pkt->ip_dst.s_addr,
						    egp, egplen, EBADHEAD, 0);
				return;
			}				/* end switch type */
			break;
		}					/* end if legit */
	}					/* end for all legit neigh */

	switch(egp->egp_type) {

/* May be legit. neighbor */

	case EGPERR:
		if( tracing & TR_EXT) {
		    if( tracing & TR_PKT)
			printf("egpin: recv above error packet\n");
		    else
			traceegp("egpin: recv error pkt: ",
				pkt->ip_src.s_addr, pkt->ip_dst.s_addr,
				egp, egplen);
		}
		break;

/* Not legit. neighbor */
	case EGPACQ:
		switch( egp->egp_code) {
		case NAREQ:
			egpsacq( pkt->ip_src.s_addr, pkt->ip_dst.s_addr,
					NAREFUS, ADMINPROHIB, egprid_h);
			break;
		case NACEASE:
			egpsacq( pkt->ip_src.s_addr, pkt->ip_dst.s_addr,
					NACACK, egp->egp_status, egprid_h);
			break;
		case NACACK:
			break;
		case NACONF:
		default:
			egpsacq( pkt->ip_src.s_addr, pkt->ip_dst.s_addr,
						NACEASE, PROTOVIOL, egprid_h);
			break;
		}
		break;
	case EGPNR:
	case EGPHELLO:
	case EGPPOLL:
	default:
		egpsacq( pkt->ip_src.s_addr, pkt->ip_dst.s_addr, 
						NACEASE, PROTOVIOL, egprid_h);
		break;
	}						/* end switch type */

	return;
}


/* process received acquisition packet */

egpacq(ngp, egp, egplen)
/*
 * External variables: terminate
 */
reg	egpng	*ngp;
reg	egpkt	*egp;
	int	egplen;
{
	int	error =0;

	switch(egp->egp_code) {
	case NAREQ:			/* Neighbor acquisition request */
		ngp->ng_rcmd++;
		if (egplen < sizeof(struct egpacq)) {
			error = 1;
			break;
		}

		if (ngp->ng_state == CEASE_SENT) 
			egpsacq( ngp->ng_addr, ngp->ng_myaddr, NACEASE,
						ngp->ng_status, egprid_h);
		else if (terminate) 
			egpsacq( ngp->ng_addr, ngp->ng_myaddr, NAREFUS,
							GODOWN, egprid_h);
		else if( ngp->ng_state != NEIGHBOR && n_acquired == maxacq)
			egpsacq( ngp->ng_addr, ngp->ng_myaddr, NAREFUS,
							NORESOURCE, egprid_h);
		else if( error = egpsetint(ngp, egp))
						    /* intervals too big */
			egpsacq( ngp->ng_addr, ngp->ng_myaddr, NAREFUS,
							PARAMPROB, egprid_h);
		else {
			egpstngh(ngp, egp);
			ngp->ng_rid = egprid_h;
			egpsacq( ngp->ng_addr, ngp->ng_myaddr, NACONF,
							ACTIVE, egprid_h);
		}

		break;

	case NACONF:			/* Neighbor acq. confirm */
		if( ngp->ng_state == ACQUIRE_SENT
		    && egprid_h == ngp->ng_sid
		    && egplen >= sizeof(struct egpacq) ) {
			if( error = egpsetint( ngp, egp)) {
						     /* intervals too big */
				ngp->ng_flags |= NG_BAD; /*dont retry acq*/
				egpcease( ngp, PARAMPROB);
			}
			else {
				egpstngh(ngp, egp);
				ngp->ng_rid = egprid_h;
			}
		}
		else {
			if( ngp->ng_state == UNACQUIRED)
				egpsacq( ngp->ng_addr, ngp->ng_myaddr,
					NACEASE, PROTOVIOL, egprid_h);
			error = 1;
		}
		break;

	case NAREFUS:			/* Neighbor acq. refuse */
		if( ngp->ng_state == ACQUIRE_SENT
		    && egprid_h == ngp->ng_sid )
			egpstunacq(ngp);
		else
			error = 1;
		break;

	case NACEASE:			/* Neighbor acq. cease */
		ngp->ng_rcmd++;
		egpstunacq(ngp);
		egpsacq( ngp->ng_addr, ngp->ng_myaddr, NACACK,
						egp->egp_status, egprid_h);
		return;

	case NACACK:			/* Neighbor acq. cease ack */
		if( ngp->ng_state == CEASE_SENT
		    && egprid_h == ngp->ng_sid )
			egpstunacq(ngp);
		else	
					/* in unacq. state cease may be sent
					as response to invalid packets so
					may get valid ceaseack */
			if( ngp->ng_state != UNACQUIRED)
				error = 1;
		break;

	default:
		error = 1;
		break;
	}

	if( error && tracing & TR_EXT) {
	    if( !(tracing & TR_PKT))
		traceegp("egpacq: recv bad pkt: ",
			ngp->ng_addr, ngp->ng_myaddr, egp, egplen);
	    printf("egpacq: above pkt bad, recvd in state %d\n",
							ngp->ng_state);
	}
	return;
}


/* egpsetint  set egp hello and poll intervals and times.
 * Returns 1 if either poll or hello intervals too big, 0 otherwise.
 */

egpsetint(ngp, egpa)
	reg	egpng	*ngp;
	reg	struct  egpacq	*egpa;
{
		u_short	helloint,
			pollint,
			ratio;
		long	time;

	getod(&time);
					/* check parameters within bounds */
	helloint = ntohs(egpa->ea_hint);
	pollint  = ntohs(egpa->ea_pint);
	if( helloint > MAXHELLOINT || pollint > MAXPOLLINT) {
		TRACE_EXT("Hello interval = %d or poll interval = %d",
			helloint, pollint);
		TRACE_EXT(" too big\n");
		return(1);
	}
	if (helloint < MINHELLOINT)
		helloint = MINHELLOINT;
	if (pollint < MINPOLLINT)
		pollint = MINPOLLINT;

	ratio = (pollint - 1)/helloint + 1;		/* keep integer ratio
							pollint:helloint */

	helloint += HELLOMARGIN;			/* add margin for net
							delay variation */
	ngp->ng_hint = helloint;
	ngp->ng_htime = time + helloint;
	ngp->ng_spint = helloint * ratio;
	ngp->ng_stime = time + helloint; 	/* so first poll as soon as
						both considered up */
	TRACE_EXT("egpsetint %s, hello %d poll %d => hello %d poll %d\n",
		inet_ntoa(ngp->ng_addr),
		ntohs(egpa->ea_hint), ntohs(egpa->ea_pint),
		ngp->ng_hint, ngp->ng_spint);
	return(0);
}

/* egpstngh() go into neighbor state, init most vars
*/
egpstngh(ngp, egp)
reg	egpng	*ngp;
reg	egpkt	*egp;
{
	long	time;
	char	*strtime;

	getod(&time);

	if( ngp->ng_state != NEIGHBOR) {
		n_acquired++;
		strtime = ctime(&time);
		TRACE_EXT("egpstngh: acquired %s %s\n",
				inet_ntoa( ngp->ng_addr), strtime);
	}
	ngp->ng_state = NEIGHBOR;
	ngp->ng_reach = ME_DOWN;	/* so don't send initial poll until
					after peer reports me up, but I will
					respond to peer polls immediately 
					as a poll implies he thinks I am up */
	ngp->ng_responses = ~0;		/* assume neighbor up when acquired */
	ngp->ng_flags = 0;
	ngp->ng_status = 0;
	ngp->ng_retry = 0;
	ngp->ng_snpoll = 0;
	ngp->ng_runsol = 0;
	ngp->ng_rpint = MINPOLLINT;
	ngp->ng_rnpoll = 0;
	ngp->ng_rtime = time;

	if( n_acquired == maxacq) 
		egp_maxacq();

	egpstime();
}


/* egp_maxacq() ceases any neighbors with outstanding acquisition requests
 * once the maximum number of neighbors have been acquired. It also resets
 * all acquisition intervals to the short value.
 */

egp_maxacq()
{
	struct egpngh *ngp;

	for( ngp = egpngh; ngp != NULL; ngp = ngp->ng_next) {
		if( ngp->ng_state == ACQUIRE_SENT)
			egpcease( ngp, NORESOURCE);
		if( ngp->ng_state == UNACQUIRED)
			ngp->ng_hint = MINHELLOINT + HELLOMARGIN;
	}
	return;
}


/* egpstime() sets egpsleep, the time between periodic sending of EGP packets;
 * maxpollint - the maximum poll interval for all acquired neighbors;
 * and rt_maxage - the maximum life time of routes. egpsleep is set to the
 * minimum interval for all potential peers or 60 s, whichever is less.
 * rt_maxage is set to the maximum of RT_MINAGE and RT_NPOLLAGE times the
 * maximum poll interval.
 * Called by egpstunacq(), egpstngh() and egpjob()
 */

egpstime()
{
	reg	egpng	*ngp;

	egpsleep = 60;		/* at least run egpjob every minute - then if
				/* I receive an acq. req. I will wait at most
				/* 1 minute before I start sending hellos etc.
				*/
	maxpollint = MINPOLLINT;

	for(ngp=egpngh; ngp != NULL; ngp = ngp->ng_next) {
		switch(ngp->ng_state) {
		case NEIGHBOR:
			if( ngp->ng_spint > maxpollint)
				maxpollint = ngp->ng_spint;
		case UNACQUIRED:
		case ACQUIRE_SENT:
		case CEASE_SENT:
			if (ngp->ng_hint < egpsleep)
				egpsleep = ngp->ng_hint;
			break;

		default:
			break;
			}
		}

	if (egpsleep < MINHELLOINT) {
		TRACE_EXT("egpstime: bad sleep time %d\n", egpsleep);
		egpsleep = MINHELLOINT;
	}
					/* only change rt_maxage if have an
					acquired neighbor */
	if( n_acquired > 0)
		rt_maxage = (maxpollint*RT_NPOLLAGE > RT_MINAGE) ? 
					(maxpollint*RT_NPOLLAGE) : RT_MINAGE;
	return;
}


/* egpstunacq  go into state UNACQUIRED
 *
 * External variables:
 * terminate
 *
 */
egpstunacq(ngp)
reg	egpng	*ngp;
{
	long	time;
	char	*strtime;
	int	allceased;

	getod(&time);

	if( ngp->ng_state == NEIGHBOR) {
		n_acquired--;
		strtime = ctime(&time);
		TRACE_EXT("egpstunacq: cease from %s %s\n",
				inet_ntoa( ngp->ng_addr), strtime);
	}
	if( ngp->ng_flags & NG_BAD)
		ngp->ng_state = BAD;
	else
		ngp->ng_state = UNACQUIRED;
	ngp->ng_status =0;
	ngp->ng_retry = 0;
				/* if max # acq., set acq. interval to short
				value for faster retry if current neigh goes
				down */
	ngp->ng_hint = (n_acquired < maxacq) ?
		LONGACQINT : (MINHELLOINT + HELLOMARGIN);
	ngp->ng_htime = time + ngp->ng_hint;

	egpstime();

/* If terminate signal received and all neighbors ceased then exit */

	if( terminate) {
		allceased = TRUE;
		for(ngp=egpngh; ngp != NULL; ngp = ngp->ng_next)
			if(ngp->ng_state == ACQUIRE_SENT ||
		   	ngp->ng_state == NEIGHBOR ||
		   	ngp->ng_state == CEASE_SENT) allceased = FALSE;
		if(allceased) quit();
	}
}


/* process received hello packet */

egphello(ngp, egp)
reg	egpng	*ngp;
reg	egpkt	*egp;
{
	int	error = 0;
	long	time;

	if (ngp->ng_state != NEIGHBOR) {
		if (ngp->ng_state == UNACQUIRED)
			egpsacq( ngp->ng_addr, ngp->ng_myaddr, NACEASE,
							 PROTOVIOL, egprid_h);
		ngp->ng_rcmd++;
		error = 1;
	}
	else {
		if( egp->egp_status == DOWN)
			ngp->ng_reach |= ME_DOWN;
		else
			ngp->ng_reach &= ~ME_DOWN;

		switch(egp->egp_code) {
		case HELLO:
			ngp->ng_rcmd++;
			ngp->ng_rid == egprid_h;
			egpshello( ngp, HEARDU, egprid_h);
			return;

		case HEARDU:
			if( egprid_h == ngp->ng_sid)
				ngp->ng_responses |= 1;
			break;

		default:
			error = 1;
			break;
		}
	}
	if( error && tracing & TR_EXT) {
	    if( !(tracing & TR_PKT))
		traceegp("egphello: recv bad pkt: ", ngp->ng_addr,
			ngp->ng_myaddr, egp, sizeof( struct egppkt));
	    printf("egphello: above pkt bad, recvd in state %d\n",
							ngp->ng_state);
	}

	return;
}


/* Process received egp NR poll packet. */

egppoll(ngp, egp)
reg	egpng	*ngp;
reg	struct  egppkt	*egp;
{
	int	error = NOERROR;
	long	time;
	u_char  status;

	ngp->ng_rcmd++;
	ngp->ng_reach &= ~ME_DOWN;		/* poll => neighbor considers
						me up */
	if (ngp->ng_state != NEIGHBOR) {
		if (ngp->ng_state == UNACQUIRED)
			egpsacq( ngp->ng_addr, ngp->ng_myaddr, NACEASE,
							 PROTOVIOL, egprid_h);
 		error = 100;
	}
	else if ( egp->egp_code != 0) error = EBADHEAD;
				/* compare source net with expected net */
	else if ( inet_netof( ((struct egppoll *)egp)->ep_net) 
					!= inet_netof( ngp->ng_addr))
		error = EBADHEAD;
	else if( ngp->ng_reach & NG_DOWN)
		error = EUNSPEC;
	else {
		getod(&time);
		if (time < ngp->ng_rtime) { 	/* my NR may be lost*/
			if( egprid_h != ngp->ng_rid
			    && ++ngp->ng_rnpoll > 2*NPOLL )
				error = EXSPOLL;
		}
		else {
			ngp->ng_rnpoll = 1;	/* new poll */
					/* reduce next expected poll time by a
					small margin to allow for variation in
					network delay */
			ngp->ng_rtime = time + ngp->ng_rpint - HELLOMARGIN;
		}
	}

	if (error == NOERROR) {
		ngp->ng_rid = egprid_h;
		egpsnr( ngp, 0, egprid_h);
	}
	else {
		if (error < 100) 
		    egpserr( ngp->ng_addr, ngp->ng_myaddr, egp, 
					sizeof( struct egppoll), error, ngp);
		else if( tracing & TR_EXT) {
		    if( !(tracing & TR_PKT))
			traceegp("egppoll: recv bad pkt: ", ngp->ng_addr,
				ngp->ng_myaddr,	egp, sizeof( struct egppoll));
		    printf("egpnr: above pkt bad, error %d, recvd in state %d\n",
						error, ngp->ng_state);
		}
	}

	return;
}


/* Process received NR message packet. */

egpnr( ngp, egp, egplen)
	reg	egpng	*ngp;
	reg	egpkt	*egp;
		int	egplen;
{
	int	error = NOERROR,
		egpid;
	long		time;

	if (ngp->ng_state != NEIGHBOR) {
		if (ngp->ng_state == UNACQUIRED)
			egpsacq( ngp->ng_addr, ngp->ng_myaddr, NACEASE,
							 PROTOVIOL, egprid_h);
		error = 100;				
	}
	else if (egp->egp_code != 0)
		error = EBADHEAD;
				/* compare source net with expected net */
	else if ( inet_netof( ((struct egpnr *)egp)->en_net)
					!= inet_netof( ngp->ng_myaddr))
		error = EBADHEAD;
	else if (egp->egp_status & UNSOLICITED) {
		if (egprid_h != ngp->ng_sid) 	/* wrong seq. # */
			error = 100;
		else if (++ngp->ng_runsol > 1)  /* too many unsol. NR */
			error = EUNSPEC;
	}
	else if( egprid_h != ngp->ng_sid 	/* wrong seq. # */
		 || ngp->ng_snpoll == 0)	/* NR already recvd */
		error = 100;			/* discard */
	else {
		getod(&time);
					/* reduce next poll time slightly to
					take account of net delays on response
					in order to preserve sending ratio to
					hellos. Note HELLOMARGIN*ratio already
					added to advised min. poll interval so
					latter still met */
		ngp->ng_stime = time + ngp->ng_spint - HELLOMARGIN;
		ngp->ng_snpoll = 0;		/* no more repolls required */
		ngp->ng_reach &= ~ME_DOWN;	/* update => neighbor 
						considers me up */
		ngp->ng_responses |= 1;
		if( rt_NRupdate(ngp, egp, egplen))
			error = EBADDATA;
	}

	if( error != NOERROR) {
	    if (error < 100)
		egpserr( ngp->ng_addr, ngp->ng_myaddr, egp, egplen, error,
									 ngp);
	    else if( tracing & TR_EXT) {
		    if( !(tracing & TR_PKT))
			traceegp("egpnr: recv bad pkt: ",
				ngp->ng_addr, ngp->ng_myaddr, egp, egplen);
		    printf("egpnr: above pkt bad, error %d, recvd in state %d\n",
						error, ngp->ng_state);
	    }
	}

	return;
}


/* send acquisition or cease packet */

egpsacq( dst, src, code, status, id)
	u_long  dst,
		src;	/* destination and source internet address */
	u_char	code,
		status;
	u_short	id;
{
	struct egpacq	acqpkt;
	int length;

	acqpkt.ea_pkt.egp_ver = EGPVER;
	acqpkt.ea_pkt.egp_type = EGPACQ;
	acqpkt.ea_pkt.egp_code = code;
	acqpkt.ea_pkt.egp_status = status;
	acqpkt.ea_pkt.egp_system = htons(mysystem);
	acqpkt.ea_pkt.egp_id = htons(id);

	acqpkt.ea_hint = ntohs(MINHELLOINT);
	acqpkt.ea_pint = ntohs(MINPOLLINT);

	if( code == NAREQ || code == NACONF)
		length = sizeof( acqpkt);
	else					/* omit hello & poll int */
		length = sizeof( acqpkt.ea_pkt);

	acqpkt.ea_pkt.egp_chksum = 0;
	acqpkt.ea_pkt.egp_chksum = egp_cksum( &acqpkt, length);


	egp_send( dst, src, &acqpkt, length);
	return;
}


/* send hello or I-H-U packet */

egpshello( ngp, code, id)
	egpng	*ngp;
	u_char	code;
	u_short id;
{
	struct egppkt hellopkt;

	hellopkt.egp_ver = EGPVER;
	hellopkt.egp_type = EGPHELLO;
	hellopkt.egp_code = code;
	hellopkt.egp_status = (ngp->ng_reach & NG_DOWN) ? DOWN:UP;
	hellopkt.egp_system = htons(mysystem);
	hellopkt.egp_id = htons(id);
	hellopkt.egp_chksum = 0;
	hellopkt.egp_chksum = egp_cksum( &hellopkt, sizeof( hellopkt));

	egp_send( ngp->ng_addr, ngp->ng_myaddr, &hellopkt, sizeof( hellopkt) );

	return;
}


/* send NR poll packet */

egpspoll(ngp)
reg	egpng	*ngp;
{
	struct egppoll	pollpkt;
	struct in_addr  addr;

	pollpkt.ep_pkt.egp_ver = EGPVER;
	pollpkt.ep_pkt.egp_type = EGPPOLL;
	pollpkt.ep_pkt.egp_code = 0;
	pollpkt.ep_pkt.egp_status = (ngp->ng_reach & NG_DOWN) ? DOWN:UP;
	pollpkt.ep_pkt.egp_system = htons(mysystem);
	pollpkt.ep_pkt.egp_id = htons(ngp->ng_sid);
	pollpkt.ep_unused = 0;
	addr = inet_makeaddr( inet_netof( ngp->ng_myaddr), 0);
	pollpkt.ep_net = addr.s_addr;

	pollpkt.ep_pkt.egp_chksum = 0;
	pollpkt.ep_pkt.egp_chksum = egp_cksum( &pollpkt, sizeof( pollpkt));

	egp_send( ngp->ng_addr, ngp->ng_myaddr, &pollpkt, sizeof( pollpkt));

	return;
}


/* send error packet */

egpserr( dst, src, egp, length, error, ngp)
	u_long	dst,
		src;	/* destination and source internet address */
	egpkt	*egp;	/* erroneous egp packet */
	int	length;	/* length erroneous packet */
	int	error;	
	egpng	*ngp;	/* ponter to legit. neighbor table, else zero */
{
	struct egperr errpkt;

	errpkt.ee_pkt.egp_ver = EGPVER;
	errpkt.ee_pkt.egp_type = EGPERR;
	errpkt.ee_pkt.egp_code = 0;
	if (ngp && ngp->ng_state == NEIGHBOR)
	    errpkt.ee_pkt.egp_status = (ngp->ng_reach & NG_DOWN) ? DOWN:UP;
	else
	    errpkt.ee_pkt.egp_status = 0;
	errpkt.ee_pkt.egp_system = htons(mysystem);
	errpkt.ee_pkt.egp_id = htons(egprid_h);		/* recvd seq.# */
	errpkt.ee_rsn = htons( (u_short)error);
				/* copy header of erroneous egp packet */
	bzero( errpkt.ee_egphd, sizeof( errpkt.ee_egphd) );
	if( length > sizeof( errpkt.ee_egphd))
		length =  sizeof( errpkt.ee_egphd);
	bcopy( (char *)egp, errpkt.ee_egphd, length );
		

	errpkt.ee_pkt.egp_chksum = 0;
	errpkt.ee_pkt.egp_chksum = egp_cksum( &errpkt, sizeof( errpkt));

	if( tracing & TR_EXT) {
	    if( tracing & TR_PKT) printf("egpserr: send error pkt:\n");
	    else traceegp("egpserr: send error pkt ", src, dst, &errpkt,
				sizeof( errpkt));
	}
	egp_send( dst, src, &errpkt, sizeof( errpkt) );

	return;
}


/* egpsnr() sends an NR message packet.
 * It fills in the header information, calls rt_ifcheck() to update the
 * interface status information and rt_NRnets() to fill in the reachable
 * networks.
 */
extern	u_short	mysystem;
extern  int	n_interfaces;
extern  int	n_remote_nets;

egpsnr(ngp, unsol, id)
	egpng	*ngp;
	int  unsol;			/* TRUE => set unsolicited bit */
	u_short	id;
{
	int	maxsize,
		length;
	register  struct  egpnr   *nrp;
	struct  in_addr   addr;

				/* allocate message buffer */
	maxsize = sizeof( struct egpnr)
			 + NRMAXNETUNIT*( n_interfaces + n_remote_nets);
	if( maxsize > MAXPACKETSIZE - sizeof( struct ip) ) {
	    printf("egpsnr: NR message not sent, possibly larger than IP\n");
	    return;
	}
	nrp = (struct egpnr *)malloc( maxsize);
	if( nrp == NULL) {
	    printf("egpsnr:malloc: out of memory\n");
	    return;
	}
				/* prepare static part of NR message header */
	nrp->en_pkt.egp_ver = EGPVER;
	nrp->en_pkt.egp_type = EGPNR;
	nrp->en_pkt.egp_code = 0;
	nrp->en_pkt.egp_status = (ngp->ng_reach & NG_DOWN) ? DOWN:UP;
	if( unsol)
		nrp->en_pkt.egp_status |= UNSOLICITED;
	nrp->en_pkt.egp_system = htons(mysystem);
	nrp->en_pkt.egp_id = htons(id);
	nrp->en_egw = 0;		/* no exterior gateways */
					/* copy shared net address */
	addr = inet_makeaddr( inet_netof( ngp->ng_myaddr), 0);
	nrp->en_net = addr.s_addr;

	if( if_check()) rt_ifupdate();	/* update interface status and if any
					   changed update interior route
					   status */

					/* prepare network reachability part
					   of EGP NR message */

	length = rt_NRnets( nrp, ngp->ng_myaddr);

	if(length != ERROR) {
		nrp->en_pkt.egp_chksum = 0;
		nrp->en_pkt.egp_chksum = egp_cksum( nrp, length);

		egp_send( ngp->ng_addr, ngp->ng_myaddr, nrp, length);
	}
	else
		printf("egpsnr: NR message not sent\n");

	free( (char *)nrp);
	return;
}


/* compute egp checksum */

egp_cksum(egp, len)		/* returns checksum, note that C-GW routine
				just returned ones complement sum */
	u_char	*egp;		/* pointer to start egp packet */
	int	len;		/* length of egp packet */
{
    register    u_short * wd, *end_wd;
    register	u_long temp;
    union {
	u_long l;
	u_short s[2];
    } sum;

    if (len & 01) {		/* pad if odd number octets */
	*(egp + len) = 0;
	len++;
    }

    wd = (u_short *) egp;	/* start word */
    end_wd = wd + (len >> 1);	/* end word */
    temp = 0;
    while (wd < end_wd)
	temp += *wd++;
    sum.l = temp;
				 /* convert to 16-bit ones complement
				    sum - add 16-bit carry */
    sum.l = sum.s[0] + sum.s[1];
    sum.l = sum.s[0] + sum.s[1];

    return ((~sum.s[0]) & 0xffff);	/* return ones complement sum */
}


/* send an egp packet. */

egp_send( dst, src, egp, length)
	u_long	dst,		/* destination and source internet address */
		src;
	u_char	*egp;		/* pointer to start of egp packet */
	int	length;		/* length in octets of egp packet */

{
	struct sockaddr_in addr;
	struct interface   *ifp;
	int	error = FALSE;

	bzero( &addr, sizeof (addr));
	addr.sin_family = AF_INET;
					/* find interface to send packet on */
	addr.sin_addr.s_addr = src;
	ifp = if_withnet( &addr);
	if( ifp == NULL) {
		printf("egp_send: no interface for source address %s\n",
			inet_ntoa( src));
		error = TRUE;
	}

	else {
		addr.sin_addr.s_addr = dst;
		if( sendto( ifp->int_egpsock, egp, length, 0,
						&addr, sizeof( addr)) < 0) {
			if( tracing & TR_EXT || tracing & TR_PKT) {
			    printf( "egp_send: sendto: %s", inet_ntoa( dst) );
			    p_error(" ");
			    error = TRUE;
			}
		}
	}
	if( !error) TRACE_EGPPKT(EGP SENT, src, dst, egp, length)
	else	    TRACE_EGPPKT(EGP *NOT* SENT, src, dst, egp, length);

	return;
}


/* Initiate egp neighbor cease */

egpcease( ngp, rsn)
reg	egpng	*ngp;
	u_char	rsn;
{
	long	time;
	char	*strtime;

	getod(&time);
	egpsacq( ngp->ng_addr, ngp->ng_myaddr, NACEASE, rsn, ngp->ng_sid);
	if( ngp->ng_state == NEIGHBOR) {
		n_acquired--;
		strtime = ctime(&time);
		TRACE_EXT("egpcease: cease to %s %s\n",
				inet_ntoa(ngp->ng_addr), strtime);
	}
	ngp->ng_state = CEASE_SENT;
	ngp->ng_retry = 0;
	if( ngp->ng_state != NEIGHBOR)
		ngp->ng_hint = MINHELLOINT + HELLOMARGIN;
	ngp->ng_htime = time + ngp->ng_hint;
	ngp->ng_status = rsn;		/* save reason for retransmission */
	return;
}


/* Send Cease to all neighbors when going down.
 *
 * Global variables:
 * terminate - set here, tested by egpstunacq()
 */

egpallcease()
{

register egpng *ngp;
	 egpng *ngf;
	int	ceasesent = FALSE;
					/* scan neighbor state tables */
	for(ngp=egpngh; ngp != NULL; ngp = ngp->ng_next) {
		if( (ngp->ng_state == NEIGHBOR)
		    || (ngp->ng_state == ACQUIRE_SENT)) {
			egpcease( ngp, GODOWN);
			ceasesent = TRUE;
		    }
	}
	if( ceasesent)
		terminate = TRUE;
	else
		quit();
}
