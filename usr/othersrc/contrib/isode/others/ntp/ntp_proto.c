#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/ntp/RCS/ntp_proto.c,v 7.1 91/02/22 09:33:51 mrose Interim $";
#endif

/*
 * This module actually implements the the bulk of the NTP protocol processing.
 * It contains a minimum of machine and operating system dependencies (or at
 * least that's the idea).  Setup of UDP sockets, timers, etc is done in the
 * ntpd.c module, while arithmetic conversion routines are in ntpsubs.c
 *
 * Some of this is now factored out as it was too protocol specific.
 */

/*
 * Based on the ntp 3.4 code - but modified for use with OSI.
 * $Log:	ntp_proto.c,v $
 * Revision 7.1  91/02/22  09:33:51  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/12/10  17:21:31  mrose
 * *** empty log message ***
 * 
 * Revision 1.2  90/08/14  10:13:56  jpo
 * new protocol version
 * 
 * Revision 1.1  89/06/15  20:36:57  jpo
 * Initial revision
 * 
 * 
 */

#include "ntp.h"

int peer_switches, peer_sw_inhibited;

struct ntp_peer dummy_peer;
extern double WayTooBig;
extern unsigned long clock_watchdog;
extern LLog	*pgm_log;
#ifdef	DEBUG
extern int debug;
extern void dump_pkt();
#endif
extern unsigned int servport;
extern int trusting, logstats;
extern struct sysdata sys;
extern struct list peer_list;
extern struct ntp_peer *check_peer();
extern char *malloc(), *ntoa();
extern double drift_comp, compliance;	/* logical clock variables */
extern double s_fixed_to_double(), ul_fixed_to_double();
extern void make_new_peer(), double_to_s_fixed(), tstamp(), receive ();
extern int demobilize();
	

char actions[5][5] = {

 /*      Sym Act   Sym Pas    Client     Server     Broadcast  |Host /       */
 /*      --------   --------  --------   ---------  ---------  |    / Peer   */
 /*                                                            ------------  */
	{ACT_PKT,  ACT_PKT,   ACT_RECV,  ACT_XMIT,  ACT_XMIT},	/* Sym Act   */
	{ACT_PKT,  ACT_ERROR, ACT_RECV,  ACT_ERROR, ACT_ERROR},	/* Sym Pas   */
	{ACT_XMIT, ACT_XMIT,  ACT_ERROR, ACT_XMIT,  ACT_XMIT},	/* Client    */
	{ACT_PKT,  ACT_ERROR, ACT_RECV,  ACT_ERROR, ACT_ERROR},	/* Server    */
	{ACT_PKT,  ACT_ERROR, ACT_RECV,  ACT_ERROR, ACT_ERROR}};/* Broadcast */


#ifdef	REFCLOCK
void	refclock_input();
#endif

void	process_packet(), clock_update(), clear(), clock_filter(),
	receive(), select_clock(), poll_update();

/* 3.4. Event Processing */

/* 3.4.1. Transmit Procedure */
void
transmit(peer)
	struct ntp_peer *peer;
{
	struct timeval txtv;
	static struct ntpdata ntpframe;
	struct ntpdata *pkt = &ntpframe;
	int i;

	if (peer->src.type == AF_OSI) {
		transmit_osi (peer);
		return;
	}

	pkt->status = sys.leap | peer->hmode;
	VERS2PKT (pkt -> status, peer -> vers);
	pkt->stratum = sys.stratum;
	pkt->ppoll = peer->hpoll;
	pkt->precision = (char) sys.precision;
	pkt->distance = sys.distance;
	pkt->dispersion = sys.dispersion;
	if (peer -> src.type == AF_INET)
		pkt->refid = sys.refid.rid_inet;
	pkt->reftime = sys.reftime;
	pkt->org = peer->org;
	pkt->rec = peer->rec;
	(void) gettimeofday(&txtv, (struct timezone *) 0);

#ifdef notdef
	if (peer->flags & PEER_FL_AUTHENABLE &&
	    peer -> vers == 2 &&
	    authhavekey (peer -> keyid)) {
		pkt->keyid = htonl(peer -> keyid);
		auth1crypt (peer -> keyid, pkt, LEN_PKT_NOMAC);
		tstamp(&pkt->xmt, &txtv);
		auth2crypt (peer -> keyid, pkt, LEN_PKT_NOMAC);
	} else 
#endif
	{
#ifdef notdef
/* Not Yet in this version */
		pkt->keyid = 0;			/* XXX */
#endif
		tstamp(&pkt->xmt, &txtv);
	}

	peer->xmt = pkt->xmt;

	if ((peer->flags & (PEER_FL_BCAST|PEER_FL_REFCLOCK)) == 0) {
		/* select correct socket to send reply on */
		struct intf *ap;

		ap = &addrs[peer->sock < 0 ? 0 : peer-> sock];
		switch (peer->src.type) {
		    case AF_INET:
			if (send_inet (ap, (char *)pkt, sizeof (*pkt),
				       &peer->src) < 0)
				return;
			break;
		}
#ifdef	REFCLOCK
	} else if (peer->flags & PEER_FL_REFCLOCK) {
		/* Special version of code below, adjusted for refclocks */


		peer->pkt_sent++;
		i = peer->reach;	/* save a copy */

		peer->reach = (peer->reach << 1) & NTP_WINDOW_SHIFT_MASK;

		if (i && peer->reach == 0) {
			advise (LLOG_NOTICE, NULLCP,
				"Lost reachability with %.4s",
				peer->refid.rid_string);
		}

		if (peer->reach == 0)
			clear(peer);

		if (peer->valid < 2)
			peer->valid++;
		else {
			clock_filter(peer, 0.0, 0.0);	/* call with invalid values */
			select_clock();		/* and try to reselect clock */
		}

		peer->timer = 1<<NTP_MINPOLL;	/* poll refclocks frequently */

		refclock_input(peer, pkt);
		return;
#endif REFCLOCK
	} else {
#ifdef	BROADCAST_NTP
		if (addrs[peer->sock].addr.type == AF_INET) {
			if (send_inet (&addrs[peer->sock], pkt, peer) < 0)
				return;
		}
		else
			return;
#else
		return;
#endif
	}

#ifdef	DEBUG
	if (debug > 5) {
		printf("\nSent ");
		dump_pkt(&peer->src, pkt, (struct ntp_peer *)NULL);
	}
#endif
	peer->pkt_sent++;
	i = peer->reach;	/* save a copy */

	peer->reach = (peer->reach << 1) & NTP_WINDOW_SHIFT_MASK;

	if ((peer->reach == 0) && 
	    ((peer->flags & PEER_FL_CONFIG) == 0) &&
	    (peer != &dummy_peer) && demobilize(&peer_list, peer))
		return;

	if (i && peer->reach == 0) {
		advise (LLOG_NOTICE, NULLCP,
			"Lost reachability with %s",
			paddr (&peer->src));
	}

	if (peer->reach == 0) {
		clear(peer);
		if (peer->src.type == AF_INET)
			peer->sock = -1; /* since he fell off the end of the
					   earth, don't depend on local address
					   any longer */
	}

	if (peer->valid < 2)
		peer->valid++;
	else {
		clock_filter(peer, 0.0, 0.0);	/* call with invalid values */
		select_clock();		/* and try to reselect clock */
		if (sys.peer != NULL)
			poll_update(sys.peer, NTP_MINPOLL);
	}

	peer->timer = 1<<(MAX(MIN(peer->ppoll, MIN(peer->hpoll, NTP_MAXPOLL)),
			       NTP_MINPOLL));

	if (peer->reach == 0) {
		if (peer->backoff == 0)
			peer->backoff = BACKOFF_COUNT;
		else {
			if (peer->backoff == 1)
				poll_update (peer, (int)peer->hpoll + 1);
			peer->backoff --;
		}
	}
	else if (peer->estdisp > PEER_THRESHOLD)
		poll_update(peer, (int)peer->hpoll - 1);
	else
		poll_update(peer, (int)peer->hpoll + 1);
}

#ifdef REFCLOCK
void
refclock_input(peer, pkt)
	struct ntpdata *pkt;
	struct ntp_peer *peer;
{
	struct timeval *tvp;
	struct timeval *otvp;

	if (read_clock(peer->sock, &tvp, &otvp))
		return;

	tstamp(&pkt->rec, tvp);
	pkt->xmt = pkt->rec;
	pkt->reftime = pkt->rec;
	tstamp(&pkt->org, otvp);
	peer->xmt = pkt->org;
	pkt->refid = peer->refid.rid_inet;	/* XXX */
	pkt->status &= ~ALARM;
	pkt->stratum = peer->stratum;
	pkt->ppoll = 0xff;
	pkt->precision = peer->precision;
	double_to_s_fixed(&pkt->distance, 0.0);
	double_to_s_fixed(&pkt->dispersion, 0.0);
#ifdef	DEBUG
	if (debug > 5) {
		printf("\nFaking packet ");
		dump_pkt(&peer->src, pkt, (struct ntp_peer *)NULL);
	}
#endif
	receive((struct Naddr *)peer, pkt, otvp, -1);
	return;
}
#endif REFCLOCK

/* 3.4.2. Receive Procedure */
void
receive(dst, pkt, tvp, sock)
	struct Naddr *dst;
	struct ntpdata *pkt;
	struct timeval *tvp;
	int sock;
{
	struct ntp_peer *peer;
	int peer_mode;

	/* if we're only going to support NTP Version 2 then this stuff
	   isn't necessary, right? */

	if ((peer_mode = pkt->status & MODEMASK) == 0 && dst) {
		/* packet from an older NTP implementation.  Synthesize the
		   correct mode.  The mapping goes like this:

		   pkt source port      pkt dst port	Mode
		   ---------------	------------	----
		   NTP Port		NTP Port	symmetric active
		   NTP Port		not NTP Port	server
		   not NTP Port		NTP Port	client
		   not NTP Port		not NTP Port	<not possible>

		   Now, since we only are processing packets with the
		   destination being NTP Port, it reduces to the two cases:

		   pkt source port      pkt dst port	Mode
		   ---------------	------------	----
		   NTP Port		NTP Port	symmetric active
		   not NTP Port		NTP Port	client		 */

		if (dst->inet_ad.sin_port == servport)
			peer_mode = MODE_SYM_ACT;
		else
			peer_mode = MODE_CLIENT;
	}

	if (peer_mode == MODE_CLIENT) {
		/*
		 * Special case: Use the dummy peer item that we keep around
		 * just for this type of thing
		 */
		peer = &dummy_peer;
		make_new_peer(peer);
		peer->src = *dst;
		peer->sock = sock;
		peer->hmode = MODE_SYM_PAS;
		peer->reach = 0;
		clear(peer);
#ifdef	REFCLOCK
	} else if (sock == -1) {
		/* we're begin called by refclock_input(), get peer ptr */
		peer = (struct ntp_peer *)dst;
#endif
	} else
		peer = check_peer(dst, sock);

	if (peer == NULL) {
		peer = (struct ntp_peer *) malloc(sizeof(struct ntp_peer));
		if (peer == NULL) {
			advise (LLOG_EXCEPTIONS, "malloc", "peer");
			return;
		}
		make_new_peer(peer);
		peer->src = *dst;
		peer->sock = sock;	/* remember which socket we heard 
					   this from */
		peer->hmode = MODE_SYM_PAS;
		peer->reach = 0;
		clear(peer);
		/*
		 *  If we decide to consider any random NTP peer that might
		 *  come as a peer we might sync to, then set the PEER_FL_SYNC
		 *  flag in the peer structure.
		 *
		 *  Alternatively, we could change the hmode to MODE_SERVER, 
		 *  but then the peer state wouldn't be persistant.
		 */
		if (trusting)
			peer->flags |= PEER_FL_SYNC;

		enqueue(&peer_list, peer);
	}

	/* 
	 *  "pre-configured" peers are initially assigned a socket index of
	 *  -1, which means we don't know which interface we'll use to talk
	 *  to them.  Once the first reply comes back, we'll update the
	 *  peer structure
	 */
	if (peer->sock == -1)
		peer->sock = sock;

#ifdef	BROADCAST_NTP
	/*
	 *  Input frame matched a funny broadcast peer;  these peers only
	 *  exist to periodically generate broadcasts.  If an input packet
	 *  matched, it means that it looked like it *came* from the broadcast
	 *  address.  This is clearly bogus.
	 */
	if (peer->flags & PEER_FL_BCAST) {
		TRACE (1, ("receive: input frame for broadcast peer?"));
		return;
	}
#endif	/* BROADCAST_NTP */

#if	0
	if ((peer->flags & PEER_FL_AUTHENABLE) &&
	    pkt->mac) {
		/* verify computed crypto-checksum */
	}
#endif

	if (peer_mode < MODE_SYM_ACT || peer_mode > MODE_BROADCAST) {
		TRACE (1, ("Bogus peer_mode %d from %s", peer_mode,
			   (struct ntp_peer *) dst == peer ?
			   "refclock" : paddr (dst)));
#ifdef	DEBUG
		if (debug > 3) abort();
#endif
		return;
	}

	if (peer->hmode < MODE_SYM_ACT || peer->hmode > MODE_BROADCAST) {
		advise (LLOG_EXCEPTIONS, NULLCP,
			"Bogus hmode %d for peer %s", peer->hmode,
			paddr (&peer->src));
		abort();
	}

	peer->backoff = 0;
	switch (actions[peer_mode - 1][peer->hmode - 1]) {
	case ACT_RECV:
		if (!(((peer->flags & PEER_FL_CONFIG) == 0) &&
		      STRMCMP(pkt->stratum, >, sys.stratum))) {
			peer->flags &= ~PEER_FL_SNOOZE;
			peer->reach |= 1;
			process_packet(dst, pkt, tvp, peer);
			break;
		}
		/* Note fall-through */
	case ACT_ERROR:
		if (((peer->flags & PEER_FL_CONFIG) == 0) &&
		    (peer != &dummy_peer) && demobilize(&peer_list, peer))
			break;
		break;

	case ACT_PKT:
		if (!(((peer->flags & PEER_FL_CONFIG) == 0) &&
		      STRMCMP(pkt->stratum, >, sys.stratum))) {
			peer->flags &= ~PEER_FL_SNOOZE;
			peer->reach |= 1;
			process_packet((struct ntp_peer *) dst == peer ?
				       NULL : dst,
				       pkt, tvp, peer);
			break;
		}
		/* Note fall-through */
	case ACT_XMIT:
		process_packet((struct ntp_peer *) dst == peer ? NULL : dst,
			       pkt, tvp, peer);
		poll_update(peer, (int)peer->ppoll);
		transmit(peer);
		break;

	default:
		abort();
	}
}


/* 3.4.3 Packet procedure */
void
process_packet(dst, pkt, tvp, peer)
	struct Naddr *dst;
	struct ntpdata *pkt;
	struct timeval *tvp;
	struct ntp_peer *peer;
{
	double t1, t2, t3, t4, offset, delay;
	short duplicate, bogus;

	duplicate = (pkt->xmt.int_part == peer->org.int_part) &&
		(pkt->xmt.fraction == peer->org.fraction);

	bogus = ((pkt->org.int_part != peer->xmt.int_part) ||
		 (pkt->org.fraction != peer->xmt.fraction))
		|| (peer->xmt.int_part == 0);

	peer->pkt_rcvd++;
	peer->leap = pkt->status & LEAPMASK;
	peer->vers = PKT2VERS(pkt->status);
	peer->stratum = pkt->stratum;
	peer->ppoll = pkt->ppoll;
	peer->precision = pkt->precision;
	peer->distance = pkt->distance;
	peer->dispersion = pkt->dispersion;
	if (peer->src.type == AF_INET) {
		peer->refid.rid_type = peer -> stratum == 1 ?
			RID_STRING : RID_INET;
		peer->refid.rid_inet = pkt->refid;
	}
	peer->reftime = pkt->reftime;
	peer->org = pkt->xmt;
	tstamp(&peer->rec, tvp);
	poll_update(peer, (int)peer->hpoll);

	/* 
	 * may want to do something special here for Broadcast Mode peers to
	 * allow these through 
	 */
	if (bogus || duplicate || 
	    (pkt->org.int_part == 0 && pkt->org.fraction == 0) ||
	    (pkt->rec.int_part == 0 && pkt->rec.fraction == 0)) {
		peer->pkt_dropped++;
		TRACE (3, ("process_packet: dropped duplicate or bogus"));
		return;
	}

	/*
	 *  Now compute local adjusts 
	 */
	t1 = ul_fixed_to_double(&pkt->org);
	t2 = ul_fixed_to_double(&pkt->rec);
	t3 = ul_fixed_to_double(&pkt->xmt);
	t4 = ul_fixed_to_double(&peer->rec);

	/* 
	 * although the delay computation looks different than the one in the
	 * specification, it is correct.  Think about it.
	 */
	delay = (t2 - t1) - (t3 - t4);
	offset = ((t2 - t1) + (t3 - t4)) / 2.0;

	delay += 1.0/(unsigned long)(1L << -sys.precision)
#ifndef	REFCLOCK
		+ NTP_MAXSKW;
#else
		+ (peer->flags&PEER_FL_REFCLOCK) ? NTP_REFMAXSKW : NTP_MAXSKW;
#endif
	if (peer->precision < 0 && -peer->precision < sizeof(long)*NBBY)
		delay += 1.0/(unsigned long)(1L << -peer->precision);

	if (delay < 0.0) {
		peer->pkt_dropped++;
		return;
	}

#ifndef	REFCLOCK
	delay = MAX(delay, NTP_MINDIST);
#else
	delay = MAX(delay, (peer->flags & PEER_FL_REFCLOCK) ?
		    NTP_REFMINDIST : NTP_MINDIST);
#endif

	peer->valid = 0;
	clock_filter(peer, delay, offset);  /* invoke clock filter procedure */

	TRACE (1, ("host: %s : %f : %f : %f : %f : %f : %o",
		   dst ? paddr (dst) : "refclock",
		   delay, offset,
		   peer->estdelay, peer->estoffset, peer->estdisp,
		   peer->reach));
	clock_update(peer);		/* call clock update procedure */
}

/* 3.4.4 Primary clock procedure */
/*
 *  We don't have a primary clock.
 *
 *  TODO:
 *
 *  ``When a  primary clock is connected to the host, it is convient to
 *    incorporate its information into the database as if the clock was
 *    represented as an ordinary peer.  The clock can be polled once a
 *    minute or so and the returned timecheck used to produce a new update
 *    for the logical clock.''
 */


/* 3.4.5 Clock update procedure */

void
clock_update(peer)
	struct ntp_peer *peer;
{
	double temp;
	extern int adj_logical();

	select_clock();
	if (sys.peer != NULL)
		poll_update(sys.peer, NTP_MINPOLL);

	/*
	 * Did we just sync to this peer?
	 */
	if ((peer == sys.peer) && (sys.hold == 0)) {
		/*
		 *  Update the local system variables
		 */
		sys.leap = peer->leap;
#ifndef	REFCLOCK
		sys.stratum = peer->stratum + 1;
		if (peer->src.type == AF_INET) {
			sys.refid.rid_type = RID_INET;
			sys.refid.rid_inet = peer->src.inet_ad.sin_addr.s_addr;
		}
		else if (peer -> src.type == AF_OSI) {
			sys.refid.rid_type = RID_PSAP;
			sys.refid.rid_psap = peer->src.psap_ad;
		}
#else
		if (peer->flags & PEER_FL_REFCLOCK) {
			/* once we re-map the stratums so that stratum 0 is
			   better than stratum 1, some of this foolishness
			   can go away */
			sys.stratum = peer->stratum;
			sys.refid = peer->refid;
		} else {
			sys.stratum = peer->stratum + 1;
			if (peer->src.type == AF_INET) {
				sys.refid.rid_type = RID_INET;
				sys.refid.rid_inet =
					peer->src.inet_ad.sin_addr.s_addr;
			}
			else if (peer -> src.type == AF_OSI) {
				sys.refid.rid_type = RID_PSAP;
				sys.refid.rid_psap = peer->src.psap_ad;
			}
		}
#endif

		temp = s_fixed_to_double(&peer->distance) + peer->estdelay;
		double_to_s_fixed(&sys.distance, temp);

		temp = s_fixed_to_double(&peer->dispersion) + peer->estdisp;
		double_to_s_fixed(&sys.dispersion, temp);

		sys.reftime = peer->rec;

		TRACE (3, ("clock_update: synced to peer, adj clock"));

		/*
		 * Sanity check: is computed offset insane?
		 */
		if (peer->estoffset > WayTooBig ||
		    peer->estoffset < -WayTooBig) {
			advise (LLOG_EXCEPTIONS, NULLCP,
				"Clock is too far off %f sec. [%s]",
				peer->estoffset, paddr (&peer->src));
			return;
		}

		clock_watchdog = 0;	/* reset watchdog timer */
		if (adj_logical(peer->estoffset) > 0) {
			register struct ntp_peer *p = peer_list.head;

			advise (LLOG_NOTICE, NULLCP,
			        "adjust: STEP %s st %d off %f drft %f cmpl %f",
				paddr (&peer->src), peer->stratum,
				peer->estoffset, drift_comp, compliance);
			TRACE (1, ("Clockset from %s stratum %d offset %f",
				   paddr (&peer->src),
				   peer->stratum, peer->estoffset));

			while (p) {
				clear(p);
				p = p->next;
			}
			sys.hold = PEER_SHIFT * (1 << NTP_MINPOLL);
			TRACE (3, ("clock_updates: STEP ADJ"));
		} else {
			if (logstats) {
				advise (LLOG_NOTICE, NULLCP,
			        "adjust: SLEW %s st %d off %f drft %f cmpl %f",
					paddr (&peer->src),
					peer->stratum,
					peer->estoffset, drift_comp,
					compliance);
			}
		}
	}
}

/* 3.4.6 Initialization procedure */

void
initialize()
{
	sys.leap = ALARM;	/* indicate unsynchronized */
	sys.stratum = 0;
	sys.precision = 0;	/* may be specified in the config file;
				   if not, gets set in init_kern_vars() */
#if	0	/* under construction */
	sys.keyid = 0;
	sys.keys = ??;
#endif
	sys.distance.int_part = sys.distance.fraction = 0; 
	sys.dispersion.int_part = sys.dispersion.fraction = 0; 
	sys.refid.rid_type = 0;
	sys.refid.rid_inet = 0;
	sys.reftime.int_part = sys.reftime.fraction = 0;
	sys.hold = 0;
	sys.peer = NULL;
}

/* 3.4.7 Clear Procedure */
void
clear(peer)
	register struct ntp_peer *peer;
{
	register int i;

	TRACE (3, ("clear: emptied filter for %s",
		   paddr (&peer->src)));
	if (peer->reach != 0)
		peer->hpoll = NTP_MINPOLL;
	peer->estdisp = PEER_MAXDISP;
	for (i = 0; i < NTP_WINDOW; i++)
		peer->filter.offset[i] = 0.0;
	peer->filter.samples = 0;	/* Implementation specific */
	peer->valid = 0;
	peer->org.int_part = peer->org.fraction = 0;
	peer->rec.int_part = peer->rec.fraction = 0;
	peer->xmt.int_part = peer->xmt.fraction = 0;
	if (peer->reach != 0)
		poll_update(peer, NTP_MINPOLL);
	select_clock();
	if (sys.peer != NULL)
		poll_update(sys.peer, NTP_MINPOLL);
}


/* 3.4.8 Poll Update Procedure */
void
poll_update(peer, new_hpoll)
	register struct ntp_peer *peer;
	int new_hpoll;
{
	int interval;

	peer->hpoll = MAX(NTP_MINPOLL, MIN(NTP_MAXPOLL, new_hpoll));

#if	XTAL	/* if crystal controlled clock */
	if (peer == sys.peer)
#endif
		peer->hpoll = NTP_MINPOLL;

	interval = 1 << (MAX(MIN(peer->ppoll, MIN(peer->hpoll, NTP_MAXPOLL)),
		       NTP_MINPOLL));

#ifdef	REFCLOCK
	if (peer->flags & PEER_FL_REFCLOCK)
		interval = 1 << NTP_MINPOLL;
#endif
	if (interval == peer->timer)
		return;

	/* only randomize when poll interval changes */
	if (interval < peer->timer) {
		interval = (double)interval * 
			(double)(random () % 100 / 100.0);
		peer -> timer = interval;
	}
	TRACE (3, ("poll_update: timer %d, poll=%d", peer->timer,
		   interval));
}


/* 3.4.9 Authentication Procedures */
#if	0
encrypt() {}
decrypt() {}
#endif

/* 4.1 Clock Filter Procedure */
/*
 *  The previous incarnation of this code made the assumption that
 *  the value of PEER_FILTER was a power of two and used shifting.
 *  This version has been generalized, so that experimenting with
 *  different PEER_FILTER values should be much easier.
 */

void
clock_filter(peer, new_delay, new_offset)
	register struct ntp_peer *peer;
	double new_delay, new_offset;
{
	double offset[PEER_SHIFT], delay[PEER_SHIFT];
	register double temp, d, w;
	register int i, j, samples;

	if (peer->filter.samples < PEER_SHIFT)
		peer->filter.samples++;
	/*
	 *  Too bad C doesn't have a barrel shifter...
	 */
	for (i = PEER_SHIFT - 1; i; i--) {
		peer->filter.offset[i] = peer->filter.offset[i - 1];
		peer->filter.delay[i] = peer->filter.delay[i - 1];
	}
	peer->filter.offset[0] = new_offset;
	peer->filter.delay[0] = new_delay;

	samples = 0;
	/*
	 *  Now sort the valid (non-zero delay) samples into a temporary
	 *  list by delay.
	 *
	 *  First, build the temp list...
	 */
	for (i = 0; i < peer->filter.samples; i++) {
		if (peer->filter.delay[i] != 0.0) {
			offset[samples] = peer->filter.offset[i];
			delay[samples++] = peer->filter.delay[i];
		}
	}
	/* ..and now sort it. */
	if (samples) {
		for (i = 0; i < samples - 1; i++) {
			for (j = i + 1; j < samples; j++) {
				if (delay[i] > delay[j]) {
					temp = delay[i];
					delay[i] = delay[j];
					delay[j] = temp;
					temp = offset[i];
					offset[i] = offset[j];
					offset[j] = temp;
				}
			}
		}
		/* samples are now sorted by delay */

		peer->estdelay = delay[0];
		peer->estoffset = offset[0];
	}

	temp = 0.0;
	w = 1.0;

	for (i = 0; i < PEER_SHIFT; i++) {
		if (i >= samples)
			d = PEER_MAXDISP;
		else {
			if ((d = offset[i] - offset[0]) < 0)
				d = -d;
			if (d > PEER_MAXDISP)
				d = PEER_MAXDISP;
		}
		temp += d * w;
		/* compute  PEER_FILTER**i  as we go along */
		w *= PEER_FILTER;
	}
	peer->estdisp = temp;
	TRACE (3, ("clock_filter: estdelay %f, estoffset %f, estdisp %f",
		   peer->estdelay, peer->estoffset, peer->estdisp));
}

/* 4.2 Clock Select Procedure */
void
select_clock() 
{
	struct ntp_peer *ptmp, *peer = peer_list.head;
	struct sel_lst {
		struct ntp_peer *peer;
		double distance;
		double precision;
	} sel_lst[X_NTP_CANDIDATES];
	int i, j, stratums, candidates;
	int sanity_check();
	double dtmp;

	candidates = 0;
	stratums = 0;

	while (peer != NULL && candidates < X_NTP_CANDIDATES) {
		/*
		 * Check if this is a candidate for "sys.peer" 
		 */
		peer->flags &= ~(PEER_FL_SANE | PEER_FL_CANDIDATE);
		if(sanity_check(peer)) {
			sel_lst[candidates].peer = peer;
			sel_lst[candidates].distance = peer->estdisp + 
				s_fixed_to_double(&peer->dispersion);
			peer->flags |= PEER_FL_SANE;
			candidates++;
		}
		peer = peer->next;
	}
	TRACE (3, ("select_clock: step1 %d candidates", candidates));
	/*
	 *  If no candidates passed the sanity check, then give up.
	 */
	if (!candidates) {
		if (sys.peer != NULL) {
			advise (LLOG_NOTICE, NULLCP, "Lost NTP peer %s",
				paddr (&sys.peer->src));
		}
		TRACE (3, ("select_clock: no candidates"));
		sys.peer = NULL;
		/*
		 * leave sys.stratum and sys.refid intact after losing 
		 * reachability to all clocks.  After 24 hours, we'll
		 * set the alarm condition if we didn't get any clock
		 * updates.
		 */
		return;
	}

	/* 
	 *  Sort the list.  We assume that sanity_check() above trashed any
	 *  peers which were stratum 0, so we can safely compare stratums
	 *  below.  Sort the list by stratum.  Where stratums are equal, the
	 *  peer with the lowest (peer.estdisp + peer.dispersion) is preferred.
	 */
	for (i = 0; i < candidates - 1; i++) {
		for (j = i + 1; j < candidates; j++) {
			if ((sel_lst[i].peer->stratum > sel_lst[j].peer->stratum) ||
			    ((sel_lst[i].peer->stratum == sel_lst[j].peer->stratum)
			     && (sel_lst[i].distance > sel_lst[j].distance))) {
				ptmp = sel_lst[i].peer;
				dtmp = sel_lst[i].distance;
				sel_lst[i].peer = sel_lst[j].peer;
				sel_lst[i].distance = sel_lst[j].distance;
				sel_lst[j].peer = ptmp;
				sel_lst[j].distance = dtmp;
			}
		}
	}
	       
	TRACE (3, ("select_clock: step2 %d candidates",
		   candidates));

	/* truncate the list at NTP_MAXLIST peers */
	if (candidates > NTP_MAXLIST)
		candidates = NTP_MAXLIST;

	TRACE (3, ("select_clock: step3 %d candidates",
		   candidates));

	/* truncate list where number of different strata exceeds NTP_MAXSTRA */
	for (stratums = 0, i = 1; i < candidates; i++) {
		if (sel_lst[i - 1].peer->stratum != sel_lst[i].peer->stratum) {
			if (++stratums > NTP_MAXSTRA) {
				TRACE (2, ("select_clock: truncated to %d peers", i));
				candidates = i;

				break;
			}
		}
	}
	TRACE (3, ("select_clock: step4 %d candidates",
		   candidates));
	/*
	 * Kick out falsetickers
	 */
	/* now, re-sort the list by peer.stratum and peer.estdelay */
	for (i = 0; i < candidates - 1; i++) {
		for (j = i + 1; j < candidates; j++) {
			if ((sel_lst[i].peer->stratum > sel_lst[j].peer->stratum) ||
			    ((sel_lst[i].peer->stratum == sel_lst[j].peer->stratum)
			     && (sel_lst[i].peer->estdelay >
				 sel_lst[j].peer->estdelay))) {
				ptmp = sel_lst[i].peer;
				sel_lst[i].peer = sel_lst[j].peer;
				sel_lst[j].peer = ptmp;
			}
		}
	}
	while (candidates > 1) {
		double maxdispersion = 0.0, dispersion, weight;
		double min_precision_thres = 10e20, precision_thres;
		short worst = 0; /* shut up GNU CC about unused var */
		TRACE (3, ("select_clock: step5 %d candidates",
			   candidates));
		for (i = 0; i < candidates; i++) {
			/* compute dispersion of candidate `i' relative to the
			   rest of the candidates */
			dispersion = 0.0;
			weight = 1.0;
			sel_lst[i].peer->flags |= PEER_FL_CANDIDATE;
			for (j = 0; j < candidates; j++) {
				dtmp = sel_lst[j].peer->estoffset -
					sel_lst[i].peer->estoffset;
				if (dtmp < 0)
					dtmp = -dtmp;
				dispersion += dtmp * weight;
				weight *= NTP_SELECT;
			}
			/* since we just happen to have this double floating
			   around.. */
			sel_lst[i].distance = dispersion;
			
			precision_thres = NTP_MAXSKW + 1.0/(1<<-sys.precision);
			if (sel_lst[i].peer->precision < 0 &&
			    -sel_lst[i].peer->precision < sizeof(long)*NBBY)
				precision_thres +=
					1.0/(1<<-sel_lst[i].peer->precision);

			sel_lst[i].precision = precision_thres;

			if (dispersion >= maxdispersion) {
				maxdispersion = dispersion;
				worst = i;
			}
			if (precision_thres < min_precision_thres) {
				min_precision_thres = precision_thres;
			}
			TRACE (4, (" peer %s => disp %f prec_th %f",
				   paddr(&sel_lst[i].peer->src),
				   dispersion, precision_thres));
		}
		/*
		 *  Now check to see if the max dispersion is greater than
		 *  the min dispersion limit.  If so, crank again, otherwise
		 *  bail out.
		 */
		if (! (maxdispersion > min_precision_thres)) {
			TRACE (4, (" %d left valid", candidates));
			break;
		}
		
		TRACE (4, (" peer %s => TOSS",
			   paddr(&sel_lst[worst].peer->src)));
		/*
		 *  now, we need to trash the peer with the worst dispersion
		 *  and interate until there is only one candidate peer left.
		 */
		if (worst != candidates - 1) {
			sel_lst[worst].peer->flags &= ~PEER_FL_CANDIDATE;
			for (i = worst, j = worst + 1; j < candidates; )
				sel_lst[i++].peer = sel_lst[j++].peer;
		}
		candidates--;
		/* one more time.. */
	}

	TRACE (3, ("select_clock: step6 %d candidates",
		   candidates));

	/*
	 *  Check to see if current peer is on the list of candidate peers.  If
	 *  don't change sys.peer.  Note that if the first selected clock is
	 *  at a lower stratum, don't even bother; we're going to want to
	 *  switch to it.
	 */
	if (sys.peer != NULL && 
	    (sys.peer->stratum <= sel_lst[0].peer->stratum)) {
		for (i = 0; i < candidates; i++) {
			if (sys.peer == sel_lst[i].peer) {
				/*
				 * The clock we're currently synchronized to
				 * is among the candidate peers.  Don't switch.
				 */
				if (i != 0) {
					/*
					 *  Count instances where the best 
					 *  candidate is different from the
					 *  current clock, thus inhibiting
					 *  clockhopping.
					 */
					peer_sw_inhibited++;
				}
				return;
			}
		}
	}

	/*
	 *  The currently selected peer (if any) isn't on the candidate list.
	 *  Grab the first one and let it be.
	 */

	if (sys.peer != sel_lst[0].peer) {
		if (sys.peer != NULL)
			advise (LLOG_NOTICE, NULLCP,
				"clock: select peer %s stratum %d was %s stratum %d",
				paddr (&sel_lst[0].peer->src),
				sel_lst[0].peer->stratum,
				paddr (&sys.peer->src), sys.peer->stratum);
		else
			advise (LLOG_NOTICE, NULLCP,
				"clock: select peer %s stratum %d was UNSYNCED",
				paddr (&sel_lst[0].peer->src),
				sel_lst[0].peer->stratum);
		
		sys.peer = sel_lst[0].peer;
		peer_switches++;
	}
}

int
sanity_check(peer)
	struct ntp_peer *peer;
{
	TRACE (7, ("Checking peer %s stratum %d",
		   paddr (&peer->src), peer->stratum));

	/* Snity check -1 - not really in consideration */
	if (peer->flags & PEER_FL_SNOOZE)
		return 0;
	/* Sanity check 0. ?? */
	if (!(peer->flags & PEER_FL_SYNC))
		return(0);

	/* Sanity check 1. */
	if (peer->stratum <= 0 || peer->stratum >= NTP_INFIN)
		return(0);

	/* Sanity check 2.
	   if peer.stratum is greater than one (synchronized via NTP),
	   peer.refid must not match peer.dstadr */

	if (peer->stratum > 1) {
		register int i;
		for (i = 1; i < nintf; i++) {
			if ((addrs[i].flags & INTF_VALID) == 0)
				continue;

			if (addrs[i].addr.type == AF_INET &&
			    peer->refid.rid_type == RID_INET &&
			    addrs[i].addr.inet_ad.sin_addr.s_addr
			    == peer->refid.rid_inet) 
				return (0);
			if (addrs[i].addr.type == AF_OSI &&
			    peer->refid.rid_type == RID_PSAP &&
			    psapaddr_cmp (&peer->refid.rid_psap,
					  &addrs[i].addr.psap_ad))
				return 0;
		}

	}

	/* Sanity check 3.
	   Both peer.estdelay and
	   peer.estdisp to be less than NTP_MAXWGT, which insures that the
	   filter register at least half full, yet avoids using data from
	   very noisy associations or broken implementations.  	*/
	if (peer->estdisp > (float)NTP_MAXWGT || 
	    peer->estdelay > (float)NTP_MAXWGT)
		return(0);

	/*  Sanity check 4.
	    The peer clock must be synchronized... and the interval since
	    the peer clock was last updated satisfy
	    
	    peer.org - peer.reftime < NTP.MAXAGE
	    */
	if (peer->leap == ALARM ||
	    (ul_fixed_to_double(&peer->org)
	     - ul_fixed_to_double(&peer->reftime)) >= NTP_MAXAGE)
		return(0);

	TRACE (7, ("That one is certainly qualified %s",
		   paddr (&peer->src)));
	return(1);
}
