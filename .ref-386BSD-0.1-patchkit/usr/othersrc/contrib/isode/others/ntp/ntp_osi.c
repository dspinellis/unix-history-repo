/* OSI ntp stuff */
#ifndef lint
static char *rcsid = "$Header: /f/osi/others/ntp/RCS/ntp_osi.c,v 7.2 91/02/22 09:33:47 mrose Interim $";
#endif

/*
 * Ntp OSI specific code (mainly)
 * $Log:	ntp_osi.c,v $
 * Revision 7.2  91/02/22  09:33:47  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/12/10  23:15:45  mrose
 * isode/
 * 
 * Revision 7.0  90/12/10  17:21:29  mrose
 * *** empty log message ***
 * 
 * Revision 1.3  90/08/14  10:13:54  jpo
 * new protocol version
 * 
 * Revision 1.2  89/12/19  08:32:43  jpo
 * Updated for ISODE 6.0ish
 * 
 * Revision 1.1  89/06/15  20:36:56  jpo
 * Initial revision
 * 
 *
 */

#include "ntp.h"

#include "NTP-ops.h"
#include "NTP-types.h"

void	ros_advise (), acs_advise ();
extern LLog *pgm_log;
extern double WayTooBig;
extern unsigned long clock_watchdog;
extern LLog	*pgm_log;
#ifdef	DEBUG
extern int debug;
extern void dump_pkt();
#endif
extern int trusting, logstats;
extern struct sysdata sys;
extern struct list peer_list;
extern struct ntp_peer *check_peer();
extern unsigned int servport;
extern char *malloc(), *ntoa();
extern double drift_comp, compliance;	/* logical clock variables */
extern void make_new_peer(), tstamp(), clock_update (),
	receive (), clear (), clock_filter (),
	select_clock (), poll_update (), adios (), advise ();
extern struct ntp_peer *find_peer ();
extern int demobilize ();
static double ul_fixed_to_doublep ();
static double ul2_fixed_to_double ();
static void tstamp_osi ();
static void ros_indication ();
static int  acsap_retry ();
static int  acsap_initial ();
static int bindfailed ();
static PE build_bind_arg ();
static int check_accept ();
static int handle_reject ();

static int  TMagic (vecp, vec, td)
int     *vecp;
char   **vec;
struct TSAPdisconnect *td;
{
    int     sd;
    struct TSAPstart tss;
    register struct TSAPstart  *ts = &tss;

    if (TInit (*vecp, vec, ts, td) == NOTOK)
        return NOTOK;
    sd = ts -> ts_sd;

    if (TConnResponse (sd, &ts -> ts_called, ts -> ts_expedited, NULLCP, 0,
            NULLQOS, td) == NOTOK)
        return NOTOK;

    if (TSaveState (sd, vec + 1, td) == NOTOK)
        return NOTOK;
    vec[*vecp = 2] = NULL;

    return OK;
}

void create_osilisten (addr)
char	*addr;
{
	int result_func (), query_func ();
	struct RoSAPindication  rois;
	register struct RoSAPindication *roi = &rois;
	struct TSAPdisconnect tds;
	struct TSAPdisconnect *td = &tds;
	struct PSAPaddr *pa;

	if (addr == NULL)
		return;

	if ((pa = str2paddr (addr)) == NULLPA)
		adios (NULLCP, "Address translation failed for %s", addr);
		

	if (TNetListenAux (&pa -> pa_addr.sa_addr, TMagic, td) == NOTOK)
		adios (NULLCP, "Address listen failed");

	if (RyDispatch (NOTOK, table_NTP_Operations, operation_NTP_update,
			result_func, roi) == NOTOK)
		adios (NULLCP, "RyDispatch failed");
	if (RyDispatch (NOTOK, table_NTP_Operations, operation_NTP_query,
			query_func, roi) == NOTOK)
		adios (NULLCP, "RyDispatch failed");
	TRACE (1, ("Listening on address %s", addr));
}

struct type_NTP_TimeStamp *sstamp ();
struct type_NTP_SmallFixed *sfixed ();
struct type_NTP_ClockIdentifier *srclock ();
Refid *gclock ();
struct timeval *osi_tvp;

extern struct ntp_peer dummy_peer;
extern struct list peer_list;
extern struct sysdata sys;

static void process_packet_osi ();
static void terminate ();

int transmit_osi (peer)
struct ntp_peer *peer;
{
	struct RoSAPindication  rois;
	register struct RoSAPindication *roi = &rois;
	register struct RoSAPpreject   *rop = &roi -> roi_preject;
	struct type_NTP_Packet *packet;
	struct type_NTP_Leap *leap;
	struct intf *ap;
	struct timeval txtv;
	int	result_func ();
	int	i;

	ap = peer -> sock < 0 ? addrs : &addrs[peer->sock];

	TRACE (2, ("Sending OSI packet to %s fd %d if %d",
		   paddr(&ap -> addr), ap -> fd, ap - addrs));

	if (ap -> addr.type != AF_OSI) {
		advise (LLOG_EXCEPTIONS, NULLCP,
			"Wrong address family in send_osi!");
		return -1;
	}
	packet = (struct type_NTP_Packet *) malloc (sizeof *packet);
	leap = (struct type_NTP_Leap *) malloc (sizeof *leap);
	packet -> leap = leap;
	switch (sys.leap & LEAPMASK) {
	    case NO_WARNING:
		leap -> parm = int_NTP_Leap_nowarning;
		break;
	    case PLUS_SEC:
		leap -> parm = int_NTP_Leap_plussecond;
		break;
	    case MINUS_SEC:
		leap -> parm = int_NTP_Leap_minussecond;
		break;
	    case ALARM:
		leap -> parm = int_NTP_Leap_alarm;
		break;
	}
#ifdef notdef
/* really 2 */
	packet -> version = peer -> vers == 1 ? 2 : peer -> vers;
#endif
	packet -> mode = (struct type_NTP_Mode *)
		malloc (sizeof (struct type_NTP_Mode));
	packet -> mode -> parm = peer -> hmode;
	packet -> stratum = sys.stratum;
	packet -> pollInterval = peer -> hpoll;
	packet -> precision = sys.precision;
	packet -> synchDistance = sfixed (&sys.distance);
	packet -> synchDispersion = sfixed (&sys.dispersion);
	packet -> referenceClockIdentifier = srclock (&sys.refid);
	packet -> referenceTimestamp = sstamp (&sys.reftime);
	packet -> originateTimestamp = sstamp (&peer -> org);
	packet -> receiveTimestamp = sstamp (&peer -> rec);

	(void) gettimeofday (&txtv, (struct timezone *)0);
	tstamp_osi (&peer->xmt, &txtv);
	packet -> transmitTimestamp = sstamp (&peer -> xmt);

	switch (RyStub (ap -> fd, table_NTP_Operations, operation_NTP_update,
			RyGenID (ap -> fd), NULLIP, (caddr_t) packet,
			result_func, NULLIFP, ROS_ASYNC, roi)) {
	    case NOTOK:
		ros_advise (rop, "STUB");
		if (ROS_FATAL (rop -> rop_reason))
			terminate (ap, roi);
		break;
	    case OK:
		break;
	    case DONE:
		terminate (ap, roi);
		break;
	}
	free_NTP_Packet (packet);

	peer->pkt_sent++;
	i = peer->reach;	/* save a copy */

	peer->reach = (peer->reach << 1) & NTP_WINDOW_SHIFT_MASK;

	if ((peer->reach == 0) && 
	    ((peer->flags & PEER_FL_CONFIG) == 0) &&
	    (peer != &dummy_peer) && demobilize(&peer_list, peer))
		return 0;

	if (i && peer->reach == 0) {
		advise (LLOG_NOTICE, NULLCP,
			"Lost reachability with %s",
			paddr (&peer->src));
	}

	if (peer->reach == 0)
		clear(peer);

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

	return 0;
}

struct s_fixedpt gfixed ();
struct l_fixedpt gstamp ();

/* ARGSUSED */
int result_func (sd, ryo, rox, in, roi)
int     sd;
struct RyOperation *ryo;
struct RoSAPinvoke *rox;
caddr_t in;
struct RoSAPindication *roi;
{
	struct ntp_peer *peer;
	int peer_mode;
	struct intf *ap;
	struct Naddr *dst;
	int	sock;
	struct type_NTP_Packet *result = (struct type_NTP_Packet *)in;

	for (ap = addrs; ap < &addrs[nintf]; ap++)
		if (ap -> fd == sd)
			break;
	if (ap >= &addrs[nintf])
		return OK;

	dst = &ap -> addr;
	sock = ap - addrs;
	if ((peer_mode = result -> mode -> parm) == int_NTP_Mode_client) {
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
	} else
		peer = check_peer(dst, sock);

	if (peer == NULL) {
		peer = (struct ntp_peer *) malloc(sizeof(struct ntp_peer));
		if (peer == NULL) {
			advise (LLOG_EXCEPTIONS, "malloc", "peer");
			return OK;
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

	if (peer_mode < MODE_SYM_ACT || peer_mode > MODE_BROADCAST) {
		TRACE (1, ("Bogus peer_mode %d from %s", peer_mode,
			   paddr (dst)));
		return OK;
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
		      STRMCMP(result->stratum, >, sys.stratum))) {
			peer->reach |= 1;
			process_packet_osi(dst, result, osi_tvp, peer);
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
		      STRMCMP(result->stratum, >, sys.stratum))) {
			peer->reach |= 1;
			process_packet_osi(dst, result, osi_tvp, peer);
			break;
		}
		/* Note fall-through */
	case ACT_XMIT:
		process_packet_osi(dst, result, osi_tvp, peer);
		poll_update(peer, (int)peer->ppoll);
		transmit_osi(peer);
		break;

	default:
		abort();
	}
	return OK;
}

/* 3.4.3 Packet procedure */

static void process_packet_osi (dst, pkt, tvp, peer)
struct Naddr *dst;
struct type_NTP_Packet *pkt;
struct timeval *tvp;
struct ntp_peer *peer;
{
	double t1, t2, t3, t4, offset, delay;
	short duplicate, bogus;

	duplicate = (pkt->transmitTimestamp->integer == peer->org.int_part) &&
		(pkt->transmitTimestamp->fraction == peer->org.fraction);

	bogus = ((pkt->originateTimestamp -> integer != peer->xmt.int_part) ||
		 (pkt->originateTimestamp -> fraction != peer->xmt.fraction))
		|| (peer->xmt.int_part == 0);

	peer->pkt_rcvd++;
	switch (pkt -> leap -> parm) {
	    case int_NTP_Leap_minussecond:
		peer->leap = MINUS_SEC;
		break;

	    case int_NTP_Leap_alarm:
		peer->leap = ALARM;
		break;

	    case int_NTP_Leap_plussecond:
		peer->leap = PLUS_SEC;
		break;

	    case int_NTP_Leap_nowarning:
		peer->leap = NO_WARNING;
		break;
	}
	peer->stratum = pkt->stratum;
	peer->ppoll = pkt-> pollInterval;
	peer->precision = pkt->precision;
	peer->distance = gfixed (pkt->synchDistance);
	peer->dispersion = gfixed (pkt->synchDispersion);
	peer ->refid = *gclock (pkt -> referenceClockIdentifier);
	peer->reftime = gstamp (pkt->referenceTimestamp);
	peer->org = gstamp (pkt->transmitTimestamp);
	tstamp_osi (&peer->rec, tvp);
	poll_update(peer, (int)peer->hpoll);

	/* 
	 * may want to do something special here for Broadcast Mode peers to
	 * allow these through 
	 */
	if (bogus || duplicate || 
	    (pkt->originateTimestamp -> integer == 0 &&
	     pkt->originateTimestamp -> fraction == 0) ||
	    (pkt->receiveTimestamp -> integer == 0 &&
	     pkt->receiveTimestamp -> fraction == 0)) {
		peer->pkt_dropped++;
		TRACE (3, ("process_packet_osi: dropped duplicate or bogus"));
		return;
	}

	/*
	 *  Now compute local adjusts 
	 */
	t1 = ul2_fixed_to_double(pkt->originateTimestamp);
	t2 = ul2_fixed_to_double(pkt->receiveTimestamp);
	t3 = ul2_fixed_to_double(pkt->transmitTimestamp);
	t4 = ul_fixed_to_doublep(&peer->rec);
/* END Protocol specific stuff */

	/* 
	 * although the delay computation looks different than the one in the
	 * specification, it is correct.  Think about it.
	 */
	delay = (t2 - t1) - (t3 - t4);
	offset = ((t2 - t1) + (t3 - t4)) / 2.0;

	delay += 1.0/(unsigned long)(1L << -sys.precision)
		+ (peer->flags&PEER_FL_REFCLOCK) ? NTP_REFMAXSKW : NTP_MAXSKW;

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

struct l_fixedpt gstamp (ts)
struct type_NTP_TimeStamp *ts;
{
	static struct l_fixedpt fp;

	fp.int_part = ts -> integer;
	fp.fraction = ts -> fraction;
	return fp;
}

struct s_fixedpt gfixed (ts)
struct type_NTP_SmallFixed *ts;
{
	static struct s_fixedpt fp;

	fp.int_part = ts -> integer;
	fp.fraction = ts -> fraction;
	return fp;
}

Refid *gclock (ci)
struct type_NTP_ClockIdentifier *ci;
{
	static Refid rid;
	char	*p;
	struct PSAPaddr *pa;

	switch (ci -> offset) {
	    case type_NTP_ClockIdentifier_referenceClock:
		rid.rid_type = RID_STRING;
		p = qb2str (ci->un.referenceClock);
		(void) strncpy (rid.rid_string, p, 4);
		free (p);
		break;
	    case type_NTP_ClockIdentifier_inetaddr:
		p = qb2str (ci->un.inetaddr);
		rid.rid_inet = inet_addr (p);
		rid.rid_type = RID_INET;
		free (p);
		break;
	    case type_NTP_ClockIdentifier_psapaddr:
		p = qb2str (ci->un.psapaddr);
		pa = str2paddr (p);
		rid.rid_psap = *pa;
		rid.rid_type = RID_PSAP;
		free (p);
		break;
	    default:
		(void) strncpy (rid.rid_string, "????", 4);
		rid.rid_type = RID_STRING;
	}
	return &rid;
}


struct type_NTP_TimeStamp *sstamp (ts)
struct l_fixedpt *ts;
{
	struct type_NTP_TimeStamp *nts;

	nts = (struct type_NTP_TimeStamp *)malloc (sizeof (*nts));
	nts -> integer = ts -> int_part;
	nts -> fraction = ts -> fraction;
	return nts;
}

struct type_NTP_SmallFixed *sfixed (ts)
struct s_fixedpt *ts;
{
	struct type_NTP_SmallFixed *nts;

	nts = (struct type_NTP_SmallFixed *)malloc (sizeof *nts);
	nts -> integer = ts -> int_part;
	nts -> fraction = ts -> fraction;
	return nts;
}

struct type_NTP_ClockIdentifier *srclock (rid)
Refid	*rid;
{
	struct type_NTP_ClockIdentifier *ci;
	char	*p;

	ci = (struct type_NTP_ClockIdentifier *) malloc (sizeof *ci);
	switch (rid -> rid_type) {
	    default:
	    case RID_STRING:
		ci -> offset = type_NTP_ClockIdentifier_referenceClock;
		if (rid -> rid_type == RID_STRING)
			ci -> un.referenceClock =
				str2qb (rid->rid_string, 4, 1);
		else
			ci -> un.referenceClock = str2qb ("??", 2, 1);
		break;

	    case RID_INET:
		{
			struct in_addr in;
			ci -> offset = type_NTP_ClockIdentifier_inetaddr;
			in.s_addr = rid -> rid_inet;
			p = inet_ntoa (in);
			ci -> un.inetaddr = str2qb (p, strlen (p), 1);
			break;
		}

	    case RID_PSAP:
		ci -> offset = type_NTP_ClockIdentifier_psapaddr;
		p = _paddr2str (&rid->rid_psap, NULLNA, -1);
		ci -> un.psapaddr = str2qb (p, strlen(p), 1);
		break;

	}
	return ci;
}


int	recv_osi (ap, tvp)
struct intf *ap;
struct timeval *tvp;
{
	caddr_t out;
	struct RoSAPindication  rois;
	register struct RoSAPindication *roi = &rois;
	register struct RoSAPpreject   *rop = &roi -> roi_preject;

	TRACE (2, ("Received OSI packet from %s", paddr (&ap->addr)));

	osi_tvp = tvp;
	switch (RyWait (ap -> fd, NULLIP, &out, OK, roi)) {
	    case NOTOK:
		if (rop -> rop_reason == ROS_TIMER)
			break;
	    case OK:
	    case DONE:
		ros_indication (ap -> fd, ap, roi);
		break;
	    default:
		advise (LLOG_EXCEPTIONS, NULLCP, "Unknown return from RyWait");
	}
	return 0;
}

static void ros_indication (fd, ap, roi)
int	fd;
struct intf *ap;
register struct RoSAPindication *roi;
{
	int	    result;

	switch (roi -> roi_type) {
	    case ROI_INVOKE: 
	    case ROI_RESULT: 
	    case ROI_ERROR: 
		advise (LLOG_EXCEPTIONS, NULLCP, "unexpected indication type=%d",
			roi -> roi_type);
		terminate (ap, roi);
		break;

	    case ROI_UREJECT: 
	{
		register struct RoSAPureject   *rou = &roi -> roi_ureject;

		if (rou -> rou_noid)
			advise (LLOG_EXCEPTIONS, NULLCP,
				"RO-REJECT-U.INDICATION/%d: %s",
				fd, RoErrString (rou -> rou_reason));
		else
			advise (LLOG_EXCEPTIONS, NULLCP,
				"RO-REJECT-U.INDICATION/%d: %s (id=%d)",
				fd, RoErrString (rou -> rou_reason),
				rou -> rou_id);
	}
		break;

	    case ROI_PREJECT: 
	{
		register struct RoSAPpreject   *rop = &roi -> roi_preject;

		ros_advise (rop, "RO-REJECT-P.INDICATION");
		if (ROS_FATAL (rop -> rop_reason)) {
			terminate (ap, roi);
		}
			
	}
		break;

	    case ROI_FINISH: 
	{
		register struct AcSAPfinish *acf = &roi -> roi_finish;
		struct AcSAPindication  acis;
                register struct AcSAPabort *aca = &acis.aci_abort;

		advise (LLOG_EXCEPTIONS, NULLCP, "A-RELEASE.INDICATION/%d: %d",
			fd, acf -> acf_reason);

		result = AcRelResponse (fd, ACS_ACCEPT, ACR_NORMAL, NULLPEP, 0,
					&acis);

		ACFFREE (acf);

		if (result == NOTOK)
			acs_advise (aca, "A-RELEASE.RESPONSE");
		terminate (ap, roi);
		break;
	}
		/* NOTREACHED */

	    default: 
		advise (LLOG_EXCEPTIONS, NULLCP,
			"unknown indication type=%d", roi -> roi_type);
	}
}

static void terminate (ap, roi)
struct intf *ap;
struct RoSAPindication *roi;
{
	struct AcSAPindication  acsis;
	extern struct list peer_list;
	struct ntp_peer *peer;
	int	fd = ap -> fd;

	(void) AcUAbortRequest (fd, NULLPEP, 0, &acsis);
	(void) RyLose (fd, roi);
	if (fd >= 0) {
		FD_CLR (fd, &globmask);
		FD_CLR (fd, &globwmask);
		if (fd == selfds + 1)
			selfds --;
		ap -> fd = -1;
	}
	
	if ((peer = find_peer (ap - addrs)) != NULL) {
		peer-> flags &= ~PEER_FL_CONNSTATE;
		peer -> reach = 0;
		clear (peer);
	}
	ap -> flags = 0;

	advise (LLOG_NOTICE, NULLCP,
		"Connection on %d if %d TERMINATED", ap -> fd, fd);
}

void iso_init (vecp, vec, fd)
int vecp;
char **vec;
int fd;
{
	struct intf *ap;
	int	acount;

	ap = getintf (&acount);
	ap->name = "OSI";
	ap->addr.type = AF_OSI;
	ap->fd = fd;
	ap -> flags = INTF_ACCEPTING;
	if (vecp > 0)
		ap -> vec[0] = strdup (vec[0]);
	if (vecp > 1)
		ap -> vec[1] = strdup (vec[1]);
	if (vecp > 2)
		ap -> vec[2] = strdup (vec[2]);
	if (vecp > 3)
		ap -> vec[3] = strdup (vec[3]);
	ap -> vecp = vecp;
	ap -> vec[vecp] = NULLCP;
	ap -> inum = acount;
	FD_SET (fd, &globmask);
	if (fd >= selfds)
		selfds = fd + 1;
	TRACE (1, ("Incoming Connection pending on %d", fd));
}

int iso_accept (ap)
struct intf *ap;
{
	int     result,
		i,
		sd;
	struct AcSAPstart   acss;
	register struct AcSAPstart *acs = &acss;
	struct AcSAPindication  acis;
	register struct AcSAPindication *aci = &acis;
	register struct AcSAPabort   *aca = &aci -> aci_abort;
	register struct PSAPstart *ps = &acs -> acs_start;
	struct PSAPaddr *pa;
	struct RoSAPindication  rois;
	register struct RoSAPindication *roi = &rois;
	register struct RoSAPpreject   *rop = &roi -> roi_preject;
	struct Naddr *adr;
	struct ntp_peer *peer;
	struct type_NTP_BindArgument *bindarg;
	struct type_NTP_BindResult *bindresult;
	PE	*pep, pe;
	int version, mode;

	if (AcInit (ap -> vecp, ap -> vec, acs, aci) == NOTOK) {
		acs_advise (aca, "Initialisation fails");
		return NOTOK;
	}
	for (i = 0; i < ap -> vecp; i++) {
		free (ap -> vec[i]);
		ap -> vec[i] = NULLCP;
	}
	ap -> vecp = 0;
	TRACE (1,("A-ASSOCIATE.INDICATION: <%d, %s, %s, %s, %d>",
		  acs -> acs_sd, oid2ode (acs -> acs_context),
		  sprintaei (&acs -> acs_callingtitle),
		  sprintaei (&acs -> acs_calledtitle), acs -> acs_ninfo));
	sd = acs -> acs_sd;
	if (acs -> acs_ninfo > 0) {
		PLOG (pgm_log, print_NTP_BindArgument, acs -> acs_info[0],
		      "NTP.BindArgument", 1);
		if (decode_NTP_BindArgument (acs -> acs_info[0], 1,
					     NULLINTP, NULLVP,
					     &bindarg) == NOTOK) {
			advise (LLOG_EXCEPTIONS, NULLCP,
				"bind decode failed [%s]", PY_pepy);
			free_NTP_BindArgument(bindarg);
			return bindfailed (ap, acs, int_NTP_reason_badarg,
					   NULLCP);
		}
		if (bindarg ->  psap == NULL)
			pa = &acs -> acs_start.ps_calling;
		else {
			char	*p;

			p = qb2str(bindarg -> psap);
			if ((pa = str2paddr (p)) == NULLPA)
				pa = &acs -> acs_start.ps_calling;
			free (p);
		}

		if (bit_test (bindarg ->  version,
			      bit_NTP_version_version__2))
			version = 2;
		else if (bit_test (bindarg -> version,
				   bit_NTP_version_version__1))
			version = 1;
		else {
			free_NTP_BindArgument(bindarg);
			return bindfailed (ap, acs,
					   int_NTP_reason_version,
					   "No acceptable version");
		}
		if (bindarg  -> authentication) {
			advise (LLOG_NOTICE,
				"Connection specifies authentication");
			free_NTP_BindArgument(bindarg);
			return bindfailed (ap, acs,
					   int_NTP_reason_validation,
					   "Authentication not supported");
		}
		switch (bindarg -> mode -> parm) {
		    case int_NTP_BindMode_normal:
			mode = PEERMODE_NORMAL;
			break;
		    case int_NTP_BindMode_query:
			mode = PEERMODE_QUERY;
			break;

		    default:
			free_NTP_BindArgument(bindarg);
			return bindfailed (ap, acs, int_NTP_reason_badarg,
					   "Unknown mode");
		}
		free_NTP_BindArgument(bindarg);

		bindresult = (struct type_NTP_BindResult *)
			calloc (1, sizeof *bindresult);
		bindresult -> version = version;
		bindresult -> mode = (struct type_NTP_BindMode *)
			calloc (1, sizeof *bindresult -> mode);
		bindresult -> mode -> parm =
			mode == PEERMODE_QUERY ?
				int_NTP_BindMode_query :
		int_NTP_BindMode_normal;
		if (encode_NTP_BindResult (&pe, 1, NULLINT, NULLCP,
					   bindresult) == NOTOK) {
			advise (LLOG_EXCEPTIONS, NULLCP,
				"encode failed [%s]", PY_pepy);
			return bindfailed (ap, acs,
					   int_NTP_reason_congested,
					   "Can't build result");
		}
		PLOG (pgm_log, print_NTP_BindResult, pe,
		      "NTP.BindResult", 0);
		pe -> pe_context = 3;
		free_NTP_BindResult (bindresult);
		pep = &pe;
	}
	else {
		pa = &acs -> acs_start.ps_calling;
		mode = int_NTP_BindMode_normal;
		version = 1;
		pep = NULLPEP;
	}
	ap->addr.psap_ad = *pa;
	ap->addr.type = AF_OSI;
	ap ->flags = INTF_VALID;
	ap->fd = sd;
	adr = &ap->addr;
	pa = &adr->psap_ad;

	result = AcAssocResponse (sd, ACS_ACCEPT,
				  ACS_USER_NULL,
				  NULLOID, NULLAEI, NULLPA, NULLPC,
				  ps -> ps_defctxresult,
				  ps -> ps_prequirements,
				  ps -> ps_srequirements, SERIAL_NONE,
				  ps -> ps_settings, &ps -> ps_connect,
				  pep, pep == NULLPEP ? 0 : 1, aci);

	if (pep)
		pe_free (*pep);
	ACSFREE (acs);

	if (result == NOTOK) {
		acs_advise (aca, "Association response failed");
		terminate (ap, roi);
		return NOTOK;
	}
	if (RoSetService (sd, RoPService, roi) == NOTOK) {
		ros_advise (rop, "set RO/PS fails");
		terminate (ap, roi);
		return NOTOK;
	}
		
	ap -> flags |= INTF_VALID;

	FD_SET (sd, &globmask);
	if (sd >= selfds)
		selfds = sd + 1;
	result = 0;
	for (peer = peer_list.head; peer; peer = peer->next) {
		if (peer->src.type != AF_OSI)
			continue;
		if (psapaddr_cmp (pa, &peer->src.psap_ad)) {
			result = 1;
			peer -> flags |= PEER_FL_CONNECTED;
			peer -> sock = ap -> inum;
			peer -> vers = version;
			peer -> mode = mode;
		}
	}
	if (result == 0 && mode == int_NTP_BindMode_normal) {
		peer = (struct ntp_peer *) malloc(sizeof(struct ntp_peer));
		if (peer == NULL) {
			advise (LLOG_EXCEPTIONS, "malloc", "peer");
			return OK;
		}
		make_new_peer(peer);
		peer -> src = ap->addr;
		peer -> flags |= PEER_FL_CONNECTED;
		peer->sock = ap -> inum;
		peer->hmode = MODE_SYM_PAS;
		peer->vers = version;
		peer->mode = mode;
		peer->reach = 0;
		clear(peer);
		if (trusting)
			peer -> flags |= PEER_FL_SYNC;
		enqueue(&peer_list, peer);
	}
	peer = find_peer (ap -> inum);
	TRACE (2, ("Association accepted from %s sd %d if %d",
		   paddr (adr), sd, ap -> inum));
	if (peer && peer -> flags & PEER_FL_CONFIG)
		transmit_osi (peer);
	return OK;
}

static int bindfailed (ap, acs, type, msg)
struct intf *ap;
struct AcSAPstart *acs;
int	type;
char	*msg;
{
	PE	pe;
	register struct PSAPstart *ps = &acs -> acs_start;
	struct type_NTP_BindError *binderr;
	struct RoSAPindication rois;
	struct RoSAPindication *roi = &rois;
	struct AcSAPindication  acis;
	register struct AcSAPindication *aci = &acis;

	binderr = (struct type_NTP_BindError *)
		calloc (1, sizeof *binderr);

	binderr -> reason = type;
	if (msg != NULLCP)
		binderr -> supplementary = str2qb (msg, strlen (msg), 1);
	if (encode_NTP_BindError (&pe, 1, NULLINT, NULLCP, binderr) == NOTOK) {
		advise (LLOG_EXCEPTIONS, NULLCP, "ecode binderror failed [%s]",
			PY_pepy);
		ACSFREE (acs);
		terminate (ap, roi);
		return NOTOK;
	}
	PLOG (pgm_log, print_NTP_BindError, pe, "NTP.BindError", 0);
	pe -> pe_context = 3;
	free_NTP_BindError (binderr);
	AcAssocResponse (ap -> fd, ACS_REJECT, ACS_USER_NULL,
			 NULLOID, NULLAEI, NULLPA, NULLPC,
			 ps -> ps_defctxresult,
			 ps -> ps_prequirements,
			 ps -> ps_srequirements, SERIAL_NONE,
			 ps -> ps_settings, &ps -> ps_connect,
			 &pe, 1, aci);
	pe_free (pe);
	ACSFREE (acs);
	terminate (ap, roi);
	return NOTOK;
}

char	*mycontext = "ntp";
char	*mypci = "ntp pci";

int make_osi_conn (peer, addr)
struct ntp_peer *peer;
char	*addr;
{
	int	result = NOTOK;
	struct intf *ap;
	struct RoSAPindication rois;
	register struct RoSAPindication *roi = &rois;

	switch (peer->flags & PEER_FL_CONNSTATE) {
	    case PEER_FL_CONNECTED:
		return OK;
	    case 0:
		switch (acsap_initial (peer, addr, roi)) {
		    case NOTOK:
			return NOTOK;
		    case CONNECTING_1:
			ap = &addrs[peer->sock];
			peer -> flags |= PEER_FL_CONINP1;
			FD_SET (ap -> fd, &globwmask);
			return NOTOK;

		    case CONNECTING_2:
			ap = &addrs[peer->sock];
			peer -> flags |= PEER_FL_CONINP2;
			FD_SET (ap -> fd, &globmask);
			return NOTOK;
		    case DONE:
			ap = &addrs[peer->sock];
			peer -> flags &= ~ PEER_FL_CONNSTATE;
			peer -> flags |= PEER_FL_CONNECTED;
			FD_CLR (ap -> fd, &globwmask);
			FD_SET (ap -> fd, &globmask);
			ap -> flags = INTF_VALID;
			return OK;
		}
		return NOTOK;

	    case PEER_FL_CONINP1:
		ap = &addrs[peer->sock];
		switch (result = acsap_retry (peer, roi)) {
		    case CONNECTING_1:
			peer -> flags &= ~ PEER_FL_CONNSTATE;
			peer -> flags |= PEER_FL_CONINP1;
			FD_CLR (ap -> fd, &globmask);
			FD_SET (ap -> fd, &globwmask);
			return NOTOK;

		    case CONNECTING_2:
			peer -> flags &= ~ PEER_FL_CONNSTATE;
			peer -> flags |= PEER_FL_CONINP2;
			FD_CLR (ap -> fd, &globwmask);
			FD_SET (ap -> fd, &globmask);
			return NOTOK;
		    case NOTOK:
			terminate (ap, roi);
			return NOTOK;
		    case DONE:
			peer -> flags &= ~ PEER_FL_CONNSTATE;
			peer -> flags |= PEER_FL_CONNECTED;
			FD_CLR (ap -> fd, &globwmask);
			FD_SET (ap -> fd, &globmask);
			ap -> flags = INTF_VALID;
			return OK;
		}
		break;

	    case PEER_FL_CONINP2:
		ap = &addrs[peer->sock];
		switch( result = acsap_retry (peer, roi)) {
		    case CONNECTING_1:
			peer -> flags &= ~ PEER_FL_CONNSTATE;
			peer -> flags |= PEER_FL_CONINP1;
			FD_CLR (ap -> fd, &globmask);
			FD_SET (ap -> fd, &globwmask);
			return OK;
			break;
		    case CONNECTING_2:
			peer -> flags &= ~ PEER_FL_CONNSTATE;
			peer -> flags |= PEER_FL_CONINP2;
			FD_CLR (ap -> fd, &globwmask);
			FD_SET (ap -> fd, &globmask);
			return OK;
			break;
		    case DONE:
			peer -> flags &= ~ PEER_FL_CONNSTATE;
			peer -> flags |= PEER_FL_CONNECTED;
			FD_CLR (ap -> fd, &globwmask);
			FD_SET (ap -> fd, &globmask);
			ap -> flags = INTF_VALID;
			return OK;
		    case NOTOK:
			terminate (ap, roi);
			return NOTOK;
		}
	}
	return result == DONE ? OK : NOTOK;
}

static int acsap_initial (peer, addr, roi)
struct ntp_peer *peer;
char *addr;
struct RoSAPindication *roi;
{
	int	    sd;
	struct SSAPref sfs;
	register struct SSAPref *sf;
	register struct PSAPaddr *pa, *pa2;
	struct AcSAPconnect accs;
	register struct AcSAPconnect   *acc = &accs;
	struct AcSAPindication  acis;
	register struct AcSAPindication *aci = &acis;
	register struct AcSAPabort   *aca = &aci -> aci_abort;
	OID	    ctx,
		pci;
	PE	pep[1];
	struct PSAPctxlist pcs;
	register struct PSAPctxlist *pc = &pcs;
	struct intf *ap;
	int	acount;
	int	result;

	if (peer -> src.type != AF_OSI)
		return NOTOK;
	ap = getintf (&acount);
	ap->addr = peer->src;
	ap->fd = -1;
	ap->name = "OSI";
	peer-> sock = acount;

	pa = &ap -> addr.psap_ad;
	TRACE (2, ("Making connection to %s", paddr2str (pa, NULLNA)));

	if ((pa2 = str2paddr (addr)) == NULLPA) {
		advise (LLOG_EXCEPTIONS, NULLCP, "Can't translate %s", addr);
                return NOTOK;
        }

	pep[0] = build_bind_arg (pa2, peer);
	if ((ctx = ode2oid (mycontext)) == NULLOID) {
		advise (LLOG_EXCEPTIONS, NULLCP,
			"%s: unknown object descriptor", mycontext);
		return NOTOK;
	}
	if ((ctx = oid_cpy (ctx)) == NULLOID) {
		advise (LLOG_EXCEPTIONS, "memory", "out of");
		return NOTOK;
	}
	if ((pci = ode2oid (mypci)) == NULLOID) {
		advise (LLOG_EXCEPTIONS, NULLCP,
			"%s: unknown object descriptor", mypci);
		return NOTOK;
	}
	if ((pci = oid_cpy (pci)) == NULLOID) {
		advise (LLOG_EXCEPTIONS, "memory", "out of");
		return NOTOK;
	}
	pc -> pc_nctx = 1;
	pc -> pc_ctx[0].pc_id = 1;
	pc -> pc_ctx[0].pc_asn = pci;
	pc -> pc_ctx[0].pc_atn = NULLOID;

	if ((sf = addr2ref (PLocalHostName ())) == NULL) {
		sf = &sfs;
		(void) bzero ((char *) sf, sizeof *sf);
	}

	result = AcAsynAssocRequest (ctx, NULLAEI, NULLAEI, pa2, pa,
				     pc, NULLOID,
				     0, ROS_MYREQUIRE, SERIAL_NONE, 0, sf,
				     pep, 1, NULLQOS,
				     acc, aci, 1);
	pe_free (pep[0]);
	switch (result) {
	    case NOTOK:
		acs_advise (aca, "A-ASSOCIATE.REQUEST");
		ap -> flags = 0;
		return result;

	    case CONNECTING_1:
	    case CONNECTING_2:
		ap -> fd = sd = acc -> acc_sd;
		ap -> flags |= INTF_PENDING;
		FD_SET (sd, &globwmask);
		if (sd >= selfds)
			selfds = sd + 1;
		ACCFREE (acc);
		return result;
	    case DONE:
		if (acc -> acc_result != ACS_ACCEPT)
			return handle_reject (acc, ap);
		return check_accept (acc, ap, peer);

	    default:
		advise (LLOG_EXCEPTIONS, NULLCP, "Unknown response (%d)",
			result);
		break;
	}
	return NOTOK;
}

static int check_accept (acc, ap, peer)
struct AcSAPconnect *acc;
struct intf *ap;
struct ntp_peer *peer;
{
	struct RoSAPindication rois;
	struct RoSAPindication *roi = &rois;
	struct RoSAPpreject *rop = &roi -> roi_preject;
	int	sd;
	int	version, mode;
	struct type_NTP_BindResult *bindres;

	if (acc -> acc_ninfo > 0) {
		PLOG (pgm_log, print_NTP_BindResult, acc -> acc_info[0],
		      "NTP.BindResult", 1);
		if (decode_NTP_BindResult (acc -> acc_info[0], 1,
					   NULLINTP, NULLVP,
					   &bindres) == NOTOK) {
			advise (LLOG_EXCEPTIONS,  NULLCP,
				"decode bindresult failed [%s]", PY_pepy);
			terminate (ap, roi);
			return NOTOK;
		}
		version = bindres -> version;
		mode = bindres -> mode -> parm;
		free_NTP_BindResult (bindres);
	}
	else {
		version = 1;
		mode = int_NTP_BindMode_normal;
	}
					   
	sd = acc -> acc_sd;
		
	ACCFREE (acc);

	if (RoSetService (sd, RoPService, roi) == NOTOK) {
		ros_advise (rop, "set RO/PS fails");
		terminate (ap, roi);
		ap -> flags = 0;
		return NOTOK;
	}
	FD_SET (sd, &globmask);
	if (sd >= selfds)
		selfds = sd + 1;

	peer -> flags |= PEER_FL_CONNECTED;
	peer -> vers = version;
	peer -> mode = mode;
	ap -> fd = sd;
	ap->flags = INTF_VALID;

	TRACE (1,  ("CONNECTED to %s on %d if %d",
		    paddr2str (&peer->src.psap_ad, NULLNA),
		    sd, peer->sock));
	return DONE;
}

static PE build_bind_arg (psap, peer)
struct PSAPaddr *psap;
struct ntp_peer *peer;
{
	struct type_NTP_BindArgument *bindarg;
	char	*str;
	PE	pe;

	bindarg = (struct type_NTP_BindArgument *)
		calloc (1, sizeof *bindarg);
	str = _paddr2str (psap, NULLNA, -1);
	bindarg -> psap = str2qb (str, strlen(str), 1);
	bindarg -> version =
		pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM,
			  PE_PRIM_BITS);
	bit_on (bindarg -> version,
		bit_NTP_version_version__1);
	bit_on (bindarg -> version,
		bit_NTP_version_version__2);

	bindarg -> mode = (struct type_NTP_BindMode *)
		calloc (1, sizeof *bindarg-> mode);
	bindarg -> mode -> parm =
		int_NTP_BindMode_normal;
	if (encode_NTP_BindArgument (&pe, 1, NULLINT, NULLCP,
				     bindarg) == NOTOK) {
		pe = NULLPE;
		advise (LLOG_EXCEPTIONS, NULLCP,
			"encode Bindargument failed [%s]", PY_pepy);
	}
	else	{
		pe -> pe_context = 3;
		PLOG (pgm_log, print_NTP_BindArgument, pe,
		      "NTP.BindArgument", 0);
	}
	free_NTP_BindArgument (bindarg);
	return pe;
}

	
static int acsap_retry (peer, roi)
struct ntp_peer *peer;
struct RoSAPindication *roi;
{
	struct AcSAPconnect accs;
        register struct AcSAPconnect   *acc = &accs;
        struct AcSAPindication  acis;
        register struct AcSAPindication *aci = &acis;
        register struct AcSAPabort *aca = &aci -> aci_abort;
	int	result;
	struct intf *ap;

	TRACE (2, ("retry request on %s", paddr (&peer->src)));
	ap = &addrs[peer->sock];

	switch (result = AcAsynRetryRequest (ap -> fd, acc, aci)) {
	    case CONNECTING_1:
	    case CONNECTING_2:
		return result;
	    case NOTOK:
		acs_advise (aca, "A-ASSOCIATE.REQUEST");
		ap -> flags = 0;
		return NOTOK;

	    case DONE:
		if (acc -> acc_result != ACS_ACCEPT)
			return handle_reject (acc, ap);
		return check_accept (acc, ap, peer);

	    default:
		advise (LLOG_EXCEPTIONS, NULLCP,
			"Bad response from retry %d", result);
		terminate (ap, roi);
		break;
	}
	return NOTOK;
}

static int handle_reject (acc, ap)
struct AcSAPconnect *acc;
struct intf *ap;
{
	if (acc -> acc_ninfo > 0) {
		struct type_NTP_BindError *binderr;
		char	*cp = NULLCP;

		PLOG (pgm_log, print_NTP_BindError, acc -> acc_info[0],
		      "NTP.BindError", 1);
		if (decode_NTP_BindError (acc -> acc_info[0], 1,
					  NULLINTP, NULLVP,
					  &binderr) != NOTOK) {
			if (binderr -> supplementary)
				cp = qb2str (binderr -> supplementary);
			switch (binderr -> reason) {
			    case int_NTP_reason_refused:
				advise (LLOG_EXCEPTIONS,
					"connection refused: %s",
					cp ? cp : "");
				break;
			    case int_NTP_reason_validation:
				advise (LLOG_EXCEPTIONS,
					"validation failure: %s",
					cp ? cp : "");
				break;
			    case int_NTP_reason_version:
				advise (LLOG_EXCEPTIONS,
					"version mismatch: %s",
					cp ? cp : "");
				break;
			    case int_NTP_reason_badarg:
				advise (LLOG_EXCEPTIONS,
					"bad connect argument: %s",
					cp ? cp : "");
				break;
			    case int_NTP_reason_congested:
				advise (LLOG_EXCEPTIONS,
					"congested: %s",
					cp ? cp : "");
				break;
			    default:
				advise (LLOG_EXCEPTIONS, NULLCP,
					"Unknown reason (%d) %s",
					binderr -> reason,
					cp ? cp : "");
				break;
			}
			free_NTP_BindError (binderr);
		}
		else
			advise (LLOG_EXCEPTIONS, NULLCP,
				"decode bind error failed [%s]", PY_pepy);
	}
	else
		advise (LLOG_EXCEPTIONS, NULLCP,
			"Connection failed: %s",
			AcErrString (acc -> acc_result));
	ACCFREE (acc);
	ap -> flags = 0;
	return NOTOK;
}

void    ros_advise (rop, event)
register struct RoSAPpreject *rop;
char   *event;
{
	char    buffer[BUFSIZ];

	if (rop -> rop_cc > 0)
		(void) sprintf (buffer, "[%s] %*.*s",
				RoErrString (rop -> rop_reason),
				rop -> rop_cc, rop -> rop_cc, rop -> rop_data);
	else
		(void) sprintf (buffer, "[%s]",
				RoErrString (rop -> rop_reason));

	advise (LLOG_EXCEPTIONS, NULLCP, "%s: %s", event, buffer);
}

void    acs_advise (aca, event)
register struct AcSAPabort *aca;
char   *event;
{
	char    buffer[BUFSIZ];

	if (aca -> aca_cc > 0)
		(void) sprintf (buffer, "[%s] %*.*s",
				AcErrString (aca -> aca_reason),
				aca -> aca_cc, aca -> aca_cc, aca -> aca_data);
	else
		(void) sprintf (buffer, "[%s]",
				AcErrString (aca -> aca_reason));

	advise (LLOG_EXCEPTIONS, NULLCP, "%s: %s (source %d)", event, buffer,
                aca -> aca_source);
}

static double
ul2_fixed_to_double(t)
struct type_NTP_TimeStamp *t;
{
	double a, b;
#ifdef	GENERIC_UNS_BUG
	register int i;

	i = t->fraction;
	a = (long)((i >> 1) & 0x7fffffff);
	a *= 2.0;
	if (i & 1)
		a += 1.0;
	a = a / (4.294967296e9);	/* shift dec point over by 32 bits */
	i = t->integer;
	b = (long)((i >> 1) & 0x7fffffff);
	b *= 2.0;
	if (i & 1)
		b += 1.0;
#else	/* GENERIC_UNS_BUG */
	a = (unsigned long) t->fraction;
#ifdef	VAX_COMPILER_FLT_BUG
	if (a < 0.0) a += 4.294967296e9;
#endif
	a = a / (4.294967296e9);/* shift dec point over by 32 bits */
	b = (unsigned long) t->integer;
#ifdef	VAX_COMPILER_FLT_BUG
	if (b < 0.0) b += 4.294967296e9;
#endif
#endif	/* GENERIC_UNS_BUG */
	return (a + b);
}

static double
ul_fixed_to_doublep(t)
	struct l_fixedpt *t;
{
	double a, b;
#ifdef	GENERIC_UNS_BUG
	register int i;

	i = t->fraction;
	a = (long)((i >> 1) & 0x7fffffff);
	a *= 2.0;
	if (i & 1)
		a += 1.0;
	a = a / (4.294967296e9);	/* shift dec point over by 32 bits */
	i = t->int_part;
	b = (long)((i >> 1) & 0x7fffffff);
	b *= 2.0;
	if (i & 1)
		b += 1.0;
#else	/* GENERIC_UNS_BUG */
	a = (unsigned long) t->fraction;
#ifdef	VAX_COMPILER_FLT_BUG
	if (a < 0.0) a += 4.294967296e9;
#endif
	a = a / (4.294967296e9);/* shift dec point over by 32 bits */
	b = (unsigned long) t->int_part;
#ifdef	VAX_COMPILER_FLT_BUG
	if (b < 0.0) b += 4.294967296e9;
#endif
#endif	/* GENERIC_UNS_BUG */
	return (a + b);
}

#ifdef	SUN_FLT_BUG
static void
tstamp_osi (stampp, tvp)
struct l_fixedpt *stampp;
struct timeval *tvp;
{
	int tt;
	double dd;

	stampp->int_part = JAN_1970 + tvp->tv_sec;
	dd = (float) tvp->tv_usec / 1000000.0;
	tt = dd * 2147483648.0;
	stampp->fraction = tt << 1;
}
#else
static void
tstamp_osi (stampp, tvp)
	struct l_fixedpt *stampp;
	struct timeval *tvp;
{
	stampp->int_part = JAN_1970 + tvp->tv_sec;
	stampp->fraction = (float) tvp->tv_usec * 4294.967295;
}
#endif

struct type_NTP_ClockIdentifier *cli_refid (refid)
Refid refid;
{
	struct type_NTP_ClockIdentifier *rid;
	char	*cp;

	rid = (struct type_NTP_ClockIdentifier *) malloc (sizeof *rid);

	switch (refid.rid_type) {
	    case 0:
		free (rid);
		return NULL;

	    case RID_STRING:
		rid -> offset = type_NTP_ClockIdentifier_referenceClock;
		rid -> un.referenceClock = str2qb (refid.rid_string,
						   strlen(refid.rid_string),
						   1);
		break;
	    case RID_INET:
		rid -> offset = type_NTP_ClockIdentifier_inetaddr;
		cp = ntoa(&refid.rid_inet);
		rid -> un.inetaddr = str2qb(cp, strlen(cp), 1);
		break;
	    case RID_PSAP:
		rid -> offset = type_NTP_ClockIdentifier_psapaddr;
		cp = paddr2str (&refid.rid_psap, NULLNA);
		rid -> un.inetaddr = str2qb (cp, strlen (cp), 1);
		break;
	}
	return rid;
}


struct type_NTP_ClockInfo *peer2clock (peer)
struct ntp_peer *peer;
{
	struct type_NTP_ClockInfo *ci;
	char	*cp;

	ci = (struct type_NTP_ClockInfo *) calloc (1, sizeof *ci);

	if (peer -> sock < 0)
		cp = "none";
	else
		cp = paddr (&addrs[peer->sock].addr);
	ci -> localAddress = str2qb (cp, strlen (cp), 1);

	cp = paddr (&peer -> src);
	ci -> remoteAddress = str2qb (cp, strlen (cp), 1);
	ci -> flags = pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_BITS);
#define setflbit(x,y) if (peer -> flags & (x)) bit_on (ci -> flags, (y))
	setflbit (PEER_FL_CONFIG, bit_NTP_flags_configured);
	setflbit (PEER_FL_AUTHENABLE, bit_NTP_flags_authentable);
	setflbit (PEER_FL_SANE, bit_NTP_flags_sane);
	setflbit (PEER_FL_CANDIDATE, bit_NTP_flags_candidate);
	setflbit (PEER_FL_SYNC, bit_NTP_flags_sync);
	setflbit (PEER_FL_BCAST, bit_NTP_flags_broadcast);
	setflbit (PEER_FL_REFCLOCK, bit_NTP_flags_referenceClock);
	setflbit (PEER_FL_SELECTED, bit_NTP_flags_selected);
	setflbit (PEER_FL_SNOOZE, bit_NTP_flags_inactive);
	if (sys.peer == peer)
		bit_on (ci -> flags, bit_NTP_flags_selected);
#undef setflbit
	ci -> packetsSent = peer -> pkt_sent;
	ci -> packetsReceived = peer -> pkt_rcvd;
	ci -> packetsDropped = peer -> pkt_dropped;
	ci -> timer = peer -> timer;
	ci -> leap = (struct type_NTP_Leap *) calloc (1, sizeof *ci -> leap);
	ci -> leap -> parm = peer -> leap;
	ci -> stratum = peer -> stratum;
	ci -> ppoll = peer -> ppoll;
	ci -> hpoll = peer -> hpoll;
	ci -> precision = peer -> precision;
	ci -> reachability = peer -> reach & NTP_WINDOW_SHIFT_MASK;
	ci -> estdisp = peer -> estdisp * 1000.0;
	ci -> estdelay = peer -> estdelay * 1000.0;
	ci -> estoffset = peer -> estoffset * 1000.0;
	ci -> reference = cli_refid (peer -> refid);
	ci -> reftime = sstamp (&peer -> reftime);
	ci -> filters = NULL;
	return ci;
}

int query_func (sd, ryo, rox, in, roi)
int     sd;
struct RyOperation *ryo;
struct RoSAPinvoke *rox;
caddr_t in;
struct RoSAPindication *roi;
{
	struct type_NTP_ClockInfoList *clbase, *cl;
	struct ntp_peer *peer;

	clbase = cl = NULL;
	for (peer = peer_list.head; peer != NULL; peer = peer -> next) {
		if (clbase == NULL)
			clbase = cl = (struct type_NTP_ClockInfoList *)
				calloc (1, sizeof *cl);
		else {
			cl -> next = (struct type_NTP_ClockInfoList *)
				calloc (1, sizeof *cl);
			cl = cl -> next;
		}

		cl -> ClockInfo = peer2clock (peer);
	}
	if (RyDsResult (sd, rox -> rox_id, (caddr_t) clbase,
			ROS_NOPRIO, roi) == NOTOK)
		ros_advise (&roi -> roi_preject, "RyDsResult failed");
	free_NTP_ClockInfoList (clbase);
	return OK;
}
