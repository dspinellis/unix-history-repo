/* snmpb.c - snmpi bulk load */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/snmp/RCS/snmpb.c,v 7.15 91/02/22 09:44:08 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/snmp/RCS/snmpb.c,v 7.15 91/02/22 09:44:08 mrose Interim $
 *
 * Contributed by NYSERNet Inc.  This work was partially supported by the
 * U.S. Defense Advanced Research Projects Agency and the Rome Air Development
 * Center of the U.S. Air Force Systems Command under contract number
 * F30602-88-C-0016.
 *
 *
 * $Log:	snmpb.c,v $
 * Revision 7.15  91/02/22  09:44:08  mrose
 * Interim 6.8
 * 
 * Revision 7.14  90/09/17  11:18:42  mrose
 * update
 * 
 * Revision 7.13  90/09/10  13:52:21  mrose
 * kzm
 * 
 * Revision 7.11  90/09/03  12:57:30  mrose
 * update
 * 
 * Revision 7.10  90/08/28  10:29:24  mrose
 * kzm
 * 
 * Revision 7.9  90/08/23  12:34:29  mrose
 * update
 * 
 * Revision 7.8  90/08/20  21:25:56  mrose
 * touch-up
 * 
 * Revision 7.7  90/08/20  14:00:14  mrose
 * kzm
 * 
 * Revision 7.6  90/08/19  16:26:25  mrose
 * again
 * 
 * Revision 7.5  90/08/18  15:24:08  mrose
 * one more time
 * 
 * Revision 7.4  90/08/18  01:28:40  mrose
 * again
 * 
 * Revision 7.3  90/08/18  00:44:39  mrose
 * touch-up
 * 
 * Revision 7.2  90/08/16  16:50:37  mrose
 * again
 * 
 * Revision 7.1  90/08/14  14:28:43  mrose
 * pktin
 * 
 * Revision 7.0  90/08/08  14:00:19  mrose
 * *** empty log message ***
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include <stdio.h>
#include "SNMP-types.h"
#include "objects.h"
#include "tailor.h"

#ifdef	BSD42

#define	MAXTIME		(60 * 1000L)	/* in milli-seconds */
#define	MINTIME		 1		/*   .. */

#define	SETTLETIME	30		/* # of iterations to gauge RTT */

#define	MAXTRIES	 3		/* for retries */

#define	MAXSPACE   0x10000		/* in between threads */
#define	MAXTHREADS	10		/* maximum #-simultaneous */
#define	MAXBOUNDS	10
#define	MINBOUNDS	 3

/*  */

					/* TIMING INFORMATION */
static	long	timeout;
static	u_long	timenow;
static	long	timemin;
static	long	timemax;
static	int	timelap;
static	int	timelap2;
static  int     RTTperthread = MAXTIME;


					/* BINDING INFORMATION (results) */
struct binding {
    OID	    b_name;

    union {
	PE	        un_value;

	struct binding *un_cols;
    }	    b_un;
#define	b_value	b_un.un_value
#define	b_cols	b_un.un_cols

    struct binding *b_next;
};


					/* INVOCATION INFORMATION */
static	int	last_id = 0;

struct invocation {
    struct type_SNMP_Message *i_msg;
    int	    i_info;		/* info >  0: response
				   info == 0: request
				   info <  0: not yet ready */

    int	    i_rid;		/* request-id of interest */
    int	    i_mid;		/* exclusive ceiling on i_rid */

    PE	    i_pe;		/* message to retry */
    int	    i_retries;		/* number of times request retried */
    u_long  i_lastime;		/* time last request sent */
    int     i_curinvokes;       /* current # threads when last request sent */
};

static	int	totreqs = 0;
static	int	totretr = 0;
static	int	totrsps = 0;
static	int	totdups = 0;


					/* THREAD INFORMATION */
struct thread {
    struct thread *t_forw;	/* doubly-linked list */
    struct thread *t_back;	/*   .. */

    OID	    t_lo;		/* inclusive lower-bound */
    OID	    t_arg;		/* current pointer */
    OID	    t_hi;		/* exclusive upper-bound */

    struct binding *t_binding;	/* for bulk2_aux() */

    struct invocation t_invoke;	/* invocation in progress */
#define	t_msg	      t_invoke.i_msg
#define	t_info	      t_invoke.i_info
#define	t_rid	      t_invoke.i_rid
#define	t_mid	      t_invoke.i_mid
#define	t_pe	      t_invoke.i_pe
#define	t_retries     t_invoke.i_retries
#define	t_lastime     t_invoke.i_lastime
#define	t_curinvokes  t_invoke.i_curinvokes

				/* statistics */
    int	    t_gns;		/* how many gn's done */
};

static int	once_only = 0;

static struct thread tque;	/* active thread list */
static struct thread *THead = &tque;

static	int	curthreads = 0;
static	int	maxthreads = 0;
static	int	tothreads = 0;
static	int	nilthreads = 0;
static  int	dedthreads = 0;

static  int     threadlimit = MAXTHREADS;


					/* REQUEST INFORMATION */
struct request {
    struct request *r_forw;	/* doubly-linked list */
    struct request *r_back;	/*   .. */

    int	    r_nbound;		/* number of bounds active */
    struct bound {
	OID	r_lo;		    /* inclusive lower bound */
	OID	r_arg;		    /* current pointer */
	OID	r_hi;		    /* exclusive upper bound */
        int     b_gns;		    /* stats: how many gn's done */

	struct bound *r_next;
    }	    *r_bounds;

    struct invocation r_invoke;	/* invocation in progress... */
#define	r_msg	      r_invoke.i_msg
#define	r_info	      r_invoke.i_info
#define	r_rid	      r_invoke.i_rid
#define	r_mid	      r_invoke.i_mid
#define	r_pe	      r_invoke.i_pe
#define	r_retries     r_invoke.i_retries
#define	r_lastime     r_invoke.i_lastime
#define	r_curinvokes  r_invoke.i_curinvokes
};
    
static struct request rque;	/* active request list */
static struct request *RHead = &rque;

static	int	currequests = 0;
static	int	maxrequests = 0;
static	int	totbounds = 0;
static	int	maxbounds = 0;
static	int	nilbounds = 0;
static	int	dedrequests = 0;

static	int	boundlimit = MAXBOUNDS;


					/* MISCELLANEOUS INFORMATION */
OID	oid_median (), oid_copy ();


extern	int	debug;
extern	int	watch;

void	adios (), advise ();
char   *snmp_error ();

/*    BULK1 */

/* algorithm assumes that first variable in VarBind has ubiquitous agent
   support...
 */

bulk1 (ps, sd, vb, community)
PS	ps;
int	sd;
struct type_SNMP_VarBindList *vb;
char   *community;
{
    int	    backoff,
	    rows;
    register struct thread *t;
    register struct binding **bp;
    struct binding *bl;
    struct timeval  tvs,
		    now;
    register OID    a,
		    b;
    OID	    arg;

    timeout = 2 * 1000L;
    timemin = timemax = timeout;

    curthreads = maxthreads = tothreads = nilthreads = dedthreads = 0;

    threadlimit = MAXTHREADS;
    RTTperthread = MAXTIME;

    totreqs = totretr = totrsps = totdups = 0;

    (void) gettimeofday (&tvs, (struct timezone *) 0);
    timenow = tvs.tv_sec * 1000L + tvs.tv_usec / 1000L;

    a = oid_copy (arg = vb -> VarBind -> name);
    a -> oid_elements[a -> oid_nelem++] = 127;
    if (new_thread (ps, vb, community, arg, a) == NOTOK)
	goto losing;

    b = oid_copy (arg);
    b -> oid_elements[b -> oid_nelem++] = 192;
    if (new_thread (ps, vb, community, a, b) == NOTOK)
	goto losing;

    a -> oid_elements[(--a -> oid_nelem) - 1]++;
    if (new_thread (ps, vb, community, b, a) == NOTOK)
	goto losing;

    oid_free (a);
    oid_free (b);

    bp = &bl, bl = NULL, rows = 0;
    for (timelap = 0; THead -> t_forw != THead; timelap++) {
	register struct binding *bv;
	struct thread *u;

	if ((backoff = wait_for_action (sd, ps)) == NOTOK)
	    break;

	for (t = THead -> t_forw; t != THead; t = u) {
	    register struct binding **bz;
	    register struct type_SNMP_VarBindList *vp,
						  *vb2;
	    struct OIDentifier oids;

	    u = t -> t_forw;
	    if (!t -> t_info)
		continue;

	    if (oid_cmp (t -> t_arg, b = t -> t_msg -> data -> un.get__response
			 		   -> variable__bindings -> VarBind
								 -> name)
		    >= 0) {
		char buffer[BUFSIZ];

		(void) strcpy (buffer, oid2ode (t -> t_arg));
		advise (NULLCP,
			"agent botched get-next (%s -> %s), thread dead",
			buffer, oid2ode (b));

		free_thread (t);
		dedthreads++;
		continue;
	    }

	    if (oid_cmp (b, t -> t_hi) >= 0) {
		free_thread (t);
		continue;
	    }
	    oid_free (t -> t_arg);
	    t -> t_arg = oid_copy (b);

	    a = vb -> VarBind -> name, b = t -> t_arg;
	    oids.oid_nelem = b -> oid_nelem - a -> oid_nelem;
	    oids.oid_elements = b -> oid_elements + a -> oid_nelem;

	    if ((bv = (struct binding *) calloc (1, sizeof *bv)) == NULL)
		adios (NULLCP, "out of memory");
	    *bp = bv, bp = &bv -> b_next;

	    bv -> b_name = oid_copy (&oids);
	    bz = &bv -> b_cols;
	    rows++;

	    for (vp = t -> t_msg -> data -> un.get__response 
					 -> variable__bindings, vb2 = vb;
		     vp;
		     vp = vp -> next, vb2 = vb2 -> next) {
		int	fixup = 0;
		a = vb2 -> VarBind -> name, b = vp -> VarBind -> name;

		if (a -> oid_nelem > b -> oid_nelem
		        || bcmp ((char *) a -> oid_elements,
				 (char *) b -> oid_elements,
				 a -> oid_nelem
				         * sizeof a -> oid_elements[0])) {
		    oid_free (b);
		    vp -> VarBind -> name = b = oid_copy (t -> t_arg);
		    bcopy ((char *) a -> oid_elements,
			   (char *) b -> oid_elements,
			   a -> oid_nelem * sizeof *a -> oid_elements);

/* not really needed...
		    pe_free (vp -> VarBind -> value);
		    if ((vp -> VarBind -> value = pe_alloc (PE_CLASS_UNIV,
							     PE_FORM_PRIM,
							     PE_PRIM_NULL))
			    == NULL)
			adios (NULLCP, "out of memory");
 */
		    fixup = 1;
		}

		if ((bv = (struct binding *) calloc (1, sizeof *bv)) == NULL)
		    adios (NULLCP, "out of memory");
		*bz = bv, bz = &bv -> b_next;

		bv -> b_name = oid_copy (b);
		if (!fixup) {
		    bv -> b_value = vp -> VarBind -> value;
		    if ((vp -> VarBind -> value = pe_alloc (PE_CLASS_UNIV,
							    PE_FORM_PRIM,
							    PE_PRIM_NULL))
		            == NULLPE)
			adios (NULLCP, "out of memory");
		}
	    }

	    if (curthreads < threadlimit
		    && !backoff
		    && (a = oid_median (t -> t_arg, t -> t_hi))) {
		if (new_thread (ps, vb, community, a, t -> t_hi) == NOTOK)
		    goto losing;

		remque (t);		/* fairness in splits... */
		insque (t, THead -> t_back);

		oid_free (t -> t_hi);
		t -> t_hi = a;
		backoff = 1;
	    }

	    if (next_thread (t, ps, 1) == NOTOK)
		goto losing;
	}
    }

    if (backoff == NOTOK) {
losing: ;
	advise (NULLCP, "aborting bulk retrieval...");

	while ((t = THead -> t_forw) != THead)
	    free_thread (t);
    }

    (void) gettimeofday (&now, (struct timezone *) 0);
    now.tv_sec -= tvs.tv_sec;
    if ((now.tv_usec -= tvs.tv_usec) < 0)
	now.tv_sec--, now.tv_usec += 1000000;
    advise (NULLCP,
	    "%d row%s retrieved in %d.%06d seconds during %d iterations",
	    rows, rows != 1 ? "s" : "", now.tv_sec, now.tv_usec, timelap);
    advise (NULLCP,
	    "threads:  at most %d active, total of %d created, and %d did nothing",
	    maxthreads, tothreads, nilthreads);
    advise (NULLCP, "messages: %d request%s sent,  along with %d retr%s",
	    totreqs, totreqs != 1 ? "s" : "", totretr,
	    totretr != 1 ? "ies" : "y");
    advise (NULLCP, "          %d response%s rcvd, along with %d duplicate%s",
	    totrsps, totrsps != 1 ? "s" : "", totdups,
	    totdups != 1 ? "s" : "");
    advise (NULLCP, "timeouts: min=%d.%03d fin=%d.%03d max=%d.%03d seconds",
	    timemin / 1000, timemin % 1000, timeout / 1000, timeout % 1000,
	    timemax / 1000, timemax % 1000);

    print_bulk (bl, vb, dedthreads);
}

/*  */

static int  wait_for_action (sd, ps)
int	sd;
PS	ps;
{
    int	    backoff,
	    n,
	    nfds,
	    request_id;
    long    maxrtt;
    u_long  lastime;
    fd_set  rfds;
    struct timeval tvs;
    struct type_SNMP_Message *msg;
    register struct type_SNMP_PDU *parm;
    register struct invocation *i;
    register struct request *r,
                            *u;
    register struct thread  *t,
			    *s;
    PE	    pe;

    FD_ZERO (&rfds);

    nfds = sd + 1;
    FD_SET (sd, &rfds);

    backoff = 1;

    lastime = timenow;
    for (t = THead -> t_forw; t != THead; t = t -> t_forw) {
	if (t -> t_info)	/* should always fail... */
	    continue;

	for (s = t -> t_forw; s != THead; s = s -> t_forw)
	    if (!s -> t_info && s -> t_lastime < t -> t_lastime)
		t = s;

	if (lastime > t -> t_lastime)
	    lastime = t -> t_lastime;
    }
    for (r = RHead -> r_forw; r != RHead; r = r -> r_forw) {
	if (r -> r_info)	/* should always fail... */
	    continue;

	for (u = r -> r_forw; u != RHead; u = u -> r_forw)
	    if (!u -> r_info && u -> r_lastime < r -> r_lastime)
		r = u;

	if (lastime > r -> r_lastime)
	    lastime = r -> r_lastime;
    }
    if ((maxrtt = timeout - (timenow - lastime)) < 0)
	maxrtt = 0;
    if (debug && maxrtt < timeout)
	fprintf (stderr, "timeout reduced from %u to %u (delta %d)\n",
		 timeout, (u_long) maxrtt, (int) (timeout - maxrtt));
    tvs.tv_sec = maxrtt / 1000L, tvs.tv_usec = (maxrtt % 1000) * 1000L;
    maxrtt = 0;

    switch (n = select (nfds, &rfds, NULLFD, NULLFD, &tvs)) {
	case NOTOK:
	    advise ("failed", "select");
	    return NOTOK;

	case OK:
	default:
	    (void) gettimeofday (&tvs, (struct timezone *) 0);
	    timenow = tvs.tv_sec * 1000L + tvs.tv_usec / 1000L;

	    if (n == OK)
		break;

again: ;
	    if ((pe = ps2pe (ps)) == NULLPE) {
		advise (NULLCP, "ps2pe: %s", ps_error (ps -> ps_errno));
		return NOTOK;
	    }

	    msg = NULL;
	    if (decode_SNMP_Message (pe, 1, NULLIP, NULLVP, &msg) == NOTOK) {
		advise (NULLCP, "decode_SNMP_Message: %s", PY_pepy);
oops: ;
		if (msg)
		    free_SNMP_Message (msg);
		pe_free (pe);
		return NOTOK;
	    }
	    if (watch) {
		fprintf (stdout, "read PDU\n");
		(void) print_SNMP_Message (pe, 1, NULLIP, NULLVP, NULLCP);
		(void) fflush (stdout);
	    }
	    totrsps++;
	    if (msg -> data -> offset != type_SNMP_PDUs_get__response) {
		advise (NULLCP, "unexpected message type %d",
			msg -> data -> offset);
		goto oops;
	    }
	    
	    request_id =
		    (parm = msg -> data -> un.get__response) -> request__id;

	    for (t = THead -> t_forw; t != THead; t = s) {
		s = t -> t_forw;
		if (t -> t_rid != request_id)
		    continue;

		if (t -> t_info) {
		    advise (NULLCP, "duplicated response for request-id %d",
			    request_id);
		    goto duplicate_id;
		}

		t -> t_info = 1;
		free_SNMP_Message (t -> t_msg);
		t -> t_msg = msg;
		if (t -> t_pe)
		    pe_free (t -> t_pe);
		t -> t_pe = pe;

		if (parm -> error__status != int_SNMP_error__status_noError) {
		    if (parm -> error__status
			    != int_SNMP_error__status_noSuchName)
			advise (NULLCP, "%s for get-next of %s, thread dead",
				snmp_error (parm -> error__status),
				oid2ode (t -> t_arg));
		    free_thread (t);
		    dedthreads++;
		}

		i = &t -> t_invoke;
		goto finish_invoke;
	    }

	    for (r = RHead -> r_forw; r != RHead; r = u) {
		u = r -> r_forw;
		if (r -> r_rid != request_id)
		    continue;

		if (r -> r_info) {
		    advise (NULLCP, "duplicated response for request-id %d",
			    request_id);
		    goto duplicate_id;
		}

		switch (parm -> error__status) {
		    case int_SNMP_error__status_noError:
		        r -> r_info = 1;
			free_SNMP_Message (r -> r_msg);
			r -> r_msg = msg;
			if (r -> r_pe)
			    pe_free (r -> r_pe);
			r -> r_pe = pe;
			break;

		    case int_SNMP_error__status_tooBig:
			advise (NULLCP, "got %s on request of size %d",
				snmp_error (parm -> error__status),
				r -> r_nbound);
			if (1 < r -> r_nbound && r -> r_nbound <= boundlimit)
			    boundlimit = r -> r_nbound - 1;

toss_it: ;
			r -> r_info = -1;
			free_SNMP_Message (msg);
			pe_free (pe);
			goto next_request;

		    case int_SNMP_error__status_noSuchName:
			if (parm -> error__index == 0) {
invalid_index: ;
			    advise (NULLCP,
				    "got %s with invalid index (%d)",
				    snmp_error (parm -> error__status),
				    parm -> error__index);
			    goto drop_request;
			}
			{
			    register int    j;
			    register struct bound  *b,
						  **bp;
			    register struct type_SNMP_VarBindList  *v,
							          **vp;

			    bp = &r -> r_bounds;
			    vp = &r -> r_msg -> data -> un.get__request
							-> variable__bindings;
			    for (j = parm -> error__index;
				     --j > 0;
				     bp = &((*bp) -> r_next),
				 	vp = &((*vp) -> next))
				if (*bp == NULL)
				    goto invalid_index;
			    advise (NULLCP,
				    "got %s on %s",
				    snmp_error (parm -> error__status),
				    oid2ode ((*bp) -> r_arg));

			    b = *bp, bp = &b -> r_next;
	                    free_bound (b);
			    r -> r_nbound--;

			    v = *vp, vp = &v -> next;
			    v -> next = NULL;
			    free_SNMP_VarBindList (v);

			    goto toss_it;
			}

		    default:
			advise (NULLCP, "%s for get-next of %s, et. al.",
				    snmp_error (parm -> error__status),
				    oid2ode (r -> r_bounds ->  r_arg));
drop_request: ;
			free_request (r);
			dedrequests++;
			goto next_request;
		}
		i = &r -> r_invoke;

finish_invoke: ;
		if (i -> i_retries == 0) {
		    long    val = timenow - i -> i_lastime;

		    if (maxrtt < val)
			maxrtt = val;
		    if (timelap < SETTLETIME) {
		        int  rtt = maxrtt / i -> i_curinvokes;

			if (RTTperthread > rtt) {
			    RTTperthread = rtt;

			    if ((threadlimit = i -> i_curinvokes + 1) 
						> MAXTHREADS)
			        threadlimit = MAXTHREADS;
			}
		    }
		    if (debug)
			fprintf (stderr,
                                 "time now %u, xmit-time %u, RTT %u\n",
				 timenow, i -> i_lastime,
				 timenow - i -> i_lastime);
		    backoff = 0;
		}
		else
		    i -> i_retries = 0;
		goto next_request;
	    }
	    if (debug)
		fprintf (stderr, "request-id mismatch, not expecting %d\n",
			 request_id);
duplicate_id: ;
	    free_SNMP_Message (msg);
	    pe_free (pe);
	    totrsps--, totdups++;
next_request: ;

	    FD_SET (sd, &rfds);
	    if (select_dgram_socket (nfds, &rfds, NULLFD, NULLFD, OK) > OK)
		goto again;

	    break;
    }

    if (curthreads > maxthreads)
	maxthreads = curthreads;
    if (currequests > maxrequests)
	maxrequests = currequests;

    if (backoff) {
	if ((timeout <<= 1) > MAXTIME)
	    timeout = MAXTIME;
	if (debug)
	    fprintf (stderr, "adjusted timeout to %g seconds(0)\n",
		     timeout / 1000.0);
    }
    else
	if (maxrtt > 0 && maxrtt != timeout) {
	    long    timedelta = maxrtt + (maxrtt >> 1);

	    if (timedelta > timeout) {
		if ((timeout = timedelta) > MAXTIME)
		    timeout = MAXTIME;
		backoff = 1;
		goto outta_time;
	    }
	    else {
		timeout -= (timeout - timedelta) >> 1;
		if (timeout  < MINTIME)
		    timeout = MINTIME;

outta_time: ;
		if (debug)
		    fprintf (stderr,
			    "adjusted timeout to %g seconds(1)\n",
			    timeout / 1000.0);
	    }
	}

    if (timeout < timemin)
	timemin = timeout;
    else
	if (timeout > timemax)
	    timemax = timeout;

    i = NULL;
    for (r = RHead -> r_forw; r != RHead; r = r -> r_forw) {
	if (r -> r_info)
	    continue;

	for (u = r -> r_forw; u != RHead; u = u -> r_forw)
	    if (!u -> r_info && u -> r_lastime < r -> r_lastime)
		r = u;

	i = &r -> r_invoke;
    }
    for (t = THead -> t_forw; t != THead; t = t -> t_forw) {
	if (t -> t_info)
	    continue;

	for (s = t -> t_forw; s != THead; s = s -> t_forw)
	    if (!s -> t_info && s -> t_lastime < t -> t_lastime)
		t = s;

	if (i == NULL || t -> t_lastime < i -> i_lastime)
	    i = &t -> t_invoke;
    }

    if (i && i -> i_lastime + timeout < timenow) {
	if (++i -> i_retries > MAXTRIES) {
	    advise (NULLCP, "too many retries (%d)", MAXTRIES);
	    return NOTOK;
	}

	if (pe2ps (ps, i -> i_pe) == NOTOK) {
	    advise (NULLCP, "pe2ps: %s", ps_error (ps -> ps_errno));
	    return NOTOK;
	}
	if (watch) {
	    fprintf (stdout, "retry ID %d\n", i -> i_rid);
	    (void) print_SNMP_Message (i -> i_pe, 1, NULLIP, NULLVP, NULLCP);
	    (void) fflush (stdout);
	}
	else
	    if (debug)
		fprintf (stderr,
			 "retry ID %d, time now %u, waiting %u, timeout %u\n",
			 i -> i_rid, timenow, timenow - i -> i_lastime,
			 timeout);

	totretr++;

	i -> i_lastime = timenow;
	i -> i_curinvokes = currequests ? currequests : 1;
	backoff = 1;
    }

    return backoff;
}

/*  */

static	print_bulk (bl, vb, partial)
struct binding *bl;
struct type_SNMP_VarBindList *vb;
int	partial;
{
    int	    i;
    register struct binding *bv,
			    *bz;

    if (partial)
	printf ("partial results only...\n");

    i = strlen ("row");
    for (bv = bl; bv; bv = bv -> b_next) {
	register int    j;
	char   *cp = sprintoid (bv -> b_name);

	if (i < (j = strlen (cp)))
	    i = j;
    }

    printf ("%-*s", i, "row");
    for (; vb; vb = vb -> next)
	printf ("\t%s", oid2ode (vb -> VarBind -> name));

    for (bv = bl; bv; bv = bz) {
	register struct binding *bp,
				*bq;

	bz = bv -> b_next;

	printf ("\n%-*s", i, sprintoid (bv -> b_name));

	for (bp = bv -> b_cols; bp; bp = bq) {
	    bq = bp -> b_next;

	    printf ("\t");

	    if (bp -> b_value) {
		caddr_t	value;
		register OI	oi;
		register OS	os;

		if ((oi = name2inst (bp -> b_name)) == NULL
		    	|| (os = oi -> oi_type -> ot_syntax) == NULL
		    	|| (*os -> os_decode) (&value, bp -> b_value) == NOTOK)
		    vunknown (bp -> b_value);
		else {
		    (*os -> os_print) (value, os);
		    (*os -> os_free) (value);
		}

		pe_free (bp -> b_value);
	    }
	    else
		printf ("NULL");

	    oid_free (bp -> b_name);

	    free ((char *) bp);
	}

	oid_free (bv -> b_name);

	free ((char *) bv);
    }
    printf ("\n");
}

/*  */

static struct type_SNMP_Message *new_message (arg, vb, community, next)
OID	arg;
struct type_SNMP_VarBindList *vb;
char   *community;
int	next;
{
    register struct type_SNMP_Message *msg;
    register struct type_SNMP_PDUs *pdu;
    register struct type_SNMP_PDU *parm;
    register struct type_SNMP_VarBindList **vp;

    if ((msg = (struct type_SNMP_Message *) calloc (1, sizeof *msg)) == NULL)
	adios (NULLCP, "out of memory");

    msg -> version = int_SNMP_version_version__1;

    if ((msg -> community = str2qb (community, strlen (community), 1)) == NULL)
	adios (NULLCP, "out of memory");

    if ((pdu = (struct type_SNMP_PDUs *) calloc (1, sizeof *pdu)) == NULL)
	adios (NULLCP, "out of memory");
    msg -> data = pdu;

    pdu -> offset = next ? type_SNMP_PDUs_get__next__request
			 : type_SNMP_PDUs_get__request;
    
/* for now, always a PDU... */

    if ((parm = (struct type_SNMP_PDU *) calloc (1, sizeof *parm)) == NULL)
	adios (NULLCP, "out of memory");
    pdu -> un.get__request = parm;

    for (vp = &parm -> variable__bindings; vb; vb = vb -> next) {
	register struct type_SNMP_VarBindList *bind;
	register struct type_SNMP_VarBind *v;

	if ((bind = (struct type_SNMP_VarBindList *) calloc (1, sizeof *bind))
	    	    == NULL)
	    adios (NULLCP, "out of memory");
	*vp = bind, vp = &bind -> next;

	if ((v = (struct type_SNMP_VarBind *) calloc (1, sizeof *v)) == NULL)
	    adios (NULLCP, "out of memory");
	bind -> VarBind = v;

	v -> name = oid_copy (arg);
	bcopy ((char *) vb -> VarBind -> name -> oid_elements,
	       (char *) v -> name -> oid_elements,
	       vb -> VarBind -> name -> oid_nelem
	           * sizeof *v -> name -> oid_elements);

	if ((v -> value = pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_NULL))
	        == NULL)
	    adios (NULLCP, "out of memory");
    }
    
    return msg;
}

/*    THREADS */

static int  new_thread (ps, vb, community, start, stop)
PS	ps;
struct type_SNMP_VarBindList *vb;
char   *community;
OID	start,
	stop;
{
    register struct thread *t;

    t = (struct thread *) calloc (1, sizeof *t);
    if (t == NULL)
	adios (NULLCP, "new_thread: out of memory");

    t -> t_rid = last_id;
    t -> t_mid = (last_id += MAXSPACE);

    t -> t_lo = oid_copy (start);
    t -> t_arg = oid_copy (start);
    t -> t_hi = oid_copy (stop);

    t -> t_msg = new_message (t -> t_arg, vb, community, 1);

    if (once_only == 0) {
	THead -> t_forw = THead -> t_back = THead;
	RHead -> r_forw = RHead -> r_back = RHead;
	once_only++;
    }

    insque (t, THead -> t_back);

    curthreads++;
    tothreads++;

    return next_thread (t, ps, 1);
}

/*  */

static int  new_string (ps, vb, community, bp)
PS	ps;
struct type_SNMP_VarBindList *vb;
char   *community;
struct binding *bp;
{
    register struct thread *t;

    t = (struct thread *) calloc (1, sizeof *t);
    if (t == NULL)
	adios (NULLCP, "new_thread: out of memory");

    t -> t_rid = last_id;
    t -> t_mid = (last_id += MAXSPACE);

    t -> t_binding = bp;

    t -> t_msg = new_message (bp -> b_cols -> b_name, vb, community, 0);

    insque (t, THead -> t_back);

    curthreads++;
    tothreads++;

    return next_thread (t, ps, 0);
}

/*  */

static	free_thread (t)
register struct thread *t;
{
    if (debug && t -> t_lo) {
	fprintf (stderr, "thread from %s to ", oid2ode (t -> t_lo));
	fprintf (stderr, "%s did %d get-nexts\n", oid2ode (t -> t_hi),
		 t -> t_gns);
    }
    curthreads--;
    if (t -> t_gns <= 1)
	nilthreads++;

    if (t -> t_pe)
	pe_free (t -> t_pe);

    oid_free (t -> t_lo);
    oid_free (t -> t_arg);
    oid_free (t -> t_hi);

    if (t -> t_msg)
	free_SNMP_Message (t -> t_msg);

    remque (t);
    free ((char *) t);
}

/*  */

static int  next_thread (t, ps, next)
register struct thread *t;
PS	ps;
int	next;
{
    if (++t -> t_rid >= t -> t_mid)
	t -> t_rid = t -> t_mid - MAXSPACE;

    t -> t_info = 0;
    t -> t_msg -> data -> offset =
	next ? type_SNMP_PDUs_get__next__request : type_SNMP_PDUs_get__request;
    t -> t_msg -> data -> un.get__response -> request__id = t -> t_rid;

    if (t -> t_pe)
	pe_free (t -> t_pe), t -> t_pe = NULL;

    if (encode_SNMP_Message (&t -> t_pe, 1, 0, NULLCP, t -> t_msg) == NOTOK) {
	advise (NULLCP, "encode_SNMP_Message: %s", PY_pepy);
	return NOTOK;
    }

    if (pe2ps (ps, t -> t_pe) == NOTOK) {
	advise (NULLCP, "pe2ps: %s", ps_error (ps -> ps_errno));
	return NOTOK;
    }
    if (watch) {
	fprintf (stdout, "write PDU\n");
	(void) print_SNMP_Message (t -> t_pe, 1, NULLIP, NULLVP, NULLCP);
	(void) fflush (stdout);
    }
    totreqs++;

    t -> t_lastime = timenow;
    t -> t_curinvokes = curthreads ? curthreads : 1;
    t -> t_gns++;

    return OK;
}

/*    BULK2 */

bulk2 (ps, sd, vb, community)
PS	ps;
int	sd;
register struct type_SNMP_VarBindList *vb;
char   *community;
{
    int	    backoff,
	    evalreq,
	    rows;
    register struct request  *r;
    register struct binding **bp;
    struct binding *bl;
    struct timeval  tvs,
		    now;
    register OID    a,
		    b;
    OID	    arg;

    timeout = 2 * 1000L;
    timemin = timemax = timeout;

    currequests = maxrequests = totbounds = maxbounds = dedrequests = 0;
    nilbounds = 0;

    threadlimit = MAXTHREADS;
    boundlimit = MAXBOUNDS;
    RTTperthread = MAXTIME;

    totreqs = totretr = totrsps = totdups = 0;

    (void) gettimeofday (&tvs, (struct timezone *) 0);
    timenow = tvs.tv_sec * 1000L + tvs.tv_usec / 1000L;

    a = oid_copy (arg = vb -> VarBind -> name);
    a -> oid_elements[a -> oid_nelem++] = 127;
    new_bound (community, arg, a);

    b = oid_copy (arg);
    b -> oid_elements[b -> oid_nelem++] = 192;
    new_bound (community, a, b);

    a -> oid_elements[(--a -> oid_nelem) - 1]++;
    new_bound (community, b, a);

    oid_free (a);
    oid_free (b);

    if (push_requests (ps, community, 0) == NOTOK)
	goto losing;

    bp = &bl, bl = NULL, rows = 0;
    for (timelap = 0; RHead -> r_forw != RHead; timelap++) {
	struct request *u;

	if ((backoff = wait_for_action (sd, ps)) == NOTOK)
	    goto losing;

	for (r = RHead -> r_forw; r != RHead; r = u) {
	    register struct bound  *br,
				   *bz,
				  **bb;
	    register struct type_SNMP_VarBindList  *vr,
						  **vv;

	    u = r -> r_forw;
	    if (r -> r_info < 1)
		continue;

	    bb = &r -> r_bounds;
	    vv = &r -> r_msg -> data -> un.get__request -> variable__bindings;
	    evalreq = 0;
	    while (br = *bb) {
		register struct binding *bv,
					*bv2;
		struct OIDentifier oids;

		if ((vr = *vv) == NULL) {
		    advise (NULLCP,
			    "missing variable in response, continuing");
		    dedrequests++;

		    do {
			bz = br -> r_next;
			free_bound (br);
			r -> r_nbound--;
		    }
		    while (br = bz);
		    *bb = NULL;

		    break;
		}

		if (oid_cmp (br -> r_arg, b = vr -> VarBind -> name) >= 0) {
		    char    buffer[BUFSIZ];

		    (void) strcpy (buffer, oid2ode (br -> r_arg));
		    advise (NULLCP,
			    "agent botched get-next (%s -> %s), continuing",
			    oid2ode (b));

		    dedrequests++;
		    goto drop_bound;
		}

		if (oid_cmp (b, br -> r_hi) >= 0) {
		    if (br -> b_gns <=1)
			evalreq--;
drop_bound: ;
		    *bb = br -> r_next;
		    free_bound (br);
		    r -> r_nbound--;

		    *vv = vr -> next;
		    vr -> next = NULL;
		    free_SNMP_VarBindList (vr);

		    continue;
		}
		evalreq++;
		oid_free (br -> r_arg);
		br -> r_arg = oid_copy (b);

		oids.oid_nelem = b -> oid_nelem - arg -> oid_nelem;
		oids.oid_elements = b -> oid_elements + arg -> oid_nelem;

		if ((bv = (struct binding *) calloc (1, sizeof *bv)) == NULL)
		    adios (NULLCP, "out of memory");
		*bp = bv, bp = &bv -> b_next;

		bv -> b_name = oid_copy (&oids);
		rows++;

		if ((bv2 = (struct binding *) calloc (1, sizeof *bv2)) == NULL)
		    adios (NULLCP, "out of memory");
		bv -> b_cols = bv2;

		bv2 -> b_name = oid_copy (b);
		bv2 -> b_value = vr -> VarBind -> value;
		if ((vr -> VarBind -> value = pe_alloc (PE_CLASS_UNIV,
							PE_FORM_PRIM,
							PE_PRIM_NULL))
		        == NULLPE)
		    adios (NULLCP, "out of memory");

		bb = &br -> r_next, vv = &vr -> next;
	    }
	    if (vr = *vv) {
		advise (NULLCP, "too many variables in response");

		free_SNMP_VarBindList (vr);
		*vv = NULL;
	    }
	    if (timelap >= SETTLETIME
		    && evalreq < 0 && boundlimit != MINBOUNDS)
		boundlimit--;
	}

	if (push_requests (ps, community,
			   currequests < threadlimit && !backoff) == NOTOK)
	    goto losing;
    }

    if (bulk2_aux (ps, sd, bl, vb, community) == NOTOK) {
losing: ;
	advise (NULLCP, "aborting bulk retrieval...");

	while ((r = RHead -> r_forw) != RHead)
	    free_request (r);
    }

    (void) gettimeofday (&now, (struct timezone *) 0);
    now.tv_sec -= tvs.tv_sec;
    if ((now.tv_usec -= tvs.tv_usec) < 0)
	now.tv_sec--, now.tv_usec += 1000000;
    advise (NULLCP,
	    "%d row%s retrieved in %d.%06d seconds during %d/%d iterations",
	    rows, rows != 1 ? "s" : "", now.tv_sec, now.tv_usec, timelap,
	    timelap2);
    advise (NULLCP, "requests: at most %d active", maxrequests);
    advise (NULLCP,
	    "bounds:   %d created, at most %d active, %d integral, %d did nothing",
	    totbounds, maxbounds, boundlimit, nilbounds);
    if (timelap2)
	advise (NULLCP,
		"threads:  at most %d active, total of %d created",
		maxthreads, tothreads);
    advise (NULLCP, "messages: %d request%s sent,  along with %d retr%s",
	    totreqs, totreqs != 1 ? "s" : "", totretr,
	    totretr != 1 ? "ies" : "y");
    advise (NULLCP, "          %d response%s rcvd, along with %d duplicate%s",
	    totrsps, totrsps != 1 ? "s" : "", totdups,
	    totdups != 1 ? "s" : "");
    advise (NULLCP, "timeouts: min=%d.%03d fin=%d.%03d max=%d.%03d seconds",
	    timemin / 1000, timemin % 1000, timeout / 1000, timeout % 1000,
	    timemax / 1000, timemax % 1000);

    print_bulk (bl, vb, dedrequests || dedthreads);
}

/*  */

/*  */

static int  bulk2_aux (ps, sd, bl, vb, community)
PS	ps;
int	sd;
struct binding *bl;
register struct type_SNMP_VarBindList *vb;
char   *community;
{
    int	    backoff;
    register struct thread *t;

    curthreads = maxthreads = tothreads = nilthreads = dedthreads = 0;
    timelap2 = 0;

    if (bl == NULL || (vb = vb -> next) == NULL)
	return OK;

    while (bl)
	if (curthreads < threadlimit) {
	    if (new_string (ps, vb, community, bl) == NOTOK)
		goto losing;
	    bl = bl -> b_next;
	}
	else
	    break;

    for (; THead -> t_forw != THead; timelap2++) {
	struct thread *u;

	if ((backoff = wait_for_action (sd, ps)) == NOTOK)
	    break;

	for (t = THead -> t_forw; t != THead; t = u) {
	    register struct binding  *bv,
				    **bz;
	    register struct type_SNMP_VarBindList *vp;

	    u = t -> t_forw;
	    if (!t -> t_info)
		continue;

	    bv = t -> t_binding;
	    bz = &bv -> b_cols -> b_next;

	    for (vp = t -> t_msg -> data -> un.get__response
		 			 -> variable__bindings;
		     vp;
		     vp = vp -> next) {
		if ((bv = (struct binding *) calloc (1, sizeof *bv)) == NULL)
		    adios (NULLCP, "out of memory");
		*bz = bv, bz = &bv -> b_next;

		bv -> b_name = oid_copy (vp -> VarBind -> name);
		bv -> b_value = vp -> VarBind -> value;
		if ((vp -> VarBind -> value = pe_alloc (PE_CLASS_UNIV,
							PE_FORM_PRIM,
							PE_PRIM_NULL))
		        == NULLPE)
		    adios (NULLCP, "out of memory");
	    }

	    if (curthreads < threadlimit && !backoff && bl) {
		if (new_string (ps, vb, community, bl) == NOTOK)
		    goto losing;
		bl = bl -> b_next;
		backoff = 1;
	    }

	    free_thread (t);
	    if (bl) {
		if (new_string (ps, vb, community, bl) == NOTOK)
		    goto losing;
		bl = bl -> b_next;
	    }		
	}
    }

    if (backoff == NOTOK) {
losing: ;
	while ((t = THead -> t_forw) != THead)
	    free_thread (t);
    }

    return backoff;
}

/*    REQUESTS */

static struct request *new_request (community)
char   *community;
{
    register struct request *r;

    r = (struct request *) calloc (1, sizeof *r);
    if (r == NULL)
	adios (NULLCP, "new_request: out of memory");

    r -> r_rid = last_id;
    r -> r_mid = (last_id += MAXSPACE);

    r -> r_info = -1;
    r -> r_msg = new_message (NULLOID, (struct type_SNMP_VarBindList *) NULL,
			      community, 1);

    insque (r, RHead -> r_back);

    currequests++;

    return r;
}

/*  */

static	free_request (r)
register struct request *r;
{
    register struct bound *bp,
			  *bq;

    currequests--;

    if (r -> r_pe)
	pe_free (r -> r_pe);

    for (bp = r -> r_bounds; bp; bp = bq) {
	bq = bp -> r_next;
	free_bound (bp);
    }

    if (r -> r_msg)
	free_SNMP_Message (r -> r_msg);

    remque (r);
    free ((char *) r);
}

/*  */

static int  new_bound (community, start, stop)
char   *community;
OID	start,
	stop;
{
    register struct request *r;
    register struct bound *b;
    register struct type_SNMP_VarBindList *vb;
    register struct type_SNMP_VarBind *v;

    if (once_only == 0) {
	THead -> t_forw = THead -> t_back = THead;
	RHead -> r_forw = RHead -> r_back = RHead;
	once_only++;
    }

    if ((r = RHead -> r_forw) == RHead)
	r = new_request (community);

    if ((b = (struct bound *) calloc (1, sizeof *b)) == NULL)
	adios (NULLCP, "new_bound: out of memory");
    b -> r_next = r -> r_bounds, r -> r_bounds = b;

    b -> b_gns = 0;
    b -> r_lo = oid_copy (start);
    b -> r_arg = oid_copy (start);
    b -> r_hi = oid_copy (stop);
    totbounds++;

    if ((vb = (struct type_SNMP_VarBindList *) calloc (1, sizeof *vb)) == NULL)
	adios (NULLCP, "new_bound: out of memory");
    vb -> next = r -> r_msg -> data -> un.get__request -> variable__bindings,
	r -> r_msg -> data -> un.get__request -> variable__bindings = vb;

    if ((v = (struct type_SNMP_VarBind *) calloc (1, sizeof *v)) == NULL)
	adios (NULLCP, "new_bound: out of memory");
    vb -> VarBind = v;

    v -> name = oid_copy (start);
    if ((v -> value = pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_NULL))
	    == NULL)
	adios (NULLCP, "new_bound: out of memory");

    r -> r_nbound++;
}

/*  */

static	free_bound (b)
register struct bound *b;
{
    if (debug && b -> r_lo)  {
	fprintf (stderr, "%d get-nexts on bound: %s to",
                                   b -> b_gns, oid2ode (b -> r_lo));
        fprintf (stderr, " %s\n", oid2ode (b -> r_hi));
    }

    oid_free (b -> r_lo);
    oid_free (b -> r_arg);
    oid_free (b -> r_hi);

    if (b -> b_gns <= 1) 
        nilbounds++;

    free ((char *) b);
}

/*  */

static	push_requests (ps, community, onemore)
PS	ps;
char   *community;
int	onemore;
{
    register int    nbound,
		    nrequest,
		    tbound;
    int	    new,
    	    wen;
    register struct request *r;
    struct request *u;
    register struct bound  *bn,
			   *bz,
			  **bp;
    struct bound *b;
    register struct type_SNMP_VarBindList  *vz,
					  **vp;
    struct type_SNMP_VarBindList *v;

    bp = &b, b = NULL;
    vp = &v, v = NULL;
    nrequest = nbound = tbound = 0;
    for (r = RHead -> r_forw; r != RHead; r = r -> r_forw) {
	if (!r -> r_info) {
	    tbound += r -> r_nbound;
	    continue;
	}
	nrequest++;

	if (*bp = r -> r_bounds) {
	    for (bz = *bp; bz; bz = bz -> r_next)
		if (bz -> r_next == NULL)
		    bp = &bz -> r_next;

	    r -> r_bounds = NULL;
	    nbound += r -> r_nbound;
	}

	if (*vp = r -> r_msg -> data -> un.get__request -> variable__bindings){
	    for (vz = *vp; vz; vz = vz -> next)
		if (vz -> next == NULL)
		    vp = &vz -> next;
	    
	    r -> r_msg -> data -> un.get__request -> variable__bindings = NULL;
	}

	r -> r_nbound = 0;
    }
    if ((tbound += nbound) > maxbounds)
	maxbounds = tbound;
    if (nrequest == 0)
	return OK;

    if ((new = (nbound + (boundlimit - 1)) / boundlimit - nrequest) <= 0
	    && onemore)
	new = 1;
    for (; new-- > 0; nrequest++)
	(void) new_request (community);
    if ((new = nrequest * boundlimit) > (wen = nbound << 1))
	new = wen;

    for (new -= nbound, bn = b; (new > 0) && bn; new--, bn = bn -> r_next) {
	OID	mid;

	if ((mid = oid_median (bn -> r_arg, bn -> r_hi)) == NULLOID)
	    continue;

	if ((bz = (struct bound *) calloc (1, sizeof *bz)) == NULL)
	    adios (NULLCP, "push_requests: out of memory");
	*bp = bz, bp = &bz -> r_next;

	bz -> r_lo = oid_copy (mid);
	bz -> r_arg = oid_copy (mid);
	bz -> r_hi = bn -> r_hi;
	tbound++, totbounds++;

	bn -> r_hi = mid;

	if ((vz = (struct type_SNMP_VarBindList *) calloc (1, sizeof *vz))
	        == NULL)
	    adios (NULLCP, "push_requests: out of memory");
	*vp = vz, vp = &vz -> next;

	if ((vz -> VarBind = (struct type_SNMP_VarBind *)
						calloc (1, sizeof *v)) == NULL)
	    adios (NULLCP, "push_requests: out of memory");

	vz -> VarBind -> name = oid_copy (mid);
	if ((vz -> VarBind -> value = pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM,
						PE_PRIM_NULL)) == NULL)
	    adios (NULLCP, "push_requests: out of memory");
    }
    if (tbound > maxbounds)
	maxbounds = tbound;

    if (b == NULL)
	goto send_them;
    for (r = RHead -> r_forw; r != RHead; r = r -> r_forw) {
	register int	i;
	register struct bound **inb;
	register struct type_SNMP_VarBindList **inv;

	if (!r -> r_info)
	    continue;

	inb = &r -> r_bounds;
	inv = &r -> r_msg -> data -> un.get__request -> variable__bindings;
	for (i = boundlimit; i > 0; i--) {
	    bz = b -> r_next, vz = v -> next;

	    *inb = b, inb = &b -> r_next, b -> r_next = NULL;
	    *inv = v, inv = &v -> next, v -> next = NULL;

            b -> b_gns++;
	    r -> r_nbound++;

	    if ((b = bz) == NULL)
		goto send_them;
	    v = vz;
	}
    }
    /* should never get here, since we should run out bounds before
       running out of requests; if we do ... ?? */

    if (debug)
	fprintf (stderr, "push_requests: possibly loss of bounds\n");

send_them: ;
    for (r = RHead -> r_forw; r != RHead; r = u) {
	u = r -> r_forw;
	if (!r -> r_info)
	    continue;

	if (r -> r_nbound <= 0) {
	    free_request (r);
	    continue;
	}

	if (++r -> r_rid >= r -> r_mid)
	    r -> r_rid = r -> r_mid - MAXSPACE;

	r -> r_info = 0;
	r -> r_msg -> data -> offset = type_SNMP_PDUs_get__next__request;
	r -> r_msg -> data -> un.get__response -> request__id = r -> r_rid;

	if (r -> r_pe)
	    pe_free (r -> r_pe), r -> r_pe = NULL;

	if (encode_SNMP_Message (&r -> r_pe, 1, 0, NULLCP, r -> r_msg)
	        == NOTOK) {
	    advise (NULLCP, "encode_SNMP_Message: %s", PY_pepy);
	    return NOTOK;
	}

	if (pe2ps (ps, r -> r_pe) == NOTOK) {
	    advise (NULLCP, "pe2ps: %s", ps_error (ps -> ps_errno));
	    return NOTOK;
	}
	if (watch) {
	    fprintf (stdout, "write PDU\n");
	    (void) print_SNMP_Message (r -> r_pe, 1, NULLIP, NULLVP, NULLCP);
	    (void) fflush (stdout);
	}
	totreqs++;

	r -> r_lastime = timenow;
	r -> r_curinvokes = currequests ? currequests : 1;
    }

    return OK;
}

/*    OIDS */

static OID  oid_median (a, b)
OID	a,
	b;
{
    register int    i;
    register unsigned int *ap,
			  *bp;
    register OID    c = NULL;

    if (oid_cmp (a, b) >= 0) {
	char    buffer[BUFSIZ];

	(void) strcpy (buffer, sprintoid (a));
	adios (NULLCP, "oid_median(%s <= %s)", buffer, sprintoid (b));
    }

    for (i = 1, ap = a -> oid_elements, bp = b -> oid_elements;
	    i <= b -> oid_nelem;
	    i++, ap++, bp++) {
	if (i > a -> oid_nelem) {
	    for (; *bp == NULL; bp++)
		if (++i > b -> oid_nelem)
		    goto losing;
	    c = oid_copy (b);
	    c -> oid_elements[(c -> oid_nelem = i) - 1] = *bp >> 1;
	    break;
	}
	
	if (*ap == *bp)
	    continue;

	c = oid_copy (a);
	if ((c -> oid_elements[(c -> oid_nelem = i) - 1] =
						    *ap + ((*bp - *ap) >> 1))
	        == *ap) {
	    bp = &c -> oid_elements[c -> oid_nelem++];

	    if (a -> oid_nelem <= i)
		*bp = 127;
	    else {
                while (*++ap == 255) {
                    bp++, c -> oid_nelem++;
 		    if (++i >= a -> oid_nelem) {
                        *bp = 127;
                        goto testc;
	            }
	        }

		*bp = *ap >= 16383 ? *ap + 16383
		    : *ap >= 4095  ? *ap + 4095
		    : *ap >= 1023  ? *ap + 1023
		    : *ap >   255  ? *ap + 255
		    :               (*ap >> 1) + 128;
	    }
	}

testc: ;
	if (c -> oid_nelem < 2)
	    c -> oid_elements[c -> oid_nelem++] = 0;
	break;
    }

    if (c == NULL) {
	char    buffer[BUFSIZ];

losing: ;
	if (debug) {
	    (void) strcpy (buffer, sprintoid (a));
	    fprintf (stderr, "oid_median(%s, %s) fails",
		     buffer, sprintoid (b));
	}
	return NULL;
    }
    if (oid_cmp (a, c) >= 0) {
	char    buf1[BUFSIZ],
		buf2[BUFSIZ];

	(void) strcpy (buf1, sprintoid (a));
	(void) strcpy (buf2, sprintoid (b));
	adios (NULLCP, "oid_median(%s, %s) -> %s loses(1)",
	       buf1, buf2, sprintoid (c));
    }
    if (oid_cmp (c, b) >= 0) {
	char    buf1[BUFSIZ],
		buf2[BUFSIZ];

	(void) strcpy (buf1, sprintoid (a));
	(void) strcpy (buf2, sprintoid (b));
	adios (NULLCP, "oid_median(%s, %s) -> %s loses(2)",
	       buf1, buf2, sprintoid (c));
    }

    return c;
}

/*  */

static OID  oid_copy (a)
OID	a;
{
    OID	    b;

    if ((b = oid_cpy (a)) == NULL)
	adios (NULLCP, "oid_copy: out of memory");

    return b;
}

#else

/*    DUMMY */

bulk_dummy () {}

#endif

