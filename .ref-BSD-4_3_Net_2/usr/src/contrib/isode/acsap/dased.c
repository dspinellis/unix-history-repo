/* dased.c - stand-alone DASE */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/acsap/RCS/dased.c,v 7.9 91/02/22 09:14:33 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/acsap/RCS/dased.c,v 7.9 91/02/22 09:14:33 mrose Interim $
 *
 *
 * $Log:	dased.c,v $
 * Revision 7.9  91/02/22  09:14:33  mrose
 * Interim 6.8
 * 
 * Revision 7.8  91/01/07  12:39:44  mrose
 * update
 * 
 * Revision 7.7  90/12/23  18:39:06  mrose
 * update
 * 
 * Revision 7.6  90/12/17  22:12:53  mrose
 * -call
 * 
 * Revision 7.5  90/12/11  10:52:08  mrose
 * lock-and-load
 * 
 * Revision 7.4  90/11/04  19:14:45  mrose
 * update
 * 
 * Revision 7.3  90/10/29  18:37:57  mrose
 * updates
 * 
 * Revision 7.2  90/07/27  08:41:41  mrose
 * update
 * 
 * Revision 7.1  90/07/09  14:30:57  mrose
 * sync
 * 
 * Revision 7.0  90/07/07  16:11:31  mrose
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


#include <signal.h>
#include <stdio.h>
#include <varargs.h>
#include "manifest.h"
#include <sys/ioctl.h>
#ifdef BSD42
#include <sys/file.h>
#endif
#ifdef SYS5
#include <fcntl.h>
#endif
#include "DASE-types.h"
#include "psap.h"
#include "tsap.h"
#include "dgram.h"
#include "tailor.h"

#include "quipu/ufn.h"
#include "quipu/util.h"
#include "quipu/read.h"
#include "quipu/dua.h"
#include "quipu/bind.h"


#ifdef	DEBUG
#define	STATS
#endif

/*  */

static int debug = 0;
static int nbits = FD_SETSIZE;

static LLog _pgm_log = {
	"dased.log", NULLCP,	NULLCP,
	LLOG_FATAL | LLOG_EXCEPTIONS | LLOG_NOTICE, LLOG_FATAL,
	-1, LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};

static	LLog	*pgm_log = &_pgm_log;

static char	*pgmname = "dased";

static struct TSAPaddr tas;

static	int	isbound = 0;
static	int	prebind = 0;
extern	int	dsap_ad;

static	DN	userdn = NULL;
static	char	passwd[DBA_MAX_PASSWD_LEN];

static	PS	ps;
static	PS	nps;


int	dns_compar ();
DNS	dase_interact (), just_say_no ();
PE	name2psap ();

void	adios (), advise (), ts_adios (), ts_advise ();


char   *dn2str ();
PE	grab_pe ();

/*  */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char	**argv,
        **envp;
{
    int	    vecp;
    char   *vec[4];
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect  *td = &tds;

    arginit (argv);
    envinit ();

    advise (LLOG_NOTICE, NULLCP, "listening on %s", taddr2str (&tas));
    if (TNetListen (&tas, td) == NOTOK)
	ts_adios (td, "TNetListen failed");

    for (;;) {
	int	nfds,
		secs;
	fd_set  ifds,
	       *rfds;

	if (!isbound && prebind)
	    (void) bind_to_dsa ();

	if (isbound) {
	    rfds = &ifds;

	    nfds = dsap_ad + 1;
	    
	    FD_ZERO (rfds);
	    FD_SET (dsap_ad, rfds);

	    secs = NOTOK;
	}
	else
	    nfds = 0, rfds = NULLFD, secs = prebind ? 5 * 60L : NOTOK;

	if (TNetAcceptAux (&vecp, vec, NULLIP, NULLTA, nfds, rfds, NULLFD,
			   NULLFD, secs, td) == NOTOK) {
	    ts_advise (td, LLOG_EXCEPTIONS, "TNetAccept failed");
	    continue;
	}

	if (rfds && FD_ISSET (dsap_ad, rfds)) {	/* DSA timed us out... */
	    if (debug)
		advise (LLOG_DEBUG, NULLCP, "unbound from directory");

	    (void) ds_unbind ();
	    isbound = 0;
	}

	if (vecp <= 0)
	    continue;

	if (debug)
	    break;

	switch (TNetFork (vecp, vec, td)) {
	    case OK:
	        ll_hdinit (pgm_log, pgmname);
		break;

	    case NOTOK:
		ts_advise (td, LLOG_EXCEPTIONS, "TNetFork failed");
		continue;

	    default:
		if (isbound) {
		    if (dsap_ad != NOTOK)
			(void) close (dsap_ad), dsap_ad = NOTOK;
		    isbound = 0;
		}
		continue;
	}
	break;
    }

    dased (vecp, vec);
}

/*    DASE */

static	dased (vecp, vec)
int	vecp;
char  **vec;
{
    int	    sd;
    struct TSAPstart tss;
    register struct TSAPstart *ts = &tss;
    struct TSAPdisconnect   tds;
    register struct TSAPdisconnect  *td = &tds;

    if (TInit (vecp, vec, ts, td) == NOTOK)
	ts_adios (td, "T-CONNECT.INDICATION failed");

    sd = ts -> ts_sd;
    advise (LLOG_NOTICE, NULLCP, "T-CONNECT.INDICATION: <%d, %s, %s, %d, %d>",
	    ts -> ts_sd, taddr2str (&ts -> ts_calling),
	    taddr2str (&ts -> ts_called), ts -> ts_expedited,
	    ts -> ts_tsdusize);

    if (TConnResponse (sd, NULLTA, 0, NULLCP, 0, NULLQOS, td) == NOTOK)
	ts_adios (td, "T-CONNECT.RESPONSE failed");

    if ((ps = ps_alloc (dg_open)) == NULLPS)
	adios (NULLCP, "ps_alloc: out of memory");
    if (dg_setup (ps, sd, MAXDGRAM, ts_read, ts_write, NULLIFP) == NOTOK)
	adios (NULLCP, "dg_setup: %s", ps_error (ps -> ps_errno));

    for (;;) {
	struct type_DASE_Query__REQ *req;
	PE	pe;

	if ((pe = ps2pe (ps)) == NULLPE) {
	    if (ps -> ps_errno == PS_ERR_NONE) {
		advise (LLOG_NOTICE, NULLCP, "T-DISCONNECT.INDICATION");
		break;
	    }
	    else
		adios (NULLCP, "ps2pe: %s", ps_error (ps -> ps_errno));
	}

	if (decode_DASE_Query__REQ (pe, 1, NULLIP, NULLVP, &req) == NOTOK)
	    adios (NULLCP, "decode_DASE_Query__REQ: %s", PY_pepy);
	PLOGP (pgm_log,DASE_Message, pe, "message", 1);

	dase_aux (req);

	free_DASE_Query__REQ (req);
	pe_free (pe);
    }

    if (isbound) {
	if (debug)
	    advise (LLOG_DEBUG, NULLCP, "unbound from directory");

	(void) ds_unbind ();
	isbound = 0;
    }

    exit (0);
}

/*  */

static	dase_aux (req)
register struct type_DASE_Query__REQ *req;
{
    register int    i;
    int	    vecp;
    register char **vp;
    char   *context,
	  **vec;
    register struct type_DASE_Query__RSP *rsp;
    register struct element_DASE_0 *d0;
    register struct element_DASE_1 *d1;
    envlist el;
    register envlist  en,
		     *ep;
    DN	   *dn;
    DNS	    dns;
    PE	    pe;

    if ((rsp = (struct type_DASE_Query__RSP *) calloc (1, sizeof *rsp))
	    == NULL) {
no_mem: ;
	adios (NULLCP, "out of memory");
    }

    vec = NULL, el = NULL, context = NULL, dns = NULL;

    i = 1;
    for (d0 = req -> name; d0; d0 = d0 -> next)
	i++;
    if ((vec = (char **) calloc ((unsigned) i, sizeof *vec)) == NULL)
	goto no_mem;
    for (vp = vec, d0 = req -> name; d0; vp++, d0 = d0 -> next)
	if ((*vp = qb2str (d0 -> IA5String)) == NULL)
	    goto no_mem;
#ifdef	STATS
	else
	    advise (LLOG_NOTICE, NULLCP, "lookup: %s", *vp);
#endif
    vecp = i - 1;

    el = NULL, ep = &el;
    for (d1 = req -> envlist; d1; d1 = d1 -> next) {
	struct type_DASE_Environment *ev = d1 -> Environment;
	register struct element_DASE_2 *d2;
	register struct dn_seq **dp;

	if ((en = (envlist) calloc (1, sizeof *en)) == NULL)
	    goto no_mem;
	*ep = en, ep = &en -> Next;

	en -> Upper = ev -> upper;
	en -> Lower = ev -> lower;

	dp = &en -> Dns;
	for (d2 = ev -> path; d2; d2 = d2 -> next) {
	    char   *cp;
	    register struct dn_seq *ds;

	    if ((ds = (struct dn_seq *) calloc (1, sizeof *ds)) == NULL)
		goto no_mem;
	    *dp = ds, dp = &ds -> dns_next;

	    if ((cp = qb2str (d2 -> IA5String)) == NULL)
		goto no_mem;
	    if (*cp != '-')
		ds -> dns_dn = str2dn (cp);
	    free (cp);

	    if (*cp != '-' && ds -> dns_dn == NULLDN) {
		PY_advise (NULLCP, "bad DN in environment (%s)", cp);
		goto send_rsp;
	    }
	}
    }

    if ((context = qb2str (req -> context)) == NULL)
	goto no_mem;
#ifdef	STATS
    advise (LLOG_NOTICE, NULLCP, "context: %s", context);
#endif

    if (req -> userdn) {
	int	changed = 0;
	char   *cp;
	DN	newdn;

	if ((cp = qb2str (req -> userdn)) == NULL)
	    goto no_mem;
#ifdef	STATS
	advise (LLOG_NOTICE, NULLCP, "userdn: %s", cp);
#endif
	if ((newdn = str2dn (*cp != '@' ? cp : cp + 1)) == NULLDN) {
	    PY_advise (NULLCP, "bad DN for userdn (%s)", cp);
	    free (cp);
	    goto send_rsp;
	}
	if (!userdn || dn_cmp (userdn, newdn))
	    changed++;
	if (userdn)
	    dn_free (userdn);
	userdn = newdn;
	free (cp);

	if (req -> passwd) {
	    if ((cp = qb2str (req -> passwd)) == NULL)
		goto no_mem;
	    if (strcmp (passwd, cp))
		changed++;
	    (void) strcpy (passwd, cp);
	    free (cp);
	}
	else {
	    if (passwd[0])
		changed++;
	    passwd[0] = NULL;
	}

	if (isbound && changed) {
	    (void) ds_unbind ();
	    isbound = 0;
	}
    }

    if (!isbound && bind_to_dsa () == NOTOK)
	goto send_rsp;

    PY_pepy[0] = NULL;
    pe = NULLPE;
    if (vecp == 1 && *vec[0] == '@') {
	static DN dnstat;
	
	if ((dnstat = str2dn (vec[0])) == NULLDN) {
	    PY_advise (NULLCP, "invalid name");
	    goto send_rsp;
	}

	rsp -> value = name2psap (*(dn = &dnstat));
	goto all_done;
    }

    if (!aet_match (vecp, vec, req -> interactive ? dase_interact
						  : just_say_no,
		    &dns, el, context)) {
	if (PY_pepy[0] == NULL)
	    PY_advise (NULLCP, "unable to resolve name");
	goto send_rsp;
    }

    if (dns == NULL) {
	PY_advise (NULLCP, "search failed to find anything");
	goto send_rsp;
    }
    dn = NULL;

    if (dns -> dns_next) {
	if (req -> interactive)
	    (void) dnSelect (vec[0], &dns, dase_interact, el -> Dns);

	for (; dns; dns = dns -> dns_next) {
	    dn = &dns -> dns_dn;
	    if (rsp -> value = name2psap (*dn))
		break;
	}
    }
    else {
	dn = &dns -> dns_dn;
	rsp -> value = name2psap (*dn);
    }

all_done: ;
    if (dn) {
	(void) encode_IF_DistinguishedName (&rsp -> name, 1, NULL, NULLCP,*dn);
#ifdef	STATS
	advise (LLOG_NOTICE, NULLCP, "answer: %s", dn2str (*dn));
#endif

	ufn_dn_print_aux (nps, *dn, NULLDN, 0);
	ps_print (nps, " ");
	*--nps -> ps_ptr = NULL, nps -> ps_cnt++;

	rsp -> friendly = str2qb (nps -> ps_base, strlen (nps -> ps_base), 1);

	nps -> ps_ptr = nps -> ps_base, nps -> ps_cnt = nps -> ps_bufsiz;

	dn_free (*dn);
	*dn = NULLDN;
    }

send_rsp: ;
    if (PY_pepy[0]) {
	advise (LLOG_NOTICE, NULLCP, "diagnostic: %s", PY_pepy);

	if ((rsp -> diagnostic = str2qb (PY_pepy, strlen (PY_pepy), 1))
	        == NULL)
	    goto no_mem;
    }

    if (encode_DASE_Query__RSP (&pe, 1, NULL, NULLCP, rsp) == NOTOK)
	adios (NULLCP, "encode_DASE_Query__RSP: %s", PY_pepy);
    if (pe2ps (ps, pe) == NOTOK)
	adios (NULLCP, "pe2ps: %s", ps_error (ps -> ps_errno));
    PLOGP (pgm_log,DASE_Message, pe, "message", 0);

    free_DASE_Query__RSP (rsp);
    pe_free (pe);

    if (vec) {
	for (vp = vec; *vp; vp++)
	    free (*vp);
	free ((char *) vec);
    }

    for (; el; el = en) {
	en = el -> Next;

	dn_seq_free (el -> Dns);
	free ((char *) el);
    }

    if (context)
	free (context);

    if (dns)
	dn_seq_free (dns);
}

/*  */

static bind_to_dsa ()
{
    struct ds_bind_arg ba;
    struct ds_bind_arg br;
    struct ds_bind_error be;
    static int very_first_time = 1;

    if (isbound) {
	if (debug)
	    advise (LLOG_DEBUG, NULLCP, "unbound from directory");

	(void) ds_unbind ();
	isbound = 0;
    }

    make_bind_args (&ba, &br, &be);

    if (ds_bind (&ba, &be, &br) == DS_OK) {
	isbound = 1;
	very_first_time = 0;
	if (debug)
	    advise (LLOG_DEBUG, NULLCP, "bound to directory");

	return OK;
    }

    PY_advise (NULLCP, "unable to bind to directory (%s)",
	       be.dbe_type == DBE_TYPE_SECURITY ? "security error"
						: "DSA unavailable");
    if (prebind && very_first_time)
	very_first_time = 0;
    else
	advise (LLOG_EXCEPTIONS, NULLCP, "%s", PY_pepy);

    return NOTOK;
}

/*  */

static	int	make_bind_args (ba, br, be)
register struct ds_bind_arg *ba,
			    *br;
register struct ds_bind_error *be;
{
    bzero ((char *) ba, sizeof *ba);
    bzero ((char *) br, sizeof *br);
    bzero ((char *) be, sizeof *be);

    ba -> dba_version = DBA_VERSION_V1988;
    if (ba -> dba_dn = userdn)
	ba -> dba_auth_type = DBA_AUTH_SIMPLE;
    if (ba -> dba_passwd_len = strlen (passwd))
	(void) strcpy (ba -> dba_passwd, passwd);
}

/*  */

static DNS  dase_interact (dns, dn, s)
DNS	dns;
DN	dn;
char   *s;
{
    register int i;
    register struct type_DASE_Callback__REQ *req = NULL;
    register struct element_DASE_3 **dp;
    struct type_DASE_Callback__RSP *rsp = NULL;
    register struct type_DASE_Callback__RSP *rp;
    DNS	    ds,
	   *dq;
    PE	    pe = NULLPE;

    if (dns == NULLDNS)
	return NULL;

    i = 0;
    for (ds = dns; ds; ds = ds -> dns_next)
	i++;

    if (i > 1) {
	struct dn_seq **base,
		      **bp,
		      **ep;

	if (base = (struct dn_seq **) malloc ((unsigned) (i * sizeof *base))) {
	    ep = base;
	    for (ds = dns; ds; ds = ds -> dns_next)
		*ep++ = ds;

	    qsort ((char *) base, i, sizeof *base, dns_compar);

	    bp = base;
	    ds = dns = *bp++;
	    while (bp < ep) {
		ds -> dns_next = *bp;
		ds = *bp++;
	    }
	    ds -> dns_next = NULL;

	    free ((char *) base);
	}
    }

    if ((req = (struct type_DASE_Callback__REQ *) calloc (1, sizeof *req))
	    == NULL) {
no_mem: ;
        advise (LLOG_EXCEPTIONS, NULLCP, "out of memory");
out: ;
	if (req)
	    free_DASE_Callback__REQ (req);
	if (rsp)
	    free_DASE_Callback__RSP (rsp);
	if (pe)
	    pe_free (pe);

	dn_seq_free (dns);
	return NULL;
    }
    if ((req -> string = str2qb (s, strlen (s), 1)) == NULL)
	goto no_mem;
    
    dp = &req -> choices;
    for (; dns; dns = ds) {
	register struct element_DASE_3 *d3;
	register struct type_DASE_Pair *pair;

	if ((d3 = (struct element_DASE_3 *) calloc (1, sizeof *d3)) == NULL)
	    goto no_mem;
	*dp = d3, dp = &d3 -> next;

	if ((pair = (struct type_DASE_Pair *) calloc (1, sizeof *pair))
	        == NULL)
	    goto no_mem;
	d3 -> Pair = pair;

	dn_print (nps, dns -> dns_dn, EDBOUT);
	ps_print (nps, " ");
	*--nps -> ps_ptr = NULL, nps -> ps_cnt++;

	pair -> complete = str2qb (nps -> ps_base, strlen (nps -> ps_base), 1);
	
	nps -> ps_ptr = nps -> ps_base, nps -> ps_cnt = nps -> ps_bufsiz;
	
	ufn_dn_print_aux (nps, dns -> dns_dn, dn, 0);
	ps_print (nps, " ");
	*--nps -> ps_ptr = NULL, nps -> ps_cnt++;

	pair -> friendly = str2qb (nps -> ps_base, strlen (nps -> ps_base), 1);

	nps -> ps_ptr = nps -> ps_base, nps -> ps_cnt = nps -> ps_bufsiz;
	
	if (pair -> complete == NULL || pair -> friendly == NULL)
	    goto no_mem;

	ds = dns -> dns_next;

	dn_free (dns -> dns_dn);
	free ((char *) dns);
    }

    dns = NULL;

    if (encode_DASE_Callback__REQ (&pe, 1, NULL, NULLCP, req) == NOTOK) {
	advise (LLOG_EXCEPTIONS, NULLCP, "encode_DASE_Callback__REQ: %s",
		PY_pepy);
	goto out;
    }
    if (pe2ps (ps, pe) == NOTOK)
	adios (NULLCP, "pe2ps: %s", ps_error (ps -> ps_errno));
    PLOGP (pgm_log,DASE_Message, pe, "message", 0);

    free_DASE_Callback__REQ (req);
    req = NULL;
    pe_free (pe);

    if ((pe = ps2pe (ps)) == NULLPE)
	adios (NULLCP, "ps2pe: %s", ps_error (ps -> ps_errno));

    if (decode_DASE_Callback__RSP (pe, 1, NULLIP, NULLVP, &rsp) == NOTOK) {
	advise (LLOG_EXCEPTIONS, NULLCP, "decode_DASE_Callback__RSP: %s",
		PY_pepy);
	goto out;
    }
    PLOGP (pgm_log,DASE_Message, pe, "message", 1);

    dq = &dns;
    for (rp = rsp; rp; rp = rp -> next) {
	char   *cp;

	if ((ds = (struct dn_seq *) calloc (1, sizeof *ds)) == NULL)
	    goto no_mem;
	*dq = ds, dq = &ds -> dns_next;

	if ((cp = qb2str (rp -> IA5String)) == NULL)
	    goto no_mem;
	ds -> dns_dn = str2dn (cp);
	free (cp);
    }

    free_DASE_Callback__RSP (rsp);
    pe_free (pe);

    return dns;
}

/*  */

static	int	dns_compar (a, b)
struct dn_seq **a,
	      **b;
{
    int	    i;
    DN	    adn,
	    bdn;

    for (adn = (*a) -> dns_dn; adn -> dn_parent; adn = adn -> dn_parent)
	continue;
    for (bdn = (*b) -> dns_dn; bdn -> dn_parent; bdn = bdn -> dn_parent)
	continue;

    i = rdn_cmp (adn -> dn_rdn, bdn -> dn_rdn);
    return (i == (-1) || i == 1 ? i : 0);
}

/*  */

/* ARGSUSED */

static DNS  just_say_no (dns, dn, s)
DNS	dns;
DN	dn;
char   *s;
{
    dn_seq_free (dns);

    return NULL;
}

/*  */

static PE  name2psap (dn)
DN	dn;
{
    int	    i;
    AttributeType at;
    PE	    pe;
static struct ds_read_arg read_arg = 
	{
		default_common_args,
		NULLDN,   /* read_arg DN */
		{       /* entry info selection */
			FALSE,
			NULLATTR,
			EIS_ATTRIBUTESANDVALUES
		}
	};
    struct DSError  error;
    struct ds_read_result result;

    if ((at = AttrT_new (DSAADDRESS_OID)) == NULLAttrT) {
	PY_advise (NULLCP, "build of attribute failed (%s)", DSAADDRESS_OID);
	return NULLPE;
    }

    read_arg.rda_common.ca_servicecontrol.svc_prio = SVC_PRIO_HIGH;
    read_arg.rda_object = dn;
    read_arg.rda_eis.eis_select = as_comp_new (AttrT_cpy (at), NULLAV,
					       NULLACL_INFO);

    i = ds_read (&read_arg, &error, &result);

    AttrT_free (at);
    as_free (read_arg.rda_eis.eis_select);

    if (i != DS_OK) {
	PY_advise (NULLCP, "DAP lookup failed (%s)", dn2str (dn));
	return NULLPE;
    }

    if (result.rdr_entry.ent_attr == NULLATTR) {
	PY_advise (NULLCP, "no '%s' attribute in entry '%s'",
		   DSAADDRESS_OID, dn2str (dn));
	return NULLPE;
    }

    pe = grab_pe (&result.rdr_entry.ent_attr -> attr_value -> avseq_av);
    as_free (result.rdr_entry.ent_attr);
    return pe;
}

/*    INIT */

static	arginit (vec)
char	**vec;
{
    int	    argp;
    register char   *ap;
    char  **argptr,
	   *args[4];
    register struct TSAPaddr *ta = NULL;

    if (pgmname = rindex (*vec, '/'))
	pgmname++;
    if (pgmname == NULL || *pgmname == NULL)
	pgmname = *vec;

    isodetailor (pgmname, 0);
    ll_hdinit (pgm_log, pgmname);

    quipu_syntaxes ();

    argp = 0;
    args[argp++] = pgmname;
    for (argptr = vec, argptr++; ap = *argptr; argptr++) {
	if (*ap == '-')
	    switch (*++ap) {
		case 'a':
		case 'u':
		case 'p':
		    if ((ap = *++argptr) == NULL || *ap == '-')
			break;
		    continue;

		case 'c':
		    if ((ap = *++argptr) == NULL || *ap == '-')
			break;
		    args[argp++] = "-c";
		    args[argp++] = ap;
		    break;

		default:
		    continue;
	     }

	break;
    }
    args[argp] = NULLCP;

    dsap_init (&argp, (argptr = args, &argptr));

    userdn = NULLDN, passwd[0] = NULL;
    for (vec++; ap = *vec; vec++) {
	if (*ap == '-')
	    switch (*++ap) {
		case 'a':
		    if ((ap = *++vec) == NULL || *ap == '-')
			adios (NULLCP, "usage: %s -a address", pgmname);
		    if ((ta = str2taddr (ap)) == NULLTA)
			adios (NULLCP, "bad address \"%s\"", ap);
		    continue;

		case 'b':
		    prebind++;
		    continue;

		case 'd':
		    debug++;
		    continue;

		case 'c':
		    if ((ap = *++vec) == NULL || *ap == '-')
			adios (NULLCP, "usage: %s -c DSA-name-or-address",
			       pgmname);
		    continue;

		case 'u':
		    if ((ap = *++vec) == NULL || *ap == '-')
			adios (NULLCP, "usage: %s -u username", pgmname);
		    if ((userdn = str2dn (*ap != '@' ? ap : ap + 1)) == NULLDN)
			adios (NULLCP, "invalid DN for username: %s", ap);
		    bzero ((char *) ap, strlen (ap));
		    continue;

		case 'p':
		    if ((ap = *++vec) == NULL || *ap == '-')
			adios (NULLCP, "usage: %s -p passwd", pgmname);
		    (void) strcpy (passwd, ap);
		    bzero ((char *) ap, strlen (ap));
		    continue;

	        default:
		    adios (NULLCP, "unknown switch -%s", ap);
	    }

	adios (NULLCP, "usage: %s [switches]", pgmname);
    }

    if (ta == NULL && (ta = str2taddr (ns_address)) == NULLTA)
	adios (NULLCP, "bad default address \"%s\"", ns_address);
    tas = *ta;		/* struct copy */

    if ((nps = ps_alloc (str_open)) == NULLPS)
	adios (NULLCP, "ps_alloc: out of memory");
    if (str_setup (nps, NULLCP, 0, 0) == NOTOK)
	adios (NULLCP, "str_setup: %s", ps_error (ps -> ps_errno));
}

/*  */

static	envinit () {
    int     i,
	    sd;

    nbits = getdtablesize ();

    if (debug == 0 && !(debug = isatty (2))) {
	for (i = 0; i < 5; i++) {
	    switch (fork ()) {
		case NOTOK:
		    sleep (5);
		    continue;
		
		case OK:
		    break;

		default:
		    _exit (0);
		}
	    break;
	}

	(void) chdir ("/");

	if ((sd = open ("/dev/null", O_RDWR)) == NOTOK)
	    adios ("/dev/null", "unable to read");
	if (sd != 0)
	    (void) dup2 (sd, 0), (void) close (sd);
	(void) dup2 (0, 1);
	(void) dup2 (0, 2);

#ifdef	SETSID
	if (setsid () == NOTOK)
	    advise (LLOG_EXCEPTIONS, "failed", "setsid");
#endif
#ifdef  TIOCNOTTY
	if ((sd = open ("/dev/tty", O_RDWR)) != NOTOK) {
	    (void) ioctl (sd, TIOCNOTTY, NULLCP);
	    (void) close (sd);
	}
#else
#ifdef  SYS5
	(void) setpgrp ();
	(void) signal (SIGINT, SIG_IGN);
	(void) signal (SIGQUIT, SIG_IGN);
#endif
#endif
    }
    else
	ll_dbinit (pgm_log, pgmname);

#ifndef sun			/* damn YP... */
    for (sd = 3; sd < nbits; sd++)
	if (pgm_log -> ll_fd != sd)
	    (void) close (sd);
#endif

    (void) signal (SIGPIPE, SIG_IGN);

    ll_hdinit (pgm_log, pgmname);
    advise (LLOG_NOTICE, NULLCP, "starting");
}

/*    ERRORS */

#ifndef lint
static void    adios (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _ll_log (pgm_log, LLOG_FATAL, ap);

    va_end (ap);

    _exit (1);
}
#else
/* VARARGS */

static void    adios (what, fmt)
char   *what,
       *fmt;
{
    adios (what, fmt);
}
#endif


#ifndef lint
static void    advise (va_alist)
va_dcl
{
    int     code;
    va_list ap;

    va_start (ap);

    code = va_arg (ap, int);

    _ll_log (pgm_log, code, ap);

    va_end (ap);
}
#else
/* VARARGS */

static void    advise (code, what, fmt)
char   *what,
       *fmt;
int     code;
{
    advise (code, what, fmt);
}
#endif

/*  */

static void  ts_adios (td, event)
register struct TSAPdisconnect *td;
char	*event;
{
    ts_advise (td, LLOG_EXCEPTIONS, event);

    exit (1);
}

/*  */

static void  ts_advise (td, code, event)
register struct TSAPdisconnect *td;
int     code;
char   *event;
{
    char    buffer[BUFSIZ];

    if (td -> td_cc > 0)
	(void) sprintf (buffer, "[%s] %*.*s",
			TErrString (td -> td_reason),
			td -> td_cc, td -> td_cc, td -> td_data);
    else
	(void) sprintf (buffer, "[%s]", TErrString (td -> td_reason));

    advise (code, NULLCP, "%s: %s", event, buffer);
}
