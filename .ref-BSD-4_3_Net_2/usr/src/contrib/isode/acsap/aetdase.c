/* aetdase.c - DASE-based DSE */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/acsap/RCS/aetdase.c,v 7.6 91/02/22 09:14:28 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/acsap/RCS/aetdase.c,v 7.6 91/02/22 09:14:28 mrose Interim $
 *
 *
 * $Log:	aetdase.c,v $
 * Revision 7.6  91/02/22  09:14:28  mrose
 * Interim 6.8
 * 
 * Revision 7.5  91/01/07  12:39:41  mrose
 * update
 * 
 * Revision 7.4  90/12/23  18:39:01  mrose
 * update
 * 
 * Revision 7.3  90/12/11  10:52:00  mrose
 * lock-and-load
 * 
 * Revision 7.2  90/10/29  18:37:54  mrose
 * updates
 * 
 * Revision 7.1  90/07/09  14:30:48  mrose
 * sync
 * 
 * Revision 7.0  90/07/07  16:11:32  mrose
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


/* LINTLIBRARY */

#include <ctype.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include "DASE-types.h"
#include "psap.h"
#include "tsap.h"
#include "dgram.h"
#include "tailor.h"


/*    DATA */

static	int	stayopen = 0;

static	struct TSAPconnect tcs;
static	PS	ps = NULLPS;


static	int	armed = 0;
static	int	interrupted;
static	jmp_buf	intrenv;

SFD	intrser ();


struct element_DASE_1 *read_el ();

/*    LOOKUP */

/* ARGSUSED */

PE	name2value_dase (name, context, ontty, userdn, passwd, real_name)
char   *name,
       *context,
       *userdn,
       *passwd;
int	ontty;
PE     *real_name;
{
    int	    done,
	    err,
	    nfds,
	    vecp;
    fd_set  ifds;
    char   *vec[NVEC + 1];
    PE	    pe = NULLPE,
	    result = NULLPE;
    SFP	    istat;
    register struct type_DASE_Query__REQ *parm = NULL;

    *real_name = NULLPE;

    if (ontty) {
	istat = signal (SIGINT, intrser);
	interrupted = 0;
    }

    if (ps == NULLPS && dase_init () == NOTOK)  {
	if (ontty)
	    (void) signal (SIGINT, istat);

	return NULLPE;
    }
    err = 0;

    if (*name == '@') {
	vec[vecp = 0] = name;
	vecp++;
    }
    else
	if ((vecp = sstr2arg (name, NVEC, vec, ",")) == NOTOK) {
	    PY_advise (NULLCP, "invalid name");
	    goto out;
	}

    if ((parm = (struct type_DASE_Query__REQ *) calloc (1, sizeof *parm))
	    == NULL) {
no_mem: ;
        PY_advise (NULLCP, "name2value_dase: out of memory");
	goto out;
    }
    {
	register char **vp;
	register struct element_DASE_0  *dl,
				       **dp;

	dp = &parm -> name;
	for (vp = vec; vecp > 0; vp++, vecp--) {
	    if ((dl = (struct element_DASE_0 *) calloc (1, sizeof *dl))
		    == NULL)
		goto no_mem;
	    *dp = dl;
	    dp = &dl -> next;

	    if ((dl -> IA5String = str2qb (*vp, strlen (*vp), 1)) == NULL)
		goto no_mem;
	}
    }
    parm -> interactive = ontty ? 1 : 0;
    if ((parm -> envlist = read_el ()) == NULL)
	goto out;
    if ((parm -> context = str2qb (context, strlen (context), 1)) == NULL)
	goto no_mem;
    if (userdn
	    && (parm -> userdn = str2qb (userdn, strlen (userdn), 1)) == NULL)
	goto no_mem;
    if (passwd
	    && (parm -> passwd = str2qb (passwd, strlen (passwd), 1)) == NULL)
	goto no_mem;;

    if (encode_DASE_Query__REQ (&pe, 1, NULL, NULLCP, parm) == NOTOK)
	goto out;

    if ((err = pe2ps (ps, pe)) == NOTOK) {
	PY_advise (NULLCP, "unable to write query [%s]",
		   ps_error (ps -> ps_errno));
	goto out;
    }
    PLOGP (addr_log,DASE_Message, pe, "message", 0);

    FD_ZERO (&ifds);

    nfds = tcs.tc_sd + 1;
    FD_SET (tcs.tc_sd, &ifds);
    
    for (done = 0; !done; ) {
	fd_set	rfds;
	struct type_DASE_Provider__RSP *rsp = NULL;
	PE	in;

	if (!interrupted) {
	    rfds = ifds;		/* struct copy */

	    (void) xselect (nfds, &rfds, NULLFD, NULLFD, NOTOK);
	}
	
	if (interrupted) {
	    PY_advise (NULLCP, "interrupted");
	    break;
	}

	if (!FD_ISSET (tcs.tc_sd, &rfds))
	    continue;

	if ((in = ps2pe (ps)) == NULLPE) {
	    PY_advise (NULLCP, "unable to read response [%s]",
		       ps_error (ps -> ps_errno));
	    err++;
	    break;
	}

	if (decode_DASE_Provider__RSP (in, 1, NULLIP, NULLVP, &rsp) == NOTOK) {
you_lose: ;
	    pe_free (in);
	    if (rsp)
		free_DASE_Provider__RSP (rsp);
	    err++;
	    break;
	}
	PLOGP (addr_log,DASE_Message, in, "message", 1);

	switch (rsp -> offset) {
	    case type_DASE_Provider__RSP_callback:
	        if (!ontty) {
		    PY_advise (NULLCP, "unexpected callback from nameservice");
		    goto you_lose;
		}
		if (dase_callback (rsp -> un.callback) == NOTOK)
		    goto you_lose;
		break;

	    case type_DASE_Provider__RSP_answer:
		{
		    register char *pp,
				  *ep;
		    register struct qbuf *p,
					 *q;
		    register struct type_DASE_Query__RSP *ans
							    = rsp -> un.answer;

		    if ((q = ans -> friendly) && ontty) {
			printf ("[ using ");
			print_qb (q);
			printf (" ]\n");
			(void) fflush (stdout);
		    }
		    *real_name = ans -> name, ans -> name = NULLPE;
		    result = ans -> value, ans -> value = NULLPE;
		    ep = (pp = PY_pepy) + BUFSIZ - 1;
		    if (q = ans -> diagnostic)
			for (p = q -> qb_forw;
			         p != q && pp < ep;
			         pp += p -> qb_len, p = p -> qb_forw)
			    bcopy (p -> qb_data, pp, p -> qb_len);
		    *pp = NULL;

		    done = 1;
		}
	        break;

	    default:
		PY_advise (NULLCP, "unexpected response from nameservice: %d",
			   rsp -> offset);
	        goto you_lose;
	}

	pe_free (in);
	free_DASE_Provider__RSP (rsp);
    }

out: ;
    if (result == NULLPE)
	SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP, ("%s", PY_pepy));
    if (parm)
	free_DASE_Query__REQ (parm);
    if (pe)
	pe_free (pe);
    if (ps && (err || !stayopen)) {
	struct TSAPdisconnect tds;

	(void) TDiscRequest (tcs.tc_sd, NULLCP, 0, &tds);
	ps_free (ps);
	ps = NULLPS;
    }

    if (ontty)
	(void) signal (SIGINT, istat);

    return result;
}

/*  */

static int  dase_init () {
    int	    i,
	    nfds;
    fd_set  ifds;
    register struct TSAPaddr *tz;
    register struct TSAPconnect *tc = &tcs;
    struct TSAPdisconnect tds;
    register struct TSAPdisconnect *td = &tds;

    if ((tz = str2taddr (ns_address)) == NULL) {
	PY_advise (NULLCP, "unable to parse nameservice address \"%s\"",
		   ns_address);
out: ;
	SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP, ("%s", PY_pepy));
	return NOTOK;
    }

    if ((i = TAsynConnRequest (NULLTA, tz, 0, NULLCP, 0, NULLQOS, tc, td, 1))
	     == NOTOK) {
you_lose: ;
	PY_advise (NULLCP, td -> td_cc > 0
			       ? "unable to connect to nameservice: [%s] %*.*s"
			       : "unable to connect to nameservice: [%s]",
		   TErrString (td -> td_reason),
		   td -> td_cc, td -> td_cc, td -> td_data);
	goto out;
    }

    FD_ZERO (&ifds);

    nfds = tc -> tc_sd + 1;
    FD_SET (tc -> tc_sd, &ifds);

    while (i == CONNECTING_1 || i == CONNECTING_2) {
	fd_set	 mask,
		*rmask,
		*wmask;

	if (!interrupted) {
	    mask = ifds;		/* struct copy */
	    if (i == CONNECTING_2)
		rmask = &mask, wmask = NULLFD;
	    else
		rmask = NULLFD, wmask = &mask;

	    (void) xselect (nfds, rmask, wmask, NULLFD, NOTOK);
	}

	if (interrupted) {
	    PY_advise (NULLCP, "interrupted");
	    goto oops;
	}

	if ((rmask && !FD_ISSET (tc -> tc_sd, rmask))
	        || (wmask && !FD_ISSET (tc -> tc_sd, wmask)))
	    continue;

	if ((i = TAsynRetryRequest (tc -> tc_sd, tc, td)) == NOTOK)
	    goto you_lose;
    }

    if ((ps = ps_alloc (dg_open)) == NULLPS
	    || dg_setup (ps, tc -> tc_sd, MAXDGRAM, ts_read, ts_write, NULLIFP)
		    == NOTOK) {
	if (ps == NULLPS)
	    PY_advise (NULLCP, "ps_alloc: out of memory");
	else {
	    PY_advise (NULLCP, "dg_setup: %s", ps_error (ps -> ps_errno));
	    ps_free (ps);
	    ps = NULL;
	}

oops: ;
	(void) TDiscRequest (tcs.tc_sd, NULLCP, 0, td);
	goto out;
    }

    return OK;
}

/*  */

static int  dase_callback (arg)
register struct type_DASE_Callback__REQ *arg;
{
    register int i,
		 j;
    int	    result;
    register struct element_DASE_3 *choice;
    struct type_DASE_Callback__RSP *rsp = NULL;
    register struct type_DASE_Callback__RSP **rp = &rsp;
    PE	    pe = NULLPE;

    i = 0;
    for (choice = arg -> choices; choice; choice = choice -> next)
	i++;

    if (i > 10) {
	printf ("%d imprecise matches for '", i);
	print_qb (arg -> string);
	printf ("', select fron them [y/n] ? ");
	if (yesno () != OK)
	    goto send_rsp;
    }
    else {
	printf ("Please select from the following %d match%s for '",
		i, i != 1 ? "es" : "");
	print_qb (arg -> string);
	printf ("':\n");
    }

    j = 1;
    for (choice = arg -> choices; choice; choice = choice -> next) {
	register struct type_DASE_Callback__RSP *yes;

	printf ("  ");
	print_qb (choice -> Pair -> friendly);
	printf (" [y/n] ? ");

	switch (yesno ()) {
	    case DONE:
	        goto send_rsp;

	    case NOTOK:
	    default:
		break;

	    case OK:
		if ((yes = (struct type_DASE_Callback__RSP *)
					    calloc (1, sizeof *yes)) == NULL) {
		    PY_advise (NULLCP, "dase_callback: out of memory");
		    result = NOTOK;
		    goto out;
		}
		*rp = yes, rp = &yes -> next;

		yes -> IA5String = choice -> Pair -> complete;
		choice -> Pair -> complete = NULL;
		break;
	}

	if ((j++ % 10) == 0 && choice -> next) {
	    printf ("Continue (%d more) [y/n] ? ", i - j + 1);
	    if (yesno () != OK)
		break;
	}
    }

send_rsp: ;
    if ((result = encode_DASE_Callback__RSP (&pe, 1, NULL, NULLCP, rsp))
	    == NOTOK)
	goto out;
    if ((result = pe2ps (ps, pe)) == NOTOK)
	PY_advise (NULLCP, "unable to write callback [%s]",
		   ps_error (ps -> ps_errno));
    else {
	PLOGP (addr_log,DASE_Message, pe, "message", 0);

	result = OK;
    }

out: ;
    if (rsp)
	free_DASE_Callback__RSP (rsp);
    if (pe)
	pe_free (pe);

    return result;
}

/*  */

static int  yesno () {
    int     x,
            y,
            result;

    if (interrupted) {
	interrupted = 0;
	return DONE;
    }

    switch (setjmp (intrenv)) {
	case OK: 
	    armed++;
	    break;

	case NOTOK: 
	default: 
	    printf ("\n");
	    armed = 0;
	    return DONE;
    }
    
again: ;
    x = y = getc (stdin);
    while (y != '\n' && y != EOF)
	y = getc (stdin);

    switch (x) {
	case 'y': 
	case '\n':
	    result = OK;
	    break;

	case 'n': 
	    result = NOTOK;
	    break;

	case EOF: 
	    result = DONE;
	    clearerr (stdin);
	    break;

	default: 
	    printf ("Please type 'y' or 'n': ");
	    goto again;
    }

    armed = 0;

    return result;
}


static	print_qb (q)
struct qbuf *q;
{
    register struct qbuf *p;

    for (p = q -> qb_forw; p != q; p = p -> qb_forw)
	printf ("%*.*s", p -> qb_len, p -> qb_len, p -> qb_data);
}

/*  */

static struct element_DASE_1 *read_el () {
    register int   i;
    register char *bp,
		  *cp;
    char    buffer[BUFSIZ],
	    ufnrc[BUFSIZ];
    FILE   *fp;
    struct element_DASE_1 *top;
    register struct element_DASE_1 **etail;
    register struct element_DASE_2 **dtail;

    if (bp = getenv ("UFNRC"))
	(void) strcpy (ufnrc, bp);
    else {
	if ((bp = getenv ("HOME")) == NULL)
	    bp = ".";

	(void) sprintf (ufnrc, "%s/.ufnrc", bp);
    }

    if ((fp = fopen (ufnrc, "r")) == NULL) {
	(void) strcpy (ufnrc, isodefile ("ufnrc", 0));

	if ((fp = fopen (ufnrc, "r")) == NULL) {
	    PY_advise (ufnrc, "unable to read");
	    return NULL;
	}
    }

    top = NULL, etail = &top, dtail = NULL;

    for (i = 0; fgets (bp = buffer, sizeof buffer, fp); i++) {
	register struct element_DASE_2 *dl;

	if (*buffer == '#')
	    continue;
	if (bp = index (buffer, '\n'))
	    *bp = NULL;

	bp = buffer;
	if (*bp == NULL) {
	    dtail = NULL;
	    continue;
	}

	if (!isspace (*bp)) {
	    register char *dp;
	    register struct element_DASE_1 *el;
	    register struct type_DASE_Environment *fl;

	    if ((el = (struct element_DASE_1 *) calloc (1, sizeof *el))
		    == NULL) {
no_mem: ;
		PY_advise (NULLCP, "real_el: out of memory");

out: ;
		for (; top; top = el) {
		    el = top -> next;

		    free_DASE_Environment (top -> Environment);
		    free ((char *) top);
		}
		SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP, ("%s", PY_pepy));
		(void) fclose (fp);
		return NULL;
	    }
	    *etail = el;
	    etail = &el -> next;

	    if ((fl = (struct type_DASE_Environment *) calloc (1, sizeof *fl))
		    == NULL)
		goto no_mem;
	    el -> Environment = fl;
	    dtail = &fl -> path;

	    if ((cp = index (bp, ':')) == NULL) {
		PY_advise (NULLCP, "%s: missing ':' at line %d", ufnrc, i);
		goto out;
	    }
	    *cp++ = NULL;

	    if (dp = index (bp, ',')) {
		*dp++ = NULL;

		while (isspace (*dp))
		    dp++;
		fl -> upper = *dp == '+' ? 32767 : atoi (dp);
	    }
	    else
		fl -> upper = 0;

	    fl -> lower = atoi (bp);
	    if (fl -> upper == 0)
		fl -> upper = fl -> lower;

	    bp = cp;
	}
	else
	    if (!dtail) {
		PY_advise (NULLCP, "%s: unexpected blank at start of line %d",
			   ufnrc, i);
		goto out;
	    }

	if ((dl = (struct element_DASE_2 *) calloc (1, sizeof *dl)) == NULL)
	    goto no_mem;
	*dtail = dl;
	dtail = &dl -> next;

	while (isspace (*bp))
	    bp++;
	if ((dl -> IA5String = str2qb (bp, strlen (bp), 1)) == NULL)
	    goto no_mem;
    }

    (void) fclose (fp);

    return top;
}

/*  */

/* ARGSUSED */

static SFD  intrser (sig)
int	sig;
{
#ifndef	BSDSIGS
    (void) signal (SIGINT, intrser);
#endif

    if (armed)
	longjmp (intrenv, NOTOK);

    interrupted++;
}

/*  */

set_lookup_dase (flag)
char	flag;
{
    if (!(stayopen = flag) && ps) {
	struct TSAPdisconnect tds;

	(void) TDiscRequest (tcs.tc_sd, NULLCP, 0, &tds);
	ps_free (ps);
	ps = NULLPS;
    }

    acsap_lookup = name2value_dase;
}
