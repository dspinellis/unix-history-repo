/* imagesbr.c - image subroutines */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/image/RCS/imagesbr.c,v 7.10 91/03/09 11:54:34 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/others/quipu/image/RCS/imagesbr.c,v 7.10 91/03/09 11:54:34 mrose Exp $
 *
 *
 * $Log:	imagesbr.c,v $
 * Revision 7.10  91/03/09  11:54:34  mrose
 * update
 * 
 * Revision 7.9  91/02/22  09:33:20  mrose
 * Interim 6.8
 * 
 * Revision 7.8  91/02/14  14:36:00  mrose
 * update
 * 
 * Revision 7.7  91/01/24  14:52:07  mrose
 * update
 * 
 * Revision 7.6  90/10/30  14:33:14  mrose
 * sync
 * 
 * Revision 7.5  90/10/23  20:40:26  mrose
 * update
 * 
 * Revision 7.4  90/04/18  08:50:12  mrose
 * typo
 * 
 * Revision 7.3  90/03/29  20:39:18  mrose
 * update
 * 
 * Revision 7.2  90/03/15  11:18:10  mrose
 * quipu-sync
 * 
 * Revision 7.1  90/03/08  08:04:12  mrose
 * touch-up
 * 
 * Revision 7.0  89/11/23  21:59:58  mrose
 * Release 6.0
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


#include <ctype.h>
#include <stdio.h>
#include <varargs.h>
#include "imagesbr.h"
#include "quipu/bind.h"
#include "quipu/ds_search.h"
#include "quipu/entry.h"

/*    GENERAL */

static char *myname = "image";

int	recording = 0;

static LLog _pgm_log = {
    "./image.log", NULLCP, NULLCP, LLOG_FATAL | LLOG_EXCEPTIONS | LLOG_NOTICE,
    LLOG_FATAL, -1, LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
LLog *pgm_log = &_pgm_log;


/*    AKA */

struct aka {
    struct aka *ak_forw;
    struct aka *ak_back;

    char   *ak_domain;
    char   *ak_local;

    struct dn_seq *ak_bases;
    struct type_IMAGE_Image *ak_image;
};
static struct aka akas;
static struct aka *AHead = &akas;


static int  stay_bound = 0;

DN	local_dn;


struct dn_seq *dm2dn_seq ();


extern char *local_dit;
struct dn_seq *dn_seq_push ();
int	dn_seq_print ();
PE	grab_pe ();

/*  */

void	init_aka (pgm, stayopen, dit)
char   *pgm,
       *dit;
int	stayopen;
{
    char   *cp;
    register struct aka *ak;
    static int once_only = 0;

    if (once_only == 0) {
	int	argp;
	char   *arg[2],
	      **argptr;

	quipu_syntaxes ();

	argp = 0;
	arg[argp++] = myname;
	arg[argp] = NULLCP;
	
	dsap_init (&argp, (argptr = arg, &argptr));

	once_only = 1;
    }
    stay_bound = stayopen;
    myname = pgm;

    AHead -> ak_forw = AHead -> ak_back = AHead;
    if ((ak = (struct aka *) calloc (1, sizeof *ak)) == NULL)
	adios (NULLCP, "out of memory");

    ak -> ak_domain = ak -> ak_local = "";

    insque (ak, AHead -> ak_back);

    if ((local_dn = str2dn (cp = dit ? dit
				     : *local_dit != '@' ? local_dit
							 : local_dit + 1))
	    == NULLDN)
	adios (NULLCP, "local_dit invalid: \"%s\"", cp);
}

/*  */

static struct aka *mbox2ak (local, domain)
char   *local,
       *domain;
{
    register struct aka *ak,
			*am;

    if (domain == NULL)
	domain = "";
    if (local == NULL)
	local = "";

    for (ak = AHead -> ak_forw; ak != AHead; ak = ak -> ak_forw)
	if (!lexequ (domain, ak -> ak_domain)
	        && !lexequ (local, ak -> ak_local)) {
	    if (recording)
		LLOG (pgm_log, LLOG_NOTICE,
		      ("hit \"%s\" \"%s\"", domain, local));

	    return ak;
	}

    if ((am = (struct aka *) calloc (1, sizeof *am)) == NULL
	    || (am -> ak_domain = strdup (domain)) == NULL
	    || (am -> ak_local = strdup (local)) == NULL)
	adios (NULLCP, "out of memory");

    if (recording)
	LLOG (pgm_log, LLOG_NOTICE, ("miss \"%s\" \"%s\"", domain, local));

    if (index (domain, '.'))
	am -> ak_bases = dm2dn_seq (domain);
    else
	am -> ak_bases = dn_seq_push (local_dn, NULLDNSEQ);

    insque (ak = am, AHead);

    return ak;
}

/*    DIRECTORY */

#define	ADOMAIN	"associatedDomain"
#define	PHOTO	"photo"
#define	USERID	"userid"

static int bound = 0;
static int dlevel = 0;

struct dn_seq *dm2dn_seq_aux ();


static struct dn_seq *dm2dn_seq (dm)
char   *dm;
{
    register char *dp;

    for (dp = dm; *dp; dp++)
	if (isupper (*dp))
	    *dp = tolower (*dp);

    dlevel = 0;

    if (!bound && do_bind () == NOTOK)
	return NULLDNSEQ;

    return dm2dn_seq_aux (dm, NULLDN, NULLDNSEQ);
}

/*  */

static struct dn_seq *dm2dn_seq_aux (dm, dn, dlist)
char   *dm;
DN	dn;
struct dn_seq *dlist;
{
    register char   *dp;
    char   buffer[BUFSIZ];
    struct ds_search_arg search_arg;
    register struct ds_search_arg *sa = &search_arg;
    struct ds_search_result search_result;
    register CommonArgs *ca;
    register struct ds_search_result *sr = &search_result;
    struct DSError error;
    register struct DSError *se = &error;
    PS	    ps;

    if ((ps = ps_alloc (str_open))
	    && str_setup (ps, buffer, sizeof buffer, 1) != NOTOK) {
	dn_print (ps, dn, EDBOUT);
	*ps -> ps_ptr = NULL;
    }
    else
	buffer[0] = NULL;
    if (ps)
	(void) ps_free (ps);

    if (debug)
	fprintf (stderr, "dlevel=%d dm=%s dn=%s\n", dlevel, dm, buffer);

    bzero ((char *) sa, sizeof *sa);

    ca = &sa -> sra_common;
    ca -> ca_servicecontrol.svc_options = SVC_OPT_PREFERCHAIN;
    ca -> ca_servicecontrol.svc_timelimit = SVC_NOTIMELIMIT;
    ca -> ca_servicecontrol.svc_sizelimit = SVC_NOSIZELIMIT;

    sa -> sra_baseobject = dn;
    sa -> sra_subset = SRA_ONELEVEL;
    sa -> sra_searchaliases = FALSE;

    dp = dm;
    for (;;) {
	int	    i;
	EntryInfo  *ptr;
	register filter *fi;
	register AttributeType at;

	if (debug)
	    fprintf (stderr, "-- dlevel=%d dp=%s(%d) dn=%s\n",
		     dlevel, dp, strlen (dp), buffer);

	if ((i = strlen (dp)) < dlevel)
	    break;

	sa -> sra_filter = fi = filter_alloc ();

	bzero ((char *) fi, sizeof *fi);
	fi -> flt_type = FILTER_ITEM;
	fi -> FUITEM.fi_type = FILTERITEM_EQUALITY;
	if ((fi -> FUITEM.UNAVA.ava_type = at = AttrT_new (ADOMAIN)) == NULL)
	    adios ("invalid attribute type \"%s\"", ADOMAIN);
	fi -> FUITEM.UNAVA.ava_value = str2AttrV (dp, at -> oa_syntax);

	if (ds_search (sa, se, sr) != DS_OK) {
	    if (recording)
		LLOG (pgm_log, LLOG_EXCEPTIONS,
		      ("search operation failed"));

	    (void) ds_unbind ();

	    bound = 0;

	    goto free_filter;
	}

	if (sr -> srr_correlated != TRUE)
	    correlate_search_results (sr);

	if (sr -> CSR_entries == NULLENTRYINFO) {
	    if (recording)
		LLOG (pgm_log, LLOG_NOTICE,
		      ("search for %s \"%s\" at baseobject \"%s\" failed",
		       ADOMAIN, dp, buffer));

	    filter_free (sa -> sra_filter);
	    if (dp = index (dp, '.'))
		dp++;
	    if (dp == NULL)
		break;
	    continue;
	}

	if (recording)
	    LLOG (pgm_log, LLOG_NOTICE,
		  ("search for %s \"%s\" at baseobject \"%s\" succeeded",
		   ADOMAIN, dp, buffer));

	if (i > dlevel) {
	    dlevel = i;
	    if (dlist)
		dn_seq_free (dlist), dlist = NULLDNSEQ;
	}

	if (i == dlevel)
	    for (ptr = sr -> CSR_entries; ptr; ptr = ptr -> ent_next) {
		struct dn_seq *dprev = dlist;

		dlist = dm2dn_seq_aux (dm, ptr -> ent_dn, dlist);

		if (dprev == dlist)
		    dlist = dn_seq_push (ptr -> ent_dn, dlist);
		else
		    if (i < dlevel)
			break;
	    }

	dn_free (sr -> CSR_object);
	entryinfo_free (sr -> CSR_entries, 0);
	crefs_free (sr -> CSR_cr);
free_filter: ;
	filter_free (sa -> sra_filter);
	break;
    }

    return dlist;
}

/*  */

static  PE  image_search (ak)
register struct aka *ak;
{
    register struct dn_seq *dlist;
    struct ds_search_arg search_arg;
    register struct ds_search_arg *sa = &search_arg;
    struct ds_search_result search_result;
    register CommonArgs *ca;
    register struct ds_search_result *sr = &search_result;
    struct DSError error;
    register struct DSError *se = &error;
    PE	    pe = NULLPE;
    extern int dn_print();

    bzero ((char *) sa, sizeof *sa);

    ca = &sa -> sra_common;
    ca -> ca_servicecontrol.svc_options = SVC_OPT_PREFERCHAIN;
    ca -> ca_servicecontrol.svc_timelimit = SVC_NOTIMELIMIT;
    ca -> ca_servicecontrol.svc_sizelimit = SVC_NOSIZELIMIT;

    sa -> sra_subset = SRA_WHOLESUBTREE;
    sa -> sra_searchaliases = FALSE;

    if (recording) {
	LLOG (pgm_log, LLOG_NOTICE, ("searching for %s %s",
				     ak -> ak_domain, ak -> ak_local));
	pslog (pgm_log, LLOG_NOTICE, "  using baseobjects ",
	       dn_seq_print, (caddr_t) ak -> ak_bases);
    }

    for (dlist = ak -> ak_bases; dlist; dlist = dlist -> dns_next) {
	register filter *fi;
	register AttributeType at;

	if ((sa -> sra_baseobject = dlist -> dns_dn) == NULL)
	    continue;

	sa -> sra_filter = fi = filter_alloc ();

	bzero ((char *) fi, sizeof *fi);
	fi -> flt_type = FILTER_ITEM;
	fi -> FUITEM.fi_type = FILTERITEM_EQUALITY;
	if ((fi -> FUITEM.UNAVA.ava_type = at = AttrT_new (USERID)) == NULL)
	    adios ("invalid attribute type \"%s\"", USERID);
	fi -> FUITEM.UNAVA.ava_value =
				str2AttrV (ak -> ak_local, at -> oa_syntax);

	sa -> sra_eis.eis_allattributes = FALSE;
	sa -> sra_eis.eis_select = as_comp_new (str2AttrT (PHOTO), NULLAV,
						NULLACL_INFO);
	sa -> sra_eis.eis_infotypes = EIS_ATTRIBUTESANDVALUES;

	if (ds_search (sa, se, sr) != DS_OK) {
	    if (recording)
		LLOG (pgm_log, LLOG_EXCEPTIONS, ("search operation failed"));

	    (void) ds_unbind ();

	    bound = 0;

	    goto free_filter;
	}

	if (sr -> srr_correlated != TRUE)
	    correlate_search_results (sr);

	if (sr -> CSR_entries == NULLENTRYINFO) {
	    if (recording)
		pslog (pgm_log, LLOG_NOTICE,
		      "search failed at baseobject ", dn_print,
		       (caddr_t) dlist -> dns_dn);
	    filter_free (sa -> sra_filter);
	    continue;
	}

	if (sr -> CSR_entries -> ent_attr == NULLATTR) {
	    if (recording)
		pslog (pgm_log, LLOG_NOTICE,
		      "search succeeded (but no attribute) at baseobject ",
		       dn_print, (caddr_t) dlist -> dns_dn);
	}
	else {
	    AttributeValue av = &sr -> CSR_entries -> ent_attr -> attr_value
								   -> avseq_av;

	    pe = pe_cpy (grab_pe (av));

	    if (recording)
		pslog (pgm_log, LLOG_NOTICE, "search succeeded at baseobject ",
		       dn_print, (caddr_t) dlist -> dns_dn);
	}

	dn_free (sr -> CSR_object);
	entryinfo_free (sr -> CSR_entries, 0);
	crefs_free (sr -> CSR_cr);
free_filter: ;
	filter_free (sa -> sra_filter);
	break;
    }

    return pe;
}

/*  */

static	do_bind ()
{
    struct ds_bind_arg bind_arg,
		       bind_result;
    register struct ds_bind_arg *ba = &bind_arg,
				*br = &bind_result;
    struct ds_bind_error bind_error;
    register struct ds_bind_error *be = &bind_error;

    bzero ((char *) ba, sizeof *ba);
    ba -> dba_version = DBA_VERSION_V1988;

    if (ds_bind (ba, be, br) != DS_OK) {
	if (recording)
	    LLOG (pgm_log, LLOG_EXCEPTIONS,
		  ("directory bind failed: %s",
		   be -> dbe_type == DBE_TYPE_SECURITY ? "security error"
						       : "DSA unavailable"));

	return NOTOK;
    }

    bound = 1;

    return OK;
}

/*    IMAGE */

static int    passno;
static int    x, y, maxx;

static struct type_IMAGE_Image *im = NULL;

/*  */

struct type_IMAGE_Image *fetch_image (local, domain)
char   *local,
       *domain;
{
    PE	    pe;
    PS	    ps;
    register struct aka *ak;

    if ((ak = mbox2ak (local, domain)) == NULL)
	return NULL;
    if (ak -> ak_image)
	return ak -> ak_image;

    if (!bound && do_bind () == NOTOK)
	return NULL;

    if ((pe = image_search (ak)) == NULLPE)
	return NULL;

    if (!stay_bound) {
	(void) ds_unbind ();
	bound = 0;
    }

    im = NULL;
    
    if ((ps = ps_alloc (str_open)) == NULLPS) {
	if (debug)
	    fprintf (stderr, "ps_alloc: failed\n");

	goto out;
    }
    if (str_setup (ps, NULLCP, 0, 0) == NOTOK) {
	if (debug)
	    fprintf (stderr, "ps_alloc: %s\n", ps_error (ps -> ps_errno));

	goto out;
    }
    
    if (pe2ps (ps, pe) == NOTOK) {
	if (debug)
	    fprintf (stderr, "pe2ps: %s\n", pe_error (pe -> pe_errno));

	goto out;
    }

    for (passno = 1; passno < 3; passno++)
	if (decode_t4 (ps -> ps_base, PHOTO, ps -> ps_ptr - ps -> ps_base)
	        == NOTOK) {
	    fprintf (stderr, "\n");
	    if (im) {
		qb_free (im -> data);
		free ((char *) im);
		im = NULL;
	    }
	    break;
	}

out: ;
    if (ps)
	ps_free (ps);
    pe_free (pe);

    return (ak -> ak_image = im);
}

/*  */

/* ARGSUSED */

photo_start (name)
char   *name;
{
    if (passno == 1)
	maxx = 0;
    x = y = 0;

    return OK;
}


/* ARGSUSED */

photo_end (name)
char   *name;
{
    int	    len;
    register struct qbuf *qb,
			 *pb;
    
    if (passno == 1) {
	x = maxx, y--;

	if (debug)
	    fprintf (stderr, "ending pass one for \"%s\", %dx%d\n",
		     name, x, y);

	if ((im = (struct type_IMAGE_Image *) calloc (1, sizeof *im))
	        == NULL) {
	    if (debug)
		fprintf (stderr, "calloc fails");
	    return NOTOK;
	}
	im -> width = x, im -> height = y;

	len = ((im -> width + 7) / 8) * im -> height;
	if ((im -> data = qb = (struct qbuf *) malloc (sizeof *qb)) == NULL) {
	    if (debug)
		fprintf (stderr, "unable to allocate qbuf");
	    return NOTOK;
	}
	qb -> qb_forw = qb -> qb_back = qb;
	qb -> qb_data = NULL, qb -> qb_len = len;
	
	if ((pb = (struct qbuf *) calloc ((unsigned) (sizeof *pb + len), 1))
	        == NULL) {
	    if (debug)
		fprintf (stderr, "unable to allocate qbuf (%d+%d)",
		     sizeof *qb, len);
	    return NOTOK;
	}
	pb -> qb_data = pb -> qb_base, pb -> qb_len = len;
	insque (pb, qb);
    }

    return OK;
}


photo_black (length)
int	length;
{
    if (passno == 2) {
	register int    i,
			j;
	register unsigned char *cp;

	cp = (unsigned char *) im -> data -> qb_forw -> qb_data
				       + ((im -> width + 7) / 8) * y + x / 8;
	i = x % 8;
	for (j = length; j > 0; j--) {
	    *cp |= 1 << i;
	    if (++i == 8)
		cp++, i = 0;
	}

    }

    x += length;

    return OK;
}


photo_white (length)
int	length;
{
    x += length;

    return OK;
}


/* ARGSUSED */

photo_line_end (line)
caddr_t line;
{
    if (passno == 1 && x > maxx)
	maxx = x;
    x = 0, y++;

    return OK;
}

/*    ERRORS */

#ifndef	lint
void	_advise ();


void	adios (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _advise (ap);

    va_end (ap);

    _exit (1);
}
#else
/* VARARGS */

void	adios (what, fmt)
char   *what,
       *fmt;
{
    adios (what, fmt);
}
#endif


#ifndef	lint
void	advise (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _advise (ap);

    va_end (ap);
}


static void  _advise (ap)
va_list	ap;
{
    char    buffer[BUFSIZ];

    asprintf (buffer, ap);

    (void) fflush (stdout);

    if (errsw != NOTOK) {
	fprintf (stderr, "%s: ", myname);
	(void) fputs (buffer, stderr);
	(void) fputc ('\n', stderr);

	(void) fflush (stderr);
    }    
}
#else
/* VARARGS */

void	advise (what, fmt)
char   *what,
       *fmt;
{
    advise (what, fmt);
}
#endif

/*    MISC */

#ifndef	lint
char   *strdup (s)
char   *s;
{
    char   *p;

    if (p = malloc ((unsigned) (strlen (s) + 1)))
	(void) strcpy (p, s);

    return p;
}
#endif
