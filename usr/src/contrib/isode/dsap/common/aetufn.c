/* aetufn.c - UFN-based DSE */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/aetufn.c,v 7.4 91/02/22 09:18:09 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/aetufn.c,v 7.4 91/02/22 09:18:09 mrose Interim $
 *
 *
 * $Log:	aetufn.c,v $
 * Revision 7.4  91/02/22  09:18:09  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/12/11  10:53:50  mrose
 * lock-and-load
 * 
 * Revision 7.2  90/10/17  11:40:51  mrose
 * sync
 * 
 * Revision 7.1  90/07/09  14:33:49  mrose
 * sync
 * 
 * Revision 7.0  90/07/06  23:18:10  mrose
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

#include "quipu/ufn.h"
#include "quipu/util.h"
#include "quipu/read.h"
#include "quipu/dua.h"
#include "quipu/bind.h"
#include "tailor.h"

extern LLog * addr_log;

extern char * dn2str();
extern char * dn2ufn();
extern struct dn_seq *dn_seq_push ();


/* ARGSUSED */
static DNS ufn_interact (dns,dn,s)
DNS dns;
DN dn;
char * s;
{
char    buf[LINESIZE];
DNS result = NULLDNS;
DNS tmp;

	if (dns == NULLDNS)
		return NULLDNS;

	(void) printf ("Please select from the following (matching '%s'):\n",s);
	while (dns != NULLDNS) {
		(void) printf ("  %s [y/n] ? ",dn2ufn(dns->dns_dn,FALSE));
		(void) fflush (stdout);
again:;
		if (gets (buf) == NULL) {
			clearerr (stdin);
			(void) printf ("\n");
			return result;
		}

		if ((buf[0] == NULL) 
			|| (strlen(buf) != 1)
			|| ((buf[0] != 'y') && (buf[0] != 'n'))) {
				(void) printf ("Please type 'y' or 'n': ");
				(void) fflush (stdout);
				goto again;
			}

		if (buf[0] == 'y') {
		    tmp = dns -> dns_next;
		    dns -> dns_next = result;
		    result = dns;
		    dns = tmp;
		} else {
		    tmp = dns;
		    dns = dns -> dns_next;
		    tmp -> dns_next = NULL;
		    dn_seq_free (tmp);
		}
	}
	return result;
}

/* ARGSUSED */

static DNS just_say_no (dns,dn,s)
DNS dns;
DN dn;
char * s;
{
	/* we only want good hits ! */

	dn_seq_free (dns);
	SLOG (addr_log, LLOG_NOTICE, NULLCP,
	      ("UFN asked for interactive response -- auto reply of NO"));
	return NULLDNS;
}

static DN username = NULLDN;
static char password[DBA_MAX_PASSWD_LEN] = "";

static bind_to_dsa ()
{
  struct ds_bind_arg bindarg;
  struct ds_bind_arg bindresult;
  struct ds_bind_error binderr;

  bindarg.dba_version = DBA_VERSION_V1988;
  bindarg.dba_dn = username;
  if (bindarg.dba_passwd_len = strlen (password))
      (void) strcpy (bindarg.dba_passwd, password);

  if (ds_bind (&bindarg,&binderr,&bindresult) != DS_OK) {
	PY_advise (NULLCP, "unable to bind to directory (%s)",
		   binderr.dbe_type == DBE_TYPE_SECURITY ? "security error"
		   					 : "DSA unavailable");

    	return FALSE;
    }

    return TRUE;
}

static char bound = FALSE;

static PE name2psap (dn)
DN dn;
{
AttributeType at;
extern PE grab_pe();
PE res_pe;
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


	if (! bound) {
	    if (! bind_to_dsa())
		goto out;

	    bound = TRUE;
	}

	if ( (at = AttrT_new (DSAADDRESS_OID)) == NULLAttrT) {
		PY_advise (NULLCP, "build of attribute failed: %s",
			   DSAADDRESS_OID);
out: ;
		SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP, ("%s", PY_pepy));
		return NULLPE;
	}

	read_arg.rda_common.ca_servicecontrol.svc_prio = SVC_PRIO_HIGH;
	read_arg.rda_object = dn;
	read_arg.rda_eis.eis_select = as_comp_new (AttrT_cpy (at), NULLAV, NULLACL_INFO);

	if (ds_read (&read_arg,&error,&result) != DS_OK) {
		PY_advise (NULLCP, "DAP lookup failed: %s",dn2str(dn));
		log_ds_error (&error);
		ds_error_free (&error);
		AttrT_free (at);
		as_free (read_arg.rda_eis.eis_select);
		goto out;
	} else {
		if (result.rdr_entry.ent_attr == NULLATTR) {
			PY_advise (NULLCP, "No '%s' attribute in entry '%s'",
				   DSAADDRESS_OID,dn2str(dn));
			AttrT_free (at);
			as_free (read_arg.rda_eis.eis_select);
			goto out;
		}
		AttrT_free (at);
		as_free (read_arg.rda_eis.eis_select);
		res_pe = grab_pe(&result.rdr_entry.ent_attr->attr_value->avseq_av);
		as_free (result.rdr_entry.ent_attr);
		return (res_pe);
	}
}

static char unbind = FALSE;
static envlist el = NULLEL;

/* ARGSUSED */

static PE  name2value_ufn (name, context, ontty, userdn, passwd, real_name)
char   *name,
       *context,
       *userdn,
       *passwd;
int	ontty;
PE     *real_name;
{
int n;
char * v[20],buffer[LINESIZE];
DNS dns = NULLDNS;
DN *dn;
PE addr;

	*real_name = NULLPE;

	if (username)
	    dn_free (username), username = NULLDN;
	if (userdn) {
	    if ((username = str2dn (userdn)) == NULLDN) {
		PY_advise (NULLCP, "invalid DN for binding: \"%s\"", userdn);
		goto out;
	    }
	}
	password[0] = NULL;
	if (passwd)
	    (void) strcpy (password, passwd);

	(void) strcpy (buffer, name);
	if (*buffer == '@') {
	    static DN dnstat;

	    if ((dnstat = str2dn (buffer)) == NULLDN) {
		PY_advise (NULLCP, "invalid name");
		goto out;
	    }
	    addr = name2psap (*(dn = &dnstat));
	    goto all_done;
	}

	if (el == NULLEL)
	    set_el ();

	PY_pepy[0] = NULL;

	if ((n = sstr2arg (buffer,20,v,",")) == NOTOK) {
	    PY_advise (NULLCP, "invalid name");
out: ;
	    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP, ("%s", PY_pepy));
	    return NULLPE;
	}

	if (! bound) {
	    if (! bind_to_dsa())
		goto out;

	    bound = TRUE;
	}

	if ( ! aet_match (n, v, ontty ? ufn_interact : just_say_no, &dns, el,
			  context)) {
	    if (PY_pepy[0] == NULL) {
		PY_advise (NULLCP, "unable to resolve name");
		goto out;
	    }
	    return NULLPE;
	}

	if (dns == NULLDNS) {
	    PY_advise (NULLCP, "search failed to find anything");
	    goto out;
	}
	dn = NULL;

	addr = NULLPE;
	if (dns->dns_next == NULLDNS) {
	        dn = &dns -> dns_dn;
		addr = name2psap (*dn);
	}
	else {
		/* Multiple hits */
		/* Continue until one works */
		if ( ontty ) 
			(void) dnSelect (name,&dns,ufn_interact,el->Dns);

		for (; dns!= NULLDNS; dns=dns->dns_next) {
		        dn = &dns -> dns_dn;
			if (addr = name2psap (*dn))
				break;
		}
	}

all_done: ;
	if (dn) {
	    PS	    ps = NULLPS;

	    (void) encode_IF_DistinguishedName (real_name, 1, 0, NULLCP, *dn);

	    if (ontty
		    && (ps = ps_alloc (str_open))
		    && str_setup (ps, NULLCP, 0, 0) != NOTOK) {
		(void) ufn_dn_print_aux (ps, *dn, NULLDN, 0);
		ps_print (ps, " ");
		*--ps -> ps_ptr = NULL, ps -> ps_cnt++;

		(void) printf ("[ using %s ]\n", ps -> ps_base);
		(void) fflush (stdout);

		ps -> ps_ptr = ps -> ps_base, ps -> ps_cnt = ps -> ps_bufsiz;
	    }
	    if (ps)
		ps_free (ps);

	    dn_free (*dn);
	    *dn = NULLDN;
	}
	dn_seq_free (dns);

	if (unbind) {
	    bound = FALSE;
	    ds_unbind ();
	}

	return addr;
}


static	set_el ()
{
    register envlist  en,
		     *ep;
    DN	    local_dn,
	    c_dn;
    static int  inited = FALSE;
    extern char *local_dit;

    if (!inited) {
	quipu_syntaxes ();
	dsap_init ((int *)0,(char ***)0);
	inited = TRUE;
    }
    if (el = read_envlist ())
	return;

    if (local_dn = str2dn (local_dit)) {
	DN	dn = local_dn -> dn_parent;

	local_dn -> dn_parent = NULLDN;
	c_dn = dn_cpy (local_dn);
	local_dn -> dn_parent = dn;
    }

    ep = &el;

    if ((en = (envlist) calloc (1, sizeof **ep)) == NULL) {
no_mem: ;
	SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP, ("out of memory"));
	if (el) {
	    for (; el; el = en) {
		en = el -> Next;

		dn_seq_free (el -> Dns);
		free ((char *) el);
	    }

	    el = NULLEL;
	}
	goto done;
    }
    *ep = en, ep = &en -> Next;
    en -> Dns =
	dn_seq_push (local_dn,
		     dn_seq_push (c_dn, dn_seq_push (NULLDN, NULLDNSEQ)));
    en -> Upper = en -> Lower = 1;

    if ((en = (envlist) calloc (1, sizeof **ep)) == NULL)
	goto no_mem;
    *ep = en, ep = &en -> Next;
    en -> Dns =
	dn_seq_push (c_dn,
		     dn_seq_push (local_dn, dn_seq_push (NULLDN, NULLDNSEQ)));
    en -> Upper = en -> Lower = 2;

    if ((en = (envlist) calloc (1, sizeof **ep)) == NULL)
	goto no_mem;
    *ep = en, ep = &en -> Next;
    en -> Dns =
	dn_seq_push (NULLDN,
		     dn_seq_push (c_dn, dn_seq_push (local_dn, NULLDNSEQ)));
    en -> Upper = 32767, en -> Lower = 3;

done: ;
    dn_free (local_dn);
    dn_free (c_dn);
}


set_lookup_ufn (flag)
char	flag;	/* if TRUE always unbind */
{
    if ((unbind = flag) && bound) {
	bound = FALSE;
	ds_unbind ();
    }

    acsap_lookup = name2value_ufn;

    if (el == NULLEL)
	set_el ();
}


#ifdef STANDALONE_AET_TEST

main (argc,argv)
int argc;
char ** argv;
{
char buffer [1024];
int ontty, n;
PE title, paddr;

	buffer[0] = NULL;

	ontty = isatty (fileno (stdin));
	for (n=1; n<argc; n++) {
		(void) strcat (buffer," ");
		(void) strcat (buffer, argv[n]);
	}

	isodetailor ("ufn_aet",1);

	quipu_syntaxes ();
	dsap_init ((int *)0,(char ***)0);

	addr_log -> ll_events |= LLOG_ALL, addr_log -> ll_stat |= LLOGTTY;

	if (pe = name2value_ufn (buffer, "iso ftam", ontty, &title)) {
	    struct PSAPaddr pas;

	    if (parse_DSE_PSAPaddr (pe, 1, NULLIP, NULLVP, (char *) &pas)
		    == NOTOK)
		fprintf (stderr, "parse of presentation address failed: %s",
			 PY_pepy);
	    else
		(void) printf ("%s\n", paddr2str (&pas));
	}
	else
	    fprintf (stderr, "directory returns no value");

	if (title) {
	    (void) printf ("AETitle\n");
	    vunknown (title);
	}

	exit (0);
}

advise ()
{
;
}

#endif
