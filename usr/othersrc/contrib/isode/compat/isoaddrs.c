/* isoaddrs.c - simple parsing of ISODE addresses */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/isoaddrs.c,v 7.6 91/02/22 09:15:16 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/isoaddrs.c,v 7.6 91/02/22 09:15:16 mrose Interim $
 *
 *
 * $Log:	isoaddrs.c,v $
 * Revision 7.6  91/02/22  09:15:16  mrose
 * Interim 6.8
 * 
 * Revision 7.5  90/12/23  18:39:31  mrose
 * update
 * 
 * Revision 7.4  90/12/11  10:52:46  mrose
 * lock-and-load
 * 
 * Revision 7.3  90/11/21  11:29:45  mrose
 * sun
 * 
 * Revision 7.2  90/07/09  14:31:51  mrose
 * sync
 * 
 * Revision 7.1  90/01/11  18:35:10  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  21:23:05  mrose
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


/* LINTLIBRARY */

#include <ctype.h>
#include <stdio.h>
#include "general.h"
#include "manifest.h"
#include "isoaddrs.h"
#include "internet.h"
#include "tailor.h"

/*    DATA */

static char *isomacros = "isomacros";

#define	MBUCKETS	128
#define	MHASH(nm) \
    (((nm)[1]) ? (((chrcnv[((nm)[0])] - chrcnv[((nm)[1])]) & 0x1f) \
		  	+ ((chrcnv[(nm)[2]]) & 0x5f)) \
	       : (chrcnv[(nm)[0]]) & 0x7f)

struct macro {
    char   *m_name;
    char   *m_value;

    struct macro *m_chain;
};

static int inited = 0;
static struct macro *Mbuckets[MBUCKETS];

/*    MACROS */

static struct macro *name2macro (name)
char   *name;
{
    register struct macro *m;

    read_macros ();

    for (m = Mbuckets[MHASH (name)];
	     m && lexequ (m -> m_name, name);
	     m = m -> m_chain)
	continue;

    if (m)
	LLOG (addr_log, LLOG_DEBUG,
	      ("MACRO \"%s\" VALUE \"%s\"", m -> m_name, m -> m_value));
    else
	LLOG (addr_log, LLOG_DEBUG,
	      ("lookup of MACRO \"%s\" failed", name));

    return m;
}

/*  */

static struct macro *value2macro (value)
char   *value;
{
    register int   i,
		   j,
		   k;
    register struct macro *m,
			  *p,
			 **np,
			 **pp;

    read_macros ();

    p = NULL, i = 0;
    k = strlen (value);
    for (pp = (np = Mbuckets) + MBUCKETS; np < pp; np++)
	for (m = *np; m; m = m -> m_chain)
	    if ((j = strlen (m -> m_value)) <= k
		    &&  j > i
		    && strncmp (value, m -> m_value, j) == 0)
		p = m, i = j;

    if (p)
	LLOG (addr_log, LLOG_DEBUG,
	      ("MACRO \"%s\" VALUE \"%s\" differential %d",
	       p -> m_name, p -> m_value, k - strlen (p -> m_value)));
    else
	LLOG (addr_log, LLOG_DEBUG,
	      ("lookup of VALUE \"%s\" failed", value));

    return p;
}

/*  */

static int  read_macros ()
{
    register char *hp;
    char    buffer[BUFSIZ];

    if (inited)
	return;
    inited = 1;

    bzero ((char *) Mbuckets, sizeof Mbuckets);

    read_file (isodefile (isomacros, 0));

    if ((hp = getenv ("HOME")) == NULL)
	hp = ".";
    (void) sprintf (buffer, "%s/.isode_macros", hp);
    read_file (buffer);
}

/*  */

static int  read_file (file)
char   *file;
{
    register char *cp;
    char    buffer[BUFSIZ + 1],
	   *vec[NVEC + NSLACK + 1];
    register FILE *fp;

    if ((fp = fopen (file, "r")) == NULL)
	return;

    while (fgets (buffer, sizeof buffer, fp)) {
	if (*buffer == '#')
	    continue;
	if (cp = index (buffer, '\n'))
	    *cp  = NULL;
	if (str2vec (buffer, vec) < 2)
	    continue;

	if (add_macro (vec[0], vec[1]) == NOTOK)
	    break;
    }

    (void) fclose (fp);
}

/*  */

static int  add_macro (name, value)
char   *name,
       *value;
{
    int	    i;
    register char  *cp;
    char    buffer[BUFSIZ];
    register struct macro *m,
			  *p;

    if (cp = index (value, '=')) {
	*cp++ = NULL;
	if ((p = name2macro (value)) == NULL) {
	    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
		  ("macro \"%s\" references non-existant macro \"%s\"",
		   name, value));
	    return OK;
	}

	(void) sprintf (value = buffer, "%s%s", p -> m_value, cp);
    }

    if ((m = (struct macro *) calloc (1, sizeof *m)) == NULL) {
	SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
	      ("calloc of macro structure failed"));
	return NOTOK;
    }
    if ((m -> m_name = malloc ((unsigned) (strlen (name) + 1))) == NULL
		|| (m -> m_value = malloc ((unsigned) (strlen (value) + 1)))
	    == NULL) {
	SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
	      ("malloc of alias structure failed"));
	if (m -> m_name)
	    free (m -> m_name);
	free ((char *) m);
	return NOTOK;
    }
    (void) strcpy (m -> m_name, name);
    (void) strcpy (m -> m_value, value);

    m -> m_chain = Mbuckets[i = MHASH (m -> m_name)];
    Mbuckets[i] = m;

    return OK;
}

/*  */

char   *macro2str (name)
char   *name;
{
    register struct macro *m = name2macro (name);

    return (m ? m -> m_value : NULLCP);
}

/*    STR2PADDR */

#define	PS_INIT	0	/* <selector> or <network-address> */
#define	PS_SEL1	1	/*   .. got one selector already */
#define	PS_SEL2	2	/*   .. got two selectors already */
#define	PS_SEL3	3	/* <network-address> */


static struct afi_info {
    char   *p_name;
    char   *p_dec0, *p_hex0, *p_dec1, *p_hex1, *p_ia5;
    int	   p_idi_len, p_dec_dsp_len, p_hex_dsp_len;	
}	afi_entries[] = {
    "X121",  "36", "37", "52", "53", NULL, 14, 24,  9,
    "DCC",   "38", "39", NULL, NULL, NULL,  3, 35, 14,
    "TELEX", "40", "41", "54", "55", NULL,  8, 30, 12,
    "PSTN",  "42", "43", "56", "57", NULL, 12, 26, 10,
    "ISDN",  "44", "45", "58", "59", NULL, 15, 23,  9,
    "ICD",   "46", "47", NULL, NULL, NULL,  4, 34, 13,
    "LOCAL", "48", "49", NULL, NULL, "50",  0, 38, 19,

    NULL
};
#define p_ia5_dsp_len(pp) \
	(pp -> p_ia5 == NULL ? NOTOK : (pp -> p_dec_dsp_len >> 1))


static char sel1[TSSIZE];
static char sel2[TSSIZE];
static char sel3[TSSIZE];
static char *sels[3] = {
    sel1, sel2, sel3
};


#define	IMPLODE(intres,octres,octval,intval,losing,loslab) \
{ \
    register int   z = (intval); \
    register char *y = (octval); \
    register char *zp = y + z; \
 \
    while (zp-- > y) \
	if (!isxdigit ((u_char) *zp)) { \
loslab: ; \
	    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP, \
		  ("invalid hexstring: \"%*.*s\"", \
		   z, z, y)); \
	    return (losing); \
	} \
    if (z % 2) \
	goto loslab; \
    (intres) = implode ((u_char *) (octres), y, z); \
}

/*  */

struct PSAPaddr *str2paddr (str)
char   *str;
{
    register int    state,
		   *lp;
    int	    j,
	    lens[3];
    register char  *cp,
		   *dp,
		   *ep,
    		   *fp,
		   *np,
		  **sp;
    char    buf1[BUFSIZ],
	    buf2[BUFSIZ],
	    nsap[NASIZE * 2 + 1];
    register struct macro *m;
    register struct afi_info *pp;
    static int i = 0;
    static struct PSAPaddr pas[2];
    register struct PSAPaddr *pa = &pas[i++];
    register struct SSAPaddr *sa = &pa -> pa_addr;
    register struct TSAPaddr *ta = &sa -> sa_addr;
    register struct NSAPaddr *na = ta -> ta_addrs;

    i = i % 2;

    bzero ((char *) pa, sizeof *pa);
    (void) strcpy (buf1, str);

    state = PS_INIT;
    sp = sels, lp = lens;

    for (cp = buf1; *cp; )
	switch (state) {
	    case PS_INIT:	
	    case PS_SEL1:
	    case PS_SEL2:
		switch (*cp) {
		    case '"':		/* '"' <otherstring> '"' */
		        if ((cp = index (dp = cp + 1, '"')) == NULL) {
			    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
				  ("missing double-quote in selector: %s",
				   str));
			    return NULLPA;
			}
			*cp++ = NULL;
			(void) strcpy (*sp, dp);
			*lp = strlen (dp);
			break;

		    case '#':		/* '#' <digitstring> */
			j = 0;
			for (cp++; isdigit ((u_char) *cp); cp++)
			    j = j * 10 + *cp - '0';
			if (j > 0xffff) {
			    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
				  ("invalid #-style selector: %s", str));
			    return NULLPA;
			}
			(*sp)[0] = (j >> 8) & 0xff;
			(*sp)[1] = j & 0xff;
			*lp = 2;
			break;

		    case '\'':		/* "'" <hexstring> "'H" */
			if ((cp = index (dp = cp + 1, '\'')) == NULL) {
missing_quoteH: ;
			    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
				  ("missing 'H in selector: %s",str));
			    return NULLPA;
			}
			*cp++ = NULL;
			if (*cp++ != 'H')
			    goto missing_quoteH;
			IMPLODE (*lp, *sp, dp, strlen (dp), NULLPA, L1);
			break;

		    case '/':		/* empty selector */
			*lp = 0;
			break;

		    default:
			goto stuff_selectors;
		}
		sp++, lp++;
		if (*cp++ != '/') {
		    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
			  ("missing selector seperator at position %d: %s",
			   cp - buf1, str));
		    return NULLPA;
		}
		state++;
		break;

stuff_selectors: ;
		state = PS_SEL3;
		/* and fall */

	    case PS_SEL3:
		if ((cp = index (ep = cp, '|')) == NULL)
		    cp = ep + strlen (ep);
		else
		    *cp++ = NULL;

		if (dp = index (ep, '=')) {
		    *dp++ = NULL;
		    if ((m = name2macro (ep)) == NULL) {
			SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
			      ("non-existant macro \"%s\"", ep));
			return NULLPA;
		    }
		    (void) sprintf (ep = buf2, "%s%s", m -> m_value, dp);
		}

		{
		    register int    k,
				    l,
				    n;
		    register struct ts_interim *ts,
					       *tp;

		    tp = NULL, n = 0;
		    k = strlen (ep);
		    for (ts = ts_interim; ts -> ts_name; ts++)
			if (ts -> ts_value
				&& (l = strlen (ts -> ts_value)) <= k
			        && l > n
			        && strncmp (ep, ts -> ts_value, l) == 0)
			    tp = ts, n = l;
		    if (tp)
			na -> na_community = tp -> ts_subnet;
		    else {
			/* Assume all the AFI+IDI[+DSP] formats are 
			 * SUBNET_REALNS until later. Note that the X121 format
			 * without a DSP is converted to SUBNET_INT_X25 later.
			 */
			for (pp = afi_entries; pp -> p_name; pp++)
			    if (strncmp (pp -> p_name, ep, 
					 strlen (pp -> p_name) - 1) == 0 &&
				ep[strlen (pp -> p_name)] == '+')
			        break;
			if (!pp -> p_name) {
			    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
				  ("unable to determine community for %s",ep));
			    return NULLPA;
			}
			na -> na_community = SUBNET_REALNS;
		    }
		}

		if ((ep = index (dp = ep, '+')) == NULL) {
missing_seperator: ;
		    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
			  ("missing network-address seperator: %s", str));
		    return NULLPA;
		}
		*ep++ = NULL;
		if (ta -> ta_naddr >= NTADDR) {
#ifdef	h_addr
too_many: ;
#endif
		    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
			  ("too many network addresses starting at position %d: %s",
			   dp - buf1 + 1, str));
		    return pa;
		}

		na -> na_stack = NA_NSAP;
		if (lexequ (dp, "NS") == 0) {
		    IMPLODE (na -> na_addrlen, na -> na_address, ep,
			     strlen (ep), NULLPA, L2);
		}
		else {
		    int	    len;
		    char    padchar;

		    for (pp = afi_entries; pp -> p_name; pp++)
			if (lexequ (pp -> p_name, dp) == 0)
			    break;
		    if (!pp -> p_name) {
			SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
			      ("unknown AFI \"%s\": %s", dp, str));
			return NULLPA;
		    }
		    if ((ep = index (dp = ep, '+')) == NULL)
			ep = dp + strlen (dp);
		    else
			*ep++ = NULL;
		    for (fp = dp; *fp; fp++)
			    if (!isdigit ((u_char) *fp))
				    break;
		    if (*fp) {
			    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
				  ("invalid IDI \"%s\": %s", dp, cp));
			    return NULLPA;
		    }
		    if (lexequ (pp -> p_name, "X121") == 0 &&
			*ep == NULL) {
			/* X121 form -- should be more general
			 * Only applies if the DSP is NULL
			 */
		        (void) strcpy (nsap, dp);
			if ((na -> na_dtelen = strlen (nsap)) > NSAP_DTELEN) {
			    dp = nsap;
			    goto invalid_dte;
			}
			(void) strcpy (na -> na_dte, nsap);
#ifdef	BRIDGE_X25
			na -> na_stack = bridgediscrim (na) ? NA_BRG : NA_X25;
#else
			na -> na_stack = NA_X25;
#endif
			na -> na_community = SUBNET_INT_X25;
			goto next;
		    }
		    switch (*ep) {
			case 'd':
			case NULL:
			    if (*dp == '0' && pp -> p_dec1 != NULL)
				fp = pp -> p_dec1, padchar = '1';
			    else
				fp = pp -> p_dec0, padchar = '0';
			    (void) strcpy (nsap, fp);
			    fp = nsap + strlen (nsap);
			    for (len = pp -> p_idi_len - strlen (dp);
				     len > 0;
				     len--)
				*fp++ = padchar;
			    (void) strcpy (fp, dp);
			    fp += strlen (fp);
			    if (*ep != NULL)
			    	(void) strcpy (fp, ep + 1);
			    goto handle_dsp;	

			case 'x':
			    if (*dp == '0' && pp -> p_hex1 != NULL)
				fp = pp -> p_hex1, padchar = '1';
			    else
				fp = pp -> p_hex0, padchar = '0';
			    (void) strcpy (nsap, fp);
			    fp = nsap + strlen (nsap);
			    for (len = pp -> p_idi_len - strlen (dp);
				     len > 0;
				     len--)
				*fp++ = padchar;
			    (void) strcpy (fp, dp);
			    /* Odd length IDI padded below */
			    goto handle_dsp;

			case 'l':
			    if (pp -> p_ia5 == NULL) {
				SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
				      ("No IA5 syntax for AFI \"%s\"",
				       pp -> p_name));
				    return NULLPA;
			    }
			    (void) strcpy (nsap, pp -> p_ia5);
			    goto handle_dsp;

handle_dsp: ;
			    np = nsap, dp = na -> na_address;
			    while (*np) {
				*dp = (*np++ - '0') << 4;
				if (*np)
				    *dp++ |= (*np++ - '0') & 0x0f;
				else
				    *dp++ |= 0x0f;
			    }
			    na -> na_addrlen = dp - na -> na_address;
			    if (*ep == 'x') {
				IMPLODE (j, dp, ep + 1, strlen (ep + 1),
					 NULLPA, L3);
				na -> na_addrlen += j;
			    }
			    else
				if (*ep == 'l') {
				    (void) strcpy (dp, ep + 1);
				    na -> na_addrlen += strlen (ep + 1);
				}
			    break;

		        default:
			    if (*dp == '0' && pp -> p_dec1 != NULL)
				fp = pp -> p_dec1, padchar = '1';
			    else
				fp = pp -> p_dec0, padchar = '0';
			    (void) strcpy (nsap, fp);
			    fp = nsap + strlen (nsap);
			    for (len = pp -> p_idi_len - strlen (dp);
				     len > 0;
				     len--)
				*fp++ = padchar;
			    (void) strcpy (fp, dp);
			    if (strncmp ("RFC-1006+", ep,
					 sizeof "RFC-1006+" - 1) == 0) {
#ifdef	h_addr
				register char **ap;
#endif
				register struct hostent *hp;

				na -> na_stack = NA_TCP;
				ep += sizeof "RFC-1006+" - 1;
				if ((ep = index (dp = ep, '+')) == NULL)
				    goto missing_seperator;
				*ep++ = NULL;
				if ((ep = index (dp = ep, '+')) == NULL)
				    ep = dp + strlen (dp);
				else
				    *ep++ = NULL;

				if ((hp = gethostbystring (dp)) == NULL) {
				    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
					  ("%s: unknown host", dp));
				    return NULLPA;
				}
				(void) strcpy (na -> na_domain,
					       inet_ntoa (*(struct in_addr *)
							        hp -> h_addr));
				if (*ep) {
				    if ((ep = index (dp = ep, '+')) == NULL)
					ep = dp + strlen (dp);
				    else
					*ep++ = NULL;
				    na -> na_port = htons ((u_short) atoi(dp));

				    if (*ep)
					na -> na_tset = atoi (ep);
				}
#ifdef	h_addr
				for (ap = hp -> h_addr_list + 1; *ap; ap++) {
				    ta -> ta_naddr++, na++;

				    if (ta -> ta_naddr >= NTADDR)
					goto too_many;
				    bcopy ((char *) (na - 1), (char *) na,
					   sizeof *na);
				    (void) strcpy (na -> na_domain,
					  inet_ntoa (*(struct in_addr *) *ap));
				}
#endif
				break;
			    }
			    if (strncmp ("X.25(80)+", ep,
					 sizeof "X.25(80)+" - 1) == 0) {
				na -> na_stack = NA_X25;
				ep += sizeof "X.25(80)+" - 1;
				if ((ep = index (dp = ep, '+')) == NULL)
				    goto missing_seperator;
				*ep++ = NULL;
				if ((ep = index (dp = ep, '+')) == NULL)
				    ep = dp + strlen (dp);
				else
				    *ep++ = NULL;
				for (np = dp; *np; np++)
				    if (!isdigit ((u_char) *np)) {
invalid_dte: ;
					SLOG (addr_log, LLOG_EXCEPTIONS,
					      NULLCP,
					      ("invalid DTE \"%s\": %s",
					       dp, str));
					return NULLPA;
				    }
				if (np - dp > NSAP_DTELEN + 1)
				    goto invalid_dte;
				(void) strcpy (na -> na_dte, dp);
				na -> na_dtelen = strlen (na -> na_dte);
				if (*ep) {
				    char   *cudf,
					   *clen;

				    if ((ep = index (dp = ep, '+')) == NULL)
					goto missing_seperator;
				    *ep++ = NULL;
				    
				    if (lexequ (dp, "CUDF") == 0) {
					cudf = na -> na_cudf;
					clen = &na -> na_cudflen;
					j = sizeof na -> na_cudf;
				    }
				    else
					if (lexequ (dp, "PID") == 0) {
					    cudf = na -> na_pid;
					    clen = &na -> na_pidlen;
					    j = sizeof na -> na_pid;
					}
					else {
invalid_field: ;
					    SLOG (addr_log, LLOG_EXCEPTIONS,
						  NULLCP,
						  ("invalid field \"%s\": %s",
						   dp, str));
					    return NULLPA;
					}
				    if (j * 2 < strlen (ep))
					goto invalid_field;
				    IMPLODE (j, cudf, ep, strlen (ep),
					     NULLPA, L4);
				    *clen = j & 0xff;
				}
#ifdef	BRIDGE_X25
				na -> na_stack = bridgediscrim (na) ? NA_BRG
								   : NA_X25;
#endif
				break;
			    }
#ifdef	notdef
			    if (lexequ ("ECMA-117-Binary", ep) == 0) {
				/* some day support this... */
				break;
			    }
			    if (lexequ ("ECMA-117-Decimal", ep) == 0) {
				/* some day support this... */
				break;
			    }
#endif
			    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
				  ("unknown DSP \"%s\": %s", ep, str));
			    return NULLPA;
		    }
		}
next: ;
		ta -> ta_naddr++, na++;
		break;
	}
    
    switch (sp - sels) {
        case 3:	/* PSEL+SSEL+TSEL */
	    bcopy (*--sp, ta -> ta_selector,
		   ta -> ta_selectlen = *--lp);
	    bcopy (*--sp, sa -> sa_selector,
		   sa -> sa_selectlen = *--lp);
	    bcopy (*--sp, pa -> pa_selector,
		   pa -> pa_selectlen = *--lp);
	    break;

	case 2:	/* SSEL+TSEL */
	    bcopy (*--sp, ta -> ta_selector,
		   ta -> ta_selectlen = *--lp);
	    bcopy (*--sp, sa -> sa_selector,
		   sa -> sa_selectlen = *--lp);
	    break;

	case 1:	/* TSEL */
	    bcopy (*--sp, ta -> ta_selector,
		   ta -> ta_selectlen = *--lp);
	    break;

	default:
	    break;
    }
    
    return pa;
}

/*  */

int	macro2comm (name, ts)
char   *name;
register struct ts_interim *ts;
{
    int	    j,
	    len;
    register char  *ap,
		   *cp,
		   *dp,
		   *ep,
		   *fp,
		   *np;
    char    padchar,
	    addr[NASIZE * 2 + 1],
	    buffer[BUFSIZ];
    register struct afi_info *pp;

    ts -> ts_length = 0, ts -> ts_syntax = NA_NSAP;
    if ((cp = macro2str (name)) == NULLCP)
	return NOTOK;
    ts -> ts_value = cp;
    (void) strcpy (buffer, cp);
    ap = addr;

    if ((ep = index (dp = buffer, '+')) == NULL)
	ep = dp + strlen (dp);
    else
	*ep++ = NULL;

    if (lexequ (dp, "NS") == 0) {
	IMPLODE (ts -> ts_length, ts -> ts_prefix, ep, strlen (ep),
		 NOTOK, L5);

	return OK;
    }
    
    for (pp = afi_entries; pp -> p_name; pp++)
	if (lexequ (pp -> p_name, dp) == 0)
	    break;
    if (!pp -> p_name) {
	SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
	      ("unknown AFI \"%s\": %s", dp, cp));
	return NOTOK;
    }

    if (!ep) {
	    /* No IDI */
	    (void) strcpy (ap, pp -> p_dec0);
	    ap += strlen (ap);
	    goto out;
    }

    if ((ep = index (dp = ep, '+')) == NULL)
	ep = dp + strlen (dp);
    else
	*ep++ = NULL;

    for (fp = dp; *fp; fp++)
	if (!isdigit ((u_char) *fp))
	    break;
    if (*fp) {
	SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
	      ("invalid IDI \"%s\": %s", dp, cp));
	return NOTOK;
    }

    if (lexequ (pp -> p_name, "X121") == 0 && *ep == NULL) {
	/* Only used if there is no DSP */
 	(void) strcpy (ap, dp);
	ap += strlen (ap);

	ts -> ts_syntax = NA_X25;
	ts -> ts_subnet = SUBNET_INT_X25;
	goto out;
    }

    switch (*ep) {
	case 'd':
        case NULL:
	    if (*dp == '0' && pp -> p_dec1 != NULL)
		fp = pp -> p_dec1, padchar = '1';
	    else
		fp = pp -> p_dec0, padchar = '0';
	    (void) strcpy (ap, fp);
	    ap += strlen (ap);
	    for (len = pp -> p_idi_len - strlen (dp); len > 0; len--)
		*ap++ = padchar;
	    (void) strcpy (ap, dp);
	    ap += strlen (ap);
	    if (*ep != NULL)
		(void) strcpy (ap, ep + 1);
	    break;

	case 'x':
	    if (*dp == '0' && pp -> p_hex1 != NULL)
		fp = pp -> p_hex1, padchar = '1';
	    else
		fp = pp -> p_hex0, padchar = '0';
	    (void) strcpy (ap, fp);
	    ap += strlen (ap);
	    for (len = pp -> p_idi_len - strlen (dp); len > 0; len--)
		*ap++ = padchar;
	    (void) strcpy (ap, dp);
	    /* Odd length IDI padded below */
	    break;

 	case 'l':
	    if (pp -> p_ia5 == NULL) {
		SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
		      ("No IA5 syntax for AFI \"%s\": %s", pp -> p_name, cp));
		return NOTOK;
	    }
	    (void) strcpy (ap, pp -> p_ia5);
	    break;

	default:
	    if (*dp == '0' && pp -> p_dec1 != NULL)
		fp = pp -> p_dec1, padchar = '1';
	    else
		fp = pp -> p_dec0, padchar = '0';
	    (void) strcpy (ap, fp);
	    ap += strlen (ap);
	    for (len = pp -> p_idi_len - strlen (dp); len > 0; len--)
		*ap++ = padchar;
	    (void) strcpy (ap, dp);
	    ap += strlen (ap);

	    if ((ep = index (dp = ep, '+')) == NULL)
		ep = dp + strlen (dp);
	    else
		*ep++ = NULL;
	    if (lexequ (dp, "RFC-1006") == 0)
		ts -> ts_syntax = NA_TCP;
	    else
		if (lexequ (dp, "X.25(80)") == 0)
		    ts -> ts_syntax = NA_X25;
		else {
		    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
			  ("unknown DSP \"%s\": %s", dp, cp));
		    return NOTOK;
		}
	    if ((ep = index (dp = ep, '+')) == NULL)
		ep = dp + strlen (dp);
	    else
		*ep++ = NULL;

	    (void) strcpy (ap, dp);

	    if (*ep) {
		SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
		      ("invalid MACRO for community \"%s\": %s", name, cp));
		return NOTOK;
	    }
	    break;
    }

out: ;
    ap = addr, np = ts -> ts_prefix;
    while (*ap) {
	*np = (*ap++ - '0') << 4;
	if (*ap)
	    *np++ |= (*ap++ - '0') & 0x0f;
	else
	    *np++ |= 0x0f;
    }
    switch (*ep) {
	case 'x':
	    IMPLODE (j, np, ep + 1, strlen (ep + 1), NOTOK, L6);
	    np += j;
	    break;

	case 'l':
	    (void) strcpy (np, ep + 1);
	    np += strlen (ep + 1);
	    break;

       default:
	    break;
    }
    ts -> ts_length = np - ts -> ts_prefix;

    return OK;
}

/*    PADDR2STR */

static char   *SEL2STR (sel, len)
char   *sel;
int	len;
{
    register char  *cp,
		   *dp,
		   *ep;
    static char buffer[PSSIZE * 2 + 4];

    if (len <= 0)
	return "";

    cp = buffer;
    *cp++ = '"';
    for (ep = (dp = sel) + len; dp < ep; dp++) {
	switch (*dp) {
	    case '+':
	    case '-':
	    case '.':
	        break;

	    default:
		if (!isalnum ((u_char) *dp)) {
		    cp = buffer;
		    *cp++ = '\'';
		    cp += explode (cp, (u_char *) sel, len);
		    (void) strcpy (cp, "'H");
		    return buffer;
		}		
		break;
	}

	*cp++ = *dp;
    }
    *cp++ = '"';
    *cp = NULL;

    return buffer;
}

/*  */

char    *_paddr2str (pa, na, compact)
register struct PSAPaddr *pa;
register struct NSAPaddr *na;
int	compact;
{
    register int   n;
    int	    first;
    register char *bp,
		  *cp,
		  *dp;
    register struct macro *m;
    register struct SSAPaddr *sa;
    register struct TSAPaddr *ta;
    register struct NSAPaddr *ca;
    static int    i = 0;
    static char buf1[BUFSIZ],
		buf2[BUFSIZ];
    static char *bufs[] = { buf1, buf2 };

    bp = cp = bufs[i++];
    i = i % 2;

    if (pa == NULLPA) {
bad_pa: ;
	(void) strcpy (bp, "NULLPA");
	return bp;
    }
    sa = &pa -> pa_addr;
    ta = &sa -> sa_addr;

    if (na)
	n = 1;
    else
	if ((n = ta -> ta_naddr) > 0)
	    na = ta -> ta_addrs;

    if (pa -> pa_selectlen > 0) {
	(void) sprintf (cp, "%s/",
			SEL2STR (pa -> pa_selector, pa -> pa_selectlen));
	cp += strlen (cp);
    }
    if (sa -> sa_selectlen > 0 || bp != cp) {
	(void) sprintf (cp, "%s/",
			SEL2STR (sa -> sa_selector, sa -> sa_selectlen));
	cp += strlen (cp);
    }
    if (ta -> ta_selectlen > 0 || bp != cp) {
	(void) sprintf (cp, "%s/",
			SEL2STR (ta -> ta_selector, ta -> ta_selectlen));
	cp += strlen (cp);
    }

    for (first = 1; n > 0; na++, n--) {
	register struct ts_interim *ts;

	if (first)
	    first = 0;
	else
	    *cp++ = '|';

	if (compact > 0) {
	    if ((ca = na2norm (na)) == NULLNA)
		goto bad_pa;

	    (void) strcpy (cp, "NS+");
	    cp += strlen (cp);

	    cp += explode (cp, (u_char *) ca -> na_address, ca -> na_addrlen);
	    *cp = NULL;
	    continue;
	}
	
	for (ts = ts_interim; ts -> ts_name; ts++)
	    if (ts -> ts_subnet == na -> na_community)
		break;
	if (!ts -> ts_name) {
	    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
		  ("unable to find community #%d", na -> na_community));
	    goto bad_pa;
	}
	if (!ts -> ts_value) {
	    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
		  ("community \"%s\" (subnet #%d) has no corresponding MACRO",
		   ts -> ts_name, na -> na_community));
	    goto bad_pa;
	}
	(void) strcpy (dp = cp, ts -> ts_value);
	cp += strlen (cp) - 1;
	if (*cp != '+')
	    *++cp = '+', *++cp = NULL;
	else
	    cp++;

	switch (na -> na_stack) {
	    case NA_NSAP:
		/* TBD Use afi_info to pretty print as AFI+IDI+DSP */
	        (void) strcpy (cp = dp, "NS+");
		cp += strlen (cp);

		cp += explode (cp, (u_char *) na -> na_address, na -> na_addrlen);
		*cp = NULL;
		break;

	    case NA_TCP:
		(void) strcpy (cp, na -> na_domain);
		cp += strlen (cp);
		if (na -> na_port) {
		    (void) sprintf (cp, "+%d", (int) ntohs (na -> na_port));
		    cp += strlen (cp);

		    if (na -> na_tset) {
			(void) sprintf (cp, "+%d", (int) na -> na_tset);
			cp += strlen (cp);
		    }
		}
		break;

	    case NA_X25:
	    case NA_BRG:
		if (na -> na_community == SUBNET_INT_X25
		        && na -> na_cudflen == 0
		        && na -> na_pidlen == 0
		        && na -> na_dte[0] != '0') {
		    (void) sprintf (cp = dp, "X121+%s", na -> na_dte);
		    cp += strlen (cp);
		}
		else {
		    (void) strcpy (cp, na -> na_dte);
		    cp += strlen (cp);
		    if (na -> na_pidlen > 0) {
		        (void) strcpy (cp, "+PID+");
		        cp += strlen (cp);

		        cp += explode (cp, (u_char *) na -> na_pid,
				       (int) na -> na_pidlen);
		    }
		    else
		        if (na -> na_cudflen > 0) {
			    (void) strcpy (cp, "+CUDF+");
			    cp += strlen (cp);

			    cp += explode (cp, (u_char *) na -> na_cudf,
					   (int) na -> na_cudflen);
		        }
		    *cp = NULL;
		}
		break;

	    default:
		SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
		      ("unknown address type 0x%x", na -> na_stack));
		goto bad_pa;
	}

	SLOG (addr_log, LLOG_DEBUG, NULLCP, ("dp = %s",dp));

	if (!compact && (m = value2macro (dp))) {
	    char    buffer[BUFSIZ];

	    (void) sprintf (buffer, "%s=%s", m -> m_name,
			    dp + strlen (m -> m_value));
	    (void) strcpy (dp, buffer);
	    cp = dp + strlen (dp);
	}
    }
    *cp = NULL;

    return bp;
}

/*  */

#ifdef PEP_TEST
free_macros () {
    register int    i;
    register struct macro *m,
			  *p;

    for (i = MBUCKETS; i-- > 0; )
	for (p = Mbuckets[i]; p; p = m) {
	    m = p -> m_chain;

	    if (p -> m_name)
		free (p -> m_name);
	    if (p -> m_value)
		free (p -> m_value);

	    free (p);
	}
}
#endif
