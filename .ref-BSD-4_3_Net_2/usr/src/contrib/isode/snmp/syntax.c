/* syntax.c - SMI syntax handling */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/snmp/RCS/syntax.c,v 7.19 91/02/22 09:44:38 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/snmp/RCS/syntax.c,v 7.19 91/02/22 09:44:38 mrose Interim $
 *
 * Contributed by NYSERNet Inc.  This work was partially supported by the
 * U.S. Defense Advanced Research Projects Agency and the Rome Air Development
 * Center of the U.S. Air Force Systems Command under contract number
 * F30602-88-C-0016.
 *
 *
 * $Log:	syntax.c,v $
 * Revision 7.19  91/02/22  09:44:38  mrose
 * Interim 6.8
 * 
 * Revision 7.18  91/01/13  11:05:45  mrose
 * update
 * 
 * Revision 7.17  91/01/12  21:22:58  mrose
 * update
 * 
 * Revision 7.16  91/01/11  15:35:36  mrose
 * sets
 * 
 * Revision 7.15  90/12/18  10:14:07  mrose
 * update
 * 
 * Revision 7.14  90/11/20  15:32:55  mrose
 * update
 * 
 * Revision 7.13  90/11/04  19:16:36  mrose
 * update
 * 
 * Revision 7.12  90/10/29  18:39:12  mrose
 * updates
 * 
 * Revision 7.11  90/10/23  20:37:17  mrose
 * update
 * 
 * Revision 7.10  90/08/18  00:44:42  mrose
 * touch-up
 * 
 * Revision 7.9  90/08/08  14:01:39  mrose
 * stuff
 * 
 * Revision 7.8  90/07/09  14:49:41  mrose
 * sync
 * 
 * Revision 7.7  90/04/18  08:52:03  mrose
 * oid_normalize
 * 
 * Revision 7.6  90/04/08  03:23:20  mrose
 * touch-up
 * 
 * Revision 7.5  90/03/23  17:27:35  mrose
 * update
 * 
 * Revision 7.4  90/03/08  08:05:25  mrose
 * touch-up
 * 
 * Revision 7.3  90/02/19  19:23:14  mrose
 * again
 * 
 * Revision 7.2  90/02/19  19:17:11  mrose
 * again
 * 
 * Revision 7.1  90/01/11  18:34:48  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  22:23:32  mrose
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
#include "SNMP-types.h"
#include "objects.h"
#include "tailor.h"

#include "internet.h"
#include "clns.h"

/*    DATA */

#define	MAXSYN	50

static object_syntax syntaxes[MAXSYN + 1];
static OS    synlast = syntaxes;

/*    INTEGER */

static int  integer_encode (x, pe)
integer	*x;
PE     *pe;
{
    if ((*pe = int2prim (*x)) == NULLPE)
	return NOTOK;

    return OK;
}


static int  integer_decode (x, pe)
integer **x;
PE	pe;
{
    integer	i = prim2num (pe);

    if (i == NOTOK && pe -> pe_errno != PE_ERR_NONE)
	return NOTOK;
    if ((*x = (integer *) malloc (sizeof **x)) == NULL)
	return NOTOK;
    **x = i;

    return OK;
}


static int  integer_free (x)
integer *x;
{
    free ((char *) x);
}


static int  integer_parse (x, s)
integer **x;
char   *s;
{
    long    l;

    if (sscanf (s, "%ld", &l) != 1)
	return NOTOK;
    if ((*x = (integer *) malloc (sizeof **x)) == NULL)
	return NOTOK;
    **x = (integer) l;

    return OK;
}


/* ARGSUSED */

static int  integer_print (x, os)
integer *x;
OS	os;
{
    printf ("%d", *x);
}


/* ARGSUSED */

static int  services_print (x, os)
integer *x;
OS	os;
{
    printf ("%s", sprintb ((int) *x,
			   "\020\01physical\02datalink/subnetwork\03internet\04transport\05session\06presentation\07application"));
}


/* ARGSUSED */

static int  privs_print (x, os)
integer *x;
OS	os;
{
    printf ("%s", sprintb ((int) *x,
			   "\020\01get\02get-next\03get-response\04set\05trap"));
}


static	add_integer ()
{
    (void) add_syntax ("INTEGER", integer_encode, integer_decode, integer_free,
		integer_parse, integer_print);
    (void) add_syntax ("Services", integer_encode, integer_decode,
		       integer_free, integer_parse, services_print);
    (void) add_syntax ("Privileges", integer_encode, integer_decode,
		       integer_free, integer_parse, privs_print);
}

/*    OCTET STRING */

static int  string_encode (x, pe)
struct qbuf *x;
PE     *pe;
{
    if ((*pe = qb2prim_aux (x, PE_CLASS_UNIV, PE_PRIM_OCTS, 0)) == NULLPE)
	return NOTOK;

    return OK;
}


static int  string_decode (x, pe)
struct qbuf **x;
PE	pe;
{
    struct qbuf *qb = prim2qb (pe);

    if (qb == NULL)
	return NOTOK;
    *x = qb;

    return OK;
}


static int  string_parse (x, s)
struct qbuf **x;
char   *s;
{
    struct qbuf *qb;

    if (strncmp (s, "0x", 2) == 0) {
	int	len;
	char   *p;

	s += 2;
	if ((len = strlen (s)) % 3 != 2)
	    return NOTOK;
	len /= 3, len++;
	if ((qb = str2qb (NULLCP, len, 1)) == NULL)
	    return NOTOK;
	p = qb -> qb_forw -> qb_data;
	while (*s) {
	    int	    i;

	    if (sscanf (s, "%x", &i) != 1) {
oops: ;
		qb_free (qb);
		return NOTOK;
	    }
	    *p++ = i & 0xff;

	    s += 2;
	    if (*s && *s++ != ':')
		goto oops;
	}
    }
    else
	if ((qb = str2qb (s, strlen (s), 1)) == NULL)
	    return NOTOK;

    *x = qb;

    return OK;    
}


/* ARGSUSED */

static int  string_print (x, os)
struct qbuf *x;
OS	os;
{
    register char *cp,
		  *ep;
    char   *p;
    register struct qbuf *qb;

    p = "0x";
    for (qb = x -> qb_forw; qb != x; qb = qb -> qb_forw)
	for (ep = (cp = qb -> qb_data) + qb -> qb_len; cp < ep; cp++) {
	    printf ("%s%02x", p, *cp & 0xff);
	    p = ":";
	}
}


/* ARGSUSED */

static int  string_display (x, os)
struct qbuf *x;
OS	os;
{
    register struct qbuf *qb;

    printf ("\"");
    for (qb = x -> qb_forw; qb != x; qb = qb -> qb_forw)
	printf ("%*.*s", qb -> qb_len, qb -> qb_len, qb -> qb_data);
    printf ("\"");
}


static	add_string ()
{
    (void) add_syntax ("OctetString", string_encode, string_decode, qb_free,
		string_parse, string_print);
    (void) add_syntax ("DisplayString", string_encode, string_decode, qb_free,
		string_parse, string_display);
    (void) add_syntax ("PhysAddress", string_encode, string_decode, qb_free,
		string_parse, string_print);
}

/*    OBJECT IDENTIFIER */

static int  object_encode (x, pe)
OID	x;
PE     *pe;
{
    if ((*pe = oid2prim (x)) == NULLPE)
	return NOTOK;

    return OK;
}


static int  object_decode (x, pe)
OID    *x;
PE	pe;
{
    OID    oid = prim2oid (pe);

    if (oid == NULLOID || (*x = oid_cpy (oid)) == NULLOID)
	return NOTOK;

    return OK;
}


static int  object_parse (x, s)
OID   *x;
char   *s;
{
    OID	    oid = text2oid (s);

    if (oid == NULL)
	return NOTOK;
    *x = oid;

    return OK;    
}


/* ARGSUSED */

static int  object_print (x, os)
OID	x;
OS	os;
{
    char  *cp,
	   ode[BUFSIZ];

    (void) strcpy (ode, oid2ode (x));
    printf ("%s", ode);
    if (strcmp (ode, cp = sprintoid (x)))
	printf (" (%s)", cp);
}


static	add_object ()
{
    (void) add_syntax ("ObjectID", object_encode, object_decode, oid_free,
		object_parse, object_print);
}

/*    NULL */

/* ARGSUSED */

static int  null_encode (x, pe)
char   *x;
PE     *pe;
{
    if ((*pe = pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_NULL)) == NULLPE)
	return NOTOK;

    return OK;
}


/* ARGSUSED */

static int  null_decode (x, pe)
char  **x;
PE	pe;
{
    if ((*x = (char *) calloc (1, sizeof **x)) == NULL)
	return NOTOK;

    return OK;
}


static int  null_free (x)
char *x;
{
    free ((char *) x);
}


static int  null_parse (x, s)
char  **x;
char   *s;
{
    if (lexequ (s, "NULL"))
	return NOTOK;

    if ((*x = (char *) calloc (1, sizeof **x)) == NULL)
	return NOTOK;

    return OK;    
}


/* ARGSUSED */

static int  null_print (x, os)
char   *x;
OS	os;
{
    printf ("NULL");
}


static	add_null ()
{
    (void) add_syntax ("NULL", null_encode, null_decode, null_free, null_parse,
		null_print);
}

/*    IpAddress */

static int  ipaddr_encode (x, pe)
struct sockaddr_in *x;
PE     *pe;
{
    if ((*pe = str2prim ((char *) &x -> sin_addr, 4, PE_CLASS_APPL, 0))
	    == NULLPE)
	return NOTOK;

    return OK;
}


static int  ipaddr_decode (x, pe)
struct sockaddr_in **x;
PE	pe;
{
    struct type_SNMP_IpAddress *ip;
    struct qbuf *qb;
    struct sockaddr_in *isock;

    if (decode_SNMP_IpAddress (pe, 1, NULLIP, NULLVP, &ip) == NOTOK)
	return NOTOK;
    if (qb_pullup (ip) == NOTOK
	    || (isock = (struct sockaddr_in *) calloc (1, sizeof *isock))
		    == NULL) {
	free_SNMP_IpAddress (ip);
	return NOTOK;
    }
    if ((qb = ip -> qb_forw) -> qb_len != 4) {
	free ((char *) isock);
	free_SNMP_IpAddress (ip);
	return NOTOK;
    }
    isock -> sin_family = AF_INET;
    bcopy (qb -> qb_data,
	   (char *) &isock -> sin_addr,
	   sizeof isock -> sin_addr);

    *x = isock;

    free_SNMP_IpAddress (ip);
    return OK;
}


static int  ipaddr_free (x)
struct sockaddr_in *x;
{
    free ((char *) x);
}


static int  ipaddr_parse (x, s)
struct sockaddr_in **x;
char   *s;
{
    register struct hostent *hp = gethostbystring (s);
    register struct sockaddr_in *isock;

    if (hp == NULL)
	return NOTOK;

    if ((isock = (struct sockaddr_in *) calloc (1, sizeof *isock)) == NULL)
	return NOTOK;
    isock -> sin_family = AF_INET;
    inaddr_copy (hp, isock);
    *x = isock;

    return OK;    
}


/* ARGSUSED */

static int  ipaddr_print (x, os)
struct sockaddr_in *x;
OS	os;
{
    printf ("%s", inet_ntoa (x -> sin_addr));
}


static	add_ipaddr ()
{
    (void) add_syntax ("IpAddress", ipaddr_encode, ipaddr_decode, ipaddr_free,
		ipaddr_parse, ipaddr_print);
}

/*    NetworkAddress */

/* good enough for now (and probably forever)... */

static	add_netaddr ()
{
    (void) add_syntax ("NetworkAddress", ipaddr_encode, ipaddr_decode,
		       ipaddr_free, ipaddr_parse, ipaddr_print);
}

/*    UNSIGNED LONGs */

u_long	prim2ulong (pe)		/* also used in SNMP-capable gawk... */
register PE	pe;
{
    register u_long   i;
    register PElementData dp,
			  ep;

    if (pe -> pe_form != PE_FORM_PRIM || (dp = pe -> pe_prim) == NULLPED)
	return pe_seterr (pe, PE_ERR_PRIM, 0);
    if (pe -> pe_len > sizeof (i) + 1)
	return pe_seterr (pe, PE_ERR_OVER, 0);
    if (pe -> pe_len == sizeof (i) + 1 && (*dp & 0x7f))
	return pe_seterr (pe, PE_ERR_OVER, 0);
    if (*dp & 0x80)
	return pe_seterr (pe, PE_ERR_SIGNED, 0);

    pe -> pe_errno = PE_ERR_NONE;	/* in case result is ZERO-valued */
    i = 0L;
    for (ep = dp + pe -> pe_len; dp < ep;)
	i = (i << 8) | (*dp++ & 0xff);

    return i;
}


static PE  ulong2prim (i, class, id)
register u_long i;
PElementClass	class;
PElementID	id;
{
    int	    extend;
    register int    n;
    register u_long mask;
    register PElementData dp;
    register PE	    pe;

    if ((pe = pe_alloc (class, PE_FORM_PRIM, id)) == NULLPE)
	return NULLPE;

    mask = 0xff << (((n = sizeof i) - 1) * 8);
    while (n > 1 && (i & mask) == 0)
	mask >>= 8, n--;
    extend = (i & (0x80 << ((n - 1) * 8))) ? 1 : 0;

    if ((pe -> pe_prim = PEDalloc (n + extend)) == NULLPED) {
	pe_free (pe);
	return NULLPE;
    }

    for (dp = pe -> pe_prim + (pe -> pe_len = n + extend); n-- > 0; i >>= 8)
	*--dp = i & 0xff;
    if (extend)
	*--dp = 0x00;

    return pe;
}

/*    Counter */

static int  counter_encode (x, pe)
u_long	*x;
PE     *pe;
{
    if ((*pe = ulong2prim (*x, PE_CLASS_APPL, 1)) == NULLPE)
	return NOTOK;

    return OK;
}


static int  counter_decode (x, pe)
u_long **x;
PE	pe;
{
    u_long	i = prim2ulong (pe);

    if (i == 0 && pe -> pe_errno != PE_ERR_NONE)
	return NOTOK;
    if ((*x = (u_long *) malloc (sizeof **x)) == NULL)
	return NOTOK;
    **x = i;

    return OK;
}


static int  counter_free (x)
u_long *x;
{
    free ((char *) x);
}


static int  counter_parse (x, s)
u_long **x;
char   *s;
{
    u_long  i;

    if (sscanf (s, "%U", &i) != 1)
	return NOTOK;
    if ((*x = (u_long *) malloc (sizeof **x)) == NULL)
	return NOTOK;
    **x = i;

    return OK;    
}


/* ARGSUSED */

static int  counter_print (x, os)
u_long *x;
OS	os;
{
    printf ("%U", *x);
}


static	add_counter ()
{
    (void) add_syntax ("Counter", counter_encode, counter_decode, counter_free,
		counter_parse, counter_print);
}

/*    Gauge */

static int  gauge_encode (x, pe)
u_long	*x;
PE     *pe;
{
    if ((*pe = ulong2prim (*x, PE_CLASS_APPL, 2)) == NULLPE)
	return NOTOK;

    return OK;
}


static	add_gauge ()
{
    (void) add_syntax ("Gauge", gauge_encode, counter_decode, counter_free,
		counter_parse, counter_print);
}

/*    TimeTicks */

static int  timeticks_encode (x, pe)
u_long	*x;
PE     *pe;
{
    if ((*pe = ulong2prim (*x, PE_CLASS_APPL, 3)) == NULLPE)
	return NOTOK;

    return OK;
}


/* ARGSUSED */

static int  timeticks_print (x, os)
u_long *x;
OS	os;
{
    u_long  d,
	    h,
	    m,
	    s,
	    ds;

    ds = *x;
    s = ds / 100, ds = ds % 100;
    m = s / 60, s = s % 60;
    h = m / 60, m = m % 60;
    d = h / 24, h = h % 24;

    if (d > 0)
	printf ("%d days, ", d);
    if (d > 0 || h > 0)
	printf ("%d hours, ", h);
    if (d > 0 || h > 0 || m > 0)
	printf ("%d minutes, ", m);
    printf ("%d", s);
    if (ds > 0)
	printf (".%02d", ds);
    printf (" seconds (%U timeticks)", *x);
}


static	add_timeticks ()
{
    (void) add_syntax ("TimeTicks", timeticks_encode, counter_decode,
		       counter_free, counter_parse, timeticks_print);
}

/*    CnlpAddress */

static int  clnpaddr_encode (x, pe)
struct sockaddr_iso *x;
PE     *pe;
{
    char    buffer[sizeof x -> siso_data + 1];

    buffer[0] = x -> siso_nlen & 0xff;
    bcopy (x -> siso_data, buffer + 1, (int) x -> siso_nlen);

    if ((*pe = str2prim (buffer, (int) (x -> siso_nlen + 1), PE_CLASS_APPL,
			 5)) == NULLPE)
	return NOTOK;

    return OK;
}


static int  clnpaddr_decode (x, pe)
struct sockaddr_iso **x;
PE	pe;
{
    int	    len;
    struct type_SNMP_ClnpAddress *clnp;
    struct qbuf *qb;
    struct sockaddr_iso *isock;

    if (decode_SNMP_ClnpAddress (pe, 1, NULLIP, NULLVP, &clnp) == NOTOK)
	return NOTOK;
    if (qb_pullup (clnp) == NOTOK
	    || (isock = (struct sockaddr_iso *) calloc (1, sizeof *isock))
		    == NULL) {
	free_SNMP_ClnpAddress (clnp);
	return NOTOK;
    }
    qb = clnp -> qb_forw;
    isock -> siso_family = AF_ISO;
    if ((len = qb -> qb_data[0] & 0xff) >= qb -> qb_len)
	len = qb -> qb_len - 1;
    bcopy (qb -> qb_data + 1, isock -> siso_data,
	   (int) (isock -> siso_nlen = len));

    *x = isock;

    free_SNMP_ClnpAddress (clnp);
    return OK;
}


static int  clnpaddr_free (x)
struct sockaddr_iso *x;
{
    free ((char *) x);
}


static int  clnpaddr_parse (x, s)
struct sockaddr_iso **x;
char   *s;
{
    register struct sockaddr_iso *isock;

    if ((isock = (struct sockaddr_iso *) calloc (1, sizeof *isock)) == NULL)
	return NOTOK;
    isock -> siso_family = AF_ISO;
    isock -> siso_nlen = implode ((u_char *) isock -> siso_data, s,
				  strlen (s));
    *x = isock;

    return OK;    
}


/* ARGSUSED */

static int  clnpaddr_print (x, os)
struct sockaddr_iso *x;
OS	os;
{
    char    buffer[sizeof x -> siso_data * 2 + 1];

    buffer[explode (buffer, (u_char *) x -> siso_data, (int) x -> siso_nlen)] =
	NULL;
    printf ("NS+%s", buffer);
}


static	add_clnpaddr ()
{
    (void) add_syntax ("ClnpAddress", clnpaddr_encode, clnpaddr_decode,
		       clnpaddr_free, clnpaddr_parse, clnpaddr_print);
}

/*  */

int	readsyntax ()
{
    add_integer ();
    add_string ();
    add_object ();
    add_null ();
    add_ipaddr ();
    add_netaddr ();
    add_counter ();
    add_gauge ();
    add_timeticks ();

    add_clnpaddr ();
}

/*  */

int	add_syntax (name, f_encode, f_decode, f_free, f_parse, f_print)
char   *name;
IFP	f_encode,
    	f_decode,
    	f_free,
    	f_parse,
	f_print;
{
    int	    i;
    register OS	    os = synlast++;

    if ((i = synlast - syntaxes) >= MAXSYN)
	return NOTOK;

    bzero ((char *) os, sizeof *os);
    os -> os_name = name;
    os -> os_encode = f_encode;
    os -> os_decode = f_decode;
    os -> os_free = f_free;
    os -> os_parse = f_parse;
    os -> os_print = f_print;

    return i;
}

/*  */

OS	text2syn (name)
char   *name;
{
    register OS	    os;

    for (os = syntaxes; os < synlast; os++)
	if (strcmp (os -> os_name, name) == 0)
	    return os;

    return NULLOS;
}
