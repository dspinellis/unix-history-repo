/* ap.c - Quipu access point syntax  */

#ifndef	lint
static char *rcsid = "$Header$";
#endif

/* 
 * $Header$
 *
 *
 * $Log$
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

#include "quipu/util.h"
#include "quipu/attr.h"
#include "psap.h"
#include "isoaddrs.h"
#include "../x500as/DO-types.h"

extern LLog * log_dsap;
extern char * index ();
extern struct PSAPaddr * psap_cpy ();
extern aps_free();

struct access_point * qap_cpy (a)
struct access_point * a;
{
struct access_point * r;

	r = (struct access_point *) smalloc (sizeof (struct access_point));
	bzero ((char *) r,sizeof (struct access_point));

	r -> ap_name = dn_cpy (a -> ap_name);
	if (a -> ap_address) 
		r -> ap_address = psap_cpy ( a -> ap_address );

	return (r);
}

static qap_cmp (r,a)
struct access_point *r, *a;
{
int res;

	if (( res = dn_cmp (r -> ap_name, a -> ap_name)) == 0) 
		if ( r -> ap_address && a -> ap_address )
		       return (bcmp ((char *) r -> ap_address,
				     (char *) a -> ap_address, 
				     sizeof *a -> ap_address) ? (-1) : 0);
		else 
			return ( r -> ap_address == a -> ap_address ? 0 :
				 r -> ap_address > a -> ap_address ? 1 : (-1));

	return res;
}
		

static PE qap_enc (p)
struct access_point *p;
{
PE ret_pe;

	if (encode_DO_QAccessPoint (&ret_pe,0,0,NULLCP,p) == NOTOK )
		return NULLPE;

	return (ret_pe);
}

static struct access_point * qap_dec (pe)
PE pe;
{
struct access_point *qap;

	if (decode_DO_QAccessPoint (pe,1,NULLIP,NULLVP,&qap) == NOTOK) {
		return (NULLACCESSPOINT);
	}
		
	return (qap);
}

static struct access_point * qap_parse (s)
char * s;
{
struct PSAPaddr *pa;
struct access_point *qap;
char * p;

	qap = (struct access_point *) calloc (1,sizeof (struct access_point));

	if ((p = index (s,'#')) != NULLCP) {
		*p++ = 0;
		if (pa=str2paddr(SkipSpace(p))) {
			qap->ap_address = (struct PSAPaddr *) calloc (1,sizeof (struct PSAPaddr));
			*qap->ap_address = *pa;  /* struct copy */
		} else {
			parse_error ("invalid presentation address in access point %s",p);
			free ((char *)qap);
			*(--p) = '#';
			return (NULLACCESSPOINT);
		}
		
	}

	if ((qap -> ap_name = str2dn (s)) == NULLDN) {
		if (qap->ap_address)
			free ((char *)qap->ap_address);
		free ((char *)qap);
		return NULLACCESSPOINT;
	}

	if (p)
		*--p = '#';

	return qap;
}

static qap_print (ps,p,format)
PS ps;
struct access_point *p;
int format;
{

	dn_print (ps, p -> ap_name, format);

	if ( p -> ap_address )
		if (format != READOUT)
			ps_printf (ps, " # %s", _paddr2str(p->ap_address,NULLNA,-1));
		else
			ps_printf (ps, " # %s", paddr2str(p->ap_address,NULLNA));

}

ap_syntax ()
{
	(void) add_attribute_syntax ("AccessPoint",
		(IFP) qap_enc,		(IFP) qap_dec,
		(IFP) qap_parse,	qap_print,
		(IFP) qap_cpy,		qap_cmp,
		aps_free,		NULLCP,
		NULLIFP,		TRUE );
}
