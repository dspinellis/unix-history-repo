/* dn_seq.c - General Directory Name Sequence routines */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/dn_seq.c,v 7.2 91/02/22 09:19:01 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/dn_seq.c,v 7.2 91/02/22 09:19:01 mrose Interim $
 *
 *
 * $Log:	dn_seq.c,v $
 * Revision 7.2  91/02/22  09:19:01  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:41:40  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:42:06  mrose
 * Release 6.0
 * 
 */

/*
 *                                NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

#include "quipu/util.h"
#include "quipu/entry.h"

dn_seq_free (dnseq)
struct dn_seq * dnseq;
{
struct dn_seq * ptr;
struct dn_seq * next;

	for (ptr=dnseq ; ptr!=NULLDNSEQ; ptr=next ) {
		next = ptr->dns_next;
		dn_free (ptr->dns_dn);
		free ((char *) ptr);
	}

}

struct dn_seq * dn_seq_cpy (dnseq)
struct dn_seq * dnseq;
{
register struct dn_seq * ptr;
struct dn_seq * ptr2;
struct dn_seq * result = NULLDNSEQ;


	for (ptr=dnseq ; ptr!=NULLDNSEQ; ptr=ptr->dns_next ) {
		ptr2 = dn_seq_alloc();
		ptr2 -> dns_next = result;
		result = ptr2;
		result->dns_dn = dn_cpy (ptr->dns_dn);
	}
	return (result);
}


check_dnseq (dnseq,who)
struct dn_seq * dnseq;
DN who;
{
register struct dn_seq * ptr;

	for (ptr=dnseq; ptr!=NULLDNSEQ; ptr=ptr->dns_next) {
		if (dn_cmp (who,ptr->dns_dn) == OK)
			return (OK);
	}

	return (NOTOK);
}

dn_seq_cmp (a,b)
struct dn_seq * a, * b;
{
    struct dn_seq	* dns1;
    struct dn_seq	* dns2;

    if((a == NULLDNSEQ) && (b == NULLDNSEQ))
	return(0);

    if(a==NULLDNSEQ)
	return(-1);

    if(b==NULLDNSEQ)
	return(1);

    for (dns1=a; dns1!=NULLDNSEQ; dns1=dns1->dns_next)
    {
	for (dns2=b; dns2!=NULLDNSEQ; dns2=dns2->dns_next)
	{
	    if(dn_cmp (dns1->dns_dn,dns2->dns_dn) == 0)
		break;
	}
	if(dns2 == NULLDNSEQ)
	    return(1);
    }

    for (dns2=b; dns2!=NULLDNSEQ; dns2=dns2->dns_next)
    {
	for (dns1=a; dns1!=NULLDNSEQ; dns1=dns1->dns_next)
	{
	    if(dn_cmp (dns1->dns_dn,dns2->dns_dn) == 0)
		break;
	}
	if(dns1 == NULLDNSEQ)
	    return(-1);
    }

    return (0);
}

check_dnseq_prefix (dnseq,who)
struct dn_seq * dnseq;
DN who;
{
struct dn_seq * ptr;

	for (ptr=dnseq; ptr!=NULLDNSEQ; ptr=ptr->dns_next) {
		if (dn_cmp_prefix (ptr->dns_dn,who) == OK)
			return (OK);
	}

	return (NOTOK);
}

dn_seq_print (ps,dnseq,format)
PS ps;
struct dn_seq * dnseq;
int format;
{
        if (dnseq == NULLDNSEQ) 
		return;

	dn_print (ps,dnseq->dns_dn,EDBOUT);
	for (dnseq=dnseq->dns_next; dnseq!=NULLDNSEQ;  dnseq=dnseq->dns_next) {
		if (format == READOUT)
			ps_print (ps," AND ");
		else
			ps_print (ps,"$");
		dn_print (ps,dnseq->dns_dn,EDBOUT);
	}
}

struct dn_seq * str2dnseq (str)
register char * str;
{
register char *ptr;
register char *save,val;
struct dn_seq * dns = NULLDNSEQ;
struct dn_seq * newdns;

	while ( (ptr = index (str,'$')) != 0) {
		save = ptr++;
		save--;
		if (! isspace (*save))
			save++;
		val = *save;
		*save = 0;
		newdns = dn_seq_alloc();
		if ((newdns->dns_dn = str2dn (str)) == NULLDN) {
			dn_seq_free (dns);
			free ((char *) newdns);
			return (NULLDNSEQ);
		}
		newdns->dns_next = dns;
		dns = newdns;
		*save = val;
		str = ptr;
	}

	if ((ptr = rindex (str,'#')) != 0) {
		/* a bit or reverse compatability... */
		if (*++ptr != 0) {
			parse_error ("invalid # in sequence '%s'",str);
			dn_seq_free (dns);
			return (NULLDNSEQ);
		}
		else
			*--ptr = 0;
		}

	newdns = dn_seq_alloc();
	if ((newdns->dns_dn = str2dn (str)) == NULLDN) {
		dn_seq_free (dns);
		free ((char *) newdns);
		return (NULLDNSEQ);
	}
	newdns->dns_next = dns;
	dns = newdns;

	return (dns);
}

int	  dn_in_dnseq(dn, dnseq)
DN		  dn;
struct dn_seq	* dnseq;
{
    struct dn_seq	* ptr;
    register int 	i = 1;

    for(ptr=dnseq; ptr!=NULLDNSEQ; ptr=ptr->dns_next, i++)
    {
	if(dn_cmp(dn, ptr->dns_dn) == 0)
	    break;
    }

    if(ptr == NULLDNSEQ)
	return(FALSE);

    return(i);
}

struct dn_seq	* dn_seq_push(dn,dnseq)
DN		  dn;
struct dn_seq	* dnseq;
{
    struct dn_seq * ret;

    ret = dn_seq_alloc();

    ret->dns_dn = dn_cpy(dn);
    ret->dns_next = dnseq;

    return(ret);
}

struct dn_seq	* dn_seq_pop(dnseq)
struct dn_seq	* dnseq;
{
    struct dn_seq * ret;

    if(dnseq == NULLDNSEQ)
	return(NULLDNSEQ);

    ret = dnseq->dns_next;

    dn_free(dnseq->dns_dn);
    free((char *)dnseq);

    return(ret);
}
