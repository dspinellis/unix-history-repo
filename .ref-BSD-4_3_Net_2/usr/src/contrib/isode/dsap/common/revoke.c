/* revoke.c - Certificate List attribute syntax */

/* This syntax is still at the testing stage. Accordingly, quipu should
 * not load this syntax (just in case).
 */


#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/revoke.c,v 7.4 91/02/22 09:20:11 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/revoke.c,v 7.4 91/02/22 09:20:11 mrose Interim $
 *
 *
 * $Log:	revoke.c,v $
 * Revision 7.4  91/02/22  09:20:11  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/10/17  11:42:46  mrose
 * sync
 * 
 * Revision 7.2  90/01/11  18:35:36  mrose
 * real-sync
 * 
 * Revision 7.1  89/12/19  16:19:31  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:47:45  mrose
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


#include <stdio.h>

#include "quipu/util.h"
#include "quipu/entry.h"
#include "quipu/bind.h"
#include "quipu/syntaxes.h"

PE enc_revoke(parm)
struct revocation_list *parm;
{
PE pe;
	encode_AF_CertificateList(&pe, 0, 0, NULLCP, parm);
	return (pe);
}

struct revocation_list *dec_revoke(pe)
PE pe;
{
struct revocation_list *result;

	if (decode_AF_CertificateList(pe, 0, NULLIP, NULLVP, &result) == NOTOK)
		return (struct revocation_list *) 0;
	return (result);
}

struct revocation_list *str2revoke(str)
char *str;
{
struct revocation_list *result;
struct revoked_certificate *cert;
struct revoked_certificate **last;
OID oid;
char *ptr;

	result = (struct revocation_list *) calloc(1, sizeof(*result));
 	if (result == (struct revocation_list *) 0)
		return (result);

	ptr = index(str, '#');
	if (ptr)
	{
		*ptr = '\0';
		ptr++;
	}
	else
		return (struct revocation_list *) 0;

	oid = name2oid(str);
	if (oid == NULLOID)
	{
		parse_error("Invalid OID %s", str);
		return (struct revocation_list *) 0;
	}
	result->sig.alg.algorithm = oid;

	str = ptr;
	ptr = index(str, '#');
	if (ptr)
	{
		*ptr = '\0';
		ptr++;
	}
	else
		return (struct revocation_list *) 0;

	str2alg(str, &(result->sig.alg));

	str=ptr;
	ptr = index(str, '#');
	if (ptr)
	{
		*ptr = '\0';
		ptr++;
	}

	str2encrypted(str, &(result->sig.encrypted), &(result->sig.n_bits));

	str=ptr;
	ptr = index(str, '#');
	if (ptr)
	{
		*ptr = '\0';
		ptr++;
	}

	result->issuer = str2dn(str);

	str=ptr;
	ptr = index(str, '#');
	if (ptr)
	{
		*ptr = '\0';
		ptr++;
	}
	else
		return (struct revocation_list *) 0;

	oid = name2oid(str);
	if (oid == NULLOID)
	{
		parse_error("Invalid OID %s", str);
		return (struct revocation_list *) 0;
	}
	result->alg.algorithm = oid;

	str=ptr;
	ptr = index(str, '#');
	if (ptr)
	{
		*ptr = '\0';
		ptr++;
	}
	else return (struct revocation_list *) 0;

	str2alg(str, &(result->alg));

	str=ptr;
	ptr = index(str, '#');
	if (ptr)
	{
		*ptr = '\0';
		ptr++;
	}
	/* This may be the end of the string */

	result->last_update = strdup(str);

	if ((str = ptr) == NULLCP)
		return (result);
	ptr = index(str, '#');
	if (ptr == NULLCP)
		return(result);

	*ptr = '\0';
	ptr++;

	oid = name2oid(str);
	result->sig2.alg.algorithm = oid;

	str=ptr;
	ptr = index(str, '#');
	if (ptr)
	{
		*ptr = '\0';
		ptr++;
	}
	else
		return (struct revocation_list *) 0;

	str2alg(str, &(result->sig2.alg));

	str=ptr;
	ptr = index(str, '#');
	if (ptr)
	{
		*ptr = '\0';
		ptr++;
	}
	/* This may be the end of the string */

	str2encrypted(str, &(result->sig2.encrypted), &(result->sig2.n_bits));

	last = (struct revoked_certificate **) &(result->revoked);

	while (str = ptr, ((ptr = index(str, '#')) != NULLCP))
	{
		*ptr = '\0';
		ptr++;

		cert = (struct revoked_certificate *) calloc(1, sizeof(*cert));
		if (cert == (struct revoked_certificate *) 0)
			return ((struct revocation_list *) 0);

		*last =  cert;
		last = &(cert->next);
		cert->next = (struct revoked_certificate *) 0;

		cert->subject = str2dn(str);
		if (cert->subject == NULLDN)
		{
			parse_error("Invalid DN %s", str);
			return ((struct revocation_list *) 0);
		}

		str=ptr;
		ptr = index(str, '#');
		if (ptr)
		{
			*ptr = '\0';
			ptr++;
		}
		else
			return (struct revocation_list *) 0;

		oid = name2oid(str);
		if (oid == NULLOID)
		{
			parse_error("Invalid OID %s", str);
			return (struct revocation_list *) 0;
		}

		cert->alg.algorithm = oid;

		str=ptr;
		ptr = index(str, '#');
		if (ptr)
		{
			*ptr = '\0';
			ptr++;
		}
		else
			return (struct revocation_list *) 0;

		str2alg(str, &(cert->alg));

		str=ptr;
		ptr = index(str, '#');
		if (ptr)
		{
			*ptr = '\0';
			ptr++;
		}
		else
			return (struct revocation_list *) 0;

		cert->serial = atoi(str);

		str=ptr;
		ptr = index(str, '#');
		if (ptr)
		{
			*ptr = '\0';
			ptr++;
		}
		/* may be the end of the string */

		cert->revocation_date = strdup(str);

	}

	return (result);
}

print_revoked(ps, parm, format)
PS ps;
struct revoked_certificate *parm;
int format;
{
struct revoked_certificate *tmp;

	tmp = parm;
	while (tmp)
	{
		dn_print(ps, tmp->subject, EDBOUT);
		ps_printf(ps, "#");
		print_algid(ps, &(tmp->alg), format);
		ps_printf(ps, "%d#", tmp->serial);
		ps_printf(ps, "%s#", tmp->revocation_date);
		tmp = tmp->next;
	}
}

print_revoke(ps, parm, format)
PS ps;
struct revocation_list *parm;
int format;
{
	print_algid(ps, &(parm->sig.alg), format);
	print_encrypted(ps, parm->sig.encrypted, parm->sig.n_bits, format);

	dn_print(ps, parm->issuer, EDBOUT);
	ps_printf(ps, "#");
	print_algid(ps, &(parm->alg), format);
	utcprint(ps, parm->last_update, format);
	ps_printf(ps, "#");
	if (parm->revoked)
	{
   		print_algid(ps, &(parm->sig2.alg), format);
   		print_encrypted(ps, parm->sig2.encrypted, 
			parm->sig2.n_bits, format);  
		print_revoked(ps, parm->revoked, format);
	}
	
}

struct revocation_list *revoke_cpy(parm)
struct revocation_list *parm;
{
	return (parm);
}

revoke_cmp(a, b)
struct revocation_list *a, *b;
{
int ret;

	ret = dn_cmp(a->issuer, b->issuer);
	if (ret != 0)
		return (ret);
	ret = strcmp(a->last_update, b->last_update);
	if (ret != 0)
		return (ret);

	return (0);
}

/* ARGSUSED */

revoke_free(parm)
struct signature *parm;
{
}

revoke_syntax()
{
	(void) add_attribute_syntax(
		"CertificateList",
		(IFP)enc_revoke,	(IFP)dec_revoke,
		(IFP)str2revoke,	print_revoke,
		(IFP)revoke_cpy,	revoke_cmp,
		revoke_free,	NULLCP,
		NULLIFP,	TRUE);
}
