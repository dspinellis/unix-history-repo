/* dsa_info.c - DSA Operational Information */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/dsa_info.c,v 7.2 91/02/22 09:19:07 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/dsa_info.c,v 7.2 91/02/22 09:19:07 mrose Interim $
 *
 *
 * $Log:	dsa_info.c,v $
 * Revision 7.2  91/02/22  09:19:07  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:41:46  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:42:10  mrose
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
#include "quipu/syntaxes.h"
extern LLog * log_dsap;

static edb_info_free (edb)
struct edb_info * edb;
{
	if (edb == NULLEDB)
		return;

	dn_free (edb->edb_name) ;
	dn_free (edb->edb_getfrom);
	dn_seq_free (edb->edb_allowed);
	dn_seq_free (edb->edb_sendto);
	free ((char *) edb);
}

static struct edb_info * edb_info_cpy (a)
struct edb_info * a;
{
struct edb_info * result;

	result = edb_info_alloc ();
	result->edb_name     = dn_cpy (a->edb_name);
	result->edb_getfrom  = dn_cpy (a->edb_getfrom);
	result->edb_sendto   = dn_seq_cpy (a->edb_sendto);
	result->edb_allowed  = dn_seq_cpy (a->edb_allowed);
	return (result);
}

static edb_info_cmp (a,b)
struct edb_info * a;
struct edb_info * b;
{
int i;

	if (a == NULLEDB)
		return ( b == NULLEDB ? 0 : -1 );

	if (b == NULLEDB)
		return (1);

	if ( (i = dn_cmp (a->edb_name,b->edb_name)) != 0)
		return (i);

	if ( (i = dn_cmp (a->edb_getfrom,b->edb_getfrom)) != 0)
		return (i);

	if ( (i = dn_seq_cmp (a->edb_sendto,b->edb_sendto)) != 0)
		return (i);

	if ( (i = dn_seq_cmp (a->edb_allowed,b->edb_allowed)) != 0)
		return (i);

	return (0);

}

static struct edb_info * edb_info_decode (pe)
PE pe;
{
struct edb_info * a;
	if (decode_Quipu_EDBInfoSyntax(pe,1,NULLIP,NULLVP,&a) == NOTOK) {
		return (NULLEDB);
	}
	return (a);
}


static edb_info_print (ps,edb,format)
PS ps;
struct edb_info * edb;
int format;
{
	if (edb == NULLEDB)
		return;

	if (format == READOUT) {
		if (edb->edb_name != NULLDN)
			dn_print (ps,edb->edb_name,EDBOUT);
		else
			ps_print (ps,"ROOT");

		if (edb->edb_getfrom != NULLDN) {
			ps_print (ps," ( FROM ");
			dn_print (ps,edb->edb_getfrom,EDBOUT);
			ps_print (ps, " )");
			if (edb->edb_allowed != NULLDNSEQ) {
				ps_print (ps,"\n\t\t\t");
				if (edb->edb_name != NULLDN)
					dn_print (ps,edb->edb_name,EDBOUT);
				else
					ps_print (ps,"ROOT");
			} 
		}
		if (edb->edb_allowed != NULLDNSEQ) {
			ps_print (ps," ( TO ");
			dn_seq_print (ps,edb->edb_allowed,READOUT);
			ps_print (ps, " )");
		} 
	} else {	
		if (edb->edb_name != NULLDN)
			dn_print (ps,edb->edb_name,EDBOUT);
		ps_print (ps,"#");
		dn_print (ps,edb->edb_getfrom,EDBOUT);
		ps_print (ps,"#");
		dn_seq_print (ps,edb->edb_allowed,format);
	}

	if (edb->edb_sendto != NULLDNSEQ)
		DLOG (log_dsap,LLOG_EXCEPTIONS,("edb_sendto not null"));

}


static struct edb_info * str2update (str)
char * str;
{
register char * ptr;
char * save,val;
struct edb_info * ei;

	/* dn # num # dn # dnseq # */
	if ((ptr = index (str,'#')) == 0) {
		parse_error ("# missing in update syntax '%s'",str);
		return (NULLEDB);
	}
	ei = edb_info_alloc ();
	ei->edb_sendto = NULLDNSEQ;

	/* go for name */
	save = ptr++;
	if (*str == '#') 
		ei->edb_name = NULLDN;
	else {
		if (! isspace (*--save))
			save++;
		val = *save;
		*save = 0;

		if ((ei->edb_name = str2dn(str)) == NULLDN) {
			free ((char *) ei);
			return (NULLEDB);
		}
		*save = val;
	}

	str = SkipSpace(ptr);
	if ((ptr = index (str,'#')) == 0) {
		parse_error ("2nd # missing in update syntax '%s'",str);
		dn_free (ei->edb_name);
		free ((char *) ei);
		return (NULLEDB);
	}

	save = ptr++;
	if (*str == '#') {
		ei->edb_getfrom = NULLDN;
	} else {
		if (! isspace (*--save))
			save++;
		val = *save;
		*save = 0;

		if ((ei->edb_getfrom = str2dn(str)) == NULLDN) {
			dn_free (ei->edb_name);
			free ((char *) ei);
			return (NULLEDB);
		}
		*save = val;
	}

	/* send to */
	str = SkipSpace(ptr);
	if ((ptr = index (str,'#')) == 0) {
		ptr = 0;
	} else {
		save = ptr++;
		if (! isspace (*--save))
			save++;
		val = *save;
		*save=0;
	}

	if (*str == 0)
		ei->edb_allowed = NULLDNSEQ;
	else if ((ei->edb_allowed = str2dnseq(str)) == NULLDNSEQ) {
		parse_error ("invalid dn '%s'",str);
		dn_free (ei->edb_name);
		dn_free (ei->edb_getfrom);
		free ((char *) ei);
		return (NULLEDB);
	}

	if (ptr != 0)
		*save = val;

	return (ei);	
}

static PE edb_info_enc (ei)
struct edbinfo * ei;
{
PE ret_pe;

	(void) encode_Quipu_EDBInfoSyntax (&ret_pe,0,0,NULLCP,ei);
	return (ret_pe);
}

edbinfo_syntax ()
{
	(void) add_attribute_syntax ("edbinfo",
		(IFP) edb_info_enc,	(IFP) edb_info_decode,
		(IFP) str2update,	edb_info_print,
		(IFP) edb_info_cpy,	edb_info_cmp,
		edb_info_free,		NULLCP,
		NULLIFP,		TRUE );
}
