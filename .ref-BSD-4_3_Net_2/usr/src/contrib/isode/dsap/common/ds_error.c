/* ds_error.c - Directory Operation Errors */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/ds_error.c,v 7.5 91/03/09 11:53:21 mrose Exp $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/ds_error.c,v 7.5 91/03/09 11:53:21 mrose Exp $
 *
 *
 * $Log:	ds_error.c,v $
 * Revision 7.5  91/03/09  11:53:21  mrose
 * update
 * 
 * Revision 7.4  91/02/22  09:19:04  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/10/17  11:41:43  mrose
 * sync
 * 
 * Revision 7.2  90/03/15  11:17:31  mrose
 * quipu-sync
 * 
 * Revision 7.1  89/12/19  16:19:20  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:42:08  mrose
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
#include "quipu/name.h"
#include "quipu/dsp.h"
#include "quipu/ds_error.h"
#include "quipu/bind.h"

extern LLog * log_dsap;

extern int dn_print ();
extern int AttrT_print ();
int dsa_dead;

static char * abandon_fail [] = {
	"No error !!!",
	"No such operation",
	"Too late",
	"Cannot abandon"
	};

static char * at_problem [] = {
	"No error !!!",
	"No such attribute in the entry",
	"Invalid attribute syntax",
	"Undefined Attribute type",
	"Inappropriate Matching",
	"Constrain violation",
	"Attribute or Value already exists"
	};

static char * name [] = {
	"No error !!!",
	"No such object",
	"Alias problem",
	"Invalid attribute syntax",
	"Alias dereference"
	};

static char * security [] = {
	"No error !!!",
	"Inappropriate authentication",
	"Invalid credentials",
	"Access rights",
	"Invalid signature",
	"Protection required",
	"No information"
	};

static char * service [] = {
	"No error !!!",
	"Busy",
	"Unavailable",
	"Unwilling to perform",
	"Chaining required",
	"Unable to proceed",
	"Invalid Reference",
	"Timelimit exceeded",
	"Administrative limit exceeded",
	"Loop detect",
	"Unavailable critical extension",
	"Out of scope",
	"DIT error"
	};

static char * update [] = {
	"No error !!!",
	"Naming violation",
	"Object class violation",
	"Only allowed on leaf entries",
	"Can't alter the RDN",
	"Already exists",
	"Affects multiple DSAs",
	"Object class modifications Prohibited"
	};

/* ARGSUSED */

de_print (ps, err, format)
PS	ps;
struct DSError *err;
int	format;
{
    ds_error (ps, err);
}

ds_error (ps,err)
PS ps;
struct DSError *err;
{
struct DSE_at_problem *at_prob;

switch (err->dse_type) {
   case DSE_NOERROR:
	ps_print (ps,"No error !!!\n");
	break;
   case DSE_ABANDON_FAILED:
	ps_printf (ps,"*** Abandon failure: %s, id %d ***\n" ,abandon_fail[err->ERR_ABANDON_FAIL.DSE_ab_problem], err->ERR_ABANDON_FAIL.DSE_ab_invokeid);
	break;
   case DSE_ATTRIBUTEERROR:
	ps_print (ps,"*** Attribute error ***\n");
	dn_print (ps,err->ERR_ATTRIBUTE.DSE_at_name, RDNOUT);
	ps_print (ps,"\n");
	for (at_prob = &err->ERR_ATTRIBUTE.DSE_at_plist; at_prob != DSE_AT_NOPROBLEM; at_prob = at_prob -> dse_at_next) {
		ps_print (ps,"Attribute type ");
		AttrT_print (ps,at_prob->DSE_at_type,READOUT);
		if (at_prob->DSE_at_value != NULLAttrV) {
			ps_print (ps,", value ");
			AttrV_print (ps,at_prob->DSE_at_value,READOUT);
		}
		ps_printf (ps," - %s\n", at_problem[at_prob->DSE_at_what]);
	}
	break;
   case DSE_NAMEERROR:
	ps_printf (ps,"*** Name error: %s ***\n( Matched: ",name[err->ERR_NAME.DSE_na_problem]);
	dn_print (ps,err->ERR_NAME.DSE_na_matched,RDNOUT);
	ps_print (ps," )\n");
	break;
   case DSE_SERVICEERROR:
	ps_printf (ps,"*** Service error: %s ***\n", service[err->ERR_SERVICE.DSE_sv_problem] );
	break;
   case DSE_REFERRAL:
	if (err->ERR_REFERRAL.DSE_ref_candidates == NULLCONTINUATIONREF) {
		ps_print (ps,"*** Can't contact remote DSA - Address Unknown ***\n");
		break;
	}
	if (err->ERR_REFERRAL.DSE_ref_candidates->cr_reftype == RT_NONSPECIFICSUBORDINATE) {
		ps_print (ps,"*** Non Specific Referral (unable to proceede) ***");
		break;
	}
	ps_print (ps,"*** Can't contact remote part of the directory (\"");
	ufn_dn_print (ps,err->ERR_REFERRAL.DSE_ref_candidates->cr_accesspoints->ap_name,FALSE);
	ps_printf (ps,"\") ***\n");
/*
	ps_printf (ps,"\" ***\n    (%s)\n", paddr2str (err->ERR_REFERRAL.DSE_ref_candidates->cr_accesspoints->ap_address,NULLNA));
*/
	break;
   case DSE_DSAREFERRAL:
	if (err->ERR_REFERRAL.DSE_ref_candidates == NULLCONTINUATIONREF) {
		ps_print (ps,"*** Referral error - Null reference ***\n");
		break;
	}
	ps_print (ps,"*** DSA Referral error - ");
	dn_print (ps,err->ERR_REFERRAL.DSE_ref_candidates->cr_accesspoints->ap_name,RDNOUT);
	ps_printf (ps," - %s ***\n", paddr2str (err->ERR_REFERRAL.DSE_ref_candidates->cr_accesspoints->ap_address,NULLNA));
	dn_print (ps,err->ERR_REFERRAL.DSE_ref_prefix,RDNOUT);
	break;
   case DSE_SECURITYERROR:
	ps_printf (ps,"*** Security error - %s ***\n",security[err->ERR_SECURITY.DSE_sc_problem]);
	break;
   case DSE_UPDATEERROR:
	ps_printf (ps,"*** Update error - %s ***\n",update[err->ERR_UPDATE.DSE_up_problem]);
	break;
   case DSE_ABANDONED:
	ps_print (ps,"*** Abandoned error ***\n");
	break;
   case DSE_REMOTEERROR:
	dsa_dead = TRUE;
	ps_print (ps,"*** Problem with DSA ***\n");
	break;
   case DSE_LOCALERROR:
	ps_print (ps,"*** Problem with DUA ***\n");
	break;
   case DSE_INTR_ABANDON_FAILED:
	ps_print (ps,"*** Abandoned Failed ***\n");
	break;
   case DSE_INTR_ABANDONED:
	ps_print (ps,"*** Abandoned ***\n");
	break;
   case DSE_INTRERROR:
	ps_print (ps,"*** Interrupted ***\n");
	break;
   default:
	dsa_dead = TRUE;
	ps_printf (ps,"*** Undefined error '%d' ***\n",err->dse_type);
	break;
   }

ds_error_free (err);

}


log_ds_error (err)
struct DSError *err;
{
struct DSE_at_problem *at_prob;

switch (err->dse_type) {
   case DSE_NOERROR:
	LLOG (log_dsap,LLOG_EXCEPTIONS,("No error !!!"));
	break;
   case DSE_ABANDON_FAILED:
	LLOG (log_dsap,LLOG_EXCEPTIONS,("Abandon failure"));
	LLOG (log_dsap,LLOG_TRACE,("%s,id %d" ,abandon_fail[err->ERR_ABANDON_FAIL.DSE_ab_problem], err->ERR_ABANDON_FAIL.DSE_ab_invokeid));
	break;
   case DSE_ATTRIBUTEERROR:
	LLOG (log_dsap,LLOG_EXCEPTIONS,("Attribute error\n"));
	pslog (log_dsap,LLOG_TRACE,"...",dn_print,(caddr_t)err->ERR_ATTRIBUTE.DSE_at_name);
	for (at_prob = &err->ERR_ATTRIBUTE.DSE_at_plist; at_prob != DSE_AT_NOPROBLEM; at_prob = at_prob -> dse_at_next) {
		LLOG (log_dsap,LLOG_TRACE, (at_problem[at_prob->DSE_at_what]));
		if (at_prob->DSE_at_value != NULLAttrV) 
			pslog (log_dsap,LLOG_TRACE,"type", AttrT_print, (caddr_t) at_prob->DSE_at_type);
	}
	break;
   case DSE_NAMEERROR:
	LLOG (log_dsap,LLOG_EXCEPTIONS,("Name error: %s",name[err->ERR_NAME.DSE_na_problem]));
	pslog (log_dsap,LLOG_EXCEPTIONS,"   Matched",dn_print, (caddr_t)err->ERR_NAME.DSE_na_matched);
	break;
   case DSE_SERVICEERROR:
	LLOG (log_dsap,LLOG_EXCEPTIONS,("Service error: %s ", service[err->ERR_SERVICE.DSE_sv_problem] ));
	break;
   case DSE_REFERRAL:
	LLOG (log_dsap,LLOG_EXCEPTIONS,("Referral error"));
	if (err->ERR_REFERRAL.DSE_ref_candidates == NULLCONTINUATIONREF) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("NULL reference in referral error"));
		break;
	}
	pslog (log_dsap,LLOG_TRACE,"ap_name",dn_print, (caddr_t)err->ERR_REFERRAL.DSE_ref_candidates->cr_accesspoints->ap_name);
	break;
   case DSE_DSAREFERRAL:
	LLOG (log_dsap,LLOG_EXCEPTIONS,("DSA!! Referral error"));
	if (err->ERR_REFERRAL.DSE_ref_candidates == NULLCONTINUATIONREF) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("NULL reference in DSA referral error"));
		break;
	}
	pslog (log_dsap,LLOG_TRACE,"ap_name",dn_print, (caddr_t)err->ERR_REFERRAL.DSE_ref_candidates->cr_accesspoints->ap_name);
	break;
   case DSE_SECURITYERROR:
	LLOG (log_dsap,LLOG_EXCEPTIONS,("Security error - %s ",security[err->ERR_SECURITY.DSE_sc_problem]));
	break;
   case DSE_UPDATEERROR:
	LLOG (log_dsap,LLOG_EXCEPTIONS,("Update error - %s ",update[err->ERR_UPDATE.DSE_up_problem]));
	break;
   case DSE_ABANDONED:
	LLOG (log_dsap,LLOG_EXCEPTIONS,("Abandoned error"));
	break;
   case DSE_REMOTEERROR:
	LLOG (log_dsap, LLOG_EXCEPTIONS,("remote DSA error !!!"));
	break;
   case DSE_LOCALERROR:
	LLOG (log_dsap, LLOG_EXCEPTIONS,("local DUA error !!!"));
	break;
   case DSE_INTR_ABANDON_FAILED:
	LLOG (log_dsap, LLOG_EXCEPTIONS,("Abandon failed !!!"));
	break;
   case DSE_INTR_ABANDONED:
	LLOG (log_dsap, LLOG_EXCEPTIONS,("Abandoned !!!"));
	break;
   case DSE_INTRERROR:
	LLOG (log_dsap, LLOG_EXCEPTIONS,("Interrupted !!!"));
	break;
   default:
	LLOG (log_dsap,LLOG_EXCEPTIONS,("Unknown ds error type (%d)",err->dse_type));
	break;
   }

}


ds_error_free (err)
struct DSError * err;
{
register struct DSE_at_problem *at_prob;

switch (err->dse_type) {
	case DSE_ATTRIBUTEERROR:
		dn_free (err->ERR_ATTRIBUTE.DSE_at_name);
		at_prob = &err->ERR_ATTRIBUTE.DSE_at_plist;
		if (at_prob->DSE_at_value != NULLAttrV)
			AttrV_free (at_prob->DSE_at_value);
		if (at_prob->DSE_at_type != NULLAttrT)
			AttrT_free (at_prob->DSE_at_type);

		for (at_prob = at_prob->dse_at_next; at_prob != DSE_AT_NOPROBLEM; at_prob = at_prob -> dse_at_next) {
			if (at_prob->DSE_at_value != NULLAttrV)
				AttrV_free (at_prob->DSE_at_value);
			if (at_prob->DSE_at_type != NULLAttrT)
				AttrT_free (at_prob->DSE_at_type);
			free ((char *) at_prob);
		}
		break;
	case DSE_NAMEERROR:
		dn_free (err->ERR_NAME.DSE_na_matched);
		break;
	case DSE_REFERRAL:
		if(err->ERR_REFERRAL.DSE_ref_prefix != NULLDN)
		    LLOG(log_dsap,LLOG_EXCEPTIONS,("SCREAM! prefix in referral"));
		/* fall */
	case DSE_DSAREFERRAL:
		dn_free (err->ERR_REFERRAL.DSE_ref_prefix);
		crefs_free (err->ERR_REFERRAL.DSE_ref_candidates);
		break;
	default:
		break;
	}

	err->dse_type = DSE_NOERROR;

}

ds_bind_error(ps, err)
PS ps;
struct ds_bind_error *err;
{

  switch (err->dbe_type) {
    case DBE_TYPE_SERVICE:
	ps_printf(ps, "*** Service error : %s ***\n", 
		service[err->dbe_value]);
	break;
    case DBE_TYPE_SECURITY:
	ps_printf(ps, "*** Security error : %s ***\n", 
		security[err->dbe_value]);
	break;
    default:
	ps_printf(ps, "*** Unrecognised bind error! ***");
	break;
  }
}
