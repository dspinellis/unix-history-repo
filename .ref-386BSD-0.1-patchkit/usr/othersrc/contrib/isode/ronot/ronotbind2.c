/* ronotbind2.c - RONOT: ABSTRACT-BIND mapping onto A-ASSOCIATE.RESPONSE */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ronot/RCS/ronotbind2.c,v 7.2 91/02/22 09:50:31 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ronot/RCS/ronotbind2.c,v 7.2 91/02/22 09:50:31 mrose Interim $
 *
 *
 * $Log:	ronotbind2.c,v $
 * Revision 7.2  91/02/22  09:50:31  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/26  14:33:57  mrose
 * template
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

#include "tailor.h"
#include "logger.h"
#include "ronot.h"
#include "RONOT-types.h"


/*    RO-BIND.INDICATION */

/* ARGSUSED */

int	  RoBindInit (vecp, vec, acs, rni)
int			  vecp;
char			**vec;
struct AcSAPstart	* acs;
struct RoNOTindication	* rni;
{
	int			  result;
	struct AcSAPindication	  aci_s;
	struct AcSAPindication	* aci = &(aci_s);
	struct AcSAPabort	* aca = &(aci->aci_abort);

	result = AcInit (vecp, vec, acs, aci);

	if (result != OK)
	{
		LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoBindInit: RO-BIND.INDICATION: failed"));
		/* Have an AcSAPindication, need to return RoNOTindication */
		(void) acs2ronotlose (rni, "RO-BIND.INDICATION", aca);
		ACAFREE (aca);
		return (NOTOK);
	}
	else
	{
		struct RoSAPindication	  roi_s;

		if (RoSetService (acs->acs_sd, RoPService, &(roi_s)) == NOTOK)
		{
			LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoBindInit: RoSetService failed"));
			return (ronotlose (rni, RBI_SET_ROSE_PRES, NULLCP, NULLCP));
		}

		if (ParseRoBindArgument (acs, rni) != OK)
		{
			LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoBindInit: ParseRoBindArgument failed"));
			ACSFREE (acs);
			return (NOTOK);
		}
	}

	return (result);
}

int	  ParseRoBindArgument (acs, rni)
struct AcSAPstart	* acs;
struct RoNOTindication	* rni;
{
	PE	  pe;

	if (acs->acs_ninfo == 0)
		return (OK);

	if (acs->acs_ninfo != 1)
		return (ronotlose (rni, RBI_DEC_NINFO, NULLCP, NULLCP));

	if (acs->acs_info[0] == NULLPE)
		return (ronotlose (rni, RBI_DEC_NINFO, NULLCP, NULLCP));

	pe = acs->acs_info[0];
	acs->acs_info[0] = NULLPE;
	if (decode_RONOT_BindArgumentValue (pe, 1, NULLIP, NULLVP, &acs->acs_info[0]) != OK)
	{
		LLOG (rosap_log, LLOG_EXCEPTIONS, ("ParseRoBindArgument: decode_RONOT_BindArgumentValue failed"));
		acs->acs_ninfo = 0;
		pe_free (pe);
		(void) RoBindReject (acs, ACS_TRANSIENT, ACS_USER_NOREASON, rni);
		return (ronotlose (rni, RBI_DEC_BIND_ARG, NULLCP, NULLCP));
	}
	pe_free (pe);

	return (OK);
}

/*    RO-BIND.RESULT */

int	  RoBindResult (sd, context, respondtitle, respondaddr, ctxlist,
		defctxresult, prequirements, srequirements, isn, settings,
		ref, bindrespe, rni)
int			  sd;
OID			  context;
AEI			  respondtitle;
struct PSAPaddr		* respondaddr;
struct PSAPctxlist	* ctxlist;
int			  defctxresult;
int			  prequirements;
int			  srequirements;
long			  isn;
int			  settings;
struct SSAPref		* ref;
PE			  bindrespe;
struct RoNOTindication	* rni;
{
	int			  result;
	PE			  user_data;
	PE			* user_data_p = &(user_data);
	int			  ndata;
	struct AcSAPindication	  aci_s;
	struct AcSAPindication	* aci = &(aci_s);
	struct AcSAPabort	* aca = &(aci->aci_abort);


	/* Wrap the user data with the remote operations BIND RESULT tag */

	if (bindrespe != NULLPE)
	{
		if (encode_RONOT_BindResultValue (user_data_p, 1, 0, NULLCP, bindrespe) == NOTOK)
		{ 
			LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoBindResult: encode_RONOT_BindResultValue failed"));
			return (ronotlose (rni, RBI_ENC_BIND_RES, NULLCP, NULLCP));
		}
		(*user_data_p)->pe_context = bindrespe->pe_context;
		ndata = 1;
	}
	else
	{
		(*user_data_p) = NULLPE;
		ndata = 0;
	}

	result = AcAssocResponse (sd, ACS_ACCEPT, ACS_USER_NULL, context,
		    respondtitle, respondaddr, ctxlist, defctxresult,
		    prequirements, srequirements, isn, settings, ref,
		    user_data_p, ndata, aci);

	if ((*user_data_p) != NULLPE)
	{
		pe_free ((*user_data_p));
	}

	if (result == NOTOK)
	{
		LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoBindResult: AcAssocResponse failed"));
		/* Have an AcSAPindication, need to return RoNOTindication */
		(void) acs2ronotlose (rni, "RO-BIND.RESULT", aca);
		ACAFREE (aca);
		return (NOTOK);
	}

	return (result);
}

/*    RO-BIND.ERROR */

int	  RoBindError (sd, context, respondtitle,
		respondaddr, ctxlist, defctxresult, prequirements,
		srequirements, isn, settings, ref, binderrpe, rni)
int			  sd;
OID			  context;
AEI			  respondtitle;
struct PSAPaddr		* respondaddr;
struct PSAPctxlist	* ctxlist;
int			  defctxresult;
int			  prequirements;
int			  srequirements;
long			  isn;
int			  settings;
struct SSAPref		* ref;
PE			  binderrpe;
struct RoNOTindication	* rni;
{
	int			  result;
	PE			  user_data;
	PE			* user_data_p = &(user_data);
	int			  ndata;
	struct AcSAPindication	  aci_s;
	struct AcSAPindication	* aci = &(aci_s);
	struct AcSAPabort	* aca = &(aci->aci_abort);


	/* Wrap the user data with the remote operations BIND ARGUMENT tag */

	if (binderrpe != NULLPE)
	{
		if (encode_RONOT_BindErrorValue (user_data_p, 1, 0, NULLCP, binderrpe) == NOTOK)
		{ 
			LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoBindError: encode_RONOT_BindErrorValue failed"));
			return (ronotlose (rni, RBI_ENC_BIND_ERR, NULLCP, NULLCP));
		}
		(*user_data_p)->pe_context = binderrpe->pe_context;
		ndata = 1;
	}
	else
	{
		(*user_data_p) = NULLPE;
		ndata = 0;
	}

	result = AcAssocResponse (sd, ACS_PERMANENT, ACS_USER_NOREASON,
		    context, respondtitle, respondaddr, ctxlist, defctxresult,
		    prequirements, srequirements, isn, settings, ref,
		    user_data_p, ndata, aci);

	if ((*user_data_p) != NULLPE)
	{
		pe_free ((*user_data_p));
	}

	if (result == NOTOK)
	{
		LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoBindError: AcAssocResponse failed"));
		/* Have an AcSAPindication, need to return RoNOTindication */
		(void) acs2ronotlose (rni, "RO-BIND.ERROR", aca);
		ACAFREE (aca);
		return (NOTOK);
	}

	return (result);
}

int	  RoBindReject (acs, status, reason, rni)
struct AcSAPstart	* acs;
int			  status;
int			  reason;
struct RoNOTindication	* rni;
{
	struct PSAPstart	* ps = &(acs->acs_start);
	struct AcSAPindication	  aci_s;
	struct AcSAPindication	* aci = &(aci_s);
	struct AcSAPabort	* aca = &(aci->aci_abort);

	LLOG (rosap_log, LLOG_EXCEPTIONS, ("RO-BIND.REJECT called on %d", acs->acs_sd));

	if (AcAssocResponse (acs->acs_sd, status,
	    reason, acs->acs_context, NULLAEI,
	    NULLPA, &(ps->ps_ctxlist), ps->ps_defctxresult,
	    PR_MYREQUIRE, ps->ps_srequirements, SERIAL_NONE,
	    ps->ps_settings, &(ps->ps_connect), NULLPEP, 0,
	    aci) == OK)
	{
		LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoBindReject OK"));
		return (OK);
	}
	else
	{
		LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoBindReject NOTOK"));
		return (acs2ronotlose (rni, "RO-BIND.REJECT", aca));
	}
}

