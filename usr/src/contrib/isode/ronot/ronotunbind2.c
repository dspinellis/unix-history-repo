/* ronotunbind2.c - RONOT: ABSTRACT-BIND mapping onto A-RELEASE.RESPONSE */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ronot/RCS/ronotunbind2.c,v 7.2 91/02/22 09:50:35 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ronot/RCS/ronotunbind2.c,v 7.2 91/02/22 09:50:35 mrose Interim $
 *
 *
 * $Log:	ronotunbind2.c,v $
 * Revision 7.2  91/02/22  09:50:35  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/26  14:34:03  mrose
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


/* 	RO-UNBIND.INDICATION */

/* ARGSUSED */

int     RoUnBindInit (sd, acf, rni)
int			  sd;
struct AcSAPfinish	* acf;
struct RoNOTindication	* rni;
{
	/*
	* What is provided here is in-line handling of the
	* (usually NULL) user data as an unbind argument value.
	*
	* A plausible alternative approach is to add a new finish
	* routine (in place of AcFINISHser) as an element in a
	* new ros-mapping structure so that RoWaitRequest performed
	* this automatically for RO-Notation users.
	*/

	PE	  pe;

	if (acf->acf_ninfo == 0)
		return (OK);

	if (acf->acf_ninfo != 1)
		return (ronotlose (rni, RBI_DEC_NINFO, NULLCP, NULLCP));

	if (acf->acf_info[0] == NULLPE)
		return (ronotlose (rni, RBI_DEC_NINFO, NULLCP, NULLCP));

	pe = acf->acf_info[0];
	acf->acf_info[0] = NULLPE;
	if (decode_RONOT_UnBindArgumentValue (pe, 1, NULLIP, NULLVP, &(acf->acf_info[0])) != OK)
	{
		LLOG (rosap_log, LLOG_EXCEPTIONS, ("RO-UNBIND.INDICATION: decode_RONOT_UnBindArgumentValue failed"));
		acf->acf_ninfo = 0;
		pe_free (pe);
		return (ronotlose (rni, RBI_DEC_BIND_ARG, NULLCP, NULLCP));
	}
	pe_free (pe);

	return (OK);
}

/*    RO-UNBIND.RESULT */

/* ARGSUSED */

int	  RoUnBindResult (sd, unbindrespe, rni)
int			  sd;
PE			  unbindrespe;
struct RoNOTindication	* rni;
{
	int			  result;
	PE			  user_data;
	PE			* user_data_p = &(user_data);
	int			  ndata;
	struct AcSAPindication	  aci_s;
	struct AcSAPindication	* aci = &(aci_s);
	struct AcSAPabort	* aca = &(aci->aci_abort);


	if (unbindrespe != NULLPE)
	{
		if (encode_RONOT_UnBindResultValue (user_data_p, 1, 0, NULLCP, unbindrespe) == NOTOK)
		{ 
			LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoUnBindResult: encode_RONOT_UnBindResultValue failed"));
			return (ronotlose (rni, RBI_ENC_UNBIND_RES, NULLCP, NULLCP));
		}
		(*user_data_p)->pe_context = unbindrespe->pe_context;
		ndata = 1;
	}
	else
	{
		(*user_data_p) = NULLPE;
		ndata = 0;
	}

	result = AcRelResponse (sd, ACS_ACCEPT, ACF_NORMAL, user_data_p, ndata, aci);

	if ((*user_data_p) != NULLPE)
	{
		pe_free ((*user_data_p));
	}

	if (result == NOTOK)
	{
		LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoUnBindResult: AcRelResponse failed"));
		/* Have an AcSAPindication, need to return RoNOTindication */
		(void) acs2ronotlose (rni, "RO-UNBIND.RESULT", aca);
		ACAFREE (aca);
		return (NOTOK);
	}

	return (result);
}

/*    RO-UNBIND.ERROR */

/* ARGSUSED */

int	  RoUnBindError (sd, unbinderrpe, rni)
int			  sd;
PE			  unbinderrpe;
struct RoNOTindication	* rni;
{
	int			  result;
	PE			  user_data;
	PE			* user_data_p = &(user_data);
	int			  ndata;
	struct AcSAPindication	  aci_s;
	struct AcSAPindication	* aci = &(aci_s);
	struct AcSAPabort	* aca = &(aci->aci_abort);


	if (unbinderrpe != NULLPE)
	{
		if (encode_RONOT_UnBindErrorValue (user_data_p, 1, 0, NULLCP, unbinderrpe) == NOTOK)
		{ 
			LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoUnBindError: encode_RONOT_UnBindErrorValue failed"));
			return (ronotlose (rni, RBI_ENC_UNBIND_ERR, NULLCP, NULLCP));
		}
		(*user_data_p)->pe_context = unbinderrpe->pe_context;
		ndata = 1;
	}
	else
	{
		(*user_data_p) = NULLPE;
		ndata = 0;
	}

	result = AcRelResponse (sd, ACS_REJECT, ACR_NOTFINISHED, user_data_p, ndata, aci);

	if ((*user_data_p) != NULLPE)
	{
		pe_free ((*user_data_p));
	}

	if (result == NOTOK)
	{
		LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoUnBindError: AcRelResponse failed"));
		(void) acs2ronotlose (rni, "RO-UNBIND.ERROR", aca);
		ACAFREE (aca);
		return (NOTOK);
	}

	return (result);
}

/*    RO-UNBIND.REJECT */

/* ARGSUSED */

int	  RoUnBindReject (sd, status, reason, rni)
int			  sd;
int			  status;
int			  reason;
struct RoNOTindication	* rni;
{
	int			  result;
	struct AcSAPindication	  aci_s;
	struct AcSAPindication	* aci = &(aci_s);
	struct AcSAPabort	* aca = &(aci->aci_abort);

	LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoUnBindReject: RO-UNBIND.REJECT called on %d", sd));

	result = AcRelResponse (sd, status, reason, NULLPEP, 0, aci);

	if (result == NOTOK)
	{
		LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoUnBindReject: AcRelResponse failed"));
		(void) acs2ronotlose (rni, "RO-UNBIND.ERROR", aca);
		ACAFREE (aca);
		return (NOTOK);
	}

	return (result);
}

