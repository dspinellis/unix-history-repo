/* ronotunbind1.c - RONOT: Maps ABSTRACT-UNBIND onto A-RELEASE.REQUEST */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ronot/RCS/ronotunbind1.c,v 7.2 91/02/22 09:50:34 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ronot/RCS/ronotunbind1.c,v 7.2 91/02/22 09:50:34 mrose Interim $
 *
 *
 * $Log:	ronotunbind1.c,v $
 * Revision 7.2  91/02/22  09:50:34  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/26  14:34:01  mrose
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


/*    RO-UNBIND.REQUEST */

/* ARGSUSED */

int	  RoUnBindRequest (sd, unbindargpe, secs, acr, rni)
int			  sd;
PE			  unbindargpe;
int			  secs;
struct AcSAPrelease	* acr;
struct RoNOTindication	* rni;
{
	int			  result;
	PE			  user_data;
	PE			* user_data_p = &(user_data);
	int			  ndata;
	struct AcSAPindication	  aci_s;
	struct AcSAPindication	* aci = &aci_s;
	struct AcSAPabort	* aca = &(aci->aci_abort);


	if (unbindargpe != NULLPE)
	{
		if (encode_RONOT_UnBindArgumentValue (user_data_p, 1, 0, NULLCP, unbindargpe) == NOTOK)
		{ 
			LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoUnbindRequest: encode_RONOT_UnBindArgumentValue failed"));
	    		return (ronotlose (rni, RBI_ENC_UNBIND_ARG, NULLCP, NULLCP));
		}
		(*user_data_p)->pe_context = unbindargpe->pe_context;
		ndata = 1;
	}
	else
	{
		(*user_data_p) = NULLPE;
		ndata = 0;
	}

	result = AcRelRequest (sd, ACF_NORMAL, user_data_p, ndata, secs, acr, aci);

	if ((*user_data_p) != NULLPE)
	{
		pe_free ((*user_data_p));
	}

	if (result == NOTOK)
	{
		if (aci->aci_abort.aca_reason == ACS_TIMER)
		{
			/* ADT: Watch this !! */
			/* more work needed !!! */
			ACAFREE (aca);
			return (DONE);
		}
		else
		{
			LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoUnbindRequest: AcRelRequest failed"));
			(void) acs2ronotlose (rni, "RO-UNBIND.REQUEST", aca);
			ACAFREE (aca);
			return (NOTOK);
		}
	}
	else
	{
		if (ParseRoUnBindResponse (acr, rni) != OK)
		{
			LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoUnbindRequest: ParseRoUnBindResponse failed"));
			ACRFREE (acr);
			return (NOTOK);
		}
	}

	return (result);
}

/*    RO-UNBIND.RETRY */

/* ARGSUSED */

int	  RoUnBindRetry (sd, secs, acr, rni)
int			  sd;
int			  secs;
struct AcSAPrelease	* acr;
struct RoNOTindication	* rni;
{
	int			  result;
	struct AcSAPindication	  aci_s;
	struct AcSAPindication	* aci = &(aci_s);
	struct AcSAPabort	* aca = &(aci->aci_abort);

	result = AcRelRetryRequest (sd, secs, acr, aci);

	if (result == NOTOK)
	{
		if (aci->aci_abort.aca_reason == ACS_TIMER)
		{
			/* ADT: Watch out for this */
			/* more work needed !!! */
			ACAFREE (aca);
			return (DONE);
		}
		else
		{
			LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoUnbindRetry: AcRelRetryRequest failed"));
			(void) acs2ronotlose (rni, "RO-UNBIND.RETRY", aca);
			ACAFREE (aca);
			return (NOTOK);
		}
	}
	else
	{
		if (ParseRoUnBindResponse (acr, rni) != OK)
		{
			LLOG (rosap_log, LLOG_EXCEPTIONS, ("RoUnbindRetry: ParseRoUnBindResponse failed"));
			ACRFREE (acr);
			return (NOTOK);
		}
	}

	return (result);
}

int	  ParseRoUnBindResponse (acr, rni)
struct AcSAPrelease	* acr;
struct RoNOTindication	* rni;
{
	PE	  pe;

	if (acr->acr_ninfo == 0)
		return (OK);

	if (acr->acr_ninfo != 1)
		return (ronotlose (rni, RBI_DEC_NINFO, NULLCP, NULLCP));

	if (acr->acr_info[0] == NULLPE)
		return (ronotlose (rni, RBI_DEC_NINFO, NULLCP, NULLCP));

	/* ADT: Can we get rid of this copy? */
	pe = acr->acr_info[0];
	acr->acr_info[0] = NULLPE;
	if (acr->acr_affirmative == ACS_ACCEPT)
	{
		if (decode_RONOT_UnBindResultValue (pe, 1, NULLIP, NULLVP, &acr->acr_info[0]) != OK)
		{
			LLOG (rosap_log, LLOG_EXCEPTIONS, ("ParseRoUnBindResponse: decode_RONOT_UnBindResultValue failed"));
			acr->acr_ninfo = 0;
			pe_free (pe);
			return (ronotlose (rni, RBI_DEC_UNBIND_RES, NULLCP, NULLCP));
		}
	}
	else
	{
		if (decode_RONOT_UnBindErrorValue (pe, 1, NULLIP, NULLVP, &acr->acr_info[0]) != OK)
		{
			LLOG (rosap_log, LLOG_EXCEPTIONS, ("ParseRoUnBindResponse: decode_RONOT_UnBindErrorValue failed"));
			acr->acr_ninfo = 0;
			pe_free (pe);
			return (ronotlose (rni, RBI_DEC_UNBIND_RES, NULLCP, NULLCP));
		}
	}
	pe_free (pe);

	return (OK);
}

