/* ronotbind1.c - RONOT: ABSTRACT-BIND mapping onto A-ASSOCIATE.REQUEST */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ronot/RCS/ronotbind1.c,v 7.3 91/02/22 09:50:30 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ronot/RCS/ronotbind1.c,v 7.3 91/02/22 09:50:30 mrose Interim $
 *
 *
 * $Log:	ronotbind1.c,v $
 * Revision 7.3  91/02/22  09:50:30  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/08/08  14:14:48  mrose
 * update
 * 
 * Revision 7.1  90/07/26  14:33:55  mrose
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


/*    RO-BIND.REQUEST */

/* ARGSUSED */

int	  RoAsynBindRequest (context, callingtitle, calledtitle,
		callingaddr, calledaddr, ctxlist, defctxname,
		prequirements, srequirements, isn, settings, ref,
		bindargpe, qos, acc, rni, async)
OID			  context;
AEI			  callingtitle;
AEI			  calledtitle;
struct PSAPaddr		* callingaddr;
struct PSAPaddr		* calledaddr;
struct PSAPctxlist	* ctxlist;
OID			  defctxname;
int			  prequirements;
int			  srequirements;
long			  isn;
int			  settings;
struct SSAPref		* ref;
PE			  bindargpe;
struct QOStype		* qos;
struct AcSAPconnect	* acc;
struct RoNOTindication	* rni;
int			  async;
{
	int			  result;
	PE			  user_data;
	PE			* user_data_p = &(user_data);
	int			  ndata;
	struct AcSAPindication	  aci_s;
	struct AcSAPindication	* aci = &(aci_s);
	struct AcSAPabort	* aca = &(aci->aci_abort);


	/* Wrap the user data with the remote operations BIND ARGUMENT tag */

	if (bindargpe != NULLPE)
	{
		if (encode_RONOT_BindArgumentValue (user_data_p, 1, 0, NULLCP, bindargpe) == NOTOK)
		{ 
			LLOG (rosap_log, LLOG_NOTICE, ("RO-BIND.REQUEST : encode_RONOT_BindArgumentValue failed"));
			return (ronotlose (rni, RBI_ENC_BIND_ARG, NULLCP, NULLCP));
		}

		/*
		* Set the context of the user data presentation element
		* from the context of the bind argument presentation element
		*/
		(*user_data_p)->pe_context = bindargpe->pe_context;
		ndata = 1;
	}
	else
	{
		(*user_data_p) = NULLPE;
		ndata = 0;
	}

	result = AcAsynAssocRequest (context, callingtitle, calledtitle,
	    callingaddr, calledaddr, ctxlist, defctxname,
	    prequirements, srequirements, isn, settings, ref,
	    user_data_p, ndata, qos, acc, aci, async);

	if ((*user_data_p) != NULLPE)
	{
		pe_free ((*user_data_p));
	}

	if (result == NOTOK)
	{
		LLOG (rosap_log, LLOG_NOTICE, ("RO-BIND.REQUEST : RoAsynBindRequest failed"));
		/* Have an AcSAPindication, need to return RoNOTindication */
		(void) acs2ronotlose (rni, "RO-BIND.REQUEST", aca);
		ACAFREE (aca);
		return (NOTOK);
	}

	if (((!async) && (result == OK)) || (async && (result == DONE)))
	{
		if (acc->acc_result == ACS_ACCEPT)
		{
			struct RoSAPindication	  roi_s;

			if (RoSetService (acc->acc_sd, RoPService, &(roi_s)) == NOTOK)
			{
				LLOG (rosap_log, LLOG_NOTICE, ("RO-BIND.REQUEST : RoSetService failed"));
				return (ronotlose (rni, RBI_SET_ROSE_PRES, NULLCP, NULLCP));
			}
		}

		if (ParseRoBindResponse (acc, rni) != OK)
		{
			LLOG (rosap_log, LLOG_NOTICE, ("RO-BIND.REQUEST : ParseRoBindResponse failed"));
			ACCFREE (acc);
			return (NOTOK);
		}
	}

	return (result);
}

/*    RO-BIND.RETRY */

/* ARGSUSED */

int	  RoAsynBindRetry (ad, do_next_nsap, acc, rni)
int			  ad;
int			  do_next_nsap;
struct AcSAPconnect	* acc;
struct RoNOTindication	* rni;
{
	int			  result;
	struct AcSAPindication	  aci_s;
	struct AcSAPindication	* aci = &aci_s;
	struct AcSAPabort	* aca = &(aci->aci_abort);

	if (do_next_nsap)
	{
		result = AcAsynNextRequest (ad, acc, aci);
	}
	else
	{
		result = AcAsynRetryRequest (ad, acc, aci);
	}

	if (result == NOTOK)
	{
		LLOG (rosap_log, LLOG_NOTICE, ("RO-BIND.RETRY : AcAsynRetryRequest failed"));
		(void) acs2ronotlose (rni, "RO-BIND.RETRY", aca);
		ACAFREE (aca);
		return (NOTOK);
	}

	if (result == DONE)
	{
		if (acc->acc_result == ACS_ACCEPT)
		{
			struct RoSAPindication	  roi_s;

			if (RoSetService (acc->acc_sd, RoPService, &(roi_s)) == NOTOK)
			{
				LLOG (rosap_log, LLOG_NOTICE, ("RO-BIND.REQUEST : RoSetService failed"));
				return (ronotlose (rni, RBI_SET_ROSE_PRES, NULLCP, NULLCP));
			}
		}

		if (ParseRoBindResponse (acc, rni) != OK)
		{
			LLOG (rosap_log, LLOG_NOTICE, ("RO-BIND.RETRY : ParseRoBindResponse failed"));
			ACCFREE (acc);
			return (NOTOK);
		}
	}

	return (result);
}

int	  ParseRoBindResponse (acc, rni)
struct AcSAPconnect *acc;
struct RoNOTindication	* rni;
{
	PE	  pe;

	if (acc->acc_ninfo == 0)
		return (OK);

	if (acc->acc_ninfo != 1)
		return (ronotlose (rni, RBI_DEC_NINFO, NULLCP, NULLCP));

	if (acc->acc_info[0] == NULLPE)
		return (ronotlose (rni, RBI_DEC_NINFO, NULLCP, NULLCP));

	pe = acc->acc_info[0];
	acc->acc_info[0] = NULLPE;
	if (acc->acc_result == ACS_ACCEPT)
	{
		if (decode_RONOT_BindResultValue (pe, 1, NULLIP, NULLVP, &acc->acc_info[0]) != OK)
		{
			/* ADT: Should end association here !?! */
			LLOG (rosap_log, LLOG_EXCEPTIONS, ("ParseRoBindResponse: decode_RONOT_BindResultValue failed"));
			acc->acc_ninfo = 0;
			pe_free (pe);
			return (ronotlose (rni, RBI_DEC_BIND_RES, NULLCP, NULLCP));
		}
	}
	else
	{
		if (decode_RONOT_BindErrorValue (pe, 1, NULLIP, NULLVP, &acc->acc_info[0]) != OK)
		{
			LLOG (rosap_log, LLOG_EXCEPTIONS, ("ParseRoBindResponse: decode_RONOT_BindErrorValue failed"));
			acc->acc_ninfo = 0;
			pe_free (pe);
			return (ronotlose (rni, RBI_DEC_BIND_ERR, NULLCP, NULLCP));
		}
	}
	pe_free (pe);

	return (OK);
}

