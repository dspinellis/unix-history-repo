/* dsapbind1.c - DSAP : Bind for Directory protocols DAP, DSP and QSP */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/net/RCS/dsapbind1.c,v 7.3 91/03/09 11:53:35 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/dsap/net/RCS/dsapbind1.c,v 7.3 91/03/09 11:53:35 mrose Exp $
 *
 *
 * $Log:	dsapbind1.c,v $
 * Revision 7.3  91/03/09  11:53:35  mrose
 * update
 * 
 * Revision 7.2  91/02/22  09:21:14  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:43:32  mrose
 * sync
 * 
 * Revision 7.0  90/07/26  14:45:49  mrose
 * *** empty log message ***
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

#include <stdio.h>
#include "logger.h"
#include "quipu/dsap.h"
#include "../x500as/DAS-types.h"

extern  OID     acse_pci;
extern  OID     x500_da_ac;
extern  OID     x500_ds_ac;
extern  OID     quipu_ds_ac;
extern  OID     x500_da_as;
extern  OID     x500_ds_as;
extern  OID     quipu_ds_as;
extern  struct PSAPctxlist      * x500_da_pcdl;
extern  struct PSAPctxlist      * x500_ds_pcdl;
extern  struct PSAPctxlist      * quipu_ds_pcdl;

extern LLog	* log_dsap;

static char * qlocalhost;

int	  DspAsynBindReqAux (callingtitle, calledtitle, callingaddr,
		calledaddr, prequirements, srequirements, isn, settings,
		sf, bindarg, qos, dc, di, async)
AEI			  callingtitle;
AEI			  calledtitle;
struct PSAPaddr		* callingaddr;
struct PSAPaddr		* calledaddr;
int			  prequirements;
int			  srequirements;
long			  isn;
int			  settings;
struct SSAPref		* sf;
struct ds_bind_arg	* bindarg;
struct QOStype		* qos;
struct DSAPconnect	* dc;
struct DSAPindication	* di;
int			  async;
{
	int			  result;
	OID			  app_ctx;
	struct PSAPctxlist	* pcl;
	OID			  def_ctx;
	PE			  bindargpe;
	struct RoNOTindication	  rni_s;
	struct RoNOTindication	* rni = &(rni_s);
	struct AcSAPconnect	* acc = &(dc->dc_connect);

	app_ctx = x500_ds_ac;
	def_ctx = x500_ds_as;

        pcl = (struct PSAPctxlist *) smalloc(sizeof(struct PSAPctxlist));

	(*pcl) = (*(x500_ds_pcdl)); /* struct copy */

	if (encode_DAS_DirectoryBindArgument (&bindargpe, 1, 0, NULLCP, bindarg) != OK)
	{
		return (dsaplose (di, DA_ARG_ENC, NULLCP, "DSP BIND REQUEST"));
	}
	bindargpe->pe_context = DIR_SYSTEM_PC_ID;

	watch_dog ("RoAsynBindRequest (DSP)");
	result = RoAsynBindRequest (app_ctx, callingtitle, calledtitle,
			callingaddr, calledaddr, pcl, def_ctx, prequirements,
			srequirements, isn, settings, sf, bindargpe, qos,
			acc, rni, async);
	watch_dog_reset();

	if (bindargpe != NULLPE)
		pe_free (bindargpe);
	free ((char *) pcl);

	if (result == NOTOK)
	{
		return (ronot2dsaplose (di, "DSP-BIND.REQUEST", rni));
	}

	dc->dc_sd = acc->acc_sd;

	if (((!async) && (result == OK)) || (async && (result == DONE)))
	{
		if (DBindDecode (acc, dc) != OK)
		{
			return (dsaplose (di, DA_RES_DEC, NULLCP, "DSP BIND REQUEST"));
		}
	}

	return (result);
}

int	  DspAsynBindRequest (calledaddr, bindarg, qos_maxtime, dc, di, async)
struct PSAPaddr		* calledaddr;
struct ds_bind_arg	* bindarg;
int			  qos_maxtime;
struct DSAPconnect	* dc;
struct DSAPindication	* di;
int			  async;
{
	struct SSAPref		  sf_s;
	struct SSAPref		* sf = &(sf_s);
	struct QOStype		  qos;

	if (qlocalhost == NULLCP)
		qlocalhost = PLocalHostName();

	if ((sf = addr2ref (qlocalhost)) == NULL)
	{
		sf = &sf_s;
		(void) bzero ((char *) sf, sizeof *sf);
	}

	(void) bzero ((char *) &qos, sizeof qos);
	qos.qos_sversion = NOTOK;	/* Negotiate highest session */
	qos.qos_maxtime = qos_maxtime;

	return (DspAsynBindReqAux (NULLAEI, NULLAEI, NULLPA, calledaddr,
			PR_MYREQUIRE, ROS_MYREQUIRE, SERIAL_NONE, 0, sf,
			bindarg, &qos, dc, di, async));
}

int	  QspAsynBindReqAux (callingtitle, calledtitle, callingaddr,
		calledaddr, prequirements, srequirements, isn, settings,
		sf, bindarg, qos, dc, di, async)
AEI			  callingtitle;
AEI			  calledtitle;
struct PSAPaddr		* callingaddr;
struct PSAPaddr		* calledaddr;
int			  prequirements;
int			  srequirements;
long			  isn;
int			  settings;
struct SSAPref		* sf;
struct ds_bind_arg	* bindarg;
struct QOStype		* qos;
struct DSAPconnect	* dc;
struct DSAPindication	* di;
int			  async;
{
	int			  result;
	OID			  app_ctx;
	struct PSAPctxlist	* pcl;
	OID			  def_ctx;
	PE			  bindargpe;
	struct RoNOTindication	  rni_s;
	struct RoNOTindication	* rni = &(rni_s);
	struct AcSAPconnect	* acc = &(dc->dc_connect);

	app_ctx = quipu_ds_ac;
	def_ctx = quipu_ds_as;

        pcl = (struct PSAPctxlist *) smalloc(sizeof(struct PSAPctxlist));

	(*pcl) = (*(quipu_ds_pcdl)); /* struct copy */

	/* Encode Bind Argument */
	if (encode_DAS_DirectoryBindArgument (&bindargpe, 1, 0, NULLCP, bindarg) != OK)
	{
		return (dsaplose (di, DA_ARG_ENC, NULLCP, "QSP BIND REQUEST"));
	}
	bindargpe->pe_context = DIR_SYSTEM_PC_ID;

	watch_dog ("RoAsynBindRequest (DSP)");
	result = RoAsynBindRequest (app_ctx, callingtitle, calledtitle,
			callingaddr, calledaddr, pcl, def_ctx, prequirements,
			srequirements, isn, settings, sf, bindargpe,
			qos, acc, rni, async);
	watch_dog_reset();

	if (bindargpe != NULLPE)
		pe_free (bindargpe);
	free ((char *) pcl);

	if (result == NOTOK)
	{
		/* Have an RoSAPindication, need to return DSAPindication */
		return (ronot2dsaplose (di, "QSP-BIND.REQUEST", rni));
	}

	dc->dc_sd = acc->acc_sd;

	if (((!async) && (result == OK)) || (async && (result == DONE)))
	{
		if (DBindDecode (acc, dc) != OK)
		{
			return (dsaplose (di, DA_RES_DEC, NULLCP, "QSP BIND REQUEST"));
		}
	}

	return (result);
}

int	  QspAsynBindRequest (calledaddr, bindarg, qos_maxtime, dc, di, async)
struct PSAPaddr		* calledaddr;
struct ds_bind_arg	* bindarg;
int			  qos_maxtime;
struct DSAPconnect	* dc;
struct DSAPindication	* di;
int			  async;
{
	struct SSAPref		  sf_s;
	struct SSAPref		* sf = &(sf_s);
	struct QOStype		  qos;

	if (qlocalhost == NULLCP)
		qlocalhost = PLocalHostName();

	if ((sf = addr2ref (qlocalhost)) == NULL)
	{
		sf = &sf_s;
		(void) bzero ((char *) sf, sizeof *sf);
	}

	(void) bzero ((char *) &qos, sizeof qos);
	qos.qos_sversion = NOTOK;	/* Negotiate highest session */
	qos.qos_maxtime = qos_maxtime;

	/*
	* The quipu system application context can assume that
	* the remote DSA is a Quipu DSA and can take certain actions
	* that are not generally available, such as refusing a
	* connection release request. To do this, session negotiated
	* release appears to be necessary, so SR_NEGOTIATED is
	* is included in the session requirements here.
	*/

	return (QspAsynBindReqAux (NULLAEI, NULLAEI, NULLPA,
			calledaddr, PR_MYREQUIRE,
			ROS_MYREQUIRE | SR_NEGOTIATED, SERIAL_NONE, 0, sf,
			bindarg, &qos, dc, di, async));
}

int	  DspAsynBindRetry (sd, do_next_nsap, dc, di)
int			  sd;
int			  do_next_nsap;
struct DSAPconnect	* dc;
struct DSAPindication	* di;
{
	int			  result;
	struct RoNOTindication	  rni_s;
	struct RoNOTindication	* rni = &(rni_s);
	struct AcSAPconnect	* acc = &(dc->dc_connect);

	watch_dog ("RoAsynBindRetry (DSP)");
	result = RoAsynBindRetry (sd, do_next_nsap, acc, rni);
	watch_dog_reset ();

	if (result == NOTOK)
	{
		return (ronot2dsaplose (di, "DAP-BIND.RETRY", rni));
	}

	if (result == DONE)
	{
		if (DBindDecode (acc, dc) != OK)
		{
			return (dsaplose (di, DA_RES_DEC, NULLCP, "DSP BIND RETRY"));
		}
	}

	return (result);
}

int	  QspAsynBindRetry (sd, do_next_nsap, dc, di)
int			  sd;
int			  do_next_nsap;
struct DSAPconnect	* dc;
struct DSAPindication	* di;
{
	int			  result;
	struct RoNOTindication	  rni_s;
	struct RoNOTindication	* rni = &(rni_s);
	struct AcSAPconnect	* acc = &(dc->dc_connect);

	watch_dog ("RoAsynBindRetry (QSP)");
	result = RoAsynBindRetry (sd, do_next_nsap, acc, rni);
	watch_dog_reset ();

	if (result == NOTOK)
	{
		return (ronot2dsaplose (di, "DAP-BIND.RETRY", rni));
	}

	if (result == DONE)
	{
		if (DBindDecode (acc, dc) != OK)
		{
			return (dsaplose (di, DA_RES_DEC, NULLCP, "QSP BIND RETRY"));
		}
	}

	return (result);
}

int	  DBindDecode (acc, dc)
struct AcSAPconnect	* acc;
struct DSAPconnect	* dc;
{
    struct ds_bind_arg  * bind_res;
    struct ds_bind_error        * bind_err;

    switch(acc->acc_result)
    {
	case ACS_ACCEPT:
	    DLOG(log_dsap, LLOG_NOTICE, ("DBindDecode ACCEPT"));
	    if ((acc->acc_ninfo == 1) && (acc->acc_info[0] != NULLPE))
	    {
	    	if(decode_DAS_DirectoryBindResult(acc->acc_info[0], 1, NULLIP, NULLVP, &bind_res) != OK)
	    	{
		    LLOG (log_dsap,LLOG_EXCEPTIONS,( "Unable to parse DirectoryBindResult"));
		    dc->dc_result = DS_REJECT;
		    return (NOTOK);
	    	}
		dc->dc_un.dc_bind_res = *bind_res; /* struct copy */
		free ((char *) bind_res);
	        dc->dc_result = DS_RESULT;
	    }
	    else
	    {
		LLOG(log_dsap, LLOG_EXCEPTIONS, ("No DirectoryBindResult"));
		dc->dc_result = DS_REJECT;
		return (NOTOK);
	    }
	    break;

	case ACS_PERMANENT:
	    /*
	    * Get the DirectoryBindError
	    */
	    DLOG(log_dsap, LLOG_NOTICE, ("DBindDecode PERMANENT"));
	    if ((acc->acc_ninfo == 1) && (acc->acc_info[0] != NULLPE))
	    {
		if(decode_DAS_DirectoryBindError(acc->acc_info[0], 1, NULLIP, NULLVP, &bind_err) != OK)
		{
		    LLOG(log_dsap, LLOG_EXCEPTIONS, ("Unable to decode DirectoryBindError"));
		    dc->dc_result = DS_REJECT;
		    return (NOTOK);
		}
	        dc->dc_un.dc_bind_err = *bind_err; /* struct copy */;
		free ((char *)bind_err);
		dc->dc_result = DS_ERROR;
	    }
	    else
	    {
		LLOG(log_dsap, LLOG_EXCEPTIONS, ("DirectoryBindError (%s)",
			   AcErrString (acc -> acc_result)));
		dc->dc_result = DS_REJECT;
		return (NOTOK);
	    }
	    break;

	default:
	if ((acc->acc_result == ACS_REFUSED ) || 
	    (acc->acc_result == ACS_PRESENTATION))
	    LLOG (log_dsap,LLOG_NOTICE,( "Association rejected: [%s]",
	      AcErrString(acc->acc_result)));
	else
	    LLOG (log_dsap,LLOG_EXCEPTIONS,( "Association rejected: [%s]",
	      AcErrString(acc->acc_result)));

	dc->dc_result = DS_REJECT;
	return (NOTOK);

    } /* switch acc->acc_result */

    return(OK);
}

