/* conn_init.c - deal with incoming association requests */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/conn_init.c,v 7.6 91/02/22 09:38:30 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/conn_init.c,v 7.6 91/02/22 09:38:30 mrose Interim $
 *
 *
 * $Log:	conn_init.c,v $
 * Revision 7.6  91/02/22  09:38:30  mrose
 * Interim 6.8
 * 
 * Revision 7.5  90/10/17  11:53:23  mrose
 * sync
 * 
 * Revision 7.4  90/07/09  14:45:24  mrose
 * sync
 * 
 * Revision 7.3  90/03/15  11:18:42  mrose
 * quipu-sync
 * 
 * Revision 7.2  89/12/19  16:20:03  mrose
 * sync
 * 
 * Revision 7.1  89/11/24  16:21:53  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:16:46  mrose
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


#include "quipu/dsap.h"
#include "tsap.h"
#include "quipu/util.h"
#include "quipu/connection.h"

extern LLog * log_dsap;
#ifndef NO_STATS
extern LLog * log_stat;
#endif
 extern  struct PSAPaddr		* dsaladdr;

 void	  ds_log();

 struct connection	* conn_alloc();
 void    acs_log();

 extern  time_t timenow;

 conn_init(cn)
 struct connection	* cn;
 {
     int				  result;
     char			**vec;
     struct DSAPstart		* ds;
     struct DSAPindication	  di_s;
     struct DSAPindication	* di = &di_s;
     struct DSAPabort		* da = &(di->di_abort);
     struct TSAPdisconnect	  td_s;
     struct TSAPdisconnect	* td = &td_s;

     DLOG (log_dsap,LLOG_TRACE,( "conn_init()"));

     vec = cn->cn_start.cs_vec;
     ds = &(cn->cn_start.cs_ds);

     result = DBindInit (cn->cn_start.cs_vecp, vec, ds, di);

     if (result != OK)
     {
	 LLOG (log_dsap,LLOG_EXCEPTIONS,( "conn_init(): DBindInit() != OK"));
	 ds_log(da, "initialization fails");
	 conn_extract (cn);
	 return;
     }

     if (conns_used >= MAX_CONNS) {
	 LLOG (log_dsap,LLOG_EXCEPTIONS,( "Too many connections"));
         (void) DBindReject (ds, ACS_PERMANENT, ACS_USER_NOREASON, di);
	 conn_extract (cn);
	 return;
     }

     if ( (ds->ds_start.acs_start.ps_called.pa_selectlen != 
	                   dsaladdr->pa_selectlen) ||
	  (dsaladdr->pa_selectlen && bcmp (
	   ds->ds_start.acs_start.ps_called.pa_selector,
           dsaladdr->pa_selector,
	   dsaladdr->pa_selectlen) == 0)) {
	
out:;	  
        LLOG (log_dsap,LLOG_EXCEPTIONS, ("Bad Selector (%d): %s",
	   ds->ds_sd, paddr2str(&(ds->ds_start.acs_start.ps_calling),NULLNA)));

        (void) DBindReject (ds, ACS_PERMANENT, ACS_USER_NOREASON, di);
	conn_extract (cn);
	return;
	  }
     if ( (ds->ds_start.acs_start.ps_called.pa_addr.sa_selectlen != 
	                   dsaladdr->pa_addr.sa_selectlen) ||
	  (dsaladdr->pa_addr.sa_selectlen && bcmp (
	   ds->ds_start.acs_start.ps_called.pa_addr.sa_selector,
           dsaladdr->pa_addr.sa_selector,
	   dsaladdr->pa_addr.sa_selectlen) == 0)) 
	goto out;

     /* tsels are checked in the lower layers - just check the NULL ones */
     if ( (dsaladdr->pa_addr.sa_addr.ta_selectlen == 0) && 
	 ( ds->ds_start.acs_start.ps_called.pa_addr.sa_addr.ta_selectlen != 0))
	goto out;

    DLOG (log_dsap,LLOG_TRACE,( "conn_init(): DBindInit() OK"));

    /*
    * Log the arrival of a connection request.
    */
    LLOG (log_dsap,LLOG_NOTICE, ("Association (%d) from %s",
	   ds->ds_sd, paddr2str(&(ds->ds_start.acs_start.ps_calling),NULLNA)));

    DLOG (log_dsap,LLOG_NOTICE, ("Context: %s; Caller: %s; Callee: %s.",
		oid2ode(ds->ds_start.acs_context),
		sprintaei (&ds->ds_start.acs_callingtitle),
		sprintaei (&ds->ds_start.acs_calledtitle)));

    cn->cn_ad = ds->ds_sd;
    cn->cn_initiator = FALSE;
    cn->cn_ctx = ds->ds_ctx;

#ifndef NO_STATS
    switch (cn->cn_ctx)
    {
	case DS_CTX_X500_DAP:
	    LLOG (log_stat, LLOG_NOTICE, ("X500 DAP context association (%d): %s", cn->cn_ad, paddr2str (&(ds->ds_start.acs_start.ps_calling), NULLNA)));
	    break;
	case DS_CTX_X500_DSP:
	    LLOG (log_stat, LLOG_NOTICE, ("X500 DSP context association (%d): %s", cn->cn_ad, paddr2str (&(ds->ds_start.acs_start.ps_calling), NULLNA)));
	    break;
	case DS_CTX_QUIPU_DSP:
	    LLOG (log_stat, LLOG_NOTICE, ("QUIPU DSP context association (%d): %s", cn->cn_ad, paddr2str (&(ds->ds_start.acs_start.ps_calling), NULLNA)));
	    break;
	default :
	    LLOG (log_stat, LLOG_EXCEPTIONS, ("UNKNOWN context association (%d): %s", cn->cn_ad, paddr2str (&(ds->ds_start.acs_start.ps_calling), NULLNA)));
	    break;
    }
#endif

    cn->cn_dn = dn_cpy(ds->ds_bind_arg.dba_dn);

    /*
    *  If we haven't returned yet then the protocol for binding has been
    *  satisfactorily completed.
    *  Now attempt to perform the ds_bind for the argument decoded, which
    *  can either succeed, fail or suspend waiting for a remote compare.
    *  If a remote compare has been scheduled then return, otherwise
    *  complete the connection initialisation by sending a bind result.
    */

    if (TSetQueuesOK (cn->cn_ad, 1, td) == NOTOK) 
	td_log (td, "TSetQueuesOK (incoming)");

    switch(ds_bind_init(cn))
    {
    case DS_OK:
	conn_init_res(cn);
	break;
    case DS_ERROR_CONNECT:
	conn_init_err(cn);
	break;
    case DS_CONTINUE:
	cn->cn_state = CN_INDICATED;
	break;
    default:
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("Unexpected return by ds_bind_init"));
        (void) DBindReject (ds, ACS_TRANSIENT, ACS_USER_NOREASON, di);
	break;
    }
}

conn_init_res(cn)
struct connection       * cn;
{
    int				  result;
    struct DSAPindication      di_s;
    struct DSAPindication      *di = &(di_s);
    struct DSAPabort           *da = &(di->di_abort);
    struct DSAPstart           * ds = &(cn->cn_start.cs_ds);
    struct AcSAPstart           * acs = &(ds->ds_start);
    struct PSAPstart            * ps = &(acs->acs_start);

    DLOG(log_dsap,LLOG_TRACE, ("conn_init_res()"));

    DLOG(log_dsap,LLOG_NOTICE, ("D-BIND.RESULT"));

#ifdef DEBUG
    {
	int	  i;

	for(i=0; i<ps->ps_ctxlist.pc_nctx; i++)
	{
	    DLOG(log_dsap, LLOG_DEBUG, ("ctx[%d] id = %d, res = %d.", i,
		ps->ps_ctxlist.pc_ctx[i].pc_id,
		ps->ps_ctxlist.pc_ctx[i].pc_result));
	}
    }
#endif

    result = DBindResult (cn->cn_ad, acs->acs_context, NULLAEI, NULLPA,
	&(ps->ps_ctxlist), ps->ps_defctxresult, PR_MYREQUIRE,
	ps->ps_srequirements & (ROS_MYREQUIRE | SR_NEGOTIATED),
	SERIAL_NONE, ps->ps_settings, &(ps->ps_connect),
	&(cn->cn_start.cs_res), ds->ds_pctx_id, di);

    if (result == OK)
    {
	cn->cn_state = CN_OPEN;
    }
    else
    {
	ds_log(da, "D-BIND.RESULT");
	DLOG(log_dsap, LLOG_DEBUG, ("conn_init_res(): DBindResult failed, extracting conn"));
	conn_extract(cn);
    }

    ACSFREE (acs);
}

conn_init_err(cn)
struct connection       * cn;
{
    int				  result;
    struct DSAPindication      di_s;
    struct DSAPindication      *di = &di_s;
    struct DSAPabort           *da = &di->di_abort;
    struct DSAPstart           * ds = &(cn->cn_start.cs_ds);
    struct AcSAPstart           * acs = &(ds->ds_start);
    struct PSAPstart            * ps = &(acs->acs_start);

    DLOG(log_dsap,LLOG_TRACE, ("conn_init_err()"));

    DLOG(log_dsap,LLOG_NOTICE, ("D-BIND.ERROR"));

#ifdef DEBUG
    {
	int	  i;

	for(i=0; i<ps->ps_ctxlist.pc_nctx; i++)
	{
	    DLOG(log_dsap, LLOG_DEBUG, ("ctx[%d] id = %d, res = %d.", i,
		ps->ps_ctxlist.pc_ctx[i].pc_id,
		ps->ps_ctxlist.pc_ctx[i].pc_result));
	}
    }
#endif

    result = DBindError (cn->cn_ad, acs->acs_context, NULLAEI, NULLPA,
	&(ps->ps_ctxlist), ps->ps_defctxresult, PR_MYREQUIRE,
	ps->ps_srequirements & (ROS_MYREQUIRE | SR_NEGOTIATED),
	SERIAL_NONE, ps->ps_settings, &(ps->ps_connect),
	&(cn->cn_start.cs_err), ds->ds_pctx_id, di);

    if (result == OK)
    {
	    DLOG(log_dsap,LLOG_TRACE, ("status != ACS_ACCEPT"));
	    DLOG(log_dsap, LLOG_DEBUG, ("conn_init_err(): DBindError succeeded, extracting conn"));
	    conn_extract(cn);
    }
    else
    {
	ds_log(da, "D-BIND.ERROR");
	DLOG(log_dsap, LLOG_DEBUG, ("conn_init_err(): DBindError failed, extracting conn"));
	conn_extract(cn);
    }

}

conn_pre_init(newfd, vecp, vec)
int newfd;
int	  vecp;
char	**vec;
{
    struct connection	* cn;

    cn = conn_alloc();

    cn->cn_next = connlist;
    connlist = cn;
    conns_used++;

    cn->cn_ad = newfd;
    cn->cn_initiator = FALSE;

    cn->cn_start.cs_vecp = vecp;
    if (vec[0])
	cn->cn_start.cs_svec[0] = cn->cn_start.cs_vec[0] = strdup (vec[0]);
    if (vec[1])
	cn->cn_start.cs_svec[1] = cn->cn_start.cs_vec[1] = strdup (vec[1]);
    if (vec[2])
	cn->cn_start.cs_svec[2] = cn->cn_start.cs_vec[2] = strdup (vec[2]);
    if (vec[3])
	cn->cn_start.cs_svec[3] = cn->cn_start.cs_vec[3] = strdup (vec[3]);

    cn->cn_state = CN_OPENING;

    cn->cn_last_used = timenow;

    if (newfd == NOTOK)
	conn_init (cn);
    else 
	DLOG (log_dsap,LLOG_NOTICE, ("opening association on %d",newfd ));
}
