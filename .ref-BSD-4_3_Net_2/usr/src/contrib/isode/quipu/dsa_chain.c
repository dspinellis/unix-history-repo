/* dsa_chain.c - take referral and chain if allowed */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/dsa_chain.c,v 7.5 91/02/22 09:39:06 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/dsa_chain.c,v 7.5 91/02/22 09:39:06 mrose Interim $
 *
 *
 * $Log:	dsa_chain.c,v $
 * Revision 7.5  91/02/22  09:39:06  mrose
 * Interim 6.8
 * 
 * Revision 7.4  90/10/17  11:54:03  mrose
 * sync
 * 
 * Revision 7.3  90/07/09  14:45:58  mrose
 * sync
 * 
 * Revision 7.2  89/12/19  16:20:26  mrose
 * sync
 * 
 * Revision 7.1  89/11/27  10:30:12  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:17:21  mrose
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


#include "acsap.h"
#include "quipu/util.h"
#include "quipu/connection.h"

extern LLog * log_dsap;

extern int dn_print ();
extern char	* mydsaname;
extern DN	  mydsadn;
extern int	  no_dsp_chain;
extern AttributeType at_relaydsa;
extern int dn_print ();

struct oper_act	* task2oper();
struct di_block	* di_alloc();
struct di_block	* select_refer_dsa();
struct connection	* conn_alloc();
struct oper_act * oper_alloc();
struct PSAPaddr	* psap_cpy();
struct access_point *ap_cpy ();
extern Entry local_find_entry_aux();

struct connection	* make_conn_block(name, addr, conn_ctx)
DN			  name;
struct PSAPaddr		* addr;
char			  conn_ctx;
{
    struct connection	* cn;

    struct TSAPaddr *tb;
    struct TSAPaddr *ta;
    struct NSAPaddr *na;
    struct NSAPaddr *nb;
    extern struct PSAPaddr * mydsaaddr;
    int x,y;
    char onnet = FALSE;

    /*
    * Set up a new connection block and add it to the list.
    */

    if(dn_cmp(name, mydsadn) == 0)
    {
	LLOG(log_dsap, LLOG_FATAL, ("Trying to connect to self :-)"));
	return(NULLCONN);
    }

    if (! addr) {
        pslog (log_dsap,LLOG_EXCEPTIONS,"Invalid (accesspoint) reference",dn_print,(caddr_t)name);
	return(NULLCONN);
    }

    /* see if on the appropriate net */
    ta = & (addr->pa_addr.sa_addr);
    tb = & (mydsaaddr->pa_addr.sa_addr);

    /* compare ta and tb to see if they have a network in common */
    for (na=ta->ta_addrs , x = ta->ta_naddr - 1 ;
		x >= 0;
		na++, x-- ) {
	for (nb=tb->ta_addrs , y = tb->ta_naddr - 1 ;
			y >= 0;
			nb++, y-- ) {
		if (na->na_community == nb->na_community) {
			onnet = TRUE;
			break;
		}
	}
    }

    if (! onnet) {
	LLOG(log_dsap, LLOG_NOTICE, ("make_conn_block - no network in common"));
	return(NULLCONN);
    }
    
    if((cn = conn_alloc()) == NULLCONN)
    {
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("make_conn_block - conn_alloc() out of memory"));
	return(NULLCONN);
    }
    cn->cn_state = CN_WAITING;
    cn->cn_ctx = conn_ctx;
    cn->cn_initiator = TRUE;
    make_dsa_bind_arg(&(cn->cn_connect.cc_req));

    cn->cn_dn = dn_cpy(name);
    DLOG (log_dsap,LLOG_TRACE,( "Before psap_dup: %s", paddr2str(addr,NULLNA)));
    psap_dup(&(cn->cn_addr), addr);
    DLOG (log_dsap,LLOG_TRACE,( "After psap_dup:  %s", paddr2str(&(cn->cn_addr),NULLNA)));

    return(cn);
}

int	  link_op_to_conn(on)
struct oper_act	* on;
{
    char		  conn_ctx = DS_CTX_X500_DSP;
    struct di_block	* di;
    struct di_block	**next_di;
    struct connection	* cn;
    int			  do_conn;
    struct access_point * loop_ap;
    int 		res;

    sort_dsa_list (&on->on_dsas);

    /*
    *  Use an open connection if one is available.
    */

    next_di = &(on->on_dsas);
    for(di=on->on_dsas; di!=NULL_DI_BLOCK; di=di->di_next)
    {
	for(cn=connlist; cn!=NULLCONN; cn=cn->cn_next)
	{
	    /* Must be a suitable context */
	    if(cn->cn_ctx == DS_CTX_X500_DAP)
	    {
		LLOG(log_dsap, LLOG_TRACE, ("link_op_to_conn - open conn has DAP context"));
		continue;
	    }

	    if((cn->cn_ctx == DS_CTX_X500_DSP)
		&& ((on->on_type == ON_TYPE_GET_EDB)
		    || (on->on_type == ON_TYPE_GET_DSA_INFO)
		    || (!cn->cn_initiator)))
	    {
		if (!cn->cn_initiator)
			LLOG(log_dsap, LLOG_TRACE, ("link_op_to_conn - open conn has DSP context - we must initiate it"));
		else
			LLOG(log_dsap, LLOG_TRACE, ("link_op_to_conn - open conn has DSP context - QUIPU context needed"));
		continue;
	    }

	    if((cn->cn_dn != NULLDN)
	      && (dn_cmp(cn->cn_dn, di->di_dn) == 0))
		break;

	}
	if(cn != NULLCONN)
	    break;

	next_di = &(di->di_next);
    }
    if(di != NULL_DI_BLOCK)
    {
	/* Got one - remove successful di_block and link op to conn */
	DLOG(log_dsap, LLOG_TRACE, ("link_op_to_conn - Found suitable open connection"));
	(*next_di) = di->di_next;
	if (di->di_state == DI_DEFERRED) {
		/* We have an open connection, but not a cache entry */
		/*		(must have used an access point in the past) */
		/* Need to be careful about freeing - do it later ! */
		di->di_oper = NULLOPER;
	} else
		di_extract(di);
	on->on_conn = cn;
	on->on_next_conn = cn->cn_operlist;
	cn->cn_operlist = on;
	on->on_relay = FALSE;	/* No need we have an open connection */
	return(OK);
    }

    /*
    *  Use a waiting connection if one is available.
    */
    next_di = &(on->on_dsas);
    for(di=on->on_dsas; di!=NULL_DI_BLOCK; di=di->di_next)
    {
	for(cn=connwaitlist; cn!=NULLCONN; cn=cn->cn_next)
	{
	/*
	*  Could do some clever stuff here and convert a waiting 
	*  connection to QUIPU from X500 if possible and useful.
	*  Left as an exercise for the reader.
	*/
	    /* Must be a suitable context */
	    if(cn->cn_ctx == DS_CTX_X500_DAP)
		continue;

	    if((cn->cn_ctx == DS_CTX_X500_DSP)
		&& ((on->on_type == ON_TYPE_GET_EDB)
		    || (on->on_type == ON_TYPE_GET_DSA_INFO)))
		continue;

	    if((cn->cn_dn != NULLDN)
	      && (dn_cmp(cn->cn_dn, di->di_dn) == 0))
		break;
	}
	if(cn != NULLCONN)
	    break;

	next_di = &(di->di_next);
    }
    if(di != NULL_DI_BLOCK)
    {
	/* Got one - remove successful di_block and link op to conn */
	LLOG(log_dsap, LLOG_TRACE, ("link_op_to_conn - Found suitable waiting connection"));
	(*next_di) = di->di_next;
	di_extract(di);
	on->on_conn = cn;
	on->on_next_conn = cn->cn_operlist;
	cn->cn_operlist = on;
	on->on_relay = FALSE;	/* No need we will chain sooner or later */
	return(OK);
    }

    DLOG(log_dsap, LLOG_DEBUG, ("Neither an open nor a waiting conn suitable"));

    next_di = &(on->on_dsas);
    for(di=on->on_dsas; di!=NULL_DI_BLOCK; di=(*next_di))
    {
	if(di->di_state == DI_DEFERRED)
	{
	    DLOG(log_dsap, LLOG_TRACE, ("link_op_to_conn - deferred di_block"));
	    next_di = &(di->di_next);
	    continue;
	}

	if(di->di_state == DI_ACCESSPOINT)
	{
	    /* context problem:
		if we have not got the entry, we don't know which context it
		will accept.
		If the operation is a getedb, or getdsainfo 
			ASSUME Quipu context is OK
	    */
		
	    if((on->on_type == ON_TYPE_GET_EDB)
	          || (on->on_type == ON_TYPE_GET_DSA_INFO)) 
		conn_ctx = DS_CTX_QUIPU_DSP;
	    else {
		conn_ctx = DS_CTX_X500_DSP;
	    }

	    DLOG(log_dsap, LLOG_TRACE, ("link_op_to_conn - make conn block from access point"));

	    /* There *should* only be one access point - 
	     * but QUIPU may give a choice try find one that will work...
             * This is *wrong* if it is a non specific subordinate reference.
             */
	    if (di->di_reftype == RT_NONSPECIFICSUBORDINATE) {
		    LLOG(log_dsap,LLOG_EXCEPTIONS,("Should try each access point - not just one !!!"));
		    /* throw back to user */
		    (*next_di) = di->di_next;
		    di_extract(di);
		    continue;
	    }

	    for (loop_ap=di->di_accesspoints; loop_ap != NULLACCESSPOINT; loop_ap=loop_ap->ap_next)
		    if((cn = make_conn_block(loop_ap->ap_name, loop_ap->ap_address, conn_ctx)) != NULLCONN) {
			on->on_relay = FALSE;	/* Made a connection block */
			break;
		    }

	    if (loop_ap == NULLACCESSPOINT) {
		DLOG(log_dsap, LLOG_DEBUG, ("link_op_to_conn - make_conn_block failed 1"));
		(*next_di) = di->di_next;
		di_extract(di);
		continue;
	    }
	}

	if(di->di_state == DI_COMPLETE)
	{
	    /*
	    *  Open a quipu context connection if possible: this is so if
	    *  the entry for the dsa in question has object class quipuDSA.
	    */
	    if((res = quipu_ctx_supported(di->di_entry)) != 2)
	    {
		if((on->on_type == ON_TYPE_GET_EDB)
	          || (on->on_type == ON_TYPE_GET_DSA_INFO)
		  || (res == -1)) /* DAP only !!! */
		{
		    /* Ditch this di_block and carry on looking */
		    LLOG(log_dsap, LLOG_NOTICE, ("link_op_to_conn - avoiding non-quipu context for GetEDB"));
		    (*next_di) = di->di_next;
		    di_extract(di);
		    continue;
		}
		else
		{
		    DLOG(log_dsap, LLOG_DEBUG, ("link_op_to_conn - linking to a connection without a quipu context"));
		    conn_ctx = DS_CTX_X500_DSP;
		}
	    }
	    else
	    {
		DLOG(log_dsap, LLOG_DEBUG, ("link_op_to_conn - linking to a connection with a quipu context"));
		conn_ctx = DS_CTX_QUIPU_DSP;
	    }


	    DLOG(log_dsap, LLOG_TRACE, ("link_op_to_conn - make conn block from entry"));

            if((cn = make_conn_block(di->di_dn, di->di_entry->e_dsainfo->dsa_addr, conn_ctx)) == NULLCONN)
	    {
		DLOG(log_dsap, LLOG_DEBUG, ("link_op_to_conn - make_conn_block failed 2"));
		(*next_di) = di->di_next;
		di_extract(di);
		continue;
	    } else
		on->on_relay = FALSE;	/* Made a connection block */

	}

	/*
	*  Decide whether to request connection or place it
	*  on the list of waiting connections.
	*/
	switch(on->on_type)
	{
	case ON_TYPE_GET_DSA_INFO:
	    do_conn = (conns_used < MAX_CONNS);
	    break;
	case ON_TYPE_GET_EDB:
	case ON_TYPE_SHADOW:
	    do_conn = (conns_used < (MAX_CONNS - CONNS_RESERVED_DI - CONNS_RESERVED_X500));
	    break;
	default:
	    do_conn = (conns_used < (MAX_CONNS - CONNS_RESERVED_DI));
	    break;
	}

	if(do_conn)
	{
	    DLOG(log_dsap, LLOG_TRACE, ("link_op_to_conn - about to request connection"));
	    if(conn_request(cn) != OK)
	    {
		DLOG(log_dsap, LLOG_DEBUG, ("link_op_to_conn - conn_request failed"));
		(*next_di) = di->di_next;
		di_extract(di);
		continue;
	    }
	    DLOG(log_dsap, LLOG_DEBUG, ("link_op_to_conn - conn_request OK"));

	    cn->cn_next = connlist;
	    connlist = cn;
	    conns_used++;
	}
	else
	{
	    DLOG(log_dsap, LLOG_NOTICE, ("Waiting for a free connection slot"));
	    cn->cn_next = connwaitlist;
	    connwaitlist = cn;
	}

	(*next_di) = di->di_next;
	di_extract(di);
	on->on_conn = cn;
	on->on_next_conn = cn->cn_operlist;
	cn->cn_operlist = on;
	return(OK);
    }

    /*
    *  If we get this far it means that we are waiting for a dsa info
    *  operation to complete, or there are no di_blocks left to try.
    *  Callers of link_op_to_conn must check on_dsas to discover which it is.
    */
    DLOG(log_dsap, LLOG_NOTICE, ("link_op_to_conn: returning NOTOK"));
    return(NOTOK);
}

int	  oper_chain(on)
struct oper_act		* on;
{
    if(link_op_to_conn(on) == OK)
    {
	if(on->on_conn == NULLCONN)
	{
	    DLOG(log_dsap, LLOG_DEBUG, ("oper_chain - link_op_to_conn: OK but no conn"));
	}
	else
	{
	    DLOG(log_dsap, LLOG_DEBUG, ("oper_chain - link_op_to_conn: OK got conn"));

	    if(on->on_conn->cn_state == CN_OPEN)
	    {
		DLOG(log_dsap, LLOG_DEBUG, ("oper_chain - link_op_to_conn: OK got open conn"));
		if(oper_send_invoke(on) != OK)
		{
		    LLOG(log_dsap, LLOG_EXCEPTIONS, ("oper_chain - oper_send failed"));
		    /* Have another go? */
		    return(oper_chain(on));
		}
		else
		{ 
		    DLOG(log_dsap, LLOG_DEBUG, ("oper_chain - oper_send succeeded"));
		}
	    }
	}

	on->on_state = ON_CHAINED;
    }
    else
    {
	/*
	*  If on->on_dsas is empty then chaining has failed
	*  otherwise the op is deferred.
	*/
	if(on->on_dsas == NULL_DI_BLOCK)
	{
	    if (relay_dsa (on) == NOTOK)
		return NOTOK;

	    LLOG(log_dsap, LLOG_NOTICE, ("Trying to relay..."));
	    return(oper_chain(on));
	}
	
	on->on_state = ON_DEFERRED;
    }

    return(OK);
}

int	  task_chain(tk, di)
register       struct task_act     * tk;
struct di_block	* di;
{
    struct oper_act	* on;
    struct DSError	* err = &(tk->tk_resp.di_error.de_err);
    struct di_block	* di_tmp;
    char		refer_ok = TRUE;

#ifdef DEBUG
    DLOG(log_dsap, LLOG_DEBUG, ("task_chain called with:"));
    di_list_log(di);
#endif

/* NB At some point this routine must assign the di_block list to
* either the task (if it is intended to geneate a referral) or to
* an operation hanging off that task if it is intended to chain the
* task. This is fine when there are no deferred di_blocks, but when
* there are then the information they will eventually contain is
* needed to make a full decision on whether to chain or refer.
* This needs a lot of thought to get right, for now the chain/refer
* decision is made once and for all on the basis of the information
* available now. Any information not available is assumed to force a
* referral (the safe option  -  until network connectivity is considered)!
* THis may introduce the unwelcome effect that a first request to a
* DSA may produce a referral where subsequent requests do not - so much
* for consistency but it won't happen that often if DSA info is cached
* sensibly.
*/

    /*
    *  Generate the referral which the DSA will pass back if
    *  chaining is disallowed or oper_chain fails for all
    *  DSAs listed.
    */

    sort_dsa_list (&di);

    if ((di_tmp = select_refer_dsa (di,tk)) == NULL_DI_BLOCK) {
	/* The remote END is probably unable to follow the referral - chain if allowed */
	refer_ok = FALSE;
	for(di_tmp=di; di_tmp!=NULL_DI_BLOCK; di_tmp=di_tmp->di_next)
    	{
		if(di_tmp->di_state == DI_DEFERRED)
			continue;

#ifdef DEBUG
		DLOG(log_dsap, LLOG_DEBUG, ("About to call di2cref with:"));
		di_log(di_tmp);
#endif
		if(di2cref(di_tmp, err, tk->tk_conn->cn_ctx) == OK)
		    break;
	}
    } else if (di2cref(di_tmp, err, tk->tk_conn->cn_ctx) != OK)
		di_tmp = NULL_DI_BLOCK;	/* waiting... */

    if(di_tmp == NULL_DI_BLOCK)
    {
	/*
	*  Want to generate a referral - but all di_blocks (if any)
	*  are deferred. Would we be lying too much if we said the
	*  DSA was "busy" at this point???
	*/
	ds_error_free (err);
	err->dse_type = DSE_SERVICEERROR;
	err->ERR_SERVICE.DSE_sv_problem = DSE_SV_BUSY;
	di_desist(di);
	return(NOTOK);
    }

    /*
    *  If it would be inappropriate to chain this operation, then
    *  generate a referral from the di_block list.
    */

    if(chain_ok(tk,refer_ok,di_tmp->di_dn) == FALSE)
    {
	DLOG(log_dsap, LLOG_DEBUG, ("Referring!"));
	di_desist(di);
	return(NOTOK);
    }

    DLOG(log_dsap, LLOG_DEBUG, ("Chaining!"));
    /* Chain. Generate the new operation to send */
    if((on = task2oper(tk)) == NULLOPER)
    {
	DLOG(log_dsap, LLOG_DEBUG, ("Why did task2oper fail??"));
	ds_error_free (err);
	err->dse_type = DSE_SERVICEERROR;
	err->ERR_SERVICE.DSE_sv_problem = DSE_SV_UNAVAILABLE;
	di_desist(di);
	return(NOTOK);
    }

    if(ti_is_elem(tk->tk_dx.dx_arg.dca_charg.cha_trace,
		  tk->tk_dx.dx_arg.dca_charg.cha_trace->ti_next))
	{
	    DLOG (log_dsap,LLOG_NOTICE,("Loop found in oper_chain()"));
	    ds_error_free (&on->on_resp.di_error.de_err);
	    on->on_resp.di_error.de_err.dse_type = DSE_SERVICEERROR;
	    on->on_resp.di_error.de_err.ERR_SERVICE.DSE_sv_problem = DSE_SV_LOOPDETECT;
	    return(NOTOK);
	}

    on->on_next_task = tk->tk_operlist;
    tk->tk_operlist = on;
    on->on_task = tk;

    /* Hand control of di_blocks to the operation */
    on->on_dsas = di;
    for(di_tmp = di; di_tmp != NULL_DI_BLOCK; di_tmp=di_tmp->di_next)
    {
	di_tmp->di_type = DI_OPERATION;
	di_tmp->di_oper = on;
    }

    if(oper_chain(on) != OK)
    {
	oper_task_extract(on);
	oper_free(on);
	return(NOTOK);
    }

    return(OK);
}

oper_rechain(on)
struct oper_act * on;
{
    struct DSE_referral         * ref = &(on->on_resp.di_error.de_err.ERR_REFERRAL);
    struct continuation_ref     * cref;
    register struct chain_arg	* cha = &(on->on_req.dca_charg);
    struct trace_info		* ti;
    struct di_block * ap2di();

    DLOG(log_dsap, LLOG_TRACE, ("Rechain an operation ..."));
    cref = ref->DSE_ref_candidates;
    
    if(cref == NULLCONTINUATIONREF)
    {
	LLOG(log_dsap, LLOG_FATAL, ("No continuation reference to rechain"));
	return(NOTOK);
    }

    cha->cha_target = dn_cpy(cref->cr_name);
    cha->cha_progress = cref->cr_progress;
    cha->cha_aliasderef = ((cha->cha_aliasedrdns = cref->cr_aliasedRDNs) != CR_NOALIASEDRDNS);

    if (cha->cha_aliasderef) {
	if ((on->on_arg->dca_dsarg.arg_type == OP_SEARCH) 
		&& (on->on_arg->dca_dsarg.arg_sr.sra_subset == SRA_ONELEVEL))
#ifdef COMPAT_6_0
	{
		on->on_arg->dca_dsarg.arg_sr.sra_subset = SRA_BASEOBJECT;
		cha->cha_entryonly = FALSE;
	}
#else
		cha->cha_entryonly = TRUE;
#endif
	else
		cha->cha_entryonly = FALSE;
    }

    cha->cha_returnrefs = FALSE;
    cha->cha_domaininfo = NULLPE;
    if((cha->cha_reftype = cref->cr_reftype) == RT_UNDEFINED)
	cha->cha_reftype = RT_SUPERIOR;

    DLOG(log_dsap, LLOG_DEBUG, ("oper_rechain - Setting trace info"));
    ti = (struct trace_info *) malloc(sizeof(struct trace_info));
    ti->ti_dsa = dn_cpy(on->on_conn->cn_dn); 
    ti->ti_target = dn_cpy(cref->cr_name);
    ti->ti_progress = cref->cr_progress;
    ti->ti_next = cha->cha_trace;
    cha->cha_trace = ti;

    if(ti_is_elem(ti,ti->ti_next))
	{
	    DLOG (log_dsap,LLOG_NOTICE,("Loop found in oper_rechain()"));
	    ds_error_free (&on->on_resp.di_error.de_err);
	    on->on_resp.di_error.de_err.dse_type = DSE_SERVICEERROR;
	    on->on_resp.di_error.de_err.ERR_SERVICE.DSE_sv_problem = DSE_SV_LOOPDETECT;
	    return(NOTOK);
	}

    oper_conn_extract(on);

    /*
    *  Problem - having converted to di_blocks it is harder to handle referrals
    *  Set up a single di_block with the address in the parent field ??
    */
    di_desist(on->on_dsas);
    if (cref->cr_reftype != RT_NONSPECIFICSUBORDINATE)
	    on->on_dsas = ap2di (cref->cr_accesspoints,cref->cr_name,FALSE,DI_OPERATION,on,cref->cr_reftype);
    else {
	    on->on_dsas = di_alloc();
	    on->on_dsas->di_target = dn_cpy(cref->cr_name);
	    on->on_dsas->di_dn = dn_cpy(cref->cr_accesspoints->ap_name);
	    DLOG(log_dsap, LLOG_DEBUG, ("oper_rechain allocates di_block with dn[%x]", on->on_dsas->di_dn));
	    on->on_dsas->di_type = DI_OPERATION;
	    on->on_dsas->di_reftype = RT_NONSPECIFICSUBORDINATE;
	    on->on_dsas->di_oper = on;
	    on->on_dsas->di_state = DI_ACCESSPOINT;
	    on->on_dsas->di_accesspoints = ap_cpy(cref->cr_accesspoints);
	    on->on_dsas->di_next = NULL_DI_BLOCK;
    }

    sort_dsa_list (&on->on_dsas);	/* might be able to turn DI_ACCESS into DI_COMPLETE */

    if (on->on_relay == FALSE) 		/* but not 2 ! */
	    on->on_relay = TRUE;	/* allow relay for new DSA set */

    return(oper_chain(on));
}

struct oper_act	* task2oper(tk)
struct task_act * tk;
{
    register struct chain_arg	* cha = &(tk->tk_dx.dx_arg.dca_charg);
    struct continuation_ref	* cref = tk->tk_resp.di_error.de_err.ERR_REFERRAL.DSE_ref_candidates;
    struct trace_info		* ti;
    struct oper_act		* on;

    DLOG(log_dsap, LLOG_TRACE, ("Chain a task ..."));
    
    if((on = oper_alloc()) == NULLOPER)
	return(NULLOPER);

    on->on_type = ON_TYPE_X500;

    cha->cha_target = NULLDN;
    if(cref->cr_name != NULLDN)
    {
	    cha->cha_target = dn_cpy(cref->cr_name);
    }
    cha->cha_progress = cref->cr_progress;
    cha->cha_aliasderef = ((cha->cha_aliasedrdns = cref->cr_aliasedRDNs) != CR_NOALIASEDRDNS);

    if (cha->cha_aliasderef) {
	if ((on->on_arg->dca_dsarg.arg_type == OP_SEARCH) 
		&& (on->on_arg->dca_dsarg.arg_sr.sra_subset == SRA_ONELEVEL)) {
#ifdef COMPAT_6_0
	{
		on->on_arg->dca_dsarg.arg_sr.sra_subset = SRA_BASEOBJECT;
		cha->cha_entryonly = FALSE;
	}
#else
		cha->cha_entryonly = TRUE;
#endif
	} else
		cha->cha_entryonly = FALSE;
    }

    cha->cha_returnrefs = FALSE;
    cha->cha_domaininfo = NULLPE;
    if((cha->cha_reftype = cref->cr_reftype) == RT_UNDEFINED)
	cha->cha_reftype = RT_SUPERIOR;

    DLOG(log_dsap, LLOG_DEBUG, ("Checking history of op"));
    if(tk->tk_conn->cn_ctx == DS_CTX_X500_DAP)
    {
	DLOG(log_dsap, LLOG_DEBUG, ("... user originated ..."));
	cha->cha_originator = dn_cpy(tk->tk_conn->cn_dn);
	cha->cha_trace = NULLTRACEINFO;
    }

    if(tk->tk_timed == FALSE)
    {
	cha->cha_timelimit = NULLCP;
    }
    else
    {
#ifdef CHAIN_ARGS_TIMEOUT
	struct UTCtime	ut;
	tm2ut(gmtime(&(tk->tk_timeout)), &(ut));
	cha->cha_timelimit = strdup(utct2str(&ut));
#else
	cha->cha_timelimit = NULLCP;
#endif
    }

    DLOG(log_dsap, LLOG_DEBUG, ("Setting trace info"));
    ti = (struct trace_info *) malloc(sizeof(struct trace_info));
    ti->ti_dsa = dn_cpy(mydsadn);
    ti->ti_target = dn_cpy(cref->cr_name);
    ti->ti_progress = cref->cr_progress;
    ti->ti_next = cha->cha_trace;
    cha->cha_trace = ti;

    on->on_arg = &(tk->tk_dx.dx_arg);

    return(on);
}

int     chain_ok(tk,refer_ok,dsadn)
struct task_act	* tk;
char refer_ok;
DN dsadn;
{
    struct common_args	* ca;
struct common_args	* get_ca_ref();

    ca = get_ca_ref(&(tk->tk_dx.dx_arg));

    /* if refer_ok is FALSE - we MUST chain unless prevented, otherwise operation will fail */

    DLOG (log_dsap,LLOG_TRACE,( "chain_ok: Checking if chaining is ok"));

    if ( ! refer_ok) {
	DLOG (log_dsap,LLOG_DEBUG,( "We MUST chain"));	
	
	if ((tk->tk_conn->cn_ctx != DS_CTX_X500_DAP) && no_dsp_chain)
	{
		DLOG (log_dsap,LLOG_DEBUG,( "Not chaining because of NO_DSP_CHAIN"));
		return(FALSE);
	}
	if(ca->ca_servicecontrol.svc_options & SVC_OPT_CHAININGPROHIBIT)
	{
		DLOG (log_dsap,LLOG_DEBUG,( "But prohibited"));
		return(FALSE);
	}

	if(ca->ca_servicecontrol.svc_options & SVC_OPT_LOCALSCOPE)
	{
		DLOG (log_dsap,LLOG_DEBUG,( "But out of scope"));
		return(FALSE); 
	}
	
    	DLOG (log_dsap,LLOG_DEBUG,( "Forced chain OK!"));
	return TRUE;
    }

    if (tk->tk_conn->cn_ctx != DS_CTX_X500_DAP) {
	if (no_dsp_chain)
	{
		DLOG (log_dsap,LLOG_DEBUG,( "Not chaining because of NO_DSP_CHAIN (2)"));
		return(FALSE);
	}

	if(! (ca->ca_servicecontrol.svc_options & SVC_OPT_PREFERCHAIN))
	{
		/* Don't send a self reference back to a remote DSA - chain if possible */
		/* Should not need it, when self reference bug is fixed ! */
		if ((tk->tk_conn->cn_initiator)
			|| (dn_cmp (dsadn, tk->tk_conn->cn_dn) == NOTOK)) {
				DLOG (log_dsap,LLOG_DEBUG,( "Not chaining because of preference"));
				return(FALSE);
			}
	}
    }

    if(ca->ca_servicecontrol.svc_options & SVC_OPT_CHAININGPROHIBIT)
    {
	DLOG (log_dsap,LLOG_DEBUG,( "Not chaining because of prohibition"));
	return(FALSE);
    }

    if(ca->ca_servicecontrol.svc_options & SVC_OPT_LOCALSCOPE)
    {
	DLOG (log_dsap,LLOG_DEBUG,( "Not chaining because of scope"));
	return(FALSE); 
    }

    switch (tk->tk_dx.dx_arg.dca_dsarg.arg_type) {
	case OP_ADDENTRY:
	case OP_REMOVEENTRY:
	case OP_MODIFYRDN:
	case OP_MODIFYENTRY:
		/* QUIPU DSAs will only allow modification over DAP */
		if (!(ca->ca_servicecontrol.svc_options & SVC_OPT_PREFERCHAIN)) {
			DLOG (log_dsap,LLOG_DEBUG,( "Not chaining because of authentication"));
			return(FALSE); 
		}
	default:
		break;
    }

    DLOG (log_dsap,LLOG_DEBUG,( "Chain OK!"));
    return(TRUE);
}

task_result_wakeup(on)
struct oper_act	* on;
{
    struct task_act	* tk;

    DLOG(log_dsap, LLOG_TRACE, ("task_result_wakeup"));

    if((tk = on->on_task) == NULLTASK)
    {
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("Oper can't wake up (result)extracted task"));
	oper_extract(on);
    }
    else
    {
	/*
	*  Were waiting for a remote result and here it is.
	*  Attempt to tidy up and send result.
	*/
	ds_error_free (tk->tk_error);

	tk->tk_result = &(on->on_resp.di_result.dr_res);
	
	dsp_cache (&(tk->tk_dx.dx_arg.dca_dsarg),&(tk->tk_result->dcr_dsres),tk->tk_conn->cn_ctx, tk->tk_conn->cn_dn);

	task_conn_extract(tk);
	task_result(tk);
	oper_extract(on);
	task_extract(tk);
    }
}

task_error_wakeup(on)
struct oper_act	* on;
{
    struct task_act	* tk;

    DLOG(log_dsap, LLOG_TRACE, ("task_error_wakeup"));

    if((tk = on->on_task) == NULLTASK)
    {
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("Oper can't wake up (error) extracted task"));
	oper_extract(on);
    }
    else
    {
	/*
	*  Were waiting for a remote result and got a remote error.
	*  If it is a referral, then rechain the operation if appropriate
	*  otherwise return the error.
	*/
	if ( !  ((on->on_resp.di_error.de_err.dse_type == DSE_SECURITYERROR)
	  	&& (on->on_resp.di_error.de_err.ERR_SECURITY.DSE_sc_problem == DSE_SC_AUTHENTICATION))) {
		/* If is not an authenticaton error, swap errors */
		ds_error_free(&(tk->tk_resp.di_error.de_err));
		tk->tk_error = &(on->on_resp.di_error.de_err);
	}

	if((on->on_resp.di_error.de_err.dse_type == DSE_DSAREFERRAL)
  	|| (on->on_resp.di_error.de_err.dse_type == DSE_REFERRAL))
	{
	    DLOG(log_dsap, LLOG_DEBUG, ("Try rechaining"));
	    if(oper_rechain(on) == OK)
	    {
		DLOG(log_dsap, LLOG_DEBUG, ("Succeeded rechaining"));
		return;
	    }
	    DLOG(log_dsap, LLOG_DEBUG, ("Failed rechaining"));
	}

	task_conn_extract(tk);
	task_error(tk);
	oper_extract(on);
	task_extract(tk);
    }
}

task_fail_wakeup(on)
struct oper_act	* on;
{
    struct task_act	* tk;
    struct DSError	* err;

    DLOG(log_dsap, LLOG_TRACE, ("task_fail_wakeup"));

    if((tk = on->on_task) == NULLTASK)
    {
	if (on->on_state != ON_ABANDONED)
		LLOG(log_dsap, LLOG_EXCEPTIONS, ("task_fail_wakeup: no task"));
	oper_extract(on);
	return;
    }

    /*
    *  Were waiting for a remote result and got a remote failure.
    *  If it is a referral, then rechain the operation if appropriate
    *  otherwise return the error.
    */
    /*
    *  If the task does not have a suitable referral error set up
    *  then return serviceError invalid reference.
    */
    err = &(tk->tk_resp.di_error.de_err);
    if((err->dse_type != DSE_REFERRAL) && (err->dse_type != DSE_DSAREFERRAL))
    {
	err->dse_type = DSE_SERVICEERROR;
	err->ERR_SERVICE.DSE_sv_problem = DSE_SV_UNAVAILABLE;
    }
    task_conn_extract(tk);
    task_error(tk);
    oper_conn_extract(on);
    oper_task_extract(on);
    oper_extract(on);
    task_extract(tk);
}

task_dsa_info_wakeup(di)
struct di_block	* di;
{
    struct task_act	* tk = di->di_task;


    DLOG(log_dsap, LLOG_TRACE, ("task_dsa_info_wakeup"));

    /*
    * Were waiting for a reference to return.
    * Check if the reference now returned is acceptable.
    * If it is return a referral and unwrap everything,
    * otherwise try another di_block for the reference.
    */
    sort_dsa_list (&di);

    if (tk == NULLTASK)
	return;		/* already done it ! */

    if(di2cref(di, &(tk->tk_resp.di_error.de_err), tk->tk_conn->cn_ctx) != OK)
    {
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("task_dsa_info_wakeup - reference not acceptable"));
	/* Remove di_block which generated unwanted referral wait */
	if(tk->tk_dsas == NULL_DI_BLOCK)
	{
	    /* No more dsas from which to request info to form referral */
	    tk->tk_resp.di_error.de_err.dse_type = DSE_SERVICEERROR;
	    tk->tk_resp.di_error.de_err.ERR_SERVICE.DSE_sv_problem = DSE_SV_INVALIDREFERENCE;
	    task_conn_extract(tk);
	    task_error(tk);
	    task_extract(tk);
	    return;
	}
	return;
    }

    task_conn_extract(tk);
    task_error(tk);
    task_extract(tk);
}

static struct access_point * di2ap (di)
struct di_block * di;
{
struct access_point *ap;

    switch(di->di_state)
    {
    case DI_ACCESSPOINT:
	return (ap_cpy(di->di_accesspoints));
    case DI_COMPLETE:
	if(di->di_entry == NULLENTRY)
	{
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("di2ap - di_entry NULL"));
	    return NULLACCESSPOINT;
	}
	if(di->di_entry->e_dsainfo == NULL)
	{
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("di2ap - e_dsainfo NULL"));
	    return NULLACCESSPOINT;
	}
	if(di->di_entry->e_dsainfo->dsa_addr == NULLPA)
	{
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("di2ap - dsa_addr NULL"));
	    return NULLACCESSPOINT;
	}
	ap = (struct access_point *) calloc(1, sizeof(struct access_point));
	ap->ap_name = dn_cpy(di->di_dn);
	ap->ap_address = psap_cpy(di->di_entry->e_dsainfo->dsa_addr);
	return (ap);
    default:
	return NULLACCESSPOINT;
    }
	
}

int	di2cref(di, err, ctx)
struct di_block	* di;
struct DSError	* err;
char ctx;
{
    struct continuation_ref     * cref;
    struct di_block * loop;
    struct access_point *ap_append(), *ap;

#ifdef DEBUG
    DLOG(log_dsap, LLOG_TRACE, ("di2cref"));
    di_log(di);
#endif

    switch(di->di_state)
    {
    case DI_ACCESSPOINT:
        DLOG(log_dsap, LLOG_TRACE, ("di2cref - generating referrral from di_accesspoints"));

	/* Should check context */
	err->dse_type = DSE_REFERRAL;
	err->ERR_REFERRAL.DSE_ref_prefix = NULLDN;

        cref = err->ERR_REFERRAL.DSE_ref_candidates = (struct continuation_ref *) calloc(1, sizeof(struct continuation_ref));
	cref->cr_accesspoints = ap_cpy(di->di_accesspoints);
	cref->cr_name = dn_cpy(di->di_target);
	if((cref->cr_rdn_resolved = di->di_rdn_resolved) <= 0)
	{
	    cref->cr_progress.op_resolution_phase = OP_PHASE_NOTSTARTED;
	    cref->cr_progress.op_nextrdntoberesolved = 0;
	}
	else
	{
	    cref->cr_progress.op_resolution_phase = OP_PHASE_PROCEEDING;
	    cref->cr_progress.op_nextrdntoberesolved = di->di_rdn_resolved;
	}
	cref->cr_aliasedRDNs = di->di_aliasedRDNs;
	cref->cr_reftype = di->di_reftype;
	break;
    case DI_COMPLETE:
        DLOG(log_dsap, LLOG_TRACE, ("di2cref - generating referrral from di_entry"));

	/* Should check context */
	err->dse_type = DSE_REFERRAL;
	err->ERR_REFERRAL.DSE_ref_prefix = NULLDN;

        cref = err->ERR_REFERRAL.DSE_ref_candidates = (struct continuation_ref *) calloc(1, sizeof(struct continuation_ref));
	if ((cref->cr_accesspoints = di2ap (di)) == NULLACCESSPOINT)
		return NOTOK;
	cref->cr_name = dn_cpy(di->di_target);
	if((cref->cr_rdn_resolved = di->di_rdn_resolved) <= 0)
	{
	    cref->cr_progress.op_resolution_phase = OP_PHASE_NOTSTARTED;
	    cref->cr_progress.op_nextrdntoberesolved = 0;
	}
	else
	{
	    cref->cr_progress.op_resolution_phase = OP_PHASE_PROCEEDING;
	    cref->cr_progress.op_nextrdntoberesolved = di->di_rdn_resolved;
	}
	cref->cr_aliasedRDNs = di->di_aliasedRDNs;
	cref->cr_reftype = di->di_reftype;
	break;
    case DI_DEFERRED:
	LLOG(log_dsap, LLOG_NOTICE, ("di2cref - Trying to turn deferred di_block into continuation reference!"));
	return(NOTOK);
    default:
        LLOG(log_dsap, LLOG_EXCEPTIONS, ("di2cref - invalid di_state %d",di->di_state));
        return(NOTOK);
    }

    if (ctx == DS_CTX_QUIPU_DSP) {
	/* Make QSSR */
	/* append AP's from remaining di_blocks */
	LLOG (log_dsap, LLOG_TRACE, ("Making a QSSR"));
	for (loop=di->di_next; loop!=NULL_DI_BLOCK; loop=loop->di_next) {
		if (( ap = di2ap(loop)) == NULLACCESSPOINT)
			return OK;	/* Have finished - return OK */
		cref->cr_accesspoints = ap_append (cref->cr_accesspoints,ap);
	}
    }

    return OK;
}

oper_fail_wakeup(on)
struct oper_act	* on;
{
    DLOG(log_dsap, LLOG_TRACE, ("oper_fail_wakeup()"));
    /*
    *  Attempt to perform operation remotely has failed.
    *  Check the type of operation and take appropriate action.
    */

    if (on == NULLOPER) {
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("No operation to fail"));
	return;
    }

    switch(on->on_type)
    {
    case ON_TYPE_X500:
	task_fail_wakeup(on);
	break;
    case ON_TYPE_SUBTASK:
	subtask_fail_wakeup(on);
	break;
    case ON_TYPE_BIND_COMPARE:
	bind_compare_fail_wakeup(on);
	break;
    case ON_TYPE_GET_DSA_INFO:
	dsa_info_fail_wakeup(on);
	break;
    case ON_TYPE_GET_EDB:
	get_edb_fail_wakeup(on);
	break;
    case ON_TYPE_SHADOW:
	shadow_fail_wakeup(on);
	break;
    default:
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("oper_fail_wakeup - op has invalid type"));
	break;
    }
}


subtask_chain(tk)
struct task_act * tk;
{
    struct ds_search_task	*refer;
    struct ds_search_task	*nref;
    struct ds_search_task	* trail = NULL_ST;
    struct ds_search_task 	* st_done();
    struct oper_act		* on;
    struct di_block     	* di;
    struct di_block     	* di_tmp;
    register struct chain_arg	* tkcha = &(tk->tk_dx.dx_arg.dca_charg);
    register struct chain_arg	* oncha;
    struct trace_info		* ti;
    struct DSError		err;
    struct common_args		* ca;
struct common_args		* get_ca_ref();

    ca = get_ca_ref(&(tk->tk_dx.dx_arg));


    if(tk->refer_st == NULL_ST)
	return;

    DLOG(log_dsap, LLOG_TRACE, ("Chain search subtasks ..."));

    for(refer = tk->refer_st; refer != NULL_ST; refer = nref)
    {
	nref = refer->st_next;
	if((di = refer->st_di) == NULL_DI_BLOCK)
	{
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("search referred without di_block list"));
	    continue;
	}

	sort_dsa_list (&di);

	err.ERR_REFERRAL.DSE_ref_candidates = NULLCONTINUATIONREF;
	if ((di_tmp = select_refer_dsa (di,tk)) == NULL_DI_BLOCK) {
		/* The remote END is probably unable to follow the referral - chain if allowed */
		for(di_tmp=di; di_tmp!=NULL_DI_BLOCK; di_tmp=di_tmp->di_next)
	    	{
			if(di_tmp->di_state == DI_DEFERRED)
				continue;

#ifdef DEBUG
			DLOG(log_dsap, LLOG_DEBUG, ("About to call di2cref with:"));
			di_log(di_tmp);
#endif
			if(di2cref(di_tmp, &err, tk->tk_conn->cn_ctx) == OK)
			    break;
		}
	} else 
		(void) di2cref(di_tmp, &err, tk->tk_conn->cn_ctx);

	on = oper_alloc();
	on->on_type = ON_TYPE_SUBTASK;
	on->on_dsas = di;
	for(di_tmp=di; di_tmp!=NULL_DI_BLOCK; di_tmp=di_tmp->di_next)
	{
	    di_tmp->di_type = DI_OPERATION;
	    di_tmp->di_oper = on;
	}
	on->on_subtask = refer;
	on->on_task = tk;
	on->on_next_task = tk->tk_operlist;
	tk->tk_operlist = on;

	oncha = &(on->on_req.dca_charg);

	if(refer->st_alias == NULLDN)
	{
	    if (err.ERR_REFERRAL.DSE_ref_candidates)
		    oncha->cha_target = dn_cpy(err.ERR_REFERRAL.DSE_ref_candidates->cr_name);
	    else 
		    oncha->cha_target = dn_cpy (di->di_target);
	}
	else
	{
	    oncha->cha_target = dn_cpy(refer->st_alias);
	}

	if(di->di_rdn_resolved <= 0)
	{
	    oncha->cha_progress.op_resolution_phase = OP_PHASE_NOTSTARTED;
	    oncha->cha_progress.op_nextrdntoberesolved = 0;
	}
	else
	{
	    oncha->cha_progress.op_resolution_phase = OP_PHASE_PROCEEDING;
	    oncha->cha_progress.op_nextrdntoberesolved = di->di_rdn_resolved;
	}

	oncha->cha_aliasderef = ((oncha->cha_aliasedrdns = di->di_aliasedRDNs) != CR_NOALIASEDRDNS);
	if((oncha->cha_reftype = di->di_reftype) == RT_UNDEFINED)
	    oncha->cha_reftype = RT_SUPERIOR;

#ifdef COMPAT_6_0
        oncha->cha_entryonly = FALSE;
#else
        oncha->cha_entryonly = refer->st_entryonly;
#endif

	oncha->cha_returnrefs = FALSE;
	oncha->cha_domaininfo = NULLPE;

	if(tk->tk_timed == FALSE)
	{
	    oncha->cha_timelimit = NULLCP;
	}
	else
	{
#ifdef CHAIN_ARGS_TIMEOUT
	    struct UTCtime	ut;
	    tm2ut(gmtime(&(tk->tk_timeout)), &(ut));
	    oncha->cha_timelimit = strdup(utct2str(&ut));
#else
	    oncha->cha_timelimit = NULLCP;
#endif
	}

	DLOG(log_dsap, LLOG_DEBUG, ("Checking history of op"));
	if(tk->tk_conn->cn_ctx == DS_CTX_X500_DAP)
	{
	    DLOG(log_dsap, LLOG_DEBUG, ("... user originated ..."));
	    oncha->cha_originator = dn_cpy(tk->tk_conn->cn_dn);
	    oncha->cha_trace = NULLTRACEINFO;
	}
	else
	{
	    oncha->cha_originator = dn_cpy(tk->tk_dx.dx_arg.dca_charg.cha_originator);
	    oncha->cha_trace = ti_cpy(tkcha->cha_trace);
	}

	DLOG(log_dsap, LLOG_DEBUG, ("Setting trace info"));
	ti = (struct trace_info *) malloc(sizeof(struct trace_info));
	ti->ti_dsa = dn_cpy(mydsadn);
	ti->ti_target = dn_cpy(di->di_target);

	if(di->di_rdn_resolved <= 0)
	{
	    ti->ti_progress.op_resolution_phase = OP_PHASE_NOTSTARTED;
	    ti->ti_progress.op_nextrdntoberesolved = 0;
	}
	else
	{
	    ti->ti_progress.op_resolution_phase = OP_PHASE_PROCEEDING;
	    ti->ti_progress.op_nextrdntoberesolved = di->di_rdn_resolved;
	}

	ti->ti_next = oncha->cha_trace;
	oncha->cha_trace = ti;

	 on->on_req.dca_dsarg = tk->tk_dx.dx_arg.dca_dsarg; 	/* struct copy */
/*
	(void) ds_arg_dup (&(tk->tk_dx.dx_arg.dca_dsarg), &(on->on_req.dca_dsarg));
*/

#ifdef COMPAT_6_0
	on->on_req.dca_dsarg.arg_sr.sra_subset = refer->st_subset;
#endif

	on->on_arg = &(on->on_req);

	DLOG(log_dsap, LLOG_DEBUG, ("Generating search subtask OP"));
	if( (ca->ca_servicecontrol.svc_options & SVC_OPT_CHAININGPROHIBIT) 
		|| (oper_chain(on) != OK))
	{
	   add_cref2poq (&tk->tk_resp.di_result.dr_res.dcr_dsres.res_sr, err.ERR_REFERRAL.DSE_ref_candidates);
	   oper_task_extract(on);
	   oper_free(on);
		
	   if (trail == NULL_ST)
		tk->refer_st = st_done(&refer);
	   else
		trail->st_next = st_done (&refer);
	} else {
		refer->st_cr = err.ERR_REFERRAL.DSE_ref_candidates;
		trail = refer;
	}
    }
    if (trail != NULL_ST)
	trail->st_next = tk->referred_st;
    tk->referred_st = tk->refer_st;
    tk->refer_st = NULL_ST;

    if((tk->referred_st == NULL_ST) && (tk->tk_state == TK_PASSIVE) && (tk->tk_operlist == NULLOPER))
	{
	    task_conn_extract(tk);
	    task_result(tk);
	    task_extract(tk);
	}

}

subtask_result_wakeup(on)
struct oper_act	* on;
{
    struct task_act	* tk;
    struct ds_search_task	**next_st;
    struct ds_search_task	* st;

    DLOG(log_dsap, LLOG_TRACE, ("subtask_result_wakeup"));

    if((tk = on->on_task) == NULLTASK)
    {
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("Oper can't wake up (result)extracted task"));
	oper_extract(on);
    }
    else
    {
	/*
	*  Were waiting for a remote subtask result and here it is.
	*/
	next_st = &(tk->referred_st);
	for(st=tk->referred_st; st!=NULL_ST; st=(*next_st))
	{
	    if(st == on->on_subtask)
		break;

	    next_st = &(st->st_next);
	}
	if(st == NULL_ST)
	{
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("subtask_result_wakeup - subtask lost from referred list"));
	}
	else
	{
	    /*
	    *  Correlate uncorrelated search results from oper,
	    *  then merge with correlated search results of task.
	    */

	    struct ds_search_result * tk_sr = &(tk->tk_resp.di_result.dr_res.dcr_dsres.res_sr);
	    struct ds_search_result * op_sr = &(on->on_resp.di_result.dr_res.dcr_dsres.res_sr);

	    DLOG(log_dsap, LLOG_DEBUG, ("Collating a search result"));

	    st_comp_free (st);
	    (*next_st) = st->st_next;

	    correlate_search_results(op_sr);
	    if(tk_sr->srr_next == NULLSRR)
	    {
		DLOG(log_dsap, LLOG_DEBUG, ("Search result unallocated!"));
		tk_sr->srr_next = (struct ds_search_result *) calloc(1, sizeof(struct ds_search_result));
		tk_sr->srr_next->srr_correlated = TRUE;
		tk_sr->srr_next->srr_un.srr_unit = (struct ds_search_unit *) calloc(1, sizeof(struct ds_search_unit));
		tk_sr->srr_next->CSR_limitproblem = LSR_NOLIMITPROBLEM;
	    }

	    merge_search_results(tk_sr->srr_next, op_sr);
	}
	
	oper_extract(on);

	if((tk->referred_st == NULL_ST) && (tk->tk_state == TK_PASSIVE) && (tk->tk_operlist == NULLOPER))
	{
	    task_conn_extract(tk);
	    task_result(tk);
	    task_extract(tk);
	}
    }
}

subtask_error_wakeup(on)
struct oper_act	* on;
{
    struct task_act	* tk;
    struct ds_search_task	**next_st;
    struct ds_search_task	* st;

    DLOG(log_dsap, LLOG_TRACE, ("subtask_error_wakeup"));

    if((tk = on->on_task) == NULLTASK)
    {
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("Oper can't wake up (error) extracted task"));
	oper_extract(on);
    }
    else
    {
	/*
	*  Were waiting for a remote subtask result and got a remote error.
	*  If it is a referral, then rechain the operation if appropriate
	*  otherwise dump the subtask and check the task for completion.
	*/
/*
	ds_error_free(&(tk->tk_resp.di_error.de_err));
*/
	tk->tk_error = &(on->on_resp.di_error.de_err);

	if((on->on_resp.di_error.de_err.dse_type == DSE_DSAREFERRAL)
  	|| (on->on_resp.di_error.de_err.dse_type == DSE_REFERRAL))
	{
	    DLOG(log_dsap, LLOG_DEBUG, ("Try rechaining st"));
	    if(oper_rechain(on) == OK)
	    {
		DLOG(log_dsap, LLOG_DEBUG, ("Succeeded rechaining st"));
		return;
	    }
	    DLOG(log_dsap, LLOG_DEBUG, ("Failed rechaining st"));
	    add_cref2poq (&tk->tk_resp.di_result.dr_res.dcr_dsres.res_sr, on->on_resp.di_error.de_err.ERR_REFERRAL.DSE_ref_candidates);
	    on->on_resp.di_error.de_err.ERR_REFERRAL.DSE_ref_candidates = NULLCONTINUATIONREF;
	}

	next_st = &(tk->referred_st);
	for(st=tk->referred_st; st!=NULL_ST; st=(*next_st))
	{
	    if(st == on->on_subtask)
		break;

	    next_st = &(st->st_next);
	}
	if(st == NULL_ST)
	{
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("subtask_result_wakeup - subtask lost from referred list"));
	}
	else
	{
	    st_comp_free (st);
	    (*next_st) = st->st_next;
	}

	oper_extract(on);

	if((tk->referred_st == NULL_ST) && (tk->tk_state == TK_PASSIVE) && (tk->tk_operlist == NULLOPER))
	{
	    task_conn_extract(tk);
	    task_result(tk);
	    task_extract(tk);
	}
    }
}

subtask_fail_wakeup(on)
struct oper_act	* on;
{
    struct task_act	* tk;
    struct DSError	* err;
    struct ds_search_task	**next_st;
    struct ds_search_task	* st;

    DLOG(log_dsap, LLOG_TRACE, ("subtask_fail_wakeup"));

    if((tk = on->on_task) == NULLTASK)
    {
	LLOG(log_dsap, LLOG_FATAL, ("subtask_fail_wakeup: no task"));
	oper_extract(on);
	return;
    }
    else
    {
	next_st = &(tk->referred_st);
	for(st=tk->referred_st; st!=NULL_ST; st=(*next_st))
	{
	    if(st == on->on_subtask)
		break;

	    next_st = &(st->st_next);
	}
	if(st == NULL_ST)
	{
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("subtask_result_wakeup - subtask lost from referred list"));
	}
	else
	{
	    add_cref2poq (&tk->tk_resp.di_result.dr_res.dcr_dsres.res_sr, st->st_cr);
	    st_comp_free (st);
	    (*next_st) = st->st_next;
	}

	oper_extract(on);

	if((tk->referred_st == NULL_ST) && (tk->tk_state == TK_PASSIVE) && (tk->tk_operlist == NULLOPER))
	{
	    task_conn_extract(tk);
	    task_result(tk);
	    task_extract(tk);
	}
    }
    err = &(tk->tk_resp.di_error.de_err);
    if((err->dse_type != DSE_REFERRAL) && (err->dse_type != DSE_DSAREFERRAL))
    {
	err->dse_type = DSE_SERVICEERROR;
	err->ERR_SERVICE.DSE_sv_problem = DSE_SV_UNAVAILABLE;
    }
}

subtask_dsa_info_wakeup(di)
struct di_block	* di;
{
    struct task_act	* tk = di->di_task;


    DLOG(log_dsap, LLOG_TRACE, ("subtask_dsa_info_wakeup"));

    if (tk == NULLTASK) {
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("subtask_dsa_info_wakeup no task"));
	return;
    }

    /*
    * Were waiting for a reference to return.
    * Check if the reference now returned is acceptable.
    * If it is return a referral and unwrap everything,
    * otherwise try another di_block for the reference.
    */

    if(di2cref(di, &(tk->tk_resp.di_error.de_err), tk->tk_conn->cn_ctx) != OK)
    {
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("subtask_dsa_info_wakeup - reference not acceptable"));
	/* Remove di_block which generated unwanted referral wait */
	if(tk->tk_dsas == NULL_DI_BLOCK)
	{
	    /* No more dsas from which to request info to form referral */
	    tk->tk_resp.di_error.de_err.dse_type = DSE_SERVICEERROR;
	    tk->tk_resp.di_error.de_err.ERR_SERVICE.DSE_sv_problem = DSE_SV_INVALIDREFERENCE;
	    task_conn_extract(tk);
	    task_error(tk);
	    task_extract(tk);
	    return;
	}
	return;
    }

    task_conn_extract(tk);
    task_error(tk);
    task_extract(tk);
}


add_cref2poq (res,cref)
struct ds_search_result *res;
ContinuationRef cref;	
{
ContinuationRef cr;	

	if (res->CSR_cr == NULLCONTINUATIONREF) {
		res->CSR_cr = cref;
		return;
	}
	for (cr = res->CSR_cr; cr->cr_next != NULLCONTINUATIONREF; cr=cr->cr_next)
		;

	cr->cr_next = cref;
}

relay_dsa (on)
struct oper_act * on;
{
struct DSError  err;
struct di_block *di = NULL_DI_BLOCK;
Entry my_entry;
Attr_Sequence as, entry_find_type();
AV_Sequence avs;
struct di_block	**di_trail;
struct dn_seq	* dn_stack = NULLDNSEQ;

	if (  (on == NULLOPER)
	   || (on->on_relay == FALSE)
	   || (on->on_relay == 2)	/* relayed once before */
	   || (on->on_task == NULLTASK)
	   || (on->on_task->tk_conn == NULLCONN)
	   || (on->on_task->tk_conn->cn_ctx != DS_CTX_X500_DAP))
		return NOTOK;

	if ((my_entry = local_find_entry_aux (mydsadn,TRUE)) == NULLENTRY)
		return NOTOK;

	if (( as = entry_find_type (my_entry,at_relaydsa)) == NULLATTR)
		return NOTOK;

	di_trail = &di;

	for (avs = as->attr_value; avs != NULLAV; avs=avs->avseq_next) {
		if (avs->avseq_av.av_struct == NULL)
			continue;

		switch(get_dsa_info((DN)avs->avseq_av.av_struct, dn_stack,
		       &err, di_trail))
		{
		case DS_OK:
		    /* di_trail is a completed dsa info block */
		    DLOG(log_dsap, LLOG_DEBUG, ("In relay gdiOK:"));
#ifdef DEBUG
		    di_list_log(*di_trail);
#endif
		    (*di_trail)->di_target = NULLDN;
		    di_trail = &((*di_trail)->di_next);
		    break;

		case DS_CONTINUE:
		    /* di_trail is a deferred dsa info block */
		    DLOG(log_dsap, LLOG_DEBUG, ("In relay gdiCONT:"));
#ifdef DEBUG
		    di_list_log(*di_trail);
#endif
		    (*di_trail)->di_target = NULLDN;
		    di_trail = &((*di_trail)->di_next);
		    break;

		case DS_X500_ERROR:
		    /* Error encountered generating di_block */
		    DLOG(log_dsap, LLOG_NOTICE, ("relay - get_dsa_info (slave) returned X500 ERROR"));
		    ds_error_free(&err);
		    break;

		default:
		    LLOG(log_dsap, LLOG_EXCEPTIONS, ("dsa_info_new - get_dsa_info (master) unexpected return"));
		    break;
		}

	}

	on->on_relay = 2;	/* Don't relay twice to same DSA ! */

	if (di == NULL_DI_BLOCK)
		return NOTOK;

	on->on_dsas = di;

	return OK;

}
