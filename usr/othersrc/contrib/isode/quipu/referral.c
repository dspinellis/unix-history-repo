/* referral.c - create referral notices */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/referral.c,v 7.3 91/02/22 09:39:46 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/referral.c,v 7.3 91/02/22 09:39:46 mrose Interim $
 *
 *
 * $Log:	referral.c,v $
 * Revision 7.3  91/02/22  09:39:46  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:54:41  mrose
 * sync
 * 
 * Revision 7.1  89/12/19  16:20:45  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:18:01  mrose
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


#include "quipu/util.h"
#include "quipu/connection.h"

extern LLog * log_dsap;
char remote_lookup = TRUE;
struct PSAPaddr	*	psap_cpy();
struct dn_seq	* dn_seq_push();
struct dn_seq	* dn_seq_pop();
struct di_block	* di_alloc();

extern int dn_print ();

static struct access_point * top_ap = NULLACCESSPOINT;

struct access_point	* ap_cpy(ap)
struct access_point	* ap;
{
    struct access_point	* ret_ap;
    struct access_point	**tmp_ap;

    if(ap == NULLACCESSPOINT)
	return(NULLACCESSPOINT);


    for(tmp_ap = &ret_ap; ap != NULLACCESSPOINT; ap=ap->ap_next)
    {
	(*tmp_ap) = (struct access_point *) calloc(1, sizeof(struct access_point));
	(*tmp_ap)->ap_name = dn_cpy(ap->ap_name);
	if (ap->ap_address)
		(*tmp_ap)->ap_address = psap_cpy(ap->ap_address);
	tmp_ap = &((*tmp_ap)->ap_next);
    }

    (*tmp_ap) = NULLACCESSPOINT;

    return(ret_ap);
}


static ContinuationRef new_ref (name,rt,ap)
DN name;
int rt;
struct access_point * ap;
{
ContinuationRef ptr;

	if (ap == NULLACCESSPOINT)
		return (NULLCONTINUATIONREF);

	ptr = (ContinuationRef) smalloc (sizeof(continuation_ref));
	ptr->cr_aliasedRDNs = CR_NOALIASEDRDNS;
	ptr->cr_name = dn_cpy (name);
	ptr->cr_rdn_resolved = CR_RDNRESOLVED_NOTDEFINED;
	ptr->cr_reftype = rt;
	ptr->cr_accesspoints = ap_cpy(ap);

	return (ptr);
}

struct access_point * ap_append (a,b)
struct access_point * a;
struct access_point * b;
{
struct access_point * trail;
struct access_point * top;

	if (a == NULLACCESSPOINT)
		return (b);
	if ( b == NULLACCESSPOINT)
		return (a);

	for (top = a ; a != NULLACCESSPOINT; a = a->ap_next)
		trail = a;

	trail->ap_next = b;
	return (top);
}

ContinuationRef cont_ref_parent (name)
DN name;
{
	return (new_ref(name,RT_SUPERIOR,top_ap));
}

add_str_parent (sdn,spsap)
char * sdn, *spsap;
{
DN dn,str2dn();
struct PSAPaddr *psap, * str2paddr();
struct access_point * next_ap;

/* add string DN and string PSAP to list of parents */

	if ((psap = str2paddr (spsap)) == NULLPA) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("Invalid parent address %s",spsap));
		return;
	}
	if (( dn = str2dn (sdn)) == NULLDN ) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("Invalid parent dn %s",sdn));
		return;
	}

	next_ap = (struct access_point *) smalloc (sizeof(struct access_point));
	next_ap->ap_name = dn;
	next_ap->ap_address = psap_cpy(psap);
	next_ap->ap_next = NULLACCESSPOINT;
	top_ap = ap_append (top_ap,next_ap);

}

struct PSAPaddr *parent_psap()
{
	if (top_ap == NULLACCESSPOINT)
		return (NULLPA);
	return (top_ap->ap_address);
}

/*
*  Generate a list of dsa information blocks (di_block) from the master-dsa
*  and slave-dsa attributes of the entry, the dsa dn for which an info block
*  is generated is the name from the attribute.
*  Currently all of the dsa DNs are used to generate info blocks in the
*  list; since this requires access to the entry for each DSA dn this may
*  result in some suspended operations being initiated.
*  If some info blocks are generated then DS_CONTINUE is returned;
*  If no completed or suspended info blocks can be generated then the calling
*  process is returned an invalid reference error.
*
*  NB - As with get_dsa_info, the blocks generated need to be further
*  processed by the calling routine.
*/
int	  dsa_info_new (name,dn_stack,master,entry_ptr,err,di_p)
DN name;
struct dn_seq	* dn_stack;
int		  master;
Entry entry_ptr;
struct DSError	* err;
struct di_block	**di_p;
{
AV_Sequence		  avs;
int			  ret_val;
struct DSError		  err_tmp;
struct di_block		**di_trail;
struct dn_seq		* new_dn_stack;
struct access_point * aps;

	DLOG (log_dsap,LLOG_TRACE,("in dsa_info_new"));

	ret_val = DS_ERROR_LOCAL;
	di_trail = di_p;

	new_dn_stack = dn_seq_push(name,dn_stack);

	if (entry_ptr->e_external) {
	    aps = (struct access_point *) entry_ptr->e_reference->avseq_av.av_struct;
	    (*di_p) = di_alloc();
	    (*di_p)->di_type = DI_TASK;
	    (*di_p)->di_dn = dn_cpy(aps->ap_name);
	    (*di_p)->di_target = dn_cpy (name);
	    (*di_p)->di_state = DI_ACCESSPOINT;
	    (*di_p)->di_rdn_resolved = CR_RDNRESOLVED_NOTDEFINED;
	    (*di_p)->di_aliasedRDNs = CR_NOALIASEDRDNS;
	    if (((*di_p)->di_reftype = entry_ptr->e_reftype) == RT_NONSPECIFICSUBORDINATE) {
		for (avs = entry_ptr->e_reference; avs != NULLAV; avs = avs->avseq_next) {
		    if (((struct access_point *) avs->avseq_av.av_struct)->ap_address == NULLPA) {
			    pslog (log_dsap,LLOG_EXCEPTIONS,"No address in NSSR",dn_print,(caddr_t)name);
			    continue;
		    }
		    aps = ap_cpy ((struct access_point *) avs->avseq_av.av_struct);
		    aps->ap_next = (*di_p)->di_accesspoints;
		    (*di_p)->di_accesspoints = aps;
	        }
		if ((*di_p)->di_accesspoints == NULLACCESSPOINT) {
			err->dse_type = DSE_SERVICEERROR;
			err->ERR_SERVICE.DSE_sv_problem = DSE_SV_INVALIDREFERENCE;
			new_dn_stack = dn_seq_pop(new_dn_stack);
			return DS_X500_ERROR;
		}
	    } else {

		    if (aps->ap_address != NULLPA) {
		        (*di_p)->di_accesspoints = ap_cpy (aps);
		    } else {
			di_free (*di_p);
			switch(get_dsa_info(aps->ap_name, new_dn_stack,
			       &(err_tmp), di_p)) {
			case DS_OK:
			case DS_CONTINUE:
			    (*di_p)->di_target = dn_cpy(name);
			    break;

			case DS_X500_ERROR:
			    /* Error encountered generating di_block */
			    DLOG(log_dsap, LLOG_NOTICE, ("dsa_info_new - get_dsa_info (external) returned X500 ERROR"));
			    if ((err_tmp.dse_type == DSE_SERVICEERROR )
				&& (err_tmp.ERR_SERVICE.DSE_sv_problem == DSE_SV_DITERROR)) {
					*err = err_tmp;
					new_dn_stack = dn_seq_pop(new_dn_stack);
					return DS_X500_ERROR;
				}
			    ds_error_free(&err_tmp);
			    goto out;
			}
		     }
	    }
	    
	    return DS_CONTINUE;
    }

	for (avs = entry_ptr->e_master; avs != NULLAV; avs=avs->avseq_next) {
		if (avs->avseq_av.av_struct == NULL)
			continue;

		switch(get_dsa_info((DN)avs->avseq_av.av_struct, new_dn_stack,
		       (&err_tmp), di_trail)) {
		case DS_OK:
		case DS_CONTINUE:
		    (*di_trail)->di_target = dn_cpy(name);
		    di_trail = &((*di_trail)->di_next);
		    ret_val = DS_CONTINUE;
		    break;

		case DS_X500_ERROR:
		    /* Error encountered generating di_block */
		    DLOG(log_dsap, LLOG_NOTICE, ("dsa_info_new - get_dsa_info (master) returned X500 ERROR"));
		    if ((err_tmp.dse_type == DSE_SERVICEERROR )
			&& (err_tmp.ERR_SERVICE.DSE_sv_problem == DSE_SV_DITERROR)) {
				*err = err_tmp;
				new_dn_stack = dn_seq_pop(new_dn_stack);
				return DS_X500_ERROR;
			}
		    ds_error_free(&err_tmp);
		    break;
		}
	}

	if(!master) {
	    /* repeat for slaves */
	     for (avs = entry_ptr->e_slave; avs != NULLAV; avs=avs->avseq_next) {
		if (avs->avseq_av.av_struct == NULL)
			continue;

		switch(get_dsa_info((DN)avs->avseq_av.av_struct, new_dn_stack,
		       &(err_tmp), di_trail)) {
		case DS_OK:
		case DS_CONTINUE:
		    (*di_trail)->di_target = dn_cpy(name);
		    di_trail = &((*di_trail)->di_next);
		    ret_val = DS_CONTINUE;
		    break;

		case DS_X500_ERROR:
		    /* Error encountered generating di_block */
		    DLOG(log_dsap, LLOG_NOTICE, ("dsa_info_new - get_dsa_info slave returned X500 ERROR"));
		    if ((err_tmp.dse_type == DSE_SERVICEERROR )
			&& (err_tmp.ERR_SERVICE.DSE_sv_problem == DSE_SV_DITERROR)) {
				*err = err_tmp;
				new_dn_stack = dn_seq_pop(new_dn_stack);
				return DS_X500_ERROR;
			}
		    ds_error_free(&err_tmp);
		    break;
	        }
	     }
     }

out:;

	new_dn_stack = dn_seq_pop(new_dn_stack);

	if((ret_val == DS_ERROR_LOCAL) || (ret_val == DS_X500_ERROR))
	{
	    err->dse_type = DSE_SERVICEERROR;
	    err->ERR_SERVICE.DSE_sv_problem = DSE_SV_INVALIDREFERENCE;
	    ret_val = DS_X500_ERROR;
	    pslog (log_dsap,LLOG_EXCEPTIONS,"Invalid reference in entry",dn_print,(caddr_t)name);
	}

	return (ret_val);
}

struct di_block * ap2di (ap,name,master,di_type,oper,cr_type)
struct access_point *ap;
DN name;
char master;
char di_type;
struct oper_act *oper;
int cr_type;
{
struct access_point *loop;
struct di_block	*res = NULL_DI_BLOCK;
struct di_block	*ptr;
struct di_block	*trail;

	if(ap == NULLACCESSPOINT)
	{
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("No acces point to make into a di"));
	    return NULL_DI_BLOCK;
	}

	for (loop=ap; loop!=NULLACCESSPOINT; loop=loop->ap_next) {
		ptr = di_alloc();
		ptr->di_dn = dn_cpy(loop->ap_name);
		ptr->di_target = dn_cpy(name);
		ptr->di_reftype = cr_type;
		ptr->di_state = DI_ACCESSPOINT;
		ptr->di_type = di_type;
		ptr->di_oper = oper;
		ptr->di_accesspoints = (struct access_point *) calloc(1, sizeof(struct access_point));
		ptr->di_accesspoints->ap_name = dn_cpy(loop->ap_name);
		ptr->di_accesspoints->ap_address = psap_cpy(loop->ap_address);
		if (res == NULL_DI_BLOCK)
			trail = res = ptr;
		else 
			trail = (trail->di_next = ptr);

		if (master) 
			break;	/* Only want to use first AP */
	}

	sort_dsa_list (&res);

	return res;
}


int	  dsa_info_parent (name,err,di_p,master)
DN		  name;
struct DSError	* err;
struct di_block	**di_p;
char master;
{
	DLOG(log_dsap, LLOG_TRACE, ("dsa_info_parent"));

	if(top_ap == NULLACCESSPOINT)
	{
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("No parents!"));
	    err->dse_type = DSE_SERVICEERROR;
	    err->ERR_SERVICE.DSE_sv_problem = DSE_SV_INVALIDREFERENCE;
	    return(DS_X500_ERROR);
	}

	*di_p = ap2di (top_ap,name,master,DI_TASK,NULLOPER,RT_SUPERIOR);

	return(DS_CONTINUE);
}

