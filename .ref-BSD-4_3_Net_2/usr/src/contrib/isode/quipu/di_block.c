/* di_block.c - routines to handle operation activity blocks */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/di_block.c,v 7.4 91/02/22 09:38:37 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/di_block.c,v 7.4 91/02/22 09:38:37 mrose Interim $
 *
 *
 * $Log:	di_block.c,v $
 * Revision 7.4  91/02/22  09:38:37  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/10/17  11:53:32  mrose
 * sync
 * 
 * Revision 7.2  90/07/09  14:45:34  mrose
 * sync
 * 
 * Revision 7.1  89/12/19  16:20:09  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:17:00  mrose
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
#include "tsap.h"
#include "tailor.h"

extern LLog * log_dsap;

extern time_t timenow;

struct di_block *di_alloc()
{
    struct di_block	* di_ret;

    di_ret = (struct di_block *) calloc(1,sizeof(struct di_block));

    return(di_ret);
}

di_free(di)
struct di_block *di;
{
	DLOG(log_dsap, LLOG_TRACE, ("di_free()"));

	switch (di->di_state) {
	  case DI_GLOBAL: 	break;
	  case DI_TASK: 	break;
	  case DI_OPERATION: 	break;
	  default:
		DLOG(log_dsap, LLOG_TRACE, ("di_free() of unknown type %d",di->di_state));
		return;
	}

	dn_free(di->di_dn);

	dn_free(di->di_target);

	if(di->di_accesspoints != NULLACCESSPOINT)
	    aps_free(di->di_accesspoints);

	if ((di->di_type == DI_TASK) && di->di_entry)
		entry_free (di->di_entry);

	di->di_state = -1;

	free((char *)di);
}

di_extract(old_di)
struct di_block	* old_di;
{
    struct di_block	* di;
    struct di_block	**next_di;

    LLOG(log_dsap, LLOG_TRACE, ("di_extract"));
#ifdef DEBUG
    di_log(old_di);
#endif

    switch(old_di->di_type)
    {
    case DI_GLOBAL:
        next_di = &(deferred_dis);
	for(di=deferred_dis; di!=NULL_DI_BLOCK; di=di->di_next)
	{
	    if(di == old_di)
		break;

	    next_di = &(di->di_next);
	}
	if(di == NULL_DI_BLOCK)
	{
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("di_block has escaped from global list"));
	}
	else
	{
	    (*next_di) = di->di_next;
	}
	break;
    case DI_OPERATION:
	break;
    case DI_TASK:
	break;
    default:
	break;
    }

    di_free(old_di);
}

di_desist(di)
struct di_block	* di;
{
    struct di_block	* di_tmp1;
    struct di_block	* di_tmp1_next;
    struct di_block	* di_tmp2;
    struct di_block	**di_p2;

	DLOG(log_dsap, LLOG_TRACE, ("di_desist()"));

    for(di_tmp1=di; di_tmp1 != NULL_DI_BLOCK; di_tmp1 = di_tmp1_next)
    {
	di_tmp1_next = di_tmp1->di_next;

	switch(di->di_state)
	{
	case DI_ACCESSPOINT:
	case DI_COMPLETE:
		break;
	case DI_DEFERRED:
	    di_p2 = &(di_tmp1->di_perform->on_wake_list);
	    for(di_tmp2=di_tmp1->di_perform->on_wake_list; di_tmp2!=NULL_DI_BLOCK; di_tmp2=di_tmp2->di_wake_next)
	    {
		if(di_tmp2 == di_tmp1)
		    break;

		di_p2 = &(di_tmp2->di_wake_next);
	    }
	    if(di_tmp2 == NULL_DI_BLOCK)
	    {
		LLOG(log_dsap, LLOG_EXCEPTIONS, ("di_desist - di_block lost from performing operations wake up list"));
	    }
	    else
	    {
		(*di_p2) = di_tmp2->di_wake_next;
	    }
	    break;
	}
	di_free(di_tmp1);
    }
}

di_log(di)
struct di_block	* di;
{
    DLOG (log_dsap,LLOG_DEBUG, ("di_block [%x] , state = %d, type = %d",
	di, di->di_state, di->di_type));
}

di_list_log(di)
struct di_block *di;
{
    struct di_block	* di_tmp;

    DLOG(log_dsap, LLOG_DEBUG, ("di_list:"));
#ifdef DEBUG
    for(di_tmp = di; di_tmp!=NULL_DI_BLOCK; di_tmp=di_tmp->di_next)
    {
	di_log(di_tmp);
    }
#endif
    DLOG(log_dsap, LLOG_DEBUG, ("di_list ends."));
}



static struct dn_seq * prefer_dsa_list = NULLDNSEQ;

prefer_dsa (str)
char * str;
{
struct dn_seq * dsa, *loop;

	if (( dsa=str2dnseq(str)) == NULLDNSEQ) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("Invalid prefered DSA name %s",str));	
		return;
	}
	
	if (prefer_dsa_list == NULLDNSEQ) 
		prefer_dsa_list = dsa;
	else {
		for (loop = prefer_dsa_list; loop->dns_next != NULLDNSEQ; loop=loop->dns_next)
			;
		loop->dns_next = dsa;
	}
}

static di_prefer_dsa (a,b)
DN a,b;
{
int x,y;

	if (prefer_dsa_list == NULLDNSEQ) {
		DLOG (log_dsap,LLOG_TRACE,("NO DSAs to chose from"));
		return 0;	/* not fussy !!! */
	}

	if ((b == NULLDN) || (a == NULLDN)) {
		DLOG (log_dsap,LLOG_NOTICE,("di_pref DNs NULL"));
		return 0;	/* safty catch - don't think it can happen */
	}

	if ((x = dn_in_dnseq (a,prefer_dsa_list)) == 0)
		if ((y = dn_in_dnseq (b,prefer_dsa_list)) == 0)
			return 0;
		else {
			DLOG (log_dsap,LLOG_TRACE,("DSA selected on preference"));
			return -1;
		}

	if ((y = dn_in_dnseq (b,prefer_dsa_list)) == 0) {
		DLOG (log_dsap,LLOG_TRACE,("DSA selected on preference"));
		return 1;
	}

	if ( x != y ) {
		DLOG (log_dsap,LLOG_TRACE,("DSA selected on preference"));
		return ( x > y ? -1 : 1 );
	}

	return 0;
	
}

static di_ap2comp (di)
struct di_block	**di;
{
struct di_block *loop;
Entry eptr;

	/* replace DI_ACCESSPOINT with DI_COMPLETE if possible... */
	/*	(data may have been cached since creating DI_ACCESSPOINT) */

	for (loop= *di; loop!=NULL_DI_BLOCK; loop=loop->di_next) {
		if (loop->di_state != DI_ACCESSPOINT)
			continue;

		if (loop->di_reftype == RT_NONSPECIFICSUBORDINATE) 
			continue;	/* can't split these - all must be followed... */

		if (loop->di_accesspoints->ap_next == NULLACCESSPOINT) {
			if ((eptr=local_find_entry (loop->di_accesspoints->ap_name,FALSE)) != NULLENTRY)
				if (eptr->e_dsainfo != NULLDSA) {
					loop->di_entry = eptr;
					eptr->e_refcount++;
					loop->di_state = DI_COMPLETE;	
					aps_free (loop->di_accesspoints);
					loop->di_accesspoints = NULLACCESSPOINT;
				}
		} else 
			LLOG (log_dsap,LLOG_EXCEPTIONS,("Many access points, but not a RT_NONSPECIFICSUBORDINATE"));
	}

}

dsa_reliable (cn,good,when)
struct connection * cn;
char good;
time_t when;
{
Entry ptr;

	if ( (ptr=local_find_entry(cn->cn_dn,FALSE)) == NULLENTRY)
		return;

	if (ptr->e_dsainfo == NULLDSA)
		return;

	ptr->e_dsainfo->dsa_last_attempt = when;
	if (good) {
		ptr->e_dsainfo->dsa_last_success = when;
		ptr->e_dsainfo->dsa_failures = 0;
	} else 
		ptr->e_dsainfo->dsa_failures++;
}

static di_cmp_reliability (a,b)
struct di_block *a, *b;
{
extern time_t retry_timeout;
struct dsa_info *da, *db;

	/* If we have used a DSA recently, with no failures - use it again */

	if ((da = a->di_entry->e_dsainfo) == NULLDSA)
		return 0;
	if ((db = b->di_entry->e_dsainfo) == NULLDSA)
		return 0;

	if (da->dsa_last_attempt == (time_t)0) {
		if (db->dsa_failures == 0) {
			if ((db->dsa_last_success != (time_t)0)
				&& (timenow - db->dsa_last_attempt < retry_timeout))
				return -1;	/* b worked recently */
		} else if (timenow - db->dsa_last_attempt < retry_timeout)
			return 1;		/* b failed recently */
		return 0;	/* have not tried either recently  */

	} else if (db->dsa_last_attempt == (time_t)0) {
		if (da->dsa_failures == 0) {
			if ((da->dsa_last_success != (time_t)0)
				&& (timenow - da->dsa_last_attempt < retry_timeout))
				return 1;	/* a worked recentdlry */
		} else if (timenow - da->dsa_last_attempt < retry_timeout)
			return -1; 		/* a failed recently */
		return 0;	/* have not tried either recently  */
	}

	if (da->dsa_failures == 0) {
		if (db->dsa_failures == 0)
			return 0;       /* both OK */
		return 1;	/* a worked last time, b failed - use a */
	}

	if (db->dsa_failures == 0)
		return -1;	/* b worked last time, a failed - use b */

	/* both failed last time - see if either have suceeded recently */

	if ((timenow - da->dsa_last_success) > retry_timeout ) {
		if ((timenow - db->dsa_last_success) > retry_timeout)
			return 0;	/* too long ago to tell */
		return -1; 	/* use b it worked not that long ago... */
	}
	if ((timenow - db->dsa_last_success) > retry_timeout)
		return 1;	/* use a it worked not that long ago... */

	/* neither has worked recently chose some other way */
	return 0;
}

static di_cmp_address (a,b)
struct di_block *a, *b;
{
struct NSAPaddr *na;
struct NSAPaddr *nb;
struct NSAPaddr nas;
struct TSAPaddr *ta2norm();
struct TSAPaddr *ta;
struct TSAPaddr *tb;
int *ip;
extern DN mydsadn;
DN dnptr, mydnptr, dna,dnb;
int ma,mb;

	/* select DSA with best looking address !!! */

	if (a->di_state == DI_COMPLETE) {
		ta = &(a->di_entry->e_dsainfo->dsa_addr->pa_addr.sa_addr);
		tb = &(b->di_entry->e_dsainfo->dsa_addr->pa_addr.sa_addr);
		dna = a->di_dn;
		dnb = b->di_dn;
	} else {
		/* Use 1st access point only */
		ta = &(a->di_accesspoints->ap_address->pa_addr.sa_addr);
		tb = &(b->di_accesspoints->ap_address->pa_addr.sa_addr);
		dna = NULLDN;
		dnb = NULLDN;
	}

	/* ta2norm return a static buffer */
	ta = ta2norm (ta);
	nas = *(ta->ta_addrs);	/* struct copy */
	na = &nas;
	tb = ta2norm (tb);
	nb = tb->ta_addrs;

	/* normalised, so look for first "na" match with ts_communities */
	for (ip = ts_communities; *ip; ip++) {
		if (*ip == na->na_community) {
			if (*ip == nb->na_community)
				break;		/* same primary community */
			return 1;	  	/* 'a' preferred */
		}
		if (*ip == nb->na_community)
			return -1;		/* 'b' preferred */
	}

	/* Look at the DSA name to detect locality */
	ma=0;
	for (dnptr=dna, mydnptr=mydsadn ;
		(dnptr!=NULLDN) && (mydnptr!=NULLDN) ;
		dnptr=dnptr->dn_parent, mydnptr=mydnptr->dn_parent) {
		if (rdn_cmp(dnptr->dn_rdn,mydnptr->dn_rdn) == 0)
			ma++;
	}

	mb=0;
	for (dnptr=dnb, mydnptr=mydsadn ;
		(dnptr!=NULLDN) && (mydnptr!=NULLDN) ;
		dnptr=dnptr->dn_parent, mydnptr=mydnptr->dn_parent) {
		if (rdn_cmp(dnptr->dn_rdn,mydnptr->dn_rdn) == 0)
			mb++;
	}

	if (ma != mb)
		return (ma > mb ? 1 : -1);

	/* check the DMD - NYI */
	return 0;	
}

static di_cmp (a,b)
struct di_block *a, *b;
    /*
    *  Select best di_block
    *    rule 1: deferred dsa infos have lowest preference, 
    *		 complete have highest.
    *
    *  If two block have same state, select using
    *    preference 1: quipu DSAs with quipu context
    *    preference 2: quipu DSAs
    *    preference 3: reliable DSAs
    *    preference 4: local DSAs
    */
{
int x,y;

	if (a->di_state != b->di_state)
		return (a->di_state > b->di_state ? -1 : 1);	/* rule 1 */

	switch (a->di_state) {
	case DI_DEFERRED:
		break;	
	case DI_ACCESSPOINT:
		if ((x = di_cmp_address(a,b)) != 0) {
			DLOG (log_dsap,LLOG_TRACE,("AP selected on address"));
			return x;	/* preference 4 - no info to asses 1,2 or 3 */
		}
		break;
	case DI_COMPLETE:
		x = quipu_ctx_supported (a->di_entry);
		y = quipu_ctx_supported (b->di_entry);
		if ( x != y ) {
			DLOG (log_dsap,LLOG_TRACE,("DSA selected on context"));
			return ( x > y ? 1 : -1);	/* preference 1 or 2 */
		}

		if ((x=di_cmp_reliability (a,b)) != 0) {
			DLOG (log_dsap,LLOG_TRACE,("DSA selected on relibility"));
			return x;			/* preference 3 */
		}

		if ((x=di_cmp_address(a,b)) != 0) {
			DLOG (log_dsap,LLOG_TRACE,("DSA selected on address"));
			return x;		/* preference 4 */
		}

		break;
	}

	return (di_prefer_dsa(a->di_dn, b->di_dn));
}


sort_dsa_list (dsas)
struct di_block	**dsas;
{
struct di_block	*trail;
struct di_block	*old_di, *new_di;
struct di_block *result;
char changed = FALSE;

	if (dsas == NULL)
		return;

	/* turn access point into complete references if possible */
	di_ap2comp (dsas);

	if (*dsas == NULL_DI_BLOCK)
		return;

	result = *dsas;
	if ((old_di = result->di_next) == NULL_DI_BLOCK)
		return;		/* only 1 - must be sorted !!! */

	result->di_next = NULL_DI_BLOCK;

	for(; old_di != NULL_DI_BLOCK; ) {
		trail = NULL_DI_BLOCK;
		for(new_di = result; new_di != NULL_DI_BLOCK; new_di= new_di->di_next) {
			if ( di_cmp (old_di,new_di) > 0 ) {
				if (trail == NULL_DI_BLOCK) {
					result = old_di;
					old_di = old_di->di_next;
					result->di_next = new_di;
				} else {
					trail->di_next = old_di;
					old_di = old_di->di_next;
					trail->di_next->di_next = new_di;
				}
				changed = TRUE;
				break;
			}
			trail = new_di;
		} 
		if (new_di == NULL_DI_BLOCK) {
			trail->di_next = old_di;
			if (old_di) {
				old_di = old_di->di_next;
				trail->di_next->di_next = NULL_DI_BLOCK;
			}
		} 
	}
	*dsas = result;

	if (changed) {
		LLOG (log_dsap,LLOG_TRACE,("DSA order changed"));
#ifdef DEBUG
		di_list_log (result);
#endif
	} else
		DLOG (log_dsap,LLOG_TRACE,("DSA order not changed"));

}

static int	  common_address (a,tb)
struct di_block *a;
struct TSAPaddr *tb;
{
struct TSAPaddr *ta;
struct NSAPaddr *na;
struct NSAPaddr *nb;
int x,y;

	/* select DSA with best looking address !!! */

	if (a->di_state == DI_DEFERRED)
		return FALSE;

	if (a->di_state == DI_COMPLETE)
		ta = &(a->di_entry->e_dsainfo->dsa_addr->pa_addr.sa_addr);
	else
		/* Use 1 access point only */
		ta = &(a->di_accesspoints->ap_address->pa_addr.sa_addr);

	/* compare ta and tb to see if they have a network in common */
	for (na=ta->ta_addrs , x = ta->ta_naddr - 1 ;
			x >= 0;
			na++, x-- ) {
		for (nb=tb->ta_addrs , y = tb->ta_naddr - 1 ;
				y >= 0;
				nb++, y-- ) {
			if (na->na_community == nb->na_community)
				return TRUE;
				}
			}
	return FALSE;
}


struct di_block * select_refer_dsa(di,tk)
struct di_block *di;
struct task_act *tk;
{
struct di_block *best;
struct di_block *loop;
Entry eptr;
struct TSAPaddr *ta;
DN rdsa;

	/* return the di block of the best DSA the other end should be
	   to contact...
	   If it can't contact any - return NULL
        */

	if (di != NULL_DI_BLOCK)
		best = di;
	else
		best = NULL_DI_BLOCK;

	/* First set - find out who the remote end is... */
	if (tk->tk_conn->cn_ctx == DS_CTX_X500_DAP)
		return best;	/* we will chain anyway - unless prevented by service control... */

	rdsa = tk->tk_conn->cn_dn;
	if ((eptr=local_find_entry (rdsa,FALSE)) == NULLENTRY)
		return best;	/* no way of knowing */

	if (eptr->e_dsainfo == NULLDSA) 
		return best;	/* no way of knowing */

	ta = &(eptr->e_dsainfo->dsa_addr->pa_addr.sa_addr);
	ta = ta2norm (ta);	/* calculate subnets... */

	for (loop=di; loop!=NULL_DI_BLOCK; loop=loop->di_next)
		if (common_address (loop,ta))
			return loop;

	/* nothing on the same network - chain if possible !!! */
	return NULL_DI_BLOCK;	
}

di_rdns (di,rdns,aliases)
struct di_block * di;
int rdns, aliases;
{
register struct di_block *loop;

	for (loop=di; loop!=NULL_DI_BLOCK; loop=loop->di_next) {
		di->di_rdn_resolved = rdns;
		di->di_aliasedRDNs = aliases;
	}
}


