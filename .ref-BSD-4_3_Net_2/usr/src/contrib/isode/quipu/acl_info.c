/* acl_info.c - ? */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/acl_info.c,v 7.2 91/02/22 09:38:24 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/acl_info.c,v 7.2 91/02/22 09:38:24 mrose Interim $
 *
 *
 * $Log:	acl_info.c,v $
 * Revision 7.2  91/02/22  09:38:24  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:53:13  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:16:37  mrose
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
#include "quipu/entry.h"

extern AV_Sequence super_user;
extern LLog * log_dsap;
extern int dn_print ();

check_acl (who,mode,acl,node)
register DN     who;
register int    mode;
struct acl_info *acl;
DN     node;
{
register struct acl_info *ptr;
AV_Sequence avs;

	for (ptr=acl; ptr!= NULLACL_INFO; ptr=ptr->acl_next) {
		switch (ptr->acl_selector_type) {
		case ACL_ENTRY:
			if ( mode <= ptr->acl_categories ) {
				if (who == NULLDN) {
					break;
				} if (dn_cmp (who,node) == OK)
					return (OK);
				}
			break;
		case ACL_OTHER:
			if ( mode <= ptr->acl_categories )
				return (OK);
			break;
		case ACL_PREFIX:
			if ( mode <= ptr->acl_categories ) {
				if ( who == NULLDN)
					break;
				if (check_dnseq_prefix (ptr->acl_name,who) == OK)
					return (OK);
				}
			break;
		case ACL_GROUP:
			if ( mode <= ptr->acl_categories ) {
				if ( who == NULLDN) {
					break;
				}
				if (check_dnseq (ptr->acl_name,who) == OK)
					return (OK);
				}
			break;
		}
	}

	/* one last try for access */
	for (avs=super_user; avs != NULLAV;  avs=avs->avseq_next) 
		if ( dn_cmp (who,(DN) avs->avseq_av.av_struct) == OK)
			return (OK);

#ifdef DEBUG
	if (log_dsap -> ll_events & LLOG_TRACE) {
		pslog (log_dsap,LLOG_TRACE,"access denied for user ",
		       dn_print,(caddr_t)who);
		LLOG (log_dsap,LLOG_TRACE,("  attempting mode=%d", mode));
		pslog (log_dsap,LLOG_TRACE,"  on entry ",dn_print,(caddr_t)node);
	}
#endif

	return (NOTOK);
}


manager (dn)
DN dn;
{
register AV_Sequence avs;

	for (avs=super_user; avs != NULLAV;  avs=avs->avseq_next) 
		if ( dn_cmp (dn,(DN) avs->avseq_av.av_struct) == OK)
			return (TRUE);

	return (FALSE);
}
