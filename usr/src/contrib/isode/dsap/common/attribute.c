/* attribute.c - */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/attribute.c,v 7.5 91/02/22 09:18:23 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/attribute.c,v 7.5 91/02/22 09:18:23 mrose Interim $
 *
 *
 * $Log:	attribute.c,v $
 * Revision 7.5  91/02/22  09:18:23  mrose
 * Interim 6.8
 * 
 * Revision 7.4  90/10/17  11:53:15  mrose
 * sync
 * 
 * Revision 7.3  90/04/18  08:49:44  mrose
 * 6.2
 * 
 * Revision 7.2  90/01/11  23:55:49  mrose
 * lint
 * 
 * Revision 7.1  90/01/11  18:37:17  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  22:16:38  mrose
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
#include "quipu/ds_error.h"
#include "quipu/oid.h"
#include "quipu/malloc.h"

AttributeType at_certificate;
AttributeType at_p_password;
AttributeType at_password;
AttributeType at_acl;
AttributeType at_control;
AttributeType at_schema;
AttributeType at_applctx;
AttributeType at_edbinfo;
AttributeType at_masterdsa;
AttributeType at_slavedsa;
AttributeType at_relaydsa;
AttributeType at_dsaaddress;
AttributeType at_version;
AttributeType at_inherit;
AttributeType at_subord;
AttributeType at_xref;
AttributeType at_nssr;
AttributeType at_listen;
OID alias_oc;
OID quipu_dsa_oid;
OID extern_obj_oid;

extern AttributeType at_objectclass;
extern AttributeType at_alias;
extern LLog * log_dsap;

extern short acl_sntx;

extern AV_Sequence oc_avs();
Attr_Sequence entry_find_type();

check_dsa_known_oids ()
{
	/* set pointers to special attributes */

	check_known_oids ();

	at_password 	= AttrT_new (PASSWORD_OID);
	at_control 	= AttrT_new (CONTROL_OID);
	at_acl 		= AttrT_new (ACL_OID);
	at_applctx 	= AttrT_new (APPLCTX_OID);
	at_schema 	= AttrT_new (SCHEMA_OID);
	at_edbinfo 	= AttrT_new (EDBINFO_OID);
	at_masterdsa 	= AttrT_new (MASTERDSA_OID);
	at_slavedsa 	= AttrT_new (SLAVEDSA_OID);
	at_relaydsa 	= AttrT_new (RELAYDSA_OID);
	at_dsaaddress 	= AttrT_new (DSAADDRESS_OID);
	at_version 	= AttrT_new (VERSION_OID);
	at_p_password 	= AttrT_new (PROTECTED_OID);
	at_certificate 	= AttrT_new (CERTIFICATE_OID);
	at_inherit 	= AttrT_new (INHERIT_OID);

	at_subord 	= AttrT_new (SUBORD_OID);
	at_xref 	= AttrT_new (XREF_OID);
	at_nssr 	= AttrT_new (NSSR_OID);
	at_listen 	= AttrT_new (LISTEN_OID);

	alias_oc = oid_cpy (str2oid (ALIAS_OC));
	quipu_dsa_oid = oid_cpy (str2oid(QUIPU_DSA));
	extern_obj_oid = oid_cpy (str2oid(EXTERNOBJECT));

}

real_unravel_attribute (eptr,error)
Entry eptr;
struct DSError * error;
{
register Attr_Sequence as;
Attr_Sequence ias;
RDN new_rdn, rdn_test;
AttributeType at;
AV_Sequence   avs;
struct acl * acl = (struct acl *) NULL;
extern oid_cmp ();
int rdn_print();

	/* take rdn's and make sure an attribute, if not add it in */
    for (new_rdn = eptr->e_name; new_rdn != NULLRDN; new_rdn = new_rdn->rdn_next) {
	if (new_rdn->rdn_at != NULLTABLE_ATTR)
		new_rdn->rdn_av.av_syntax = new_rdn->rdn_at->oa_syntax;

	if (new_rdn->rdn_av.av_syntax == 0) {
		/* Check we know about local RDNs syntax */
		if (eptr->e_data == E_DATA_MASTER) {
			LLOG (log_dsap, LLOG_EXCEPTIONS, ("RDN of unknown attribute type"));
			error->dse_type = DSE_UPDATEERROR;
			error->ERR_UPDATE.DSE_up_problem = DSE_UP_NAMINGVIOLATION;
			return NOTOK;
		}
	}

	for (rdn_test = eptr->e_name; rdn_test != new_rdn; rdn_test = rdn_test->rdn_next)
		/* check for repeated attribute in RDN */
		if (AttrT_cmp (new_rdn->rdn_at, rdn_test->rdn_at) == 0) {
			LLOG (log_dsap, LLOG_EXCEPTIONS, ("RDN with two AVAs of same attribute type"));
			error->dse_type = DSE_UPDATEERROR;
			error->ERR_UPDATE.DSE_up_problem = DSE_UP_NAMINGVIOLATION;
			return NOTOK;
		}
			
	if ((as = as_find_type (eptr->e_attributes,new_rdn->rdn_at)) == NULLATTR) {
		SET_HEAP (new_rdn->rdn_at);
		at  = AttrT_cpy (new_rdn->rdn_at);
		avs = avs_comp_new (AttrV_cpy(&new_rdn->rdn_av));
		as  = as_comp_new (at, avs, NULLACL_INFO);
		eptr->e_attributes = as_merge (eptr->e_attributes,as);
		RESTORE_HEAP;
	} else {
		for (avs=as->attr_value; avs!=NULLAV; avs=avs->avseq_next) 
			if (AttrV_cmp (&new_rdn->rdn_av,&avs->avseq_av) == 0)
				break;
		if (avs == NULLAV) {
			SET_HEAP (new_rdn->rdn_at);
			avs = avs_comp_new (AttrV_cpy(&new_rdn->rdn_av));
			as->attr_value = avs_merge (as->attr_value,avs);
			RESTORE_HEAP;
		}
	}
    }

	/* now get special attributes into structure */
	/* first reset pointers - incase deleted. */

	eptr->e_alias = NULLDN;
	eptr->e_dsainfo = NULLDSA;
	eptr->e_external = 0;
	eptr->e_master = NULLAV;
	eptr->e_slave = NULLAV;
	eptr->e_leaf = TRUE;
	eptr->e_oc = NULLAV;

	if ((as = entry_find_type (eptr,at_objectclass)) == NULLATTR) {
		/* Might inherit it ! */
		set_inheritance (eptr);
		if ((as = entry_find_type (eptr,at_objectclass)) == NULLATTR) {
			LLOG (log_dsap, LLOG_EXCEPTIONS, ("Object class attribute missing"));
			error->dse_type = DSE_UPDATEERROR;
			error->ERR_UPDATE.DSE_up_problem = DSE_UP_OBJECTCLASSVIOLATION;
			return (NOTOK);
		}
		eptr->e_oc = as->attr_value;
	} else {
		set_inheritance (eptr);
		eptr->e_oc = as->attr_value;
	/* order swapped ! */
	}

	if (as = entry_find_type (eptr,at_acl)) {
		eptr->e_acl = (struct acl *) as->attr_value->avseq_av.av_struct;
		acl = eptr->e_acl;
		if (acl->ac_child == NULLACL_INFO)
			acl->ac_child = acl_default ();
		if (acl->ac_entry == NULLACL_INFO)
			acl->ac_entry = acl_default ();
		if (acl->ac_default == NULLACL_INFO)
			acl->ac_default = acl_default ();
	}

	if (as = entry_find_type (eptr,at_edbinfo)) {
		if (eptr->e_dsainfo == NULLDSA) {
			eptr->e_dsainfo = (struct dsa_info *) smalloc (sizeof (struct dsa_info));
			bzero ((char *)eptr->e_dsainfo,sizeof (struct dsa_info));
		}
		eptr->e_dsainfo->dsa_attr = as->attr_value;
	}

	if (as = entry_find_type (eptr,at_dsaaddress)) {
		if (eptr->e_dsainfo == NULLDSA) {
			eptr->e_dsainfo = (struct dsa_info *) smalloc (sizeof (struct dsa_info));
			bzero ((char *)eptr->e_dsainfo,sizeof (struct dsa_info));
		}
		eptr->e_dsainfo->dsa_addr = (struct PSAPaddr *) as->attr_value->avseq_av.av_struct;
	}

	if (as = entry_find_type (eptr,at_masterdsa)) {
		eptr->e_master = as->attr_value;
		eptr->e_leaf = FALSE;
	}

	if (as = entry_find_type (eptr,at_slavedsa)) {
		eptr->e_slave = as->attr_value;
		eptr->e_leaf = FALSE;
	}

	if (as = entry_find_type (eptr,at_alias))
		eptr->e_alias = (DN) as->attr_value->avseq_av.av_struct;

	if (eptr->e_dsainfo != NULLDSA) /* set version number */
		if (as = entry_find_type (eptr,at_version))
			eptr->e_dsainfo->dsa_version = (char *) as->attr_value->avseq_av.av_struct;

	if (eptr->e_leaf) {
		/* still a leaf -> look for any external references */
		if (as = entry_find_type (eptr,at_xref)) {
			eptr->e_reference = as->attr_value;
			eptr->e_reftype = RT_CROSS;
			eptr->e_external = TRUE;
			eptr->e_leaf = FALSE;
			eptr->e_allchildrenpresent = FALSE;
			if (as->attr_value->avseq_next)
				pslog (log_dsap,LLOG_EXCEPTIONS,
				       "Multi valued cross reference",
				       rdn_print, (caddr_t)eptr->e_name);
		}
		if (as = entry_find_type (eptr,at_subord)) {
			if (eptr->e_external)
				pslog (log_dsap,LLOG_EXCEPTIONS,
				       "cross & subordinate reference",
				       rdn_print, (caddr_t)eptr->e_name);
			eptr->e_reference = as->attr_value;
			eptr->e_reftype = RT_SUBORDINATE;
			eptr->e_external = TRUE;
			eptr->e_leaf = FALSE;
			eptr->e_allchildrenpresent = FALSE;
			if (as->attr_value->avseq_next)
				pslog (log_dsap,LLOG_EXCEPTIONS,
				       "Multi valued subordinate reference",
				       rdn_print, (caddr_t)eptr->e_name);
		}
		if (as = entry_find_type (eptr,at_nssr)) {
			if (eptr->e_external)
				pslog (log_dsap,LLOG_EXCEPTIONS,
				       "NSSR & cross | subordinate reference",
				       rdn_print, (caddr_t)eptr->e_name);
			eptr->e_reference = as->attr_value;
			eptr->e_reftype = RT_NONSPECIFICSUBORDINATE;
			eptr->e_external = TRUE;
			eptr->e_leaf = FALSE;
			eptr->e_allchildrenpresent = FALSE;
		}
	}

	/* Make sure acl attribute exists */
	if (eptr->e_acl == (struct acl *) NULL) {
		Attr_Sequence as1;
		AV_Sequence avs1;
		AttributeValue av;

		SET_HEAP (at_acl);

		acl = acl_alloc();
		eptr->e_acl = acl;
		acl->ac_child = acl_default ();
		acl->ac_entry = acl_default ();
		acl->ac_default = acl_default ();
		acl->ac_attributes = NULLACL_ATTR;
		av = AttrV_alloc();
		av->av_syntax = acl_sntx;
		av->av_struct = (caddr_t) acl;
		avs1 = avs_comp_new (av);
		as1 = as_comp_new (AttrT_cpy(at_acl),avs1,NULLACL_INFO);
		eptr->e_attributes = as_merge(eptr->e_attributes,as1);

		RESTORE_HEAP;
	}

	/* now do the attribute acl */
	/* first of all create and oid_seq of all attribute, and point them to */
	/* the default.  */
	if (acl->ac_attributes == NULLACL_ATTR) {
		/* the easy case !!! - set every attribute to ac_default */
		for ( as = eptr->e_attributes; as != NULLATTR; as = as->attr_link) 
			as->attr_acl = acl->ac_default;
	} else {
		register struct acl_attr * aa;
		struct acl_attr * found_aa;
		register struct oid_seq * oidptr;
		char once;

		/* The following is probably in efficient */
		/* There must be a better way of setting these pointers */
		for ( as = eptr->e_attributes; as != NULLATTR; as = as->attr_link) {
			found_aa = NULLACL_ATTR;
			once = FALSE;

			for ( aa = acl->ac_attributes; aa!=NULLACL_ATTR; aa=aa->aa_next) {
				for ( oidptr=aa->aa_types;oidptr != NULLOIDSEQ; oidptr=oidptr->oid_next) {
					if (oid_cmp (oidptr->oid_oid,grab_oid(as->attr_type)) == 0) {
						if (once == TRUE)
							pslog (log_dsap,LLOG_EXCEPTIONS,
								"WARNING Inconsistent ACL in entry",
								rdn_print,
							        (caddr_t)eptr->e_name);
						else
							once = TRUE;
						found_aa = aa;
					}
				}
			}

			if (found_aa != NULLACL_ATTR) 
				/* found the apprioriate acl - add oid to it */
				as->attr_acl = found_aa->aa_acl;
			else
				as->attr_acl = acl->ac_default;
		}
	}

	if (ias = entry_find_type (eptr,at_inherit)) {
		/* set inherit ACL pointers */
		InheritAttr tmp;
		Attr_Sequence nas;

		eptr->e_inherit = avs_cpy (ias->attr_value);

		for (avs=eptr->e_inherit; avs!=NULLAV; avs=avs->avseq_next) {
			tmp = (InheritAttr) avs->avseq_av.av_struct;
			for (as = tmp->i_always; as != NULLATTR; as=as->attr_link) {
				if (  (AttrT_cmp (as->attr_type,at_acl) == 0)
				   || (AttrT_cmp (as->attr_type,at_masterdsa) == 0)
				   || (AttrT_cmp (as->attr_type,at_slavedsa) == 0)
				   || (AttrT_cmp (as->attr_type,at_relaydsa) == 0)
				   || (AttrT_cmp (as->attr_type,at_schema) == 0)
				   || (AttrT_cmp (as->attr_type,at_applctx) == 0)
				   || (AttrT_cmp (as->attr_type,at_objectclass) == 0)
				   || (AttrT_cmp (as->attr_type,at_edbinfo) == 0)
				   || (AttrT_cmp (as->attr_type,at_dsaaddress) == 0)
				   || (AttrT_cmp (as->attr_type,at_alias) == 0)
				   || (AttrT_cmp (as->attr_type,at_inherit) == 0)) {
					LLOG(log_dsap,LLOG_EXCEPTIONS,("Inherited system attribute only allowed in default case"));
					error->ERR_ATTRIBUTE.DSE_at_name = get_copy_dn (eptr);
					error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_what =DSE_AT_CONSTRAINTVIOLATION;
					error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_type = AttrT_cpy (as->attr_type);
					error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_value = NULLAttrV;
					error->ERR_ATTRIBUTE.DSE_at_plist.dse_at_next = DSE_AT_NOPROBLEM;
					return (DS_ERROR_REMOTE);
				   }
				as->attr_acl = ias->attr_acl;
				if (as->attr_value == NULLAV) {
					if ((nas = entry_find_type (eptr,as->attr_type)) == NULLATTR) {
						LLOG(log_dsap,LLOG_EXCEPTIONS,("Value missing for always inherited attribute type"));
						error->ERR_ATTRIBUTE.DSE_at_name = get_copy_dn (eptr);
						error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_what =DSE_AT_NOSUCHATTRIBUTE;
						error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_type = AttrT_cpy (as->attr_type);
						error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_value = NULLAttrV;
						error->ERR_ATTRIBUTE.DSE_at_plist.dse_at_next = DSE_AT_NOPROBLEM;
						return (DS_ERROR_REMOTE);
					}
					as->attr_value = avs_cpy(nas->attr_value);
				}
			}
			for (as = tmp->i_default; as != NULLATTR; as=as->attr_link) {
				as->attr_acl = ias->attr_acl;
				if (as->attr_value == NULLAV) {
					if ((nas = entry_find_type (eptr,as->attr_type)) == NULLATTR) {
						LLOG(log_dsap,LLOG_EXCEPTIONS,("Value missing for default inherited attribute type"));
						error->ERR_ATTRIBUTE.DSE_at_name = get_copy_dn (eptr);
						error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_what =DSE_AT_NOSUCHATTRIBUTE;
						error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_type = AttrT_cpy (as->attr_type);
						error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_value = NULLAttrV;
						error->ERR_ATTRIBUTE.DSE_at_plist.dse_at_next = DSE_AT_NOPROBLEM;
						return (DS_ERROR_REMOTE);
					}
					as->attr_value = avs_cpy(nas->attr_value);
				}
			}
		}
	}

	return (OK);
}


set_inheritance (eptr)
Entry eptr;
{
AV_Sequence avs;
InheritAttr tmp;
Attr_Sequence as;

	if ((eptr == NULLENTRY)
		|| (eptr->e_parent == NULLENTRY)
		|| (eptr->e_parent->e_inherit == NULLAV))
		return;

	eptr->e_iattr = NULLINHERIT;

	if ((eptr->e_oc == NULLAV) &&
	   ((as = entry_find_type (eptr,at_objectclass)) != NULLATTR))
		eptr->e_oc = as->attr_value;

	if (eptr->e_oc == NULLAV)
		for (avs=eptr->e_parent->e_inherit; avs != NULLAV; avs=avs->avseq_next) {
			tmp = (InheritAttr) avs->avseq_av.av_struct;
			if (tmp->i_oid == NULLOID) {
				eptr->e_iattr = tmp;
				return;
			}
		}

	/* scan through inherit atribute looking of the right object class */
	for (avs=eptr->e_parent->e_inherit; avs != NULLAV; avs=avs->avseq_next) {
		tmp = (InheritAttr) avs->avseq_av.av_struct;
		if (check_in_oc (tmp->i_oid,eptr->e_oc)) {
			eptr->e_iattr = tmp;
			return;
		}
	}
	return;

}


Attr_Sequence entry_find_type (a,b)
Entry a;
AttributeType b;
{
register int i;
register Attr_Sequence ptr;

	/* if Attr_cmp returns <0 no point in continuing due to ordering */

	for(ptr = a->e_attributes; ptr != NULLATTR; ptr=ptr->attr_link) {
		if (  (i = AttrT_cmp (ptr->attr_type,b)) <= 0)
			if ( i == 0 )
				return ptr;
			else 
				break;
	}

	if (a->e_iattr == NULLINHERIT)
		return (NULLATTR);

	for(ptr = a->e_iattr->i_default; ptr != NULLATTR; ptr=ptr->attr_link) {
		if (  (i = AttrT_cmp (ptr->attr_type,b)) <= 0)
			if ( i == 0 )
				return ptr;
			else 
				break;
	}
	for(ptr = a->e_iattr->i_always; ptr != NULLATTR; ptr=ptr->attr_link) {
		if (  (i = AttrT_cmp (ptr->attr_type,b)) <= 0)
			return (i ? NULLATTR : ptr);
	}

	return (NULLATTR);
}
