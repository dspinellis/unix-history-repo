/* ds_init.c - initialise the DSA */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/ds_init.c,v 7.5 91/03/09 11:56:37 mrose Exp $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/ds_init.c,v 7.5 91/03/09 11:56:37 mrose Exp $
 *
 *
 * $Log:	ds_init.c,v $
 * Revision 7.5  91/03/09  11:56:37  mrose
 * update
 * 
 * Revision 7.4  91/02/22  09:38:48  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/10/17  11:53:43  mrose
 * sync
 * 
 * Revision 7.2  90/07/09  14:45:42  mrose
 * sync
 * 
 * Revision 7.1  90/03/15  11:18:49  mrose
 * quipu-sync
 * 
 * Revision 7.0  89/11/23  22:17:08  mrose
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


#include "quipu/config.h"
#include "quipu/util.h"
#include "quipu/read.h"
#include "quipu/dua.h"
#include "quipu/connection.h"
#include "quipu/entry.h"

AV_Sequence super_user;
Entry subtree_load ();
Entry make_path ();
extern LLog * log_dsap;
extern DN mydsadn;
extern Entry database_root;
static Entry load_dsa_cache_entry(), load_dsa_remote_entry();
extern Attr_Sequence entry_find_type();
extern Attr_Sequence dsa_real_attr;
extern char * mydsaname;
extern char * treedir;
extern char * get_entry_passwd();
extern char * new_version();
extern char * quipuversion, * TidyString();
extern int parse_status;
extern IFP unrav_fn;
extern IFP schema_fn;
extern IFP restart_fn;
extern AttributeType at_version;
extern SFD attempt_restart ();
extern time_t	time();
time_t	timenow;

dsa_init ()
{
Attr_Sequence as, get_cacheEDB();
AttributeType manager;
DN str2dn();
struct edb_info * dsainfo;
AV_Sequence avs;
Entry newentry;
Entry my_entry, rem_entry;
int real_unravel_attribute ();
int real_check_schema ();
char loadstate = TRUE;
struct DSError error;
#ifdef TURBO_AVL
Entry akid;
#endif

	check_dsa_known_oids ();

	unrav_fn = (IFP) real_unravel_attribute;
	schema_fn = (IFP) real_check_schema;
	restart_fn = (IFP) attempt_restart;

	if (( manager = AttrT_new (MANAGER_OID)) == NULLAttrT)
		fatal (-1,"Manager - unknown attribute - check oid tables");

	LLOG (log_dsap,LLOG_NOTICE,("dsa name %s",mydsaname));

	if ((mydsadn = str2dn (mydsaname)) == NULLDN) {
		fatal (-2,"Invalid dsa name");
	}

	if ((my_entry = load_dsa_cache_entry (mydsadn)) == NULLENTRY) {
		if (parse_status != 0)
			fatal (-3,"can't load my entry");

		if (database_root != NULLENTRY) 
			fatal (-4,"Found EDB - but I'm not in it!");

		LLOG (log_dsap,LLOG_NOTICE,("Can't find my own entry - trying another DSA"));
		if ((my_entry = load_dsa_remote_entry (mydsadn)) == NULLENTRY)
			fatal (-4,"can't get my own entry!");
		else {
#ifdef TURBO_DISK
			if (turbo_write(my_entry) != OK)
				fatal (-4,"Can't write my entry");
#else
			if (journal (my_entry) != OK)
				fatal (-4,"Can't write my entry");
#endif
		}
	} else if (my_entry->e_data == E_TYPE_CACHE_FROM_MASTER)
		if ((rem_entry = load_dsa_remote_entry (mydsadn)) != NULLENTRY) {
			if (rem_entry->e_parent != NULLENTRY)
				rem_entry->e_parent->e_edbversion = new_version();
#ifdef TURBO_DISK
			if (turbo_write(rem_entry) != OK)
				fatal (-4,"Can't re-write my entry");
#else
			if (journal (rem_entry) != OK)
				fatal (-4,"Can't re-write my entry");
#endif
			my_entry = rem_entry;
		}

	if (get_entry_passwd (my_entry->e_attributes) == NULLCP) 
		/* This is not a fatal error, but some remote operations may fail */
		LLOG(log_dsap,LLOG_EXCEPTIONS,("Can't find my own PASSWORD"));

	if (dsa_real_attr) {
		if (as_cmp (my_entry->e_attributes,dsa_real_attr) != 0) {
			LLOG (log_dsap,LLOG_EXCEPTIONS,("DSA entry inconsistent - assuming EDB wrong, DSA file correct"));
			as_free (my_entry->e_attributes);
			my_entry->e_attributes = as_cpy (dsa_real_attr);
			if (unravel_attribute (my_entry,&error) != OK) 
				fatal (-82,"DSA entry bad error");
		}
	} else
		dsa_real_attr = as_cpy (my_entry->e_attributes);

	if (my_entry->e_dsainfo != NULLDSA) {
		/* get manager attribute */
		if ((as = entry_find_type(my_entry,manager)) == NULLATTR )
			fatal (-5,"Manager attribute missing in my own entry");
		AttrT_free (manager);
		super_user = avs_cpy ((AV_Sequence)as->attr_value);

		if (quipu_ctx_supported(my_entry) != 2)
			LLOG(log_dsap,LLOG_EXCEPTIONS,(
			"WARNING: I don't appear to support the QUIPU Context !!!"));
		my_entry->e_dsainfo->dsa_version = 
			TidyString (strdup (quipuversion));
		if (as = entry_find_type (my_entry,at_version)) 
			if ( strcmp (
			     (char *) as->attr_value->avseq_av.av_struct,
			     my_entry->e_dsainfo->dsa_version) != 0) {

			if (as->attr_value->avseq_av.av_struct)
				free (as->attr_value->avseq_av.av_struct);
			as->attr_value->avseq_av.av_struct = 
				(caddr_t) strdup (my_entry->e_dsainfo->dsa_version);

			if (as = as_find_type (dsa_real_attr,at_version)) {
				if (as->attr_value->avseq_av.av_struct)
					free (as->attr_value->avseq_av.av_struct);
				as->attr_value->avseq_av.av_struct = 
				    (caddr_t) strdup (my_entry->e_dsainfo->dsa_version);
			}

			if (parse_status == 0) 
			 if (my_entry->e_data == E_DATA_MASTER) {
			    if (my_entry->e_parent != NULLENTRY)
				my_entry->e_parent->e_edbversion = new_version();
			    LLOG (log_dsap,LLOG_NOTICE,("Updating version number"));
#ifdef TURBO_DISK
				if (turbo_write(my_entry) != OK)
					fatal (-33,"self rewrite failed - check database");
#else
#ifdef TURBO_AVL
			        akid = (Entry) avl_getone(my_entry->e_parent->e_children);
			        if (journal (akid) != OK)
#else
			        if (journal (my_entry->e_parent->e_child) != OK)
#endif
					fatal (-33,"self rewrite failed - check database");
#endif
		         } else {
				write_dsa_entry(my_entry);
			 }
		}

	} else 
		fatal (-6,"No edbinfo attribute in my own entry");

	if (parse_status != 0)
		loadstate = FALSE;

	for (avs = my_entry->e_dsainfo->dsa_attr ; avs != NULLAV; avs=avs->avseq_next) {
		if (avs->avseq_av.av_struct == NULL)
			continue;
		dsainfo = (struct edb_info *) avs->avseq_av.av_struct;
		if ((newentry = make_path (dsainfo->edb_name)) == NULLENTRY)
			continue;

		(void) subtree_load (newentry,dsainfo->edb_name);
		if (parse_status != 0)
			loadstate = FALSE;
	}

	if (loadstate == FALSE)
		fatal (-7,"DSA Halted");

#ifdef TURBO_AVL
	if ((akid = (Entry) avl_getone(database_root->e_children))
	    != NULLENTRY )
		database_root->e_data = akid->e_data;
#else
	if (database_root->e_child != NULLENTRY)
		database_root->e_data = database_root->e_child->e_data;
#endif

	/* Load cached EDB files - if any */
	if ((as = get_cacheEDB()) != NULLATTR) {
		(void) time (&timenow);

		for (avs = as -> attr_value; avs != NULLAV; avs = avs -> avseq_next) {
			if ((newentry = make_path ((DN)avs->avseq_av.av_struct)) == NULLENTRY)
				continue;
			newentry = subtree_load (newentry,(DN)avs->avseq_av.av_struct);
			/* Should timestamp using version number ! */
			if (newentry)
				newentry->e_age = timenow;
		}
	}


	return (OK);

}

static Entry load_dsa_cache_entry(dn)
DN dn;
{
DN ptr,trail = NULLDN;
Entry newentry, res;
DN tmp;
DN tmp2;
int fail = FALSE;

	tmp = dn_cpy (dn);

	if (tmp->dn_parent == NULLDN) {
		database_root = subtree_load (NULLENTRY,NULLDN);
		dn_free (tmp);
		if (parse_status != 0)
			return NULLENTRY;
		if ((res = local_find_entry (dn,TRUE)) != NULLENTRY) 
			load_pseudo_attrs (res->e_data);
		return res;
	}

	database_root = subtree_load (NULLENTRY,NULLDN);
	if (parse_status != 0)
		fail = TRUE;

	for (ptr=tmp; ptr->dn_parent != NULLDN; ptr=ptr->dn_parent) {
		trail = ptr;
		tmp2 = trail->dn_parent;
		trail->dn_parent = NULLDN;
		newentry = make_path (tmp);
		(void) subtree_load (newentry,tmp);

		trail->dn_parent = tmp2;
	
		if (parse_status != 0) 
			fail = TRUE;
		
	}

	dn_free (tmp);

	if ((res = local_find_entry (dn,TRUE)) != NULLENTRY)
		load_pseudo_attrs (res->e_data);

	if (fail)
		return NULLENTRY;

	return res;
}

static Entry load_dsa_remote_entry(dn)
DN dn;
{
static struct ds_read_arg read_arg =
	{
		default_common_args,
		NULLDN,
		{       /* entry info selection */
			TRUE,
			NULLATTR,
			EIS_ATTRIBUTESANDVALUES
		}
	};
struct ds_read_result result;
struct DSError error;
extern Entry current_entry;
static struct ds_bind_arg bindarg;
static struct ds_bind_arg bindresult;
static struct ds_bind_error binderr;
int ad, id;
extern struct PSAPaddr *parent_psap();
struct PSAPaddr * addr;

#ifndef NO_STATS
extern LLog * log_stat;
extern dn_print ();

	pslog (log_stat,LLOG_NOTICE,"SYNC DAP remote entry lookup",dn_print,(caddr_t)dn);
#endif

	/* read from of remote DSA */
	/* Do synchronus read for now */
	/* Async read eventually */

	bzero ((char *)&bindarg, sizeof bindarg);

	make_dsa_bind_arg (&bindarg);

	if ((addr = parent_psap ()) == NULLPA) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("No parent or quipu-db defined - check quiputailor"));
		return (NULLENTRY);
	}
	
	if (dap_bind (&ad, &bindarg, &binderr, &bindresult,addr) != OK) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("Can't bind to remote DSA thus can't read my own entry"));
		bind_arg_free (&bindarg);
		return (NULLENTRY);
	}

	bind_arg_free (&bindarg);
	bind_arg_free (&bindresult);

	read_arg.rda_object = dn;

	id = 1;
	if (dap_read (ad, &id,&read_arg, &error, &result) != DS_OK) {
		log_ds_error (&error);
		LLOG (log_dsap,LLOG_EXCEPTIONS,("Remote DSA failed on lookup of my address"));
		(void) dap_unbind (ad);
		return (NULLENTRY);
	}

	cache_entry (&(result.rdr_entry), TRUE, EIS_ATTRIBUTESANDVALUES);

	if (unravel_attribute (current_entry,&error) != OK) {
		log_ds_error (&error);
		LLOG (log_dsap, LLOG_EXCEPTIONS, ("Remote copy of my entry is wrong!"));
		return (NULLENTRY);
	}

	(void) dap_unbind (ad);

	return (current_entry);
}


