/* pseudo.c -- Handle pseudo DSA attributes */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/pseudo.c,v 7.2 91/03/09 11:57:03 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/quipu/RCS/pseudo.c,v 7.2 91/03/09 11:57:03 mrose Exp $
 *
 *
 * $Log:	pseudo.c,v $
 * Revision 7.2  91/03/09  11:57:03  mrose
 * update
 * 
 * Revision 7.1  91/02/22  09:39:40  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/12/01  18:08:19  mrose
 * *** empty log message ***
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


#include "quipu/util.h"
#include "quipu/commonarg.h"
#include "quipu/entry.h"
#include "quipu/connection.h"
#include "tailor.h"
#include <sys/stat.h>
#include <errno.h>

#define cacheEDB  "0.9.2342.19200300.99.1.29"

Attr_Sequence dsa_pseudo_attr = NULLATTR;
Attr_Sequence dsa_real_attr = NULLATTR;
extern char * parse_file;
extern RDN parse_rdn;
extern LLog * log_dsap;
extern char * treedir;
struct dn_seq * dn_cached = NULLDNSEQ;
extern DN mydsadn;
#ifdef TURBO_DISK
extern Attr_Sequence fget_attributes ();
#else
extern Attr_Sequence get_attributes ();
#endif
extern Entry local_find_entry_aux();

update_pseudo_attr ()
{
/*
	Called just before dsa_pseudo_attr is referenced.
	Any dynamic changes should be reflected.
*/

	;
}

new_cacheEDB (dn)
DN dn;
{
Attr_Sequence as;
AttributeType at;
AttributeValue av;
AV_Sequence avs;
Entry e;
DN trail;

	at = AttrT_new (cacheEDB);

	av = AttrV_alloc ();
	av -> av_syntax = str2syntax ("DN");
	av -> av_struct = (caddr_t) dn_cpy (dn);

	/* remove last component of DN */
	for (dn = (DN)av->av_struct; dn->dn_parent != NULLDN; dn = dn->dn_parent)
		trail = dn;

	dn_comp_free (dn);
	trail->dn_parent = NULLDN;
	
	avs = avs_comp_new (av);

	if ((as = as_find_type (dsa_pseudo_attr,at)) == NULLATTR)
		dsa_pseudo_attr = as_comp_new (at,avs,NULLACL_INFO);
	else
		dsa_pseudo_attr = as_merge (dsa_pseudo_attr,
			as_comp_new (at,avs,NULLACL_INFO));

	if ((e = local_find_entry_aux (mydsadn,TRUE)) != NULLENTRY)
		write_dsa_entry (e);

}

Attr_Sequence get_cacheEDB ()
{
AttributeType at;

	at = AttrT_new (cacheEDB);

	return (as_find_type (dsa_pseudo_attr,at));
}

write_dsa_entry(eptr)
Entry eptr;
{
int um;
FILE * fptr;
char filename[LINESIZE];
PS ps;
	/* write e_attributes, and preserved attributes to DSA file */

	(void) sprintf (filename,"%sDSA.pseudo",isodefile(treedir,0));

	um = umask (0177);
	if ((fptr = fopen (filename,"w")) == (FILE *) NULL) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("can't write DSA pseudo entry: \"%s\" (%d)",filename,errno));
	}
	(void) umask (um);
	
	if ((ps = ps_alloc (std_open)) == NULLPS) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("ps_alloc failed"));
		(void) fclose (fptr);
		return;
	}
	if (std_setup (ps,fptr) == NOTOK) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("std_setup failed"));
		(void) fclose (fptr);
		return;
	}

	parse_file = filename;
	parse_rdn = eptr->e_name;

	update_pseudo_attr ();
	as_print (ps,dsa_pseudo_attr,EDBOUT);

	if (ps->ps_errno != PS_ERR_NONE) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("write DSA ps error: %s",ps_error(ps->ps_errno)));
		(void) fclose (fptr);
		return;
	}
	ps_free (ps);

	if (fflush (fptr) != 0) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("write DSA flush error: %d",errno));
		return;
	}
	if (fsync (fileno(fptr)) != 0) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("write DSA fsync error: %d",errno));
		return;
	}
	if (fclose (fptr) != 0) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("write DSA EDB close error: %d",errno));
		return;
	}

	LLOG (log_dsap,LLOG_NOTICE,("Written %s",filename));

	if (eptr->e_data == E_DATA_MASTER)
		return;

	(void) sprintf (filename,"%sDSA.real",isodefile(treedir,0));

	um = umask (0177);
	if ((fptr = fopen (filename,"w")) == (FILE *) NULL) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("can't write DSA pseudo entry: \"%s\" (%d)",filename,errno));
	}
	(void) umask (um);
	
	if ((ps = ps_alloc (std_open)) == NULLPS) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("ps_alloc failed"));
		(void) fclose (fptr);
		return;
	}
	if (std_setup (ps,fptr) == NOTOK) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("std_setup failed"));
		(void) fclose (fptr);
		return;
	}

	parse_file = filename;
	parse_rdn = eptr->e_name;

	if (dsa_real_attr)
		as_free (dsa_real_attr);
	dsa_real_attr = as_cpy (eptr->e_attributes);

	as_print (ps,dsa_real_attr,EDBOUT);

	if (ps->ps_errno != PS_ERR_NONE) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("write DSA ps error: %s",ps_error(ps->ps_errno)));
		(void) fclose (fptr);
		return;
	}
	ps_free (ps);

	if (fflush (fptr) != 0) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("write DSA flush error: %d",errno));
		return;
	}
	if (fsync (fileno(fptr)) != 0) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("write DSA fsync error: %d",errno));
		return;
	}
	if (fclose (fptr) != 0) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("write DSA EDB close error: %d",errno));
		return;
	}

	LLOG (log_dsap,LLOG_NOTICE,("Written %s",filename));

	
}

load_pseudo_attrs (data_type)
char data_type;
{
FILE * fptr;
char filename[LINESIZE];
DN dn;

	/* write e_attributes, and preserved attributes to DSA file */

	(void) sprintf (filename,"%sDSA.pseudo",isodefile(treedir,0));

	/* What if DSA at top level with same name as us !?! */
	parse_file = filename;
	if (mydsadn) 
		for (dn=mydsadn; dn != NULLDN; dn = dn->dn_parent)
			parse_rdn = dn->dn_rdn;

	if ((fptr = fopen (filename,"r")) == (FILE *) NULL) 
		LLOG (log_dsap,LLOG_TRACE,("No DSA pseudo entry: \"%s\" (%d)",filename,errno));
	else {

#ifdef TURBO_DISK
		if ((dsa_pseudo_attr = fget_attributes (fptr)) == NULLATTR)
#else
		if ((dsa_pseudo_attr = get_attributes (fptr)) == NULLATTR)
#endif
			LLOG (log_dsap,LLOG_TRACE,("Error in DSA pseudo entry: \"%s\" (%d)",filename,errno));

		(void) fclose (fptr);

	}

	if (data_type == E_DATA_MASTER)
		return;

	(void) sprintf (filename,"%sDSA.real",isodefile(treedir,0));

	if ((fptr = fopen (filename,"r")) == (FILE *) NULL) 
		LLOG (log_dsap,LLOG_TRACE,("No DSA real entry: \"%s\" (%d)",filename,errno)); 
	else {

#ifdef TURBO_DISK
		if ((dsa_real_attr = fget_attributes (fptr)) == NULLATTR)
#else
		if ((dsa_real_attr = get_attributes (fptr)) == NULLATTR)
#endif
			LLOG (log_dsap,LLOG_TRACE,("Error in DSA real entry: \"%s\" (%d)",filename,errno));
	}

	(void) fclose (fptr);
}
