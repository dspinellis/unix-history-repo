/* tree_struct.c - Tree Structure utility routines */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/tree_struct.c,v 7.2 91/02/22 09:20:34 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/tree_struct.c,v 7.2 91/02/22 09:20:34 mrose Interim $
 *
 *
 * $Log:	tree_struct.c,v $
 * Revision 7.2  91/02/22  09:20:34  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/03/15  11:17:42  mrose
 * quipu-sync
 * 
 * Revision 7.0  89/11/23  21:44:39  mrose
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


/* LINTLIBRARY */

#include "quipu/util.h"
#include "quipu/entry.h"
#include "quipu/syntaxes.h"

extern int oidformat;

/* ARGSUSED */
static tree_struct_free (ptr)
struct tree_struct * ptr;
{
	/* don't free objectclass - in static table */
	free ((char *)ptr);
}


static struct tree_struct * tree_struct_cpy (a)
struct tree_struct * a;
{
struct tree_struct * result;

	result = tree_struct_alloc ();
	result->tree_object = a->tree_object;
	return (result);
}

static tree_struct_cmp (a,b)
struct tree_struct * a, *b;
{
	if (a == NULLTREE)
		return (b==NULLTREE ? 0 : -1 );

	if (b == NULLTREE)
		return (1);

	return (  objclass_cmp(a->tree_object,b->tree_object));
}


/* ARGSUSED */
static tree_struct_print (ps,tree,format)
register PS ps;
struct   tree_struct * tree;
int format;
{
	ps_printf (ps,"%s",oc2name(tree->tree_object,oidformat));
}


static struct tree_struct * str2schema (str)
char * str;
{
struct tree_struct * ts;
objectclass * str2oc();

	ts = tree_struct_alloc ();
	if ((ts->tree_object = str2oc(str)) == NULLOBJECTCLASS) {
		parse_error ("invalid oid in schema '%s'",str);
		free ((char *) ts);
		return (NULLTREE);
	}
	return (ts);
}

static PE ts_enc (ts)
struct tree_struct * ts;
{
PE ret_pe;

        (void) encode_Quipu_TreeStructureSyntax(&ret_pe,0,0,NULLCP,ts);

	return (ret_pe);
}

static struct tree_struct * ts_dec (pe)
PE pe;
{
struct tree_struct * ts;

	if (decode_Quipu_TreeStructureSyntax(pe,1,NULLIP,NULLVP,&ts) == NOTOK) 
		return (struct tree_struct *)NULL;

	return (ts);
}

schema_syntax ()
{
	(void) add_attribute_syntax ("schema",
		(IFP) ts_enc,		(IFP) ts_dec,
		(IFP) str2schema,	tree_struct_print,
		(IFP) tree_struct_cpy,	tree_struct_cmp,
		tree_struct_free,	NULLCP,
		NULLIFP,		FALSE );
}
