/* inherit.c - inherit attribute */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/inherit.c,v 7.2 91/02/22 09:19:24 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/inherit.c,v 7.2 91/02/22 09:19:24 mrose Interim $
 *
 *
 * $Log:	inherit.c,v $
 * Revision 7.2  91/02/22  09:19:24  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:42:04  mrose
 * sync
 * 
 * Revision 7.0  90/08/08  08:34:04  mrose
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


/*
	SYNTAX
	inherit ::= [<objectclass> '$'] ["ALWAYS $"] <attribute>
	
*/

/* LINTLIBRARY */

#include "quipu/util.h"
#include "quipu/attrvalue.h"
#include "quipu/syntaxes.h"

static inherit_free (ptr)
InheritAttr ptr;
{
	oid_free (ptr->i_oid);
	as_free  (ptr->i_default);
	as_free  (ptr->i_always);

	free ((char *) ptr);
}


static InheritAttr inherit_cpy (a)
InheritAttr a;
{
InheritAttr result;

	result = (InheritAttr) smalloc (sizeof (*result));
	result->i_always  = as_cpy (a->i_always);
	result->i_default = as_cpy (a->i_default);
	result->i_oid	  = oid_cpy (a->i_oid);
	return (result);
}

static inherit_cmp (a,b)
InheritAttr a;
InheritAttr b;
{
int res;

	if (a == NULLINHERIT)
		if (b == NULLINHERIT)
			return (0);
		else 
			return (-1);

	if ( a->i_oid ) {
		if ( b->i_oid) {
			if ( (res = oid_cmp (a->i_oid,b->i_oid)) != 0) 
				return (res);
		} else
			return -1;
	} else if ( b->i_oid )
		return 1;

	if ( (res = as_cmp (a->i_always,b->i_always)) != 0)
		return (res);

	return (as_cmp (a->i_default,b->i_default));
}


static inherit_print (ps,inherit,format)
register PS ps;
InheritAttr inherit;
int format;
{
	if (format == READOUT) {
		if (inherit->i_oid) {
			if (inherit->i_always) {
				ps_printf (ps,"Object class %s, ",oid2name(inherit->i_oid, OIDPART));
				ps_print (ps,"ALWAYS:\n");
				as_print (ps,inherit->i_always,READOUT);
			}
			if (inherit->i_default) {
				if (inherit->i_always)
					ps_print (ps,"--------\t\t");
				ps_printf (ps,"Object class %s, ",oid2name(inherit->i_oid, OIDPART));
				ps_print (ps,"DEFAULT:\n");
				as_print (ps,inherit->i_default,READOUT);
			}
		} else {
			if (inherit->i_always) {
				ps_print (ps,"ALWAYS inherit:\n");
				as_print (ps,inherit->i_always,READOUT);
			}
			if (inherit->i_default) {
				if (inherit->i_always)
					ps_print (ps,"--------\t\t");
				ps_print (ps,"DEFAULT inheritance:\n");
				as_print (ps,inherit->i_default,READOUT);
			}
		}
		ps_print (ps,"--------");
			
	} else {
		if (inherit->i_oid)
			ps_printf (ps,"%s $ ",oid2name(inherit->i_oid, OIDPART));

		if (inherit->i_always) {
			ps_print (ps,"ALWAYS (\n");
			as_print (ps,inherit->i_always,EDBOUT);
			ps_print (ps,") ");
		}
		if (inherit->i_default) {
			ps_print (ps,"DEFAULT (\n");
			as_print (ps,inherit->i_default,EDBOUT);
			ps_print (ps,")");
		}
	}
}

static char * getInheritAttrs (asptr, needsoc)
Attr_Sequence * asptr;
char		needsoc;
{
Attr_Sequence as = NULLATTR;
Attr_Sequence as_combine ();
Attr_Sequence as_find_type();
static AttributeType octype = NULLAttrT;
char * ptr, * getnextline();

	if ((ptr = getnextline ()) == NULLCP) {
		parse_error ("Inherit: EOF unexpected",NULLCP);
		return (NULLCP);
	}

	if (*ptr == ')') {
		parse_error ("Attributes missing",NULLCP);
		return (++ptr);
	}

	while ( *ptr != 0 ) {
		as = as_combine (as,ptr,TRUE);
		if ((ptr = getnextline ()) == NULLCP)
			break;
		if (*ptr == ')') {
			if (needsoc && octype == NULLAttrT)
				octype = str2AttrT("objectClass");
			if (needsoc && as_find_type(as, octype) == NULLATTR) {
				parse_error("Required inherited attribute objectClass missing",NULLCP);
				as_free(as);
				return(NULLCP);
			}
			*asptr = as;
			return (++ptr);	

		}
	}

	parse_error ("Inherit: EOF unexpected (2)",NULLCP);
	return (NULLCP);
	
}

static InheritAttr str2inherit (str)
char * str;
{
InheritAttr	result;
char 		*ptr;
char		needsoc = FALSE;

	result = (InheritAttr) smalloc (sizeof *result);
	result->i_oid = NULLOID;	
	result->i_always = NULLATTR;
	result->i_default = NULLATTR;

	if ( (ptr=index (str,'$')) != NULLCP) {
		*ptr-- = 0;
		if (isspace (*ptr)) 
			*ptr = 0;
		ptr++;
		ptr = SkipSpace (++ptr);

		if ((result->i_oid = name2oid (str)) == NULLOID)
			needsoc = TRUE;
	} else
		ptr = str;

	if (lexnequ (ptr,"ALWAYS",strlen("ALWAYS")) == 0) {
		ptr = SkipSpace (ptr+strlen("ALWAYS"));
		if (*ptr++ != '(')
			parse_error ("Inherit: open bracket missing",NULLCP);
		ptr = SkipSpace (ptr);
		if (*ptr != 0) 
			parse_error ("Inherit: extra data '%s'",ptr);
		ptr = getInheritAttrs(&result->i_always, FALSE);
	}

	ptr = SkipSpace (ptr);
	if (lexnequ (ptr,"DEFAULT",strlen("DEFAULT")) == 0) {
		ptr = SkipSpace (ptr+strlen("DEFAULT"));
		if (*ptr++ != '(')
			parse_error ("Inherit: open bracket missing",NULLCP);
		ptr = SkipSpace (ptr);
		if (*ptr != 0) 
			parse_error ("Inherit: extra data '%s'",ptr);
		ptr = getInheritAttrs(&result->i_default, needsoc);
	}

	if ((result->i_always == NULLATTR) 
		&& (result->i_default == NULLATTR)) {
		parse_error ("Inherited Attribute syntax incorrect",NULLCP);
		return NULLINHERIT;
	}

	ptr = SkipSpace (ptr);
	if (*ptr != 0) 
		parse_error ("Inherit: extra data '%s'",ptr);
	
	return result;
}

static PE inherit_enc (m)
InheritAttr m;
{
PE ret_pe;

        (void) encode_Quipu_InheritedAttribute (&ret_pe,0,0,NULLCP,m);

	return (ret_pe);
}

static InheritAttr inherit_dec (pe)
PE pe;
{
InheritAttr m;

	if (decode_Quipu_InheritedAttribute (pe,1,NULLIP,NULLVP,&m) == NOTOK) 
		return (NULLINHERIT);
	return (m);
}

inherit_syntax ()
{
extern short inherit_sntx;

	inherit_sntx = add_attribute_syntax ("InheritedAttribute",
		(IFP) inherit_enc,	(IFP) inherit_dec,
		(IFP) str2inherit,	inherit_print,
		(IFP) inherit_cpy,	inherit_cmp,
		inherit_free,		NULLCP,
		NULLIFP,		TRUE);
}
