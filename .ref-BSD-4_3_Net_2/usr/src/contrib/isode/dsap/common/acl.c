/* acl.c - General Access Control routines */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/acl.c,v 7.4 91/02/22 09:18:05 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/acl.c,v 7.4 91/02/22 09:18:05 mrose Interim $
 *
 *
 * $Log:	acl.c,v $
 * Revision 7.4  91/02/22  09:18:05  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/11/20  15:29:09  mrose
 * cjr
 * 
 * Revision 7.2  90/10/17  11:40:47  mrose
 * sync
 * 
 * Revision 7.1  89/12/19  16:19:09  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:41:28  mrose
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
#include "cmd_srch.h"
#include "quipu/syntaxes.h"

extern char dsa_mode;

static struct acl_info * defaultacl = (struct acl_info *) NULL;

static acl_free (aclptr)
register struct acl * aclptr;
{
	acl_info_free (aclptr->ac_child);
	acl_info_free (aclptr->ac_entry);
	acl_info_free (aclptr->ac_default);
	acl_attr_free (aclptr);
	free ((char *) aclptr);
}

static acl_attr_free (aclptr)
register struct acl * aclptr;
{
register struct acl_attr * ptr;
register struct acl_attr * next;

	for (ptr=aclptr->ac_attributes ; ptr!=NULLACL_ATTR; ptr=next ) {
		next = ptr->aa_next;
		oid_seq_free (ptr->aa_types);
		if (ptr->aa_acl != aclptr->ac_default)
			acl_info_free (ptr->aa_acl);
		free ((char *) ptr);
	}
}

static acl_info_free (aclptr)
register struct acl_info * aclptr;
{
register struct acl_info * ptr;
register struct acl_info * next;

	if (test_acl_default(aclptr) == OK)
		return;

	for (ptr=aclptr ; ptr!=NULLACL_INFO; ptr=next ) {
		next = ptr->acl_next;
		dn_seq_free (ptr->acl_name);
		free ((char *) ptr);
	}

}

int	  acl_cmp (acl1, acl2)
struct acl	* acl1;
struct acl	* acl2;
{
    int	  i;

    if((acl1 == NULLACL) && (acl2 == NULLACL))
	return(0);

    if(acl1 == NULLACL)
	return(-1);

    if(acl2 == NULLACL)
	return(1);

    if((i = acl_info_cmp(acl1->ac_child, acl2->ac_child)) != 0)
	return(i);

    if((i = acl_info_cmp(acl1->ac_entry, acl2->ac_entry)) != 0)
	return(i);

    if((i = acl_info_cmp(acl1->ac_default, acl2->ac_default)) != 0)
	return(i);

    if((i = acl_attr_cmp(acl1->ac_attributes, acl2->ac_attributes)) != 0)
	return(i);

    return(0);
}

static int	  acl_attr_cmp (acl_attr1, acl_attr2)
struct acl_attr	* acl_attr1;
struct acl_attr	* acl_attr2;
{
    struct acl_attr	* aa1;
    struct acl_attr	* aa2;

    if((acl_attr1 == NULLACL_ATTR) && (acl_attr2 == NULLACL_ATTR))
	return(0);

    if(acl_attr1 == NULLACL_ATTR)
	return(-1);

    if(acl_attr2 == NULLACL_ATTR)
	return(1);

    for(aa1=acl_attr1; aa1 != NULLACL_ATTR; aa1=aa1->aa_next)
    {
	for(aa2=acl_attr2; aa2 != NULLACL_ATTR; aa2=aa2->aa_next)
	{
	    if(acl_attr_comp_cmp(aa1, aa2) == 0)
		break;
	}
	if(aa2 == NULLACL_ATTR)
	    return(1);
    }

    for(aa2=acl_attr2; aa2 != NULLACL_ATTR; aa2=aa2->aa_next)
    {
	for(aa1=acl_attr1; aa1 != NULLACL_ATTR; aa1=aa1->aa_next)
	{
	    if(acl_attr_comp_cmp(aa1, aa2) == 0)
		break;
	}
	if(aa1 == NULLACL_ATTR)
	    return(-1);
    }

    return(0);

}

static int	  acl_attr_comp_cmp (acl_attr1, acl_attr2)
struct acl_attr	* acl_attr1;
struct acl_attr	* acl_attr2;
{
    int	  i;

    if((acl_attr1 == NULLACL_ATTR) && (acl_attr2 == NULLACL_ATTR))
	return(0);

    if(acl_attr1 == NULLACL_ATTR)
	return(-1);

    if(acl_attr2 == NULLACL_ATTR)
	return(1);

    if((i = oid_seq_cmp(acl_attr1->aa_types, acl_attr2->aa_types)) != 0)
	return(i);

    if((i = acl_info_cmp(acl_attr1->aa_acl, acl_attr2->aa_acl)) != 0)
	return(i);

    return(0);
}

static int	  acl_info_cmp (acl_info1, acl_info2)
struct acl_info	* acl_info1;
struct acl_info	* acl_info2;
{
    struct acl_info	* ai1;
    struct acl_info	* ai2;

    if((acl_info1 == NULLACL_INFO) && (acl_info2 == NULLACL_INFO))
	return(0);

    if(acl_info1 == NULLACL_INFO) 
        if (test_acl_default(acl_info2) == OK)
		return(0);
	else
		return(-1);

    if(acl_info2 == NULLACL_INFO)
        if (test_acl_default(acl_info1) == OK)
		return(0);
	else
		return(1);

    for(ai1=acl_info1; ai1 != NULLACL_INFO; ai1=ai1->acl_next)
    {
	for(ai2=acl_info2; ai2 != NULLACL_INFO; ai2=ai2->acl_next)
	{
	    if(acl_info_comp_cmp(ai1, ai2) == 0)
		break;
	}
	if(ai2 == NULLACL_INFO)
	    return(1);
    }

    for(ai2=acl_info2; ai2 != NULLACL_INFO; ai2=ai2->acl_next)
    {
	for(ai1=acl_info1; ai1 != NULLACL_INFO; ai1=ai1->acl_next)
	{
	    if(acl_info_comp_cmp(ai2, ai1) == 0)
		break;
	}
	if(ai1 == NULLACL_INFO)
	    return(-1);
    }

    return(0);
}

static int	  acl_info_comp_cmp (acl_info1, acl_info2)
struct acl_info	* acl_info1;
struct acl_info	* acl_info2;
{
    int	  i;

    if((acl_info1 == NULLACL_INFO) && (acl_info2 == NULLACL_INFO))
	return(0);

    if(acl_info1 == NULLACL_INFO)
	return(-1);

    if(acl_info2 == NULLACL_INFO)
	return(1);

    if(acl_info1->acl_categories > acl_info2->acl_categories)
	return(1);

    if(acl_info2->acl_categories > acl_info1->acl_categories)
	return(-1);

    if(acl_info1->acl_selector_type > acl_info2->acl_selector_type)
	return(1);

    if(acl_info2->acl_selector_type > acl_info1->acl_selector_type)
	return(-1);

    if((i = dn_seq_cmp(acl_info1->acl_name, acl_info2->acl_name)) != 0)
	return(i);

    return(0);
}


struct acl_info * acl_info_new (x,y,z)
register int x,y;
struct dn_seq * z;
{
register struct acl_info * ptr;

	ptr = acl_info_alloc ();
	acl_info_fill (ptr,x,y,z);
	ptr->acl_next = NULLACL_INFO;
	return (ptr);
}

static struct acl * acl_cpy (aclptr)
register struct acl * aclptr;
{
register struct acl * ptr;

	ptr = (struct acl *) smalloc (sizeof (struct acl));
	ptr->ac_child = acl_info_cpy (aclptr->ac_child);
	ptr->ac_entry = acl_info_cpy (aclptr->ac_entry);
	ptr->ac_default = acl_info_cpy (aclptr->ac_default);
	ptr->ac_attributes = acl_attr_cpy (aclptr->ac_attributes,ptr->ac_default);
	return (ptr);

}

static struct acl * acl_decode (pe)
PE pe;
{
struct acl * aclptr;

	if (decode_Quipu_ACLSyntax(pe,1,NULLIP,NULLVP,&aclptr) == NOTOK) {
		return (struct acl *) NULL;
	}
	return (aclptr);

}

static struct acl_attr * acl_attr_cpy (aclptr,dflt)
struct acl_attr * aclptr;
struct acl_info * dflt;
{
register struct acl_attr * ptr;
register struct acl_attr * ptr2;
register struct acl_attr * result = NULLACL_ATTR;

	for (ptr=aclptr ; ptr!=NULLACL_ATTR; ptr=ptr->aa_next ) {
		ptr2 = acl_attr_alloc ();
		ptr2->aa_next = result;
		result = ptr2;
		ptr2->aa_types = oid_seq_cpy (ptr->aa_types);
		if (ptr->aa_acl != dflt)
			ptr2->aa_acl = acl_info_cpy (ptr->aa_acl);
		else
			ptr2->aa_acl = dflt;
	}
	return (result);
}


static struct acl_info * acl_info_cpy (aclptr)
struct acl_info * aclptr;
{
register struct acl_info * ptr;
register struct acl_info * ptr2;
register struct acl_info * result = NULLACL_INFO;

	if (test_acl_default(aclptr) == OK) {
		return (defaultacl);
	}

	for (ptr=aclptr ; ptr!=NULLACL_INFO; ptr=ptr->acl_next ) {
		ptr2 = acl_info_alloc();
		ptr2 -> acl_next = result;
		result = ptr2;
		result->acl_categories = ptr->acl_categories;
		result->acl_selector_type = ptr->acl_selector_type;
		result->acl_name = dn_seq_cpy (ptr->acl_name);
	}
	return (result);
}

struct acl_info * acl_default ()
{
	return (defaultacl);
}

get_default_acl ()
{
	defaultacl = acl_info_alloc ();
	set_default_acl(defaultacl);
}

set_default_acl (ai_ptr)
struct acl_info * ai_ptr;

{
	/* default -   others # read & self # write */

	ai_ptr ->acl_categories = ACL_READ;
	ai_ptr ->acl_selector_type = ACL_OTHER;
	ai_ptr ->acl_name = NULLDNSEQ;
	ai_ptr ->acl_next = acl_info_alloc();
	ai_ptr ->acl_next->acl_categories = ACL_WRITE;
	ai_ptr ->acl_next->acl_selector_type = ACL_ENTRY;
	ai_ptr ->acl_next->acl_next = NULLACL_INFO;
	ai_ptr ->acl_next->acl_name = NULLDNSEQ;

}

test_acl_default (a)
struct acl_info * a;
{
	if ((a == NULLACL_INFO) || (a == defaultacl))
		return (OK);

	if (a ->acl_categories != ACL_READ) {
	
		if (a ->acl_categories != ACL_WRITE)
			return (NOTOK);
		if (a ->acl_selector_type != ACL_ENTRY)
			return (NOTOK);
		if (a ->acl_next == NULLACL_INFO)
			return (NOTOK);
		if (a ->acl_next->acl_categories != ACL_READ)
			return (NOTOK);
		if (a ->acl_next->acl_selector_type != ACL_OTHER)
			return (NOTOK);
		if (a ->acl_next->acl_next != NULLACL_INFO)
			return (NOTOK);
		return (OK);
		
	} if (a ->acl_selector_type != ACL_OTHER)
		return (NOTOK);
	if (a ->acl_next == NULLACL_INFO)
		return (NOTOK);
	if (a ->acl_next->acl_categories != ACL_WRITE)
		return (NOTOK);
	if (a ->acl_next->acl_selector_type != ACL_ENTRY)
		return (NOTOK);
	if (a ->acl_next->acl_next != NULLACL_INFO)
		return (NOTOK);

	return (OK);
}

static struct acl_attr * acl_attr_merge (a,b)
struct acl_attr *a;
struct acl_attr *b;
{
struct acl_attr *c;

	if (b == NULLACL_ATTR)
		return (a);

	for (c=a ; c!= NULLACL_ATTR; c=c->aa_next) {
		if (oid_seq_cmp (c->aa_types,b->aa_types) == 0) {
			b->aa_acl->acl_next = c->aa_acl;
			c->aa_acl = b->aa_acl;
			return (a);
		}
	}

	b->aa_next = a;
	return (b);
			
	
	
}

static acl_merge (a,str)
AV_Sequence a;
char * str;
{
struct acl * aclptr, aclstr;
struct acl * newacl, *str2acl_aux();

	bzero ((char*)&aclstr,sizeof(struct acl));

	if ((newacl = str2acl_aux(str,&aclstr)) == NULLACL)
		return;

	aclptr = (struct acl *) a->avseq_av.av_struct;

	if (newacl->ac_child != NULLACL_INFO) {
		newacl->ac_child->acl_next = aclptr->ac_child;
		aclptr->ac_child = newacl->ac_child;
	}
	if (newacl->ac_entry != NULLACL_INFO) {
		newacl->ac_entry->acl_next = aclptr->ac_entry;
		aclptr->ac_entry = newacl->ac_entry;
	}
	if (newacl->ac_default != NULLACL_INFO) {
		newacl->ac_default->acl_next = aclptr->ac_default;
		aclptr->ac_default = newacl->ac_default;
	}
	if (newacl->ac_attributes != NULLACL_ATTR) 
		aclptr->ac_attributes = acl_attr_merge (aclptr->ac_attributes,newacl->ac_attributes);
}


static char * acl_cat [] = {
	"none",
	"detect",
	"compare",
	"read",
	"add",
	"write"
	};
static char * acl_sel  []  = {
	"ACL SELECTOR INTERNAL ERROR",
	"self",
	"others",
	"prefix",
	"group"
	};

static acl_info_comp_print (ps,aclptr,format)
register PS ps;
register struct acl_info * aclptr;
register int format;
{
	if (format == READOUT) {
	   switch (aclptr->acl_selector_type) {
		case ACL_PREFIX:
		case ACL_GROUP:
			ps_printf (ps,"%s ( ",acl_sel[aclptr->acl_selector_type]);
			dn_seq_print  (ps,aclptr->acl_name,format);
			ps_printf (ps," ) can %s ",acl_cat[aclptr->acl_categories]);
			break;
		default:
			ps_printf (ps,"%s can %s ", acl_sel[aclptr->acl_selector_type], acl_cat[aclptr->acl_categories]);
	   }
	} else {
	   switch (aclptr->acl_selector_type) {
		case ACL_PREFIX:
		case ACL_GROUP:
			ps_printf (ps,"%s # ",acl_sel[aclptr->acl_selector_type]);
			dn_seq_print  (ps,aclptr->acl_name,format);
			ps_printf (ps," # %s ",acl_cat[aclptr->acl_categories]);
			break;
		default:
			ps_printf (ps,"%s # %s ", acl_sel[aclptr->acl_selector_type], acl_cat[aclptr->acl_categories]);
	   }
	}
}

static acl_info_print (ps,aclptr,format,acl_type,oidseq)
register PS ps;
struct   acl_info * aclptr;
register int format;
char * acl_type;
struct oid_seq *oidseq;
{
register struct acl_info * ptr;
char printed = FALSE;

	if (test_acl_default(aclptr) == OK)
		return;

	for (ptr=aclptr ; ptr!=NULLACL_INFO; ptr=ptr->acl_next ) {

		if (printed)
			if (format != READOUT)
				if (dsa_mode)
					ps_print (ps," &\\\n\t");
				else
					ps_print (ps,"\nacl= ");
			else
				ps_print (ps,"\n\t\t\t");
		else
			printed = TRUE;

		acl_info_comp_print (ps,ptr,format);

		if (format == READOUT) {
			if (oidseq != NULLOIDSEQ) {
				ps_printf (ps,"the %s: ",acl_type);
				oid_seq_print (ps,oidseq,format) ;
			} else
				ps_printf (ps,"the %s",acl_type);

		} else {
			ps_printf (ps,"# %s",acl_type);
			if (oidseq != NULLOIDSEQ) {
				ps_print (ps," # ");
				oid_seq_print (ps,oidseq,format) ;
			}
		}
	}
}

static acl_print (ps,aclptr,format)
register PS ps;
struct   acl * aclptr;
register int format;
{
char printed = FALSE;
register struct acl_attr * ptr;

	if (test_acl_default(aclptr->ac_child) != OK) {
		acl_info_print (ps,aclptr->ac_child,format, "child", NULLOIDSEQ);
		printed = TRUE;
	}

	if (test_acl_default(aclptr->ac_entry) != OK) {
		if (printed)
			if (format != READOUT)
				if (dsa_mode)
					ps_print (ps," &\\\n\t");
				else
					ps_print (ps,"\nacl= ");
			else
				ps_print (ps,"\n\t\t\t");
		else
			printed = TRUE;
		acl_info_print (ps,aclptr->ac_entry,format,"entry", NULLOIDSEQ);
	}

	if (test_acl_default(aclptr->ac_default) != OK) {
		if (printed)
			if (format != READOUT)
				if (dsa_mode)
					ps_print (ps," &\\\n\t");
				else
					ps_print (ps,"\nacl= ");
			else
				ps_print (ps,"\n\t\t\t");
		else {
			printed = TRUE;
		}
		acl_info_print (ps,aclptr->ac_default,format,"default", NULLOIDSEQ);
	}

	for (ptr=aclptr->ac_attributes ; ptr!=NULLACL_ATTR; ptr=ptr->aa_next ) {
		if (test_acl_default(ptr->aa_acl) == OK)
			continue;

		if (acl_info_cmp(ptr->aa_acl,aclptr->ac_default) == 0)
			continue;

		if (printed)
			if (format != READOUT)
				if (dsa_mode)
					ps_print (ps," &\\\n\t");
				else
					ps_print (ps,"\nacl= ");
			else
				ps_print (ps,"\n\t\t\t");
		else {
			printed = TRUE;
		}
		acl_info_print (ps,ptr->aa_acl,format, "attributes", ptr->aa_types);
	}

	if (! printed)
	     if (format == READOUT)
		ps_print (ps,"(default)");
}

static struct acl_info * str2acl_info (strptr)
char ** strptr;
{
char * ptr;
char * save,val;
int class,what;
struct dn_seq * dnseq = NULLDNSEQ;

static CMD_TABLE cmd_what [] = {
        "none",         ACL_NONE,
        "detect",       ACL_DETECT,
        "compare",      ACL_COMPARE,
        "read",         ACL_READ,
        "add",          ACL_ADD,
        "write",        ACL_WRITE,
        0,              -1
        } ;

static CMD_TABLE cmd_class [] = {
	"SELF",		ACL_ENTRY,
	"OTHERS",	ACL_OTHER,
	"GROUP",	ACL_GROUP,
	"PREFIX",	ACL_PREFIX,
        0,              -1,
        } ;

	if ((ptr = index (*strptr,'#')) == 0) {
		parse_error ("# missing in acl syntax '%s'",*strptr);
		return (NULLACL_INFO);
	}

	save = ptr++;
	if (**strptr == '#') {
		parse_error ("acl class missing before first '#' ",NULLCP);
		return (NULLACL_INFO);
	} else {
		if (! isspace (*--save))
			save++;
		val = *save;
		*save = 0;

		if (( class = cmd_srch (*strptr,cmd_class)) == -1) {
			parse_error ("unknown acl class '%s'",*strptr);
			*save = val;
			return (NULLACL_INFO);
		}
		*save = val;
	}

	*strptr = SkipSpace(ptr);
	if ((ptr = index (*strptr,'#')) == 0) {
		parse_error ("2nd # missing in acl syntax ",NULLCP);
		return (NULLACL_INFO);
	}

	if ( (class == ACL_GROUP) || (class == ACL_PREFIX) ) {  /* group or prefix */
		save = ptr++;
		if (**strptr == '#') {
			parse_error ("acl class missing before first '#' ",NULLCP);
			return (NULLACL_INFO);
		} else {
			if (! isspace (*--save))
				save++;
			val = *save;
			*save = 0;

			if ((dnseq = str2dnseq (*strptr)) == NULLDNSEQ)
				return (NULLACL_INFO);

			*save = val;
		}
		*strptr = SkipSpace(ptr);
		if ((ptr = index (*strptr,'#')) == 0) {
			parse_error ("3rd # missing in acl syntax ",NULLCP);
			return (NULLACL_INFO);
		}
	}

	save = ptr++;
	if (**strptr == '#') {
		parse_error ("acl level missing",NULLCP);
		return (NULLACL_INFO);
	} else {
		if (! isspace (*--save))
			save++;
		val = *save;
		*save = 0;

		if (( what = cmd_srch (*strptr,cmd_what)) == -1) {
			parse_error ("unknown level '%s'",*strptr);
			*save = val;
			return (NULLACL_INFO);
		}
		*save = val;
	}

	*strptr = SkipSpace(ptr);

	return (acl_info_new (what,class,dnseq));
}

static struct acl * str2acl_aux (str,the_acl)
char * str;
struct acl * the_acl;
{
struct acl_info * info;
char * save, *ptr, val = 0;
int oidlist;
struct oid_seq * str2oidseq();

static CMD_TABLE cmd_who [] = {
        "child",        0,
        "entry",        1,
        "default",      2,
        0,              -1,
        };

	if ((info = str2acl_info (&str)) == NULLACL_INFO)
		return ( (struct acl *) NULL );

	/* this has left us with "string [#oidlist] [#]" */

	if ((ptr = index (str,'#')) != 0) {
		save = ptr++;
		if (*ptr == 0) 
			oidlist = FALSE;
		else
			oidlist = TRUE;

		if (! isspace (*--save))
			save++;
		val = *save;
		*save = 0;

	} else
		oidlist = FALSE;

	if (oidlist) {
		struct acl_attr * at_acl;

		if (lexequ (str,"attributes") != 0) {
			parse_error ("\"attributes\" expected",NULLCP);
			if (val != 0)
				*save = val;
			return ( (struct acl *) NULL );
		}
		at_acl = acl_attr_alloc();
		at_acl->aa_next = NULLACL_ATTR;
		at_acl->aa_acl = info;

		if ((str = rindex(ptr,'#')) != NULLCP) {
			*str-- = 0;
			if (isspace (*str))
				*str = 0;
		}
		
		if ((at_acl->aa_types = str2oidseq (SkipSpace(ptr))) == NULLOIDSEQ) {
			if (val != 0)
				*save = val;
			return ( (struct acl *) NULL );
		}
		the_acl->ac_child = NULLACL_INFO;
		the_acl->ac_entry = NULLACL_INFO;
		the_acl->ac_default = NULLACL_INFO;
		the_acl->ac_attributes = at_acl;
		
	} else {
		int who;
		if ((who = cmd_srch (str,cmd_who)) == -1) {
			parse_error ("unknown acl type specifier '%s'",str);
			if (val != 0)
				*save = val;
			return ( (struct acl *) NULL );
		}
		the_acl->ac_child = NULLACL_INFO;
		the_acl->ac_entry = NULLACL_INFO;
		the_acl->ac_default = NULLACL_INFO;
		the_acl->ac_attributes = NULLACL_ATTR;
		switch (who) {
		case 0:
			the_acl->ac_child = info;
			break;
		case 1:
			the_acl->ac_entry = info;
			break;
		case 2:
			the_acl->ac_default = info;
			break;
		}
	}

	if (val != 0)
		*save = val;	

	return (the_acl);
}

static struct acl * str2acl (str)
char * str;
{
struct acl * the_acl;

	the_acl = acl_alloc ();
	if (str2acl_aux(str,the_acl) != NULLACL)
		return (the_acl);
	free ((char *)the_acl);
	return (NULLACL);
}


static PE acl_enc (acl)
struct acl * acl;
{
PE ret_pe;

	(void) encode_Quipu_ACLSyntax (&ret_pe,0,0,NULLCP,acl);
	return (ret_pe);
}


acl_syntax ()
{
extern short acl_sntx;
extern IFP merge_acl;
extern IFP acl_fn;

	acl_sntx = add_attribute_syntax ("acl",
		(IFP) acl_enc,	(IFP) acl_decode,
		(IFP) str2acl,	acl_print,
		(IFP) acl_cpy,	acl_cmp,
		acl_free,	NULLCP,
		NULLIFP,	TRUE);

	merge_acl = (IFP) acl_merge;
	acl_fn = (IFP) acl_default;
	get_default_acl();
}
