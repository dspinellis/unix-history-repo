/* cilist.c - Case Ignore List */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/cilist.c,v 7.1 91/02/22 09:18:49 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/cilist.c,v 7.1 91/02/22 09:18:49 mrose Interim $
 *
 *
 * $Log:	cilist.c,v $
 * Revision 7.1  91/02/22  09:18:49  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/08/24  12:10:42  mrose
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
	SYNTAX:
	list = <list_component> | <list_component> '$' <list>
	list_component = [ '{T61}' ] <string>

	EXAMPLE:
		An example $ of a case ignore list $ syntax attribute
*/

/* LINTLIBRARY */

#include "quipu/util.h"
#include "quipu/attrvalue.h"
#include "quipu/syntaxes.h"

static cilistfree (cilist)
struct CIList * cilist;
{
	struct CIList * next;
	for (; cilist != NULLCILIST; cilist = next) {
	        next = cilist->l_next;
		free (cilist->l_str);
		free ( (char *)cilist);
	}
}

static cilistcmp (a,b)
struct CIList * a, *b;
{
int res;

        for (; (a != NULLCILIST) && (b != NULLCILIST) ;
			a = a->l_next, b=b->l_next) 
		if ((res = lexequ (a->l_str, b->l_str)) != 0)
			return (res);

	if ( a != b)
		return ( a > b ? 1 : -1 );
	else
		return (0);
	
}

static struct CIList * cilistcpy (a)
struct CIList * a;
{
struct CIList * b, *c, *result = NULLCILIST;

	c = result; /* to keep lint quiet ! */

        for (; a != NULLCILIST; a = a->l_next) {
	        b = (struct CIList *) smalloc (sizeof (struct CIList));
		b -> l_type = a->l_type;
		b -> l_str = strdup (a->l_str);
		
		if (result == NULLCILIST) 
			result = b;
		else 
			c->l_next = b;
		c = b;
	}

	b->l_next = NULLCILIST;
	return (result);
}

static struct CIList* cilistparse (str)
char * str;
{
struct CIList * result = NULLCILIST;
struct CIList * a, *b;
char * ptr;
char * mark = NULLCP;
char t61_str = FALSE;
extern char t61_flag;
char * octparse ();
char * prtparse ();

   b = result; /* to keep lint quiet */

   if (t61_flag) {
	t61_str = TRUE;
	t61_flag = FALSE;  /* indicate recognition */
   }

   ptr = str = SkipSpace(str);

   while (ptr) {
	mark = NULLCP;
	a = (struct CIList *) smalloc (sizeof (struct CIList));

	if ( (ptr=index (str,'$')) != NULLCP) {
		*ptr-- = 0;
		if (isspace (*ptr)) {
			*ptr = 0;
			mark = ptr;
		}
		ptr++;
	}

	if (*str == 0) {
		parse_error ("Null string not allowed",NULLCP);	
		return NULLCILIST;
	}

	if ((t61_str) || (! check_print_string(str))) {
		a -> l_type = 1;
		if ((a -> l_str = octparse (str)) == NULLCP)
			return (NULLCILIST);
	} else {
		a -> l_str = strdup(str);
		a -> l_type = 2;
	}

	if (result == NULLCILIST) 
		result = a;
	else 
		b->l_next = a;
	b = a;

	t61_str = FALSE;

	if (ptr != NULLCP) {
		*ptr++ = '$';
		if (mark != NULLCP)
			*mark = ' ';
		str = (SkipSpace(ptr));	
		ptr = str;

		if (*ptr++ == '{') {
			if (( str = index (ptr,'}')) == 0) {
	                        parse_error ("close bracket missing '%s'",--ptr);
        	                return (NULLCILIST);
	                }
			*str = 0;
			if (lexequ ("T.61",ptr) != 0) {
				*str = '}';
                                parse_error ("{T.61} expected '%s'",--ptr);
                                return (NULLCILIST);
                        }
			*str++ = '}';
			str = (SkipSpace(str));
			t61_str = TRUE;
		}
	}
   }

   a -> l_next = NULLCILIST ;

   return (result);
}

static cilistprint (ps,cilist,format)
PS ps;
struct CIList * cilist;
int format;
{
char * prefix = NULLCP;

	for (; cilist != NULLCILIST; cilist = cilist->l_next) {
		if (prefix != NULLCP)
			ps_print (ps,prefix);

		if (cilist->l_type == 1) {
			if (format != READOUT)
				ps_print (ps,"{T.61}");
			octprint (ps,cilist->l_str,format);
		} else
			ps_print (ps,cilist->l_str);

		if (format == READOUT)
			prefix = "\n\t\t\t";
		else 
			prefix = " $\\\n\t";
	}
}


static PE cilistenc (m)
struct CIList * m;
{
PE ret_pe;

        (void) encode_SA_CaseIgnoreList (&ret_pe,0,0,NULLCP,m);

	return (ret_pe);
}

static struct CIList * cilistdec (pe)
PE pe;
{
struct CIList * m;

	if (decode_SA_CaseIgnoreList (pe,1,NULLIP,NULLVP,&m) == NOTOK)
		return (NULLCILIST);
	return (m);
}

cilist_syntax ()
{
	(void) add_attribute_syntax ("CaseIgnoreList",
		(IFP) cilistenc,	(IFP) cilistdec,
		(IFP) cilistparse,	cilistprint,
		(IFP) cilistcpy,	cilistcmp,
		cilistfree,	NULLCP,
		NULLIFP,	TRUE);

}

