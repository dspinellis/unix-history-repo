/* post.c - postal address handling */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/post.c,v 7.1 91/02/22 09:19:59 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/post.c,v 7.1 91/02/22 09:19:59 mrose Interim $
 *
 *
 * $Log:	post.c,v $
 * Revision 7.1  91/02/22  09:19:59  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:44:22  mrose
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

/*
	SYNTAX:
	address = <address_component> | <address_component> '$' <address>
	address_component = [ '{T61}' ] <string>

	EXAMPLE:
		UCL $ Gower Street $ London

	Maximum of 6 (UB_POSTAL_LINE) <address_component>'s

*/

/* LINTLIBRARY */

#include "quipu/util.h"
#include "quipu/attrvalue.h"
#include "quipu/syntaxes.h"

static addrfree (addr)
struct postaddr * addr;
{
	struct postaddr * next;
	for (; addr != (struct postaddr *) NULL; addr = next) {
	        next = addr->pa_next;
		free (addr->addrcomp);
		free ( (char *)addr);
	}
}

static addrcmp (a,b)
struct postaddr * a, *b;
{
int res;
        for (; (a != (struct postaddr *) NULL) && (b != (struct postaddr *) NULL) ;
			a = a->pa_next, b=b->pa_next) 
		if ((res = lexequ (a->addrcomp, b->addrcomp)) != 0)
			return (res);

	if ( a != b)
		return ( a > b ? 1 : -1 );
	else
		return (0);
	
}

static struct postaddr * addrcpy (a)
struct postaddr * a;
{
struct postaddr * b, *c, *result = (struct postaddr *) NULL;

	c = result; /* to keep lint quiet ! */

        for (; a != (struct postaddr *) NULL; a = a->pa_next) {
	        b = (struct postaddr *) smalloc (sizeof (struct postaddr));
		b -> addrtype = a->addrtype;
		b -> addrcomp = strdup (a->addrcomp);
		
		if (result == (struct postaddr *) NULL) 
			result = b;
		else 
			c->pa_next = b;
		c = b;
	}

	b->pa_next = (struct postaddr *) NULL;
	return (result);
}

static struct postaddr* addrparse (str)
char * str;
{
struct postaddr * result = (struct postaddr *) NULL;
struct postaddr * a, *b;
char * ptr;
char * mark = NULLCP;
char t61_str = FALSE;
extern char t61_flag;
int i;
char * octparse ();
char * prtparse ();

   b = result; /* to keep lint quiet */

   if (t61_flag) {
	t61_str = TRUE;
	t61_flag = FALSE;  /* indicate recognition */
   }

   str = SkipSpace(str);

   for (i=0; i < UB_POSTAL_LINE; i++) {
	mark = NULLCP;
	a = (struct postaddr *) smalloc (sizeof (struct postaddr));

	if ( (ptr=index (str,'$')) != NULLCP) {
		*ptr-- = 0;
		if (isspace (*ptr)) {
			*ptr = 0;
			mark = ptr;
		}
		ptr++;
	}

	if (t61_str) {
		a -> addrtype = 1;
		if ((a -> addrcomp = octparse (str)) == NULLCP)
			return ((struct postaddr *)NULL);
		if (strlen (a->addrcomp) > UB_POSTAL_STRING) {
			parse_error ("address string too long",NULLCP);
			return ((struct postaddr *)NULL);
		}
	} else {
		a -> addrtype = 2;
		if ((a -> addrcomp = prtparse (str)) == NULLCP)
			return ((struct postaddr *)NULL);
		if (strlen (a->addrcomp) > UB_POSTAL_STRING) {
			parse_error ("address string too long",NULLCP);
			return ((struct postaddr *)NULL);
		}
	}


	if (result == (struct postaddr *) NULL) 
		result = a;
	else 
		b->pa_next = a;
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
        	                return ((struct postaddr *) NULL);
	                }
			*str = 0;
			if (lexequ ("T.61",ptr) != 0) {
				*str = '}';
                                parse_error ("{T.61} expected '%s'",--ptr);
                                return ((struct postaddr *) NULL);
                        }
			*str++ = '}';
			str = (SkipSpace(str));
			t61_str = TRUE;
		}
	} else
		break;
   }

   if (ptr != NULLCP) {
	parse_error ("Too many address components",NULLCP);
	return ((struct postaddr *) NULL);
   }
   a -> pa_next = (struct postaddr *) NULL ;

   return (result);
}


int	postal_indent = -1;

static addrprint (ps,addr,format)
PS ps;
struct postaddr * addr;
int format;
{
char * prefix = NULLCP;

	for (; addr != (struct postaddr *) NULL; addr = addr->pa_next) {
		if (prefix != NULLCP)
		        if (format != READOUT || postal_indent < 0)
			    ps_print (ps,prefix);
			else {
			    ps_print (ps, "\n");
			    if (postal_indent > 0)
				ps_printf (ps, "%*s", postal_indent, "");
			}

		if (addr->addrtype == 1) {
			if (format != READOUT)
				ps_print (ps,"{T.61}");
			octprint (ps,addr->addrcomp,format);
		} else
			ps_print (ps,addr->addrcomp);
		if (format == READOUT) {
			prefix = "\n\t\t\t";
			if (postal_indent == 0)
			    postal_indent = 2;
		}
		else
			prefix = " $ ";
	}
}


static PE addrenc (m)
struct postaddr * m;
{
PE ret_pe;

        (void) encode_SA_PostalAddress (&ret_pe,0,0,NULLCP,m);

	return (ret_pe);
}

static struct postaddr * addrdec (pe)
PE pe;
{
struct postaddr * m;

	if (decode_SA_PostalAddress (pe,1,NULLIP,NULLVP,&m) == NOTOK)
		return ((struct postaddr *) NULL);
	return (m);
}

post_syntax ()
{
	(void) add_attribute_syntax ("PostalAddress",
		(IFP) addrenc,	(IFP) addrdec,
		(IFP) addrparse,addrprint,
		(IFP) addrcpy,	addrcmp,
		addrfree,	NULLCP,
		NULLIFP,	TRUE);

}

