/* teletex.c - Teletex attribute */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/teletex.c,v 7.1 91/02/22 09:20:28 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/teletex.c,v 7.1 91/02/22 09:20:28 mrose Interim $
 *
 *
 * $Log:	teletex.c,v $
 * Revision 7.1  91/02/22  09:20:28  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:44:35  mrose
 * Release 6.0
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
	teletex ::= <printablestring> '$' <printablestring> '$' <printablestring>...
	
	REPRESENTING
	terminal $ graphic $ control $ page $ misc $ private
*/

/* LINTLIBRARY */

#include "quipu/util.h"
#include "quipu/entry.h"
#include "quipu/syntaxes.h"

#define nfree(x) if (x != NULLCP) free (x)

static teletex_free (ptr)
struct teletex * ptr;
{
	nfree (ptr->terminal);
	nfree (ptr->graphic);
	nfree (ptr->control);
	nfree (ptr->page);
	nfree (ptr->misc);
	nfree (ptr->t_private);

	free ((char *) ptr);
}

static char * xstrdup (a)
char * a;
{
	if (( a == NULLCP) || (*a == NULL))
		return (NULLCP);
	else
		return (strdup (a));
}

static struct teletex * teletex_cpy (a)
struct teletex * a;
{
struct teletex * result;

	result = (struct teletex *) smalloc (sizeof (struct teletex));
	result->terminal = strdup (a->terminal);
	result->graphic  = xstrdup (a->graphic);
	result->control  = xstrdup (a->control);
	result->page     = xstrdup (a->page);
	result->misc     = xstrdup (a->misc);
	result->t_private  = xstrdup (a->t_private);
	return (result);
}

static teletex_cmp (a,b)
struct teletex * a;
struct teletex * b;
{
int res;

	if (a == (struct teletex *) NULL)
		if (b == (struct teletex *) NULL)
			return (0);
		else 
			return (-1);

	if ( (res = lexequ(a->terminal,b->terminal)) != 0) 
		return (res);
	if ( (res = lexequ(a->graphic,b->graphic)) != 0) 
		return (res);
	if ( (res = lexequ(a->control,b->control)) != 0) 
		return (res);
	if ( (res = lexequ(a->page,b->page)) != 0) 
		return (res);
	if ( (res = lexequ(a->misc,b->misc)) != 0) 
		return (res);
	if ( (res = lexequ(a->t_private,b->t_private)) != 0) 
		return (res);
	return (0);
}


static teletex_print (ps,teletex,format)
register PS ps;
struct   teletex* teletex;
int format;
{
	if (format == READOUT)
		ps_print (ps,"terminal: ");
	ps_print (ps,teletex->terminal);

	if (format != READOUT)
		ps_print (ps," $ ");
	if (teletex->graphic != NULLCP) {
		if (format == READOUT)
			ps_print (ps,", graphic: ");
		ps_print (ps,teletex->graphic);
	} 

	if (format != READOUT)
		ps_print (ps," $ ");
	if (teletex->control != NULLCP) {
		if (format == READOUT)
			ps_print (ps,", control: ");
		ps_print (ps,teletex->control);
	} 

	if (format != READOUT)
		ps_print (ps," $ ");
	if (teletex->page != NULLCP) {
		if (format == READOUT)
			ps_print (ps,", page: ");
		ps_print (ps,teletex->page);
	} 

	if (format != READOUT)
		ps_print (ps," $ ");
	if (teletex->misc != NULLCP) {
		if (format == READOUT)
			ps_print (ps,", misc: ");
		ps_print (ps,teletex->misc);
	} 

	if (format != READOUT)
		ps_print (ps," $ ");
	if (teletex->t_private != NULLCP) {
		if (format == READOUT)
			ps_print (ps,", private: ");
		ps_print (ps,teletex->t_private);
	} 
		
}


static struct teletex* str2teletex (str)
char * str;
{
struct teletex * result;
char * ptr;
char * mark = NULLCP;
char * prtparse ();

	if ( (ptr=index (str,'$')) == NULLCP) {
		parse_error ("seperator missing in teletex '%s'",str);
		return ((struct teletex *) NULL);
	}

	result = (struct teletex *) smalloc (sizeof (struct teletex));
	*ptr--= 0;
	if (isspace (*ptr)) {
		*ptr = 0;
		mark = ptr;
	}
	ptr++;
	if ((result->terminal = prtparse(str)) == NULLCP)
                return ((struct teletex *) NULL);

        if (strlen (result->terminal) > UB_TELETEX_TERMINAL_ID) {
                parse_error ("teletex string too long",NULLCP);
                return ((struct teletex *) NULL);
        }

	*ptr++ = '$';

	if (mark != NULLCP)
		*mark = ' ';

	str = SkipSpace(ptr);	
	if ( (ptr=index (str,'$')) == NULLCP) {
		parse_error ("2nd seperator missing in teletex '%s'",str);
		return ((struct teletex *) NULL);
	}
	*ptr--= 0;
	if (isspace (*ptr)) {
		*ptr = 0;
		mark = ptr;
	} else
		mark = NULLCP;
	ptr++;
	result->graphic = xstrdup(str);
	*ptr++ = '$';
	if (mark != NULLCP)
		*mark = ' ';


	str = SkipSpace(ptr);	
	if ( (ptr=index (str,'$')) == NULLCP) {
		parse_error ("3rd seperator missing in teletex '%s'",str);
		return ((struct teletex *) NULL);
	}
	*ptr--= 0;
	if (isspace (*ptr)) {
		*ptr = 0;
		mark = ptr;
	} else
		mark = NULLCP;
	ptr++;
	result->control = xstrdup(str);
	*ptr++ = '$';
	if (mark != NULLCP)
		*mark = ' ';


	str = SkipSpace(ptr);	
	if ( (ptr=index (str,'$')) == NULLCP) {
		parse_error ("4th seperator missing in teletex '%s'",str);
		return ((struct teletex *) NULL);
	}
	*ptr--= 0;
	if (isspace (*ptr)) {
		*ptr = 0;
		mark = ptr;
	} else
		mark = NULLCP;
	ptr++;
	result->page = xstrdup(str);
	*ptr++ = '$';
	if (mark != NULLCP)
		*mark = ' ';


	str = SkipSpace(ptr);	
	if ( (ptr=index (str,'$')) == NULLCP) {
		parse_error ("5th seperator missing in teletex '%s'",str);
		return ((struct teletex *) NULL);
	}
	*ptr--= 0;
	if (isspace (*ptr)) {
		*ptr = 0;
		mark = ptr;
	} else
		mark = NULLCP;
	ptr++;
	result->misc = xstrdup(str);
	*ptr++ = '$';
	if (mark != NULLCP)
		*mark = ' ';

	result->t_private = xstrdup(SkipSpace(ptr));

	return (result);
}

static PE teletex_enc (m)
struct teletex * m;
{
PE ret_pe;

        (void) encode_SA_TeletexTerminalIdentifier (&ret_pe,0,0,NULLCP,m);

	return (ret_pe);
}

static struct teletex * teletex_dec (pe)
PE pe;
{
struct teletex * m;

	if (decode_SA_TeletexTerminalIdentifier (pe,1,NULLIP,NULLVP,&m) == NOTOK) {
		return ((struct teletex *) NULL);
	}

	return (m);
}

teletex_syntax ()
{
	(void) add_attribute_syntax ("TeletexTerminalIdentifier",
		(IFP) teletex_enc,	(IFP) teletex_dec,
		(IFP) str2teletex,	teletex_print,
		(IFP) teletex_cpy,	teletex_cmp,
		teletex_free,		NULLCP,
		NULLIFP,		TRUE);
}
