/* telex.c - Telex attribute */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/telex.c,v 7.1 91/02/22 09:20:30 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/telex.c,v 7.1 91/02/22 09:20:30 mrose Interim $
 *
 *
 * $Log:	telex.c,v $
 * Revision 7.1  91/02/22  09:20:30  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:44:37  mrose
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
	telex ::= <printablestring> '$' <printablestring> '$' <printablestring>
	
	REPRESENTING
		number $ country $ answerback
*/

/* LINTLIBRARY */

#include "quipu/util.h"
#include "quipu/entry.h"
#include "quipu/syntaxes.h"

static telex_free (ptr)
struct telex * ptr;
{
	free (ptr->telexnumber);
	free (ptr->countrycode);
	free (ptr->answerback);

	free ((char *) ptr);
}


static struct telex * telex_cpy (a)
struct telex * a;
{
struct telex * result;

	result = (struct telex *) smalloc (sizeof (struct telex));
	result->telexnumber = strdup (a->telexnumber);
	result->countrycode = strdup (a->countrycode);
	result->answerback  = strdup (a->answerback);
	return (result);
}

static telex_cmp (a,b)
struct telex * a;
struct telex * b;
{
int res;

	if (a == (struct telex *) NULL)
		if (b == (struct telex *) NULL)
			return (0);
		else 
			return (-1);

	if ( (res = lexequ(a->telexnumber,b->telexnumber)) != 0) 
		return (res);
	if ( (res = lexequ(a->countrycode,b->countrycode)) != 0)
		return (res);
	if ( (res = lexequ(a->answerback,b->answerback)) != 0)
		return (res);
	return (0);
}


static telex_print (ps,telex,format)
register PS ps;
struct   telex* telex;
int format;
{
	if (format == READOUT)
		ps_printf (ps,"number: %s, country: %s, answerback: %s",telex->telexnumber, telex->countrycode, telex->answerback);
	else
		ps_printf (ps,"%s $ %s $ %s",telex->telexnumber, telex->countrycode, telex->answerback);
}


static struct telex* str2telex (str)
char * str;
{
struct telex * result;
char * ptr;
char * mark = NULLCP;
char * prtparse ();

	if ( (ptr=index (str,'$')) == NULLCP) {
		parse_error ("seperator missing in telex '%s'",str);
		return ((struct telex *) NULL);
	}

	result = (struct telex *) smalloc (sizeof (struct telex));
	*ptr--= 0;
	if (isspace (*ptr)) {
		*ptr = 0;
		mark = ptr;
	}
	ptr++;
	if ((result->telexnumber = prtparse(str)) == NULLCP)
                return ((struct telex *) NULL);
	if (strlen (result->telexnumber) > UB_TELEX_NUMBER) {
		parse_error ("telexnumber too big",NULLCP);
		return ((struct telex *) NULL);
	}
		
	*ptr++ = '$';

	if (mark != NULLCP)
		*mark = ' ';

	str = SkipSpace(ptr);	

	if ( (ptr=index (str,'$')) == NULLCP) {
		parse_error ("2nd seperator missing in telex '%s'",str);
		return ((struct telex *) NULL);
	}

	*ptr--= 0;
	if (isspace (*ptr)) {
		*ptr = 0;
		mark = ptr;
	} else
		mark = NULLCP;

	ptr++;

	if ((result->countrycode = prtparse(str)) == NULLCP)
                return ((struct telex *) NULL);
	if (strlen (result->countrycode) > UB_COUNTRY_CODE) {
		parse_error ("countrycode too big",NULLCP);
		return ((struct telex *) NULL);
	}

	*ptr++ = '$';

	if (mark != NULLCP)
		*mark = ' ';

	if ((result->answerback = prtparse(SkipSpace(ptr))) == NULLCP)
                return ((struct telex *) NULL);
	if (strlen (result->answerback) > UB_ANSWERBACK) {
		parse_error ("answerback too big",NULLCP);
		return ((struct telex *) NULL);
	}

	return (result);
}

static PE telex_enc (m)
struct telex * m;
{
PE ret_pe;

        (void) encode_SA_TelexNumber (&ret_pe,0,0,NULLCP,m);

	return (ret_pe);
}

static struct telex * telex_dec (pe)
PE pe;
{
struct telex * m;

	if (decode_SA_TelexNumber (pe,1,NULLIP,NULLVP,&m) == NOTOK) {
		return ((struct telex *) NULL);
	}
	return (m);
}

telex_syntax ()
{
	(void) add_attribute_syntax ("TelexNumber",
		(IFP) telex_enc,	(IFP) telex_dec,
		(IFP) str2telex,	telex_print,
		(IFP) telex_cpy,	telex_cmp,
		telex_free,		NULLCP,
		NULLIFP,		TRUE);
}
