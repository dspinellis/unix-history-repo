/* pdm.c - preferred delivery method handling */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/pdm.c,v 7.1 91/02/22 09:19:54 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/pdm.c,v 7.1 91/02/22 09:19:54 mrose Interim $
 *
 *
 * $Log:	pdm.c,v $
 * Revision 7.1  91/02/22  09:19:54  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:42:37  mrose
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
	pdm = <pdm_component> | <pdm_component> '$' <pdm>
	pdm_component = 'any' | 'mhs' | 'physical' | 'telex' | 'teletex' |
			'g3fax' | 'g4fax' | 'ia5' | 'videotex' | 'telephone'

	EXAMPLE:
		mhs $ physical $ telex $ telephone


*/

/* LINTLIBRARY */

#include "quipu/util.h"
#include "quipu/attrvalue.h"
#include "cmd_srch.h"
#include "quipu/syntaxes.h"

static CMD_TABLE pdm_table [] = {
	"ANY",		0,
	"MHS",		1,
	"PHYSICAL",	2,
	"TELEX",	3,
	"TELETEX",	4,
	"G3FAX",	5,
	"G4FAX",	6,
	"IA5",		7,
	"VIDEOTEX",	8,
	"TELEPHONE",	9,
	"UNKNOWN",	-1,
	0,		-1
	};


static pdmfree (pdm)
struct pref_deliv * pdm;
{
        struct pref_deliv *next;
	
	for (; pdm != (struct pref_deliv *) NULL; pdm = next)  {
	        next = pdm -> pd_next;
		free ((char *) pdm);
	}
}

static pdmcmp (a,b)
struct pref_deliv * a, *b;
{
	/* matching here is a bit dubious !!! */

        for (; (a != (struct pref_deliv *) NULL) && (b != (struct pref_deliv *) NULL) ;
			a = a->pd_next, b=b->pd_next) 
		if ( a->deliv != b->deliv )
			return (a->deliv > b->deliv ? 1 : -1);

	if ( a != b)
		return ( a > b ? 1 : -1 );
	else
		return (0);
	
}

static struct pref_deliv * pdmcpy (a)
struct pref_deliv * a;
{
struct pref_deliv * b, *c, *result = (struct pref_deliv *) NULL;

	c = result; /* to keep lint happy */

        for (; a != (struct pref_deliv *) NULL; a = a->pd_next) {
	        b = (struct pref_deliv *) smalloc (sizeof (struct pref_deliv));
		b -> deliv = a -> deliv;
		
		if (result == (struct pref_deliv *) NULL) 
			result = b;
		else 
			c->pd_next = b;
		c = b;
	}

	b->pd_next = (struct pref_deliv *) NULL;
	return (result);
}

static struct pref_deliv* pdmparse (str)
char * str;
{
struct pref_deliv * result = (struct pref_deliv *) NULL;
struct pref_deliv * a, *b;
char * ptr;
char * mark = NULLCP;

   b = result; /* to keep lint happy */

   for (;;) {
	mark = NULLCP;
	a = (struct pref_deliv *) smalloc (sizeof (struct pref_deliv));

	if ( (ptr=index (str,'$')) != NULLCP) {
		*ptr-- = 0;
		if (isspace (*ptr)) {
			*ptr = 0;
			mark = ptr;
		}
		ptr++;
	}

	if ((a -> deliv = cmd_srch (str,pdm_table)) == -1) {
		parse_error ("Unknown method %s",str);
		return ((struct pref_deliv *) NULL);
	}

	if (result == (struct pref_deliv *) NULL) 
		result = a;
	else 
		b->pd_next = a;
	b = a;

	if (ptr != NULLCP) {
		*ptr++ = '$';
		if (mark != NULLCP)
			*mark = ' ';
		str = (SkipSpace(ptr));	
		ptr = str;
	} else
		break;
   }
   a -> pd_next = (struct pref_deliv *) NULL ;

   return (result);
}

static pdmprint (ps,pdm,format)
PS ps;
struct pref_deliv * pdm;
int format;
{
char * prefix = NULLCP;

	for (; pdm != (struct pref_deliv *) NULL; pdm = pdm->pd_next) {
		if (prefix != NULLCP)
			ps_print (ps,prefix);

			
		ps_print (ps,rcmd_srch (pdm->deliv,pdm_table));

		if (format == READOUT)
			prefix = " or ";
		else
			prefix = " $ ";
	}
}


static PE pdmenc (m)
struct pref_deliv * m;
{
PE ret_pe;

        (void) encode_SA_PreferredDeliveryMethod (&ret_pe,0,0,NULLCP,m);

	return (ret_pe);
}

static struct pref_deliv * pdmdec (pe)
PE pe;
{
struct pref_deliv * m;

	if (decode_SA_PreferredDeliveryMethod (pe,1,NULLIP,NULLVP,&m) == NOTOK)
		return ((struct pref_deliv *) NULL);
	return (m);
}

pref_deliv_syntax ()
{
	(void) add_attribute_syntax ("DeliveryMethod",
		(IFP) pdmenc,	(IFP) pdmdec,
		(IFP) pdmparse,pdmprint,
		(IFP) pdmcpy,	pdmcmp,
		pdmfree,	NULLCP,
		NULLIFP,	TRUE);

}

