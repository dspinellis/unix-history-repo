/* pe2pl.c - presentation element to presentation list */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/pe2pl.c,v 7.2 91/02/22 09:36:00 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/pe2pl.c,v 7.2 91/02/22 09:36:00 mrose Interim $
 *
 *
 * $Log:	pe2pl.c,v $
 * Revision 7.2  91/02/22  09:36:00  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/11/21  11:31:01  mrose
 * sun
 * 
 * Revision 7.0  89/11/23  22:12:53  mrose
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


/* Presentation lists are a human-readable, unambiguous way of describing
   a presentation element.

    SYNTAX:		list	::	"(" class code arguments ")"

			class	::	"UNIV" / "APPL" / "CONT" / "PRIV"

			code	::	name / number

			name	::	letter (letter / digit / dash)*

			number	::	"0x" [0-f] [0-f]* /
					"0" [0-7] [0-7]* /
					[1-9] [0-9]* /
					"\"" (IA5 subset)* "\""

			arguments::	primitive / constructor

			primitive::	number number*

			constructor::	list*

   NOTE WELL:	A single "number" must be representable in no more than
		(sizeof (int)) bytes.
 */


/* LINTLIBRARY */

#include <ctype.h>
#include <stdio.h>
#include "psap.h"


#define	bf_write()	\
    if (ps_write (ps, (PElementData) buffer, (PElementLen) strlen (buffer)) == NOTOK) \
	return NOTOK

/*  */

int	pe2pl (ps, pe)
register PS	ps;
register PE	pe;
{
    int     result;

    if ((result = pe2pl_aux (ps, pe, 0)) != NOTOK)
	result = ps_flush (ps);

    return result;
}

/*  */

static int  pe2pl_aux (ps, pe, level)
register PS	ps;
register PE	pe;
int	level;
{
    register int    i,
		    ia5,
		    ia5ok;
    register char  *bp;
    char    buffer[BUFSIZ];
    register PE	    p;
    register PElementID id;
    register PElementData dp,
			  ep,
			  fp,
			  gp;

    (void) sprintf (buffer, "%*s( %s ",
	    level * 4, "", pe_classlist[pe -> pe_class]);
    bf_write ();

    switch (pe -> pe_class) {
	case PE_CLASS_UNIV: 
	    if ((id = pe -> pe_id) < pe_maxuniv && (bp = pe_univlist[id])) {
		if (ps_write (ps, (PElementData) bp, (PElementLen) strlen (bp))
			== NOTOK)
		    return NOTOK;
	    }
	    else
		goto no_code;
	    break;

	case PE_CLASS_APPL: 
	    if ((id = pe -> pe_id) < pe_maxappl && (bp = pe_applist[id])) {
		if (ps_write (ps, (PElementData) bp, (PElementLen) strlen (bp))
			== NOTOK)
		    return NOTOK;
	    }
	    else
		goto no_code;

	case PE_CLASS_PRIV: 
	    if ((id = pe -> pe_id) < pe_maxpriv && (bp = pe_privlist[id])) {
		if (ps_write (ps, (PElementData) bp, (PElementLen) strlen (bp))
			== NOTOK)
		    return NOTOK;
	    }			/* else fall */

	case PE_CLASS_CONT: 
    no_code: ;
	    (void) sprintf (buffer, "0x%x", pe -> pe_id);
	    bf_write ();
	    break;
    }

    level++;
    switch (pe -> pe_form) {
	case PE_FORM_PRIM: 
	case PE_FORM_ICONS: 
	    (void) sprintf (buffer, " 0x%x%c",
			pe -> pe_len, pe -> pe_len ? '\n' : ' ');
	    bf_write ();

	    if (pe -> pe_len) {
		ia5ok = 0;
		if (pe -> pe_form == PE_FORM_PRIM
		        && pe -> pe_class == PE_CLASS_UNIV)
		    switch (pe -> pe_id) {
			case PE_PRIM_OCTS:
			case PE_DEFN_IA5S:
			case PE_DEFN_NUMS: 
			case PE_DEFN_PRTS: 
			case PE_DEFN_T61S:
			case PE_DEFN_VTXS:
			case PE_DEFN_VISS:
			case PE_DEFN_GENT:
			case PE_DEFN_UTCT:
			case PE_DEFN_GFXS:
			case PE_PRIM_ODE:
			case PE_DEFN_GENS:
			    ia5ok = 1;
			    break;

			default:
			    break;
		    }

		for (ep = (dp = pe -> pe_prim) + pe -> pe_len; dp < ep;) {
		    i = min (ep - dp, sizeof (int));
		    if (ia5 = ia5ok) {
			for (gp = (fp = dp) + i; fp < gp; fp++) {
			    switch (*fp) {
				case ' ':
				    continue;
				case '"':
				    break;
				default:
				    if (iscntrl ((u_char) *fp)
					    || isspace ((u_char) *fp)
					    || (*fp & 0x80))
					break;
				    continue;
			    }
			    ia5 = 0;
			    break;
			}
		    }
		    (void) sprintf (buffer, ia5 ? "%*s\"" : "%*s0x",
				level * 4, "");
		    bp = buffer + strlen (buffer);
		    while (i-- > 0) {
			(void) sprintf (bp, ia5 ? (i ? "%c" : "%c\"\n")
				    : (i ? "%02x" : "%02x\n"), *dp++);
			bp += strlen (bp);
		    }
		    bf_write ();
		}
	    }
 	    else
		level = 1;
	    break;

	case PE_FORM_CONS: 
	    if (p = pe -> pe_cons) {
		if (ps_write (ps, (PElementData) "\n", (PElementLen) 1)
		    == NOTOK)
		    return NOTOK;
		for (p = pe -> pe_cons; p; p = p -> pe_next)
		    if (pe2pl_aux (ps, p, level) == NOTOK)
			return NOTOK;
	    }
	    else {
		if (ps_write (ps, (PElementData) " ", (PElementLen) 1)
		    == NOTOK)
		    return NOTOK;
		level = 1;
	    }
	    break;
    }
    level--;

    (void) sprintf (buffer, "%*s)\n", level * 4, "");
    bf_write ();

    return OK;
}
