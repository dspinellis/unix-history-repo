/* filteritem.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/dish/RCS/filteritem.c,v 7.3 91/02/22 09:40:33 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/dish/RCS/filteritem.c,v 7.3 91/02/22 09:40:33 mrose Interim $
 *
 *
 * $Log:	filteritem.c,v $
 * Revision 7.3  91/02/22  09:40:33  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:55:20  mrose
 * sync
 * 
 * Revision 7.1  90/07/09  14:47:10  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:20:07  mrose
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


#include "quipu/util.h"
#include "quipu/ds_search.h"

#define	OPT	(!frompipe || rps -> ps_byteno == 0 ? opt : rps)
#define	RPS	(!frompipe || opt -> ps_byteno == 0 ? rps : opt)
extern	char	frompipe;
extern	PS	opt, rps;

/* Regular Expression parser written by P.Sharpe */
/* QUIPU specific tailoring by CJR */

#define debug(a,b)		/* remove debug statements */

Filter          get_filter ();
char           *TidyString ();

filteritem (str, fltr)
char           *str;
Filter          fltr;
{

	char           *ptr,
	               *index (), *rindex ();
	AttributeValue  av;
	AttributeType   at;

	fltr->flt_type = FILTER_ITEM;

	if ((ptr = index (str, '=')) == NULLCP) {
		/* set default (cn~=) */
		fltr->FUITEM.fi_type = FILTERITEM_APPROX;
		at = AttrT_new (CN_OID);
	} else {
		switch (*--ptr) {
		case '~':
			fltr->FUITEM.fi_type = FILTERITEM_APPROX;
			*ptr = 0;
			break;
		case '>':
			fltr->FUITEM.fi_type = FILTERITEM_GREATEROREQUAL;
			*ptr = 0;
			break;
		case '<':
			fltr->FUITEM.fi_type = FILTERITEM_LESSOREQUAL;
			*ptr = 0;
			break;
		default:
			fltr->FUITEM.fi_type = FILTERITEM_EQUALITY;
			break;
		}
		*++ptr = '\0';
		str = TidyString (str);
		at = AttrT_new (str);
		if (at == NULLAttrT) {
			ps_printf (OPT,"invalid attribute type (%s)\n", str);
			return (NOTOK);
		}
		str = ptr + 1;
	}

	if ((*str == '*') || (*str == 0)) {
		if (*++str == 0) {
			fltr->FUITEM.fi_type = FILTERITEM_PRESENT;
			fltr->FUITEM.UNTYPE = at;
			return (OK);
		} else
			str--;
	}

	/* test for whether there is only the simple 'equality' case */
	if ((ptr = index (str, '*')) == NULLCP) {
		debug (1, ("[EXACT(%s)]", str));

		fltr->FUITEM.UNAVA.ava_type = at;
		str = TidyString (str);
		if ((fltr->FUITEM.UNAVA.ava_value = str2AttrV (str, at->oa_syntax)) == NULLAttrV) 
			return (NOTOK);
		return (OK);
	}

	/*
	 * We have to parse the string for 'initial', 'final' and 'any'
	 * components 
	 */

	fltr->FUITEM.UNSUB.fi_sub_initial = NULLAV;
	fltr->FUITEM.UNSUB.fi_sub_any = NULLAV;
	fltr->FUITEM.UNSUB.fi_sub_final = NULLAV;
	fltr->FUITEM.UNSUB.fi_sub_type = at;
	fltr->FUITEM.fi_type = FILTERITEM_SUBSTRINGS;
	if ( ! sub_string (at->oa_syntax)) {
		ps_print (OPT,"Can only substring search on strings\n");
		return (NOTOK);
	}

	debug (1, ("[ "));
	/* This is the 'initial' section of the string - maybe NULL */
	*ptr = '\0';
	str = TidyString (str);
	if (*str != 0) {
		debug (1, ("INITIAL(%s) ", str));
		if ((av = str2AttrV (str, at->oa_syntax)) == NULLAttrV)
			return NOTOK;
		fltr->FUITEM.UNSUB.fi_sub_initial = avs_comp_new (av);
	}
	str = ptr + 1;

	/* Test for whether there are going to be any 'any' bits */
	if ((ptr = rindex (str, '*')) == NULLCP) {
		ptr = TidyString (str);
		if (*str != 0) {
			debug (1, ("FINAL(%s) ", str));
			if ((av = str2AttrV (str, at->oa_syntax)) == NULLAttrV)
				return NOTOK;
			fltr->FUITEM.UNSUB.fi_sub_final = avs_comp_new (av);
		}
		debug (1, ("]"));
		return (OK);
	}
	*ptr = '\0';
	ptr = TidyString (ptr + 1);
	if (*ptr != 0) {
		debug (1, ("FINAL(%s) ", ptr));
		if ((av = str2AttrV (ptr, at->oa_syntax)) == NULLAttrV)
			return NOTOK;

		fltr->FUITEM.UNSUB.fi_sub_final = avs_comp_new (av);
	}
	/* There are some internal 'any's to be found */
	do {
		AV_Sequence any_end;

		if ((ptr = index (str, '*')) != NULLCP)
			*ptr = '\0';

		if (*str != 0) {
			str = TidyString (str);
			debug (1, ("ANY(%s) ", str));
			if ((av = str2AttrV (str, at->oa_syntax)) == NULLAttrV)
				return NOTOK;
			if (fltr->FUITEM.UNSUB.fi_sub_any == NULLAV) {
				fltr->FUITEM.UNSUB.fi_sub_any = avs_comp_new (av);
				any_end = fltr->FUITEM.UNSUB.fi_sub_any;
			} else {
				any_end->avseq_next = avs_comp_new (av);
				any_end = any_end->avseq_next;
			}
		}
		if (ptr != NULLCP)
			str = ptr + 1;
	}

	while (ptr != NULLCP);
	debug (1, ("]"));
	return (OK);
}
