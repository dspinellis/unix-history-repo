/* get_filter.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/dish/RCS/get_filter.c,v 7.3 91/02/22 09:40:39 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/dish/RCS/get_filter.c,v 7.3 91/02/22 09:40:39 mrose Interim $
 *
 *
 * $Log:	get_filter.c,v $
 * Revision 7.3  91/02/22  09:40:39  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:55:26  mrose
 * sync
 * 
 * Revision 7.1  89/12/19  16:21:06  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:20:10  mrose
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

char           *TidyString ();

Filter          get_filter_aux (str)
char           *str;
{
	int             gotit,
	                bracketed;
	char            ch,
	                och = '\0';
	Filter          result,
	                next,
	                ptr = NULLFILTER;

	result = filter_alloc ();
	result->FUFILT = NULLFILTER;
	result->flt_next = NULLFILTER;

	str = TidyString (str);

	/* Got a multiple-component string for parsing */

	do {
		str = SkipSpace (str);
		bracketed = FALSE;
		if ((gotit = getop (str, &ch)) == -2)
			return (NULLFILTER);

		if (gotit < 0) {/* Match an open bracket. */
			if (*str == '(')
				if (str[strlen (str) - 1] == ')') {
					str[strlen (str) - 1] = '\0';
					++str;
					bracketed = TRUE;
				} else {
					ps_print (OPT, "Too many open brackets\n");
					return (NULLFILTER);
				}

			if (och == '\0') {
				if (bracketed == TRUE) {
					gotit = 0;	/* Stop 'while' loop
							 * falling */
					continue;	/* Parse the internals */
				} else
					break;	/* Single item only */
			} else
				ch = och;	/* Use last operation */
		}
		if (och == '\0')/* Remember last operation */
			och = ch;
		else if (och != ch) {
			ps_print (OPT, "Can't mix operations without using brackets!\n");
			return (NULLFILTER);
		}
		if (gotit >= 0)	/* If got an op, make it null */
			str[gotit] = '\0';
		/* Recurse on the 'first' string */
		if (gotit != 0)
		{
		if ((next = get_filter_aux (str)) == NULLFILTER)
			return (NULLFILTER);
		if (ptr == NULLFILTER)
			ptr = next;
		else
			filter_append (ptr, next);
		}
		str += gotit + 1;

		if (gotit >= 0) {	/* Match an and symbol */
			if (och == '&') {
				result->flt_type = FILTER_AND;
			} else {/* Match an or symbol */
				result->flt_type = FILTER_OR;
			}
			result->FUFILT = ptr;
		}
	}
	while (gotit >= 0);
	if (och == '\0') {
		if (*str == '!') {	/* Match a not symbol */
			result->flt_type = FILTER_NOT;
			if ((result->FUFILT = get_filter_aux (str + 1)) == NULLFILTER)
				return (NULLFILTER);
		} else if (*str != 0)
		    if (filteritem (str, result) == NOTOK)
			return (NULLFILTER);
	}
	return (result);
}


Filter          get_filter (str)
char           *str;
{
char * ptr;
Filter f;

	ptr = strdup (str);
	f = get_filter_aux (ptr);
	free (ptr);
	return (f);

}


getop (str, ch)
char           *str,
               *ch;
{
	int             i,
	                bracket = 0;

	for (i = 0; i < strlen (str); i++) {
		if (bracket == 0 && (str[i] == '&' || str[i] == '|')) {
			*ch = str[i];
			return (i);
		}
		if (str[i] == '(')
			++bracket;
		if (str[i] == ')')
			--bracket;
		if (bracket < 0) {
			ps_print (OPT, "Too many close brackets\n");
			return (-2);
		}
	}
	return (-1);
}

