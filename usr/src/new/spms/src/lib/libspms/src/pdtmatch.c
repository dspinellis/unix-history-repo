/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pdtmatch() returns YES if the type labels in type label buffer tb
 * satisfy the boolean postfix type label expression, otherwise NO.
 */
#include "null.h"
#include "pdtyp.h"
#include "truefalse.h"
#include "yesno.h"

pdtmatch(postfix, tb)
	register PDTYP *postfix;	/* postfix expression struct */
	register char *tb;		/* type label buffer */
{
	register int i;			/* postfix expression index */
	register int j;			/* top-of-evaluation stack pointer */
	char *pdtfind();		/* find type label in buffer */

	j = -1;
	for (i = 0; i < postfix->pfxsize; i++)
		switch ((postfix->pfx)[i].p_class)
			{
			case B_ID:
				j++;
				(postfix->pfx)[i].p_label =
				pdtfind((postfix->pfx)[i].p_id, tb);
				(postfix->pfx)[i].p_sw = (postfix->eval)[j] =
				(postfix->pfx)[i].p_label != NULL;
				break;
			case B_OR:
				j--;
				(postfix->pfx)[i].p_sw = (postfix->eval)[j] =
				(postfix->eval)[j] == TRUE ||
				(postfix->eval)[j+1] == TRUE;
				break;
			case B_AND:
				j--;
				(postfix->pfx)[i].p_sw = (postfix->eval)[j] =
				(postfix->eval)[j] == TRUE &&
				(postfix->eval)[j+1] == TRUE;
				break;
			case B_NOT:
				(postfix->pfx)[i].p_sw = (postfix->eval)[j] =
				!(postfix->eval)[j];
				break;
			}
	return(((postfix->eval)[j] == TRUE) ? YES : NO);
}
