/*-
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 *
 *	@(#)2.def.h	8.1 (Berkeley) 6/6/93
 */

extern int accessnum;		/* number of nodes accessible from START */
extern VERT *after;		/* node numbers associated with after numbers of depth first search */
extern int *ntobef;		/* before numbers associated with nodes */
extern int *ntoaft;		/* after numbers associated with nodes */
