/*-
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 *
 *	@(#)3.def.h	8.1 (Berkeley) 6/6/93
 */

#define RECURSE(p,v,r)	{ for (r = 0; r < CHILDNUM(v); ++r) if (DEFINED(LCHILD(v,r))) p(LCHILD(v,r)); if (DEFINED(RSIB(v))) p(RSIB(v)); }

#define IFTHEN(v)		( NTYPE(v) == IFVX && !DEFINED(LCHILD(v,ELSE)))

#define BRK(v)	FATH(v)		/* lexical successor of v, for ITERVX only */
#define LABEL(v)	REACH(v)
