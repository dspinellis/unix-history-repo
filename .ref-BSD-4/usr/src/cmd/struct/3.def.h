#define RECURSE(p,v,r)	{ for (r = 0; r < CHILDNUM(v); ++r) if (DEFINED(LCHILD(v,r))) p(LCHILD(v,r)); if (DEFINED(RSIB(v))) p(RSIB(v)); }

#define IFTHEN(v)		( NTYPE(v) == IFVX && !DEFINED(LCHILD(v,ELSE)))

#define BRK(v)	FATH(v)		/* lexical successor of v, for ITERVX only */
#define LABEL(v)	REACH(v)
