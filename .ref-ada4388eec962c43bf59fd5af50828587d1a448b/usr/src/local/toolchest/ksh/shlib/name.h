/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)name.h	1.1 */

/* Nodes can have all kinds of values */
union Namval
{
	char		*cp;
	int		*ip;
	char		c;
	int		i;
	unsigned	u;
	long		*lp;
	double		*dp;	/* for floating point arithmetic */
	struct Namaray	*aray;	/* for array node */
	union Namval	*up;	/* for indirect node */
	struct Bfunction *fp;	/* builtin-function like $RANDOM */
	struct Ufunction *rp;	/* shell user defined functions */
};

/* each Namnod and each array element has one of these */
struct Nodval
{
        unsigned        namflg;         /* attributes */
        union Namval    namval;         /* value field */
};

/* This is an array template */
struct Namaray
{
	unsigned short		adot;		/* index of last reference */
	unsigned short		maxi;		/* maximum index of array */
        struct Nodval	*val[1];	/* array of value holders */
};

/* This is a template for a storage tree */
struct Amemory
{
	struct Amemory  *nexttree;	/* search trees can be chained */
        short           memsize;        /* number of listheads */
        struct Namnod   *memhead[1];    /* listhead pointers   */
};

/* This describes a named node */
struct Namnod
{
        struct Nodval   value;          /* determines value of the item */
	struct Namnod	*namnxt;	/* pointer to next Namnod  */
	char		*namid;		/* pointer to name of item */
	short 		namsz;		/* size of item */
};

/* This describes a builtin function node */
struct Bfunction
{
	long	(*f_vp)();		/* value function */
	int	(*f_ap)();		/* assignment function */
};

/* This describes a user defined function node */
struct Ufunction
{
	long	hoffset;		/* offset into history file */
	int	**ptree;		/* address of parse tree */
};

#define MEMSIZE   32*sizeof(int)	/* default memory size for shell.
						Must be a power of 2 */
#define PSEPS	":"
#define ARRMAX   512	/* maximum number of elements in an array */
#define ARRINCR    16	/* number of elements to grow when array bound exceeded 
				 Must be a power of 2 */
#define MAXTREES   20	/* maximum number of mounted search trees */

#define NO_SUBSCRIPT	ARRMAX	/* subscript not defined */
#ifndef NULL
#define NULL	0
#endif

/* types of namenode items */

#define N_DEFAULT 0
#define INT_GER		I_FLAG	/* integer type */
#define CPOIN_TER	W_FLAG
#define N_AVAIL		B_FLAG	/* node is logically non-existent, blocked */
#define C_WRITE		C_FLAG	/* make copy of node on assignment */
#define ARRAY		F_FLAG	/* node is an array */
#define IN_DIR		P_FLAG	/* value is a pointer to a value node */
#define N_ALLOC		V_FLAG	/* don't allocate space for the value */
#define N_FREE		S_FLAG	/* don't free the space when releasing value */
#define T_FORM		T_FLAG	/* program usable tflag */
#define L_TO_U		U_FLAG	/* convert to uppercase */
#define U_TO_L		L_FLAG	/* convert to lowercase */
#define Z_FILL		Z_FLAG	/* right justify and fill with leading zeros */
#define R_JUST		W_FLAG	/* right justify and blank fill */
#define L_JUST		O_FLAG	/* left justify and blank fill */
#define HOST_N		M_FLAG	/* convert to host file name in non-unix */
#define N_EXPORT	X_FLAG	/* export bit */
#define N_RDONLY	R_FLAG	/* readonly bit */
#define N_IMPORT	N_FLAG	/* imported from environment */


/* The following are used with INT_FLG */
#define	BLT_NOD	M_FLAG		/* builtin function flag */
#define OC_TAL	O_FLAG
#define UN_SIGN	U_FLAG

#define is_afunction(n)	(((n)->value.namflg&(~(N_EXPORT|T_FLAG)))==(INT_GER|L_FLAG))
#define	funtree(n)	((n)->value.namval.rp->ptree)
#define NO_ALIAS	(L_TO_U|U_TO_L|N_FLAG)


/* namenode states */

#define NO_ERR_ 0
#define SANERR  E_FLAG

#define ADD_NOD	1 /* add node if not found */
#define CHK_FOR	2 /* look for only if valid name */
#define	RE_USE	4 /* used for efficiency in multitree searches */

/* NAMNOD MACROS */

/* ...  for arrays */

#define arayp(v)        (v->value.namval.aray)
#define curdot(n)	((arayp(n))->adot)
#define abound(n)       ((int)((n)->value.namval.aray->maxi))
#ifdef KSHELL
#define setdot(n,i)     ((n)->value.namval.aray->adot = i)
#endif	/* KSHELL */

/* ... for attributes */

#define namflag(n)	(n)->value.namflg
#define attest(n,f)     (namflag(n) & (f))
#ifdef KSHELL
#define attrib(n,f)	((n)->value.namflg |= f)
#define sattrib(n,f)	((n)->value.namflg = f)
#define pattrib(n,f)	((n)->value.namflg &= f)
#else
#define attrib(n,f)     (chattrib (n, namflag(n)|(f)))
#define sattrib(n,f)    (chattrib (n, f))
#define pattrib(n,f)    (chattrib (n, namflag(n)&(f)))
#endif	/* KSHELL */

/* ... etc */

#define isnull(n)       ((n)->value.namval.cp == NULL)  /* strings only */
#define freeble(nv)     (((int)(nv)) & 01)
#define mrkfree(nv)     ((struct Nodval*)(((int)(nv)) | 01))
#define unmark(nv)      ((struct Nodval*)(((int)(nv)) & ~01))
#define errorp(np)     ((np)->namerr)
#define asscadr(np,val)	assiadr(np,((int*)(val)))

extern char synmsg[];
extern char subscript[];
extern char badnum[];
extern char badparam[];
extern char divzero[];
extern char hdigits[];
extern char wtfailed[];
extern char notid[];
extern struct Amemory *namep;
extern int lastbase;
