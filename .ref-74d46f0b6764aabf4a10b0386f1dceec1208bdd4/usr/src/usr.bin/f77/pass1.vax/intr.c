/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)intr.c	5.4 (Berkeley) %G%";
#endif /* not lint */

/*
 * intr.c
 *
 * Routines for handling intrinsic functions, f77 compiler pass 1, 4.2 BSD.
 *
 * University of Utah CS Dept modification history:
 *
 * $Log:	intr.c,v $
 * Revision 5.2  85/08/10  04:39:23  donn
 * Various changes from Jerry Berkman.  We now call the new builtin log10()
 * instead of the f77 library emulations; we figure out that builtins will
 * return type double instead of type float; we get rid of lots of
 * undocumented material; we ifdef 66 code and handle -r8/double flag.
 * 
 * Revision 5.1  85/08/10  03:47:37  donn
 * 4.3 alpha
 * 
 * Revision 1.4  85/02/22  00:54:59  donn
 * Mark intrinsic functions as having storage class STGINTR.  builtin()
 * always returns STGEXT nodes.  Notice that the reference to the function
 * in the external symbol table still uses STGEXT...  I hope this is right.
 * 
 * Revision 1.3  85/01/15  21:05:40  donn
 * Changes to distinguish explicit from implicit conversions with intrconv().
 * 
 * Revision 1.2  84/12/15  01:02:33  donn
 * Added a case for an integer*4 result from len() in Inline().  Previously
 * only -i2 provoked len() inline, sigh.
 * 
 */

#include "defs.h"

extern ftnint intcon[14];
extern double realcon[6];

union
	{
	int ijunk;
	struct Intrpacked bits;
	} packed;

struct Intrbits
	{
	int intrgroup /* :3 */;
	int intrstuff /* result type or number of specifics */;
	int intrno /* :7 */;
	};

LOCAL struct Intrblock
	{
	char intrfname[VL];
	struct Intrbits intrval;
	} intrtab[ ] =
{
"int", 		{ INTRCONV, TYLONG },
"real", 	{ INTRCONV, TYREAL },
"dble", 	{ INTRCONV, TYDREAL },
"dreal",	{ INTRCONV, TYDREAL },
"cmplx", 	{ INTRCONV, TYCOMPLEX },
"dcmplx", 	{ INTRCONV, TYDCOMPLEX },
"ifix", 	{ INTRCONV, TYLONG },
"idint", 	{ INTRCONV, TYLONG },
"float", 	{ INTRCONV, TYREAL },
"dfloat",	{ INTRCONV, TYDREAL },
"sngl", 	{ INTRCONV, TYREAL },
"ichar", 	{ INTRCONV, TYLONG },
"char", 	{ INTRCONV, TYCHAR },

"max", 		{ INTRMAX, TYUNKNOWN },
"max0", 	{ INTRMAX, TYLONG },
"amax0", 	{ INTRMAX, TYREAL },
"max1", 	{ INTRMAX, TYLONG },
"amax1", 	{ INTRMAX, TYREAL },
"dmax1", 	{ INTRMAX, TYDREAL },

"and",		{ INTRBOOL, TYUNKNOWN, OPBITAND },
"or",		{ INTRBOOL, TYUNKNOWN, OPBITOR },
"xor",		{ INTRBOOL, TYUNKNOWN, OPBITXOR },
"not",		{ INTRBOOL, TYUNKNOWN, OPBITNOT },
"lshift",	{ INTRBOOL, TYUNKNOWN, OPLSHIFT },
"rshift",	{ INTRBOOL, TYUNKNOWN, OPRSHIFT },

"min", 		{ INTRMIN, TYUNKNOWN },
"min0", 	{ INTRMIN, TYLONG },
"amin0", 	{ INTRMIN, TYREAL },
"min1", 	{ INTRMIN, TYLONG },
"amin1", 	{ INTRMIN, TYREAL },
"dmin1", 	{ INTRMIN, TYDREAL },

"aint", 	{ INTRGEN, 2, 0 },
"dint", 	{ INTRSPEC, TYDREAL, 1 },

"anint", 	{ INTRGEN, 2, 2 },
"dnint", 	{ INTRSPEC, TYDREAL, 3 },

"nint", 	{ INTRGEN, 4, 4 },
"idnint", 	{ INTRGEN, 2, 6 },

"abs", 		{ INTRGEN, 6, 8 },
"iabs", 	{ INTRGEN, 2, 9 },
"dabs", 	{ INTRSPEC, TYDREAL, 11 },
"cabs", 	{ INTRSPEC, TYREAL, 12 },
"zabs", 	{ INTRSPEC, TYDREAL, 13 },
"cdabs",	{ INTRSPEC, TYDREAL, 13 },

"mod", 		{ INTRGEN, 4, 14 },
"amod", 	{ INTRSPEC, TYREAL, 16 },
"dmod", 	{ INTRSPEC, TYDREAL, 17 },

"sign", 	{ INTRGEN, 4, 18 },
"isign", 	{ INTRGEN, 2, 19 },
"dsign", 	{ INTRSPEC, TYDREAL, 21 },

"dim", 		{ INTRGEN, 4, 22 },
"idim", 	{ INTRGEN, 2, 23 },
"ddim", 	{ INTRSPEC, TYDREAL, 25 },

"dprod", 	{ INTRSPEC, TYDREAL, 26 },

"len", 		{ INTRSPEC, TYLONG, 27 },
"index", 	{ INTRSPEC, TYLONG, 29 },

"imag", 	{ INTRGEN, 2, 31 },
"aimag", 	{ INTRSPEC, TYREAL, 31 },
"dimag", 	{ INTRSPEC, TYDREAL, 32 },

"conjg", 	{ INTRGEN, 2, 33 },
"dconjg", 	{ INTRSPEC, TYDCOMPLEX, 34 },

"sqrt", 	{ INTRGEN, 4, 35 },
"dsqrt", 	{ INTRSPEC, TYDREAL, 36 },
"csqrt", 	{ INTRSPEC, TYCOMPLEX, 37 },
"zsqrt", 	{ INTRSPEC, TYDCOMPLEX, 38 },
"cdsqrt",	{ INTRSPEC, TYDCOMPLEX, 38 },

"exp", 		{ INTRGEN, 4, 39 },
"dexp", 	{ INTRSPEC, TYDREAL, 40 },
"cexp", 	{ INTRSPEC, TYCOMPLEX, 41 },
"zexp", 	{ INTRSPEC, TYDCOMPLEX, 42 },
"cdexp",	{ INTRSPEC, TYDCOMPLEX, 42 },

"log", 		{ INTRGEN, 4, 43 },
"alog", 	{ INTRSPEC, TYREAL, 43 },
"dlog", 	{ INTRSPEC, TYDREAL, 44 },
"clog", 	{ INTRSPEC, TYCOMPLEX, 45 },
"zlog", 	{ INTRSPEC, TYDCOMPLEX, 46 },
"cdlog",	{ INTRSPEC, TYDCOMPLEX, 46 },

"log10", 	{ INTRGEN, 2, 47 },
"alog10", 	{ INTRSPEC, TYREAL, 47 },
"dlog10", 	{ INTRSPEC, TYDREAL, 48 },

"sin", 		{ INTRGEN, 4, 49 },
"dsin", 	{ INTRSPEC, TYDREAL, 50 },
"csin", 	{ INTRSPEC, TYCOMPLEX, 51 },
"zsin", 	{ INTRSPEC, TYDCOMPLEX, 52 },
"cdsin",	{ INTRSPEC, TYDCOMPLEX, 52 },

"cos", 		{ INTRGEN, 4, 53 },
"dcos", 	{ INTRSPEC, TYDREAL, 54 },
"ccos", 	{ INTRSPEC, TYCOMPLEX, 55 },
"zcos", 	{ INTRSPEC, TYDCOMPLEX, 56 },
"cdcos",	{ INTRSPEC, TYDCOMPLEX, 56 },

"tan", 		{ INTRGEN, 2, 57 },
"dtan", 	{ INTRSPEC, TYDREAL, 58 },

"asin", 	{ INTRGEN, 2, 59 },
"dasin", 	{ INTRSPEC, TYDREAL, 60 },

"acos", 	{ INTRGEN, 2, 61 },
"dacos", 	{ INTRSPEC, TYDREAL, 62 },

"atan", 	{ INTRGEN, 2, 63 },
"datan", 	{ INTRSPEC, TYDREAL, 64 },

"atan2", 	{ INTRGEN, 2, 65 },
"datan2", 	{ INTRSPEC, TYDREAL, 66 },

"sinh", 	{ INTRGEN, 2, 67 },
"dsinh", 	{ INTRSPEC, TYDREAL, 68 },

"cosh", 	{ INTRGEN, 2, 69 },
"dcosh", 	{ INTRSPEC, TYDREAL, 70 },

"tanh", 	{ INTRGEN, 2, 71 },
"dtanh", 	{ INTRSPEC, TYDREAL, 72 },

"lge",		{ INTRSPEC, TYLOGICAL, 73},
"lgt",		{ INTRSPEC, TYLOGICAL, 75},
"lle",		{ INTRSPEC, TYLOGICAL, 77},
"llt",		{ INTRSPEC, TYLOGICAL, 79},

"",		{ INTREND, 0, 0} };


LOCAL struct Specblock
	{
	char atype;
	char rtype;
	char nargs;
	char spxname[XL];
	char othername;	/* index into callbyvalue table */
	} spectab[ ] =
{
	{ TYREAL,TYREAL,1,"r_int" },
	{ TYDREAL,TYDREAL,1,"d_int" },

	{ TYREAL,TYREAL,1,"r_nint" },
	{ TYDREAL,TYDREAL,1,"d_nint" },

	{ TYREAL,TYSHORT,1,"h_nint" },
	{ TYREAL,TYLONG,1,"i_nint" },

	{ TYDREAL,TYSHORT,1,"h_dnnt" },
	{ TYDREAL,TYLONG,1,"i_dnnt" },

	{ TYREAL,TYREAL,1,"r_abs" },
	{ TYSHORT,TYSHORT,1,"h_abs" },
	{ TYLONG,TYLONG,1,"i_abs" },
	{ TYDREAL,TYDREAL,1,"d_abs" },
	{ TYCOMPLEX,TYREAL,1,"c_abs" },
	{ TYDCOMPLEX,TYDREAL,1,"z_abs" },

	{ TYSHORT,TYSHORT,2,"h_mod" },
	{ TYLONG,TYLONG,2,"i_mod" },
	{ TYREAL,TYREAL,2,"r_mod" },
	{ TYDREAL,TYDREAL,2,"d_mod" },

	{ TYREAL,TYREAL,2,"r_sign" },
	{ TYSHORT,TYSHORT,2,"h_sign" },
	{ TYLONG,TYLONG,2,"i_sign" },
	{ TYDREAL,TYDREAL,2,"d_sign" },

	{ TYREAL,TYREAL,2,"r_dim" },
	{ TYSHORT,TYSHORT,2,"h_dim" },
	{ TYLONG,TYLONG,2,"i_dim" },
	{ TYDREAL,TYDREAL,2,"d_dim" },

	{ TYREAL,TYDREAL,2,"d_prod" },

	{ TYCHAR,TYSHORT,1,"h_len" },
	{ TYCHAR,TYLONG,1,"i_len" },

	{ TYCHAR,TYSHORT,2,"h_indx" },
	{ TYCHAR,TYLONG,2,"i_indx" },

	{ TYCOMPLEX,TYREAL,1,"r_imag" },
	{ TYDCOMPLEX,TYDREAL,1,"d_imag" },
	{ TYCOMPLEX,TYCOMPLEX,1,"r_cnjg" },
	{ TYDCOMPLEX,TYDCOMPLEX,1,"d_cnjg" },

	{ TYREAL,TYREAL,1,"r_sqrt", 1 },
	{ TYDREAL,TYDREAL,1,"d_sqrt", 1 },
	{ TYCOMPLEX,TYCOMPLEX,1,"c_sqrt" },
	{ TYDCOMPLEX,TYDCOMPLEX,1,"z_sqrt" },

	{ TYREAL,TYREAL,1,"r_exp", 2 },
	{ TYDREAL,TYDREAL,1,"d_exp", 2 },
	{ TYCOMPLEX,TYCOMPLEX,1,"c_exp" },
	{ TYDCOMPLEX,TYDCOMPLEX,1,"z_exp" },

	{ TYREAL,TYREAL,1,"r_log", 3 },
	{ TYDREAL,TYDREAL,1,"d_log", 3 },
	{ TYCOMPLEX,TYCOMPLEX,1,"c_log" },
	{ TYDCOMPLEX,TYDCOMPLEX,1,"z_log" },

	{ TYREAL,TYREAL,1,"r_lg10", 14 },
	{ TYDREAL,TYDREAL,1,"d_lg10", 14 },

	{ TYREAL,TYREAL,1,"r_sin", 4 },
	{ TYDREAL,TYDREAL,1,"d_sin", 4 },
	{ TYCOMPLEX,TYCOMPLEX,1,"c_sin" },
	{ TYDCOMPLEX,TYDCOMPLEX,1,"z_sin" },

	{ TYREAL,TYREAL,1,"r_cos", 5 },
	{ TYDREAL,TYDREAL,1,"d_cos", 5 },
	{ TYCOMPLEX,TYCOMPLEX,1,"c_cos" },
	{ TYDCOMPLEX,TYDCOMPLEX,1,"z_cos" },

	{ TYREAL,TYREAL,1,"r_tan", 6 },
	{ TYDREAL,TYDREAL,1,"d_tan", 6 },

	{ TYREAL,TYREAL,1,"r_asin", 7 },
	{ TYDREAL,TYDREAL,1,"d_asin", 7 },

	{ TYREAL,TYREAL,1,"r_acos", 8 },
	{ TYDREAL,TYDREAL,1,"d_acos", 8 },

	{ TYREAL,TYREAL,1,"r_atan", 9 },
	{ TYDREAL,TYDREAL,1,"d_atan", 9 },

	{ TYREAL,TYREAL,2,"r_atn2", 10 },
	{ TYDREAL,TYDREAL,2,"d_atn2", 10 },

	{ TYREAL,TYREAL,1,"r_sinh", 11 },
	{ TYDREAL,TYDREAL,1,"d_sinh", 11 },

	{ TYREAL,TYREAL,1,"r_cosh", 12 },
	{ TYDREAL,TYDREAL,1,"d_cosh", 12 },

	{ TYREAL,TYREAL,1,"r_tanh", 13 },
	{ TYDREAL,TYDREAL,1,"d_tanh", 13 },

	{ TYCHAR,TYLOGICAL,2,"hl_ge" },
	{ TYCHAR,TYLOGICAL,2,"l_ge" },

	{ TYCHAR,TYLOGICAL,2,"hl_gt" },
	{ TYCHAR,TYLOGICAL,2,"l_gt" },

	{ TYCHAR,TYLOGICAL,2,"hl_le" },
	{ TYCHAR,TYLOGICAL,2,"l_le" },

	{ TYCHAR,TYLOGICAL,2,"hl_lt" },
	{ TYCHAR,TYLOGICAL,2,"l_lt" },

	{ TYDREAL,TYDREAL,2,"d_dprod"}  /* dprod() with dblflag */
} ;

char callbyvalue[ ][XL] =
	{
	"sqrt",
	"exp",
	"log",
	"sin",
	"cos",
	"tan",
	"asin",
	"acos",
	"atan",
	"atan2",
	"sinh",
	"cosh",
	"tanh",
	"log10"
	};

expptr intrcall(np, argsp, nargs)
Namep np;
struct Listblock *argsp;
int nargs;
{
int i, rettype;
Addrp ap;
register struct Specblock *sp;
register struct Chain *cp;
expptr Inline(), mkcxcon(), mkrealcon();
expptr q, ep;
int mtype;
int op;
int f1field, f2field, f3field;

packed.ijunk = np->vardesc.varno;
f1field = packed.bits.f1;
f2field = packed.bits.f2;
f3field = packed.bits.f3;
if(nargs == 0)
	goto badnargs;

mtype = 0;
for(cp = argsp->listp ; cp ; cp = cp->nextp)
	{
/* TEMPORARY */ ep = (expptr) (cp->datap);
/* TEMPORARY */	if( ISCONST(ep) && ep->headblock.vtype==TYSHORT )
/* TEMPORARY */		cp->datap = (tagptr) mkconv(tyint, ep);
	mtype = maxtype(mtype, ep->headblock.vtype);
	}

switch(f1field)
	{
	case INTRBOOL:
		op = f3field;
		if( ! ONEOF(mtype, MSKINT|MSKLOGICAL) )
			goto badtype;
		if(op == OPBITNOT)
			{
			if(nargs != 1)
				goto badnargs;
			q = mkexpr(OPBITNOT, argsp->listp->datap, ENULL);
			}
		else
			{
			if(nargs != 2)
				goto badnargs;
			q = mkexpr(op, argsp->listp->datap,
				argsp->listp->nextp->datap);
			}
		frchain( &(argsp->listp) );
		free( (charptr) argsp);
		return(q);

	case INTRCONV:
		if (nargs == 1)
			{
			if(argsp->listp->datap->headblock.vtype == TYERROR)
				{
				free( (charptr) argsp->listp->datap);
				frchain( &(argsp->listp) );
				free( (charptr) argsp);
				return( errnode() );
				}
			}
		else if (nargs == 2)
			{
			if(argsp->listp->nextp->datap->headblock.vtype == 
				TYERROR ||
				argsp->listp->datap->headblock.vtype == TYERROR)
				{
				free( (charptr) argsp->listp->nextp->datap);
				free( (charptr) argsp->listp->datap);
				frchain( &(argsp->listp) );
				free( (charptr) argsp);
				return( errnode() );
				}
			}
		rettype = f2field;
		if( ISCOMPLEX(rettype) && nargs==2)
			{
			expptr qr, qi;
			if(dblflag) rettype = TYDCOMPLEX;
			qr = (expptr) (argsp->listp->datap);
			qi = (expptr) (argsp->listp->nextp->datap);
			if(ISCONST(qr) && ISCONST(qi))
				q = mkcxcon(qr,qi);
			else	q = mkexpr(OPCONV,intrconv(rettype-2,qr),
					intrconv(rettype-2,qi));
			}
		else if(nargs == 1)
			{
			if(rettype == TYLONG) rettype = tyint;
			else if( dblflag )
				{
				if ( rettype == TYREAL )
					rettype = TYDREAL;
				else if( rettype == TYCOMPLEX )
					rettype = TYDCOMPLEX;
				}
			q = intrconv(rettype, argsp->listp->datap);
			}
		else goto badnargs;

		q->headblock.vtype = rettype;
		frchain(&(argsp->listp));
		free( (charptr) argsp);
		return(q);

	case INTRGEN:
		sp = spectab + f3field;
#ifdef ONLY66
		if(no66flag)
			if(sp->atype == mtype)
				goto specfunct;
			else err66("generic function");
#endif

		for(i=0; i<f2field ; ++i)
			if(sp->atype == mtype)
				goto specfunct;
			else
				++sp;
		goto badtype;

	case INTRSPEC:
		sp = spectab + f3field;
		if( dblflag )
			{
			/* convert specific complex functions to double complex:
			 *	 cabs,csqrt,cexp,clog,csin,ccos, aimag
			 * and convert real specifics to double:
			 *	 amod,alog,alog10
			 * (sqrt,cos,sin,... o.k. since go through INTRGEN)
			 */
			if( (sp->atype==TYCOMPLEX && (sp+1)->atype==TYDCOMPLEX)
				||(sp->atype==TYREAL && (sp+1)->atype==TYDREAL))
					sp++;
			}
	specfunct:
		if(tyint==TYLONG && ONEOF(sp->rtype,M(TYSHORT)|M(TYLOGICAL))
			&& (sp+1)->atype==sp->atype)
				++sp;

		if(nargs != sp->nargs)
			goto badnargs;
		if(mtype != sp->atype
			&& (!dblflag || f3field != 26 || mtype != TYDREAL ) )
				goto badtype;
		fixargs(YES, argsp);
		if(q = Inline(sp-spectab, mtype, argsp->listp))
			{
			frchain( &(argsp->listp) );
			free( (charptr) argsp);
			}
		else if(sp->othername)
			{
			ap = builtin(TYDREAL,
				varstr(XL, callbyvalue[sp->othername-1]) );
			ap->vstg = STGINTR;
			q = fixexpr( mkexpr(OPCCALL, ap, argsp) );
			if( sp->rtype != TYDREAL )
				q = mkconv( sp->rtype, q );
			}
		else
			{
			ap = builtin(sp->rtype, varstr(XL, sp->spxname) );
			ap->vstg = STGINTR;
			q = fixexpr( mkexpr(OPCALL, ap, argsp) );
			}
		return(q);

	case INTRMIN:
	case INTRMAX:
		if(nargs < 2)
			goto badnargs;
		if( ! ONEOF(mtype, MSKINT|MSKREAL) )
			goto badtype;
		argsp->vtype = mtype;
		q = mkexpr( (f1field==INTRMIN ? OPMIN : OPMAX), argsp, ENULL);

		q->headblock.vtype = mtype;
		rettype = f2field;
		if(rettype == TYLONG)
			rettype = tyint;
		else if(rettype == TYUNKNOWN)
			rettype = mtype;
		else if( dblflag && rettype == TYREAL )
			rettype = TYDREAL;
		return( intrconv(rettype, q) );

	default:
		fatali("intrcall: bad intrgroup %d", f1field);
	}
badnargs:
	errstr("bad number of arguments to intrinsic %s",
		varstr(VL,np->varname) );
	goto bad;

badtype:
	errstr("bad argument type to intrinsic %s", varstr(VL, np->varname) );

bad:
	return( errnode() );
}




intrfunct(s)
char s[VL];
{
register struct Intrblock *p;
char nm[VL];
register int i;

for(i = 0 ; i<VL ; ++s)
	nm[i++] = (*s==' ' ? '\0' : *s);

for(p = intrtab; p->intrval.intrgroup!=INTREND ; ++p)
	{
	if( eqn(VL, nm, p->intrfname) )
		{
		packed.bits.f1 = p->intrval.intrgroup;
		packed.bits.f2 = p->intrval.intrstuff;
		packed.bits.f3 = p->intrval.intrno;
		return(packed.ijunk);
		}
	}

return(0);
}





Addrp intraddr(np)
Namep np;
{
Addrp q;
register struct Specblock *sp;
int f3field;

if(np->vclass!=CLPROC || np->vprocclass!=PINTRINSIC)
	fatalstr("intraddr: %s is not intrinsic", varstr(VL,np->varname));
packed.ijunk = np->vardesc.varno;
f3field = packed.bits.f3;

switch(packed.bits.f1)
	{
	case INTRGEN:
		/* imag, log, and log10 arent specific functions */
		if(f3field==31 || f3field==43 || f3field==47)
			goto bad;

	case INTRSPEC:
		sp = spectab + f3field;
		if( dblflag )
			{
			if((sp->atype==TYCOMPLEX && (sp+1)->atype==TYDCOMPLEX)
				||(sp->atype==TYREAL && (sp+1)->atype==TYDREAL))
					sp++;
			else if( f3field==4 )
					sp += 2;  /* h_nint -> h_dnnt */
			else if( f3field==8 || f3field==18 || f3field==22)
					sp += 3;  /* r_{abs,sign,dim} ->d_... */
			else if( f3field==26 )
					sp = spectab + 81; /* dprod */

			}
		if(tyint==TYLONG && sp->rtype==TYSHORT)
			++sp;
		q = builtin(sp->rtype, varstr(XL,sp->spxname) );
		q->vstg = STGINTR;
		return(q);

	case INTRCONV:
	case INTRMIN:
	case INTRMAX:
	case INTRBOOL:
	bad:
		errstr("cannot pass %s as actual",
			varstr(VL,np->varname));
		return( (Addrp) errnode() );
	}
fatali("intraddr: impossible f1=%d\n", (int) packed.bits.f1);
/* NOTREACHED */
}





expptr Inline(fno, type, args)
int fno;
int type;
struct Chain *args;
{
register expptr q, t, t1;

switch(fno)
	{
	case 8:	/* real abs */
	case 9:	/* short int abs */
	case 10:	/* long int abs */
	case 11:	/* double precision abs */
		if( addressable(q = (expptr) (args->datap)) )
			{
			t = q;
			q = NULL;
			}
		else
			t = (expptr) mktemp(type,PNULL);
		t1 = mkexpr(OPQUEST,
			mkexpr(OPLE, intrconv(type,ICON(0)), cpexpr(t)),
			mkexpr(OPCOLON, cpexpr(t),
				mkexpr(OPNEG, cpexpr(t), ENULL) ));
		if(q)
			t1 = mkexpr(OPCOMMA, mkexpr(OPASSIGN, cpexpr(t),q), t1);
		frexpr(t);
		return(t1);

	case 26:	/* dprod */
		q = mkexpr(OPSTAR, intrconv(TYDREAL,args->datap), args->nextp->datap);
		return(q);

	case 27:	/* len of character string */
	case 28:
		q = (expptr) cpexpr(args->datap->headblock.vleng);
		frexpr(args->datap);
		return(q);

	case 14:	/* half-integer mod */
	case 15:	/* mod */
		return( mkexpr(OPMOD, (expptr) (args->datap),
			(expptr) (args->nextp->datap) ));
	}
return(NULL);
}
