/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)putpcc.c	5.3 (Berkeley) %G%";
#endif not lint

/*
 * putpcc.c
 *
 * Intermediate code generation for S. C. Johnson C compilers
 * New version using binary polish postfix intermediate
 *
 * University of Utah CS Dept modification history:
 *
 * $Header: putpcc.c,v 5.2 86/03/04 17:49:38 donn Exp $
 * $Log:	putpcc.c,v $
 * Revision 5.2  86/03/04  17:49:38  donn
 * Change putct1() to emit the memoffset before the vleng -- the memoffset
 * may define a temporary which is used by the vleng to avoid repeated
 * evaluation of an expression with side effects.
 * 
 * Revision 5.1  85/08/10  03:49:26  donn
 * 4.3 alpha
 * 
 * Revision 3.2  85/03/25  09:35:57  root
 * fseek return -1 on error.
 * 
 * Revision 3.1  85/02/27  19:06:55  donn
 * Changed to use pcc.h instead of pccdefs.h.
 * 
 * Revision 2.12  85/02/22  01:05:54  donn
 * putaddr() didn't know about intrinsic functions...
 * 
 * Revision 2.11  84/11/28  21:28:49  donn
 * Hacked putop() to handle any character expression being converted to int,
 * not just function calls.  Previously it bombed on concatenations.
 * 
 * Revision 2.10  84/11/01  22:07:07  donn
 * Yet another try at getting putop() to work right.  It appears that the
 * second pass can't abide certain explicit conversions (e.g. short to long)
 * so the conversion code in putop() tries to remove them.  I think this
 * version (finally) works.
 * 
 * Revision 2.9  84/10/29  02:30:57  donn
 * Earlier fix to putop() for conversions was insufficient -- we NEVER want to
 * see the type of the left operand of the thing left over from stripping off
 * conversions...
 * 
 * Revision 2.8  84/09/18  03:09:21  donn
 * Fixed bug in putop() where the left operand of an addrblock was being
 * extracted...  This caused an extremely obscure conversion error when
 * an array of longs was subscripted by a short.
 * 
 * Revision 2.7  84/08/19  20:10:19  donn
 * Removed stuff in putbranch that treats STGARG parameters specially -- the
 * bug in the code generation pass that motivated it has been fixed.
 * 
 * Revision 2.6  84/08/07  21:32:23  donn
 * Bumped the size of the buffer for the intermediate code file from 0.5K
 * to 4K on a VAX.
 * 
 * Revision 2.5  84/08/04  20:26:43  donn
 * Fixed a goof in the new putbranch() -- it now calls mkaltemp instead of
 * mktemp().  Correction due to Jerry Berkman.
 * 
 * Revision 2.4  84/07/24  19:07:15  donn
 * Fixed bug reported by Craig Leres in which putmnmx() mistakenly assumed
 * that mkaltemp() returns tempblocks, and tried to free them with frtemp().
 * 
 * Revision 2.3  84/07/19  17:22:09  donn
 * Changed putch1() so that OPPAREN expressions of type CHARACTER are legal.
 * 
 * Revision 2.2  84/07/19  12:30:38  donn
 * Fixed a type clash in Bob Corbett's new putbranch().
 * 
 * Revision 2.1  84/07/19  12:04:27  donn
 * Changed comment headers for UofU.
 * 
 * Revision 1.8  84/07/19  11:38:23  donn
 * Replaced putbranch() routine so that you can ASSIGN into argument variables.
 * The code is from Bob Corbett, donated by Jerry Berkman.
 * 
 * Revision 1.7  84/05/31  00:48:32  donn
 * Fixed an extremely obscure bug dealing with the comparison of CHARACTER*1
 * expressions -- a foulup in the order of COMOP and the comparison caused
 * one operand of the comparison to be garbage.
 * 
 * Revision 1.6  84/04/16  09:54:19  donn
 * Backed out earlier fix for bug where items in the argtemplist were
 * (incorrectly) being given away; this is now fixed in mkargtemp().
 * 
 * Revision 1.5  84/03/23  22:49:48  donn
 * Took out the initialization of the subroutine argument temporary list in
 * putcall() -- it needs to be done once per statement instead of once per call.
 * 
 * Revision 1.4  84/03/01  06:48:05  donn
 * Fixed bug in Bob Corbett's code for argument temporaries that caused an
 * addrblock to get thrown out inadvertently when it was needed for recycling
 * purposes later on.
 * 
 * Revision 1.3  84/02/26  06:32:38  donn
 * Added Berkeley changes to move data definitions around and reduce offsets.
 * 
 * Revision 1.2  84/02/26  06:27:45  donn
 * Added code to catch TTEMP values passed to putx().
 * 
 */

#if FAMILY != PCC
	WRONG put FILE !!!!
#endif

#include "defs.h"
#include <pcc.h>

Addrp putcall(), putcxeq(), putcx1(), realpart();
expptr imagpart();
ftnint lencat();

#define FOUR 4
extern int ops2[];
extern int types2[];

#if HERE==VAX
#define PCC_BUFFMAX 1024
#else
#define PCC_BUFFMAX 128
#endif
static long int p2buff[PCC_BUFFMAX];
static long int *p2bufp		= &p2buff[0];
static long int *p2bufend	= &p2buff[PCC_BUFFMAX];


puthead(s, class)
char *s;
int class;
{
char buff[100];
#if TARGET == VAX
	if(s)
		p2ps("\t.globl\t_%s", s);
#endif
/* put out fake copy of left bracket line, to be redone later */
if( ! headerdone )
	{
#if FAMILY == PCC
	p2flush();
#endif
	headoffset = ftell(textfile);
	prhead(textfile);
	headerdone = YES;
	p2triple(PCCF_FEXPR, (strlen(infname)+FOUR-1)/FOUR, 0);
	p2str(infname);
#if TARGET == PDP11
	/* fake jump to start the optimizer */
	if(class != CLBLOCK)
		putgoto( fudgelabel = newlabel() );
#endif

#if TARGET == VAX
	/* jump from top to bottom */
	if(s!=CNULL && class!=CLBLOCK)
		{
		int proflab = newlabel();
		p2ps("_%s:", s);
		p2pi("\t.word\tLWM%d", procno);
		prsave(proflab);
		p2pi("\tjbr\tL%d", fudgelabel = newlabel());
		}
#endif
	}
}





/* It is necessary to precede each procedure with a "left bracket"
 * line that tells pass 2 how many register variables and how
 * much automatic space is required for the function.  This compiler
 * does not know how much automatic space is needed until the
 * entire procedure has been processed.  Therefore, "puthead"
 * is called at the begining to record the current location in textfile,
 * then to put out a placeholder left bracket line.  This procedure
 * repositions the file and rewrites that line, then puts the
 * file pointer back to the end of the file.
 */

putbracket()
{
long int hereoffset;

#if FAMILY == PCC
	p2flush();
#endif
hereoffset = ftell(textfile);
if(fseek(textfile, headoffset, 0) == -1)
	fatal("fseek failed");
prhead(textfile);
if(fseek(textfile, hereoffset, 0) == -1)
	fatal("fseek failed 2");
}




putrbrack(k)
int k;
{
p2op(PCCF_FRBRAC, k);
}



putnreg()
{
}






puteof()
{
p2op(PCCF_FEOF, 0);
p2flush();
}



putstmt()
{
p2triple(PCCF_FEXPR, 0, lineno);
}




/* put out code for if( ! p) goto l  */
putif(p,l)
register expptr p;
int l;
{
register int k;

if( ( k = (p = fixtype(p))->headblock.vtype) != TYLOGICAL)
	{
	if(k != TYERROR)
		err("non-logical expression in IF statement");
	frexpr(p);
	}
else
	{
	putex1(p);
	p2icon( (long int) l , PCCT_INT);
	p2op(PCC_CBRANCH, 0);
	putstmt();
	}
}





/* put out code for  goto l   */
putgoto(label)
int label;
{
p2triple(PCC_GOTO, 1, label);
putstmt();
}


/* branch to address constant or integer variable */
putbranch(p)
register Addrp p;
{
  putex1((expptr) p);
  p2op(PCC_GOTO, PCCT_INT);
  putstmt();
}



/* put out label  l:     */
putlabel(label)
int label;
{
p2op(PCCF_FLABEL, label);
}




putexpr(p)
expptr p;
{
putex1(p);
putstmt();
}




putcmgo(index, nlab, labs)
expptr index;
int nlab;
struct Labelblock *labs[];
{
int i, labarray, skiplabel;

if(! ISINT(index->headblock.vtype) )
	{
	execerr("computed goto index must be integer", CNULL);
	return;
	}

#if TARGET == VAX
	/* use special case instruction */
	vaxgoto(index, nlab, labs);
#else
	labarray = newlabel();
	preven(ALIADDR);
	prlabel(asmfile, labarray);
	prcona(asmfile, (ftnint) (skiplabel = newlabel()) );
	for(i = 0 ; i < nlab ; ++i)
		if( labs[i] )
			prcona(asmfile, (ftnint)(labs[i]->labelno) );
	prcmgoto(index, nlab, skiplabel, labarray);
	putlabel(skiplabel);
#endif
}

putx(p)
expptr p;
{
char *memname();
int opc;
int ncomma;
int type, k;

if (!p)
	return;

switch(p->tag)
	{
	case TERROR:
		free( (charptr) p );
		break;

	case TCONST:
		switch(type = p->constblock.vtype)
			{
			case TYLOGICAL:
				type = tyint;
			case TYLONG:
			case TYSHORT:
				p2icon(p->constblock.constant.ci, types2[type]);
				free( (charptr) p );
				break;

			case TYADDR:
				p2triple(PCC_ICON, 1, PCCT_INT|PCCTM_PTR);
				p2word(0L);
				p2name(memname(STGCONST,
					(int) p->constblock.constant.ci) );
				free( (charptr) p );
				break;

			default:
				putx( putconst(p) );
				break;
			}
		break;

	case TEXPR:
		switch(opc = p->exprblock.opcode)
			{
			case OPCALL:
			case OPCCALL:
				if( ISCOMPLEX(p->exprblock.vtype) )
					putcxop(p);
				else	putcall(p);
				break;

			case OPMIN:
			case OPMAX:
				putmnmx(p);
				break;


			case OPASSIGN:
				if(ISCOMPLEX(p->exprblock.leftp->headblock.vtype)
				|| ISCOMPLEX(p->exprblock.rightp->headblock.vtype) )
					frexpr( putcxeq(p) );
				else if( ISCHAR(p) )
					putcheq(p);
				else
					goto putopp;
				break;

			case OPEQ:
			case OPNE:
				if( ISCOMPLEX(p->exprblock.leftp->headblock.vtype) ||
				    ISCOMPLEX(p->exprblock.rightp->headblock.vtype) )
					{
					putcxcmp(p);
					break;
					}
			case OPLT:
			case OPLE:
			case OPGT:
			case OPGE:
				if(ISCHAR(p->exprblock.leftp))
					{
					putchcmp(p);
					break;
					}
				goto putopp;

			case OPPOWER:
				putpower(p);
				break;

			case OPSTAR:
#if FAMILY == PCC
				/*   m * (2**k) -> m<<k   */
				if(INT(p->exprblock.leftp->headblock.vtype) &&
				   ISICON(p->exprblock.rightp) &&
				   ( (k = log2(p->exprblock.rightp->constblock.constant.ci))>0) )
					{
					p->exprblock.opcode = OPLSHIFT;
					frexpr(p->exprblock.rightp);
					p->exprblock.rightp = ICON(k);
					goto putopp;
					}
#endif

			case OPMOD:
				goto putopp;
			case OPPLUS:
			case OPMINUS:
			case OPSLASH:
			case OPNEG:
				if( ISCOMPLEX(p->exprblock.vtype) )
					putcxop(p);
				else	goto putopp;
				break;

			case OPCONV:
				if( ISCOMPLEX(p->exprblock.vtype) )
					putcxop(p);
				else if( ISCOMPLEX(p->exprblock.leftp->headblock.vtype) )
					{
					ncomma = 0;
					putx( mkconv(p->exprblock.vtype,
						realpart(putcx1(p->exprblock.leftp,
							&ncomma))));
					putcomma(ncomma, p->exprblock.vtype, NO);
					free( (charptr) p );
					}
				else	goto putopp;
				break;

			case OPNOT:
			case OPOR:
			case OPAND:
			case OPEQV:
			case OPNEQV:
			case OPADDR:
			case OPPLUSEQ:
			case OPSTAREQ:
			case OPCOMMA:
			case OPQUEST:
			case OPCOLON:
			case OPBITOR:
			case OPBITAND:
			case OPBITXOR:
			case OPBITNOT:
			case OPLSHIFT:
			case OPRSHIFT:
		putopp:
				putop(p);
				break;

			case OPPAREN:
				putx (p->exprblock.leftp);
				break;
			default:
				badop("putx", opc);
			}
		break;

	case TADDR:
		putaddr(p, YES);
		break;

	case TTEMP:
		/*
		 * This type is sometimes passed to putx when errors occur
		 *	upstream, I don't know why.
		 */
		frexpr(p);
		break;

	default:
		badtag("putx", p->tag);
	}
}



LOCAL putop(p)
expptr p;
{
int k;
expptr lp, tp;
int pt, lt, tt;
int comma;
Addrp putch1();

switch(p->exprblock.opcode)	/* check for special cases and rewrite */
	{
	case OPCONV:
		tt = pt = p->exprblock.vtype;
		lp = p->exprblock.leftp;
		lt = lp->headblock.vtype;
		if (pt == TYREAL && lt == TYDREAL)
			{
			putx(lp);
			p2op(PCC_SCONV, PCCT_FLOAT);
			return;
			}
		while(p->tag==TEXPR && p->exprblock.opcode==OPCONV &&
		      ( (ISREAL(pt)&&ISREAL(lt)) ||
			(INT(pt)&&(ONEOF(lt,MSKINT|MSKADDR|MSKCHAR|M(TYSUBR)))) ))
			{
#if SZINT < SZLONG
			if(lp->tag != TEXPR)
				{
				if(pt==TYINT && lt==TYLONG)
					break;
				if(lt==TYINT && pt==TYLONG)
					break;
				}
#endif

#if TARGET == VAX
			if(pt==TYDREAL && lt==TYREAL)
				{
				if(lp->tag==TEXPR &&
				   lp->exprblock.opcode==OPCONV &&
				   lp->exprblock.leftp->headblock.vtype==TYDREAL)
					{
					putx(lp->exprblock.leftp);
					p2op(PCC_SCONV, PCCT_FLOAT);
					p2op(PCC_SCONV, PCCT_DOUBLE);
					free( (charptr) p );
					return;
					}
				else break;
				}
#endif
			if(lt==TYCHAR && lp->tag==TEXPR)
				{
				int ncomma = 0;
				p->exprblock.leftp = (expptr) putch1(lp, &ncomma);
				putop(p);
				putcomma(ncomma, pt, NO);
				free( (charptr) p );
				return;
				}
			free( (charptr) p );
			p = lp;
			pt = lt;
			if (p->tag == TEXPR)
				{
				lp = p->exprblock.leftp;
				lt = lp->headblock.vtype;
				}
			}
		if(p->tag==TEXPR && p->exprblock.opcode==OPCONV)
			break;
		putx(p);
		if (types2[tt] != types2[pt] &&
		    ! ( (ISREAL(tt)&&ISREAL(pt)) ||
			(INT(tt)&&(ONEOF(pt,MSKINT|MSKADDR|MSKCHAR|M(TYSUBR)))) ))
			p2op(PCC_SCONV,types2[tt]);
		return;

	case OPADDR:
		comma = NO;
		lp = p->exprblock.leftp;
		if(lp->tag != TADDR)
			{
			tp = (expptr) mkaltemp
				(lp->headblock.vtype,lp->headblock.vleng);
			putx( mkexpr(OPASSIGN, cpexpr(tp), lp) );
			lp = tp;
			comma = YES;
			}
		putaddr(lp, NO);
		if(comma)
			putcomma(1, TYINT, NO);
		free( (charptr) p );
		return;
#if TARGET == VAX
/* take advantage of a glitch in the code generator that does not check
   the type clash in an assignment or comparison of an integer zero and
   a floating left operand, and generates optimal code for the correct
   type.  (The PCC has no floating-constant node to encode this correctly.)
*/
	case OPASSIGN:
	case OPLT:
	case OPLE:
	case OPGT:
	case OPGE:
	case OPEQ:
	case OPNE:
		if(ISREAL(p->exprblock.leftp->headblock.vtype) &&
		   ISREAL(p->exprblock.rightp->headblock.vtype) &&
		   ISCONST(p->exprblock.rightp) &&
		   p->exprblock.rightp->constblock.constant.cd[0]==0)
			{
			p->exprblock.rightp->constblock.vtype = TYINT;
			p->exprblock.rightp->constblock.constant.ci = 0;
			}
#endif
	}

if( (k = ops2[p->exprblock.opcode]) <= 0)
	badop("putop", p->exprblock.opcode);
putx(p->exprblock.leftp);
if(p->exprblock.rightp)
	putx(p->exprblock.rightp);
p2op(k, types2[p->exprblock.vtype]);

if(p->exprblock.vleng)
	frexpr(p->exprblock.vleng);
free( (charptr) p );
}

putforce(t, p)
int t;
expptr p;
{
p = mkconv(t, fixtype(p));
putx(p);
p2op(PCC_FORCE,
	(t==TYSHORT ? PCCT_SHORT : (t==TYLONG ? PCCT_LONG : PCCT_DOUBLE)) );
putstmt();
}



LOCAL putpower(p)
expptr p;
{
expptr base;
Addrp t1, t2;
ftnint k;
int type;
int ncomma;

if(!ISICON(p->exprblock.rightp) ||
    (k = p->exprblock.rightp->constblock.constant.ci)<2)
	fatal("putpower: bad call");
base = p->exprblock.leftp;
type = base->headblock.vtype;

if ((k == 2) && base->tag == TADDR && ISCONST(base->addrblock.memoffset))
{
	putx( mkexpr(OPSTAR,cpexpr(base),cpexpr(base)));
	
	return;
}
t1 = mkaltemp(type, PNULL);
t2 = NULL;
ncomma = 1;
putassign(cpexpr(t1), cpexpr(base) );

for( ; (k&1)==0 && k>2 ; k>>=1 )
	{
	++ncomma;
	putsteq(t1, t1);
	}

if(k == 2)
	putx( mkexpr(OPSTAR, cpexpr(t1), cpexpr(t1)) );
else
	{
	t2 = mkaltemp(type, PNULL);
	++ncomma;
	putassign(cpexpr(t2), cpexpr(t1));
	
	for(k>>=1 ; k>1 ; k>>=1)
		{
		++ncomma;
		putsteq(t1, t1);
		if(k & 1)
			{
			++ncomma;
			putsteq(t2, t1);
			}
		}
	putx( mkexpr(OPSTAR, cpexpr(t2),
		mkexpr(OPSTAR, cpexpr(t1), cpexpr(t1)) ));
	}
putcomma(ncomma, type, NO);
frexpr(t1);
if(t2)
	frexpr(t2);
frexpr(p);
}




LOCAL Addrp intdouble(p, ncommap)
Addrp p;
int *ncommap;
{
register Addrp t;

t = mkaltemp(TYDREAL, PNULL);
++*ncommap;
putassign(cpexpr(t), p);
return(t);
}





LOCAL Addrp putcxeq(p)
register expptr p;
{
register Addrp lp, rp;
int ncomma;

if(p->tag != TEXPR)
	badtag("putcxeq", p->tag);

ncomma = 0;
lp = putcx1(p->exprblock.leftp, &ncomma);
rp = putcx1(p->exprblock.rightp, &ncomma);
putassign(realpart(lp), realpart(rp));
if( ISCOMPLEX(p->exprblock.vtype) )
	{
	++ncomma;
	putassign(imagpart(lp), imagpart(rp));
	}
putcomma(ncomma, TYREAL, NO);
frexpr(rp);
free( (charptr) p );
return(lp);
}



LOCAL putcxop(p)
expptr p;
{
Addrp putcx1();
int ncomma;

ncomma = 0;
putaddr( putcx1(p, &ncomma), NO);
putcomma(ncomma, TYINT, NO);
}



LOCAL Addrp putcx1(p, ncommap)
register expptr p;
int *ncommap;
{
expptr q;
Addrp lp, rp;
register Addrp resp;
int opcode;
int ltype, rtype;
expptr mkrealcon();

if(p == NULL)
	return(NULL);

switch(p->tag)
	{
	case TCONST:
		if( ISCOMPLEX(p->constblock.vtype) )
			p = (expptr) putconst(p);
		return( (Addrp) p );

	case TADDR:
		if( ! addressable(p) )
			{
			++*ncommap;
			resp = mkaltemp(tyint, PNULL);
			putassign( cpexpr(resp), p->addrblock.memoffset );
			p->addrblock.memoffset = (expptr)resp;
			}
		return( (Addrp) p );

	case TEXPR:
		if( ISCOMPLEX(p->exprblock.vtype) )
			break;
		++*ncommap;
		resp = mkaltemp(TYDREAL, NO);
		putassign( cpexpr(resp), p);
		return(resp);

	default:
		badtag("putcx1", p->tag);
	}

opcode = p->exprblock.opcode;
if(opcode==OPCALL || opcode==OPCCALL)
	{
	++*ncommap;
	return( putcall(p) );
	}
else if(opcode == OPASSIGN)
	{
	++*ncommap;
	return( putcxeq(p) );
	}
resp = mkaltemp(p->exprblock.vtype, PNULL);
if(lp = putcx1(p->exprblock.leftp, ncommap) )
	ltype = lp->vtype;
if(rp = putcx1(p->exprblock.rightp, ncommap) )
	rtype = rp->vtype;

switch(opcode)
	{
	case OPPAREN:
		frexpr (resp);
		resp = lp;
		lp = NULL;
		break;

	case OPCOMMA:
		frexpr(resp);
		resp = rp;
		rp = NULL;
		break;

	case OPNEG:
		putassign( realpart(resp), mkexpr(OPNEG, realpart(lp), ENULL) );
		putassign( imagpart(resp), mkexpr(OPNEG, imagpart(lp), ENULL) );
		*ncommap += 2;
		break;

	case OPPLUS:
	case OPMINUS:
		putassign( realpart(resp),
			mkexpr(opcode, realpart(lp), realpart(rp) ));
		if(rtype < TYCOMPLEX)
			putassign( imagpart(resp), imagpart(lp) );
		else if(ltype < TYCOMPLEX)
			{
			if(opcode == OPPLUS)
				putassign( imagpart(resp), imagpart(rp) );
			else	putassign( imagpart(resp),
					mkexpr(OPNEG, imagpart(rp), ENULL) );
			}
		else
			putassign( imagpart(resp),
				mkexpr(opcode, imagpart(lp), imagpart(rp) ));

		*ncommap += 2;
		break;

	case OPSTAR:
		if(ltype < TYCOMPLEX)
			{
			if( ISINT(ltype) )
				lp = intdouble(lp, ncommap);
			putassign( realpart(resp),
				mkexpr(OPSTAR, cpexpr(lp), realpart(rp) ));
			putassign( imagpart(resp),
				mkexpr(OPSTAR, cpexpr(lp), imagpart(rp) ));
			}
		else if(rtype < TYCOMPLEX)
			{
			if( ISINT(rtype) )
				rp = intdouble(rp, ncommap);
			putassign( realpart(resp),
				mkexpr(OPSTAR, cpexpr(rp), realpart(lp) ));
			putassign( imagpart(resp),
				mkexpr(OPSTAR, cpexpr(rp), imagpart(lp) ));
			}
		else	{
			putassign( realpart(resp), mkexpr(OPMINUS,
				mkexpr(OPSTAR, realpart(lp), realpart(rp)),
				mkexpr(OPSTAR, imagpart(lp), imagpart(rp)) ));
			putassign( imagpart(resp), mkexpr(OPPLUS,
				mkexpr(OPSTAR, realpart(lp), imagpart(rp)),
				mkexpr(OPSTAR, imagpart(lp), realpart(rp)) ));
			}
		*ncommap += 2;
		break;

	case OPSLASH:
		/* fixexpr has already replaced all divisions
		 * by a complex by a function call
		 */
		if( ISINT(rtype) )
			rp = intdouble(rp, ncommap);
		putassign( realpart(resp),
			mkexpr(OPSLASH, realpart(lp), cpexpr(rp)) );
		putassign( imagpart(resp),
			mkexpr(OPSLASH, imagpart(lp), cpexpr(rp)) );
		*ncommap += 2;
		break;

	case OPCONV:
		putassign( realpart(resp), realpart(lp) );
		if( ISCOMPLEX(lp->vtype) )
			q = imagpart(lp);
		else if(rp != NULL)
			q = (expptr) realpart(rp);
		else
			q = mkrealcon(TYDREAL, 0.0);
		putassign( imagpart(resp), q);
		*ncommap += 2;
		break;

	default:
		badop("putcx1", opcode);
	}

frexpr(lp);
frexpr(rp);
free( (charptr) p );
return(resp);
}




LOCAL putcxcmp(p)
register expptr p;
{
int opcode;
int ncomma;
register Addrp lp, rp;
expptr q;

if(p->tag != TEXPR)
	badtag("putcxcmp", p->tag);

ncomma = 0;
opcode = p->exprblock.opcode;
lp = putcx1(p->exprblock.leftp, &ncomma);
rp = putcx1(p->exprblock.rightp, &ncomma);

q = mkexpr( opcode==OPEQ ? OPAND : OPOR ,
	mkexpr(opcode, realpart(lp), realpart(rp)),
	mkexpr(opcode, imagpart(lp), imagpart(rp)) );
putx( fixexpr(q) );
putcomma(ncomma, TYINT, NO);

free( (charptr) lp);
free( (charptr) rp);
free( (charptr) p );
}

LOCAL Addrp putch1(p, ncommap)
register expptr p;
int * ncommap;
{
register Addrp t;

switch(p->tag)
	{
	case TCONST:
		return( putconst(p) );

	case TADDR:
		return( (Addrp) p );

	case TEXPR:
		++*ncommap;

		switch(p->exprblock.opcode)
			{
			expptr q;

			case OPCALL:
			case OPCCALL:
				t = putcall(p);
				break;

			case OPPAREN:
				--*ncommap;
				t = putch1(p->exprblock.leftp, ncommap);
				break;

			case OPCONCAT:
				t = mkaltemp(TYCHAR, ICON(lencat(p)) );
				q = (expptr) cpexpr(p->headblock.vleng);
				putcat( cpexpr(t), p );
				/* put the correct length on the block */
				frexpr(t->vleng);
				t->vleng = q;

				break;

			case OPCONV:
				if(!ISICON(p->exprblock.vleng)
				   || p->exprblock.vleng->constblock.constant.ci!=1
				   || ! INT(p->exprblock.leftp->headblock.vtype) )
					fatal("putch1: bad character conversion");
				t = mkaltemp(TYCHAR, ICON(1) );
				putop( mkexpr(OPASSIGN, cpexpr(t), p) );
				break;
			default:
				badop("putch1", p->exprblock.opcode);
			}
		return(t);

	default:
		badtag("putch1", p->tag);
	}
/* NOTREACHED */
}




LOCAL putchop(p)
expptr p;
{
int ncomma;

ncomma = 0;
putaddr( putch1(p, &ncomma) , NO );
putcomma(ncomma, TYCHAR, YES);
}




LOCAL putcheq(p)
register expptr p;
{
int ncomma;
expptr lp, rp;

if(p->tag != TEXPR)
	badtag("putcheq", p->tag);

ncomma = 0;
lp = p->exprblock.leftp;
rp = p->exprblock.rightp;
if( rp->tag==TEXPR && rp->exprblock.opcode==OPCONCAT )
	putcat(lp, rp);
else if( ISONE(lp->headblock.vleng) && ISONE(rp->headblock.vleng) )
	{
	putaddr( putch1(lp, &ncomma) , YES );
	putaddr( putch1(rp, &ncomma) , YES );
	putcomma(ncomma, TYINT, NO);
	p2op(PCC_ASSIGN, PCCT_CHAR);
	}
else
	{
	putx( call2(TYINT, "s_copy", lp, rp) );
	putcomma(ncomma, TYINT, NO);
	}

frexpr(p->exprblock.vleng);
free( (charptr) p );
}




LOCAL putchcmp(p)
register expptr p;
{
int ncomma;
expptr lp, rp;

if(p->tag != TEXPR)
	badtag("putchcmp", p->tag);

ncomma = 0;
lp = p->exprblock.leftp;
rp = p->exprblock.rightp;

if(ISONE(lp->headblock.vleng) && ISONE(rp->headblock.vleng) )
	{
	putaddr( putch1(lp, &ncomma) , YES );
	putcomma(ncomma, TYINT, NO);
	ncomma = 0;
	putaddr( putch1(rp, &ncomma) , YES );
	putcomma(ncomma, TYINT, NO);
	p2op(ops2[p->exprblock.opcode], PCCT_CHAR);
	free( (charptr) p );
	}
else
	{
	p->exprblock.leftp = call2(TYINT,"s_cmp", lp, rp);
	p->exprblock.rightp = ICON(0);
	putop(p);
	}
}





LOCAL putcat(lhs, rhs)
register Addrp lhs;
register expptr rhs;
{
int n, ncomma;
Addrp lp, cp;

ncomma = 0;
n = ncat(rhs);
lp = mkaltmpn(n, TYLENG, PNULL);
cp = mkaltmpn(n, TYADDR, PNULL);

n = 0;
putct1(rhs, lp, cp, &n, &ncomma);

putx( call4(TYSUBR, "s_cat", lhs, cp, lp, mkconv(TYLONG, ICON(n)) ) );
putcomma(ncomma, TYINT, NO);
}





LOCAL putct1(q, lp, cp, ip, ncommap)
register expptr q;
register Addrp lp, cp;
int *ip, *ncommap;
{
int i;
Addrp lp1, cp1;

if(q->tag==TEXPR && q->exprblock.opcode==OPCONCAT)
	{
	putct1(q->exprblock.leftp, lp, cp, ip, ncommap);
	putct1(q->exprblock.rightp, lp, cp , ip, ncommap);
	frexpr(q->exprblock.vleng);
	free( (charptr) q );
	}
else
	{
	i = (*ip)++;
	cp1 = (Addrp) cpexpr(cp);
	cp1->memoffset = mkexpr(OPPLUS, cp1->memoffset, ICON(i*SZADDR));
	lp1 = (Addrp) cpexpr(lp);
	lp1->memoffset = mkexpr(OPPLUS,lp1->memoffset, ICON(i*SZLENG));
	putassign( cp1, addrof(putch1(cpexpr(q),ncommap)) );
	putassign( lp1, q->headblock.vleng );
	free( (charptr) q );
	*ncommap += 2;
	}
}

LOCAL putaddr(p, indir)
register Addrp p;
int indir;
{
int type, type2, funct;
ftnint offset, simoffset();
expptr offp, shorten();

if( p->tag==TERROR || (p->memoffset!=NULL && ISERROR(p->memoffset)) )
	{
	frexpr(p);
	return;
	}
if (p->tag != TADDR) badtag ("putaddr",p->tag);

type = p->vtype;
type2 = types2[type];
funct = (p->vclass==CLPROC ? PCCTM_FTN<<2 : 0);

offp = (p->memoffset ? (expptr) cpexpr(p->memoffset) : (expptr)NULL );


#if (FUDGEOFFSET != 1)
if(offp)
	offp = mkexpr(OPSTAR, ICON(FUDGEOFFSET), offp);
#endif

offset = simoffset( &offp );
#if SZINT < SZLONG
	if(offp)
		if(shortsubs)
			offp = shorten(offp);
		else
			offp = mkconv(TYINT, offp);
#else
	if(offp)
		offp = mkconv(TYINT, offp);
#endif

if (p->vclass == CLVAR
    && (p->vstg == STGBSS || p->vstg == STGEQUIV)
    && SMALLVAR(p->varsize)
    && offset >= -32768 && offset <= 32767)
  {
    anylocals = YES;
    if (indir && !offp)
      p2ldisp(offset, memname(p->vstg, p->memno), type2);
    else
      {
	p2reg(11, type2 | PCCTM_PTR);
	p2triple(PCC_ICON, 1, PCCT_INT);
	p2word(offset);
	p2ndisp(memname(p->vstg, p->memno));
	p2op(PCC_PLUS, type2 | PCCTM_PTR);
	if (offp)
	  {
	    putx(offp);
	    p2op(PCC_PLUS, type2 | PCCTM_PTR);
	  }
	if (indir)
	  p2op(PCC_DEREF, type2);
      }
    frexpr((tagptr) p);
    return;
  }

switch(p->vstg)
	{
	case STGAUTO:
		if(indir && !offp)
			{
			p2oreg(offset, AUTOREG, type2);
			break;
			}

		if(!indir && !offp && !offset)
			{
			p2reg(AUTOREG, type2 | PCCTM_PTR);
			break;
			}

		p2reg(AUTOREG, type2 | PCCTM_PTR);
		if(offp)
			{
			putx(offp);
			if(offset)
				p2icon(offset, PCCT_INT);
			}
		else
			p2icon(offset, PCCT_INT);
		if(offp && offset)
			p2op(PCC_PLUS, type2 | PCCTM_PTR);
		p2op(PCC_PLUS, type2 | PCCTM_PTR);
		if(indir)
			p2op(PCC_DEREF, type2);
		break;

	case STGARG:
		p2oreg(
#ifdef ARGOFFSET
			ARGOFFSET +
#endif
			(ftnint) (FUDGEOFFSET*p->memno),
			ARGREG,   type2 | PCCTM_PTR | funct );

	based:
		if(offset)
			{
			p2icon(offset, PCCT_INT);
			p2op(PCC_PLUS, type2 | PCCTM_PTR);
			}
		if(offp)
			{
			putx(offp);
			p2op(PCC_PLUS, type2 | PCCTM_PTR);
			}
		if(indir)
			p2op(PCC_DEREF, type2);
		break;

	case STGLENG:
		if(indir)
			{
			p2oreg(
#ifdef ARGOFFSET
				ARGOFFSET +
#endif
				(ftnint) (FUDGEOFFSET*p->memno),
				ARGREG,   type2 );
			}
		else	{
			p2reg(ARGREG, type2 | PCCTM_PTR );
			p2icon(
#ifdef ARGOFFSET
				ARGOFFSET +
#endif
				(ftnint) (FUDGEOFFSET*p->memno), PCCT_INT);
			p2op(PCC_PLUS, type2 | PCCTM_PTR );
			}
		break;


	case STGBSS:
	case STGINIT:
	case STGEXT:
	case STGINTR:
	case STGCOMMON:
	case STGEQUIV:
	case STGCONST:
		if(offp)
			{
			putx(offp);
			putmem(p, PCC_ICON, offset);
			p2op(PCC_PLUS, type2 | PCCTM_PTR);
			if(indir)
				p2op(PCC_DEREF, type2);
			}
		else
			putmem(p, (indir ? PCC_NAME : PCC_ICON), offset);

		break;

	case STGREG:
		if(indir)
			p2reg(p->memno, type2);
		else
			fatal("attempt to take address of a register");
		break;

	case STGPREG:
		if(indir && !offp)
			p2oreg(offset, p->memno, type2);
		else
			{
			p2reg(p->memno, type2 | PCCTM_PTR);
			goto based;
			}
		break;

	default:
		badstg("putaddr", p->vstg);
	}
frexpr(p);
}




LOCAL putmem(p, class, offset)
expptr p;
int class;
ftnint offset;
{
int type2;
int funct;
char *name,  *memname();

funct = (p->headblock.vclass==CLPROC ? PCCTM_FTN<<2 : 0);
type2 = types2[p->headblock.vtype];
if(p->headblock.vclass == CLPROC)
	type2 |= (PCCTM_FTN<<2);
name = memname(p->addrblock.vstg, p->addrblock.memno);
if(class == PCC_ICON)
	{
	p2triple(PCC_ICON, name[0]!='\0', type2|PCCTM_PTR);
	p2word(offset);
	if(name[0])
		p2name(name);
	}
else
	{
	p2triple(PCC_NAME, offset!=0, type2);
	if(offset != 0)
		p2word(offset);
	p2name(name);
	}
}



LOCAL Addrp putcall(p)
register Exprp p;
{
chainp arglist, charsp, cp;
int n, first;
Addrp t;
register expptr q;
Addrp fval, mkargtemp();
int type, type2, ctype, qtype, indir;

type2 = types2[type = p->vtype];
charsp = NULL;
indir =  (p->opcode == OPCCALL);
n = 0;
first = YES;

if(p->rightp)
	{
	arglist = p->rightp->listblock.listp;
	free( (charptr) (p->rightp) );
	}
else
	arglist = NULL;

for(cp = arglist ; cp ; cp = cp->nextp)
	{
	q = (expptr) cp->datap;
	if(indir)
		++n;
	else	{
		q = (expptr) (cp->datap);
		if( ISCONST(q) )
			{
			q = (expptr) putconst(q);
			cp->datap = (tagptr) q;
			}
		if( ISCHAR(q) && q->headblock.vclass!=CLPROC )
			{
			charsp = hookup(charsp,
					mkchain(cpexpr(q->headblock.vleng),
						CHNULL));
			n += 2;
			}
		else
			n += 1;
		}
	}

if(type == TYCHAR)
	{
	if( ISICON(p->vleng) )
		{
		fval = mkargtemp(TYCHAR, p->vleng);
		n += 2;
		}
	else	{
		err("adjustable character function");
		return;
		}
	}
else if( ISCOMPLEX(type) )
	{
	fval = mkargtemp(type, PNULL);
	n += 1;
	}
else
	fval = NULL;

ctype = (fval ? PCCT_INT : type2);
putaddr(p->leftp, NO);

if(fval)
	{
	first = NO;
	putaddr( cpexpr(fval), NO);
	if(type==TYCHAR)
		{
		putx( mkconv(TYLENG,p->vleng) );
		p2op(PCC_CM, type2);
		}
	}

for(cp = arglist ; cp ; cp = cp->nextp)
	{
	q = (expptr) (cp->datap);
	if(q->tag==TADDR && (indir || q->addrblock.vstg!=STGREG) )
		putaddr(q, indir && q->addrblock.vtype!=TYCHAR);
	else if( ISCOMPLEX(q->headblock.vtype) )
		putcxop(q);
	else if (ISCHAR(q) )
		putchop(q);
	else if( ! ISERROR(q) )
		{
		if(indir)
			putx(q);
		else	{
			t = mkargtemp(qtype = q->headblock.vtype,
				q->headblock.vleng);
			putassign( cpexpr(t), q );
			putaddr(t, NO);
			putcomma(1, qtype, YES);
			}
		}
	if(first)
		first = NO;
	else
		p2op(PCC_CM, type2);
	}

if(arglist)
	frchain(&arglist);
for(cp = charsp ; cp ; cp = cp->nextp)
	{
	putx( mkconv(TYLENG,cp->datap) );
	p2op(PCC_CM, type2);
	}
frchain(&charsp);
p2op(n>0 ? PCC_CALL : PCC_UCALL , ctype);
free( (charptr) p );
return(fval);
}



LOCAL putmnmx(p)
register expptr p;
{
int op, type;
int ncomma;
expptr qp;
chainp p0, p1;
Addrp sp, tp;

if(p->tag != TEXPR)
	badtag("putmnmx", p->tag);

type = p->exprblock.vtype;
op = (p->exprblock.opcode==OPMIN ? OPLT : OPGT );
p0 = p->exprblock.leftp->listblock.listp;
free( (charptr) (p->exprblock.leftp) );
free( (charptr) p );

sp = mkaltemp(type, PNULL);
tp = mkaltemp(type, PNULL);
qp = mkexpr(OPCOLON, cpexpr(tp), cpexpr(sp));
qp = mkexpr(OPQUEST, mkexpr(op, cpexpr(tp),cpexpr(sp)), qp);
qp = fixexpr(qp);

ncomma = 1;
putassign( cpexpr(sp), p0->datap );

for(p1 = p0->nextp ; p1 ; p1 = p1->nextp)
	{
	++ncomma;
	putassign( cpexpr(tp), p1->datap );
	if(p1->nextp)
		{
		++ncomma;
		putassign( cpexpr(sp), cpexpr(qp) );
		}
	else
		putx(qp);
	}

putcomma(ncomma, type, NO);
frexpr(sp);
frexpr(tp);
frchain( &p0 );
}




LOCAL putcomma(n, type, indir)
int n, type, indir;
{
type = types2[type];
if(indir)
	type |= PCCTM_PTR;
while(--n >= 0)
	p2op(PCC_COMOP, type);
}




ftnint simoffset(p0)
expptr *p0;
{
ftnint offset, prod;
register expptr p, lp, rp;

offset = 0;
p = *p0;
if(p == NULL)
	return(0);

if( ! ISINT(p->headblock.vtype) )
	return(0);

if(p->tag==TEXPR && p->exprblock.opcode==OPSTAR)
	{
	lp = p->exprblock.leftp;
	rp = p->exprblock.rightp;
	if(ISICON(rp) && lp->tag==TEXPR &&
	   lp->exprblock.opcode==OPPLUS && ISICON(lp->exprblock.rightp))
		{
		p->exprblock.opcode = OPPLUS;
		lp->exprblock.opcode = OPSTAR;
		prod = rp->constblock.constant.ci *
			lp->exprblock.rightp->constblock.constant.ci;
		lp->exprblock.rightp->constblock.constant.ci = rp->constblock.constant.ci;
		rp->constblock.constant.ci = prod;
		}
	}

if(p->tag==TEXPR && p->exprblock.opcode==OPPLUS &&
    ISICON(p->exprblock.rightp))
	{
	rp = p->exprblock.rightp;
	lp = p->exprblock.leftp;
	offset += rp->constblock.constant.ci;
	frexpr(rp);
	free( (charptr) p );
	*p0 = lp;
	}

if( ISCONST(p) )
	{
	offset += p->constblock.constant.ci;
	frexpr(p);
	*p0 = NULL;
	}

return(offset);
}





p2op(op, type)
int op, type;
{
p2triple(op, 0, type);
}

p2icon(offset, type)
ftnint offset;
int type;
{
p2triple(PCC_ICON, 0, type);
p2word(offset);
}




p2oreg(offset, reg, type)
ftnint offset;
int reg, type;
{
p2triple(PCC_OREG, reg, type);
p2word(offset);
p2name("");
}




p2reg(reg, type)
int reg, type;
{
p2triple(PCC_REG, reg, type);
}



p2pi(s, i)
char *s;
int i;
{
char buff[100];
sprintf(buff, s, i);
p2pass(buff);
}



p2pij(s, i, j)
char *s;
int i, j;
{
char buff[100];
sprintf(buff, s, i, j);
p2pass(buff);
}




p2ps(s, t)
char *s, *t;
{
char buff[100];
sprintf(buff, s, t);
p2pass(buff);
}




p2pass(s)
char *s;
{
p2triple(PCCF_FTEXT, (strlen(s) + FOUR-1)/FOUR, 0);
p2str(s);
}




p2str(s)
register char *s;
{
union { long int word; char str[FOUR]; } u;
register int i;

i = 0;
u.word = 0;
while(*s)
	{
	u.str[i++] = *s++;
	if(i == FOUR)
		{
		p2word(u.word);
		u.word = 0;
		i = 0;
		}
	}
if(i > 0)
	p2word(u.word);
}




p2triple(op, var, type)
int op, var, type;
{
register long word;
word = PCCM_TRIPLE(op, var, type);
p2word(word);
}





p2name(s)
register char *s;
{
register int i;

#ifdef UCBPASS2
	/* arbitrary length names, terminated by a null,
	   padded to a full word */

#	define WL   sizeof(long int)
	union { long int word; char str[WL]; } w;
	
	w.word = 0;
	i = 0;
	while(w.str[i++] = *s++)
		if(i == WL)
			{
			p2word(w.word);
			w.word = 0;
			i = 0;
			}
	if(i > 0)
		p2word(w.word);
#else
	/* standard intermediate, names are 8 characters long */

	union  { long int word[2];  char str[8]; } u;
	
	u.word[0] = u.word[1] = 0;
	for(i = 0 ; i<8 && *s ; ++i)
		u.str[i] = *s++;
	p2word(u.word[0]);
	p2word(u.word[1]);

#endif

}




p2word(w)
long int w;
{
*p2bufp++ = w;
if(p2bufp >= p2bufend)
	p2flush();
}



p2flush()
{
if(p2bufp > p2buff)
	write(fileno(textfile), p2buff, (p2bufp-p2buff)*sizeof(long int));
p2bufp = p2buff;
}



LOCAL
p2ldisp(offset, vname, type)
ftnint offset;
char *vname;
int type;
{
  char buff[100];

  sprintf(buff, "%s-v.%d", vname, bsslabel);
  p2triple(PCC_OREG, 11, type);
  p2word(offset);
  p2name(buff);
}



p2ndisp(vname)
char *vname;
{
  char buff[100];

  sprintf(buff, "%s-v.%d", vname, bsslabel);
  p2name(buff);
}
