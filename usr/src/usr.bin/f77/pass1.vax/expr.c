/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char *sccsid[] = "@(#)expr.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * expr.c
 *
 * Routines for handling expressions, f77 compiler pass 1.
 *
 * University of Utah CS Dept modification history:
 *
 * Revision 3.13  85/05/13  16:40:37  mckusick
 * fix to copy character values into numerics (from donn@utah-cs)
 * 
 * Revision 3.12  85/03/18  08:07:47  donn
 * Fixes to help out with short integers -- if integers are by default short,
 * then so are constants; and if addresses can't be stored in shorts, complain.
 * 
 * Revision 3.11  85/03/16  22:31:27  donn
 * Added hack to mkconv() to allow character values of length > 1 to be
 * converted to numeric types, for Helge Skrivervik.  Note that this does
 * not affect use of the intrinsic ichar() conversion.
 * 
 * Revision 3.10  85/01/15  21:06:47  donn
 * Changed mkconv() to comment on implicit conversions; added intrconv() for
 * use with explicit conversions by intrinsic functions.
 * 
 * Revision 3.9  85/01/11  21:05:49  donn
 * Added changes to implement SAVE statements.
 * 
 * Revision 3.8  84/12/17  02:21:06  donn
 * Added a test to prevent constant folding from being done on expressions
 * whose type is not known at that point in mkexpr().
 * 
 * Revision 3.7  84/12/11  21:14:17  donn
 * Removed obnoxious 'excess precision' warning.
 * 
 * Revision 3.6  84/11/23  01:00:36  donn
 * Added code to trim excess precision from single-precision constants, and
 * to warn the user when this occurs.
 * 
 * Revision 3.5  84/11/23  00:10:39  donn
 * Changed stfcall() to remark on argument type clashes in 'calls' to
 * statement functions.
 * 
 * Revision 3.4  84/11/22  21:21:17  donn
 * Fixed bug in fix to mkexpr() that caused IMPLICIT to affect intrinsics.
 * 
 * Revision 3.3  84/11/12  18:26:14  donn
 * Shuffled some code around so that the compiler remembers to free some vleng
 * structures which used to just sit around.
 * 
 * Revision 3.2  84/10/16  19:24:15  donn
 * Fix for Peter Montgomery's bug with -C and invalid subscripts -- prevent
 * core dumps by replacing bad subscripts with good ones.
 * 
 * Revision 3.1  84/10/13  01:31:32  donn
 * Merged Jerry Berkman's version into mine.
 * 
 * Revision 2.7  84/09/27  15:42:52  donn
 * The last fix for multiplying undeclared variables by 0 isn't sufficient,
 * since the type of the 0 may not be the (implicit) type of the variable.
 * I added a hack to check the implicit type of implicitly declared
 * variables...
 * 
 * Revision 2.6  84/09/14  19:34:03  donn
 * Problem noted by Mike Vevea -- mkexpr will sometimes attempt to convert
 * 0 to type UNKNOWN, which is illegal.  Fix is to use native type instead.
 * Not sure how correct (or important) this is...
 * 
 * Revision 2.5  84/08/05  23:05:27  donn
 * Added fixes to prevent fixexpr() from slicing and dicing complex conversions
 * with two operands.
 * 
 * Revision 2.4  84/08/05  17:34:48  donn
 * Added an optimization to mklhs() to detect substrings of the form ch(i:i)
 * and assign constant length 1 to them.
 * 
 * Revision 2.3  84/07/19  19:38:33  donn
 * Added a typecast to the last fix.  Somehow I missed it the first time...
 * 
 * Revision 2.2  84/07/19  17:19:57  donn
 * Caused OPPAREN expressions to inherit the length of their operands, so
 * that parenthesized character expressions work correctly.
 * 
 * Revision 2.1  84/07/19  12:03:02  donn
 * Changed comment headers for UofU.
 * 
 * Revision 1.2  84/04/06  20:12:17  donn
 * Fixed bug which caused programs with mixed-type multiplications involving
 * the constant 0 to choke the compiler.
 * 
 */

#include "defs.h"


/* little routines to create constant blocks */

Constp mkconst(t)
register int t;
{
register Constp p;

p = ALLOC(Constblock);
p->tag = TCONST;
p->vtype = t;
return(p);
}


expptr mklogcon(l)
register int l;
{
register Constp  p;

p = mkconst(TYLOGICAL);
p->const.ci = l;
return( (expptr) p );
}



expptr mkintcon(l)
ftnint l;
{
register Constp p;
int usetype;

if(tyint == TYSHORT)
  {
    short s = l;
    if(l != s)
      usetype = TYLONG;
    else
      usetype = TYSHORT;
  }
else
  usetype = tyint;
p = mkconst(usetype);
p->const.ci = l;
return( (expptr) p );
}



expptr mkaddcon(l)
register int l;
{
register Constp p;

p = mkconst(TYADDR);
p->const.ci = l;
return( (expptr) p );
}



expptr mkrealcon(t, d)
register int t;
double d;
{
register Constp p;

if(t == TYREAL)
  {
    float f = d;
    if(f != d)
      {
#ifdef notdef
	warn("excess precision in real constant lost");
#endif notdef
	d = f;
      }
  }
p = mkconst(t);
p->const.cd[0] = d;
return( (expptr) p );
}


expptr mkbitcon(shift, leng, s)
int shift;
register int leng;
register char *s;
{
  Constp p;
  register int i, j, k;
  register char *bp;
  int size;

  size = (shift*leng + BYTESIZE -1)/BYTESIZE;
  bp = (char *) ckalloc(size);

  i = 0;

#if (TARGET == PDP11 || TARGET == VAX)
  j = 0;
#else
  j = size;
#endif

  k = 0;

  while (leng > 0)
    {
      k |= (hextoi(s[--leng]) << i);
      i += shift;
      if (i >= BYTESIZE)
	{
#if (TARGET == PDP11 || TARGET == VAX)
	  bp[j++] = k & MAXBYTE;
#else
	  bp[--j] = k & MAXBYTE;
#endif
	  k = k >> BYTESIZE;
	  i -= BYTESIZE;
	}
    }

  if (k != 0)
#if (TARGET == PDP11 || TARGET == VAX)
    bp[j++] = k;
#else
    bp[--j] = k;
#endif

  p = mkconst(TYBITSTR);
  p->vleng = ICON(size);
  p->const.ccp = bp;

  return ((expptr) p);
}



expptr mkstrcon(l,v)
int l;
register char *v;
{
register Constp p;
register char *s;

p = mkconst(TYCHAR);
p->vleng = ICON(l);
p->const.ccp = s = (char *) ckalloc(l);
while(--l >= 0)
	*s++ = *v++;
return( (expptr) p );
}


expptr mkcxcon(realp,imagp)
register expptr realp, imagp;
{
int rtype, itype;
register Constp p;

rtype = realp->headblock.vtype;
itype = imagp->headblock.vtype;

if( ISCONST(realp) && ISNUMERIC(rtype) && ISCONST(imagp) && ISNUMERIC(itype) )
	{
	p = mkconst( (rtype==TYDREAL||itype==TYDREAL) ? TYDCOMPLEX : TYCOMPLEX);
	if( ISINT(rtype) )
		p->const.cd[0] = realp->constblock.const.ci;
	else	p->const.cd[0] = realp->constblock.const.cd[0];
	if( ISINT(itype) )
		p->const.cd[1] = imagp->constblock.const.ci;
	else	p->const.cd[1] = imagp->constblock.const.cd[0];
	}
else
	{
	err("invalid complex constant");
	p = (Constp) errnode();
	}

frexpr(realp);
frexpr(imagp);
return( (expptr) p );
}


expptr errnode()
{
struct Errorblock *p;
p = ALLOC(Errorblock);
p->tag = TERROR;
p->vtype = TYERROR;
return( (expptr) p );
}





expptr mkconv(t, p)
register int t;
register expptr p;
{
register expptr q;
Addrp r, s;
register int pt;
expptr opconv();

if(t==TYUNKNOWN || t==TYERROR)
	badtype("mkconv", t);
pt = p->headblock.vtype;
if(t == pt)
	return(p);

if( pt == TYCHAR && ISNUMERIC(t) )
	{
	warn("implicit conversion of character to numeric type");

	/*
	 * Ugly kluge to copy character values into numerics.
	 */
	s = mkaltemp(t, ENULL);
	r = (Addrp) cpexpr(s);
	r->vtype = TYCHAR;
	r->varleng = typesize[t];
	r->vleng = mkintcon(r->varleng);
	q = mkexpr(OPASSIGN, r, p);
	q = mkexpr(OPCOMMA, q, s);
	return(q);
	}

#if SZADDR > SZSHORT
if( pt == TYADDR && t == TYSHORT)
	{
	err("insufficient precision to hold address type");
	return( errnode() );
	}
#endif
if( pt == TYADDR && ISNUMERIC(t) )
	warn("implicit conversion of address to numeric type");

if( ISCONST(p) && pt!=TYADDR)
	{
	q = (expptr) mkconst(t);
	consconv(t, &(q->constblock.const),
		p->constblock.vtype, &(p->constblock.const) );
	frexpr(p);
	}
#if TARGET == PDP11
else if(ISINT(t) && pt==TYCHAR)
	{
	q = mkexpr(OPBITAND, opconv(p,TYSHORT), ICON(255));
	if(t == TYLONG)
		q = opconv(q, TYLONG);
	}
#endif
else
	q = opconv(p, t);

if(t == TYCHAR)
	q->constblock.vleng = ICON(1);
return(q);
}



/* intrinsic conversions */
expptr intrconv(t, p)
register int t;
register expptr p;
{
register expptr q;
register int pt;
expptr opconv();

if(t==TYUNKNOWN || t==TYERROR)
	badtype("intrconv", t);
pt = p->headblock.vtype;
if(t == pt)
	return(p);

else if( ISCONST(p) && pt!=TYADDR)
	{
	q = (expptr) mkconst(t);
	consconv(t, &(q->constblock.const),
		p->constblock.vtype, &(p->constblock.const) );
	frexpr(p);
	}
#if TARGET == PDP11
else if(ISINT(t) && pt==TYCHAR)
	{
	q = mkexpr(OPBITAND, opconv(p,TYSHORT), ICON(255));
	if(t == TYLONG)
		q = opconv(q, TYLONG);
	}
#endif
else
	q = opconv(p, t);

if(t == TYCHAR)
	q->constblock.vleng = ICON(1);
return(q);
}



expptr opconv(p, t)
expptr p;
int t;
{
register expptr q;

q = mkexpr(OPCONV, p, PNULL);
q->headblock.vtype = t;
return(q);
}



expptr addrof(p)
expptr p;
{
return( mkexpr(OPADDR, p, PNULL) );
}



tagptr cpexpr(p)
register tagptr p;
{
register tagptr e;
int tag;
register chainp ep, pp;
tagptr cpblock();

static int blksize[ ] =
	{	0,
		sizeof(struct Nameblock),
		sizeof(struct Constblock),
		sizeof(struct Exprblock),
		sizeof(struct Addrblock),
		sizeof(struct Tempblock),
		sizeof(struct Primblock),
		sizeof(struct Listblock),
		sizeof(struct Errorblock)
	};

if(p == NULL)
	return(NULL);

if( (tag = p->tag) == TNAME)
	return(p);

e = cpblock( blksize[p->tag] , p);

switch(tag)
	{
	case TCONST:
		if(e->constblock.vtype == TYCHAR)
			{
			e->constblock.const.ccp =
				copyn(1+strlen(e->constblock.const.ccp),
					e->constblock.const.ccp);
			e->constblock.vleng =
				(expptr) cpexpr(e->constblock.vleng);
			}
	case TERROR:
		break;

	case TEXPR:
		e->exprblock.leftp =  (expptr) cpexpr(p->exprblock.leftp);
		e->exprblock.rightp = (expptr) cpexpr(p->exprblock.rightp);
		break;

	case TLIST:
		if(pp = p->listblock.listp)
			{
			ep = e->listblock.listp =
				mkchain( cpexpr(pp->datap), CHNULL);
			for(pp = pp->nextp ; pp ; pp = pp->nextp)
				ep = ep->nextp =
					mkchain( cpexpr(pp->datap), CHNULL);
			}
		break;

	case TADDR:
		e->addrblock.vleng = (expptr)  cpexpr(e->addrblock.vleng);
		e->addrblock.memoffset = (expptr)cpexpr(e->addrblock.memoffset);
		e->addrblock.istemp = NO;
		break;

	case TTEMP:
		e->tempblock.vleng = (expptr)  cpexpr(e->tempblock.vleng);
		e->tempblock.istemp = NO;
		break;

	case TPRIM:
		e->primblock.argsp = (struct Listblock *)
					cpexpr(e->primblock.argsp);
		e->primblock.fcharp = (expptr) cpexpr(e->primblock.fcharp);
		e->primblock.lcharp = (expptr) cpexpr(e->primblock.lcharp);
		break;

	default:
		badtag("cpexpr", tag);
	}

return(e);
}

frexpr(p)
register tagptr p;
{
register chainp q;

if(p == NULL)
	return;

switch(p->tag)
	{
	case TCONST:
		switch (p->constblock.vtype)
			{
			case TYBITSTR:
			case TYCHAR:
			case TYHOLLERITH:
				free( (charptr) (p->constblock.const.ccp) );
				frexpr(p->constblock.vleng);
			}
		break;

	case TADDR:
		if (!optimflag && p->addrblock.istemp)
			{
			frtemp(p);
			return;
			}
		frexpr(p->addrblock.vleng);
		frexpr(p->addrblock.memoffset);
		break;

	case TTEMP:
		frexpr(p->tempblock.vleng);
		break;

	case TERROR:
		break;

	case TNAME:
		return;

	case TPRIM:
		frexpr(p->primblock.argsp);
		frexpr(p->primblock.fcharp);
		frexpr(p->primblock.lcharp);
		break;

	case TEXPR:
		frexpr(p->exprblock.leftp);
		if(p->exprblock.rightp)
			frexpr(p->exprblock.rightp);
		break;

	case TLIST:
		for(q = p->listblock.listp ; q ; q = q->nextp)
			frexpr(q->datap);
		frchain( &(p->listblock.listp) );
		break;

	default:
		badtag("frexpr", p->tag);
	}

free( (charptr) p );
}

/* fix up types in expression; replace subtrees and convert
   names to address blocks */

expptr fixtype(p)
register tagptr p;
{

if(p == 0)
	return(0);

switch(p->tag)
	{
	case TCONST:
		return( (expptr) p );

	case TADDR:
		p->addrblock.memoffset = fixtype(p->addrblock.memoffset);
		return( (expptr) p);

	case TTEMP:
		return( (expptr) p);

	case TERROR:
		return( (expptr) p);

	default:
		badtag("fixtype", p->tag);

	case TEXPR:
		return( fixexpr(p) );

	case TLIST:
		return( (expptr) p );

	case TPRIM:
		if(p->primblock.argsp && p->primblock.namep->vclass!=CLVAR)
			{
			if(p->primblock.namep->vtype == TYSUBR)
				{
				err("function invocation of subroutine");
				return( errnode() );
				}
			else
				return( mkfunct(p) );
			}
		else	return( mklhs(p) );
	}
}





/* special case tree transformations and cleanups of expression trees */

expptr fixexpr(p)
register Exprp p;
{
expptr lp;
register expptr rp;
register expptr q;
int opcode, ltype, rtype, ptype, mtype;
expptr lconst, rconst;
expptr mkpower();

if( ISERROR(p) )
	return( (expptr) p );
else if(p->tag != TEXPR)
	badtag("fixexpr", p->tag);
opcode = p->opcode;
if (ISCONST(p->leftp))
	lconst = (expptr) cpexpr(p->leftp);
else
	lconst = NULL;
if (p->rightp && ISCONST(p->rightp))
	rconst = (expptr) cpexpr(p->rightp);
else
	rconst = NULL;
lp = p->leftp = fixtype(p->leftp);
ltype = lp->headblock.vtype;
if(opcode==OPASSIGN && lp->tag!=TADDR && lp->tag!=TTEMP)
	{
	err("left side of assignment must be variable");
	frexpr(p);
	return( errnode() );
	}

if(p->rightp)
	{
	rp = p->rightp = fixtype(p->rightp);
	rtype = rp->headblock.vtype;
	}
else
	{
	rp = NULL;
	rtype = 0;
	}

if(ltype==TYERROR || rtype==TYERROR)
	{
	frexpr(p);
	frexpr(lconst);
	frexpr(rconst);
	return( errnode() );
	}

/* force folding if possible */
if( ISCONST(lp) && (rp==NULL || ISCONST(rp)) )
	{
	q = mkexpr(opcode, lp, rp);
	if( ISCONST(q) )
		{
		frexpr(lconst);
		frexpr(rconst);
		return(q);
		}
	free( (charptr) q );	/* constants did not fold */
	}

if( (ptype = cktype(opcode, ltype, rtype)) == TYERROR)
	{
	frexpr(p);
	frexpr(lconst);
	frexpr(rconst);
	return( errnode() );
	}

switch(opcode)
	{
	case OPCONCAT:
		if(p->vleng == NULL)
			p->vleng = mkexpr(OPPLUS,
				cpexpr(lp->headblock.vleng),
				cpexpr(rp->headblock.vleng) );
		break;

	case OPASSIGN:
	case OPPLUSEQ:
	case OPSTAREQ:
		if(ltype == rtype)
			break;
		if( ! rconst && ISREAL(ltype) && ISREAL(rtype) )
			break;
		if( ISCOMPLEX(ltype) || ISCOMPLEX(rtype) )
			break;
		if( ONEOF(ltype, MSKADDR|MSKINT) && ONEOF(rtype, MSKADDR|MSKINT)
#if FAMILY==PCC
		    && typesize[ltype]>=typesize[rtype] )
#else
		    && typesize[ltype]==typesize[rtype] )
#endif
			break;
		if (rconst)
			{
			p->rightp = fixtype( mkconv(ptype, cpexpr(rconst)) );
			frexpr(rp);
			}
		else
			p->rightp = fixtype(mkconv(ptype, rp));
		break;

	case OPSLASH:
		if( ISCOMPLEX(rtype) )
			{
			p = (Exprp) call2(ptype,
				ptype==TYCOMPLEX? "c_div" : "z_div",
				mkconv(ptype, lp), mkconv(ptype, rp) );
			break;
			}
	case OPPLUS:
	case OPMINUS:
	case OPSTAR:
	case OPMOD:
		if(ptype==TYDREAL && ( (ltype==TYREAL && ! lconst ) ||
		    (rtype==TYREAL && ! rconst ) ))
			break;
		if( ISCOMPLEX(ptype) )
			break;
		if(ltype != ptype)
			if (lconst)
				{
				p->leftp = fixtype(mkconv(ptype,
						cpexpr(lconst)));
				frexpr(lp);
				}
			else
				p->leftp = fixtype(mkconv(ptype,lp));
		if(rtype != ptype)
			if (rconst)
				{
				p->rightp = fixtype(mkconv(ptype,
						cpexpr(rconst)));
				frexpr(rp);
				}
			else
				p->rightp = fixtype(mkconv(ptype,rp));
		break;

	case OPPOWER:
		return( mkpower(p) );

	case OPLT:
	case OPLE:
	case OPGT:
	case OPGE:
	case OPEQ:
	case OPNE:
		if(ltype == rtype)
			break;
		mtype = cktype(OPMINUS, ltype, rtype);
		if(mtype==TYDREAL && ( (ltype==TYREAL && ! lconst) ||
		    (rtype==TYREAL && ! rconst) ))
			break;
		if( ISCOMPLEX(mtype) )
			break;
		if(ltype != mtype)
			if (lconst)
				{
				p->leftp = fixtype(mkconv(mtype,
						cpexpr(lconst)));
				frexpr(lp);
				}
			else
				p->leftp = fixtype(mkconv(mtype,lp));
		if(rtype != mtype)
			if (rconst)
				{
				p->rightp = fixtype(mkconv(mtype,
						cpexpr(rconst)));
				frexpr(rp);
				}
			else
				p->rightp = fixtype(mkconv(mtype,rp));
		break;


	case OPCONV:
		if(ISCOMPLEX(p->vtype))
			{
			ptype = cktype(OPCONV, p->vtype, ltype);
			if(p->rightp)
				ptype = cktype(OPCONV, ptype, rtype);
			break;
			}
		ptype = cktype(OPCONV, p->vtype, ltype);
		if(lp->tag==TEXPR && lp->exprblock.opcode==OPCOMMA)
			{
			lp->exprblock.rightp =
				fixtype( mkconv(ptype, lp->exprblock.rightp) );
			free( (charptr) p );
			p = (Exprp) lp;
			}
		break;

	case OPADDR:
		if(lp->tag==TEXPR && lp->exprblock.opcode==OPADDR)
			fatal("addr of addr");
		break;

	case OPCOMMA:
	case OPQUEST:
	case OPCOLON:
		break;

	case OPPAREN:
		p->vleng = (expptr) cpexpr( lp->headblock.vleng );
		break;

	case OPMIN:
	case OPMAX:
		ptype = p->vtype;
		break;

	default:
		break;
	}

p->vtype = ptype;
frexpr(lconst);
frexpr(rconst);
return((expptr) p);
}

#if SZINT < SZLONG
/*
   for efficient subscripting, replace long ints by shorts
   in easy places
*/

expptr shorten(p)
register expptr p;
{
register expptr q;

if(p->headblock.vtype != TYLONG)
	return(p);

switch(p->tag)
	{
	case TERROR:
	case TLIST:
		return(p);

	case TCONST:
	case TADDR:
		return( mkconv(TYINT,p) );

	case TEXPR:
		break;

	default:
		badtag("shorten", p->tag);
	}

switch(p->exprblock.opcode)
	{
	case OPPLUS:
	case OPMINUS:
	case OPSTAR:
		q = shorten( cpexpr(p->exprblock.rightp) );
		if(q->headblock.vtype == TYINT)
			{
			p->exprblock.leftp = shorten(p->exprblock.leftp);
			if(p->exprblock.leftp->headblock.vtype == TYLONG)
				frexpr(q);
			else
				{
				frexpr(p->exprblock.rightp);
				p->exprblock.rightp = q;
				p->exprblock.vtype = TYINT;
				}
			}
		break;

	case OPNEG:
	case OPPAREN:
		p->exprblock.leftp = shorten(p->exprblock.leftp);
		if(p->exprblock.leftp->headblock.vtype == TYINT)
			p->exprblock.vtype = TYINT;
		break;

	case OPCALL:
	case OPCCALL:
		p = mkconv(TYINT,p);
		break;
	default:
		break;
	}

return(p);
}
#endif

/* fix an argument list, taking due care for special first level cases */

fixargs(doput, p0)
int doput;	/* doput is true if the function is not intrinsic;
		   was used to decide whether to do a putconst,
		   but this is no longer done here (Feb82)*/
struct Listblock *p0;
{
register chainp p;
register tagptr q, t;
register int qtag;
int nargs;
Addrp mkscalar();

nargs = 0;
if(p0)
    for(p = p0->listp ; p ; p = p->nextp)
	{
	++nargs;
	q = p->datap;
	qtag = q->tag;
	if(qtag == TCONST)
		{
		if(q->constblock.vtype == TYSHORT)
			q = (tagptr) mkconv(tyint, q);
		p->datap = q ;
		}
	else if(qtag==TPRIM && q->primblock.argsp==0 &&
		q->primblock.namep->vclass==CLPROC)
			p->datap = (tagptr) mkaddr(q->primblock.namep);
	else if(qtag==TPRIM && q->primblock.argsp==0 &&
		q->primblock.namep->vdim!=NULL)
			p->datap = (tagptr) mkscalar(q->primblock.namep);
	else if(qtag==TPRIM && q->primblock.argsp==0 &&
		q->primblock.namep->vdovar && 
		(t = (tagptr) memversion(q->primblock.namep)) )
			p->datap = (tagptr) fixtype(t);
	else
		p->datap = (tagptr) fixtype(q);
	}
return(nargs);
}


Addrp mkscalar(np)
register Namep np;
{
register Addrp ap;

vardcl(np);
ap = mkaddr(np);

#if TARGET == VAX
	/* on the VAX, prolog causes array arguments
	   to point at the (0,...,0) element, except when
	   subscript checking is on
	*/
#ifdef SDB
	if( !checksubs && !sdbflag && np->vstg==STGARG)
#else
	if( !checksubs && np->vstg==STGARG)
#endif
		{
		register struct Dimblock *dp;
		dp = np->vdim;
		frexpr(ap->memoffset);
		ap->memoffset = mkexpr(OPSTAR,
				(np->vtype==TYCHAR ?
					cpexpr(np->vleng) :
					(tagptr)ICON(typesize[np->vtype]) ),
				cpexpr(dp->baseoffset) );
		}
#endif
return(ap);
}





expptr mkfunct(p)
register struct Primblock *p;
{
struct Entrypoint *ep;
Addrp ap;
struct Extsym *extp;
register Namep np;
register expptr q;
expptr intrcall(), stfcall();
int k, nargs;
int class;

if(p->tag != TPRIM)
	return( errnode() );

np = p->namep;
class = np->vclass;

if(class == CLUNKNOWN)
	{
	np->vclass = class = CLPROC;
	if(np->vstg == STGUNKNOWN)
		{
		if(np->vtype!=TYSUBR && (k = intrfunct(np->varname)) )
			{
			np->vstg = STGINTR;
			np->vardesc.varno = k;
			np->vprocclass = PINTRINSIC;
			}
		else
			{
			extp = mkext( varunder(VL,np->varname) );
			extp->extstg = STGEXT;
			np->vstg = STGEXT;
			np->vardesc.varno = extp - extsymtab;
			np->vprocclass = PEXTERNAL;
			}
		}
	else if(np->vstg==STGARG)
		{
		if(np->vtype!=TYCHAR && !ftn66flag)
		    warn("Dummy procedure not declared EXTERNAL. Code may be wrong.");
		np->vprocclass = PEXTERNAL;
		}
	}

if(class != CLPROC)
	fatali("invalid class code %d for function", class);
if(p->fcharp || p->lcharp)
	{
	err("no substring of function call");
	goto error;
	}
impldcl(np);
nargs = fixargs( np->vprocclass!=PINTRINSIC,  p->argsp);

switch(np->vprocclass)
	{
	case PEXTERNAL:
		ap = mkaddr(np);
	call:
		q = mkexpr(OPCALL, ap, p->argsp);
		if( (q->exprblock.vtype = np->vtype) == TYUNKNOWN)
			{
			err("attempt to use untyped function");
			goto error;
			}
		if(np->vleng)
			q->exprblock.vleng = (expptr) cpexpr(np->vleng);
		break;

	case PINTRINSIC:
		q = intrcall(np, p->argsp, nargs);
		break;

	case PSTFUNCT:
		q = stfcall(np, p->argsp);
		break;

	case PTHISPROC:
		warn("recursive call");
		for(ep = entries ; ep ; ep = ep->entnextp)
			if(ep->enamep == np)
				break;
		if(ep == NULL)
			fatal("mkfunct: impossible recursion");
		ap = builtin(np->vtype, varstr(XL, ep->entryname->extname) );
		goto call;

	default:
		fatali("mkfunct: impossible vprocclass %d",
			(int) (np->vprocclass) );
	}
free( (charptr) p );
return(q);

error:
	frexpr(p);
	return( errnode() );
}



LOCAL expptr stfcall(np, actlist)
Namep np;
struct Listblock *actlist;
{
register chainp actuals;
int nargs;
chainp oactp, formals;
int type;
expptr q, rhs, ap;
Namep tnp;
register struct Rplblock *rp;
struct Rplblock *tlist;

if(actlist)
	{
	actuals = actlist->listp;
	free( (charptr) actlist);
	}
else
	actuals = NULL;
oactp = actuals;

nargs = 0;
tlist = NULL;
if( (type = np->vtype) == TYUNKNOWN)
	{
	err("attempt to use untyped statement function");
	q = errnode();
	goto ret;
	}
formals = (chainp) (np->varxptr.vstfdesc->datap);
rhs = (expptr) (np->varxptr.vstfdesc->nextp);

/* copy actual arguments into temporaries */
while(actuals!=NULL && formals!=NULL)
	{
	rp = ALLOC(Rplblock);
	rp->rplnp = tnp = (Namep) (formals->datap);
	ap = fixtype(actuals->datap);
	if(tnp->vtype==ap->headblock.vtype && tnp->vtype!=TYCHAR
	   && (ap->tag==TCONST || ap->tag==TADDR || ap->tag==TTEMP) )
		{
		rp->rplvp = (expptr) ap;
		rp->rplxp = NULL;
		rp->rpltag = ap->tag;
		}
	else	{
		rp->rplvp = (expptr) mktemp(tnp->vtype, tnp->vleng);
		rp->rplxp = fixtype( mkexpr(OPASSIGN, cpexpr(rp->rplvp), ap) );
		if( (rp->rpltag = rp->rplxp->tag) == TERROR)
			err("disagreement of argument types in statement function call");
		else if(tnp->vtype!=ap->headblock.vtype)
			warn("argument type mismatch in statement function");
		}
	rp->rplnextp = tlist;
	tlist = rp;
	actuals = actuals->nextp;
	formals = formals->nextp;
	++nargs;
	}

if(actuals!=NULL || formals!=NULL)
	err("statement function definition and argument list differ");

/*
   now push down names involved in formal argument list, then
   evaluate rhs of statement function definition in this environment
*/

if(tlist)	/* put tlist in front of the rpllist */
	{
	for(rp = tlist; rp->rplnextp; rp = rp->rplnextp)
		;
	rp->rplnextp = rpllist;
	rpllist = tlist;
	}

q = (expptr) mkconv(type, fixtype(cpexpr(rhs)) );

/* now generate the tree ( t1=a1, (t2=a2,... , f))))) */
while(--nargs >= 0)
	{
	if(rpllist->rplxp)
		q = mkexpr(OPCOMMA, rpllist->rplxp, q);
	rp = rpllist->rplnextp;
	frexpr(rpllist->rplvp);
	free(rpllist);
	rpllist = rp;
	}

ret:
	frchain( &oactp );
	return(q);
}




Addrp mkplace(np)
register Namep np;
{
register Addrp s;
register struct Rplblock *rp;
int regn;

/* is name on the replace list? */

for(rp = rpllist ; rp ; rp = rp->rplnextp)
	{
	if(np == rp->rplnp)
		{
		if(rp->rpltag == TNAME)
			{
			np = (Namep) (rp->rplvp);
			break;
			}
		else	return( (Addrp) cpexpr(rp->rplvp) );
		}
	}

/* is variable a DO index in a register ? */

if(np->vdovar && ( (regn = inregister(np)) >= 0) )
	if(np->vtype == TYERROR)
		return( (Addrp) errnode() );
	else
		{
		s = ALLOC(Addrblock);
		s->tag = TADDR;
		s->vstg = STGREG;
		s->vtype = TYIREG;
		s->issaved = np->vsave;
		s->memno = regn;
		s->memoffset = ICON(0);
		return(s);
		}

vardcl(np);
return(mkaddr(np));
}




expptr mklhs(p)
register struct Primblock *p;
{
expptr suboffset();
register Addrp s;
Namep np;

if(p->tag != TPRIM)
	return( (expptr) p );
np = p->namep;

s = mkplace(np);
if(s->tag!=TADDR || s->vstg==STGREG)
	{
	free( (charptr) p );
	return( (expptr) s );
	}

/* compute the address modified by subscripts */

s->memoffset = mkexpr(OPPLUS, s->memoffset, suboffset(p) );
frexpr(p->argsp);
p->argsp = NULL;

/* now do substring part */

if(p->fcharp || p->lcharp)
	{
	if(np->vtype != TYCHAR)
		errstr("substring of noncharacter %s", varstr(VL,np->varname));
	else	{
		if(p->lcharp == NULL)
			p->lcharp = (expptr) cpexpr(s->vleng);
		frexpr(s->vleng);
		if(p->fcharp)
			{
			if(p->fcharp->tag == TPRIM && p->lcharp->tag == TPRIM
			&& p->fcharp->primblock.namep == p->lcharp->primblock.namep)
				/* A trivial optimization -- upper == lower */
				s->vleng = ICON(1);
			else
				s->vleng = mkexpr(OPMINUS, p->lcharp,
					mkexpr(OPMINUS, p->fcharp, ICON(1) ));
			}
		else
			s->vleng = p->lcharp;
		}
	}

s->vleng = fixtype( s->vleng );
s->memoffset = fixtype( s->memoffset );
free( (charptr) p );
return( (expptr) s );
}





deregister(np)
Namep np;
{
if(nregvar>0 && regnamep[nregvar-1]==np)
	{
	--nregvar;
#if FAMILY == DMR
	putnreg();
#endif
	}
}




Addrp memversion(np)
register Namep np;
{
register Addrp s;

if(np->vdovar==NO || (inregister(np)<0) )
	return(NULL);
np->vdovar = NO;
s = mkplace(np);
np->vdovar = YES;
return(s);
}



inregister(np)
register Namep np;
{
register int i;

for(i = 0 ; i < nregvar ; ++i)
	if(regnamep[i] == np)
		return( regnum[i] );
return(-1);
}




enregister(np)
Namep np;
{
if( inregister(np) >= 0)
	return(YES);
if(nregvar >= maxregvar)
	return(NO);
vardcl(np);
if( ONEOF(np->vtype, MSKIREG) )
	{
	regnamep[nregvar++] = np;
	if(nregvar > highregvar)
		highregvar = nregvar;
#if FAMILY == DMR
	putnreg();
#endif
	return(YES);
	}
else
	return(NO);
}




expptr suboffset(p)
register struct Primblock *p;
{
int n;
expptr size;
expptr oftwo();
chainp cp;
expptr offp, prod;
expptr subcheck();
struct Dimblock *dimp;
expptr sub[MAXDIM+1];
register Namep np;

np = p->namep;
offp = ICON(0);
n = 0;
if(p->argsp)
	for(cp = p->argsp->listp ; cp ; ++n, cp = cp->nextp)
		{
		sub[n] = fixtype(cpexpr(cp->datap));
		if ( ! ISINT(sub[n]->headblock.vtype)) {
			errstr("%s: non-integer subscript expression",
				varstr(VL, np->varname) );
			/* Provide a substitute -- go on to find more errors */
			frexpr(sub[n]);
			sub[n] = ICON(1);
		}
		if(n > maxdim)
			{
			   char str[28+VL];
			   sprintf(str, "%s: more than %d subscripts",
				varstr(VL, np->varname), maxdim );
			   err( str );
			break;
			}
		}

dimp = np->vdim;
if(n>0 && dimp==NULL)
	errstr("%s: subscripts on scalar variable", 
		varstr(VL, np->varname), maxdim );
else if(dimp && dimp->ndim!=n)
	errstr("wrong number of subscripts on %s",
		varstr(VL, np->varname) );
else if(n > 0)
	{
	prod = sub[--n];
	while( --n >= 0)
		prod = mkexpr(OPPLUS, sub[n],
			mkexpr(OPSTAR, prod, cpexpr(dimp->dims[n].dimsize)) );
#if TARGET == VAX
#ifdef SDB
	if(checksubs || np->vstg!=STGARG || sdbflag)
#else
	if(checksubs || np->vstg!=STGARG)
#endif
		prod = mkexpr(OPMINUS, prod, cpexpr(dimp->baseoffset));
#else
	prod = mkexpr(OPMINUS, prod, cpexpr(dimp->baseoffset));
#endif
	if(checksubs)
		prod = subcheck(np, prod);
	size = np->vtype == TYCHAR ?
		(expptr) cpexpr(np->vleng) : ICON(typesize[np->vtype]);
	if (!oftwo(size))
		prod = mkexpr(OPSTAR, prod, size);
	else
		prod = mkexpr(OPLSHIFT,prod,oftwo(size));

	offp = mkexpr(OPPLUS, offp, prod);
	}

if(p->fcharp && np->vtype==TYCHAR)
	offp = mkexpr(OPPLUS, offp, mkexpr(OPMINUS, cpexpr(p->fcharp), ICON(1) ));

return(offp);
}




expptr subcheck(np, p)
Namep np;
register expptr p;
{
struct Dimblock *dimp;
expptr t, checkvar, checkcond, badcall;

dimp = np->vdim;
if(dimp->nelt == NULL)
	return(p);	/* don't check arrays with * bounds */
checkvar = NULL;
checkcond = NULL;
if( ISICON(p) )
	{
	if(p->constblock.const.ci < 0)
		goto badsub;
	if( ISICON(dimp->nelt) )
		if(p->constblock.const.ci < dimp->nelt->constblock.const.ci)
			return(p);
		else
			goto badsub;
	}
if(p->tag==TADDR && p->addrblock.vstg==STGREG)
	{
	checkvar = (expptr) cpexpr(p);
	t = p;
	}
else	{
	checkvar = (expptr) mktemp(p->headblock.vtype, ENULL);
	t = mkexpr(OPASSIGN, cpexpr(checkvar), p);
	}
checkcond = mkexpr(OPLT, t, cpexpr(dimp->nelt) );
if( ! ISICON(p) )
	checkcond = mkexpr(OPAND, checkcond,
			mkexpr(OPLE, ICON(0), cpexpr(checkvar)) );

badcall = call4(p->headblock.vtype, "s_rnge",
		mkstrcon(VL, np->varname),
		mkconv(TYLONG,  cpexpr(checkvar)),
		mkstrcon(XL, procname),
		ICON(lineno) );
badcall->exprblock.opcode = OPCCALL;
p = mkexpr(OPQUEST, checkcond,
	mkexpr(OPCOLON, checkvar, badcall));

return(p);

badsub:
	frexpr(p);
	errstr("subscript on variable %s out of range", varstr(VL,np->varname));
	return ( ICON(0) );
}




Addrp mkaddr(p)
register Namep p;
{
struct Extsym *extp;
register Addrp t;
Addrp intraddr();

switch( p->vstg)
	{
	case STGUNKNOWN:
		if(p->vclass != CLPROC)
			break;
		extp = mkext( varunder(VL, p->varname) );
		extp->extstg = STGEXT;
		p->vstg = STGEXT;
		p->vardesc.varno = extp - extsymtab;
		p->vprocclass = PEXTERNAL;

	case STGCOMMON:
	case STGEXT:
	case STGBSS:
	case STGINIT:
	case STGEQUIV:
	case STGARG:
	case STGLENG:
	case STGAUTO:
		t = ALLOC(Addrblock);
		t->tag = TADDR;
		if(p->vclass==CLPROC && p->vprocclass==PTHISPROC)
			t->vclass = CLVAR;
		else
			t->vclass = p->vclass;
		t->vtype = p->vtype;
		t->vstg = p->vstg;
		t->memno = p->vardesc.varno;
		t->issaved = p->vsave;
                if(p->vdim) t->isarray = YES;
		t->memoffset = ICON(p->voffset);
		if(p->vleng)
			{
			t->vleng = (expptr) cpexpr(p->vleng);
			if( ISICON(t->vleng) )
				t->varleng = t->vleng->constblock.const.ci;
			}
		if (p->vstg == STGBSS)
			t->varsize = p->varsize;
		else if (p->vstg == STGEQUIV)
			t->varsize = eqvclass[t->memno].eqvleng;
		return(t);

	case STGINTR:
		return( intraddr(p) );

	}
/*debug*/fprintf(diagfile,"mkaddr. vtype=%d, vclass=%d\n", p->vtype, p->vclass);
badstg("mkaddr", p->vstg);
/* NOTREACHED */
}




Addrp mkarg(type, argno)
int type, argno;
{
register Addrp p;

p = ALLOC(Addrblock);
p->tag = TADDR;
p->vtype = type;
p->vclass = CLVAR;
p->vstg = (type==TYLENG ? STGLENG : STGARG);
p->memno = argno;
return(p);
}




expptr mkprim(v, args, substr)
register union
	{
	struct Paramblock paramblock;
	struct Nameblock nameblock;
	struct Headblock headblock;
	} *v;
struct Listblock *args;
chainp substr;
{
register struct Primblock *p;

if(v->headblock.vclass == CLPARAM)
	{
	if(args || substr)
		{
		errstr("no qualifiers on parameter name %s",
			varstr(VL,v->paramblock.varname));
		frexpr(args);
		if(substr)
			{
			frexpr(substr->datap);
			frexpr(substr->nextp->datap);
			frchain(&substr);
			}
		frexpr(v);
		return( errnode() );
		}
	return( (expptr) cpexpr(v->paramblock.paramval) );
	}

p = ALLOC(Primblock);
p->tag = TPRIM;
p->vtype = v->nameblock.vtype;
p->namep = (Namep) v;
p->argsp = args;
if(substr)
	{
	p->fcharp = (expptr) (substr->datap);
	p->lcharp = (expptr) (substr->nextp->datap);
	frchain(&substr);
	}
return( (expptr) p);
}



vardcl(v)
register Namep v;
{
int nelt;
struct Dimblock *t;
Addrp p;
expptr neltp;
int eltsize;
int varsize;
int tsize;
int align;

if(v->vdcldone)
	return;
if(v->vclass == CLNAMELIST)
	return;

if(v->vtype == TYUNKNOWN)
	impldcl(v);
if(v->vclass == CLUNKNOWN)
	v->vclass = CLVAR;
else if(v->vclass!=CLVAR && v->vprocclass!=PTHISPROC)
	{
	dclerr("used both as variable and non-variable", v);
	return;
	}
if(v->vstg==STGUNKNOWN)
	v->vstg = implstg[ letter(v->varname[0]) ];

switch(v->vstg)
	{
	case STGBSS:
		v->vardesc.varno = ++lastvarno;
		if (v->vclass != CLVAR)
			break;
		nelt = 1;
		t = v->vdim;
		if (t)
			{
			neltp = t->nelt;
			if (neltp && ISICON(neltp))
				nelt = neltp->constblock.const.ci;
			else
				dclerr("improperly dimensioned array", v);
			}

		if (v->vtype == TYCHAR)
			{
			v->vleng = fixtype(v->vleng);
			if (v->vleng == NULL)
				eltsize = typesize[TYCHAR];
			else if (ISICON(v->vleng))
				eltsize = typesize[TYCHAR] *
					v->vleng->constblock.const.ci;
			else if (v->vleng->tag != TERROR)
				{
				errstr("nonconstant string length on %s",
					varstr(VL, v->varname));
				eltsize = 0;
				}
			}
		else
			eltsize = typesize[v->vtype];

		v->varsize = nelt * eltsize;
		break;
	case STGAUTO:
		if(v->vclass==CLPROC && v->vprocclass==PTHISPROC)
			break;
		nelt = 1;
		if(t = v->vdim)
			if( (neltp = t->nelt) && ISCONST(neltp) )
				nelt = neltp->constblock.const.ci;
			else
				dclerr("adjustable automatic array", v);
		p = autovar(nelt, v->vtype, v->vleng);
		v->vardesc.varno = p->memno;
		v->voffset = p->memoffset->constblock.const.ci;
		frexpr(p);
		break;

	default:
		break;
	}
v->vdcldone = YES;
}




impldcl(p)
register Namep p;
{
register int k;
int type, leng;

if(p->vdcldone || (p->vclass==CLPROC && p->vprocclass==PINTRINSIC) )
	return;
if(p->vtype == TYUNKNOWN)
	{
	k = letter(p->varname[0]);
	type = impltype[ k ];
	leng = implleng[ k ];
	if(type == TYUNKNOWN)
		{
		if(p->vclass == CLPROC)
			return;
		dclerr("attempt to use undefined variable", p);
		type = TYERROR;
		leng = 1;
		}
	settype(p, type, leng);
	}
}




LOCAL letter(c)
register int c;
{
if( isupper(c) )
	c = tolower(c);
return(c - 'a');
}

#define ICONEQ(z, c)  (ISICON(z) && z->constblock.const.ci==c)
#define COMMUTE	{ e = lp;  lp = rp;  rp = e; }


expptr mkexpr(opcode, lp, rp)
int opcode;
register expptr lp, rp;
{
register expptr e, e1;
int etype;
int ltype, rtype;
int ltag, rtag;
expptr q, q1;
expptr fold();
int k;

ltype = lp->headblock.vtype;
ltag = lp->tag;
if(rp && opcode!=OPCALL && opcode!=OPCCALL)
	{
	rtype = rp->headblock.vtype;
	rtag = rp->tag;
	}
else	{
	rtype = 0;
	rtag = 0;
	}

/*
 * Yuck.  Why can't we fold constants AFTER
 * variables are implicitly declared???
 */
if(ltype == TYUNKNOWN && ltag == TPRIM && lp->primblock.argsp == NULL)
	{
	k = letter(lp->primblock.namep->varname[0]);
	ltype = impltype[ k ];
	}
if(rtype == TYUNKNOWN && rtag == TPRIM && rp->primblock.argsp == NULL)
	{
	k = letter(rp->primblock.namep->varname[0]);
	rtype = impltype[ k ];
	}

etype = cktype(opcode, ltype, rtype);
if(etype == TYERROR)
	goto error;

if(etype != TYUNKNOWN)
switch(opcode)
	{
	/* check for multiplication by 0 and 1 and addition to 0 */

	case OPSTAR:
		if( ISCONST(lp) )
			COMMUTE

		if( ISICON(rp) )
			{
			if(rp->constblock.const.ci == 0)
				{
				if(etype == TYUNKNOWN)
					break;
				rp = mkconv(etype, rp);
				goto retright;
				}
			if ((lp->tag == TEXPR) &&
			    ((lp->exprblock.opcode == OPPLUS) ||
			     (lp->exprblock.opcode == OPMINUS)) &&
			    ISCONST(lp->exprblock.rightp) &&
			    ISINT(lp->exprblock.rightp->constblock.vtype))
				{
				q1 = mkexpr(OPSTAR, lp->exprblock.rightp,
					   cpexpr(rp));
				q = mkexpr(OPSTAR, lp->exprblock.leftp, rp);
				q = mkexpr(lp->exprblock.opcode, q, q1);
				free ((char *) lp);
				return q;
				}
			else
				goto mulop;
			}
		break;

	case OPSLASH:
	case OPMOD:
		if( ICONEQ(rp, 0) )
			{
			err("attempted division by zero");
			rp = ICON(1);
			break;
			}
		if(opcode == OPMOD)
			break;


	mulop:
		if( ISICON(rp) )
			{
			if(rp->constblock.const.ci == 1)
				goto retleft;

			if(rp->constblock.const.ci == -1)
				{
				frexpr(rp);
				return( mkexpr(OPNEG, lp, PNULL) );
				}
			}

		if( ISSTAROP(lp) && ISICON(lp->exprblock.rightp) )
			{
			if(opcode == OPSTAR)
				e = mkexpr(OPSTAR, lp->exprblock.rightp, rp);
			else  if(ISICON(rp) &&
				(lp->exprblock.rightp->constblock.const.ci %
					rp->constblock.const.ci) == 0)
				e = mkexpr(OPSLASH, lp->exprblock.rightp, rp);
			else	break;

			e1 = lp->exprblock.leftp;
			free( (charptr) lp );
			return( mkexpr(OPSTAR, e1, e) );
			}
		break;


	case OPPLUS:
		if( ISCONST(lp) )
			COMMUTE
		goto addop;

	case OPMINUS:
		if( ICONEQ(lp, 0) )
			{
			frexpr(lp);
			return( mkexpr(OPNEG, rp, ENULL) );
			}

		if( ISCONST(rp) )
			{
			opcode = OPPLUS;
			consnegop(rp);
			}

	addop:
		if( ISICON(rp) )
			{
			if(rp->constblock.const.ci == 0)
				goto retleft;
			if( ISPLUSOP(lp) && ISICON(lp->exprblock.rightp) )
				{
				e = mkexpr(OPPLUS, lp->exprblock.rightp, rp);
				e1 = lp->exprblock.leftp;
				free( (charptr) lp );
				return( mkexpr(OPPLUS, e1, e) );
				}
			}
		break;


	case OPPOWER:
		break;

	case OPNEG:
		if(ltag==TEXPR && lp->exprblock.opcode==OPNEG)
			{
			e = lp->exprblock.leftp;
			free( (charptr) lp );
			return(e);
			}
		break;

	case OPNOT:
		if(ltag==TEXPR && lp->exprblock.opcode==OPNOT)
			{
			e = lp->exprblock.leftp;
			free( (charptr) lp );
			return(e);
			}
		break;

	case OPCALL:
	case OPCCALL:
		etype = ltype;
		if(rp!=NULL && rp->listblock.listp==NULL)
			{
			free( (charptr) rp );
			rp = NULL;
			}
		break;

	case OPAND:
	case OPOR:
		if( ISCONST(lp) )
			COMMUTE

		if( ISCONST(rp) )
			{
			if(rp->constblock.const.ci == 0)
				if(opcode == OPOR)
					goto retleft;
				else
					goto retright;
			else if(opcode == OPOR)
				goto retright;
			else
				goto retleft;
			}
	case OPLSHIFT:
		if (ISICON(rp))
			{
			if (rp->constblock.const.ci == 0)
				goto retleft;
			if ((lp->tag == TEXPR) &&
			    ((lp->exprblock.opcode == OPPLUS) ||
			     (lp->exprblock.opcode == OPMINUS)) &&
			    ISICON(lp->exprblock.rightp))
				{
				q1 = mkexpr(OPLSHIFT, lp->exprblock.rightp,
					cpexpr(rp));
				q = mkexpr(OPLSHIFT, lp->exprblock.leftp, rp);
				q = mkexpr(lp->exprblock.opcode, q, q1);
				free((char *) lp);
				return q;
				}
			}

	case OPEQV:
	case OPNEQV:

	case OPBITAND:
	case OPBITOR:
	case OPBITXOR:
	case OPBITNOT:
	case OPRSHIFT:

	case OPLT:
	case OPGT:
	case OPLE:
	case OPGE:
	case OPEQ:
	case OPNE:

	case OPCONCAT:
		break;
	case OPMIN:
	case OPMAX:

	case OPASSIGN:
	case OPPLUSEQ:
	case OPSTAREQ:

	case OPCONV:
	case OPADDR:

	case OPCOMMA:
	case OPQUEST:
	case OPCOLON:

	case OPPAREN:
		break;

	default:
		badop("mkexpr", opcode);
	}

e = (expptr) ALLOC(Exprblock);
e->exprblock.tag = TEXPR;
e->exprblock.opcode = opcode;
e->exprblock.vtype = etype;
e->exprblock.leftp = lp;
e->exprblock.rightp = rp;
if(ltag==TCONST && (rp==0 || rtag==TCONST) )
	e = fold(e);
return(e);

retleft:
	frexpr(rp);
	return(lp);

retright:
	frexpr(lp);
	return(rp);

error:
	frexpr(lp);
	if(rp && opcode!=OPCALL && opcode!=OPCCALL)
		frexpr(rp);
	return( errnode() );
}

#define ERR(s)   { errs = s; goto error; }

cktype(op, lt, rt)
register int op, lt, rt;
{
char *errs;

if(lt==TYERROR || rt==TYERROR)
	goto error1;

if(lt==TYUNKNOWN)
	return(TYUNKNOWN);
if(rt==TYUNKNOWN)
	if (op!=OPNOT && op!=OPBITNOT && op!=OPNEG && op!=OPCALL &&
	    op!=OPCCALL && op!=OPADDR && op!=OPPAREN)
		return(TYUNKNOWN);

switch(op)
	{
	case OPPLUS:
	case OPMINUS:
	case OPSTAR:
	case OPSLASH:
	case OPPOWER:
	case OPMOD:
		if( ISNUMERIC(lt) && ISNUMERIC(rt) )
			return( maxtype(lt, rt) );
		ERR("nonarithmetic operand of arithmetic operator")

	case OPNEG:
		if( ISNUMERIC(lt) )
			return(lt);
		ERR("nonarithmetic operand of negation")

	case OPNOT:
		if(lt == TYLOGICAL)
			return(TYLOGICAL);
		ERR("NOT of nonlogical")

	case OPAND:
	case OPOR:
	case OPEQV:
	case OPNEQV:
		if(lt==TYLOGICAL && rt==TYLOGICAL)
			return(TYLOGICAL);
		ERR("nonlogical operand of logical operator")

	case OPLT:
	case OPGT:
	case OPLE:
	case OPGE:
	case OPEQ:
	case OPNE:
		if(lt==TYCHAR || rt==TYCHAR || lt==TYLOGICAL || rt==TYLOGICAL)
			{
			if(lt != rt)
				ERR("illegal comparison")
			}

		else if( ISCOMPLEX(lt) || ISCOMPLEX(rt) )
			{
			if(op!=OPEQ && op!=OPNE)
				ERR("order comparison of complex data")
			}

		else if( ! ISNUMERIC(lt) || ! ISNUMERIC(rt) )
			ERR("comparison of nonarithmetic data")
		return(TYLOGICAL);

	case OPCONCAT:
		if(lt==TYCHAR && rt==TYCHAR)
			return(TYCHAR);
		ERR("concatenation of nonchar data")

	case OPCALL:
	case OPCCALL:
		return(lt);

	case OPADDR:
		return(TYADDR);

	case OPCONV:
		if(ISCOMPLEX(lt))
			{
			if(ISNUMERIC(rt))
				return(lt);
			ERR("impossible conversion")
			}
		if(rt == 0)
			return(0);
		if(lt==TYCHAR && ISINT(rt) )
			return(TYCHAR);
	case OPASSIGN:
	case OPPLUSEQ:
	case OPSTAREQ:
		if( ISINT(lt) && rt==TYCHAR)
			return(lt);
		if(lt==TYCHAR || rt==TYCHAR || lt==TYLOGICAL || rt==TYLOGICAL)
			if(op!=OPASSIGN || lt!=rt)
				{
/* debug fprintf(diagfile, " lt=%d, rt=%d, op=%d\n", lt, rt, op); */
/* debug fatal("impossible conversion.  possible compiler bug"); */
				ERR("impossible conversion")
				}
		return(lt);

	case OPMIN:
	case OPMAX:
	case OPBITOR:
	case OPBITAND:
	case OPBITXOR:
	case OPBITNOT:
	case OPLSHIFT:
	case OPRSHIFT:
	case OPPAREN:
		return(lt);

	case OPCOMMA:
	case OPQUEST:
	case OPCOLON:
		return(rt);

	default:
		badop("cktype", op);
	}
error:	err(errs);
error1:	return(TYERROR);
}

LOCAL expptr fold(e)
register expptr e;
{
Constp p;
register expptr lp, rp;
int etype, mtype, ltype, rtype, opcode;
int i, ll, lr;
char *q, *s;
union Constant lcon, rcon;

opcode = e->exprblock.opcode;
etype = e->exprblock.vtype;

lp = e->exprblock.leftp;
ltype = lp->headblock.vtype;
rp = e->exprblock.rightp;

if(rp == 0)
	switch(opcode)
		{
		case OPNOT:
			lp->constblock.const.ci = ! lp->constblock.const.ci;
			return(lp);

		case OPBITNOT:
			lp->constblock.const.ci = ~ lp->constblock.const.ci;
			return(lp);

		case OPNEG:
			consnegop(lp);
			return(lp);

		case OPCONV:
		case OPADDR:
		case OPPAREN:
			return(e);

		default:
			badop("fold", opcode);
		}

rtype = rp->headblock.vtype;

p = ALLOC(Constblock);
p->tag = TCONST;
p->vtype = etype;
p->vleng = e->exprblock.vleng;

switch(opcode)
	{
	case OPCOMMA:
	case OPQUEST:
	case OPCOLON:
		return(e);

	case OPAND:
		p->const.ci = lp->constblock.const.ci &&
				rp->constblock.const.ci;
		break;

	case OPOR:
		p->const.ci = lp->constblock.const.ci ||
				rp->constblock.const.ci;
		break;

	case OPEQV:
		p->const.ci = lp->constblock.const.ci ==
				rp->constblock.const.ci;
		break;

	case OPNEQV:
		p->const.ci = lp->constblock.const.ci !=
				rp->constblock.const.ci;
		break;

	case OPBITAND:
		p->const.ci = lp->constblock.const.ci &
				rp->constblock.const.ci;
		break;

	case OPBITOR:
		p->const.ci = lp->constblock.const.ci |
				rp->constblock.const.ci;
		break;

	case OPBITXOR:
		p->const.ci = lp->constblock.const.ci ^
				rp->constblock.const.ci;
		break;

	case OPLSHIFT:
		p->const.ci = lp->constblock.const.ci <<
				rp->constblock.const.ci;
		break;

	case OPRSHIFT:
		p->const.ci = lp->constblock.const.ci >>
				rp->constblock.const.ci;
		break;

	case OPCONCAT:
		ll = lp->constblock.vleng->constblock.const.ci;
		lr = rp->constblock.vleng->constblock.const.ci;
		p->const.ccp = q = (char *) ckalloc(ll+lr);
		p->vleng = ICON(ll+lr);
		s = lp->constblock.const.ccp;
		for(i = 0 ; i < ll ; ++i)
			*q++ = *s++;
		s = rp->constblock.const.ccp;
		for(i = 0; i < lr; ++i)
			*q++ = *s++;
		break;


	case OPPOWER:
		if( ! ISINT(rtype) )
			return(e);
		conspower(&(p->const), lp, rp->constblock.const.ci);
		break;


	default:
		if(ltype == TYCHAR)
			{
			lcon.ci = cmpstr(lp->constblock.const.ccp,
					rp->constblock.const.ccp,
					lp->constblock.vleng->constblock.const.ci,
					rp->constblock.vleng->constblock.const.ci);
			rcon.ci = 0;
			mtype = tyint;
			}
		else	{
			mtype = maxtype(ltype, rtype);
			consconv(mtype, &lcon, ltype, &(lp->constblock.const) );
			consconv(mtype, &rcon, rtype, &(rp->constblock.const) );
			}
		consbinop(opcode, mtype, &(p->const), &lcon, &rcon);
		break;
	}

frexpr(e);
return( (expptr) p );
}



/* assign constant l = r , doing coercion */

consconv(lt, lv, rt, rv)
int lt, rt;
register union Constant *lv, *rv;
{
switch(lt)
	{
	case TYCHAR:
		*(lv->ccp = (char *) ckalloc(1)) = rv->ci;
		break;

	case TYSHORT:
	case TYLONG:
		if(rt == TYCHAR)
			lv->ci = rv->ccp[0];
		else if( ISINT(rt) )
			lv->ci = rv->ci;
		else	lv->ci = rv->cd[0];
		break;

	case TYCOMPLEX:
	case TYDCOMPLEX:
		switch(rt)
			{
			case TYSHORT:
			case TYLONG:
				/* fall through and do real assignment of
				   first element
				*/
			case TYREAL:
			case TYDREAL:
				lv->cd[1] = 0; break;
			case TYCOMPLEX:
			case TYDCOMPLEX:
				lv->cd[1] = rv->cd[1]; break;
			}

	case TYREAL:
	case TYDREAL:
		if( ISINT(rt) )
			lv->cd[0] = rv->ci;
		else	lv->cd[0] = rv->cd[0];
		if( lt == TYREAL)
			{
			float f = lv->cd[0];
			lv->cd[0] = f;
			}
		break;

	case TYLOGICAL:
		lv->ci = rv->ci;
		break;
	}
}



consnegop(p)
register Constp p;
{
switch(p->vtype)
	{
	case TYSHORT:
	case TYLONG:
		p->const.ci = - p->const.ci;
		break;

	case TYCOMPLEX:
	case TYDCOMPLEX:
		p->const.cd[1] = - p->const.cd[1];
		/* fall through and do the real parts */
	case TYREAL:
	case TYDREAL:
		p->const.cd[0] = - p->const.cd[0];
		break;
	default:
		badtype("consnegop", p->vtype);
	}
}



LOCAL conspower(powp, ap, n)
register union Constant *powp;
Constp ap;
ftnint n;
{
register int type;
union Constant x;

switch(type = ap->vtype)	/* pow = 1 */ 
	{
	case TYSHORT:
	case TYLONG:
		powp->ci = 1;
		break;
	case TYCOMPLEX:
	case TYDCOMPLEX:
		powp->cd[1] = 0;
	case TYREAL:
	case TYDREAL:
		powp->cd[0] = 1;
		break;
	default:
		badtype("conspower", type);
	}

if(n == 0)
	return;
if(n < 0)
	{
	if( ISINT(type) )
		{
		if (ap->const.ci == 0)
			err("zero raised to a negative power");
		else if (ap->const.ci == 1)
			return;
		else if (ap->const.ci == -1)
			{
			if (n < -2)
				n = n + 2;
			n = -n;
			if (n % 2 == 1)
				powp->ci = -1;
			}
		else
			powp->ci = 0;
		return;
		}
	n = - n;
	consbinop(OPSLASH, type, &x, powp, &(ap->const));
	}
else
	consbinop(OPSTAR, type, &x, powp, &(ap->const));

for( ; ; )
	{
	if(n & 01)
		consbinop(OPSTAR, type, powp, powp, &x);
	if(n >>= 1)
		consbinop(OPSTAR, type, &x, &x, &x);
	else
		break;
	}
}



/* do constant operation cp = a op b */


LOCAL consbinop(opcode, type, cp, ap, bp)
int opcode, type;
register union Constant *ap, *bp, *cp;
{
int k;
double temp;

switch(opcode)
	{
	case OPPLUS:
		switch(type)
			{
			case TYSHORT:
			case TYLONG:
				cp->ci = ap->ci + bp->ci;
				break;
			case TYCOMPLEX:
			case TYDCOMPLEX:
				cp->cd[1] = ap->cd[1] + bp->cd[1];
			case TYREAL:
			case TYDREAL:
				cp->cd[0] = ap->cd[0] + bp->cd[0];
				break;
			}
		break;

	case OPMINUS:
		switch(type)
			{
			case TYSHORT:
			case TYLONG:
				cp->ci = ap->ci - bp->ci;
				break;
			case TYCOMPLEX:
			case TYDCOMPLEX:
				cp->cd[1] = ap->cd[1] - bp->cd[1];
			case TYREAL:
			case TYDREAL:
				cp->cd[0] = ap->cd[0] - bp->cd[0];
				break;
			}
		break;

	case OPSTAR:
		switch(type)
			{
			case TYSHORT:
			case TYLONG:
				cp->ci = ap->ci * bp->ci;
				break;
			case TYREAL:
			case TYDREAL:
				cp->cd[0] = ap->cd[0] * bp->cd[0];
				break;
			case TYCOMPLEX:
			case TYDCOMPLEX:
				temp = ap->cd[0] * bp->cd[0] -
					    ap->cd[1] * bp->cd[1] ;
				cp->cd[1] = ap->cd[0] * bp->cd[1] +
					    ap->cd[1] * bp->cd[0] ;
				cp->cd[0] = temp;
				break;
			}
		break;
	case OPSLASH:
		switch(type)
			{
			case TYSHORT:
			case TYLONG:
				cp->ci = ap->ci / bp->ci;
				break;
			case TYREAL:
			case TYDREAL:
				cp->cd[0] = ap->cd[0] / bp->cd[0];
				break;
			case TYCOMPLEX:
			case TYDCOMPLEX:
				zdiv(cp,ap,bp);
				break;
			}
		break;

	case OPMOD:
		if( ISINT(type) )
			{
			cp->ci = ap->ci % bp->ci;
			break;
			}
		else
			fatal("inline mod of noninteger");

	default:	  /* relational ops */
		switch(type)
			{
			case TYSHORT:
			case TYLONG:
				if(ap->ci < bp->ci)
					k = -1;
				else if(ap->ci == bp->ci)
					k = 0;
				else	k = 1;
				break;
			case TYREAL:
			case TYDREAL:
				if(ap->cd[0] < bp->cd[0])
					k = -1;
				else if(ap->cd[0] == bp->cd[0])
					k = 0;
				else	k = 1;
				break;
			case TYCOMPLEX:
			case TYDCOMPLEX:
				if(ap->cd[0] == bp->cd[0] &&
				   ap->cd[1] == bp->cd[1] )
					k = 0;
				else	k = 1;
				break;
			}

		switch(opcode)
			{
			case OPEQ:
				cp->ci = (k == 0);
				break;
			case OPNE:
				cp->ci = (k != 0);
				break;
			case OPGT:
				cp->ci = (k == 1);
				break;
			case OPLT:
				cp->ci = (k == -1);
				break;
			case OPGE:
				cp->ci = (k >= 0);
				break;
			case OPLE:
				cp->ci = (k <= 0);
				break;
			default:
				badop ("consbinop", opcode);
			}
		break;
	}
}




conssgn(p)
register expptr p;
{
if( ! ISCONST(p) )
	fatal( "sgn(nonconstant)" );

switch(p->headblock.vtype)
	{
	case TYSHORT:
	case TYLONG:
		if(p->constblock.const.ci > 0) return(1);
		if(p->constblock.const.ci < 0) return(-1);
		return(0);

	case TYREAL:
	case TYDREAL:
		if(p->constblock.const.cd[0] > 0) return(1);
		if(p->constblock.const.cd[0] < 0) return(-1);
		return(0);

	case TYCOMPLEX:
	case TYDCOMPLEX:
		return(p->constblock.const.cd[0]!=0 || p->constblock.const.cd[1]!=0);

	default:
		badtype( "conssgn", p->constblock.vtype);
	}
/* NOTREACHED */
}

char *powint[ ] = { "pow_ii", "pow_ri", "pow_di", "pow_ci", "pow_zi" };


LOCAL expptr mkpower(p)
register expptr p;
{
register expptr q, lp, rp;
int ltype, rtype, mtype;

lp = p->exprblock.leftp;
rp = p->exprblock.rightp;
ltype = lp->headblock.vtype;
rtype = rp->headblock.vtype;

if(ISICON(rp))
	{
	if(rp->constblock.const.ci == 0)
		{
		frexpr(p);
		if( ISINT(ltype) )
			return( ICON(1) );
		else
			{
			expptr pp;
			pp = mkconv(ltype, ICON(1));
			return( pp );
			}
		}
	if(rp->constblock.const.ci < 0)
		{
		if( ISINT(ltype) )
			{
			frexpr(p);
			err("integer**negative");
			return( errnode() );
			}
		rp->constblock.const.ci = - rp->constblock.const.ci;
		p->exprblock.leftp = lp = fixexpr(mkexpr(OPSLASH, ICON(1), lp));
		}
	if(rp->constblock.const.ci == 1)
		{
		frexpr(rp);
		free( (charptr) p );
		return(lp);
		}

	if( ONEOF(ltype, MSKINT|MSKREAL) )
		{
		p->exprblock.vtype = ltype;
		return(p);
		}
	}
if( ISINT(rtype) )
	{
	if(ltype==TYSHORT && rtype==TYSHORT && (!ISCONST(lp) || tyint==TYSHORT) )
		q = call2(TYSHORT, "pow_hh", lp, rp);
	else	{
		if(ltype == TYSHORT)
			{
			ltype = TYLONG;
			lp = mkconv(TYLONG,lp);
			}
		q = call2(ltype, powint[ltype-TYLONG], lp, mkconv(TYLONG, rp));
		}
	}
else if( ISREAL( (mtype = maxtype(ltype,rtype)) ))
	q = call2(mtype, "pow_dd", mkconv(TYDREAL,lp), mkconv(TYDREAL,rp));
else	{
	q  = call2(TYDCOMPLEX, "pow_zz",
		mkconv(TYDCOMPLEX,lp), mkconv(TYDCOMPLEX,rp));
	if(mtype == TYCOMPLEX)
		q = mkconv(TYCOMPLEX, q);
	}
free( (charptr) p );
return(q);
}



/* Complex Division.  Same code as in Runtime Library
*/

struct dcomplex { double dreal, dimag; };


LOCAL zdiv(c, a, b)
register struct dcomplex *a, *b, *c;
{
double ratio, den;
double abr, abi;

if( (abr = b->dreal) < 0.)
	abr = - abr;
if( (abi = b->dimag) < 0.)
	abi = - abi;
if( abr <= abi )
	{
	if(abi == 0)
		fatal("complex division by zero");
	ratio = b->dreal / b->dimag ;
	den = b->dimag * (1 + ratio*ratio);
	c->dreal = (a->dreal*ratio + a->dimag) / den;
	c->dimag = (a->dimag*ratio - a->dreal) / den;
	}

else
	{
	ratio = b->dimag / b->dreal ;
	den = b->dreal * (1 + ratio*ratio);
	c->dreal = (a->dreal + a->dimag*ratio) / den;
	c->dimag = (a->dimag - a->dreal*ratio) / den;
	}

}

expptr oftwo(e)
expptr e;
{
	int val,res;

	if (! ISCONST (e))
		return (0);

	val = e->constblock.const.ci;
	switch (val)
		{
		case 2:		res = 1; break;
		case 4:		res = 2; break;
		case 8:		res = 3; break;
		case 16:	res = 4; break;
		case 32:	res = 5; break;
		case 64:	res = 6; break;
		case 128:	res = 7; break;
		case 256:	res = 8; break;
		default:	return (0);
		}
	return (ICON (res));
}
