/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)tahoe.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include "defs.h"

#ifdef SDB
#	include <a.out.h>
extern int types2[];
#	ifndef N_SO
#		include <stab.h>
#	endif
#endif

#include "pcc.h"

/*
	TAHOE - SPECIFIC ROUTINES
*/

int maxregvar = MAXREGVAR;
int regnum[] =  { 10, 9, 8, 7, 6 } ;

ftnint intcon[14] =
	{ 2, 2, 2, 2,
	  15, 31, 24, 56,
	  -128, -128, 127, 127,
	  0x7FFF, 0x7FFFFFFF };

#if HERE == VAX || HERE == TAHOE
	/* then put in constants in hex */
short realcon[6][4] =
	{
		{ 0x80, 0, 0, 0 },
		{ 0x80, 0, 0, 0 },
		{ 0x7FFF, 0xFFFF, 0, 0 },
		{ 0x7FFF, 0xFFFF, 0xFFFF, 0xFFFF },
		{ 0x3480, 0, 0, 0 },
		{ 0x2480, 0, 0, 0 },
	};
#else
double realcon[6] =
	{
	2.9387358771e-39,		/* 2 ** -128 */
	2.938735877055718800e-39,	/* 2 ** -128 */
	1.7014117332e+38,		/* 2**127 * (1 - 2**-24) */
	1.701411834604692250e+38,	/* 2**127 * (1 - 2**-56) */
	5.960464e-8,			/* 2 ** -24 */
	1.38777878078144567e-17,	/* 2 ** -56 */
	};
#endif

/*
 * The VAX assembler has a serious and not easily fixable problem
 * with generating instructions that contain expressions of the form
 * label1-label2 where there are .align's in-between the labels.
 * Therefore, the compiler must keep track of the offsets and output
 * .space where needed.
 */
LOCAL int i_offset;		/* initfile offset */
LOCAL int a_offset;		/* asmfile offset */

prsave(proflab)
int proflab;
{
if(profileflag)
	{
	fprintf(asmfile, "\t.align\t2\n");
	fprintf(asmfile, "L%d:\t.long\t0\n", proflab);
	p2pi("\tpushl\t$L%d", proflab);
	p2pass("\tcallf\t$8,mcount");
	}
p2pi("\tsubl3\t$LF%d,fp,sp", procno);
}

goret(type)
int type;
{
register int r = 0;
switch(type) {	/* from retval */
	case TYDREAL:
		r++;

	case TYLOGICAL:
	case TYADDR:
	case TYSHORT:
	case TYLONG:
	case TYREAL:
		r++;

	case TYCHAR:
	case TYCOMPLEX:
	case TYDCOMPLEX:
		break;
	case TYSUBR:
		if (substars) r++;
		break;
	default:
		badtype("goret", type);
	}
p2pi("\tret#%d", r);
}

/*
 * move argument slot arg1 (relative to fp)
 * to slot arg2 (relative to ARGREG)
 */
mvarg(type, arg1, arg2)
int type, arg1, arg2;
{
p2pij("\tmovl\t%d(fp),%d(fp)", arg1+ARGOFFSET, arg2+argloc);
}

prlabel(fp, k)
FILEP fp;
int k;
{
fprintf(fp, "L%d:\n", k);
}

prconi(fp, type, n)
FILEP fp;
int type;
ftnint n;
{
register int i;

if(type == TYSHORT)
	{
	fprintf(fp, "\t.word\t%ld\n", n);
	i = SZSHORT;
	}
else
	{
	fprintf(fp, "\t.long\t%ld\n", n);
	i = SZLONG;
	}
if(fp == initfile)
	i_offset += i;
else
	a_offset += i;
}

prcona(fp, a)
FILEP fp;
ftnint a;
{
fprintf(fp, "\t.long\tL%ld\n", a);
if(fp == initfile)
	i_offset += SZLONG;
else
	a_offset += SZLONG;
}

prconr(fp, type, x)
FILEP fp;
int type;
double x;
{
/*
fprintf(fp, "\t%s\t0f%e\n", (type==TYREAL ? ".float" : ".double"), x);
*/
	/* non-portable cheat to preserve bit patterns */
	/* this code should be the same for PDP, VAX and Tahoe */

	register struct sh4 {
		unsigned short sh[4];
	} *cheat;
	register int i;

	cheat = (struct sh4 *)&x;
	if(type == TYREAL) {	/* force rounding */
		float f;
		f = x;
		x = f;
	}
	fprintf(fp, "	.long	0x%04x%04x", cheat->sh[0], cheat->sh[1]);
	if(type == TYDREAL) {
		fprintf(fp, ", 0x%04x%04x", cheat->sh[2], cheat->sh[3]);
		fprintf(fp, "	# .double %.17g\n", x);
		i = SZDOUBLE;
	} 
	else
	{
		fprintf(fp, "	# .float %.8g\n", x);
		i = SZFLOAT;
	}
if(fp == initfile)
	i_offset += i;
else
	a_offset += i;
}

praddr(fp, stg, varno, offset)
FILE *fp;
int stg, varno;
ftnint offset;
{
char *memname();

if(stg == STGNULL)
	fprintf(fp, "\t.long\t0\n");
else
	{
	fprintf(fp, "\t.long\t%s", memname(stg,varno));
	if(offset)
		fprintf(fp, "+%ld", offset);
	fprintf(fp, "\n");
	}
if(fp == initfile)
	i_offset += SZADDR;
else
	a_offset += SZADDR;
}
pralign(k)
int k;
{
  register int lg;

  if (k > 4)
    lg = 3;
  else if (k > 2)
    lg = 2;
  else if (k > 1)
    lg = 1;
  else
    return;
  fprintf(initfile, "\t.align\t%d\n", lg);
i_offset += lg;
  return;
}



prspace(n)
int n;
{

fprintf(initfile, "\t.space\t%d\n", n);
i_offset += n;
}


preven(k)
int k;
{
register int lg;

if(k > 4)
	lg = 3;
else if(k > 2)
	lg = 2;
else if(k > 1)
	lg = 1;
else
	return;
fprintf(asmfile, "\t.align\t%d\n", lg);
a_offset += lg;
}

praspace(n)
int n;
{

fprintf(asmfile, "\t.space\t%d\n", n);
a_offset += n;
}


casegoto(index, nlab, labs)
expptr index;
register int nlab;
struct Labelblock *labs[];
{
register int i;
register int arrlab;

putforce(TYINT, index);
p2pi("\tcasel\tr0,$1,$%d\n\t.align 1", nlab-1);
p2pi("L%d:", arrlab = newlabel() );
for(i = 0; i< nlab ; ++i)
	if( labs[i] )
		p2pij("\t.word\tL%d-L%d", labs[i]->labelno, arrlab);
}


prarif(p, neg, zer, pos)
expptr p;
int neg, zer, pos;
{
putforce(p->headblock.vtype, p);
p2pass("\ttstl\tr0");
p2pi("\tjlss\tL%d", neg);
p2pi("\tjeql\tL%d", zer);
p2pi("\tjbr\tL%d", pos);
}

char *memname(stg, mem)
int stg, mem;
{
static char s[20];

switch(stg)
	{
	case STGEXT:
	case STGINTR:
		if(extsymtab[mem].extname[0] == '@') {	/* function opcodes */
			strcpy(s, varstr(XL, extsymtab[mem].extname));
			break;
		}
	case STGCOMMON:
		sprintf(s, "_%s", varstr(XL, extsymtab[mem].extname) );
		break;

	case STGBSS:
	case STGINIT:
		sprintf(s, "v.%d", mem);
		break;

	case STGCONST:
		sprintf(s, "L%d", mem);
		break;

	case STGEQUIV:
		sprintf(s, "q.%d", mem+eqvstart);
		break;

	default:
		badstg("memname", stg);
	}
return(s);
}

prlocvar(s, len)
char *s;
ftnint len;
{
int sz;
sz = len;
if (sz % SZINT)
	sz += SZINT - (sz % SZINT);
fprintf(asmfile, "\t.lcomm\t%s,%ld\n", s, sz);
}

char *
packbytes(cp)
register Constp cp;
{
#if HERE == VAX
  static char shrt[16];
  static char lng[4];
#endif

  switch (cp->vtype)
    {
#if HERE == TAHOE
    case TYSHORT:
    { static short shrt;
      shrt = cp->constant.ci;
      return ((char *)&shrt);
    }
    case TYLONG:
    case TYLOGICAL:
    case TYREAL:
    case TYDREAL:
    case TYDCOMPLEX:
      return ((char *)&cp->constant);
    case TYCOMPLEX:
      { static float quad[2];
      quad[0] = cp->constant.cd[0];
      quad[1] = cp->constant.cd[1];
      return ((char *)quad);
      }
#endif

#if HERE == VAX
    case TYLONG:
    case TYLOGICAL:
      swab4((char *)&cp->constant.ci, lng, 4);
      return (lng);

    case TYSHORT:
    case TYREAL:
    case TYDREAL:
    case TYDCOMPLEX:
      swab((char *)cp->constant.cd, shrt, typesize[cp->vtype]);
      return (shrt);
    case TYCOMPLEX:
      swab((char *)cp->constant.cd, shrt, 4);
      swab((char *)&(cp->constant.cd[1]), &shrt[4], 4);
      return (shrt);
#endif

    default:
      badtype("packbytes", cp->vtype);
    }
}

#if HERE == VAX
/* correct the byte order in longs */
LOCAL swab4(from, to, n)
  register char *to, *from;
  register int n;
{
  while(n >= 4) {
    *to++ = from[3];
    *to++ = from[2];
    *to++ = from[1];
    *to++ = from[0];
    from += 4;
    n -= 4;
  }
  while(n >= 2) {
    *to++ = from[1];
    *to++ = from[0];
    from += 2;
    n -= 2;
  }
  if(n > 0)
	*to = *from;
}
#endif

prsdata(s, len)
register char *s; /* must be aligned if HERE==TAHOE */
register int len;
{
  static char longfmt[] = "\t.long\t0x%x\n";
  static char wordfmt[] = "\t.word\t0x%x\n";
  static char bytefmt[] = "\t.byte\t0x%x\n";

  register int i;
#if HERE == VAX
  char quad[8];
  swab4(s, quad, len);
  s = quad;
#endif

  i = 0;
  if ((len - i) >= 4)
    {
      fprintf(initfile, longfmt, *((int *) s));
      i += 4;
    }
  if ((len - i) >= 2)
    {
      fprintf(initfile, wordfmt, 0xffff & (*((short *) (s + i))));
      i += 2;
    }
  if ((len - i) > 0)
    fprintf(initfile,bytefmt, 0xff & s[i]);

  i_offset += len;
  return;
}

prquad(s)
register long *s;
{
  static char quadfmt1[] = "\t.quad\t0x%x\n";
  static char quadfmt2[] = "\t.quad\t0x%x%08x\n";
#if HERE == VAX
  char quad[8];
  swab4((char *)s, quad, 8);
  s = (long *)quad;
#endif

  if (s[0] == 0 )
    fprintf(initfile, quadfmt1, s[1]);
  else
    fprintf(initfile, quadfmt2, s[0], s[1]);

  return;
}

#ifdef UCBVAXASM
prfill(n, s)
int n;
register long *s;
{
  static char fillfmt1[] = "\t.fill\t%d,8,0x%x\n";
  static char fillfmt2[] = "\t.fill\t%d,8,0x%x%08x\n";
#if HERE == VAX
  char quad[8];
  swab4((char *)s, quad, 8);
  s = (long *)quad;
#endif

  if (s[0] == 0 )
    fprintf(initfile, fillfmt1, n, s[1]);
  else
    fprintf(initfile, fillfmt2, n, s[0], s[1]);

  return;
}
#endif

prext(ep)
register struct Extsym *ep;
{
  static char globlfmt[] = "\t.globl\t_%s\n";
  static char commfmt[] = "\t.comm\t_%s,%ld\n";
  static char align2fmt[] = "\t.align\t2\n";
  static char labelfmt[] = "_%s:\n";

  static char seekerror[] = "seek error on tmp file";
  static char readerror[] = "read error on tmp file";

  char *tag;
  register int leng;
  long pos;
  register char *p;
  long oldvalue[2];
  long newvalue[2];
  register int n;
  register int repl;

  tag = varstr(XL, ep->extname);
  leng = ep->maxleng;

  if (leng == 0)
    {
      if(*tag != '@')	/* function opcodes */
      fprintf(asmfile, globlfmt, tag);
      return;
    }

  if (ep->init == NO)
    {
      fprintf(asmfile, commfmt, tag, leng);
      return;
    }

  fprintf(asmfile, globlfmt, tag);
  fprintf(initfile, align2fmt);
  fprintf(initfile, labelfmt, tag);

  pos = lseek(cdatafile, ep->initoffset, 0);
  if (pos == -1)
    {
      err(seekerror);
      done(1);
    }

  oldvalue[0] = 0;
  oldvalue[1] = 0;
  n = read(cdatafile, oldvalue, 8);
  if (n < 0)
    {
      err(readerror);
      done(1);
    }

  if (leng <= 8)
    {
      p = (char *)oldvalue + leng;
      while (p > (char *)oldvalue && *--p == '\0') /* SKIP */;
      if (*p == '\0')
	prspace(leng);
      else if (leng == 8)
	prquad(oldvalue);
      else
	prsdata(oldvalue, leng);

      return;
    }

  repl = 1;
  leng -= 8;

  while (leng >= 8)
    {
      newvalue[0] = 0;
      newvalue[1] = 0;

      n = read(cdatafile, newvalue, 8);
      if (n < 0)
	{
	  err(readerror);
	  done(1);
	}

      leng -= 8;

      if (oldvalue[0] == newvalue[0]
	  && oldvalue[1] == newvalue[1])
	repl++;
      else
	{
	  if (oldvalue[0] == 0
	      && oldvalue[1] == 0)
	    prspace(8*repl);
	  else if (repl == 1)
	    prquad(oldvalue);
	  else
#ifdef UCBVAXASM
	    prfill(repl, oldvalue);
#else
	    {
	      while (repl-- > 0)
		prquad(oldvalue);
	    }
#endif
	  oldvalue[0] = newvalue[0];
	  oldvalue[1] = newvalue[1];
	  repl = 1;
	}
    }

  newvalue[0] = 0;
  newvalue[1] = 0;

  if (leng > 0)
    {
      n = read(cdatafile, newvalue, leng);
      if (n < 0)
	{
	  err(readerror);
	  done(1);
	}
    }

  if (oldvalue[1] == 0
      && oldvalue[0] == 0
      && newvalue[1] == 0
      && newvalue[0] == 0)
    {
      prspace(8*repl + leng);
      return;
    }

  if (oldvalue[1] == 0
      && oldvalue[0] == 0)
    prspace(8*repl);
  else if (repl == 1)
    prquad(oldvalue);
  else
#ifdef UCBVAXASM
    prfill(repl, oldvalue);
#else
    {
      while (repl-- > 0)
	prquad(oldvalue);
    }
#endif

  prsdata(newvalue, leng);

  return;
}

prlocdata(sname, leng, type, initoffset, inlcomm)
char *sname;
ftnint leng;
int type;
long initoffset;
char *inlcomm;
{
  static char seekerror[] = "seek error on tmp file";
  static char readerror[] = "read error on tmp file";

  static char labelfmt[] = "%s:\n";

  register int k;
  register char *p;
  register int repl;
  register int first;
  register long pos;
  register long n;
  long oldvalue[2];
  long newvalue[2];

  *inlcomm = NO;

  k = leng;
  first = YES;

  pos = lseek(vdatafile, initoffset, 0);
  if (pos == -1)
    {
      err(seekerror);
      done(1);
    }

  oldvalue[0] = 0;
  oldvalue[1] = 0;
  n = read(vdatafile, oldvalue, 8);
  if (n < 0)
    {
      err(readerror);
      done(1);
    }

  if (k <= 8)
    {
      p = (char *)oldvalue + k;
      while (p > (char *)oldvalue && *--p == '\0')
	/*  SKIP  */ ;
      if (*p == '\0')
	{
	  if (SMALLVAR(leng))
	    {
	      pralign(typealign[type]);
	      fprintf(initfile, labelfmt, sname);
	      prspace(leng);
	    }
	  else
	    {
	      preven(ALIDOUBLE);
	      prlocvar(sname, leng);
	      *inlcomm = YES;
	    }
	}
      else
	{
	  fprintf(initfile, labelfmt, sname);
	  if (leng == 8)
	    prquad(oldvalue);
	  else
	    prsdata(oldvalue, leng);
	}
      return;
    }

  repl = 1;
  k -= 8;

  while (k >=8)
    {
      newvalue[0] = 0;
      newvalue[1] = 0;

      n = read(vdatafile, newvalue, 8);
      if (n < 0)
	{
	  err(readerror);
	  done(1);
	}

      k -= 8;

      if (oldvalue[0] == newvalue[0]
	  && oldvalue[1] == newvalue[1])
	repl++;
      else
	{
	  if (first == YES)
	    {
	      pralign(typealign[type]);
	      fprintf(initfile, labelfmt, sname);
	      first = NO;
	    }

	  if (oldvalue[0] == 0
	      && oldvalue[1] == 0)
	    prspace(8*repl);
	  else
	    {
	      while (repl-- > 0)
		prquad(oldvalue);
	    }
	  oldvalue[0] = newvalue[0];
	  oldvalue[1] = newvalue[1];
	  repl = 1;
	}
    }

  newvalue[0] = 0;
  newvalue[1] = 0;

  if (k > 0)
    {
      n = read(vdatafile, newvalue, k);
      if (n < 0)
	{
	  err(readerror);
	  done(1);
	}
    }

  if (oldvalue[1] == 0
      && oldvalue[0] == 0
      && newvalue[1] == 0
      && newvalue[0] == 0)
    {
      if (first == YES && !SMALLVAR(leng))
	{
	  prlocvar(sname, leng);
	  *inlcomm = YES;
	}
      else
	{
	  if (first == YES)
	    {
	      pralign(typealign[type]);
	      fprintf(initfile, labelfmt, sname);
	    }
	  prspace(8*repl + k);
	}
      return;
    }

  if (first == YES)	
    {
      pralign(typealign[type]);
      fprintf(initfile, labelfmt, sname);
    }

  if (oldvalue[1] == 0
      && oldvalue[0] == 0)
    	prspace(8*repl);
  else
    {
      while (repl-- > 0)
	prquad(oldvalue);
    }

  prsdata(newvalue, k);

  return;
}

prendproc()
{
}

prtail()
{
}

prolog(ep, argvec)
struct Entrypoint *ep;
Addrp  argvec;
{
int i, argslot, proflab;
int size;
register chainp p;
register Namep q;
register struct Dimblock *dp;
expptr tp;
static char maskfmt[] = "\t.word\tLWM%d";
static char align1fmt[] = "\t.align\t1";

if(procclass == CLMAIN) {
	if(fudgelabel)
		{
		if(ep->entryname) {
			p2pass(align1fmt);
			p2ps("_%s:",  varstr(XL, ep->entryname->extname));
			p2pi(maskfmt, procno);
		}
		putlabel(fudgelabel);
		fudgelabel = 0;
		}
	else
		{
		p2pass(align1fmt);
		p2pass( "_MAIN_:" );
		if(ep->entryname == NULL)
			p2pi(maskfmt, procno);
		}

} else if(ep->entryname)
	if(fudgelabel)
		{
		putlabel(fudgelabel);
		fudgelabel = 0;
		}
	else
		{
		p2pass(align1fmt);
		p2ps("_%s:",  varstr(XL, ep->entryname->extname));
		p2pi(maskfmt, procno);
		prsave(newlabel());
		}

if(procclass == CLBLOCK)
	return;
if (anylocals == YES)
	p2pi("\tmovl\t$v.%d,r11", bsslabel);
if(argvec)
	{
	if (argvec->tag != TADDR) badtag ("prolog",argvec->tag);
	argloc = argvec->memoffset->constblock.constant.ci + SZINT;
			/* first slot holds count */
	if(proctype == TYCHAR)
		{
		mvarg(TYADDR, 0, chslot);
		mvarg(TYLENG, SZADDR, chlgslot);
		argslot = SZADDR + SZLENG;
		}
	else if( ISCOMPLEX(proctype) )
		{
		mvarg(TYADDR, 0, cxslot);
		argslot = SZADDR;
		}
	else
		argslot = 0;

	for(p = ep->arglist ; p ; p =p->nextp)
		{
		q = (Namep) (p->datap);
		mvarg(TYADDR, argslot, q->vardesc.varno);
		argslot += SZADDR;
		}
	for(p = ep->arglist ; p ; p = p->nextp)
		{
		q = (Namep) (p->datap);
		if(q->vtype==TYCHAR && q->vclass!=CLPROC)
			{
			if(q->vleng && ! ISCONST(q->vleng) )
				mvarg(TYLENG, argslot,
					q->vleng->addrblock.memno);
			argslot += SZLENG;
			}
		}
	if ((ep->enamep->vtype == TYCOMPLEX) && (!ep->arglist))
		p2pass("\tmovl\tfp,r12");
	else
		p2pi("\tsubl3\t$%d,fp,r12", ARGOFFSET-argloc);
	} else 
	if((ep->arglist) || (ISCOMPLEX(proctype)) || (proctype == TYCHAR))
		p2pass("\tmovl\tfp,r12");

for(p = ep->arglist ; p ; p = p->nextp)
	{
	q = (Namep) (p->datap);
	if(dp = q->vdim)
		{
		for(i = 0 ; i < dp->ndim ; ++i)
			if(dp->dims[i].dimexpr)
				puteq( fixtype(cpexpr(dp->dims[i].dimsize)),
					fixtype(cpexpr(dp->dims[i].dimexpr)));
#ifdef SDB
                if(sdbflag) {
		for(i = 0 ; i < dp->ndim ; ++i) {
			if(dp->dims[i].lbaddr)
				puteq( fixtype(cpexpr(dp->dims[i].lbaddr)),
					fixtype(cpexpr(dp->dims[i].lb)));
			if(dp->dims[i].ubaddr)
				puteq( fixtype(cpexpr(dp->dims[i].ubaddr)),
					fixtype(cpexpr(dp->dims[i].ub)));
		    
                                                }
                            }
#endif
		size = typesize[ q->vtype ];
		if(q->vtype == TYCHAR)
			if( ISICON(q->vleng) )
				size *= q->vleng->constblock.constant.ci;
			else
				size = -1;

		/* on TAHOE, get more efficient subscripting if subscripts
		   have zero-base, so fudge the argument pointers for arrays.
		   Not done if array bounds are being checked.
		*/
		if(dp->basexpr)
			puteq( 	cpexpr(fixtype(dp->baseoffset)),
				cpexpr(fixtype(dp->basexpr)));
#ifdef SDB
		if( (! checksubs) && (! sdbflag) )
#else
		if(! checksubs)
#endif
			{
			if(dp->basexpr)
				{
				if(size > 0)
					tp = (expptr) ICON(size);
				else
					tp = (expptr) cpexpr(q->vleng);
				putforce(TYINT,
					fixtype( mkexpr(OPSTAR, tp,
						cpexpr(dp->baseoffset)) ));
				p2pi("\tsubl2\tr0,%d(r12)",
					p->datap->nameblock.vardesc.varno +
						ARGOFFSET);
				}
			else if(dp->baseoffset->constblock.constant.ci != 0)
				{
				if(size > 0)
					{
					p2pij("\tsubl2\t$%ld,%d(r12)",
						dp->baseoffset->constblock.constant.ci * size,
						p->datap->nameblock.vardesc.varno +
							ARGOFFSET);
					}
				else	{
					putforce(TYINT, mkexpr(OPSTAR, cpexpr(dp->baseoffset),
						cpexpr(q->vleng) ));
					p2pi("\tsubl2\tr0,%d(r12)",
						p->datap->nameblock.vardesc.varno +
							ARGOFFSET);
					}
				}
			}
		}
	}

if(typeaddr)
	puteq( cpexpr(typeaddr), mkaddcon(ep->typelabel) );
/* replace to avoid long jump problem
putgoto(ep->entrylabel);
*/
p2pi("\tjbr\tL%d", ep->entrylabel);
}

prhead(fp)
FILEP fp;
{
#if FAMILY==PCC
	p2triple(PCCF_FLBRAC, ARGREG-highregvar, procno);
	p2word( (long) (BITSPERCHAR*autoleng) );
	p2flush();
#endif
}
