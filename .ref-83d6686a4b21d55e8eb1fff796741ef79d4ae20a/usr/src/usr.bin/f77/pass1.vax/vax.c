/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)vax.c	5.3 (Berkeley) %G%";
#endif not lint

/*
 * vax.c
 *
 * VAX specific routines for the F77 compiler, pass 1
 *
 * University of Utah CS Dept modification history:
 *
 * $Log:	vax.c,v $
 * Revision 5.2  85/08/10  05:06:30  donn
 * Deleted intcon[] and realcon[], since they are now made redundant by
 * changes in intr.c.  From Jerry Berkman.
 * 
 * Revision 5.1  85/08/10  03:50:38  donn
 * 4.3 alpha
 * 
 * Revision 3.1  85/02/27  19:14:58  donn
 * Changed to use pcc.h instead of pccdefs.h.
 * 
 * Revision 2.3  85/02/22  01:09:22  donn
 * memname() didn't know about intrinsic functions...
 * 
 * Revision 2.2  85/02/12  17:56:44  donn
 * Put the argument to the profiling routine in data space instead of
 * constant space.  From Jerry Berkman.
 * 
 * Revision 2.1  84/07/19  12:05:08  donn
 * Changed comment headers for UofU.
 * 
 * Revision 1.2  84/02/26  06:41:04  donn
 * Added Berkeley changes to move data around to produce shorter offsets.
 * 
 */

#include "defs.h"

#ifdef SDB
#	include <a.out.h>
extern int types2[];
#	ifndef N_SO
#		include <stab.h>
#	endif
#endif

#include <pcc.h>



int maxregvar = MAXREGVAR;
int regnum[] =  { 10, 9, 8, 7, 6 } ;
static int regmask[] = { 0x800, 0xc00, 0xe00, 0xf00, 0xf80, 0xfc0 };


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
	pruse(asmfile, USEINIT);	/* This is not a constant */
	fprintf(asmfile, "L%d:\t.space\t4\n", proflab);
	pruse(asmfile, USECONST);
	p2pi("\tmovab\tL%d,r0", proflab);
	p2pass("\tjsb\tmcount");
	}
p2pi("\tsubl2\t$LF%d,sp", procno);
}



goret(type)
int type;
{
p2pass("\tret");
}




/*
 * move argument slot arg1 (relative to ap)
 * to slot arg2 (relative to ARGREG)
 */

mvarg(type, arg1, arg2)
int type, arg1, arg2;
{
p2pij("\tmovl\t%d(ap),%d(fp)", arg1+ARGOFFSET, arg2+argloc);
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



#ifndef vax
prconr(fp, type, x)
FILEP fp;
int type;
float x;
{
fprintf(fp, "\t%s\t0f%e\n", (type==TYREAL ? ".float" : ".double"), x);
}
#endif

#ifdef vax
prconr(fp, type, x)
FILEP fp;
int type;
double x;
{
/* non-portable cheat to preserve bit patterns */
union { double xd; long int xl[2]; } cheat;
register int i;

cheat.xd = x;
if(type == TYREAL)
	{
	float y = x;

	fprintf(fp, "\t.long\t0x%X\n", *(long *) &y);
	i = SZFLOAT;
	}
else
	{
	fprintf(fp, "\t.long\t0x%X,0x%X\n", cheat.xl[0], cheat.xl[1]);
	i = SZDOUBLE;
	}
if(fp == initfile)
	i_offset += i;
else
	a_offset += i;
}
#endif



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
register int lg = 0;

if(k > 4)
	{
	if(i_offset & 7)
		lg = 8 - (i_offset & 7);
	}
else if(k > 2)
	{
	if(i_offset & 3)
		lg = 4 - (i_offset & 3);
	}
else if(k > 1)
	{
	if(i_offset & 1)
		lg = 1;
	}
else
	return;
if(lg > 0)
	{
	fprintf(initfile, "\t.space\t%d\n", lg);
	i_offset += lg;
	}
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
register int lg = 0;

if(k > 4)
	{
	if(a_offset & 7)
		lg = 8 - (a_offset & 7);
	}
else if(k > 2)
	{
	if(a_offset & 3)
		lg = 4 - (a_offset & 3);
	}
else if(k > 1)
	{
	if(a_offset & 1)
		lg = 1;
	}
else
	return;
if(lg > 0)
	{
	fprintf(asmfile, "\t.space\t%d\n", lg);
	a_offset += lg;
	}
}



praspace(n)
int n;
{

fprintf(asmfile, "\t.space\t%d\n", n);
a_offset += n;
}



vaxgoto(index, nlab, labs)
expptr index;
register int nlab;
struct Labelblock *labs[];
{
register int i;
register int arrlab;

putforce(TYINT, index);
p2pi("\tcasel\tr0,$1,$%d", nlab-1);
p2pi("L%d:", arrlab = newlabel() );
for(i = 0; i< nlab ; ++i)
	if( labs[i] )
		p2pij("\t.word\tL%d-L%d", labs[i]->labelno, arrlab);
}


prarif(p, neg, zer, pos)
expptr p;
int neg, zer, pos;
{
int type;

type = p->headblock.vtype;
putforce(type, p);
if(type == TYLONG)
	p2pass("\ttstl\tr0");
else if (type == TYSHORT)
	p2pass("\ttstw\tr0");
else
	p2pass("\ttstd\tr0");
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
	case STGCOMMON:
	case STGEXT:
	case STGINTR:
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
fprintf(asmfile, "\t.lcomm\t%s,%ld\n", s, len);
}




char *
packbytes(cp)
register Constp cp;
{
  static char shrt[2];
  static char lng[4];
  static char quad[8];
  static char oct[16];

  register int type;
  register int *ip, *jp;

  switch (cp->vtype)
    {
    case TYSHORT:
      *((short *) shrt) = (short) cp->constant.ci;
      return (shrt);

    case TYLONG:
    case TYLOGICAL:
    case TYREAL:
      *((int *) lng) = cp->constant.ci;
      return (lng);

    case TYDREAL:
      ip = (int *) quad;
      jp = (int *) &(cp->constant.cd[0]);
      ip[0] = jp[0];
      ip[1] = jp[1];
      return (quad);

    case TYCOMPLEX:
      ip = (int *) quad;
      jp = (int *) &(cp->constant.cd[0]);
      ip[0] = jp[0];
      ip[1] = jp[2];
      return (quad);

    case TYDCOMPLEX:
      ip = (int *) oct;
      jp = (int *) &(cp->constant.cd[0]);
      *ip++ = *jp++;
      *ip++ = *jp++;
      *ip++ = *jp++;
      *ip = *jp;
      return (oct);

    default:
      badtype("packbytes", cp->vtype);
    }
}




prsdata(s, len)
register char *s;
register int len;
{
  static char *longfmt = "\t.long\t0x%x\n";
  static char *wordfmt = "\t.word\t0x%x\n";
  static char *bytefmt = "\t.byte\t0x%x\n";

  register int i;

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
char *s;
{
  static char *quadfmt1 = "\t.quad\t0x%x\n";
  static char *quadfmt2 = "\t.quad\t0x%x%08x\n";

  if ( *((int *) (s + 4)) == 0 )
    fprintf(initfile, quadfmt1, *((int *) s));
  else
    fprintf(initfile, quadfmt2, *((int *) (s + 4)), *((int *) s));

  i_offset += 8;
  return;
}



#ifdef NOTDEF

/*  The code for generating .fill directives has been      */
/*  ifdefed out because of bugs in the UCB VAX assembler.  */
/*  If those bugs are ever fixed (and it seems unlikely),  */
/*  the NOTDEF's should be replaced by UCBVAXASM.          */


prfill(n, s)
int n;
register char *s;
{
  static char *fillfmt1 = "\t.fill\t%d,8,0x%x\n";
  static char *fillfmt2 = "\t.fill\t%d,8,0x%x%08x\n";

  if (*((int *) (s + 4)) == 0)
    fprintf(initfile, fillfmt1, n, *((int *) s));
  else
    fprintf(initfile, fillfmt2, n, *((int *) (s + 4)), *((int *) s));

  return;
}

#endif



prext(ep)
register struct Extsym *ep;
{
  static char *globlfmt = "\t.globl\t_%s\n";
  static char *commfmt = "\t.comm\t_%s,%ld\n";
  static char *labelfmt = "_%s:\n";

  static char *seekerror = "seek error on tmp file";
  static char *readerror = "read error on tmp file";

  char *tag;
  register int leng;
  long pos;
  register int i;
  char oldvalue[8];
  char newvalue[8];
  register int n;
  register int repl;

  tag = varstr(XL, ep->extname);
  leng = ep->maxleng;

  if (leng == 0)
    {
      fprintf(asmfile, globlfmt, tag);
      return;
    }

  if (ep->init == NO)
    {
      fprintf(asmfile, commfmt, tag, leng);
      return;
    }

  fprintf(asmfile, globlfmt, tag);
  pralign(ALIDOUBLE);
  fprintf(initfile, labelfmt, tag);

  pos = lseek(cdatafile, ep->initoffset, 0);
  if (pos == -1)
    {
      err(seekerror);
      done(1);
    }

  *((int *) oldvalue) = 0;
  *((int *) (oldvalue + 4)) = 0;
  n = read(cdatafile, oldvalue, 8);
  if (n < 0)
    {
      err(readerror);
      done(1);
    }

  if (leng <= 8)
    {
      i = leng;
      while (i > 0 && oldvalue[--i] == '\0') /* SKIP */;
      if (oldvalue[i] == '\0')
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
      *((int *) newvalue) = 0;
      *((int *) (newvalue + 4)) = 0;

      n = read(cdatafile, newvalue, 8);
      if (n < 0)
	{
	  err(readerror);
	  done(1);
	}

      leng -= 8;

      if (*((int *) oldvalue) == *((int *) newvalue)
	  && *((int *) (oldvalue + 4)) == *((int *) (newvalue + 4)))
	repl++;
      else
	{
	  if (*((int *) oldvalue) == 0
	      && *((int *) (oldvalue + 4)) == 0)
	    prspace(8*repl);
	  else if (repl == 1)
	    prquad(oldvalue);
	  else
#ifdef NOTDEF
	    prfill(repl, oldvalue);
#else
	    {
	      while (repl-- > 0)
		prquad(oldvalue);
	    }
#endif
	  *((int *) oldvalue) = *((int *) newvalue);
	  *((int *) (oldvalue + 4)) = *((int *) (newvalue + 4));
	  repl = 1;
	}
    }

  *((int *) newvalue) = 0;
  *((int *) (newvalue + 4)) = 0;

  if (leng > 0)
    {
      n = read(cdatafile, newvalue, leng);
      if (n < 0)
	{
	  err(readerror);
	  done(1);
	}
    }

  if (*((int *) (oldvalue + 4)) == 0
      && *((int *) oldvalue) == 0
      && *((int *) (newvalue + 4)) == 0
      && *((int *) newvalue) == 0)
    {
      prspace(8*repl + leng);
      return;
    }

  if (*((int *) (oldvalue + 4)) == 0
      && *((int *) oldvalue) == 0)
    prspace(8*repl);
  else if (repl == 1)
    prquad(oldvalue);
  else
#ifdef NOTDEF
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
  static char *seekerror = "seek error on tmp file";
  static char *readerror = "read error on tmp file";

  static char *labelfmt = "%s:\n";

  register int k;
  register int i;
  register int repl;
  register int first;
  register long pos;
  register long n;
  char oldvalue[8];
  char newvalue[8];

  *inlcomm = NO;

  k = leng;
  first = YES;

  pos = lseek(vdatafile, initoffset, 0);
  if (pos == -1)
    {
      err(seekerror);
      done(1);
    }

  *((int *) oldvalue) = 0;
  *((int *) (oldvalue + 4)) = 0;
  n = read(vdatafile, oldvalue, 8);
  if (n < 0)
    {
      err(readerror);
      done(1);
    }

  if (k <= 8)
    {
      i = k;
      while (i > 0 && oldvalue[--i] == '\0')
	/*  SKIP  */ ;
      if (oldvalue[i] == '\0')
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
      *((int *) newvalue) = 0;
      *((int *) (newvalue + 4)) = 0;

      n = read(vdatafile, newvalue, 8);
      if (n < 0)
	{
	  err(readerror);
	  done(1);
	}

      k -= 8;

      if (*((int *) oldvalue) == *((int *) newvalue)
	  && *((int *) (oldvalue + 4)) == *((int *) (newvalue + 4)))
	repl++;
      else
	{
	  if (first == YES)
	    {
	      pralign(typealign[type]);
	      fprintf(initfile, labelfmt, sname);
	      first = NO;
	    }

	  if (*((int *) oldvalue) == 0
	      && *((int *) (oldvalue + 4)) == 0)
	    prspace(8*repl);
	  else
	    {
	      while (repl-- > 0)
		prquad(oldvalue);
	    }
	  *((int *) oldvalue) = *((int *) newvalue);
	  *((int *) (oldvalue + 4)) = *((int *) (newvalue + 4));
	  repl = 1;
	}
    }

  *((int *) newvalue) = 0;
  *((int *) (newvalue + 4)) = 0;

  if (k > 0)
    {
      n = read(vdatafile, newvalue, k);
      if (n < 0)
	{
	  err(readerror);
	  done(1);
	}
    }

  if (*((int *) (oldvalue + 4)) == 0
      && *((int *) oldvalue) == 0
      && *((int *) (newvalue + 4)) == 0
      && *((int *) newvalue) == 0)
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

  if (*((int *) (oldvalue + 4)) == 0
      && *((int *) oldvalue) == 0)
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

p2pass("\t.align\t1");


if(procclass == CLMAIN) {
	if(fudgelabel)
		{
		if(ep->entryname) {
			p2ps("_%s:",  varstr(XL, ep->entryname->extname));
			p2pi("\t.word\tLWM%d", procno);
		}
		putlabel(fudgelabel);
		fudgelabel = 0;
		fixlwm();
		}
	else
		{
		p2pass( "_MAIN_:" );
		if(ep->entryname == NULL)
			p2pi("\t.word\tLWM%d", procno);
		}

} else if(ep->entryname)
	if(fudgelabel)
		{
		putlabel(fudgelabel);
		fudgelabel = 0;
		fixlwm();
		}
	else
		{
		p2ps("_%s:",  varstr(XL, ep->entryname->extname));
		p2pi("\t.word\tLWM%d", procno);
		prsave(newlabel());
		}

if(procclass == CLBLOCK)
	return;
if (anylocals == YES)
	{
	char buff[30];
	sprintf(buff, "\tmovl\t$v.%d,r11", bsslabel);
	p2pass(buff);
	}
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
	p2pi("\taddl3\t$%d,fp,ap", argloc-ARGOFFSET);
	p2pi("\tmovl\t$%d,(ap)\n", lastargslot/SZADDR);
	}

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

		/* on VAX, get more efficient subscripting if subscripts
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
				p2pi("\tsubl2\tr0,%d(ap)",
					p->datap->nameblock.vardesc.varno +
						ARGOFFSET);
				}
			else if(dp->baseoffset->constblock.constant.ci != 0)
				{
				char buff[25];
				if(size > 0)
					{
					sprintf(buff, "\tsubl2\t$%ld,%d(ap)",
						dp->baseoffset->constblock.constant.ci * size,
						p->datap->nameblock.vardesc.varno +
							ARGOFFSET);
					}
				else	{
					putforce(TYINT, mkexpr(OPSTAR, cpexpr(dp->baseoffset),
						cpexpr(q->vleng) ));
					sprintf(buff, "\tsubl2\tr0,%d(ap)",
						p->datap->nameblock.vardesc.varno +
							ARGOFFSET);
					}
				p2pass(buff);
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

fixlwm()
{
	extern lwmno;
	if (lwmno == procno)
		return;
	fprintf(asmfile, "\t.set\tLWM%d,0x%x\n",
		procno, regmask[highregvar]);
	lwmno = procno;
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
