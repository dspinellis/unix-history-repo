#include "defs.h"

#ifdef SDB
#	include <a.out.h>
extern int types2[];
#	ifndef N_SO
#		include <stab.h>
#	endif
#endif

#include "pccdefs.h"


/*
	VAX-11/780 - SPECIFIC ROUTINES
*/


int maxregvar = MAXREGVAR;
int regnum[] =  { 10, 9, 8, 7, 6 } ;
static int regmask[] = { 0x800, 0xc00, 0xe00, 0xf00, 0xf80, 0xfc0 };



ftnint intcon[14] =
	{ 2, 2, 2, 2,
	  15, 31, 24, 56,
	  -128, -128, 127, 127,
	  32767, 2147483647 };

#if HERE == VAX
	/* then put in constants in octal */
long realcon[6][2] =
	{
		{ 0200, 0 },
		{ 0200, 0 },
		{ 037777677777, 0 },
		{ 037777677777, 037777777777 },
		{ 032200, 0 },
		{ 022200, 0 }
	};
#else
double realcon[6] =
	{
	2.9387358771e-39,
	2.938735877055718800e-39
	1.7014117332e+38,
	1.701411834604692250e+38
	5.960464e-8,
	1.38777878078144567e-17,
	};
#endif




prsave(proflab)
int proflab;
{
if(profileflag)
	{
	fprintf(asmfile, "L%d:\t.space\t4\n", proflab);
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
fprintf(fp, "\t%s\t%ld\n", (type==TYSHORT ? ".word" : ".long"), n);
}



prcona(fp, a)
FILEP fp;
ftnint a;
{
fprintf(fp, "\t.long\tL%ld\n", a);
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
cheat.xd = x;
if(type == TYREAL)
	{float y = x; fprintf(fp, "\t.long\t0x%X\n", *(long *) &y); }
else
	fprintf(fp, "\t.long\t0x%X,0x%X\n", cheat.xl[0], cheat.xl[1]);
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
  return;
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
      *((short *) shrt) = (short) cp->const.ci;
      return (shrt);

    case TYLONG:
    case TYLOGICAL:
    case TYREAL:
      *((int *) lng) = cp->const.ci;
      return (lng);

    case TYDREAL:
      ip = (int *) quad;
      jp = (int *) &(cp->const.cd[0]);
      ip[0] = jp[0];
      ip[1] = jp[1];
      return (quad);

    case TYCOMPLEX:
      ip = (int *) quad;
      jp = (int *) &(cp->const.cd[0]);
      ip[0] = jp[0];
      ip[1] = jp[2];
      return (quad);

    case TYDCOMPLEX:
      ip = (int *) oct;
      jp = (int *) &(cp->const.cd[0]);
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
  static char *spacefmt = "\t.space\t%d\n";
  static char *align2fmt = "\t.align\t2\n";
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
  fprintf(initfile, align2fmt);
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
	fprintf(initfile, spacefmt, leng);
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
	    fprintf(initfile, spacefmt, 8*repl);
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
      fprintf(initfile, spacefmt, 8*repl + leng);
      return;
    }

  if (*((int *) (oldvalue + 4)) == 0
      && *((int *) oldvalue) == 0)
    fprintf(initfile, spacefmt, 8*repl);
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
  static char *spacefmt = "\t.space\t%d\n";

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
	      fprintf(initfile, spacefmt, leng);
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
	    fprintf(initfile, spacefmt, 8*repl);
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
	  fprintf(initfile, spacefmt, 8*repl + k);
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
    fprintf(initfile, spacefmt, 8*repl);
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
	argloc = argvec->memoffset->constblock.const.ci + SZINT;
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
				size *= q->vleng->constblock.const.ci;
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
			else if(dp->baseoffset->constblock.const.ci != 0)
				{
				char buff[25];
				if(size > 0)
					{
					sprintf(buff, "\tsubl2\t$%ld,%d(ap)",
						dp->baseoffset->constblock.const.ci * size,
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
p2pi("\tjmp\tL%d", ep->entrylabel);
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
	p2triple(P2LBRACKET, ARGREG-highregvar, procno);
	p2word( (long) (BITSPERCHAR*autoleng) );
	p2flush();
#endif
}
