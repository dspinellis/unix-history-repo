#include "defs"
#if FAMILY == DMR
#	include "dmrdefs"
#endif
#if FAMILY==SCJ && OUTPUT==BINARY
#	include "scjdefs"
#endif

/*
      PDP 11-SPECIFIC PRINTING ROUTINES
*/

int maxregvar = 0;
static char textline[50];
int regnum[] = { 3, 2 };


prsave()
{
}



goret(type)
int type;
{
#if  FAMILY == DMR
	p2op(P2RETURN);
#endif
#if FAMILY==SCJ
	p2pass(sprintf(textline, "\tjmp\tcret"));
#endif
}




/*
 * move argument slot arg1 (relative to ap)
 * to slot arg2 (relative to ARGREG)
 */

mvarg(type, arg1, arg2)
int type, arg1, arg2;
{
mvarg1(arg1+4, arg2);
if(type == TYLONG)
	mvarg1(arg1+6, arg2+2);
}




mvarg1(m, n)
int m, n;
{
#if FAMILY == DMR
	p2reg(ARGREG, P2SHORT|P2PTR);
	p2op2(P2ICON, P2SHORT);
	p2i(n);
	p2op2(P2PLUS, P2SHORT|P2PTR);
	p2op2(P2INDIRECT, P2SHORT);
	p2reg(AUTOREG, P2SHORT|P2PTR);
	p2op2(P2ICON, P2SHORT);
	p2i(m);
	p2op2(P2PLUS, P2SHORT|P2PTR);
	p2op2(P2INDIRECT, P2SHORT);
	p2op2(P2ASSIGN, P2SHORT);
	putstmt();
#endif
#if FAMILY == SCJ
	p2pass(sprintf(textline, "\tmov\t%d.(r5),%d.(r4)", m, n));
#endif
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
register int *np;
np = &n;
if(type == TYLONG)
	fprintf(fp, "\t%d.;%d.\n", np[0], np[1]);
else
	fprintf(fp, "\t%d.\n", np[1]);
}



prcona(fp, a)
FILEP fp;
ftnint a;
{
fprintf(fp, "L%ld\n", a);
}



#if HERE!=PDP11
BAD NEWS
#endif

#if HERE==PDP11
prconr(fp, type, x)
FILEP fp;
int type;
double x;
{
register int k, *n;
n = &x;	/* nonportable cheat */
k = (type==TYREAL ? 2 : 4);
fprintf(fp, "\t");
while(--k >= 0)
	fprintf(fp, "%d.%c", *n++, (k==0 ? '\n' : ';') );
}
#endif




preven(k)
int k;
{
if(k > 1)
	fprintf(asmfile, "\t.even\n", k);
}



#if FAMILY == SCJ

prcmgoto(p, nlab, skiplabel, labarray)
expptr p;
int nlab, skiplabel, labarray;
{
int regno;

putforce(p->vtype, p);

if(p->vtype == TYLONG)
	{
	regno = 1;
	p2pass(sprintf(textline, "\ttst\tr0"));
	p2pass(sprintf(textline, "\tbne\tL%d", skiplabel));
	}
else
	regno = 0;

p2pass(sprintf(textline, "\tcmp\tr%d,$%d.", regno, nlab));
p2pass(sprintf(textline, "\tbhi\tL%d", skiplabel));
p2pass(sprintf(textline, "\tasl\tr%d", regno));
p2pass(sprintf(textline, "\tjmp\t*L%d(r%d)", labarray, regno));
}


prarif(p, neg,zer,pos)
expptr p;
int neg, zer, pos;
{
register int ptype;

putforce( ptype = p->vtype, p);
if( ISINT(ptype) )
	{
	p2pass(sprintf(textline, "\ttst\tr0"));
	p2pass(sprintf(textline, "\tjlt\tL%d", neg));
	p2pass(sprintf(textline, "\tjgt\tL%d", pos));
	if(ptype != TYSHORT)
		{
		p2pass(sprintf(textline, "\ttst\tr1"));
		p2pass(sprintf(textline, "\tjeq\tL%d", zer));
		}
	p2pass(sprintf(textline, "\tjbr\tL%d", pos));
	}
else
	{
	p2pass(sprintf(textline, "\ttstf\tr0"));
	p2pass(sprintf(textline, "\tcfcc"));
	p2pass(sprintf(textline, "\tjeq\tL%d", zer));
	p2pass(sprintf(textline, "\tjlt\tL%d", neg));
	p2pass(sprintf(textline, "\tjmp\tL%d", pos));
	}
}

#endif




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
		sprintf(s, "q.%d", mem);
		break;

	default:
		fatal1("memname: invalid vstg %d", stg);
	}
return(s);
}


prlocvar(s, len)
char *s;
ftnint len;
{
fprintf(asmfile, "%s:", s);
prskip(asmfile, len);
}



prext(name, leng, init)
char *name;
ftnint leng;
int init;
{
if(leng==0 || init)
	fprintf(asmfile, "\t.globl\t_%s\n", name);
else
	fprintf(asmfile, "\t.comm\t_%s,%ld.\n", name, leng);
}



prendproc()
{
}



prtail()
{
#if FAMILY == SCJ
	p2pass(sprintf(textline, "\t.globl\tcsv,cret"));
#else
	p2op(P2EOF);
#endif
}



prolog(ep, argvec)
struct entrypoint *ep;
struct addrblock *argvec;
{
int i, argslot, proflab;
register chainp p;
register struct nameblock *q;
register struct dimblock *dp;
struct constblock *mkaddcon();

if(procclass == CLMAIN)
	prentry("MAIN__");

if(ep->entryname)
	prentry( varstr(XL, ep->entryname->extname) );

if(procclass == CLBLOCK)
	return;
if(profileflag)
	proflab = newlabel();
#if FAMILY == SCJ
	if(profileflag)
		{
		fprintf(asmfile, "L%d:\t. = .+2\n", proflab);
		p2pass(sprintf(textline, "\tmov\t$L%d,r0", proflab));
		p2pass(sprintf(textline, "\tjsr\tpc,mcount"));
		}
	p2pass(sprintf(textline, "\tjsr\tr5,csv"));
	p2pass(sprintf(textline, "\tsub\t$.F%d,sp", procno));
#else
	if(profileflag)
		p2op2(P2PROFILE, proflab);
	p2op(P2SAVE);
	p2op2(P2SETSTK, ( (((int) autoleng)+1) & ~01) );
#endif

if(argvec == NULL)
	addreg(argloc = 4);
else
	{
	addreg( argloc = argvec->memoffset->const.ci );
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
		q = p->datap;
		mvarg(TYADDR, argslot, q->vardesc.varno);
		argslot += SZADDR;
		}
	for(p = ep->arglist ; p ; p = p->nextp)
		{
		q = p->datap;
		if(q->vtype==TYCHAR || q->vclass==CLPROC)
			{
			if( q->vleng && ! ISCONST(q->vleng) )
				mvarg(TYLENG, argslot, q->vleng->memno);
			argslot += SZLENG;
			}
		}
	}

for(p = ep->arglist ; p ; p = p->nextp)
	if(dp = ( (struct nameblock *) (p->datap) ) ->vdim)
		{
		for(i = 0 ; i < dp->ndim ; ++i)
			if(dp->dims[i].dimexpr)
				puteq( fixtype(cpexpr(dp->dims[i].dimsize)),
					fixtype(cpexpr(dp->dims[i].dimexpr)));
		if(dp->basexpr)
			puteq( 	cpexpr(fixtype(dp->baseoffset)),
				cpexpr(fixtype(dp->basexpr)));
		}

if(typeaddr)
	puteq( cpexpr(typeaddr), mkaddcon(ep->typelabel) );
putgoto(ep->entrylabel);
}



prentry(s)
char *s;
{
#if FAMILY == SCJ
	p2pass(sprintf(textline, "_%s:", s));
#else
	p2op(P2RLABEL);
	putc('_', textfile);
	p2str(s);
#endif
}




addreg(k)
int k;
{
#if FAMILY == SCJ
	p2pass(sprintf(textline, "\tmov\tr5,r4"));
	p2pass(sprintf(textline, "\tadd\t$%d.,r4", k));
#else
	p2reg(ARGREG, P2SHORT);
	p2reg(AUTOREG, P2SHORT);
	p2op2(P2ICON, P2SHORT);
	p2i(k);
	p2op2(P2PLUS, P2SHORT);
	p2op2(P2ASSIGN, P2SHORT);
	putstmt();
#endif
}





prhead(fp)
FILEP fp;
{
#if FAMILY==SCJ
#	if OUTPUT == BINARY
		p2triple(P2LBRACKET, ARGREG-1-highregvar, procno);
		p2word( (long) (BITSPERCHAR*autoleng) );
		p2flush();
#	else
		fprintf(fp, "[%02d\t%06ld\t%02d\t\n", procno,
			BITSPERCHAR*autoleng, ARGREG-1-highregvar);
#	endif
#endif
}

prdbginfo()
{
register char *s;
char *t, buff[50];
register struct nameblock *p;
struct hashentry *hp;

if(s = entries->entryname->extname)
	s = varstr(XL, s);
else if(procclass == CLMAIN)
	s = "MAIN__";
else
	return;

if(procclass != CLBLOCK)
	fprintf(asmfile, "~~%s = _%s\n", s, s);

for(hp = hashtab ; hp<lasthash ; ++hp)
    if(p = hp->varp)
	{
	s = NULL;
	if(p->vstg == STGARG)
		s = sprintf(buff, "%o", p->vardesc.varno+argloc);
	else if(p->vclass == CLVAR)
		switch(p->vstg)
			{
			case STGBSS:
			case STGINIT:
			case STGEQUIV:
				t = memname(p->vstg, p->vardesc.varno);
				if(p->voffset)
					s = sprintf(buff, "%s+%o", t, p->voffset);
				else
					s = sprintf(buff, "%s", t);
				break;

			case STGAUTO:
				s = sprintf(buff, "%o", p->voffset);
				break;

			default:
				break;
			}
	if(s)
		fprintf(asmfile, "~%s = %s\n", varstr(VL,p->varname), s);
	}
fprintf(asmfile, "~~:\n");
}
