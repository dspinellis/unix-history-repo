/* INTERMEDIATE CODE GENERATION FOR S C JOHNSON C COMPILERS */
/* NEW VERSION USING BINARY POLISH POSTFIX INTERMEDIATE */
#if FAMILY != SCJ
	WRONG put FULE !!!!
#endif

#include "defs"
#include "scjdefs"

#define FOUR 4
extern int ops2[];
extern int types2[];

#define P2BUFFMAX 128
static long int p2buff[P2BUFFMAX];
static long int *p2bufp		= &p2buff[0];
static long int *p2bufend	= &p2buff[P2BUFFMAX];


puthead(s)
char *s;
{
char buff[100];
#if TARGET == VAX
	if(s)
		p2pass( sprintf(buff, "\t.globl\t_%s", s) );
#endif
/* put out fake copy of left bracket line, to be redone later */
if( ! headerdone )
	{
#if FAMILY==SCJ && OUTPUT==BINARY
	p2flush();
#endif
	headoffset = ftell(textfile);
	prhead(textfile);
	headerdone = YES;
	p2triple(P2STMT, (strlen(infname)+FOUR-1)/FOUR, 0);
	p2str(infname);
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

#if FAMILY==SCJ && OUTPUT==BINARY
	p2flush();
#endif
hereoffset = ftell(textfile);
if(fseek(textfile, headoffset, 0))
	fatal("fseek failed");
prhead(textfile);
if(fseek(textfile, hereoffset, 0))
	fatal("fseek failed 2");
}




putrbrack(k)
int k;
{
p2op(P2RBRACKET, k);
}



putnreg()
{
}






puteof()
{
p2op(P2EOF, 0);
p2flush();
}



putstmt()
{
p2triple(P2STMT, 0, lineno);
}




/* put out code for if( ! p) goto l  */
putif(p,l)
register expptr p;
int l;
{
register int k;

if( ( k = (p = fixtype(p))->vtype) != TYLOGICAL)
	{
	if(k != TYERROR)
		err("non-logical expression in IF statement");
	frexpr(p);
	}
else
	{
	putex1(p);
	p2icon( (long int) l , P2INT);
	p2op(P2CBRANCH, 0);
	putstmt();
	}
}





/* put out code for  goto l   */
putgoto(label)
int label;
{
p2triple(P2GOTO, 1, label);
putstmt();
}


/* branch to address constant or integer variable */
putbranch(p)
register struct addrblock *p;
{
putex1(p);
p2op(P2GOTO, P2INT);
putstmt();
}



/* put out label  l:     */
putlabel(label)
int label;
{
p2op(P2LABEL, label);
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
struct labelblock *labs[];
{
int i, labarray, skiplabel;

if(! ISINT(index->vtype) )
	{
	execerr("computed goto index must be integer", NULL);
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
		prcona(asmfile, (ftnint)(labs[i]->labelno) );
	prcmgoto(index, nlab, skiplabel, labarray);
	putlabel(skiplabel);
#endif
}

putx(p)
expptr p;
{
struct addrblock *putcall(), *putcx1(), *realpart();
char *memname();
int opc;
int ncomma;
int type, k;

switch(p->tag)
	{
	case TERROR:
		free(p);
		break;

	case TCONST:
		switch(type = p->vtype)
			{
			case TYLOGICAL:
				type = tyint;
			case TYLONG:
			case TYSHORT:
				p2icon(p->const.ci, types2[type]);
				free(p);
				break;

			case TYADDR:
				p2triple(P2ICON, 1, P2INT|P2PTR);
				p2word(0L);
				p2name(memname(STGCONST, (int) p->const.ci) );
				free(p);
				break;

			default:
				putx( putconst(p) );
				break;
			}
		break;

	case TEXPR:
		switch(opc = p->opcode)
			{
			case OPCALL:
			case OPCCALL:
				if( ISCOMPLEX(p->vtype) )
					putcxop(p);
				else	putcall(p);
				break;

			case OPMIN:
			case OPMAX:
				putmnmx(p);
				break;


			case OPASSIGN:
				if( ISCOMPLEX(p->leftp->vtype) || ISCOMPLEX(p->rightp->vtype) )
					frexpr( putcxeq(p) );
				else if( ISCHAR(p) )
					putcheq(p);
				else
					goto putopp;
				break;

			case OPEQ:
			case OPNE:
				if( ISCOMPLEX(p->leftp->vtype) || ISCOMPLEX(p->rightp->vtype) )
					{
					putcxcmp(p);
					break;
					}
			case OPLT:
			case OPLE:
			case OPGT:
			case OPGE:
				if(ISCHAR(p->leftp))
					putchcmp(p);
				else
					goto putopp;
				break;

			case OPPOWER:
				putpower(p);
				break;

			case OPSTAR:
#if FAMILY == SCJ
				/*   m * (2**k) -> m<<k   */
				if(INT(p->leftp->vtype) && ISICON(p->rightp) &&
				   ( (k = log2(p->rightp->const.ci))>0) )
					{
					p->opcode = OPLSHIFT;
					frexpr(p->rightp);
					p->rightp = ICON(k);
					goto putopp;
					}
#endif

			case OPMOD:
				goto putopp;
			case OPPLUS:
			case OPMINUS:
			case OPSLASH:
			case OPNEG:
				if( ISCOMPLEX(p->vtype) )
					putcxop(p);
				else	goto putopp;
				break;

			case OPCONV:
				if( ISCOMPLEX(p->vtype) )
					putcxop(p);
				else if( ISCOMPLEX(p->leftp->vtype) )
					{
					ncomma = 0;
					putx( mkconv(p->vtype,
						realpart(putcx1(p->leftp, &ncomma))));
					putcomma(ncomma, p->vtype, NO);
					free(p);
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

			default:
				fatal1("putx: invalid opcode %d", opc);
			}
		break;

	case TADDR:
		putaddr(p, YES);
		break;

	default:
		fatal1("putx: impossible tag %d", p->tag);
	}
}



LOCAL putop(p)
expptr p;
{
int k;
expptr lp, tp;
int pt, lt;
int comma;

switch(p->opcode)	/* check for special cases and rewrite */
	{
	case OPCONV:
		pt = p->vtype;
		lp = p->leftp;
		lt = lp->vtype;
		while(p->tag==TEXPR && p->opcode==OPCONV &&
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
			free(p);
			p = lp;
			pt = lt;
			lp = p->leftp;
			lt = lp->vtype;
			}
		if(p->tag==TEXPR && p->opcode==OPCONV)
			break;
		putx(p);
		return;

	case OPADDR:
		comma = NO;
		lp = p->leftp;
		if(lp->tag != TADDR)
			{
			tp = mktemp(lp->vtype, lp->vleng);
			putx( mkexpr(OPASSIGN, cpexpr(tp), lp) );
			lp = tp;
			comma = YES;
			}
		putaddr(lp, NO);
		if(comma)
			putcomma(1, TYINT, NO);
		free(p);
		return;
	}

if( (k = ops2[p->opcode]) <= 0)
	fatal1("putop: invalid opcode %d", p->opcode);
putx(p->leftp);
if(p->rightp)
	putx(p->rightp);
p2op(k, types2[p->vtype]);

if(p->vleng)
	frexpr(p->vleng);
free(p);
}

putforce(t, p)
int t;
expptr p;
{
p = mkconv(t, fixtype(p));
putx(p);
p2op(P2FORCE,
	(t==TYSHORT ? P2SHORT : (t==TYLONG ? P2LONG : P2DREAL)) );
putstmt();
}



LOCAL putpower(p)
expptr p;
{
expptr base;
struct addrblock *t1, *t2;
ftnint k;
int type;
int ncomma;

if(!ISICON(p->rightp) || (k = p->rightp->const.ci)<2)
	fatal("putpower: bad call");
base = p->leftp;
type = base->vtype;
t1 = mktemp(type, NULL);
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
	t2 = mktemp(type, NULL);
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




LOCAL struct addrblock *intdouble(p, ncommap)
struct addrblock *p;
int *ncommap;
{
register struct addrblock *t;

t = mktemp(TYDREAL, NULL);
++*ncommap;
putassign(cpexpr(t), p);
return(t);
}





LOCAL putcxeq(p)
register struct exprblock *p;
{
register struct addrblock *lp, *rp;
int ncomma;

ncomma = 0;
lp = putcx1(p->leftp, &ncomma);
rp = putcx1(p->rightp, &ncomma);
putassign(realpart(lp), realpart(rp));
if( ISCOMPLEX(p->vtype) )
	{
	++ncomma;
	putassign(imagpart(lp), imagpart(rp));
	}
putcomma(ncomma, TYREAL, NO);
frexpr(rp);
free(p);
return(lp);
}



LOCAL putcxop(p)
expptr p;
{
struct addrblock *putcx1();
int ncomma;

ncomma = 0;
putaddr( putcx1(p, &ncomma), NO);
putcomma(ncomma, TYINT, NO);
}



LOCAL struct addrblock *putcx1(p, ncommap)
register expptr p;
int *ncommap;
{
struct addrblock *q, *lp, *rp;
register struct addrblock *resp;
int opcode;
int ltype, rtype;

if(p == NULL)
	return(NULL);

switch(p->tag)
	{
	case TCONST:
		if( ISCOMPLEX(p->vtype) )
			p = putconst(p);
		return( p );

	case TADDR:
		if( ! addressable(p) )
			{
			++*ncommap;
			resp = mktemp(tyint, NULL);
			putassign( cpexpr(resp), p->memoffset );
			p->memoffset = resp;
			}
		return( p );

	case TEXPR:
		if( ISCOMPLEX(p->vtype) )
			break;
		++*ncommap;
		resp = mktemp(TYDREAL, NO);
		putassign( cpexpr(resp), p);
		return(resp);

	default:
		fatal1("putcx1: bad tag %d", p->tag);
	}

opcode = p->opcode;
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
resp = mktemp(p->vtype, NULL);
if(lp = putcx1(p->leftp, ncommap) )
	ltype = lp->vtype;
if(rp = putcx1(p->rightp, ncommap) )
	rtype = rp->vtype;

switch(opcode)
	{
	case OPCOMMA:
		frexpr(resp);
		resp = rp;
		rp = NULL;
		break;

	case OPNEG:
		putassign( realpart(resp), mkexpr(OPNEG, realpart(lp), NULL) );
		putassign( imagpart(resp), mkexpr(OPNEG, imagpart(lp), NULL) );
		*ncommap += 2;
		break;

	case OPPLUS:
	case OPMINUS:
		putassign( realpart(resp), mkexpr(opcode, realpart(lp), realpart(rp) ));
		if(rtype < TYCOMPLEX)
			putassign( imagpart(resp), imagpart(lp) );
		else if(ltype < TYCOMPLEX)
			{
			if(opcode == OPPLUS)
				putassign( imagpart(resp), imagpart(rp) );
			else	putassign( imagpart(resp), mkexpr(OPNEG, imagpart(rp), NULL) );
			}
		else
			putassign( imagpart(resp), mkexpr(opcode, imagpart(lp), imagpart(rp) ));

		*ncommap += 2;
		break;

	case OPSTAR:
		if(ltype < TYCOMPLEX)
			{
			if( ISINT(ltype) )
				lp = intdouble(lp, ncommap);
			putassign( realpart(resp), mkexpr(OPSTAR, cpexpr(lp), realpart(rp) ));
			putassign( imagpart(resp), mkexpr(OPSTAR, cpexpr(lp), imagpart(rp) ));
			}
		else if(rtype < TYCOMPLEX)
			{
			if( ISINT(rtype) )
				rp = intdouble(rp, ncommap);
			putassign( realpart(resp), mkexpr(OPSTAR, cpexpr(rp), realpart(lp) ));
			putassign( imagpart(resp), mkexpr(OPSTAR, cpexpr(rp), imagpart(lp) ));
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
		putassign( realpart(resp), mkexpr(OPSLASH, realpart(lp), cpexpr(rp)) );
		putassign( imagpart(resp), mkexpr(OPSLASH, imagpart(lp), cpexpr(rp)) );
		*ncommap += 2;
		break;

	case OPCONV:
		putassign( realpart(resp), realpart(lp) );
		if( ISCOMPLEX(lp->vtype) )
			q = imagpart(lp);
		else if(rp != NULL)
			q = realpart(rp);
		else
			q = mkrealcon(TYDREAL, 0.0);
		putassign( imagpart(resp), q);
		*ncommap += 2;
		break;

	default:
		fatal1("putcx1 of invalid opcode %d", opcode);
	}

frexpr(lp);
frexpr(rp);
free(p);
return(resp);
}




LOCAL putcxcmp(p)
register struct exprblock *p;
{
int opcode;
int ncomma;
register struct addrblock *lp, *rp;
struct exprblock *q;

ncomma = 0;
opcode = p->opcode;
lp = putcx1(p->leftp, &ncomma);
rp = putcx1(p->rightp, &ncomma);

q = mkexpr( opcode==OPEQ ? OPAND : OPOR ,
	mkexpr(opcode, realpart(lp), realpart(rp)),
	mkexpr(opcode, imagpart(lp), imagpart(rp)) );
putx( fixexpr(q) );
putcomma(ncomma, TYINT, NO);

free(lp);
free(rp);
free(p);
}

LOCAL struct addrblock *putch1(p, ncommap)
register expptr p;
int * ncommap;
{
register struct addrblock *t;
struct addrblock *mktemp(), *putconst();

switch(p->tag)
	{
	case TCONST:
		return( putconst(p) );

	case TADDR:
		return(p);

	case TEXPR:
		++*ncommap;

		switch(p->opcode)
			{
			case OPCALL:
			case OPCCALL:
				t = putcall(p);
				break;

			case OPCONCAT:
				t = mktemp(TYCHAR, cpexpr(p->vleng) );
				putcat( cpexpr(t), p );
				break;

			case OPCONV:
				if(!ISICON(p->vleng) || p->vleng->const.ci!=1
				   || ! INT(p->leftp->vtype) )
					fatal("putch1: bad character conversion");
				t = mktemp(TYCHAR, ICON(1) );
				putop( mkexpr(OPASSIGN, cpexpr(t), p) );
				break;
			default:
				fatal1("putch1: invalid opcode %d", p->opcode);
			}
		return(t);

	default:
		fatal1("putch1: bad tag %d", p->tag);
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
register struct exprblock *p;
{
int ncomma;

ncomma = 0;
if( p->rightp->tag==TEXPR && p->rightp->opcode==OPCONCAT )
	putcat(p->leftp, p->rightp);
else if( ISONE(p->leftp->vleng) && ISONE(p->rightp->vleng) )
	{
	putaddr( putch1(p->leftp, &ncomma) , YES );
	putaddr( putch1(p->rightp, &ncomma) , YES );
	putcomma(ncomma, TYINT, NO);
	p2op(P2ASSIGN, P2CHAR);
	}
else
	{
	putx( call2(TYINT, "s_copy", p->leftp, p->rightp) );
	putcomma(ncomma, TYINT, NO);
	}

frexpr(p->vleng);
free(p);
}




LOCAL putchcmp(p)
register struct exprblock *p;
{
int ncomma;

ncomma = 0;
if(ISONE(p->leftp->vleng) && ISONE(p->rightp->vleng) )
	{
	putaddr( putch1(p->leftp, &ncomma) , YES );
	putaddr( putch1(p->rightp, &ncomma) , YES );
	p2op(ops2[p->opcode], P2CHAR);
	free(p);
	putcomma(ncomma, TYINT, NO);
	}
else
	{
	p->leftp = call2(TYINT,"s_cmp", p->leftp, p->rightp);
	p->rightp = ICON(0);
	putop(p);
	}
}





LOCAL putcat(lhs, rhs)
register struct addrblock *lhs;
register expptr rhs;
{
int n, ncomma;
struct addrblock *lp, *cp;

ncomma = 0;
n = ncat(rhs);
lp = mktmpn(n, TYLENG, NULL);
cp = mktmpn(n, TYADDR, NULL);

n = 0;
putct1(rhs, lp, cp, &n, &ncomma);

putx( call4(TYSUBR, "s_cat", lhs, cp, lp, ICON(n) ) );
putcomma(ncomma, TYINT, NO);
}





LOCAL ncat(p)
register expptr p;
{
if(p->tag==TEXPR && p->opcode==OPCONCAT)
	return( ncat(p->leftp) + ncat(p->rightp) );
else	return(1);
}




LOCAL putct1(q, lp, cp, ip, ncommap)
register expptr q;
register struct addrblock *lp, *cp;
int *ip, *ncommap;
{
int i;
struct addrblock *lp1, *cp1;

if(q->tag==TEXPR && q->opcode==OPCONCAT)
	{
	putct1(q->leftp, lp, cp, ip, ncommap);
	putct1(q->rightp, lp, cp , ip, ncommap);
	frexpr(q->vleng);
	free(q);
	}
else
	{
	i = (*ip)++;
	lp1 = cpexpr(lp);
	lp1->memoffset = mkexpr(OPPLUS, lp1->memoffset, ICON(i*SZLENG));
	cp1 = cpexpr(cp);
	cp1->memoffset = mkexpr(OPPLUS, cp1->memoffset, ICON(i*SZADDR));
	putassign( lp1, cpexpr(q->vleng) );
	putassign( cp1, addrof(putch1(q,ncommap)) );
	*ncommap += 2;
	}
}

LOCAL putaddr(p, indir)
register struct addrblock *p;
int indir;
{
int type, type2, funct;
ftnint offset, simoffset();
expptr offp, shorten();

type = p->vtype;
type2 = types2[type];
funct = (p->vclass==CLPROC ? P2FUNCT<<2 : 0);

offp = (p->memoffset ? cpexpr(p->memoffset) : NULL);


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
			p2reg(AUTOREG, type2 | P2PTR);
			break;
			}

		p2reg(AUTOREG, type2 | P2PTR);
		if(offp)
			{
			putx(offp);
			if(offset)
				p2icon(offset, P2INT);
			}
		else
			p2icon(offset, P2INT);
		if(offp && offset)
			p2op(P2PLUS, type2 | P2PTR);
		p2op(P2PLUS, type2 | P2PTR);
		if(indir)
			p2op(P2INDIRECT, type2);
		break;

	case STGARG:
		p2oreg(
#ifdef ARGOFFSET
			ARGOFFSET +
#endif
			(ftnint) (FUDGEOFFSET*p->memno),
			ARGREG,   type2 | P2PTR | funct );

		if(offp)
			putx(offp);
		if(offset)
			p2icon(offset, P2INT);
		if(offp && offset)
			p2op(P2PLUS, type2 | P2PTR);
		if(offp || offset)
			p2op(P2PLUS, type2 | P2PTR);
		if(indir)
			p2op(P2INDIRECT, type2);
		break;

	case STGLENG:
		if(indir)
			{
			p2oreg(
#ifdef ARGOFFSET
				ARGOFFSET +
#endif
				(ftnint) (FUDGEOFFSET*p->memno),
				ARGREG,   type2 | P2PTR );
			}
		else	{
			p2reg(ARGREG, type2 | P2PTR );
			p2icon(
#ifdef ARGOFFSET
				ARGOFFSET +
#endif
				(ftnint) (FUDGEOFFSET*p->memno), P2INT);
			p2op(P2PLUS, type2 | P2PTR );
			}
		break;


	case STGBSS:
	case STGINIT:
	case STGEXT:
	case STGCOMMON:
	case STGEQUIV:
	case STGCONST:
		if(offp)
			{
			putx(offp);
			putmem(p, P2ICON, offset);
			p2op(P2PLUS, type2 | P2PTR);
			if(indir)
				p2op(P2INDIRECT, type2);
			}
		else
			putmem(p, (indir ? P2NAME : P2ICON), offset);

		break;

	case STGREG:
		if(indir)
			p2reg(p->memno, type2);
		else
			fatal("attempt to take address of a register");
		break;

	default:
		fatal1("putaddr: invalid vstg %d", p->vstg);
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

funct = (p->vclass==CLPROC ? P2FUNCT<<2 : 0);
type2 = types2[p->vtype];
if(p->vclass == CLPROC)
	type2 |= (P2FUNCT<<2);
name = memname(p->vstg, p->memno);
if(class == P2ICON)
	{
	p2triple(P2ICON, name[0]!='\0', type2|P2PTR);
	p2word(offset);
	if(name[0])
		p2name(name);
	}
else
	{
	p2triple(P2NAME, offset!=0, type2);
	if(offset != 0)
		p2word(offset);
	p2name(name);
	}
}



LOCAL struct addrblock *putcall(p)
struct exprblock *p;
{
chainp arglist, charsp, cp;
int n, first;
struct addrblock *t;
struct exprblock *q;
struct exprblock *fval;
int type, type2, ctype, indir;

type2 = types2[type = p->vtype];
charsp = NULL;
indir =  (p->opcode == OPCCALL);
n = 0;
first = YES;

if(p->rightp)
	{
	arglist = p->rightp->listp;
	free(p->rightp);
	}
else
	arglist = NULL;

for(cp = arglist ; cp ; cp = cp->nextp)
	if(indir)
		++n;
	else	{
		q = cp->datap;
		if(q->tag == TCONST)
			cp->datap = q = putconst(q);
		if( ISCHAR(q) )
			{
			charsp = hookup(charsp, mkchain(cpexpr(q->vleng), 0) );
			n += 2;
			}
		else if(q->vclass == CLPROC)
			{
			charsp = hookup(charsp, mkchain( ICON(0) , 0));
			n += 2;
			}
		else
			n += 1;
		}

if(type == TYCHAR)
	{
	if( ISICON(p->vleng) )
		{
		fval = mktemp(TYCHAR, p->vleng);
		n += 2;
		}
	else	{
		err("adjustable character function");
		return;
		}
	}
else if( ISCOMPLEX(type) )
	{
	fval = mktemp(type, NULL);
	n += 1;
	}
else
	fval = NULL;

ctype = (fval ? P2INT : type2);
putaddr(p->leftp, NO);

if(fval)
	{
	first = NO;
	putaddr( cpexpr(fval), NO);
	if(type==TYCHAR)
		{
		putx( mkconv(TYLENG,p->vleng) );
		p2op(P2LISTOP, type2);
		}
	}

for(cp = arglist ; cp ; cp = cp->nextp)
	{
	q = cp->datap;
	if(q->tag==TADDR && (indir || q->vstg!=STGREG) )
		putaddr(q, indir && q->vtype!=TYCHAR);
	else if( ISCOMPLEX(q->vtype) )
		putcxop(q);
	else if (ISCHAR(q) )
		putchop(q);
	else if( ! ISERROR(q) )
		{
		if(indir)
			putx(q);
		else	{
			t = mktemp(q->vtype, q->vleng);
			putassign( cpexpr(t), q );
			putaddr(t, NO);
			putcomma(1, q->vtype, YES);
			}
		}
	if(first)
		first = NO;
	else
		p2op(P2LISTOP, type2);
	}

if(arglist)
	frchain(&arglist);
for(cp = charsp ; cp ; cp = cp->nextp)
	{
	putx( mkconv(TYLENG,cp->datap) );
	p2op(P2LISTOP, type2);
	}
frchain(&charsp);
p2op(n>0 ? P2CALL : P2CALL0 , ctype);
free(p);
return(fval);
}



LOCAL putmnmx(p)
register struct exprblock *p;
{
int op, type;
int ncomma;
struct exprblock *qp;
chainp p0, p1;
struct addrblock *sp, *tp;

type = p->vtype;
op = (p->opcode==OPMIN ? OPLT : OPGT );
p0 = p->leftp->listp;
free(p->leftp);
free(p);

sp = mktemp(type, NULL);
tp = mktemp(type, NULL);
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
frtemp(sp);
frtemp(tp);
frchain( &p0 );
}




LOCAL putcomma(n, type, indir)
int n, type, indir;
{
type = types2[type];
if(indir)
	type |= P2PTR;
while(--n >= 0)
	p2op(P2COMOP, type);
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

if( ! ISINT(p->vtype) )
	return(0);

if(p->tag==TEXPR && p->opcode==OPSTAR)
	{
	lp = p->leftp;
	rp = p->rightp;
	if(ISICON(rp) && lp->tag==TEXPR && lp->opcode==OPPLUS && ISICON(lp->rightp))
		{
		p->opcode = OPPLUS;
		lp->opcode = OPSTAR;
		prod = rp->const.ci * lp->rightp->const.ci;
		lp->rightp->const.ci = rp->const.ci;
		rp->const.ci = prod;
		}
	}

if(p->tag==TEXPR && p->opcode==OPPLUS && ISICON(p->rightp))
	{
	rp = p->rightp;
	lp = p->leftp;
	offset += rp->const.ci;
	frexpr(rp);
	free(p);
	*p0 = lp;
	}

if(p->tag == TCONST)
	{
	offset += p->const.ci;
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
p2triple(P2ICON, 0, type);
p2word(offset);
}




p2oreg(offset, reg, type)
ftnint offset;
int reg, type;
{
p2triple(P2OREG, reg, type);
p2word(offset);
p2name("");
}




p2reg(reg, type)
int reg, type;
{
p2triple(P2REG, reg, type);
}



p2pass(s)
char *s;
{
p2triple(P2PASS, (strlen(s) + FOUR-1)/FOUR, 0);
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
word = op | (var<<8);
word |= ( (long int) type) <<16;
p2word(word);
}




p2name(s)
char *s;
{
int i;
union  { long int word[2];  char str[8]; } u;

u.word[0] = u.word[1] = 0;
for(i = 0 ; i<8 && *s ; ++i)
	u.str[i] = *s++;
p2word(u.word[0]);
p2word(u.word[1]);
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
