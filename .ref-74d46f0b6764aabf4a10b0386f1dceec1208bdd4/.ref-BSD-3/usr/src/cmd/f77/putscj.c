/* INTERMEDIATE CODE GENERATION FOR S C JOHNSON C COMPILERS */
/* NEW VERSION USING BINARY POLISH POSTFIX INTERMEDIATE */
#if FAMILY != SCJ
	WRONG put FULE !!!!
#endif

#include "defs"
#include "scjdefs"
struct Addrblock *imagpart();

#define FOUR 4
extern int ops2[];
extern int types2[];

#define P2BUFFMAX 128
static long int p2buff[P2BUFFMAX];
static long int *p2bufp		= &p2buff[0];
static long int *p2bufend	= &p2buff[P2BUFFMAX];


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
#if FAMILY==SCJ && OUTPUT==BINARY
	p2flush();
#endif
	headoffset = ftell(textfile);
	prhead(textfile);
	headerdone = YES;
	p2triple(P2STMT, (strlen(infname)+FOUR-1)/FOUR, 0);
	p2str(infname);
#if TARGET == PDP11
	/* fake jump to start the optimizer */
	if(class != CLBLOCK)
	putgoto( fudgelabel = newlabel() );
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

if( ( k = (p = fixtype(p))->headblock.vtype) != TYLOGICAL)
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
register struct Addrblock *p;
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
struct Labelblock *labs[];
{
int i, labarray, skiplabel;

if(! ISINT(index->headblock.vtype) )
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
		if( labs[i] )
			prcona(asmfile, (ftnint)(labs[i]->labelno) );
	prcmgoto(index, nlab, skiplabel, labarray);
	putlabel(skiplabel);
#endif
}

putx(p)
expptr p;
{
struct Addrblock *putcall(), *putcx1(), *realpart();
char *memname();
int opc;
int ncomma;
int type, k;

switch(p->headblock.tag)
	{
	case TERROR:
		free(p);
		break;

	case TCONST:
		switch(type = p->constblock.vtype)
			{
			case TYLOGICAL:
				type = tyint;
			case TYLONG:
			case TYSHORT:
				p2icon(p->constblock.const.ci, types2[type]);
				free(p);
				break;

			case TYADDR:
				p2triple(P2ICON, 1, P2INT|P2PTR);
				p2word(0L);
				p2name(memname(STGCONST, (int) p->constblock.const.ci) );
				free(p);
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
				if( ISCOMPLEX(p->exprblock.leftp->headblock.vtype) ||
				    ISCOMPLEX(p->exprblock.rightp->headblock.vtype) )
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
				if(INT(p->exprblock.leftp->headblock.vtype) &&
				   ISICON(p->exprblock.rightp) &&
				   ( (k = log2(p->exprblock.rightp->constblock.const.ci))>0) )
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
						realpart(putcx1(p->exprblock.leftp, &ncomma))));
					putcomma(ncomma, p->exprblock.vtype, NO);
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
				fatali("putx: invalid opcode %d", opc);
			}
		break;

	case TADDR:
		putaddr(p, YES);
		break;

	default:
		fatali("putx: impossible tag %d", p->headblock.tag);
	}
}



LOCAL putop(p)
expptr p;
{
int k;
expptr lp, tp;
int pt, lt;
int comma;

switch(p->exprblock.opcode)	/* check for special cases and rewrite */
	{
	case OPCONV:
		pt = p->exprblock.vtype;
		lp = p->exprblock.leftp;
		lt = lp->headblock.vtype;
		while(p->headblock.tag==TEXPR &&
		      p->exprblock.opcode==OPCONV &&
		      ( (ISREAL(pt)&&ISREAL(lt)) ||
			(INT(pt)&&(ONEOF(lt,MSKINT|MSKADDR|MSKCHAR|M(TYSUBR)))) ))
			{
#if SZINT < SZLONG
			if(lp->headblock.tag != TEXPR)
				{
				if(pt==TYINT && lt==TYLONG)
					break;
				if(lt==TYINT && pt==TYLONG)
					break;
				}
#endif

#if TARGET == VAX
			if(pt==TYDREAL && lt==TYREAL)
				break;
#endif

			if(lt==TYCHAR && lp->headblock.tag==TEXPR && 
			   lp->exprblock.opcode==OPCALL)
				{
				p->exprblock.leftp = putcall(lp);
				putop(p);
				putcomma(1, pt, NO);
				free(p);
				return;
				}
			free(p);
			p = lp;
			pt = lt;
			lp = p->exprblock.leftp;
			lt = lp->headblock.vtype;
			}
		if(p->headblock.tag==TEXPR && p->exprblock.opcode==OPCONV)
			break;
		putx(p);
		return;

	case OPADDR:
		comma = NO;
		lp = p->exprblock.leftp;
		if(lp->headblock.tag != TADDR)
			{
			tp = mktemp(lp->headblock.vtype, lp->headblock.vleng);
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

if( (k = ops2[p->exprblock.opcode]) <= 0)
	fatali("putop: invalid opcode %d", p->exprblock.opcode);
putx(p->exprblock.leftp);
if(p->exprblock.rightp)
	putx(p->exprblock.rightp);
p2op(k, types2[p->exprblock.vtype]);

if(p->exprblock.vleng)
	frexpr(p->exprblock.vleng);
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
struct Addrblock *t1, *t2;
ftnint k;
int type;
int ncomma;

if(!ISICON(p->exprblock.rightp) ||
    (k = p->exprblock.rightp->constblock.const.ci)<2)
	fatal("putpower: bad call");
base = p->exprblock.leftp;
type = base->headblock.vtype;
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




LOCAL struct Addrblock *intdouble(p, ncommap)
struct Addrblock *p;
int *ncommap;
{
register struct Addrblock *t;

t = mktemp(TYDREAL, NULL);
++*ncommap;
putassign(cpexpr(t), p);
return(t);
}





LOCAL putcxeq(p)
register struct Exprblock *p;
{
register struct Addrblock *lp, *rp;
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
struct Addrblock *putcx1();
int ncomma;

ncomma = 0;
putaddr( putcx1(p, &ncomma), NO);
putcomma(ncomma, TYINT, NO);
}



LOCAL struct Addrblock *putcx1(p, ncommap)
register expptr p;
int *ncommap;
{
struct Addrblock *q, *lp, *rp;
register struct Addrblock *resp;
int opcode;
int ltype, rtype;
struct Constblock *mkrealcon();

if(p == NULL)
	return(NULL);

switch(p->headblock.tag)
	{
	case TCONST:
		if( ISCOMPLEX(p->constblock.vtype) )
			p = putconst(p);
		return( p );

	case TADDR:
		if( ! addressable(p) )
			{
			++*ncommap;
			resp = mktemp(tyint, NULL);
			putassign( cpexpr(resp), p->addrblock.memoffset );
			p->addrblock.memoffset = resp;
			}
		return( p );

	case TEXPR:
		if( ISCOMPLEX(p->exprblock.vtype) )
			break;
		++*ncommap;
		resp = mktemp(TYDREAL, NO);
		putassign( cpexpr(resp), p);
		return(resp);

	default:
		fatali("putcx1: bad tag %d", p->headblock.tag);
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
resp = mktemp(p->exprblock.vtype, NULL);
if(lp = putcx1(p->exprblock.leftp, ncommap) )
	ltype = lp->headblock.vtype;
if(rp = putcx1(p->headblock.rightp, ncommap) )
	rtype = rp->headblock.vtype;

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
		fatali("putcx1 of invalid opcode %d", opcode);
	}

frexpr(lp);
frexpr(rp);
free(p);
return(resp);
}




LOCAL putcxcmp(p)
register struct Exprblock *p;
{
int opcode;
int ncomma;
register struct Addrblock *lp, *rp;
struct Exprblock *q;

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

LOCAL struct Addrblock *putch1(p, ncommap)
register expptr p;
int * ncommap;
{
register struct Addrblock *t;
struct Addrblock *mktemp(), *putconst();

switch(p->headblock.tag)
	{
	case TCONST:
		return( putconst(p) );

	case TADDR:
		return(p);

	case TEXPR:
		++*ncommap;

		switch(p->exprblock.opcode)
			{
			case OPCALL:
			case OPCCALL:
				t = putcall(p);
				break;

			case OPCONCAT:
				t = mktemp(TYCHAR, cpexpr(p->exprblock.vleng) );
				putcat( cpexpr(t), p );
				break;

			case OPCONV:
				if(!ISICON(p->exprblock.vleng)
				   || p->exprblock.vleng->constblock.const.ci!=1
				   || ! INT(p->exprblock.leftp->headblock.vtype) )
					fatal("putch1: bad character conversion");
				t = mktemp(TYCHAR, ICON(1) );
				putop( mkexpr(OPASSIGN, cpexpr(t), p) );
				break;
			default:
				fatali("putch1: invalid opcode %d",
					p->exprblock.opcode);
			}
		return(t);

	default:
		fatali("putch1: bad tag %d", p->tag);
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
register struct Exprblock *p;
{
int ncomma;

ncomma = 0;
if( p->rightp->headblock.tag==TEXPR && p->rightp->exprblock.opcode==OPCONCAT )
	putcat(p->leftp, p->rightp);
else if( ISONE(p->leftp->headblock.vleng) && ISONE(p->rightp->headblock.vleng) )
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
register struct Exprblock *p;
{
int ncomma;

ncomma = 0;
if(ISONE(p->leftp->headblock.vleng) && ISONE(p->rightp->headblock.vleng) )
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
register struct Addrblock *lhs;
register expptr rhs;
{
int n, ncomma;
struct Addrblock *lp, *cp;

ncomma = 0;
n = ncat(rhs);
lp = mktmpn(n, TYLENG, NULL);
cp = mktmpn(n, TYADDR, NULL);

n = 0;
putct1(rhs, lp, cp, &n, &ncomma);

putx( call4(TYSUBR, "s_cat", lhs, cp, lp, mkconv(TYLONG, ICON(n)) ) );
putcomma(ncomma, TYINT, NO);
}





LOCAL ncat(p)
register expptr p;
{
if(p->headblock.tag==TEXPR && p->exprblock.opcode==OPCONCAT)
	return( ncat(p->exprblock.leftp) + ncat(p->exprblock.rightp) );
else	return(1);
}




LOCAL putct1(q, lp, cp, ip, ncommap)
register expptr q;
register struct Addrblock *lp, *cp;
int *ip, *ncommap;
{
int i;
struct Addrblock *lp1, *cp1;

if(q->headblock.tag==TEXPR && q->exprblock.opcode==OPCONCAT)
	{
	putct1(q->exprblock.leftp, lp, cp, ip, ncommap);
	putct1(q->exprblock.rightp, lp, cp , ip, ncommap);
	frexpr(q->exprblock.vleng);
	free(q);
	}
else
	{
	i = (*ip)++;
	lp1 = cpexpr(lp);
	lp1->memoffset = mkexpr(OPPLUS, lp1->memoffset, ICON(i*SZLENG));
	cp1 = cpexpr(cp);
	cp1->memoffset = mkexpr(OPPLUS, cp1->memoffset, ICON(i*SZADDR));
	putassign( lp1, cpexpr(q->headblock.vleng) );
	putassign( cp1, addrof(putch1(q,ncommap)) );
	*ncommap += 2;
	}
}

LOCAL putaddr(p, indir)
register struct Addrblock *p;
int indir;
{
int type, type2, funct;
ftnint offset, simoffset();
expptr offp, shorten();

if( ISERROR(p) || ISERROR(p->memoffset) )
	{
	frexpr(p);
	return;
	}

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
				ARGREG,   type2 );
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
		fatali("putaddr: invalid vstg %d", p->vstg);
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

funct = (p->headblock.vclass==CLPROC ? P2FUNCT<<2 : 0);
type2 = types2[p->headblock.vtype];
if(p->headblock.vclass == CLPROC)
	type2 |= (P2FUNCT<<2);
name = memname(p->addrblock.vstg, p->addrblock.memno);
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



LOCAL struct Addrblock *putcall(p)
struct Exprblock *p;
{
chainp arglist, charsp, cp;
int n, first;
struct Addrblock *t;
struct Exprblock *q;
struct Exprblock *fval;
int type, type2, ctype, indir;

type2 = types2[type = p->vtype];
charsp = NULL;
indir =  (p->opcode == OPCCALL);
n = 0;
first = YES;

if(p->rightp)
	{
	arglist = p->rightp->listblock.listp;
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
register struct Exprblock *p;
{
int op, type;
int ncomma;
struct Exprblock *qp;
chainp p0, p1;
struct Addrblock *sp, *tp;

type = p->vtype;
op = (p->opcode==OPMIN ? OPLT : OPGT );
p0 = p->leftp->listblock.listp;
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

if( ! ISINT(p->headblock.vtype) )
	return(0);

if(p->headblock.tag==TEXPR && p->exprblock.opcode==OPSTAR)
	{
	lp = p->exprblock.leftp;
	rp = p->exprblock.rightp;
	if(ISICON(rp) && lp->headblock.tag==TEXPR &&
	   lp->exprblock.opcode==OPPLUS && ISICON(lp->exprblock.rightp))
		{
		p->exprblock.opcode = OPPLUS;
		lp->opcode = OPSTAR;
		prod = rp->constblock.const.ci *
			lp->exprblock.rightp->constblock.const.ci;
		lp->exprblock.rightp->constblock.const.ci = rp->constblock.const.ci;
		rp->constblock.const.ci = prod;
		}
	}

if(p->headblock.tag==TEXPR && p->exprblock.opcode==OPPLUS &&
    ISICON(p->exprblock.rightp))
	{
	rp = p->exprblock.rightp;
	lp = p->exprblock.leftp;
	offset += rp->constblock.const.ci;
	frexpr(rp);
	free(p);
	*p0 = lp;
	}

if(p->headblock.tag == TCONST)
	{
	offset += p->constblock.const.ci;
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
