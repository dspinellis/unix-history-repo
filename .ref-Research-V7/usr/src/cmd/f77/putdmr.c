/*  INTERMEDIATE CODE GENERATION FOR D. M. RITCHIE C COMPILERS */
#if FAMILY != DMR
	WRONG put FILE !!!!
#endif

#include "defs"
#include "dmrdefs"


extern int ops2[];
extern int types2[];


puthead(s, class)
char *s;
int class;
{
if( ! headerdone )
	{
	p2op2(P2SETREG, ARGREG-maxregvar);
	p2op(P2PROG);
	headerdone = YES;
#if TARGET == PDP11
	/* fake jump to start the optimizer */
	if(class != CLBLOCK)
		putgoto( fudgelabel = newlabel() );
#endif
	}
}




putnreg()
{
p2op2(P2SETREG, ARGREG-nregvar);
}






puteof()
{
p2op(P2EOF);
}



putstmt()
{
p2op2(P2EXPR, lineno);
}




/* put out code for if( ! p) goto l  */
putif(p,l)
register expptr p;
int l;
{
register int k;
if( (k = (p = fixtype(p))->vtype) != TYLOGICAL)
	{
	if(k != TYERROR)
		err("non-logical expression in IF statement");
	frexpr(p);
	}
else
	{
	putex1(p);
	p2op2(P2CBRANCH, l);
	p2i(0);
	p2i(lineno);
	}
}





/* put out code for  goto l   */
putgoto(label)
int label;
{
p2op2(P2GOTO, label);
}


/* branch to address constant or integer variable */
putbranch(p)
register struct addrblock *p;
{
register int type;

type = p->vtype;
if(p->tag != TADDR)
	fatal("invalid goto label");
putaddr(p, YES);
if(type != TYINT)
	p2op2(P2LTOI, P2INT);
p2op2(P2INDIRECT, P2INT);
p2op2(P2JUMP, P2INT);
putstmt();
}



/* put out label  l:     */
putlabel(label)
int label;
{
p2op2(P2LABEL, label);
}




putexpr(p)
expptr p;
{
putex1(p);
putstmt();
}





prarif(p, neg, zero, pos)
expptr p;
int neg ,zero, pos;
{
putx(p);
p2op(P2ARIF);
p2i(neg);
p2i(zero);
p2i(pos);
p2i(lineno);
}



putcmgo(index, nlab, labs)
expptr index;
int nlab;
struct labelblock *labs[];
{
register int i;
int skiplabel;

if(! ISINT(index->vtype) )
	{
	execerr("computed goto index must be integer", NULL);
	return;
	}

putforce(TYINT, mkconv(TYINT, index) );
p2op(P2SWITCH);
p2i(skiplabel = newlabel() );
p2i(lineno);
for(i = 0 ; i<nlab ; ++i)
	{
	p2i(labs[i]->labelno);
	p2i(i+1);
	}
p2i(0);
putlabel(skiplabel);
}

putx(p)
register expptr p;
{
struct addrblock *putcall(), *putcx1(), *realpart();
char *memname();
int opc;
int type, ncomma;

switch(p->tag)
	{
	case TERROR:
		free(p);
		break;

	case TCONST:
		switch(type = p->vtype)
			{
			case TYLOGICAL:
				type = tylogical;
			case TYLONG:
			case TYSHORT:
				if(type == TYSHORT)
					{
					p2op2(P2ICON, P2SHORT);
					p2i( (short)(p->const.ci) );
					}
				else
					{
					p2op2(P2LCON, P2LONG);
					p2li(p->const.ci);
					}
				free(p);
				break;

			case TYADDR:
				p2op(P2NAME);
				p2i(P2STATIC);
				p2i(P2INT);
				p2i( (int) p->const.ci);
				p2op2(P2ADDR, P2PTR);
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

			case OPMOD:
				goto putopp;
			case OPSTAR:

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
register expptr p;
{
int k, ncomma;
int type2, ptype, ltype;
int convop;
register expptr lp, tp;

switch(p->opcode)	/* check for special cases and rewrite */
	{

	case OPCONV:
		lp = p->leftp;
		while(p->tag==TEXPR && p->opcode==OPCONV &&
		     (	( (ptype = p->vtype) == (ltype = lp->vtype) ) ||
		     (ISREAL(ptype)&&ISREAL(ltype)) ||
		     (ONEOF(ptype, M(TYSHORT)|M(TYADDR)) &&
			ONEOF(ltype, M(TYSHORT)|M(TYADDR))) ||
		        (ptype==TYINT && ONEOF(ltype, M(TYSUBR)|M(TYCHAR))) ))
				{
				free(p);
				p = lp;
				lp = p->leftp;
				}
		if(p->tag!=TEXPR || p->opcode!=OPCONV || ISCOMPLEX((ltype = lp->vtype)) )
			{
			putx(p);
			return;
			}
		ltype = lp->vtype;
		switch(ptype = p->vtype)
			{
			case TYCHAR:
				p->leftp = lp = mkconv(TYSHORT, lp);
				convop = P2ITOC;
				break;

			case TYSHORT:
			case TYADDR:
				switch(ltype)
					{
					case TYLONG:
						convop = P2LTOI; break;
					case TYREAL:
					case TYDREAL:
						convop = P2FTOI; break;
					default:
						goto badconv;
					}
				break;

			case TYLONG:
				switch(ltype)
					{
					case TYCHAR:
					case TYSHORT:
					case TYADDR:
						convop = P2ITOL; break;
					case TYREAL:
					case TYDREAL:
						convop = P2FTOL; break;
					default:
						goto badconv;
					}
				break;

			case TYREAL:
			case TYDREAL:
				switch(ltype)
					{
					case TYCHAR:
					case TYSHORT:
					case TYADDR:
						convop = P2ITOF; break;
					case TYLONG:
						convop = P2LTOF; break;
					default:
						goto badconv;
					}
				break;

			default:
			badconv:
				fatal("putop: impossible conversion");
			}
		putx(lp);
		p2op2(convop, types2[ptype]);
		free(p);
		return;

	case OPADDR:
		lp = p->leftp;
		if(lp->tag != TADDR)
			{
			tp = mktemp(lp->vtype, lp->vleng);
			putx( mkexpr(OPASSIGN, cpexpr(tp), lp) );
			ncomma = 1;
			lp = tp;
			}
		else	ncomma = 0;
		putaddr(lp, NO);
		putcomma(ncomma, TYINT, NO);
		free(p);
		return;

	case OPASSIGN:
		if(p->vtype==TYLOGICAL && tylogical!=TYINT &&
		   p->rightp->tag==TEXPR && p->rightp->opcode!=OPCALL && p->rightp->opcode!=OPCCALL)
			{
			p->rightp->vtype = TYINT;
			p->rightp = mkconv(tylogical, p->rightp);
			}
		break;
	}

if( (k = ops2[p->opcode]) <= 0)
	fatal1("putop: invalid opcode %d", p->opcode);
putx(p->leftp);
if(p->rightp)
	putx(p->rightp);
type2 = (p->vtype==TYLOGICAL ? P2INT : types2[p->vtype]);
p2op2(k, type2);

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
p2op2(P2FORCE, (t==TYSHORT ? P2SHORT : (t==TYLONG ? P2LONG : P2DREAL)) );
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
	p2op2(P2ASSIGN, P2CHAR);
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
	p2op2(ops2[p->opcode], P2CHAR);
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

putx( call4(TYSUBR, "s_cat", lhs, cp, lp, mkconv(TYLONG, ICON(n)) ) );
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
expptr offp;

type = p->vtype;
type2 = types2[type];
if(p->vclass == CLPROC)
	{
	funct = P2FUNCT;
	if(type == TYUNKNOWN)
		type2 = P2INT;
	}
else
	funct = 0;
if(p->memoffset && (!ISICON(p->memoffset) || p->memoffset->const.ci!=0) )
	offp = cpexpr(p->memoffset);
else
	offp = NULL;

#if FUDGEOFFSET != 1
if(offp)
	offp = mkexpr(OPSTAR, ICON(FUDGEOFFSET), offp);
#endif

switch(p->vstg)
	{
	case STGAUTO:
		p2reg(AUTOREG, P2PTR);
		p2offset(type2|P2PTR, offp);
		if(indir)
			p2op2(P2INDIRECT, type2);
		break;

	case STGLENG:
	case STGARG:
		p2reg(ARGREG, type2|P2PTR|((funct?funct:P2PTR)<<2));
		if(p->memno)
			{
			putx( ICON(p->memno) );
			p2op2(P2PLUS, type2|P2PTR|(funct<<2));
			}
		if(p->vstg == STGARG)
			{
			p2op2(P2INDIRECT, type2|P2PTR);
			p2offset(type2|P2PTR|(funct<<2), offp);
			}
		if(indir)
			p2op2(P2INDIRECT, type2|funct);
		break;

	case STGBSS:
	case STGINIT:
	case STGEXT:
	case STGCOMMON:
	case STGEQUIV:
	case STGCONST:
		p2op(P2NAME);
		p2i(P2EXTERN);
		p2i(type2|funct);
		p2str( memname(p->vstg,p->memno) );
		if(!indir || offp!=NULL)
			p2op2(P2ADDR, type2|P2PTR);
		p2offset(type2|P2PTR, offp);
		if(indir && offp!=NULL)
			p2op2(P2INDIRECT, type2);
		break;

	case STGREG:
		if(indir)
			p2reg(p->memno, type2);
		break;

	default:
		fatal1("putaddr: invalid vstg %d", p->vstg);
	}
frexpr(p);
}





LOCAL struct addrblock *putcall(p)
register struct exprblock *p;
{
chainp arglist, charsp, cp;
int first;
struct addrblock *t;
register struct exprblock *q;
struct exprblock *fval;
int type, type2, ctype, indir;

if( (type = p->vtype) == TYLOGICAL)
	type = tylogical;
type2 = types2[type];
charsp = NULL;
first = YES;
indir =  (p->opcode == OPCCALL);

if(p->rightp)
	{
	arglist = p->rightp->listp;
	free(p->rightp);
	}
else
	arglist = NULL;

if(!indir)  for(cp = arglist ; cp ; cp = cp->nextp)
	{
	q = cp->datap;
	if( ISCONST(q) )
		{
		if(q->vtype == TYSHORT)
			q = mkconv(tyint, q);
		cp->datap = q = putconst(q);
		}
	if( ISCHAR(q) )
		charsp = hookup(charsp, mkchain(cpexpr(q->vleng), 0) );
	else if(q->vclass == CLPROC)
		charsp = hookup(charsp, mkchain( ICON(0) , 0));
	}

if(type == TYCHAR)
	{
	if( ISICON(p->vleng) )
		fval = mktemp(TYCHAR, p->vleng);
	else	{
		err("adjustable character function");
		return(NULL);
		}
	}
else if( ISCOMPLEX(type) )
	fval = mktemp(type, NULL);
else
	fval = NULL;

ctype = (fval ? P2INT : type2);
putaddr(p->leftp, YES);

if(fval)
	{
	first = NO;
	putaddr( cpexpr(fval), NO);
	if(type==TYCHAR)
		{
		putx( mkconv(TYLENG, p->vleng) );
		p2op2(P2LISTOP, P2INT);
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
		p2op2(P2LISTOP, P2INT);
	}

if(arglist)
	frchain(&arglist);
for(cp = charsp ; cp ; cp = cp->nextp)
	{
	putx( mkconv(TYLENG, cp->datap) );
	if(first)
		first = NO;
	else
		p2op2(P2LISTOP, P2INT);
	}
frchain(&charsp);

if(first)
	p2op(P2NULL);
p2op2(P2CALL, ctype);
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
	p2op2(P2COMOP, type);
}

/*
 *  routines that put bytes on the pass2 input stream
*/


p2i(k)
int k;
{
register char *s;
s = &k;

putc(*s++, textfile);
putc(*s, textfile);
}




p2op(op)
int op;
{
putc(op, textfile);
putc(0376, textfile);   /* MAGIC NUMBER */
}




p2str(s)
register char *s;
{
do
	putc(*s, textfile);
		while(*s++);
}



p2op2(op, i)
int op, i;
{
p2op(op);
p2i(i);
}



p2reg(k, type)
int k;
{
p2op2(P2NAME, P2REG);
p2i(type);
p2i(k);
}



LOCAL p2li(n)
long int n;
{
register int *p, i;

p = &n;
for(i = 0 ; i< sizeof(long int)/sizeof(int) ; ++i)
	p2i(*p++);
}



LOCAL p2offset(type, offp)
int type;
register expptr offp;
{
expptr shorten();

if(offp)
	{
#if SZINT < SZLONG
	if(shortsubs)
		offp = shorten(offp);
#endif
	if(offp->vtype != TYLONG)
		offp = mkconv(TYINT, offp);
	if(offp->vtype == TYLONG)
		{
		putx(offp);
		p2op2(P2LTOI, P2INT);
		}
	else
		putx( offp );
	p2op2(P2PLUS, type);
	}
}
