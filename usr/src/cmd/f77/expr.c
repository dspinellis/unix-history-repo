#include "defs"

/* little routines to create constant blocks */

struct constblock *mkconst(t)
register int t;
{
register struct constblock *p;

p = ALLOC(constblock);
p->tag = TCONST;
p->vtype = t;
return(p);
}


struct constblock *mklogcon(l)
register int l;
{
register struct constblock * p;

p = mkconst(TYLOGICAL);
p->const.ci = l;
return(p);
}



struct constblock *mkintcon(l)
ftnint l;
{
register struct constblock *p;

p = mkconst(TYLONG);
p->const.ci = l;
#ifdef MAXSHORT
	if(l >= -MAXSHORT   &&   l <= MAXSHORT)
		p->vtype = TYSHORT;
#endif
return(p);
}



struct constblock *mkaddcon(l)
register int l;
{
register struct constblock *p;

p = mkconst(TYADDR);
p->const.ci = l;
return(p);
}



struct constblock *mkrealcon(t, d)
register int t;
double d;
{
register struct constblock *p;

p = mkconst(t);
p->const.cd[0] = d;
return(p);
}


struct constblock *mkbitcon(shift, leng, s)
int shift;
int leng;
char *s;
{
register struct constblock *p;

p = mkconst(TYUNKNOWN);
p->const.ci = 0;
while(--leng >= 0)
	if(*s != ' ')
		p->const.ci = (p->const.ci << shift) | hextoi(*s++);
return(p);
}





struct constblock *mkstrcon(l,v)
int l;
register char *v;
{
register struct constblock *p;
register char *s;

p = mkconst(TYCHAR);
p->vleng = ICON(l);
p->const.ccp = s = (char *) ckalloc(l);
while(--l >= 0)
	*s++ = *v++;
return(p);
}


struct constblock *mkcxcon(realp,imagp)
register expptr realp, imagp;
{
int rtype, itype;
register struct constblock *p;

rtype = realp->vtype;
itype = imagp->vtype;

if( ISCONST(realp) && ISNUMERIC(rtype) && ISCONST(imagp) && ISNUMERIC(itype) )
	{
	p = mkconst( (rtype==TYDREAL||itype==TYDREAL) ? TYDCOMPLEX : TYCOMPLEX );
	if( ISINT(rtype) )
		p->const.cd[0] = realp->const.ci;
	else	p->const.cd[0] = realp->const.cd[0];
	if( ISINT(itype) )
		p->const.cd[1] = imagp->const.ci;
	else	p->const.cd[1] = imagp->const.cd[0];
	}
else
	{
	err("invalid complex constant");
	p = errnode();
	}

frexpr(realp);
frexpr(imagp);
return(p);
}


struct errorblock *errnode()
{
struct errorblock *p;
p = ALLOC(errorblock);
p->tag = TERROR;
p->vtype = TYERROR;
return(p);
}





expptr mkconv(t, p)
register int t;
register expptr p;
{
register expptr q;
register int pt;
expptr opconv();

if(t==TYUNKNOWN || t==TYERROR)
	fatal1("mkconv of impossible type %d", t);
pt = p->vtype;
if(t == pt)
	return(p);

else if( ISCONST(p) && pt!=TYADDR)
	{
	q = mkconst(t);
	consconv(t, &(q->const), p->vtype, &(p->const));
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
	q->vleng = ICON(1);
return(q);
}



expptr opconv(p, t)
expptr p;
int t;
{
register expptr q;

q = mkexpr(OPCONV, p, 0);
q->vtype = t;
return(q);
}



struct exprblock *addrof(p)
expptr p;
{
return( mkexpr(OPADDR, p, NULL) );
}



tagptr cpexpr(p)
register tagptr p;
{
register tagptr e;
int tag;
register chainp ep, pp;
ptr cpblock();

static int blksize[ ] =
	{	0,
		sizeof(struct nameblock),
		sizeof(struct constblock),
		sizeof(struct exprblock),
		sizeof(struct addrblock),
		sizeof(struct primblock),
		sizeof(struct listblock),
		sizeof(struct errorblock)
	};

if(p == NULL)
	return(NULL);

if( (tag = p->tag) == TNAME)
	return(p);

e = cpblock( blksize[p->tag] , p);

switch(tag)
	{
	case TCONST:
		if(e->vtype == TYCHAR)
			{
			e->const.ccp = copyn(1+strlen(e->const.ccp), e->const.ccp);
			e->vleng = cpexpr(e->vleng);
			}
	case TERROR:
		break;

	case TEXPR:
		e->leftp = cpexpr(p->leftp);
		e->rightp = cpexpr(p->rightp);
		break;

	case TLIST:
		if(pp = p->listp)
			{
			ep = e->listp = mkchain( cpexpr(pp->datap), NULL);
			for(pp = pp->nextp ; pp ; pp = pp->nextp)
				ep = ep->nextp = mkchain( cpexpr(pp->datap), NULL);
			}
		break;

	case TADDR:
		e->vleng = cpexpr(e->vleng);
		e->memoffset = cpexpr(e->memoffset);
		e->istemp = NO;
		break;

	case TPRIM:
		e->argsp = cpexpr(e->argsp);
		e->fcharp = cpexpr(e->fcharp);
		e->lcharp = cpexpr(e->lcharp);
		break;

	default:
		fatal1("cpexpr: impossible tag %d", tag);
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
		if( ISCHAR(p) )
			{
			free(p->const.ccp);
			frexpr(p->vleng);
			}
		break;

	case TADDR:
		if(p->istemp)
			{
			frtemp(p);
			return;
			}
		frexpr(p->vleng);
		frexpr(p->memoffset);
		break;

	case TERROR:
		break;

	case TNAME:
		return;

	case TPRIM:
		frexpr(p->argsp);
		frexpr(p->fcharp);
		frexpr(p->lcharp);
		break;

	case TEXPR:
		frexpr(p->leftp);
		if(p->rightp)
			frexpr(p->rightp);
		break;

	case TLIST:
		for(q = p->listp ; q ; q = q->nextp)
			frexpr(q->datap);
		frchain( &(p->listp) );
		break;

	default:
		fatal1("frexpr: impossible tag %d", p->tag);
	}

free(p);
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
		if( ! ONEOF(p->vtype, MSKINT|MSKLOGICAL|MSKADDR) )
			p = putconst(p);
		return(p);

	case TADDR:
		p->memoffset = fixtype(p->memoffset);
		return(p);

	case TERROR:
		return(p);

	default:
		fatal1("fixtype: impossible tag %d", p->tag);

	case TEXPR:
		return( fixexpr(p) );

	case TLIST:
		return( p );

	case TPRIM:
		if(p->argsp && p->namep->vclass!=CLVAR)
			return( mkfunct(p) );
		else	return( mklhs(p) );
	}
}





/* special case tree transformations and cleanups of expression trees */

expptr fixexpr(p)
register struct exprblock *p;
{
expptr lp;
register expptr rp;
register expptr q;
int opcode, ltype, rtype, ptype, mtype;
expptr mkpower();

if(p->tag == TERROR)
	return(p);
else if(p->tag != TEXPR)
	fatal1("fixexpr: invalid tag %d", p->tag);
opcode = p->opcode;
lp = p->leftp = fixtype(p->leftp);
ltype = lp->vtype;
if(opcode==OPASSIGN && lp->tag!=TADDR)
	{
	err("left side of assignment must be variable");
	frexpr(p);
	return( errnode() );
	}

if(p->rightp)
	{
	rp = p->rightp = fixtype(p->rightp);
	rtype = rp->vtype;
	}
else
	{
	rp = NULL;
	rtype = 0;
	}

/* force folding if possible */
if( ISCONST(lp) && (rp==NULL || ISCONST(rp)) )
	{
	q = mkexpr(opcode, lp, rp);
	if( ISCONST(q) )
		return(q);
	free(q);	/* constants did not fold */
	}

if( (ptype = cktype(opcode, ltype, rtype)) == TYERROR)
	{
	frexpr(p);
	return( errnode() );
	}

switch(opcode)
	{
	case OPCONCAT:
		if(p->vleng == NULL)
			p->vleng = mkexpr(OPPLUS, cpexpr(lp->vleng),
				cpexpr(rp->vleng) );
		break;

	case OPASSIGN:
	case OPPLUSEQ:
	case OPSTAREQ:
		if(ltype == rtype)
			break;
		if( ! ISCONST(rp) && ISREAL(ltype) && ISREAL(rtype) )
			break;
		if( ISCOMPLEX(ltype) || ISCOMPLEX(rtype) )
			break;
		if( ONEOF(ltype, MSKADDR|MSKINT) && ONEOF(rtype, MSKADDR|MSKINT)
#if FAMILY==SCJ
		    && typesize[ltype]>=typesize[rtype] )
#else
		    && typesize[ltype]==typesize[rtype] )
#endif
			break;
		p->rightp = fixtype( mkconv(ptype, rp) );
		break;

	case OPSLASH:
		if( ISCOMPLEX(rtype) )
			{
			p = call2(ptype, ptype==TYCOMPLEX? "c_div" : "z_div",
				mkconv(ptype, lp), mkconv(ptype, rp) );
			break;
			}
	case OPPLUS:
	case OPMINUS:
	case OPSTAR:
	case OPMOD:
		if(ptype==TYDREAL && ( (ltype==TYREAL && ! ISCONST(lp) ) ||
		    (rtype==TYREAL && ! ISCONST(rp) ) ))
			break;
		if( ISCOMPLEX(ptype) )
			break;
		if(ltype != ptype)
			p->leftp = fixtype(mkconv(ptype,lp));
		if(rtype != ptype)
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
		if(mtype==TYDREAL && ( (ltype==TYREAL && ! ISCONST(lp)) ||
		    (rtype==TYREAL && ! ISCONST(rp)) ))
			break;
		if( ISCOMPLEX(mtype) )
			break;
		if(ltype != mtype)
			p->leftp = fixtype(mkconv(mtype,lp));
		if(rtype != mtype)
			p->rightp = fixtype(mkconv(mtype,rp));
		break;


	case OPCONV:
		ptype = cktype(OPCONV, p->vtype, ltype);
		if(lp->tag==TEXPR && lp->opcode==OPCOMMA)
			{
			lp->rightp = fixtype( mkconv(ptype, lp->rightp) );
			free(p);
			p = lp;
			}
		break;

	case OPADDR:
		if(lp->tag==TEXPR && lp->opcode==OPADDR)
			fatal("addr of addr");
		break;

	case OPCOMMA:
	case OPQUEST:
	case OPCOLON:
		break;

	case OPMIN:
	case OPMAX:
		ptype = p->vtype;
		break;

	default:
		break;
	}

p->vtype = ptype;
return(p);
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

if(p->vtype != TYLONG)
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
		fatal1("shorten: invalid tag %d", p->tag);
	}

switch(p->opcode)
	{
	case OPPLUS:
	case OPMINUS:
	case OPSTAR:
		q = shorten( cpexpr(p->rightp) );
		if(q->vtype == TYINT)
			{
			p->leftp = shorten(p->leftp);
			if(p->leftp->vtype == TYLONG)
				frexpr(q);
			else
				{
				frexpr(p->rightp);
				p->rightp = q;
				p->vtype = TYINT;
				}
			}
		break;

	case OPNEG:
		p->leftp = shorten(p->leftp);
		if(p->leftp->vtype == TYINT)
			p->vtype = TYINT;
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

fixargs(doput, p0)
int doput;
struct listblock *p0;
{
register chainp p;
register tagptr q, t;
register int qtag;
int nargs;
struct addrblock *mkaddr();

nargs = 0;
if(p0)
    for(p = p0->listp ; p ; p = p->nextp)
	{
	++nargs;
	q = p->datap;
	qtag = q->tag;
	if(qtag == TCONST)
		{
		if(q->vtype == TYSHORT)
			q = mkconv(tyint, q);
		if(doput)
			p->datap = putconst(q);
		else
			p->datap = q;
		}
	else if(qtag==TPRIM && q->argsp==0 && q->namep->vclass==CLPROC)
		p->datap = mkaddr(q->namep);
	else if(qtag==TPRIM && q->argsp==0 && q->namep->vdim!=NULL)
		p->datap = mkscalar(q->namep);
	else if(qtag==TPRIM && q->argsp==0 && q->namep->vdovar && 
		(t = memversion(q->namep)) )
			p->datap = fixtype(t);
	else	p->datap = fixtype(q);
	}
return(nargs);
}


mkscalar(np)
register struct nameblock *np;
{
register struct addrblock *ap;
register struct dimblock *dp;

vardcl(np);
ap = mkaddr(np);

#if TARGET == VAX
	/* on the VAX, prolog causes array arguments
	   to point at the (0,...,0) element, except when
	   subscript checking is on
	*/
	if( !checksubs && np->vstg==STGARG)
		{
		dp = np->vdim;
		frexpr(ap->memoffset);
		ap->memoffset = mkexpr(OPSTAR, ICON(typesize[np->vtype]),
					cpexpr(dp->baseoffset) );
		}
#endif
return(ap);
}





expptr mkfunct(p)
register struct primblock * p;
{
struct entrypoint *ep;
struct addrblock *ap;
struct extsym *mkext(), *extp;
register struct nameblock *np;
register struct exprblock *q;
struct exprblock *intrcall(), *stfcall();
int k, nargs;
int class;

np = p->namep;
class = np->vclass;

if(class == CLUNKNOWN)
	{
	np->vclass = class = CLPROC;
	if(np->vstg == STGUNKNOWN)
		{
		if(k = intrfunct(np->varname))
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
	fatal1("invalid class code for function", class);
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
		q->vtype = np->vtype;
		if(np->vleng)
			q->vleng = cpexpr(np->vleng);
		break;

	case PINTRINSIC:
		q = intrcall(np, p->argsp, nargs);
		break;

	case PSTFUNCT:
		q = stfcall(np, p->argsp);
		break;

	case PTHISPROC:
		warn("recursive call");
		for(ep = entries ; ep ; ep = ep->nextp)
			if(ep->enamep == np)
				break;
		if(ep == NULL)
			fatal("mkfunct: impossible recursion");
		ap = builtin(np->vtype, varstr(XL, ep->entryname->extname) );
		goto call;

	default:
		fatal1("mkfunct: impossible vprocclass %d", np->vprocclass);
	}
free(p);
return(q);

error:
	frexpr(p);
	return( errnode() );
}



LOCAL struct exprblock *stfcall(np, actlist)
struct nameblock *np;
struct listblock *actlist;
{
register chainp actuals;
int nargs;
chainp oactp, formals;
int type;
struct exprblock *q, *rhs;
expptr ap;
register struct rplblock *rp;
struct rplblock *tlist;

if(actlist)
	{
	actuals = actlist->listp;
	free(actlist);
	}
else
	actuals = NULL;
oactp = actuals;

nargs = 0;
tlist = NULL;
type = np->vtype;
formals = np->vardesc.vstfdesc->datap;
rhs = np->vardesc.vstfdesc->nextp;

/* copy actual arguments into temporaries */
while(actuals!=NULL && formals!=NULL)
	{
	rp = ALLOC(rplblock);
	rp->rplnp = q = formals->datap;
	ap = fixtype(actuals->datap);
	if(q->vtype==ap->vtype && q->vtype!=TYCHAR
	   && (ap->tag==TCONST || ap->tag==TADDR) )
		{
		rp->rplvp = ap;
		rp->rplxp = NULL;
		rp->rpltag = ap->tag;
		}
	else	{
		rp->rplvp = mktemp(q->vtype, q->vleng);
		rp->rplxp = fixtype( mkexpr(OPASSIGN, cpexpr(rp->rplvp), ap) );
		if( (rp->rpltag = rp->rplxp->tag) == TERROR)
			err("disagreement of argument types in statement function call");
		}
	rp->nextp = tlist;
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
rpllist = hookup(tlist, rpllist);
q = mkconv(type, fixtype(cpexpr(rhs)) );

/* now generate the tree ( t1=a1, (t2=a2,... , f))))) */
while(--nargs >= 0)
	{
	if(rpllist->rplxp)
		q = mkexpr(OPCOMMA, rpllist->rplxp, q);
	rp = rpllist->nextp;
	frexpr(rpllist->rplvp);
	free(rpllist);
	rpllist = rp;
	}

frchain( &oactp );
return(q);
}




struct addrblock *mklhs(p)
register struct primblock * p;
{
register struct addrblock *s;
expptr suboffset();
struct nameblock *np;
register struct rplblock *rp;
int regn;

/* first fixup name */

if(p->tag != TPRIM)
	return(p);
np = p->namep;

/* is name on the replace list? */

for(rp = rpllist ; rp ; rp = rp->nextp)
	{
	if(np == rp->rplnp)
		{
		if(rp->rpltag == TNAME)
			{
			np = p->namep = rp->rplvp;
			break;
			}
		else	return( cpexpr(rp->rplvp) );
		}
	}

/* is variable a DO index in a register ? */

if(np->vdovar && ( (regn = inregister(np)) >= 0) )
	if(np->vtype == TYERROR)
		return( errnode() );
	else
		{
		s = ALLOC(addrblock);
		s->tag = TADDR;
		s->vstg = STGREG;
		s->vtype = TYIREG;
		s->memno = regn;
		s->memoffset = ICON(0);
		return(s);
		}

vardcl(np);
s = mkaddr(np);
s->memoffset = mkexpr(OPPLUS, s->memoffset, suboffset(p) );
frexpr(p->argsp);
p->argsp = NULL;

/* now do substring part */

if(p->fcharp || p->lcharp)
	{
	if(np->vtype != TYCHAR)
		err1("substring of noncharacter %s", varstr(VL,np->varname));
	else	{
		if(p->lcharp == NULL)
			p->lcharp = cpexpr(s->vleng);
		if(p->fcharp)
			s->vleng = mkexpr(OPMINUS, p->lcharp,
				mkexpr(OPMINUS, p->fcharp, ICON(1) ));
		else	{
			frexpr(s->vleng);
			s->vleng = p->lcharp;
			}
		}
	}

s->vleng = fixtype( s->vleng );
s->memoffset = fixtype( s->memoffset );
free(p);
return(s);
}





deregister(np)
struct nameblock *np;
{
if(nregvar>0 && regnamep[nregvar-1]==np)
	{
	--nregvar;
#if FAMILY == DMR
	putnreg();
#endif
	}
}




struct addrblock *memversion(np)
register struct nameblock *np;
{
register struct addrblock *s;

if(np->vdovar==NO || (inregister(np)<0) )
	return(NULL);
np->vdovar = NO;
s = mklhs( mkprim(np, 0,0,0) );
np->vdovar = YES;
return(s);
}



inregister(np)
register struct nameblock *np;
{
register int i;

for(i = 0 ; i < nregvar ; ++i)
	if(regnamep[i] == np)
		return( regnum[i] );
return(-1);
}




enregister(np)
struct nameblock *np;
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
register struct primblock *p;
{
int n;
expptr size;
chainp cp;
expptr offp, prod;
expptr subcheck();
struct dimblock *dimp;
expptr sub[8];
register struct nameblock *np;

np = p->namep;
offp = ICON(0);
n = 0;
if(p->argsp)
	for(cp = p->argsp->listp ; cp ; cp = cp->nextp)
		{
		sub[n++] = fixtype(cpexpr(cp->datap));
		if(n > 7)
			{
			err("more than 7 subscripts");
			break;
			}
		}

dimp = np->vdim;
if(n>0 && dimp==NULL)
	err("subscripts on scalar variable");
else if(dimp && dimp->ndim!=n)
	err1("wrong number of subscripts on %s",
		varstr(VL, np->varname) );
else if(n > 0)
	{
	prod = sub[--n];
	while( --n >= 0)
		prod = mkexpr(OPPLUS, sub[n],
			mkexpr(OPSTAR, prod, cpexpr(dimp->dims[n].dimsize)) );
#if TARGET == VAX
	if(checksubs || np->vstg!=STGARG)
		prod = mkexpr(OPMINUS, prod, cpexpr(dimp->baseoffset));
#else
	prod = mkexpr(OPMINUS, prod, cpexpr(dimp->baseoffset));
#endif
	if(checksubs)
		prod = subcheck(np, prod);
	if(np->vtype == TYCHAR)
		size = cpexpr(np->vleng);
	else	size = ICON( typesize[np->vtype] );
	prod = mkexpr(OPSTAR, prod, size);
	offp = mkexpr(OPPLUS, offp, prod);
	}

if(p->fcharp && np->vtype==TYCHAR)
	offp = mkexpr(OPPLUS, offp, mkexpr(OPMINUS, cpexpr(p->fcharp), ICON(1) ));

return(offp);
}




expptr subcheck(np, p)
struct nameblock *np;
register expptr p;
{
struct dimblock *dimp;
expptr t, checkvar, checkcond, badcall;

dimp = np->vdim;
if(dimp->nelt == NULL)
	return(p);	/* don't check arrays with * bounds */
checkvar = NULL;
checkcond = NULL;
if( ISICON(p) )
	{
	if(p->const.ci < 0)
		goto badsub;
	if( ISICON(dimp->nelt) )
		if(p->const.ci < dimp->nelt->const.ci)
			return(p);
		else
			goto badsub;
	}
if(p->tag==TADDR && p->vstg==STGREG)
	{
	checkvar = cpexpr(p);
	t = p;
	}
else	{
	checkvar = mktemp(p->vtype, NULL);
	t = mkexpr(OPASSIGN, cpexpr(checkvar), p);
	}
checkcond = mkexpr(OPLT, t, cpexpr(dimp->nelt) );
if( ! ISICON(p) )
	checkcond = mkexpr(OPAND, checkcond,
			mkexpr(OPLE, ICON(0), cpexpr(checkvar)) );

badcall = call4(p->vtype, "s_rnge", mkstrcon(VL, np->varname),
		mkconv(TYLONG,  cpexpr(checkvar)),
		mkstrcon(XL, procname), ICON(lineno));
badcall->opcode = OPCCALL;
p = mkexpr(OPQUEST, checkcond,
	mkexpr(OPCOLON, checkvar, badcall));

return(p);

badsub:
	frexpr(p);
	err1("subscript on variable %s out of range", varstr(VL,np->varname));
	return ( ICON(0) );
}




struct addrblock *mkaddr(p)
register struct nameblock *p;
{
struct extsym *mkext(), *extp;
register struct addrblock *t;
struct addrblock *intraddr();

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
		t = ALLOC(addrblock);
		t->tag = TADDR;
		if(p->vclass==CLPROC && p->vprocclass==PTHISPROC)
			t->vclass = CLVAR;
		else
			t->vclass = p->vclass;
		t->vtype = p->vtype;
		t->vstg = p->vstg;
		t->memno = p->vardesc.varno;
		t->memoffset = ICON(p->voffset);
		if(p->vleng)
			t->vleng = cpexpr(p->vleng);
		return(t);

	case STGINTR:
		return( intraddr(p) );

	}
/*debug*/ fprintf(diagfile, "mkaddr. vtype=%d, vclass=%d\n", p->vtype, p->vclass);
fatal1("mkaddr: impossible storage tag %d", p->vstg);
/* NOTREACHED */
}




mkarg(type, argno)
int type, argno;
{
register struct addrblock *p;

p = ALLOC(addrblock);
p->tag = TADDR;
p->vtype = type;
p->vclass = CLVAR;
p->vstg = (type==TYLENG ? STGLENG : STGARG);
p->memno = argno;
return(p);
}




tagptr mkprim(v, args, lstr, rstr)
register union { struct paramblock; struct nameblock; } *v;
struct listblock *args;
expptr lstr, rstr;
{
register struct primblock *p;

if(v->vclass == CLPARAM)
	{
	if(args || lstr || rstr)
		{
		err1("no qualifiers on parameter name", varstr(VL,v->varname));
		frexpr(args);
		frexpr(lstr);
		frexpr(rstr);
		frexpr(v);
		return( errnode() );
		}
	return( cpexpr(v->paramval) );
	}

p = ALLOC(primblock);
p->tag = TPRIM;
p->vtype = v->vtype;
p->namep = v;
p->argsp = args;
p->fcharp = lstr;
p->lcharp = rstr;
return(p);
}



vardcl(v)
register struct nameblock *v;
{
int nelt;
struct dimblock *t;
struct addrblock *p;
expptr neltp;

if(v->vdcldone) return;

if(v->vtype == TYUNKNOWN)
	impldcl(v);
if(v->vclass == CLUNKNOWN)
	v->vclass = CLVAR;
else if(v->vclass!=CLVAR && v->vprocclass!=PTHISPROC)
	{
	dclerr("used as variable", v);
	return;
	}
if(v->vstg==STGUNKNOWN)
	v->vstg = implstg[ letter(v->varname[0]) ];

switch(v->vstg)
	{
	case STGBSS:
		v->vardesc.varno = ++lastvarno;
		break;
	case STGAUTO:
		if(v->vclass==CLPROC && v->vprocclass==PTHISPROC)
			break;
		nelt = 1;
		if(t = v->vdim)
			if( (neltp = t->nelt) && ISCONST(neltp) )
				nelt = neltp->const.ci;
			else
				dclerr("adjustable automatic array", v);
		p = autovar(nelt, v->vtype, v->vleng);
		v->voffset = p->memoffset->const.ci;
		frexpr(p);
		break;

	default:
		break;
	}
v->vdcldone = YES;
}




impldcl(p)
register struct nameblock *p;
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

#define ICONEQ(z, c)  (ISICON(z) && z->const.ci==c)
#define COMMUTE	{ e = lp;  lp = rp;  rp = e; }


expptr mkexpr(opcode, lp, rp)
int opcode;
register expptr lp, rp;
{
register struct exprblock *e, *e1;
int etype;
int ltype, rtype;
int ltag, rtag;
expptr fold();

ltype = lp->vtype;
ltag = lp->tag;
if(rp && opcode!=OPCALL && opcode!=OPCCALL)
	{
	rtype = rp->vtype;
	rtag = rp->tag;
	}
else  rtype = 0;

etype = cktype(opcode, ltype, rtype);
if(etype == TYERROR)
	goto error;

switch(opcode)
	{
	/* check for multiplication by 0 and 1 and addition to 0 */

	case OPSTAR:
		if( ISCONST(lp) )
			COMMUTE

		if( ISICON(rp) )
			{
			if(rp->const.ci == 0)
				goto retright;
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
			if(rp->const.ci == 1)
				goto retleft;

			if(rp->const.ci == -1)
				{
				frexpr(rp);
				return( mkexpr(OPNEG, lp, 0) );
				}
			}

		if( ISSTAROP(lp) && ISICON(lp->rightp) )
			{
			if(opcode == OPSTAR)
				e = mkexpr(OPSTAR, lp->rightp, rp);
			else  if(ISICON(rp) && lp->rightp->const.ci % rp->const.ci == 0)
				e = mkexpr(OPSLASH, lp->rightp, rp);
			else	break;

			e1 = lp->leftp;
			free(lp);
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
			return( mkexpr(OPNEG, rp, 0) );
			}

		if( ISCONST(rp) )
			{
			opcode = OPPLUS;
			consnegop(rp);
			}

	addop:
		if( ISICON(rp) )
			{
			if(rp->const.ci == 0)
				goto retleft;
			if( ISPLUSOP(lp) && ISICON(lp->rightp) )
				{
				e = mkexpr(OPPLUS, lp->rightp, rp);
				e1 = lp->leftp;
				free(lp);
				return( mkexpr(OPPLUS, e1, e) );
				}
			}
		break;


	case OPPOWER:
		break;

	case OPNEG:
		if(ltag==TEXPR && lp->opcode==OPNEG)
			{
			e = lp->leftp;
			free(lp);
			return(e);
			}
		break;

	case OPNOT:
		if(ltag==TEXPR && lp->opcode==OPNOT)
			{
			e = lp->leftp;
			free(lp);
			return(e);
			}
		break;

	case OPCALL:
	case OPCCALL:
		etype = ltype;
		if(rp!=NULL && rp->listp==NULL)
			{
			free(rp);
			rp = NULL;
			}
		break;

	case OPAND:
	case OPOR:
		if( ISCONST(lp) )
			COMMUTE

		if( ISCONST(rp) )
			{
			if(rp->const.ci == 0)
				if(opcode == OPOR)
					goto retleft;
				else
					goto retright;
			else if(opcode == OPOR)
				goto retright;
			else
				goto retleft;
			}
	case OPEQV:
	case OPNEQV:

	case OPBITAND:
	case OPBITOR:
	case OPBITXOR:
	case OPBITNOT:
	case OPLSHIFT:
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
		break;

	default:
		fatal1("mkexpr: impossible opcode %d", opcode);
	}

e = ALLOC(exprblock);
e->tag = TEXPR;
e->opcode = opcode;
e->vtype = etype;
e->leftp = lp;
e->rightp = rp;
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
	if(op!=OPNOT && op!=OPBITNOT && op!=OPNEG && op!=OPCALL && op!=OPCCALL && op!=OPADDR)
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
		return(lt);

	case OPCOMMA:
	case OPQUEST:
	case OPCOLON:
		return(rt);

	default:
		fatal1("cktype: impossible opcode %d", op);
	}
error:	err(errs);
error1:	return(TYERROR);
}

LOCAL expptr fold(e)
register struct exprblock *e;
{
struct constblock *p;
#ifdef VERSION6
	expptr lp, rp;
#else
	register expptr lp, rp;
#endif
int etype, mtype, ltype, rtype, opcode;
int i, ll, lr;
char *q, *s;
union constant lcon, rcon;

opcode = e->opcode;
etype = e->vtype;

lp = e->leftp;
ltype = lp->vtype;
rp = e->rightp;

if(rp == 0)
	switch(opcode)
		{
		case OPNOT:
			lp->const.ci = ! lp->const.ci;
			return(lp);

		case OPBITNOT:
			lp->const.ci = ~ lp->const.ci;
			return(lp);

		case OPNEG:
			consnegop(lp);
			return(lp);

		case OPCONV:
		case OPADDR:
			return(e);

		default:
			fatal1("fold: invalid unary operator %d", opcode);
		}

rtype = rp->vtype;

p = ALLOC(constblock);
p->tag = TCONST;
p->vtype = etype;
p->vleng = e->vleng;

switch(opcode)
	{
	case OPCOMMA:
	case OPQUEST:
	case OPCOLON:
		return(e);

	case OPAND:
		p->const.ci = lp->const.ci && rp->const.ci;
		break;

	case OPOR:
		p->const.ci = lp->const.ci || rp->const.ci;
		break;

	case OPEQV:
		p->const.ci = lp->const.ci == rp->const.ci;
		break;

	case OPNEQV:
		p->const.ci = lp->const.ci != rp->const.ci;
		break;

	case OPBITAND:
		p->const.ci = lp->const.ci & rp->const.ci;
		break;

	case OPBITOR:
		p->const.ci = lp->const.ci | rp->const.ci;
		break;

	case OPBITXOR:
		p->const.ci = lp->const.ci ^ rp->const.ci;
		break;

	case OPLSHIFT:
		p->const.ci = lp->const.ci << rp->const.ci;
		break;

	case OPRSHIFT:
		p->const.ci = lp->const.ci >> rp->const.ci;
		break;

	case OPCONCAT:
		ll = lp->vleng->const.ci;
		lr = rp->vleng->const.ci;
		p->const.ccp = q = (char *) ckalloc(ll+lr);
		p->vleng = ICON(ll+lr);
		s = lp->const.ccp;
		for(i = 0 ; i < ll ; ++i)
			*q++ = *s++;
		s = rp->const.ccp;
		for(i = 0; i < lr; ++i)
			*q++ = *s++;
		break;


	case OPPOWER:
		if( ! ISINT(rtype) )
			return(e);
		conspower(&(p->const), lp, rp->const.ci);
		break;


	default:
		if(ltype == TYCHAR)
			{
			lcon.ci = cmpstr(lp->const.ccp, rp->const.ccp,
					lp->vleng->const.ci, rp->vleng->const.ci);
			rcon.ci = 0;
			mtype = tyint;
			}
		else	{
			mtype = maxtype(ltype, rtype);
			consconv(mtype, &lcon, ltype, &(lp->const) );
			consconv(mtype, &rcon, rtype, &(rp->const) );
			}
		consbinop(opcode, mtype, &(p->const), &lcon, &rcon);
		break;
	}

frexpr(e);
return(p);
}



/* assign constant l = r , doing coercion */

consconv(lt, lv, rt, rv)
int lt, rt;
register union constant *lv, *rv;
{
switch(lt)
	{
	case TYCHAR:
		*(lv->ccp = ckalloc(1)) = rv->ci;
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
		break;

	case TYLOGICAL:
		lv->ci = rv->ci;
		break;
	}
}



consnegop(p)
register struct constblock *p;
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
		fatal1("consnegop: impossible type %d", p->vtype);
	}
}



LOCAL conspower(powp, ap, n)
register union constant *powp;
struct constblock *ap;
ftnint n;
{
register int type;
union constant x;

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
		fatal1("conspower: invalid type %d", type);
	}

if(n == 0)
	return;
if(n < 0)
	{
	if( ISINT(type) )
		{
		err("integer ** negative power ");
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
register union constant *ap, *bp, *cp;
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
			}
		break;
	}
}




conssgn(p)
register expptr p;
{
if( ! ISCONST(p) )
	fatal( "sgn(nonconstant)" );

switch(p->vtype)
	{
	case TYSHORT:
	case TYLONG:
		if(p->const.ci > 0) return(1);
		if(p->const.ci < 0) return(-1);
		return(0);

	case TYREAL:
	case TYDREAL:
		if(p->const.cd[0] > 0) return(1);
		if(p->const.cd[0] < 0) return(-1);
		return(0);

	case TYCOMPLEX:
	case TYDCOMPLEX:
		return(p->const.cd[0]!=0 || p->const.cd[1]!=0);

	default:
		fatal1( "conssgn(type %d)", p->vtype);
	}
/* NOTREACHED */
}

char *powint[ ] = { "pow_ii", "pow_ri", "pow_di", "pow_ci", "pow_zi" };


LOCAL expptr mkpower(p)
register struct exprblock *p;
{
register expptr q, lp, rp;
int ltype, rtype, mtype;

lp = p->leftp;
rp = p->rightp;
ltype = lp->vtype;
rtype = rp->vtype;

if(ISICON(rp))
	{
	if(rp->const.ci == 0)
		{
		frexpr(p);
		if( ISINT(ltype) )
			return( ICON(1) );
		else
			return( putconst( mkconv(ltype, ICON(1))) );
		}
	if(rp->const.ci < 0)
		{
		if( ISINT(ltype) )
			{
			frexpr(p);
			err("integer**negative");
			return( errnode() );
			}
		rp->const.ci = - rp->const.ci;
		p->leftp = lp = fixexpr(mkexpr(OPSLASH, ICON(1), lp));
		}
	if(rp->const.ci == 1)
		{
		frexpr(rp);
		free(p);
		return(lp);
		}

	if( ONEOF(ltype, MSKINT|MSKREAL) )
		{
		p->vtype = ltype;
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
	q = call2(mtype, "pow_dd",
		mkconv(TYDREAL,lp), mkconv(TYDREAL,rp));
else	{
	q = call2(TYDCOMPLEX, "pow_zz",
		mkconv(TYDCOMPLEX,lp), mkconv(TYDCOMPLEX,rp));
	if(mtype == TYCOMPLEX)
		q = mkconv(TYCOMPLEX, q);
	}
free(p);
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
