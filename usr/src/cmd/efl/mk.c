#include "defs"


ptr mkcomm(s)
register char *s;
{
register ptr p;
register char *t;

for(p = commonlist ; p ; p = p->nextp)
	if(equals(s, p->datap->comname))
		return(p->datap);

p = ALLOC(comentry);
for(t = p->comname ; *t++ = *s++ ; ) ;
p->tag = TCOMMON;
p->blklevel = (blklevel>0? 1 : 0);
commonlist = mkchain(p, commonlist);
return(commonlist->datap);
}




ptr mkname(s)
char *s;
{
char *copys();
register ptr p;

if( (p = name(s,1)) == 0)
	{
	p = name(s,0);
	p->tag = TNAME;
	p->blklevel = blklevel;
	}
return(p);
}

ptr mknode(t, o, l, r)
int t,o;
register ptr l;
register ptr r;
{
register struct exprblock *p;
ptr q;
int lt, rt;
int ll, rl;
ptr mksub1(), mkchcon();

p = allexpblock();
TEST fprintf(diagfile, "mknode(%d,%d,%o,%o) = %o\n", t, o, l, r, p);

top:
	if(t!=TLIST && t!=TCONST && l!=0 && l->tag==TERROR)
		{
		frexpr(r);
		frexpblock(p);
		return(l);
		}

	if(r!=0 && r->tag==TERROR)
		{
		frexpr(l);
		frexpblock(p);
		return(r);
		}
	p->tag = t;
	p->subtype = o;
	p->leftp = l;
	p->rightp = r;

switch(t)
	{
	case TAROP:
		ckdcl(l);
		ckdcl(r);
		switch(lt = l->vtype)
			{
			case TYCHAR:
			case TYSTRUCT:
			case TYLOG:
				exprerr("non-arithmetic operand of arith op","");
				goto err;
			}

		switch(rt = r->vtype)
			{
			case TYCHAR:
			case TYSTRUCT:
			case TYLOG:
				exprerr("non-arithmetic operand of arith op","");
				goto err;
			}
		if(lt==rt || (o==OPPOWER && rt==TYINT) )
			p->vtype = lt;
		else if( (lt==TYREAL && rt==TYLREAL) ||
			(lt==TYLREAL && rt==TYREAL) )
				p->vtype = TYLREAL;
		else if(lt==TYINT)
			{
			l = coerce(rt,l);
			goto top;
			}
		else if(rt==TYINT)
			{
			r = coerce(lt,r);
			goto top;
			}
		else if( (lt==TYREAL && rt==TYCOMPLEX) ||
			 (lt==TYCOMPLEX && rt==TYREAL) )
			p->vtype = TYCOMPLEX;
		else if( (lt==TYLREAL && rt==TYCOMPLEX) ||
			 (lt==TYCOMPLEX && rt==TYLREAL) )
			p->vtype = TYLCOMPLEX;
		else	{
			exprerr("mixed mode", CNULL);
			goto err;
			}

		if( (o==OPPLUS||o==OPSTAR) && l->tag==TCONST && r->tag!=TCONST )
			{
			p->leftp = r;
			p->rightp = l;
			}

		if(o==OPPLUS && l->tag==TNEGOP &&
		  (r->tag!=TCONST || l->leftp->tag==TCONST) )
			{
			p->subtype = OPMINUS;
			p->leftp = r;
			p->rightp = l->leftp;
			}

		break;

	case TRELOP:
		ckdcl(l);
		ckdcl(r);
		p->vtype = TYLOG;
		lt = l->vtype;
		rt = r->vtype;
		if(lt==TYCHAR || rt==TYCHAR)
			{
			if(l->vtype != r->vtype)
				{
				exprerr("comparison of character and noncharacter data",CNULL);
				goto err;
				}
			ll = conval(l->vtypep);
			rl = conval(r->vtypep);
			if( (o==OPEQ || o==OPNE) &&
				( (ll==1 && rl==1 && tailor.charcomp==1)
				|| (ll<=tailor.ftnchwd && rl<=tailor.ftnchwd
				&& tailor.charcomp==2) ))
				{
				if(l->tag == TCONST)
					{
					q = cpexpr( mkchcon(l->leftp) );
					frexpr(l);
					l = q;
					}
				if(r->tag == TCONST)
					{
					q = cpexpr( mkchcon(r->leftp) );
					frexpr(r);
					r = q;
					}
				if(l->vsubs == 0)
					l->vsubs = mksub1();
				if(r->vsubs == 0)
					r->vsubs = mksub1();
				p->leftp = l;
				p->rightp = r;
				}
			else	{
				p->leftp = mkcall(builtin(TYINT,"ef1cmc"), arg4(l,r));
				p->rightp = mkint(0);
				}
			}

		else if(lt==TYLOG || rt==TYLOG)
			exprerr("relational involving logicals", CNULL);
		else if( (lt==TYCOMPLEX || rt==TYCOMPLEX) &&
			o!=OPEQ && o!=OPNE)
				exprerr("order comparison of complex numbers", CNULL);
		else if(lt != rt)
			{
			if(lt==TYINT)
				p->leftp = coerce(rt, l);
			else if(rt == TYINT)
				p->rightp = coerce(lt, r);
			}
		break;

	case TLOGOP:
		ckdcl(l);
		ckdcl(r);
		if(r->vtype != TYLOG)
			{
			exprerr("non-logical operand of logical operator",CNULL);
			goto err;
			}
	case TNOTOP:
		ckdcl(l);
		if(l->vtype != TYLOG)
			{
			exprerr("non-logical operand of logical operator",CNULL);
			}
		p->vtype = TYLOG;
		break;

	case TNEGOP:
		ckdcl(l);
		lt = l->vtype;
		if(lt!=TYINT && lt!=TYREAL && lt!=TYLREAL && lt!=TYCOMPLEX)
			{
			exprerr("impossible unary + or - operation",CNULL);
			goto err;
			}
		p->vtype = lt;
		break;

	case TCALL:
		p->vtype = l->vtype;
		p->vtypep = l->vtypep;
		break;

	case TASGNOP:
		ckdcl(l);
		ckdcl(r);
		lt = l->vtype;
		if(lt==TYFIELD)
			lt = TYINT;
		rt = r->vtype;
		if(lt==TYCHAR || rt==TYCHAR || lt==TYLOG || rt==TYLOG)
			{
			if(lt != rt)
				{
				exprerr("illegal assignment",CNULL);
				goto err;
				}
			}
		else if(lt==TYSTRUCT || rt==TYSTRUCT)
			{
			if(lt!=rt || l->vtypep->strsize!=r->vtypep->strsize
				|| l->vtypep->stralign!=r->vtypep->stralign)
				{
				exprerr("illegal structure assignment",CNULL);
				goto err;
				}
			}
		else if ( (lt==TYCOMPLEX || rt==TYCOMPLEX) && lt!=rt)
/*			p->rightp = r = coerce(lt, r) */ ;

		p->vtype = lt;
		p->vtypep = l->vtypep;
		break;

	case TCONST:
	case TLIST:
	case TREPOP:
		break;

	default:
		badtag("mknode", t);
	}

return(p);

err:	frexpr(p);
	return( errnode() );
}



ckdcl(p)
ptr p;
{
if(p->vtype==TYUNDEFINED || (p->tag==TNAME&&p->vdcldone==0&&p->vadjdim==0))
	{
/*debug*/ printf("tag=%d, typed=%d\n", p->tag, p->vtype);
	fatal("untyped subexpression");
	}
if(p->tag==TNAME) setvproc(p,PROCNO);
}

ptr mkvar(p)
register ptr p;
{
register ptr q;

TEST fprintf(diagfile, "mkvar(%s), blk %d\n", p->namep, blklevel);

if(p->blklevel > blklevel)
	p->blklevel = blklevel;

if(instruct || p->varp==0 || p->varp->blklevel<blklevel)
	{
	q = allexpblock();
	q->tag = TNAME;
	q->sthead = p;
	q->blklevel = blklevel;
	if(! instruct)
		++ndecl[blklevel];
	}
else q = p->varp;

if(!instruct)
	{
	if(p->varp && p->varp->blklevel<blklevel)
		hide(p);
	if(p->varp == 0)
		p->varp = q;
	}

p->tag = TNAME;
return(q);
}


ptr mkstruct(v,s)
register ptr v;
ptr s;
{
register ptr p;

p = ALLOC(typeblock);
p->sthead = v;
p->tag = TSTRUCT;
p->blklevel = blklevel;
p->strdesc = s;
offsets(p);
if(v)	{
	v->blklevel = blklevel;
	++ndecl[blklevel];
	v->varp = p;
	}
else	temptypelist = mkchain(p, temptypelist);
return(p);
}


ptr mkcall(fn1, args)
ptr fn1, args;
{
int i, j, first;
register ptr funct, p, q;
ptr r;

if(fn1->tag == TERROR)
	return( errnode() );
else if(fn1->tag == TNAME)
	{
	funct = fn1->sthead->varp;
	frexpblock(fn1);
	}
else
	funct = fn1;
if(funct->vclass!=0 && funct->vclass!=CLARG)
	{
	exprerr("invalid invocation of %s",funct->sthead->namep);
	frexpr(args);
	return( errnode() );
	}
else	extname(funct);

if(args)  for(p = args->leftp; p ; p = p->nextp)
	{
	q = p->datap;
	if( (q->tag==TCALL&&q->vtype==TYUNDEFINED) ||
	    (q->tag==TNAME&&q->vdcldone==0) )
		dclit(q);
	if(q->tag==TNAME && q->vproc==PROCUNKNOWN)
		setvproc(q, PROCNO);
	if( q->vtype == TYSTRUCT)
		{
		first = 1;
		for(i = 0; i<NFTNTYPES ; ++i)
			if(q->vbase[i] != 0)
				{
				r = cpexpr(q);
				if(first)
					{
					p->datap = r;
					first = 0;
					}
				else	p = p->nextp = mkchain(r, p->nextp);
				r->vtype = ftnefl[i];
				for(j=0; j<NFTNTYPES; ++j)
					if(i != j) r->vbase[j] = 0;
				}
		frexpblock(q);
		}
	}

return( mknode(TCALL,0,cpexpr(funct), args) );
}



mkcase(p,here)
ptr p;
int here;
{
register ptr q, s;

for(s = thisctl ; s!=0 && s->subtype!=STSWITCH ; s = s->prevctl)
	;
if(s==0 || (here && s!=thisctl) )
	{
	laberr("invalid case label location",CNULL);
	return(0);
	}
for(q = s->loopctl ; q!=0 && !eqcon(p,q->casexpr) ; q = q->nextcase)
	;
if(q == 0)
	{
	q = ALLOC(caseblock);
	q->tag = TCASE;
	q->casexpr = p;
	q->labelno = ( here ? thislab() : nextlab() );
	q->nextcase = s->loopctl;
	s->loopctl = q;
	}
else if(here)
	if(thisexec->labelno == 0)
		thisexec->labelno = q->labelno;
	else if(thisexec->labelno != q->labelno)
		{
		exnull();
		thisexec->labelno = q->labelno;
		thisexec->labused = 0;
		}
if(here)
	if(q->labdefined)
		laberr("multiply defined case",CNULL);
	else
		q->labdefined = 1;
return(q->labelno);
}


ptr mkilab(p)
ptr p;
{
char *s, l[30];

if(p->tag!=TCONST || p->vtype!=TYINT)
	{
	execerr("invalid label","");
	s = "";
	}
else	s = p->leftp;

while(*s == '0')
	++s;
sprintf(l,"#%s", s);


TEST fprintf(diagfile,"numeric label = %s\n", l);
return( mkname(l) );
}




mklabel(p,here)
ptr p;
int here;
{
register ptr q;

if(q = p->varp)
	{
	if(q->tag != TLABEL)
		laberr("%s is already a nonlabel\n", p->namep);
	else if(q->labinacc)
		warn1("label %s is inaccessible", p->namep);
	else if(here)
		if(q->labdefined)
			laberr("%s is already defined\n", p->namep);
		else if(blklevel > q->blklevel)
			laberr("%s is illegally placed\n",p->namep);
		else	{
			q->labdefined = 1;
			if(thisexec->labelno == 0)
				thisexec->labelno = q->labelno;
			else if(thisexec->labelno != q->labelno)
				{
				exnull();
				thisexec->labelno = q->labelno;
				thisexec->labused = 0;
				}
			}
	}
else	{
	q = ALLOC(labelblock);
	p->varp = q;
	q->tag = TLABEL;
	q->subtype = 0;
	q->blklevel = blklevel;
	++ndecl[blklevel];
	q->labdefined = here;
	q->labelno = ( here ? thislab() : nextlab() );
	q->sthead = p;
	}

return(q->labelno);
}


thislab()
{
if(thisexec->labelno == 0)
	thisexec->labelno = nextlab();
return(thisexec->labelno);
}


nextlab()
{
stnos[++labno] = 0;
return( labno );
}


nextindif()
{
if(++nxtindif < MAXINDIFS)
	return(nxtindif);
fatal("too many indifs");
}




mkkeywd(s, n)
char *s;
int n;
{
register ptr p;
register ptr q;

p = name(s, 2);
q = ALLOC(keyblock);
p->tag = TKEYWORD;
q->tag = TKEYWORD;
p->subtype = n;
q->subtype = n;
p->blklevel = 0;
p->varp = q;
q->sthead = p;
}


ptr mkdef(s, v)
char *s, *v;
{
register ptr p;
register ptr q;

if(p = name(s,1))
	if(p->blklevel == 0)
		{
		if(blklevel > 0)
			hide(p);
		else if(p->tag != TDEFINE)
			dclerr("attempt to DEFINE a variable name", s);
		else	{
			if( strcmp(v, (q=p->varp) ->valp) )
				{
				warn("macro value replaced");
				cfree(q->valp);
				q->valp = copys(v);
				}
			return(p);
			}
		}
	else	{
		dclerr("type already defined", s);
		return( errnode() );
		}
else   p = name(s,0);

q = ALLOC(defblock);
p->tag = TDEFINE;
q->tag = TDEFINE;
p->blklevel = q->blklevel = (blklevel==0 ? 0 : 1);
q->sthead = p;
p->varp = q;
p->varp->valp = copys(v);
return(p);
}



mkknown(s,t)
char *s;
int t;
{
register ptr p;

p = ALLOC(knownname);
p->nextfunct = knownlist;
p->tag = TKNOWNFUNCT;
knownlist = p;
p->funcname = s;
p->functype = t;
}







ptr mkint(k)
int k;
{
return( mkconst(TYINT, convic(k) ) );
}


ptr mkconst(t,p)
int t;
ptr p;
{
ptr q;

q = mknode(TCONST, 0, copys(p), PNULL);
q->vtype = t;
if(t == TYCHAR)
	q->vtypep = mkint( strlen(p) );
return(q);
}



ptr mkimcon(t,p)
int t;
char *p;
{
ptr q;
char *zero, buff[100];

zero = (t==TYCOMPLEX ? "0." : "0d0");
sprintf(buff, "(%s,%s)", zero, p);
q = mknode(TCONST, 0, copys(buff), PNULL);
q->vtype = t;
return(q);
}



ptr mkarrow(p,t)
register ptr p;
ptr t;
{
register ptr q, s;

if(p->vsubs == 0)
	if(p->vdim==0 && p->vtype!=TYCHAR && p->vtype!=TYSTRUCT)
		{
		exprerr("need an aggregate to the left of arrow",CNULL);
		frexpr(p);
		return( errnode() );
		}
	else	{
		if(p->vdim)
			{
			s = 0;
			for(q = p->vdim->datap ; q ; q = q->nextp)
				s = mkchain( mkint(1), s);
			subscript(p, mknode(TLIST,0,s,PNULL) );
			}
		}

p->vtype = TYSTRUCT;
p->vtypep = t->varp;
return(p);
}





mkequiv(p)
ptr p;
{
ptr q, t;
int first;

swii(iefile);
putic(ICBEGIN, 0);
putic(ICINDENT, 0);
putic(ICKEYWORD, FEQUIVALENCE);
putic(ICOP, OPLPAR);
first = 1;

for(q = p ; q ; q = q->nextp)
	{
	if(first)  first = 0;
	else putic(ICOP, OPCOMMA);
	prexpr( t =  simple(LVAL,q->datap) );
	frexpr(t);
	}

putic(ICOP, OPRPAR);
swii(icfile);
frchain( &p );
}




mkgeneric(gname,atype,fname,ftype)
char *gname, *fname;
int atype, ftype;
{
register ptr p;
ptr generic();

if(p = generic(gname))
	{
	if(p->genfname[atype])
		fatal1("generic name already defined", gname);
	}
else	{
	p = ALLOC(genblock);
	p->tag = TGENERIC;
	p->nextgenf = generlist;
	generlist = p;
	p->genname = gname;
	}

p->genfname[atype] = fname;
p->genftype[atype] = ftype;
}


ptr generic(s)
char *s;
{
register ptr p;

for(p= generlist; p ; p = p->nextgenf)
	if(equals(s, p->genname))
		return(p);
return(0);
}


knownfunct(s)
char *s;
{
register ptr p;

for(p = knownlist ; p ; p = p->nextfunct)
	if(equals(s, p->funcname))
		return(p->functype);
return(0);
}





ptr funcinv(p)
register ptr p;
{
ptr fp, fp1;
register ptr g;
char *s;
register int t;
int vt;

if(g = generic(s = p->leftp->sthead->namep))
	{
	if(p->rightp->tag==TLIST && p->rightp->leftp
		&& ( (vt = typearg(p->rightp->leftp)) >=0)
		&& (t = g->genftype[vt]) )
		{
		p->leftp = builtin(t, g->genfname[vt]);
		}
	else	{
		dclerr("improper use of generic function", s);
		frexpr(p);
		return( errnode() );
		}
	}

fp = p->leftp;
setvproc(fp, PROCYES);
fp1 = fp->sthead->varp;
s = fp->sthead->namep;

if(p->vtype==TYUNDEFINED && fp->vclass!=CLARG)
	if(t = knownfunct(s))
		{
		p->vtype = t;
		setvproc(fp, PROCINTRINSIC);
		setvproc(fp1, PROCINTRINSIC);
		fp1->vtype = t;
		builtin(t,fp1->sthead->namep);
		cpblock(fp1, fp, sizeof(struct exprblock));
		}

dclit(p);
return(p);
}




typearg(p0)
register chainp p0;
{
register chainp p;
register int vt, maxt;

if(p0 == NULL)
	return(-1);
maxt = p0->datap->vtype;

for(p = p0->nextp ; p ; p = p->nextp)
	if( (vt = p->datap->vtype) > maxt)
		maxt = vt;

for(p = p0 ; p ; p = p->nextp)
	p->datap = coerce(maxt, p->datap);

return(maxt);
}




ptr typexpr(t,e)
register ptr t, e;
{
ptr e1;
int etag;

if(t->atdim!=0 || (e->tag==TLIST && t->attype!=TYCOMPLEX) )
	goto typerr;

switch(t->attype)
	{
	case TYCOMPLEX:
		if(e->tag==TLIST)
			if(e->leftp==0 || e->leftp->nextp==0
			    || e->leftp->nextp->nextp!=0)
				{
				exprerr("bad conversion to complex", "");
				return( errnode() );
				}
			else	{
				e->leftp->datap = simple(RVAL,
						e->leftp->datap);
				e->leftp->nextp->datap = simple(RVAL,
						e->leftp->nextp->datap);
				if(isconst(e->leftp->datap) &&
				   isconst(e->leftp->nextp->datap) )
					return( compconst(e) );
				e1 = mkcall(builtin(TYCOMPLEX,"cmplx"),
					arg2( coerce(TYREAL,e->leftp->datap),
					coerce(TYREAL,e->leftp->nextp->datap)));
				frchain( &(e->leftp) );
				frexpblock(e);
				return(e1);
				}

	case TYINT:
	case TYREAL:
	case TYLREAL:
	case TYLOG:
	case TYFIELD:
		e = coerce(t->attype, simple(RVAL, e) );
		etag = e->tag;
		if(etag==TAROP || etag==TLOGOP || etag==TRELOP)
			e->needpar = YES;
		return(e);

	case TYCHAR:
	case TYSTRUCT:
		goto typerr;
	}

typerr:
	exprerr("typexpr not fully implemented", "");
	frexpr(e);
	return( errnode() );
}




ptr compconst(p)
register ptr p;
{
register ptr a, b;
int as, bs;
int prec;

prec = TYREAL;
p = p->leftp;
if(p == 0)
	goto err;
if(p->datap->vtype == TYLREAL)
	prec = TYLREAL;
a = coerce(TYLREAL, p->datap);
p = p->nextp;
if(p->nextp)
	goto err;
if(p->datap->vtype == TYLREAL)
	a = coerce(prec = TYLREAL,a);
b = coerce(TYLREAL, p->datap);

if(a->tag==TNEGOP)
	{
	as = '-';
	a = a->leftp;
	}
else	as = ' ';

if(b->tag==TNEGOP)
	{
	bs = '-';
	b = b->leftp;
	}
else	bs = ' ';

if(a->tag!=TCONST || a->vtype!=prec ||
   b->tag!=TCONST || b->vtype!=prec )
		goto err;

if(prec==TYLREAL && tailor.lngcxtype==NULL)
	{
	ptr q, e1, e2;
	struct dimblock *dp;
	sprintf(msg, "_const%d", ++constno);
	q = mkvar(mkname(msg));
	q->vtype = TYLREAL;
	dclit(q);
	dp = ALLOC(dimblock);
	dp->upperb = mkint(2);
	q->vdim = mkchain(dp,CHNULL);
	sprintf(msg, "%c%s", as, a->leftp);
	e1 = mkconst(TYLREAL, msg);
	sprintf(msg, "%c%s", bs, b->leftp);
	e2 = mkconst(TYLREAL, msg);
	mkinit(q, mknode(TLIST,0, mkchain(e1,mkchain(e2,CHNULL)),PNULL) );
	cfree(q->vdim);
	q->vtype = TYLCOMPLEX;
	return(q);
	}
else
	{
	sprintf(msg, "(%c%s, %c%s)", as, a->leftp, bs, b->leftp);
	return( mkconst(TYCOMPLEX, msg) );
	}

err:	exprerr("invalid complex constant", "");
	return( errnode() );
}




ptr mkchcon(p)
char *p;
{
register ptr q;
char buf[10];

sprintf(buf, "_const%d", ++constno);
q = mkvar(mkname(buf));
q->vtype = TYCHAR;
q->vtypep = mkint(strlen(p));
mkinit(q, mkconst(TYCHAR, p));
return(q);
}



ptr mksub1()
{
return( mknode(TLIST,0, mkchain(mkint(1),CHNULL), PNULL) );
}
