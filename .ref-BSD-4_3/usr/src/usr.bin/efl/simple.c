#include <ctype.h>
#include "defs"


/* basic simplifying procedure */

ptr simple(t,e)
int t;	/* take on the values LVAL, RVAL, and SUBVAL */
register ptr e;	/* points to an expression */
{
int tag, subtype;
ptr lp, rp;
int ltag;
int lsubt;
ptr p, e1;
ptr exio(), exioop(), dblop(), setfield(), gentemp();
int a,b,c;

top:

if(e == 0) return(0);

tag = e->tag;
subtype = e->subtype;
if(lp = e->leftp)
	{
	ltag = lp->tag;
	lsubt = lp->subtype;
	}
rp = e->rightp;

TEST fprintf(diagfile, "simple(%d; tag %d,%d)\n", t,tag,subtype);

switch(tag){

case TNOTOP:
	switch(ltag) {

	case TNOTOP:	/* not not = yes */
		frexpblock(e);
		e = lp->leftp;
		frexpblock(lp);
		goto top;

	case TLOGOP:	/* de Morgan's Law */
		lp->subtype = (OPOR+OPAND) - lp->subtype;
		lp->leftp = mknode(TNOTOP,OPNOT,lp->leftp, PNULL);
		lp->rightp=mknode(TNOTOP,OPNOT,lp->rightp, PNULL);
		frexpblock(e);
		e = lp;
		goto top;

	case TRELOP:	/* reverse the condition */
		lp->subtype = (OPEQ+OPNE) - lp->subtype;
		frexpblock(e);
		e = lp;
		goto top;

	case TCALL:
	case TASGNOP:
		e->leftp = simple(RVAL,lp);

	case TNAME:
	case TFTNBLOCK:
		lp = simple(RVAL,lp);

	case TTEMP:
		if(t == LVAL)
			e = simple(LVAL,
			      mknode(TASGNOP,0, gentemp(e->leftp), e));
		break;

	case TCONST:
		if(equals(lp->leftp, ".false."))
			e->leftp = copys(".true.");
		else if(equals(lp->leftp, ".true."))
			e->leftp = copys(".false.");
		else goto typerr;

		e->tag = TCONST;
		e->subtype = 0;
		cfree(lp->leftp);
		frexpblock(lp);
		break;

	default:  goto typerr;
		}
	break;




case TLOGOP: switch(subtype) {
		case OPOR:
		case OPAND:
			goto binop;

		case OP2OR:
		case OP2AND:
			lp = e->leftp = simple(RVAL, lp);
			if(lp->tag != TTEMP)
				lp = simple(RVAL,
					mknode(TASGNOP,0, gent(TYLOG,0),lp));
			return( simple(LVAL, mknode(TASGNOP,subtype,lp,rp)) );
		default:
			fatal("impossible logical operator");
		}

case TNEGOP:
	lp = e->leftp = simple(RVAL,lp);
	ltag = lp->tag;
	lsubt = lp->subtype;

	if(ltag==TNEGOP)
		{
		frexpblock(e);
		e = lp->leftp;
		frexpblock(lp);
		goto top;
		}
	else	goto lvcheck;

case TAROP:
case TRELOP:

binop:

	e->leftp = simple(RVAL,lp);
	lp = e->leftp;
	ltag = lp->tag;
	lsubt = lp->subtype;

	e->rightp= simple(RVAL,rp);
	rp = e->rightp;

	if(tag==TAROP && isicon(rp,&b) )
		{  /* simplify a*1, a/1 , a+0, a-0  */
		if( ((subtype==OPSTAR||subtype==OPSLASH) && b==1) ||
		    ((subtype==OPPLUS||subtype==OPMINUS) && b==0) )
			{
			frexpr(rp);
			mvexpr(lp,e);
			goto top;
			}

		if(isicon(lp, &a))	 /* try folding const op const */
			{
			e1 = fold(e);
			if(e1!=e || e1->tag!=TAROP)
				{
				e = e1;
				goto top;
				}
			}
		if(ltag==TAROP && lp->needpar==0 && isicon(lp->rightp,&a) )
			{ /* look for cases of (e op const ) op' const */

			if( (subtype==OPPLUS||subtype==OPMINUS) &&
			    (lsubt==OPPLUS||lsubt==OPMINUS) )
				{ /*  (e +- const) +- const */
				c = (subtype==OPPLUS ? 1 : -1) * b +
				    (lsubt==OPPLUS? 1 : -1) * a;
				if(c > 0)
					subtype = OPPLUS;
				else	{
					subtype = OPMINUS;
					c = -c;
					}
			fixexpr:
				frexpr(rp);
				frexpr(lp->rightp);
				frexpblock(e);
				e = lp;
				e->subtype = subtype;
				e->rightp = mkint(c);
				goto top;
				}

			else if(lsubt==OPSTAR &&
				( (subtype==OPSTAR) ||
				    (subtype==OPSLASH && a%b==0)) )
					{ /* (e * const ) (* or /) const */
					c = (subtype==OPSTAR ? a*b : a/b );
					subtype = OPSTAR;
					goto fixexpr;
					}
			}
		if(ltag==TAROP && (lsubt==OPPLUS || lsubt==OPMINUS) &&
			subtype==OPSLASH && divides(lp,conval(rp)) )
			{
			e->leftp = mknode(TAROP,OPSLASH,lp->leftp, cpexpr(rp));
			e->rightp = mknode(TAROP,OPSLASH,lp->rightp, rp);
			e->subtype = lsubt;
			goto top;
			}
		}

	else if( tag==TRELOP && isicon(lp,&a) && isicon(rp,&b) )
		{
		e1 = fold(e);
		if(e1!=e || e1->tag!=TRELOP)
			{
			e = e1;
			goto top;
			}
		}

lvcheck:
	if(t == LVAL)
		e = simple(LVAL, mknode(TASGNOP,0, gentemp(e),e));
	else if(t == SUBVAL)
		{  /* test for legal Fortran c*v +-c  form */
		if(tag==TAROP && (subtype==OPPLUS || subtype==OPMINUS))
			if(rp->tag==TCONST && rp->vtype==TYINT)
				{
				if(!cvform(lp))
					e->leftp = simple(SUBVAL, lp);
				}
			else goto makesub;
		else if( !cvform(e) ) goto makesub;
		}
	break;

case TCALL:
	if( lp->tag!=TFTNBLOCK && ioop(lp->sthead->namep) )
		{
		e = exioop(e, YES);
		exlab(0);
		break;
		}
	e->rightp = simple(RVAL, rp);
	if(t == SUBVAL)
		goto makesub;
	if(t == LVAL)
		e = simple(RVAL, mknode(TASGNOP,0, gentemp(e),e));
	break;


case TNAME:
	if(e->voffset)
		fixsubs(e);
	if(e->vsubs)
		e->vsubs = simple(SUBVAL, e->vsubs);
	if(t==SUBVAL && !vform(e))
		goto makesub;

case TTEMP:
case TFTNBLOCK:
case TCONST:
	if(t==SUBVAL && e->vtype!=TYINT)
		goto makesub;
	break;

case TASGNOP:
	lp = e->leftp = simple(LVAL,lp);
	if(subtype==OP2OR || subtype==OP2AND)
		e = dblop(e);

	else	{
		rp = e->rightp = simple(RVAL,rp);
		if(e->vtype == TYCHAR)
			excall(mkcall(mkftnblock(TYSUBR,"ef1asc"), arg4(cpexpr(lp),rp)));
		else if(e->vtype == TYSTRUCT)
			{
			if(lp->vtypep->strsize != rp->vtypep->strsize)
				fatal("simple: attempt to assign incompatible structures");
			e1 = mkchain(cpexpr(lp),mkchain(rp,
				mkchain(mkint(lp->vtypep->strsize),CHNULL)));
			excall(mkcall(mkftnblock(TYSUBR,"ef1ass"),
				mknode(TLIST, 0, e1, PNULL) ));
			}
		else if(lp->vtype == TYFIELD)
			lp = setfield(e);
		else	{
			if(subtype != OPASGN)	/* but is one of += etc */
				{
				rp = e->rightp = simple(RVAL, mknode(
					(subtype<=OPPOWER?TAROP:TLOGOP),subtype,
					cpexpr(e->leftp),e->rightp));
				e->subtype = OPASGN;
				}
			exlab(0);
			prexpr(e);
			frexpr(rp);
			}
		frexpblock(e);
		e = lp;
		if(t == SUBVAL) goto top;
		}

	break;

case TLIST:
	for(p=lp ; p ; p = p->nextp)
		p->datap = simple(t, p->datap);
	break;

case TIOSTAT:
	e = exio(e, 1);
	break;

default:
	break;
	}

return(e);


typerr:
	exprerr("type match error", CNULL);
	return(e);

makesub:
	if(t==SUBVAL && e->vtype!=TYINT)
		warn1("Line %d. Non-integer subscript", yylineno);
	return( simple(RVAL, mknode(TASGNOP,0,gent(TYINT,PNULL),e)) );
}

ptr fold(e)
register ptr e;
{
int a, b, c;
register ptr lp, rp;

lp = e->leftp;
rp = e->rightp;

if(lp->tag!=TCONST && lp->tag!=TNEGOP)
	return(e);

if(rp->tag!=TCONST && rp->tag!=TNEGOP)
	return(e);


switch(e->tag)
	{
	case TAROP:
		if( !isicon(lp,&a) || !isicon(rp,&b) )
			return(e);

		switch(e->subtype)
			{
			case OPPLUS:
				c = a + b;break;
			case OPMINUS:
				c = a - b; break;
			case OPSTAR:
				c = a * b; break;
			case OPSLASH:
				if(a%b!=0 && (a<0 || b<0) )
					return(e);
				c = a / b; break;
			case OPPOWER:
				return(e);
			default:
				fatal("fold: illegal binary operator");
			}
		frexpr(e);

		if(c >= 0)
			return( mkint(c) );
		else	return(mknode(TNEGOP,OPMINUS, mkint(-c), PNULL) );

	case TRELOP:
		if( !isicon(lp,&a) || !isicon(rp,&b) )
			return(e);
		frexpr(e);

		switch(e->subtype)
			{
			case OPEQ:
				c =  a == b; break;
			case OPLT:
				c = a < b ; break;
			case OPGT:
				c = a > b; break;
			case OPLE:
				c = a <= b; break;
			case OPGE:
				c = a >= b; break;
			case OPNE:
				c = a != b; break;
			default:
				fatal("fold: invalid relational operator");
			}
		return( mkconst(TYLOG, (c ? ".true." : ".false.")) );


	case TLOGOP:
		if(lp->vtype!=TYLOG || rp->vtype!=TYLOG)
			return(e);
		a = equals(lp->leftp, ".true.");
		b = equals(rp->leftp, ".true.");
		frexpr(e);

		switch(e->subtype)
			{
			case OPAND:
			case OP2AND:
				c = a & b; break;
			case OPOR:
			case OP2OR:
				c = a | b; break;
			default:
				fatal("fold: invalid logical operator");
			}
		return( mkconst(TYLOG, (c? ".true." : ".false")) );

	default:
		return(e);
	}
}

#define TO   + 100*


ptr coerce(t,e)	/* coerce expression  e  to type  t */
int t;
register ptr e;
{
register int et;
int econst;
char buff[100];
char *s, *s1;
ptr conrep(), xfixf();

if(e->tag == TNEGOP)
	{
	e->leftp = coerce(t, e->leftp);
	goto settype;
	}

et = e->vtype;
econst = (e->tag == TCONST);
TEST fprintf(diagfile, "coerce type %d to type %d\n", et, t);
if(t == et)
	return(e);

switch( et TO t )
	{
	case TYCOMPLEX TO TYINT:
	case TYLREAL TO TYINT:
		e = coerce(TYREAL,e);
	case TYREAL TO TYINT:
		if(econst)
			e = xfixf(e);
		if(e->vtype != TYINT)
			e = mkcall(builtin(TYINT,"ifix"), arg1(e));
		break;

	case TYINT TO TYREAL:
		if(econst)
			{
			e->leftp = conrep(e->leftp, ".");
			goto settype;
			}
		e = mkcall(builtin(TYREAL,"float"), arg1(e));
		break;

	case TYLREAL TO TYREAL:
		if(econst)
			{
			for(s=e->leftp ; *s && *s!='d';++s)
				;
			*s = 'e';
			goto settype;
			}
		e = mkcall(builtin(TYREAL,"sngl"), arg1(e));
		break;

	case TYCOMPLEX TO TYREAL:
		if(econst)
			{
			s1 = (char *)(e->leftp) + 1;
			s = buff;
			while(*s1!=',' && *s1!='\0')
				*s1++ = *s++;
			*s = '\0';
			cfree(e->leftp);
			e->leftp = copys(buff);
			goto settype;
			}
		else
			e = mkcall(mkftnblock(TYREAL,"real"), arg1(e));
		break;

	case TYINT TO TYLREAL:
		if(econst)
			{
			e->leftp = conrep(e->leftp,"d0");
			goto settype;
			}
	case TYCOMPLEX TO TYLREAL:
		e = coerce(TYREAL,e);
	case TYREAL TO TYLREAL:
		if(econst)
			{
			for(s=e->leftp ; *s && *s!='e'; ++s)
				;
			if(*s == 'e')
				*s = 'd';
			else	e->leftp = conrep(e->leftp,"d0");
			goto settype;
			}
		e = mkcall(builtin(TYLREAL,"dble"), arg1(e));
		break;

	case TYINT TO TYCOMPLEX:
	case TYLREAL TO TYCOMPLEX:
		e = coerce(TYREAL, e);
	case TYREAL TO TYCOMPLEX:
		if(e->tag == TCONST)
			{
			sprintf(buff, "(%s,0.)", e->leftp);
			cfree(e->leftp);
			e->leftp = copys(buff);
			goto settype;
			}
		else
			e = mkcall(builtin(TYCOMPLEX,"cmplx"),
				arg2(e, mkconst(TYREAL,"0.")));
		break;


	default:
		goto mismatch;
	}

return(e);


mismatch:
	exprerr("impossible conversion", "");
	frexpr(e);
	return( errnode() );


settype:
	e->vtype = t;
	return(e);
}



/* check whether expression is in form c, v, or v*c */
cvform(p)
register ptr p;
{
switch(p->tag)
	{
	case TCONST:
		return(p->vtype == TYINT);

	case TNAME:
		return(vform(p));

	case TAROP:
		if(p->subtype==OPSTAR && p->rightp->tag==TCONST
		    && p->rightp->vtype==TYINT && vform(p->leftp))
			return(1);

	default:
		return(0);
	}
}




/* is p a simple integer variable */
vform(p)
register ptr p;
{
return( p->tag==TNAME && p->vtype==TYINT && p->vdim==0
     && p->voffset==0 && p->vsubs==0) ;
}



ptr dblop(p)
ptr p;
{
ptr q;

bgnexec();
if(p->subtype == OP2OR)
	q = mknode(TNOTOP,OPNOT, cpexpr(p->leftp), PNULL);
else	q = cpexpr(p->leftp);

pushctl(STIF, q);
bgnexec();
exasgn(cpexpr(p->leftp), OPASGN,  p->rightp);
ifthen();
popctl();
addexec();
return(p->leftp);
}




divides(a,b)
ptr a;
int b;
{
if(a->vtype!=TYINT)
	return(0);

switch(a->tag)
	{
	case TNEGOP:
		return( divides(a->leftp,b) );

	case TCONST:
		return( conval(a) % b == 0);

	case TAROP:
		switch(a->subtype)
			{
			case OPPLUS:
			case OPMINUS:
				return(divides(a->leftp,b)&&
					   divides(a->rightp,b) );

			case OPSTAR:
				return(divides(a->rightp,b));

			default:
				return(0);
			}
	default:
		return(0);
	}
/* NOTREACHED */
}

/* truncate floating point constant to integer */

#define MAXD 100

ptr xfixf(e)
struct exprblock *e;
{
char digit[MAXD+1];	/* buffer into which digits are placed */
char *first;	/* points to first nonzero digit */
register char *end;	/* points at position past last digit */
register char *dot;	/* decimal point is immediately to left of this digit */
register char *s;
int expon;

dot = NULL;
end = digit;
expon = 0;

for(s = e->leftp ; *s; ++s)
	if( isdigit(*s) )
		{
		if(end-digit > MAXD)
			return(e);
		*end++ = *s;
		}
	else if(*s == '.')
		dot = end;
	else if(*s=='d' || *s=='e')
		{
		expon = convci(s+1);
		break;
		}
	else fatal1("impossible character %d in floating constant", *s);

if(dot == NULL)
	dot = end;
dot += expon;
if(dot-digit > MAXD)
	return(e);
for(first = digit; first<end && *first=='0' ; ++first)
	;
if(dot<=first)
	{
	dot = first+1;
	*first = '0';
	}
else	while(end < dot)
		*end++ = '0';
*dot = '\0';
cfree(e->leftp);
e->leftp = copys(first);
e->vtype = TYINT;
return(e);
}
