#include "defs"

/*   Logical IF codes
*/


exif(p)
expptr p;
{
pushctl(CTLIF);
ctlstack->elselabel = newlabel();
putif(p, ctlstack->elselabel);
}



exelif(p)
expptr p;
{
if(ctlstack->ctltype == CTLIF)
	{
	if(ctlstack->endlabel == 0)
		ctlstack->endlabel = newlabel();
	putgoto(ctlstack->endlabel);
	putlabel(ctlstack->elselabel);
	ctlstack->elselabel = newlabel();
	putif(p, ctlstack->elselabel);
	}

else	execerr("elseif out of place", 0);
}





exelse()
{
if(ctlstack->ctltype==CTLIF)
	{
	if(ctlstack->endlabel == 0)
		ctlstack->endlabel = newlabel();
	putgoto( ctlstack->endlabel );
	putlabel(ctlstack->elselabel);
	ctlstack->ctltype = CTLELSE;
	}

else	execerr("else out of place", 0);
}


exendif()
{
if(ctlstack->ctltype == CTLIF)
	{
	putlabel(ctlstack->elselabel);
	if(ctlstack->endlabel)
		putlabel(ctlstack->endlabel);
	popctl();
	}
else if(ctlstack->ctltype == CTLELSE)
	{
	putlabel(ctlstack->endlabel);
	popctl();
	}

else	execerr("endif out of place", 0);
}



LOCAL pushctl(code)
int code;
{
register int i;

if(++ctlstack >= lastctl)
	fatal("nesting too deep");
ctlstack->ctltype = code;
for(i = 0 ; i < 4 ; ++i)
	ctlstack->ctlabels[i] = 0;
++blklevel;
}


LOCAL popctl()
{
if( ctlstack-- < ctls )
	fatal("control stack empty");
--blklevel;
poplab();
}



LOCAL poplab()
{
register struct labelblock  *lp;

for(lp = labeltab ; lp < highlabtab ; ++lp)
	if(lp->labdefined)
		{
		/* mark all labels in inner blocks unreachable */
		if(lp->blklevel > blklevel)
			lp->labinacc = YES;
		}
	else if(lp->blklevel > blklevel)
		{
		/* move all labels referred to in inner blocks out a level */
		lp->blklevel = blklevel;
		}
}



/*  BRANCHING CODE
*/

exgoto(lab)
struct labelblock *lab;
{
putgoto(lab->labelno);
}







exequals(lp, rp)
register struct primblock *lp;
register expptr rp;
{
if(lp->tag != TPRIM)
	{
	err("assignment to a non-variable");
	frexpr(lp);
	frexpr(rp);
	}
else if(lp->namep->vclass!=CLVAR && lp->argsp)
	{
	if(parstate >= INEXEC)
		err("statement function amid executables");
	else
		mkstfunct(lp, rp);
	}
else
	{
	if(parstate < INDATA)
		enddcl();
	puteq(mklhs(lp), rp);
	}
}



mkstfunct(lp, rp)
struct primblock *lp;
expptr rp;
{
register struct primblock *p;
register struct nameblock *np;
chainp args;

np = lp->namep;
if(np->vclass == CLUNKNOWN)
	np->vclass = CLPROC;
else
	{
	dclerr("redeclaration of statement function", np);
	return;
	}
np->vprocclass = PSTFUNCT;
np->vstg = STGSTFUNCT;
impldcl(np);
args = (lp->argsp ? lp->argsp->listp : NULL);
np->vardesc.vstfdesc = mkchain(args , rp );

for( ; args ; args = args->nextp)
	if( (p = args->datap)->tag!=TPRIM ||
		p->argsp || p->fcharp || p->lcharp)
		err("non-variable argument in statement function definition");
	else
		{
		vardcl(args->datap = p->namep);
		free(p);
		}
}



excall(name, args, nstars, labels)
struct hashentry *name;
struct listblock *args;
int nstars;
struct labelblock *labels[ ];
{
register expptr p;

settype(name, TYSUBR, NULL);
p = mkfunct( mkprim(name, args, NULL, NULL) );
p->vtype = p->leftp->vtype = TYINT;
if(nstars > 0)
	putcmgo(p, nstars, labels);
else putexpr(p);
}



exstop(stop, p)
int stop;
register expptr p;
{
char *q;
int n;
struct constblock *mkstrcon();

if(p)
	{
	if( ! ISCONST(p) )
		{
		execerr("pause/stop argument must be constant", 0);
		frexpr(p);
		p = mkstrcon(0, 0);
		}
	else if( ISINT(p->vtype) )
		{
		q = convic(p->const.ci);
		n = strlen(q);
		if(n > 0)
			{
			p->const.ccp = copyn(n, q);
			p->vtype = TYCHAR;
			p->vleng = ICON(n);
			}
		else
			p = mkstrcon(0, 0);
		}
	else if(p->vtype != TYCHAR)
		{
		execerr("pause/stop argument must be integer or string", 0);
		p = mkstrcon(0, 0);
		}
	}
else	p = mkstrcon(0, 0);

putexpr( call1(TYSUBR, (stop ? "s_stop" : "s_paus"), p) );
}

/* DO LOOP CODE */

#define DOINIT	par[0]
#define DOLIMIT	par[1]
#define DOINCR	par[2]

#define VARSTEP	0
#define POSSTEP	1
#define NEGSTEP	2


exdo(range, spec)
int range;
chainp spec;
{
register expptr p, q;
expptr *q1;
register struct nameblock *np;
chainp cp;
register int i;
int dotype, incsign;
struct addrblock *dovarp, *dostgp;
expptr par[3];

pushctl(CTLDO);
dorange = ctlstack->dolabel = range;
np = spec->datap;
ctlstack->donamep = NULL;
if(np->vdovar)
	{
	err1("nested loops with variable %s", varstr(VL,np->varname));
	ctlstack->donamep = NULL;
	return;
	}

dovarp = mklhs( mkprim(np, 0,0,0) );
if( ! ONEOF(dovarp->vtype, MSKINT|MSKREAL) )
	{
	err("bad type on do variable");
	return;
	}
ctlstack->donamep = np;

np->vdovar = YES;
if( enregister(np) )
	{
	/* stgp points to a storage version, varp to a register version */
	dostgp = dovarp;
	dovarp = mklhs( mkprim(np, 0,0,0) );
	}
else
	dostgp = NULL;
dotype = dovarp->vtype;

for(i=0 , cp = spec->nextp ; cp!=NULL && i<3 ; cp = cp->nextp)
	{
	p = par[i++] = fixtype(cp->datap);
	if( ! ONEOF(p->vtype, MSKINT|MSKREAL) )
		{
		err("bad type on DO parameter");
		return;
		}
	}

frchain(&spec);
switch(i)
	{
	case 0:
	case 1:
		err("too few DO parameters");
		return;

	default:
		err("too many DO parameters");
		return;

	case 2:
		DOINCR = ICON(1);

	case 3:
		break;
	}

ctlstack->endlabel = newlabel();
ctlstack->dobodylabel = newlabel();

if( ISCONST(DOLIMIT) )
	ctlstack->domax = mkconv(dotype, DOLIMIT);
else
	ctlstack->domax = mktemp(dotype, NULL);

if( ISCONST(DOINCR) )
	{
	ctlstack->dostep = mkconv(dotype, DOINCR);
	if( (incsign = conssgn(ctlstack->dostep)) == 0)
		err("zero DO increment");
	ctlstack->dostepsign = (incsign > 0 ? POSSTEP : NEGSTEP);
	}
else
	{
	ctlstack->dostep = mktemp(dotype, NULL);
	ctlstack->dostepsign = VARSTEP;
	ctlstack->doposlabel = newlabel();
	ctlstack->doneglabel = newlabel();
	}

if( ISCONST(ctlstack->domax) && ISCONST(DOINIT) && ctlstack->dostepsign!=VARSTEP)
	{
	puteq(cpexpr(dovarp), cpexpr(DOINIT));
	if( onetripflag )
		frexpr(DOINIT);
	else
		{
		q = mkexpr(OPPLUS, ICON(1),
			mkexpr(OPMINUS, cpexpr(ctlstack->domax), cpexpr(DOINIT)) );
		if(incsign != conssgn(q))
			{
			warn("DO range never executed");
			putgoto(ctlstack->endlabel);
			}
		frexpr(q);
		}
	}
else if(ctlstack->dostepsign!=VARSTEP && !onetripflag)
	{
	if( ISCONST(ctlstack->domax) )
		q = cpexpr(ctlstack->domax);
	else
		q = mkexpr(OPASSIGN, cpexpr(ctlstack->domax), DOLIMIT);

	q1 = mkexpr(OPASSIGN, cpexpr(dovarp), DOINIT);
	q = mkexpr( (ctlstack->dostepsign==POSSTEP ? OPLE : OPGE), q1, q);
	putif(q, ctlstack->endlabel);
	}
else
	{
	if(! ISCONST(ctlstack->domax) )
		puteq( cpexpr(ctlstack->domax), DOLIMIT);
	q = DOINIT;
	if( ! onetripflag )
		q = mkexpr(OPMINUS, q,
			mkexpr(OPASSIGN, cpexpr(ctlstack->dostep), DOINCR) );
	puteq( cpexpr(dovarp), q);
	if(onetripflag && ctlstack->dostepsign==VARSTEP)
		puteq( cpexpr(ctlstack->dostep), DOINCR);
	}

if(ctlstack->dostepsign == VARSTEP)
	{
	if(onetripflag)
		putgoto(ctlstack->dobodylabel);
	else
		putif( mkexpr(OPGE, cpexpr(ctlstack->dostep), ICON(0)),
			ctlstack->doneglabel );
	putlabel(ctlstack->doposlabel);
	putif( mkexpr(OPLE,
		mkexpr(OPPLUSEQ, cpexpr(dovarp), cpexpr(ctlstack->dostep)),
		cpexpr(ctlstack->domax) ),
			ctlstack->endlabel);
	}
putlabel(ctlstack->dobodylabel);
if(dostgp)
	puteq(dostgp, cpexpr(dovarp));
frexpr(dovarp);
}



enddo(here)
int here;
{
register struct ctlframe *q;
register expptr t;
struct nameblock *np;
struct addrblock *ap;
register int i;

while(here == dorange)
	{
	if(np = ctlstack->donamep)
		{
		t = mkexpr(OPPLUSEQ, mklhs(mkprim(ctlstack->donamep, 0,0,0)),
			cpexpr(ctlstack->dostep) );
	
		if(ctlstack->dostepsign == VARSTEP)
			{
			putif( mkexpr(OPLE, cpexpr(ctlstack->dostep), ICON(0)), ctlstack->doposlabel);
			putlabel(ctlstack->doneglabel);
			putif( mkexpr(OPLT, t, ctlstack->domax), ctlstack->dobodylabel);
			}
		else
			putif( mkexpr( (ctlstack->dostepsign==POSSTEP ? OPGT : OPLT),
				t, ctlstack->domax),
				ctlstack->dobodylabel);
		putlabel(ctlstack->endlabel);
		if(ap = memversion(np))
			puteq(ap, mklhs( mkprim(np,0,0,0)) );
		for(i = 0 ; i < 4 ; ++i)
			ctlstack->ctlabels[i] = 0;
		deregister(ctlstack->donamep);
		ctlstack->donamep->vdovar = NO;
		frexpr(ctlstack->dostep);
		}

	popctl();
	dorange = 0;
	for(q = ctlstack ; q>=ctls ; --q)
		if(q->ctltype == CTLDO)
			{
			dorange = q->dolabel;
			break;
			}
	}
}

exassign(vname, labelval)
struct nameblock *vname;
struct labelblock *labelval;
{
struct addrblock *p;
struct constblock *mkaddcon();

p = mklhs(mkprim(vname,0,0,0));
if( ! ONEOF(p->vtype, MSKINT|MSKADDR) )
	err("noninteger assign variable");
else
	puteq(p, mkaddcon(labelval->labelno) );
}



exarif(expr, neglab, zerlab, poslab)
expptr expr;
struct labelblock *neglab, *zerlab, *poslab;
{
register int lm, lz, lp;

lm = neglab->labelno;
lz = zerlab->labelno;
lp = poslab->labelno;
expr = fixtype(expr);

if( ! ONEOF(expr->vtype, MSKINT|MSKREAL) )
	{
	err("invalid type of arithmetic if expression");
	frexpr(expr);
	}
else
	{
	if(lm == lz)
		exar2(OPLE, expr, lm, lp);
	else if(lm == lp)
		exar2(OPNE, expr, lm, lz);
	else if(lz == lp)
		exar2(OPGE, expr, lz, lm);
	else
		prarif(expr, lm, lz, lp);
	}
}



LOCAL exar2(op, e, l1, l2)
int op;
expptr e;
int l1, l2;
{
putif( mkexpr(op, e, ICON(0)), l2);
putgoto(l1);
}


exreturn(p)
register expptr p;
{
if(p && (proctype!=TYSUBR || procclass!=CLPROC) )
	{
	err("alternate return in nonsubroutine");
	p = 0;
	}

if(p)
	{
	putforce(TYINT, p);
	putgoto(retlabel);
	}
else
	putgoto(procclass==TYSUBR ? ret0label : retlabel);
}



exasgoto(labvar)
struct hashentry *labvar;
{
register struct addrblock *p;

p = mklhs( mkprim(labvar,0,0,0) );
if( ! ISINT(p->vtype) )
	err("assigned goto variable must be integer");
else
	putbranch(p);
}
