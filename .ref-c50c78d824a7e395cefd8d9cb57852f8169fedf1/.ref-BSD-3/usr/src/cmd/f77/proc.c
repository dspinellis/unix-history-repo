#include "defs"

#ifdef SDB
#	include <a.out.h>
char *stabline();
#endif

/* start a new procedure */

newproc()
{
if(parstate != OUTSIDE)
	{
	execerr("missing end statement", 0);
	endproc();
	}

parstate = INSIDE;
procclass = CLMAIN;	/* default */
}



/* end of procedure. generate variables, epilogs, and prologs */

endproc()
{
struct Labelblock *lp;

if(parstate < INDATA)
	enddcl();
if(ctlstack >= ctls)
	err("DO loop or BLOCK IF not closed");
for(lp = labeltab ; lp < labtabend ; ++lp)
	if(lp->stateno!=0 && lp->labdefined==NO)
		errstr("missing statement number %s", convic(lp->stateno) );

epicode();
procode();
dobss();
prdbginfo();

#if FAMILY == PCC
	putbracket();
#endif

procinit();	/* clean up for next procedure */
}



/* End of declaration section of procedure.  Allocate storage. */

enddcl()
{
register struct Entrypoint *p;

parstate = INEXEC;
docommon();
doequiv();
docomleng();
for(p = entries ; p ; p = p->nextp)
	doentry(p);
}

/* ROUTINES CALLED WHEN ENCOUNTERING ENTRY POINTS */

/* Main program or Block data */

startproc(progname, class)
struct Extsym * progname;
int class;
{
register struct Entrypoint *p;
char buff[10];

p = ALLOC(Entrypoint);
if(class == CLMAIN)
	puthead("MAIN__", CLMAIN);
else
	puthead(NULL, CLBLOCK);
if(class == CLMAIN)
	newentry( mkname(5, "MAIN_") );
p->entryname = progname;
p->entrylabel = newlabel();
entries = p;

procclass = class;
retlabel = newlabel();
fprintf(diagfile, "   %s", (class==CLMAIN ? "MAIN" : "BLOCK DATA") );
if(progname)
	fprintf(diagfile, " %s", nounder(XL, procname = progname->extname) );
fprintf(diagfile, ":\n");
#ifdef SDB
if(sdbflag && class==CLMAIN)
	{
	sprintf(buff, "L%d", p->entrylabel);
	prstab("MAIN_", N_FUN, lineno, buff);
	p2pass( stabline("MAIN_", N_FNAME, 0, 0) );
	if(progname)
		{
		prstab(nounder(XL,progname->extname), N_ENTRY, lineno,buff);
/*		p2pass(stabline(nounder(XL,progname->extname),N_FNAME,0,0));	*/
		}
	}
#endif
}

/* subroutine or function statement */

struct Extsym *newentry(v)
register struct Nameblock *v;
{
register struct Extsym *p;

p = mkext( varunder(VL, v->varname) );

if(p==NULL || p->extinit || ! ONEOF(p->extstg, M(STGUNKNOWN)|M(STGEXT)) )
	{
	if(p == 0)
		dclerr("invalid entry name", v);
	else	dclerr("external name already used", v);
	return(0);
	}
v->vstg = STGAUTO;
v->vprocclass = PTHISPROC;
v->vclass = CLPROC;
p->extstg = STGEXT;
p->extinit = YES;
return(p);
}


entrypt(class, type, length, entry, args)
int class, type;
ftnint length;
struct Extsym *entry;
chainp args;
{
register struct Nameblock *q;
register struct Entrypoint *p;
char buff[10];

if(class != CLENTRY)
	puthead( varstr(XL, procname = entry->extname), class);
if(class == CLENTRY)
	fprintf(diagfile, "       entry ");
fprintf(diagfile, "   %s:\n", nounder(XL, entry->extname));
q = mkname(VL, nounder(XL,entry->extname) );

if( (type = lengtype(type, (int) length)) != TYCHAR)
	length = 0;
if(class == CLPROC)
	{
	procclass = CLPROC;
	proctype = type;
	procleng = length;

	retlabel = newlabel();
	if(type == TYSUBR)
		ret0label = newlabel();
	}

p = ALLOC(Entrypoint);
entries = hookup(entries, p);
p->entryname = entry;
p->arglist = args;
p->entrylabel = newlabel();
p->enamep = q;

#ifdef SDB
if(sdbflag)
	{
	sprintf(buff, "L%d", p->entrylabel);
	prstab(nounder(XL, entry->extname),
		(class==CLENTRY ? N_ENTRY : N_FUN),
		lineno, buff);
	if(class != CLENTRY)
	p2pass( stabline( nounder(XL,entry->extname), N_FNAME, 0, 0) );
	}
#endif

if(class == CLENTRY)
	{
	class = CLPROC;
	if(proctype == TYSUBR)
		type = TYSUBR;
	}

q->vclass = class;
q->vprocclass = PTHISPROC;
settype(q, type, (int) length);
/* hold all initial entry points till end of declarations */
if(parstate >= INDATA)
	doentry(p);
}

/* generate epilogs */

LOCAL epicode()
{
register int i;

if(procclass==CLPROC)
	{
	if(proctype==TYSUBR)
		{
		putlabel(ret0label);
		if(substars)
			putforce(TYINT, ICON(0) );
		putlabel(retlabel);
		goret(TYSUBR);
		}
	else	{
		putlabel(retlabel);
		if(multitypes)
			{
			typeaddr = autovar(1, TYADDR, NULL);
			putbranch( cpexpr(typeaddr) );
			for(i = 0; i < NTYPES ; ++i)
				if(rtvlabel[i] != 0)
					{
					putlabel(rtvlabel[i]);
					retval(i);
					}
			}
		else
			retval(proctype);
		}
	}

else if(procclass != CLBLOCK)
	{
	putlabel(retlabel);
	goret(TYSUBR);
	}
}


/* generate code to return value of type  t */

LOCAL retval(t)
register int t;
{
register struct Addrblock *p;

switch(t)
	{
	case TYCHAR:
	case TYCOMPLEX:
	case TYDCOMPLEX:
		break;

	case TYLOGICAL:
		t = tylogical;
	case TYADDR:
	case TYSHORT:
	case TYLONG:
		p = cpexpr(retslot);
		p->vtype = t;
		putforce(t, p);
		break;

	case TYREAL:
	case TYDREAL:
		p = cpexpr(retslot);
		p->vtype = t;
		putforce(t, p);
		break;

	default:
		fatali("retval: impossible type %d", t);
	}
goret(t);
}


/* Allocate extra argument array if needed. Generate prologs. */

LOCAL procode()
{
register struct Entrypoint *p;
struct Addrblock *argvec;

#if TARGET==GCOS
	argvec = autovar(lastargslot/SZADDR, TYADDR, NULL);
#else
	if(lastargslot>0 && nentry>1)
#if TARGET == VAX
		argvec = autovar(1 + lastargslot/SZADDR, TYADDR, NULL);
#else
		argvec = autovar(lastargslot/SZADDR, TYADDR, NULL);
#endif
	else
		argvec = NULL;
#endif


#if TARGET == PDP11
	/* for the optimizer */
	if(fudgelabel)
		putlabel(fudgelabel);
#endif

for(p = entries ; p ; p = p->nextp)
	prolog(p, argvec);

#if FAMILY == PCC
	putrbrack(procno);
#endif

prendproc();
}

/*
   manipulate argument lists (allocate argument slot positions)
 * keep track of return types and labels
 */

LOCAL doentry(ep)
struct Entrypoint *ep;
{
register int type;
register struct Nameblock *np;
chainp p;
register struct Nameblock *q;

++nentry;
if(procclass == CLMAIN)
	{
	putlabel(ep->entrylabel);
	return;
	}
else if(procclass == CLBLOCK)
	return;

impldcl( np = mkname(VL, nounder(XL, ep->entryname->extname) ) );
type = np->vtype;
if(proctype == TYUNKNOWN)
	if( (proctype = type) == TYCHAR)
		procleng = (np->vleng ? np->vleng->constblock.const.ci : (ftnint) 0);

if(proctype == TYCHAR)
	{
	if(type != TYCHAR)
		err("noncharacter entry of character function");
	else if( (np->vleng ? np->vleng->constblock.const.ci : (ftnint) 0) != procleng)
		err("mismatched character entry lengths");
	}
else if(type == TYCHAR)
	err("character entry of noncharacter function");
else if(type != proctype)
	multitype = YES;
if(rtvlabel[type] == 0)
	rtvlabel[type] = newlabel();
ep->typelabel = rtvlabel[type];

if(type == TYCHAR)
	{
	if(chslot < 0)
		{
		chslot = nextarg(TYADDR);
		chlgslot = nextarg(TYLENG);
		}
	np->vstg = STGARG;
	np->vardesc.varno = chslot;
	if(procleng == 0)
		np->vleng = mkarg(TYLENG, chlgslot);
	}
else if( ISCOMPLEX(type) )
	{
	np->vstg = STGARG;
	if(cxslot < 0)
		cxslot = nextarg(TYADDR);
	np->vardesc.varno = cxslot;
	}
else if(type != TYSUBR)
	{
	if(nentry == 1)
		retslot = autovar(1, TYDREAL, NULL);
	np->vstg = STGAUTO;
	np->voffset = retslot->memoffset->constblock.const.ci;
	}

for(p = ep->arglist ; p ; p = p->nextp)
	if(! ((q = p->datap)->vdcldone) )
		q->vardesc.varno = nextarg(TYADDR);

for(p = ep->arglist ; p ; p = p->nextp)
	if(! ((q = p->datap)->vdcldone) )
		{
		impldcl(q);
		q->vdcldone = YES;
#ifdef SDB
		if(sdbflag)
			prstab(varstr(VL,q->varname), N_PSYM,
				stabtype(q),
				convic(q->vardesc.varno + ARGOFFSET) );
#endif
		if(q->vtype == TYCHAR)
			{
			if(q->vleng == NULL)	/* character*(*) */
				q->vleng = mkarg(TYLENG, nextarg(TYLENG) );
			else if(nentry == 1)
				nextarg(TYLENG);
			}
		else if(q->vclass==CLPROC && nentry==1)
			nextarg(TYLENG) ;
		}

putlabel(ep->entrylabel);
}



LOCAL nextarg(type)
int type;
{
int k;
k = lastargslot;
lastargslot += typesize[type];
return(k);
}

/* generate variable references */

LOCAL dobss()
{
register struct Hashentry *p;
register struct Nameblock *q;
register int i;
int align;
ftnint leng, iarrl;
char *memname();
int qstg, qclass, qtype;

pruse(asmfile, USEBSS);

for(p = hashtab ; p<lasthash ; ++p)
    if(q = p->varp)
	{
	qstg = q->vstg;
	qtype = q->vtype;
	qclass = q->vclass;

#ifdef SDB
		if(sdbflag&&qclass==CLVAR&&(qstg==STGBSS||qstg==STGINIT))
			{
			prstab(varstr(VL,q->varname), N_LCSYM,
				stabtype(q), memname(qstg,q->vardesc.varno) );
			prstleng(q, iarrlen(q));
			}
#endif

	if( (qclass==CLUNKNOWN && qstg!=STGARG) ||
	    (qclass==CLVAR && qstg==STGUNKNOWN) )
		warn1("local variable %s never used", varstr(VL,q->varname) );
	else if(qclass==CLVAR && qstg==STGBSS)
		{
		align = (qtype==TYCHAR ? ALILONG : typealign[qtype]);
		if(bssleng % align != 0)
			{
			bssleng = roundup(bssleng, align);
			preven(align);
			}
		prlocvar(memname(STGBSS,q->vardesc.varno), iarrl = iarrlen(q) );
		bssleng += iarrl;
		}
	else if(qclass==CLPROC && q->vprocclass==PEXTERNAL && qstg!=STGARG)
		mkext(varunder(VL, q->varname)) ->extstg = STGEXT;

	if(qclass==CLVAR && qstg!=STGARG)
		{
		if(q->vdim && !ISICON(q->vdim->nelt) )
			dclerr("adjustable dimension on non-argument", q);
		if(qtype==TYCHAR && (q->vleng==NULL || !ISICON(q->vleng)))
			dclerr("adjustable leng on nonargument", q);
		}
	}

for(i = 0 ; i < nequiv ; ++i)
	if(eqvclass[i].eqvinit==NO && (leng = eqvclass[i].eqvleng)!=0 )
		{
		bssleng = roundup(bssleng, ALIDOUBLE);
		preven(ALIDOUBLE);
		prlocvar( memname(STGEQUIV, i), leng);
		bssleng += leng;
		}
}




doext()
{
struct Extsym *p;

for(p = extsymtab ; p<nextext ; ++p)
	prext( varstr(XL, p->extname), p->maxleng, p->extinit);
}




ftnint iarrlen(q)
register struct Nameblock *q;
{
ftnint leng;

leng = typesize[q->vtype];
if(leng <= 0)
	return(-1);
if(q->vdim)
	if( ISICON(q->vdim->nelt) )
		leng *= q->vdim->nelt->constblock.const.ci;
	else	return(-1);
if(q->vleng)
	if( ISICON(q->vleng) )
		leng *= q->vleng->constblock.const.ci;
	else 	return(-1);
return(leng);
}

LOCAL docommon()
{
register struct Extsym *p;
register chainp q;
struct Dimblock *t;
expptr neltp;
register struct Nameblock *v;
ftnint size;
int type;

for(p = extsymtab ; p<nextext ; ++p)
	if(p->extstg==STGCOMMON)
		{
#ifdef SDB
		if(sdbflag)
			prstab(NULL, N_BCOMM, 0, 0);
#endif
		for(q = p->extp ; q ; q = q->nextp)
			{
			v = q->datap;
			if(v->vdcldone == NO)
				vardcl(v);
			type = v->vtype;
			if(p->extleng % typealign[type] != 0)
				{
				dclerr("common alignment", v);
				p->extleng = roundup(p->extleng, typealign[type]);
				}
			v->voffset = p->extleng;
			v->vardesc.varno = p - extsymtab;
			if(type == TYCHAR)
				size = v->vleng->constblock.const.ci;
			else	size = typesize[type];
			if(t = v->vdim)
				if( (neltp = t->nelt) && ISCONST(neltp) )
					size *= neltp->constblock.const.ci;
				else
					dclerr("adjustable array in common", v);
			p->extleng += size;
#ifdef SDB
			if(sdbflag)
				{
				prstssym(v);
				prstleng(v, size);
				}
#endif
			}

		frchain( &(p->extp) );
#ifdef SDB
		if(sdbflag)
			prstab(varstr(XL,p->extname), N_ECOMM, 0, 0);
#endif
		}
}





LOCAL docomleng()
{
register struct Extsym *p;

for(p = extsymtab ; p < nextext ; ++p)
	if(p->extstg == STGCOMMON)
		{
		if(p->maxleng!=0 && p->extleng!=0 && p->maxleng!=p->extleng
		    && !eqn(XL,"_BLNK__ ",p->extname) )
			warn1("incompatible lengths for common block %s",
				nounder(XL, p->extname) );
		if(p->maxleng < p->extleng)
			p->maxleng = p->extleng;
		p->extleng = 0;
	}
}




/* ROUTINES DEALING WITH AUTOMATIC AND TEMPORARY STORAGE */

frtemp(p)
struct Addrblock *p;
{
holdtemps = mkchain(p, holdtemps);
}




/* allocate an automatic variable slot */

struct Addrblock *autovar(nelt, t, lengp)
register int nelt, t;
expptr lengp;
{
ftnint leng;
register struct Addrblock *q;

if(t == TYCHAR)
	if( ISICON(lengp) )
		leng = lengp->constblock.const.ci;
	else	{
		fatal("automatic variable of nonconstant length");
		}
else
	leng = typesize[t];
autoleng = roundup( autoleng, typealign[t]);

q = ALLOC(Addrblock);
q->tag = TADDR;
q->vtype = t;
if(t == TYCHAR)
	q->vleng = ICON(leng);
q->vstg = STGAUTO;
q->ntempelt = nelt;
#if TARGET==PDP11 || TARGET==VAX
	/* stack grows downward */
	autoleng += nelt*leng;
	q->memoffset = ICON( - autoleng );
#else
	q->memoffset = ICON( autoleng );
	autoleng += nelt*leng;
#endif

return(q);
}


struct Addrblock *mktmpn(nelt, type, lengp)
int nelt;
register int type;
expptr lengp;
{
ftnint leng;
chainp p, oldp;
register struct Addrblock *q;

if(type==TYUNKNOWN || type==TYERROR)
	fatali("mktmpn: invalid type %d", type);

if(type==TYCHAR)
	if( ISICON(lengp) )
		leng = lengp->constblock.const.ci;
	else	{
		err("adjustable length");
		return( errnode() );
		}
for(oldp = &templist ; p = oldp->nextp ; oldp = p)
	{
	q = p->datap;
	if(q->vtype==type && q->ntempelt==nelt &&
	    (type!=TYCHAR || q->vleng->constblock.const.ci==leng) )
		{
		oldp->nextp = p->nextp;
		free(p);
		return(q);
		}
	}
q = autovar(nelt, type, lengp);
q->istemp = YES;
return(q);
}




struct Addrblock *mktemp(type, lengp)
int type;
expptr lengp;
{
return( mktmpn(1,type,lengp) );
}

/* VARIOUS ROUTINES FOR PROCESSING DECLARATIONS */

struct Extsym *comblock(len, s)
register int len;
register char *s;
{
struct Extsym *p;

if(len == 0)
	{
	s = BLANKCOMMON;
	len = strlen(s);
	}
p = mkext( varunder(len, s) );
if(p->extstg == STGUNKNOWN)
	p->extstg = STGCOMMON;
else if(p->extstg != STGCOMMON)
	{
	errstr("%s cannot be a common block name", s);
	return(0);
	}

return( p );
}


incomm(c, v)
struct Extsym *c;
struct Nameblock *v;
{
if(v->vstg != STGUNKNOWN)
	dclerr("incompatible common declaration", v);
else
	{
	v->vstg = STGCOMMON;
	c->extp = hookup(c->extp, mkchain(v,NULL) );
	}
}




settype(v, type, length)
register struct Nameblock * v;
register int type;
register int length;
{
if(type == TYUNKNOWN)
	return;

if(type==TYSUBR && v->vtype!=TYUNKNOWN && v->vstg==STGARG)
	{
	v->vtype = TYSUBR;
	frexpr(v->vleng);
	}
else if(type < 0)	/* storage class set */
	{
	if(v->vstg == STGUNKNOWN)
		v->vstg = - type;
	else if(v->vstg != -type)
		dclerr("incompatible storage declarations", v);
	}
else if(v->vtype == TYUNKNOWN)
	{
	if( (v->vtype = lengtype(type, length))==TYCHAR && length!=0)
		v->vleng = ICON(length);
	}
else if(v->vtype!=type || (type==TYCHAR && v->vleng->constblock.const.ci!=length) )
	dclerr("incompatible type declarations", v);
}





lengtype(type, length)
register int type;
register int length;
{
switch(type)
	{
	case TYREAL:
		if(length == 8)
			return(TYDREAL);
		if(length == 4)
			goto ret;
		break;

	case TYCOMPLEX:
		if(length == 16)
			return(TYDCOMPLEX);
		if(length == 8)
			goto ret;
		break;

	case TYSHORT:
	case TYDREAL:
	case TYDCOMPLEX:
	case TYCHAR:
	case TYUNKNOWN:
	case TYSUBR:
	case TYERROR:
		goto ret;

	case TYLOGICAL:
		if(length == typesize[TYLOGICAL])
			goto ret;
		break;

	case TYLONG:
		if(length == 0)
			return(tyint);
		if(length == 2)
			return(TYSHORT);
		if(length == 4)
			goto ret;
		break;
	default:
		fatali("lengtype: invalid type %d", type);
	}

if(length != 0)
	err("incompatible type-length combination");

ret:
	return(type);
}





setintr(v)
register struct Nameblock * v;
{
register int k;

if(v->vstg == STGUNKNOWN)
	v->vstg = STGINTR;
else if(v->vstg!=STGINTR)
	dclerr("incompatible use of intrinsic function", v);
if(v->vclass==CLUNKNOWN)
	v->vclass = CLPROC;
if(v->vprocclass == PUNKNOWN)
	v->vprocclass = PINTRINSIC;
else if(v->vprocclass != PINTRINSIC)
	dclerr("invalid intrinsic declaration", v);
if(k = intrfunct(v->varname))
	v->vardesc.varno = k;
else
	dclerr("unknown intrinsic function", v);
}



setext(v)
register struct Nameblock * v;
{
if(v->vclass == CLUNKNOWN)
	v->vclass = CLPROC;
else if(v->vclass != CLPROC)
	dclerr("invalid external declaration", v);

if(v->vprocclass == PUNKNOWN)
	v->vprocclass = PEXTERNAL;
else if(v->vprocclass != PEXTERNAL)
	dclerr("invalid external declaration", v);
}




/* create dimensions block for array variable */

setbound(v, nd, dims)
register struct Nameblock * v;
int nd;
struct { expptr lb, ub; } dims[ ];
{
register expptr q, t;
register struct Dimblock *p;
int i;

if(v->vclass == CLUNKNOWN)
	v->vclass = CLVAR;
else if(v->vclass != CLVAR)
	{
	dclerr("only variables may be arrays", v);
	return;
	}

v->vdim = p = (struct Dimblock *) ckalloc(sizeof(int) + (3+2*nd)*sizeof(expptr) );
p->ndim = nd;
p->nelt = ICON(1);

for(i=0 ; i<nd ; ++i)
	{
	if( (q = dims[i].ub) == NULL)
		{
		if(i == nd-1)
			{
			frexpr(p->nelt);
			p->nelt = NULL;
			}
		else
			err("only last bound may be asterisk");
		p->dims[i].dimsize = ICON(1);;
		p->dims[i].dimexpr = NULL;
		}
	else
		{
		if(dims[i].lb)
			{
			q = mkexpr(OPMINUS, q, cpexpr(dims[i].lb));
			q = mkexpr(OPPLUS, q, ICON(1) );
			}
		if( ISCONST(q) )
			{
			p->dims[i].dimsize = q;
			p->dims[i].dimexpr = NULL;
			}
		else	{
			p->dims[i].dimsize = autovar(1, tyint, NULL);
			p->dims[i].dimexpr = q;
			}
		if(p->nelt)
			p->nelt = mkexpr(OPSTAR, p->nelt,
					cpexpr(p->dims[i].dimsize) );
		}
	}

q = dims[nd-1].lb;
if(q == NULL)
	q = ICON(1);

for(i = nd-2 ; i>=0 ; --i)
	{
	t = dims[i].lb;
	if(t == NULL)
		t = ICON(1);
	if(p->dims[i].dimsize)
		q = mkexpr(OPPLUS, t, mkexpr(OPSTAR, cpexpr(p->dims[i].dimsize), q) );
	}

if( ISCONST(q) )
	{
	p->baseoffset = q;
	p->basexpr = NULL;
	}
else
	{
	p->baseoffset = autovar(1, tyint, NULL);
	p->basexpr = q;
	}
}
