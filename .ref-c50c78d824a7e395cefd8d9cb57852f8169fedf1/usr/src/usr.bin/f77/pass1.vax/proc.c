/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)proc.c	5.8 (Berkeley) %G%";
#endif not lint

/*
 * proc.c
 *
 * Routines for handling procedures, f77 compiler, pass 1.
 *
 * University of Utah CS Dept modification history:
 *
 * $Log:	proc.c,v $
 * Revision 5.9  86/01/28  22:30:28  donn
 * Let functions of type character have adjustable length.
 * 
 * Revision 5.8  86/01/10  19:02:19  donn
 * More dbx hacking -- filter out incomplete declarations (with bogus types).
 * 
 * Revision 5.7  86/01/10  13:53:02  donn
 * Since we now postpone determination of the type of an argument, we must
 * make sure to emit stab information at the end of the routine when we
 * definitely have the type.  Notice some care was taken to make sure that
 * arguments appear in order in the output file since that's how dbx wants
 * them.  Also a minor change for dummy procedures.
 * 
 * Revision 5.6  86/01/06  16:28:06  donn
 * Sigh.  We can't commit to defining a symbol as a variable instead of a
 * function based only on what we have seen through the declaration section;
 * this was properly handled for normal variables but not for arguments.
 * 
 * Revision 5.5  86/01/01  21:59:17  donn
 * Pick up CHARACTER*(*) declarations for variables which aren't dummy
 * arguments, and complain about them.
 * 
 * Revision 5.4  85/12/20  19:18:35  donn
 * Don't assume that dummy procedures of unknown type are functions of type
 * undefined until the user (mis-)uses them that way -- they may also be
 * subroutines.
 * 
 * Revision 5.3  85/09/30  23:21:07  donn
 * Print space with prspace() in outlocvars() so that alignment is preserved.
 * 
 * Revision 5.2  85/08/10  05:03:34  donn
 * Support for NAMELIST i/o from Jerry Berkman.
 * 
 * Revision 5.1  85/08/10  03:49:14  donn
 * 4.3 alpha
 * 
 * Revision 3.11  85/06/04  03:45:29  donn
 * Changed retval() to recognize that a function declaration might have
 * bombed out earlier, leaving an error node behind...
 * 
 * Revision 3.10  85/03/08  23:13:06  donn
 * Finally figured out why function calls and array elements are not legal
 * dummy array dimension declarator elements.  Hacked safedim() to stop 'em.
 * 
 * Revision 3.9  85/02/02  00:26:10  donn
 * Removed the call to entrystab() in enddcl() -- this was redundant (it was
 * also done in startproc()) and confusing to dbx to boot.
 * 
 * Revision 3.8  85/01/14  04:21:53  donn
 * Added changes to implement Jerry's '-q' option.
 * 
 * Revision 3.7  85/01/11  21:10:35  donn
 * In conjunction with other changes to implement SAVE statements, function
 * nameblocks were changed to make it appear that they are 'saved' too --
 * this arranges things so that function return values are forced out of
 * register before a return.
 * 
 * Revision 3.6  84/12/10  19:27:20  donn
 * comblock() signals an illegal common block name by returning a null pointer,
 * but incomm() wasn't able to handle it, leading to core dumps.  I put the
 * fix in incomm() to pick up null common blocks.
 * 
 * Revision 3.5  84/11/21  20:33:31  donn
 * It seems that I/O elements are treated as character strings so that their
 * length can be passed to the I/O routines...  Unfortunately the compiler
 * assumes that no temporaries can be of type CHARACTER and casually tosses
 * length and type info away when removing TEMP blocks.  This has been fixed...
 * 
 * Revision 3.4  84/11/05  22:19:30  donn
 * Fixed a silly bug in the last fix.
 * 
 * Revision 3.3  84/10/29  08:15:23  donn
 * Added code to check the type and shape of subscript declarations,
 * per Jerry Berkman's suggestion.
 * 
 * Revision 3.2  84/10/29  05:52:07  donn
 * Added change suggested by Jerry Berkman to report an error when an array
 * is redimensioned.
 * 
 * Revision 3.1  84/10/13  02:12:31  donn
 * Merged Jerry Berkman's version into mine.
 * 
 * Revision 2.1  84/07/19  12:04:09  donn
 * Changed comment headers for UofU.
 * 
 * Revision 1.6  84/07/19  11:32:15  donn
 * Incorporated fix to setbound() to detect backward array subscript limits.
 * The fix is by Bob Corbett, donated by Jerry Berkman.
 * 
 * Revision 1.5  84/07/18  18:25:50  donn
 * Fixed problem with doentry() where a placeholder for a return value
 * was not allocated if the first entry didn't require one but a later
 * entry did.
 * 
 * Revision 1.4  84/05/24  20:52:09  donn
 * Installed firewall #ifdef around the code that recycles stack temporaries,
 * since it seems to be broken and lacks a good fix for the time being.
 * 
 * Revision 1.3  84/04/16  09:50:46  donn
 * Fixed mkargtemp() so that it only passes back a copy of a temporary, keeping
 * the original for its own use.  This fixes a set of bugs that are caused by
 * elements in the argtemplist getting stomped on.
 * 
 * Revision 1.2  84/02/28  21:12:58  donn
 * Added Berkeley changes for subroutine call argument temporaries fix.
 * 
 */

#include "defs.h"

#ifdef SDB
#	include <a.out.h>
#	ifndef N_SO
#		include <stab.h>
#	endif
#endif

extern flag namesflag;

typedef
  struct SizeList
    {
      struct SizeList *next;
      ftnint size;
      struct VarList *vars;
    }
  sizelist;


typedef
  struct VarList
    {
      struct VarList *next;
      Namep np;
      struct Equivblock *ep;
    }
  varlist;


LOCAL sizelist *varsizes;


/* start a new procedure */

newproc()
{
if(parstate != OUTSIDE)
	{
	execerr("missing end statement", CNULL);
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

if (optimflag)
  optimize();

outiodata();
epicode();
procode();
donmlist();
dobss();

#if FAMILY == PCC
	putbracket();
#endif
fixlwm();
procinit();	/* clean up for next procedure */
}



/* End of declaration section of procedure.  Allocate storage. */

enddcl()
{
register struct Entrypoint *ep;

parstate = INEXEC;
docommon();
doequiv();
docomleng();
for(ep = entries ; ep ; ep = ep->entnextp) {
	doentry(ep);
}
}

/* ROUTINES CALLED WHEN ENCOUNTERING ENTRY POINTS */

/* Main program or Block data */

startproc(prgname, class)
Namep prgname;
int class;
{
struct Extsym *progname;
register struct Entrypoint *p;

if(prgname)
	procname = prgname->varname;
if(namesflag == YES) {
	fprintf(diagfile, "   %s", (class==CLMAIN ? "MAIN" : "BLOCK DATA") );
	if(prgname)
		fprintf(diagfile, " %s", varstr(XL, procname) );
	fprintf(diagfile, ":\n");
	}

if( prgname ) 
	progname = newentry( prgname );
else
	progname = NULL;

p = ALLOC(Entrypoint);
if(class == CLMAIN)
	puthead("MAIN_", CLMAIN);
else
	puthead(CNULL, CLBLOCK);
if(class == CLMAIN)
	newentry( mkname(5, "MAIN") );
p->entryname = progname;
p->entrylabel = newlabel();
entries = p;

procclass = class;
retlabel = newlabel();
#ifdef SDB
if(sdbflag) {
         entrystab(p,class);
}
#endif
}

/* subroutine or function statement */

struct Extsym *newentry(v)
register Namep v;
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


entrypt(class, type, length, entname, args)
int class, type;
ftnint length;
Namep entname;
chainp args;
{
struct Extsym *entry;
register Namep q;
register struct Entrypoint *p, *ep;

if(namesflag == YES) {
	if(class == CLENTRY)
		fprintf(diagfile, "       entry ");
	if(entname)
		fprintf(diagfile, "   %s", varstr(XL, entname->varname) );
	fprintf(diagfile, ":\n");
	}

if( entname->vclass == CLPARAM ) {
	errstr("entry name %s used in 'parameter' statement", 
		varstr(XL, entname->varname) );
	return;
	}
if( ((type == TYSUBR) || (class == CLENTRY && proctype == TYSUBR)) 
	&& (entname->vtype != TYUNKNOWN && entname->vtype != TYSUBR) ) {
	errstr("subroutine entry %s previously declared",
		varstr(XL, entname->varname) );
	return;
	}
if(  (entname->vstg != STGEXT && entname->vstg != STGUNKNOWN)
	||  (entname->vdim != NULL) ) {
	errstr("subroutine or function entry %s previously declared",
		varstr(XL, entname->varname) );
	return;
	}

if( (class == CLPROC || class == CLENTRY) && type != TYSUBR )
	/* arrange to save function return values */
	entname->vsave = YES;
	
entry = newentry( entname );

if(class != CLENTRY)
	puthead( varstr(XL, procname = entry->extname), class);
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
if(entries)	/* put new block at end of entries list */
	{
	for(ep = entries; ep->entnextp; ep = ep->entnextp)
		;
	ep->entnextp = p;
	}
else
	entries = p;

p->entryname = entry;
p->arglist = args;
p->entrylabel = newlabel();
p->enamep = q;

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
if(parstate >= INDATA) {
	doentry(p);
}
#ifdef SDB
	if(sdbflag)
	{ /* may need to preserve CLENTRY here */
	entrystab(p,class);
	}
#endif
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
		if(multitype)
			{
			typeaddr = autovar(1, TYADDR, PNULL);
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
register Addrp p;

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
		p = (Addrp) cpexpr(retslot);
		p->vtype = t;
		putforce(t, p);
		break;

	case TYREAL:
	case TYDREAL:
		p = (Addrp) cpexpr(retslot);
		p->vtype = t;
		putforce(t, p);
		break;

	case TYERROR:
		return;		/* someone else already complained */

	default:
		badtype("retval", t);
	}
goret(t);
}


/* Allocate extra argument array if needed. Generate prologs. */

LOCAL procode()
{
register struct Entrypoint *p;
Addrp argvec;

#if TARGET==GCOS
	argvec = autovar(lastargslot/SZADDR, TYADDR, PNULL);
#else
	if(lastargslot>0 && nentry>1)
#if TARGET == VAX
		argvec = autovar(1 + lastargslot/SZADDR, TYADDR, PNULL);
#else
		argvec = autovar(lastargslot/SZADDR, TYADDR, PNULL);
#endif
	else
		argvec = NULL;
#endif


#if TARGET == PDP11
	/* for the optimizer */
	if(fudgelabel)
		putlabel(fudgelabel);
#endif

for(p = entries ; p ; p = p->entnextp)
	prolog(p, argvec);

#if FAMILY == PCC
	putrbrack(procno);
#endif

prendproc();
}


/*
 * manipulate argument lists (allocate argument slot positions)
 * keep track of return types and labels
 */

LOCAL doentry(ep)
struct Entrypoint *ep;
{
register int type;
register Namep np;
chainp p;
register Namep q;
Addrp mkarg();

++nentry;
if(procclass == CLMAIN)
	{
	if (optimflag)
		optbuff (SKLABEL, 0, ep->entrylabel, 0);
	else
		putlabel(ep->entrylabel);
	return;
	}
else if(procclass == CLBLOCK)
	return;

impldcl( np = mkname(VL, nounder(XL, ep->entryname->extname) ) );
type = np->vtype;
if(proctype == TYUNKNOWN)
	if( (proctype = type) == TYCHAR)
		procleng = (np->vleng ? np->vleng->constblock.constant.ci : (ftnint) (-1));

if(proctype == TYCHAR)
	{
	if(type != TYCHAR)
		err("noncharacter entry of character function");
	else if( (np->vleng ? np->vleng->constblock.constant.ci : (ftnint) (-1)) != procleng)
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
	if(procleng < 0)
		np->vleng = (expptr) mkarg(TYLENG, chlgslot);
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
	if(retslot == NULL)
		retslot = autovar(1, TYDREAL, PNULL);
	np->vstg = STGAUTO;
	np->voffset = retslot->memoffset->constblock.constant.ci;
	}

for(p = ep->arglist ; p ; p = p->nextp)
	if(! (( q = (Namep) (p->datap) )->vdcldone) )
		q->vardesc.varno = nextarg(TYADDR);

for(p = ep->arglist ; p ; p = p->nextp)
	if(! (( q = (Namep) (p->datap) )->vdcldone) )
		{
		if(q->vclass == CLPROC && q->vtype == TYUNKNOWN)
			continue;
		impldcl(q);
		if(q->vtype == TYCHAR)
			{
			if(q->vleng == NULL)	/* character*(*) */
				q->vleng = (expptr)
						mkarg(TYLENG, nextarg(TYLENG) );
			else if(nentry == 1)
				nextarg(TYLENG);
			}
		else if(q->vclass==CLPROC && nentry==1)
			nextarg(TYLENG) ;
		}

if (optimflag)
	optbuff (SKLABEL, 0, ep->entrylabel, 0);
else
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
register Namep q;
register int i;
int align;
ftnint leng, iarrl;
char *memname();
int qstg, qclass, qtype;

pruse(asmfile, USEBSS);
varsizes = NULL;

for(p = hashtab ; p<lasthash ; ++p)
    if(q = p->varp)
	{
	qstg = q->vstg;
	qtype = q->vtype;
	qclass = q->vclass;

	if( (qclass==CLUNKNOWN && qstg!=STGARG) ||
	    (qclass==CLVAR && qstg==STGUNKNOWN) )
		warn1("local variable %s never used", varstr(VL,q->varname) );
	else if(qclass==CLPROC && q->vprocclass==PEXTERNAL && qstg!=STGARG)
		mkext(varunder(VL, q->varname)) ->extstg = STGEXT;

	if (qclass == CLVAR && qstg == STGBSS)
	  {
	    if (SMALLVAR(q->varsize))
	      {
		enlist(q->varsize, q, NULL);
		q->inlcomm = NO;
	      }
	    else
	      {
		if (q->init == NO)
		  {
		    preven(ALIDOUBLE);
		    prlocvar(memname(qstg, q->vardesc.varno), q->varsize);
		    q->inlcomm = YES;
		  }
		else
		  prlocdata(memname(qstg, q->vardesc.varno), q->varsize,
			    q->vtype, q->initoffset, &(q->inlcomm));
	      }
	  }
	else if(qclass==CLVAR && qstg!=STGARG)
		{
		if(q->vdim && !ISICON(q->vdim->nelt) )
			dclerr("adjustable dimension on non-argument", q);
		if(qtype==TYCHAR && (q->vleng==NULL || !ISICON(q->vleng)))
			dclerr("adjustable leng on nonargument", q);
		}

	chkdim(q);
	}

for (i = 0 ; i < nequiv ; ++i)
  if ( (leng = eqvclass[i].eqvleng) != 0 )
    {
      if (SMALLVAR(leng))
	enlist(leng, NULL, eqvclass + i);
      else if (eqvclass[i].init == NO)
	{
	  preven(ALIDOUBLE);
	  prlocvar(memname(STGEQUIV, i), leng);
	  eqvclass[i].inlcomm = YES;
	}
      else
	prlocdata(memname(STGEQUIV, i), leng, TYDREAL, 
		  eqvclass[i].initoffset, &(eqvclass[i].inlcomm));
    }

  outlocvars();
#ifdef SDB
    if(sdbflag) {
      register struct Entrypoint *ep;
      register chainp cp;

      for (ep = entries; ep; ep = ep->entnextp)
	for (cp = ep->arglist ; cp ; cp = cp->nextp)
	  if ((q = (Namep) cp->datap) && q->vstg == STGARG) {
	    q->vdcldone = YES;
	    namestab(q);
	  }
      for (p = hashtab ; p<lasthash ; ++p) if(q = p->varp) {
	if (q->vtype == TYUNKNOWN || q->vtype == TYERROR)
	  continue;
	qstg = q->vstg;
	qclass = q->vclass;
	q->vdcldone = YES;
	if ( ONEOF(qclass, M(CLVAR)|M(CLPARAM)|M(CLPROC)) ) {
	  if (! ONEOF(qstg,M(STGCOMMON)|M(STGARG) ) )
	    namestab(q);
	} 
      }
    }
#endif

  close(vdatafile);
  close(vchkfile);
  unlink(vdatafname);
  unlink(vchkfname);
  vdatahwm = 0;
}



donmlist()
{
register struct Hashentry *p;
register Namep q;

pruse(asmfile, USEINIT);

for(p=hashtab; p<lasthash; ++p)
	if( (q = p->varp) && q->vclass==CLNAMELIST)
		namelist(q);
}


doext()
{
struct Extsym *p;

for(p = extsymtab ; p<nextext ; ++p)
	prext(p);
}




ftnint iarrlen(q)
register Namep q;
{
ftnint leng;

leng = typesize[q->vtype];
if(leng <= 0)
	return(-1);
if(q->vdim)
	if( ISICON(q->vdim->nelt) )
		leng *= q->vdim->nelt->constblock.constant.ci;
	else	return(-1);
if(q->vleng)
	if( ISICON(q->vleng) )
		leng *= q->vleng->constblock.constant.ci;
	else 	return(-1);
return(leng);
}

/* This routine creates a static block representing the namelist.
   An equivalent declaration of the structure produced is:
	struct namelist
		{
		char namelistname[16];
		struct namelistentry
			{
			char varname[16]; #  16 plus null padding -> 20
			char *varaddr;
			short int type;
			short int len;	# length of type
			struct dimensions *dimp; # null means scalar
			} names[];
		};

	struct dimensions
		{
		int numberofdimensions;
		int numberofelements
		int baseoffset;
		int span[numberofdimensions];
		};
   where the namelistentry list terminates with a null varname
   If dimp is not null, then the corner element of the array is at
   varaddr.  However,  the element with subscripts (i1,...,in) is at
   varaddr - dimp->baseoffset + sizeoftype * (i1+span[0]*(i2+span[1]*...)
*/

namelist(np)
Namep np;
{
register chainp q;
register Namep v;
register struct Dimblock *dp;
char *memname();
int type, dimno, dimoffset;
flag bad;


preven(ALILONG);
fprintf(asmfile, LABELFMT, memname(STGINIT, np->vardesc.varno));
putstr(asmfile, varstr(VL, np->varname), 16);
dimno = ++lastvarno;
dimoffset = 0;
bad = NO;

for(q = np->varxptr.namelist ; q ; q = q->nextp)
	{
	vardcl( v = (Namep) (q->datap) );
	type = v->vtype;
	if( ONEOF(v->vstg, MSKSTATIC) )
		{
		preven(ALILONG);
		putstr(asmfile, varstr(VL,v->varname), 16);
		praddr(asmfile, v->vstg, v->vardesc.varno, v->voffset);
		prconi(asmfile, TYSHORT, type );
		prconi(asmfile, TYSHORT,
			type==TYCHAR ?
			    (v->vleng->constblock.constant.ci) :
					(ftnint) typesize[type]);
		if(v->vdim)
			{
			praddr(asmfile, STGINIT, dimno, (ftnint)dimoffset);
			dimoffset += (3 + v->vdim->ndim) * SZINT;
			}
		else
			praddr(asmfile, STGNULL,0,(ftnint) 0);
		}
	else
		{
		dclerr("may not appear in namelist", v);
		bad = YES;
		}
	}

if(bad)
	return;

putstr(asmfile, "", 16);

if(dimoffset > 0)
	{
	fprintf(asmfile, LABELFMT, memname(STGINIT,dimno));
	for(q = np->varxptr.namelist ; q ; q = q->nextp)
		if(dp = q->datap->nameblock.vdim)
			{
			int i;
			prconi(asmfile, TYINT, (ftnint) (dp->ndim) );
			prconi(asmfile, TYINT,
				(ftnint) (dp->nelt->constblock.constant.ci) );
			prconi(asmfile, TYINT,
				(ftnint) (dp->baseoffset->constblock.constant.ci));
			for(i=0; i<dp->ndim ; ++i)
				prconi(asmfile, TYINT,
					dp->dims[i].dimsize->constblock.constant.ci);
			}
	}

}

LOCAL docommon()
{
register struct Extsym *p;
register chainp q;
struct Dimblock *t;
expptr neltp;
register Namep v;
ftnint size;
int type;

for(p = extsymtab ; p<nextext ; ++p)
	if(p->extstg==STGCOMMON)
		{
#ifdef SDB
		if(sdbflag)
			prstab(varstr(XL,p->extname), N_BCOMM, 0, 0);
#endif
		for(q = p->extp ; q ; q = q->nextp)
			{
			v = (Namep) (q->datap);
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
				size = v->vleng->constblock.constant.ci;
			else	size = typesize[type];
			if(t = v->vdim)
				if( (neltp = t->nelt) && ISCONST(neltp) )
					size *= neltp->constblock.constant.ci;
				else
					dclerr("adjustable array in common", v);
			p->extleng += size;
#ifdef SDB
			if(sdbflag)
				{
				namestab(v);
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

/*  frees a temporary block  */

frtemp(p)
Tempp p;
{
Addrp t;

if (optimflag)
	{
	if (p->tag != TTEMP)
		badtag ("frtemp",p->tag);
	t = p->memalloc;
	}
else
	t = (Addrp) p;

/* restore clobbered character string lengths */
if(t->vtype==TYCHAR && t->varleng!=0)
	{
	frexpr(t->vleng);
	t->vleng = ICON(t->varleng);
	}

/* put block on chain of temps to be reclaimed */
holdtemps = mkchain(t, holdtemps);
}



/* allocate an automatic variable slot */

Addrp autovar(nelt, t, lengp)
register int nelt, t;
expptr lengp;
{
ftnint leng;
register Addrp q;

if(lengp)
	if( ISICON(lengp) )
		leng = lengp->constblock.constant.ci;
	else	{
		fatal("automatic variable of nonconstant length");
		}
else
	leng = typesize[t];
autoleng = roundup( autoleng, typealign[t]);

q = ALLOC(Addrblock);
q->tag = TADDR;
q->vtype = t;
if(lengp)
	{
	q->vleng = ICON(leng);
	q->varleng = leng;
	}
q->vstg = STGAUTO;
q->memno = newlabel();
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



/*
 *  create a temporary block (TTEMP) when optimizing,
 *  an ordinary TADDR block when not optimizing
 */

Tempp mktmpn(nelt, type, lengp)
int nelt;
register int type;
expptr lengp;
{
ftnint leng;
chainp p, oldp;
register Tempp q;
Addrp altemp;

if (! optimflag)
	return ( (Tempp) mkaltmpn(nelt,type,lengp) );
if(type==TYUNKNOWN || type==TYERROR)
	badtype("mktmpn", type);

if(type==TYCHAR)
	if( ISICON(lengp) )
		leng = lengp->constblock.constant.ci;
	else	{
		err("adjustable length");
		return( (Tempp) errnode() );
		}
else
	leng = typesize[type];

q = ALLOC(Tempblock);
q->tag = TTEMP;
q->vtype = type;
if(type == TYCHAR)
	{
	q->vleng = ICON(leng);
	q->varleng = leng;
	}

altemp = ALLOC(Addrblock);
altemp->tag = TADDR;
altemp->vstg = STGUNKNOWN;
q->memalloc = altemp;

q->ntempelt = nelt;
q->istemp = YES;
return(q);
}



Addrp mktemp(type, lengp)
int type;
expptr lengp;
{
return( (Addrp) mktmpn(1,type,lengp) );
}



/*  allocate a temporary location for the given temporary block;
    if already allocated, return its location  */

Addrp altmpn(tp)
Tempp tp;

{
Addrp t, q;

if (tp->tag != TTEMP)
	badtag ("altmpn",tp->tag);

t = tp->memalloc;
if (t->vstg != STGUNKNOWN)
	{
	if (tp->vtype == TYCHAR)
		{
		/*
		 * Unformatted I/O parameters are treated like character
		 *	strings (sigh) -- propagate type and length.
		 */
		t = (Addrp) cpexpr(t);
		t->vtype = tp->vtype;
		t->vleng = tp->vleng;
		t->varleng = tp->varleng;
		}
	return (t);
	}

q = mkaltmpn (tp->ntempelt, tp->vtype, tp->vleng);
cpn (sizeof(struct Addrblock), (char*)q, (char*)t);
free ( (charptr) q);
return(t);
}



/*  create and allocate space immediately for a temporary  */

Addrp mkaltemp(type,lengp)
int type;
expptr lengp;
{
return (mkaltmpn(1,type,lengp));
}



Addrp mkaltmpn(nelt,type,lengp)
int nelt;
register int type;
expptr lengp;
{
ftnint leng;
chainp p, oldp;
register Addrp q;

if(type==TYUNKNOWN || type==TYERROR)
	badtype("mkaltmpn", type);

if(type==TYCHAR)
	if( ISICON(lengp) )
		leng = lengp->constblock.constant.ci;
	else	{
		err("adjustable length");
		return( (Addrp) errnode() );
		}

/*
 * if a temporary of appropriate shape is on the templist,
 * remove it from the list and return it
 */

#ifdef notdef
/*
 * This code is broken until SKFRTEMP slots can be processed in putopt()
 *	instead of in optimize() -- all kinds of things in putpcc.c can
 *	bomb because of this.  Sigh.
 */
for(oldp=CHNULL, p=templist  ;  p  ;  oldp=p, p=p->nextp)
	{
	q = (Addrp) (p->datap);
	if(q->vtype==type && q->ntempelt==nelt &&
	    (type!=TYCHAR || q->vleng->constblock.constant.ci==leng) )
		{
		if(oldp)
			oldp->nextp = p->nextp;
		else
			templist = p->nextp;
		free( (charptr) p);

		if (debugflag[14])
			fprintf(diagfile,"mkaltmpn reusing offset %d\n",
				q->memoffset->constblock.constant.ci);
		return(q);
		}
	}
#endif notdef
q = autovar(nelt, type, lengp);
q->istemp = YES;

if (debugflag[14])
	fprintf(diagfile,"mkaltmpn new offset %d\n",
		q->memoffset->constblock.constant.ci);
return(q);
}



/*  The following routine is a patch which is only needed because the	*/
/*  code for processing actual arguments for calls does not allocate	*/
/*  the temps it needs before optimization takes place.  A better	*/
/*  solution is possible, but I do not have the time to implement it	*/
/*  now.								*/
/*									*/
/*					Robert P. Corbett		*/

Addrp
mkargtemp(type, lengp)
int type;
expptr lengp;
{
  ftnint leng;
  chainp oldp, p;
  Addrp q;

  if (type == TYUNKNOWN || type == TYERROR)
    badtype("mkargtemp", type);

  if (type == TYCHAR)
    {
      if (ISICON(lengp))
	leng = lengp->constblock.constant.ci;
      else
	{
	  err("adjustable length");
	  return ((Addrp) errnode());
	}
    }

  oldp = CHNULL;
  p = argtemplist;

  while (p)
    {
      q = (Addrp) (p->datap);
      if (q->vtype == type
	  && (type != TYCHAR || q->vleng->constblock.constant.ci == leng))
	{
	  if (oldp)
	    oldp->nextp = p->nextp;
	  else
	    argtemplist = p->nextp;

	  p->nextp = activearglist;
	  activearglist = p;

	  return ((Addrp) cpexpr(q));
	}

      oldp = p;
      p = p->nextp;
    }

  q = autovar(1, type, lengp);
  activearglist = mkchain(q, activearglist);
  return ((Addrp) cpexpr(q));
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
Namep v;
{
if(v->vstg != STGUNKNOWN)
	dclerr("incompatible common declaration", v);
else
	{
	if(c == (struct Extsym *) 0)
		return;		/* Illegal common block name upstream */
	v->vstg = STGCOMMON;
	c->extp = hookup(c->extp, mkchain(v,CHNULL) );
	}
}




settype(v, type, length)
register Namep  v;
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
	if( (v->vtype = lengtype(type, length))==TYCHAR )
		{
		if(length >= 0)
			v->vleng = ICON(length);
		else if(!(v->vstg == STGARG || v->vclass == CLENTRY ||
			  (v->vclass == CLPROC && v->vprocclass == PTHISPROC)))
			{
			dclerr("illegal adjustable length character variable", v);
			v->vleng = ICON(0);
			}
		}
	}
else if(v->vtype!=type || (type==TYCHAR && v->vleng->constblock.constant.ci!=length) )
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
		badtype("lengtype", type);
	}

if(length != 0)
	err("incompatible type-length combination");

ret:
	return(type);
}





setintr(v)
register Namep  v;
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
register Namep  v;
{
if(v->vclass == CLUNKNOWN)
	v->vclass = CLPROC;
else if(v->vclass != CLPROC)
	dclerr("conflicting declarations", v);

if(v->vprocclass == PUNKNOWN)
	v->vprocclass = PEXTERNAL;
else if(v->vprocclass != PEXTERNAL)
	dclerr("conflicting declarations", v);
}




/* create dimensions block for array variable */

setbound(v, nd, dims)
register Namep  v;
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
if(v->vdim)
	{
	dclerr("redimensioned array", v);
	return;
	}

v->vdim = p = (struct Dimblock *)
		ckalloc( sizeof(int) + (3+6*nd)*sizeof(expptr) );
p->ndim = nd;
p->nelt = ICON(1);

for(i=0 ; i<nd ; ++i)
	{
#ifdef SDB
        if(sdbflag) {
/* Save the bounds trees built up by the grammar routines for use in stabs */

		if(dims[i].lb == NULL) p->dims[i].lb=ICON(1);
        	else p->dims[i].lb= (expptr) cpexpr(dims[i].lb);
                if(ISCONST(p->dims[i].lb)) p->dims[i].lbaddr = (expptr) PNULL;
                else p->dims[i].lbaddr = (expptr) autovar(1, tyint, PNULL);

		if(dims[i].ub == NULL) p->dims[i].ub=ICON(1);
        	else p->dims[i].ub = (expptr) cpexpr(dims[i].ub);
                if(ISCONST(p->dims[i].ub)) p->dims[i].ubaddr = (expptr) PNULL;
                else p->dims[i].ubaddr = (expptr) autovar(1, tyint, PNULL);
	}
#endif
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
			if (!ISINT(q->headblock.vtype)) {
			   dclerr("dimension bounds must be integer expression", v);
			   frexpr(q);
			   q = ICON(0);
			   }
			if ( q->constblock.constant.ci <= 0)
			   {
			   dclerr("array bounds out of sequence", v);
			   frexpr(q);
			   q = ICON(0);
			   }
			p->dims[i].dimsize = q;
			p->dims[i].dimexpr = (expptr) PNULL;
			}
		else	{
			p->dims[i].dimsize = (expptr) autovar(1, tyint, PNULL);
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
	p->baseoffset = (expptr) autovar(1, tyint, PNULL);
	p->basexpr = q;
	}
}



/*
 * Check the dimensions of q to ensure that they are appropriately defined.
 */
LOCAL chkdim(q)
register Namep q;
{
  register struct Dimblock *p;
  register int i;
  expptr e;

  if (q == NULL)
    return;
  if (q->vclass != CLVAR)
    return;
  if (q->vdim == NULL)
    return;
  p = q->vdim;
  for (i = 0; i < p->ndim; ++i)
    {
#ifdef SDB
      if (sdbflag)
	{
	  if (e = p->dims[i].lb)
	    chkdime(e, q);
	  if (e = p->dims[i].ub)
	    chkdime(e, q);
	}
      else
#endif SDB
      if (e = p->dims[i].dimexpr)
	chkdime(e, q);
    }
}



/*
 * The actual checking for chkdim() -- examines each expression.
 */
LOCAL chkdime(expr, q)
expptr expr;
Namep q;
{
  register expptr e;

  e = fixtype(cpexpr(expr));
  if (!ISINT(e->exprblock.vtype))
    dclerr("non-integer dimension", q);
  else if (!safedim(e))
    dclerr("undefined dimension", q);
  frexpr(e);
  return;
}



/*
 * A recursive routine to find undefined variables in dimension expressions.
 */
LOCAL safedim(e)
expptr e;
{
  chainp cp;

  if (e == NULL)
    return 1;
  switch (e->tag)
    {
      case TEXPR:
	if (e->exprblock.opcode == OPCALL || e->exprblock.opcode == OPCCALL)
	  return 0;
	return safedim(e->exprblock.leftp) && safedim(e->exprblock.rightp);
      case TADDR:
	switch (e->addrblock.vstg)
	  {
	    case STGCOMMON:
	    case STGARG:
	    case STGCONST:
	    case STGEQUIV:
	      if (e->addrblock.isarray)
		return 0;
	      return safedim(e->addrblock.memoffset);
	    default:
	      return 0;
	  }
      case TCONST:
      case TTEMP:
	return 1;
    }
  return 0;
}



LOCAL enlist(size, np, ep)
ftnint size;
Namep np;
struct Equivblock *ep;
{
  register sizelist *sp;
  register sizelist *t;
  register varlist *p;

  sp = varsizes;

  if (sp == NULL)
    {
      sp = ALLOC(SizeList);
      sp->size = size;
      varsizes = sp;
    }
  else
    {
      while (sp->size != size)
	{
	  if (sp->next != NULL && sp->next->size <= size)
	    sp = sp->next;
	  else
	    {
	      t = sp;
	      sp = ALLOC(SizeList);
	      sp->size = size;
	      sp->next = t->next;
	      t->next = sp;
	    }
	}
    }

  p = ALLOC(VarList);
  p->next = sp->vars;
  p->np = np;
  p->ep = ep;

  sp->vars = p;

  return;
}



outlocvars()
{

  register varlist *first, *last;
  register varlist *vp, *t;
  register sizelist *sp, *sp1;
  register Namep np;
  register struct Equivblock *ep;
  register int i;
  register int alt;
  register int type;
  char sname[100];
  char setbuff[100];

  sp = varsizes;
  if (sp == NULL)
    return;

  vp = sp->vars;
  if (vp->np != NULL)
    {
      np = vp->np;
      sprintf(setbuff, "\t.set\tv.%d,v.%d\n", bsslabel,
	      np->vardesc.varno);
    }
  else
    {
      i = vp->ep - eqvclass;
      sprintf(setbuff, "\t.set\tv.%d,q.%d\n", bsslabel, i + eqvstart);
    }

  first = last = NULL;
  alt = NO;

  while (sp != NULL)
    {
      vp = sp->vars;
      while (vp != NULL)
	{
	  t = vp->next;
	  if (alt == YES)
	    {
	      alt = NO;
	      vp->next = first;
	      first = vp;
	    }
	  else
	    {
	      alt = YES;
	      if (last != NULL)
	        last->next = vp;
	      else
		first = vp;
	      vp->next = NULL;
	      last = vp;
	    }
	  vp = t;
	}
      sp1 = sp;
      sp = sp->next;
      free((char *) sp1);
    }

  vp = first;
  while(vp != NULL)
    {
      if (vp->np != NULL)
	{
	  np = vp->np;
	  sprintf(sname, "v.%d", np->vardesc.varno);
	  if (np->init)
	    prlocdata(sname, np->varsize, np->vtype, np->initoffset,
		      &(np->inlcomm));
	  else
	    {
	      pralign(typealign[np->vtype]);
	      fprintf(initfile, "%s:\n", sname);
	      prspace(np->varsize);
	    }
	  np->inlcomm = NO;
	}
      else
	{
	  ep = vp->ep;
	  i = ep - eqvclass;
	  if (ep->eqvleng >= 8)
	    type = TYDREAL;
	  else if (ep->eqvleng >= 4)
	    type = TYLONG;
	  else if (ep->eqvleng >= 2)
	    type = TYSHORT;
	  else
	    type = TYCHAR;
	  sprintf(sname, "q.%d", i + eqvstart);
	  if (ep->init)
	    prlocdata(sname, ep->eqvleng, type, ep->initoffset,
		      &(ep->inlcomm));
	  else
	    {
	      pralign(typealign[type]);
	      fprintf(initfile, "%s:\n", sname);
	      prspace(ep->eqvleng);
	    }
	  ep->inlcomm = NO;
	}
      t = vp;
      vp = vp->next;
      free((char *) t);
    }
  fprintf(initfile, "%s\n", setbuff);
  return;
}
