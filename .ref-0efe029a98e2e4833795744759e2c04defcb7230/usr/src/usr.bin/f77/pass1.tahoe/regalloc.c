/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)regalloc.c	5.1 (Berkeley) 6/7/85";
#endif not lint

/*
 * regalloc.c
 *
 * Register optimization routines for f77 compiler, pass 1
 *
 * University of Utah CS Dept modification history:
 *
 * $History$
 * $Log:	regalloc.c,v $
 * Revision 1.4  86/02/12  15:29:16  rcs
 * 4.3 F77. C. Keating.
 * 
 * Revision 2.9  85/03/18  21:35:05  donn
 * Bob Corbett's hack to prevent conflicts between subroutine side effects
 * and register assignment.  Makes the code a lot worse...
 * 
 * Revision 2.8  85/02/22  02:14:08  donn
 * In code like 'x = foo(x)', alreg() would copy the memory version of the
 * variable 'x' into the register version after the assignment, clobbering
 * the result.  A small change to regwrite() seems to prevent this.
 * 
 * Revision 2.7  85/02/16  03:32:45  donn
 * Fixed a bug where the loop test and increment were having register
 * substitution performed twice, once in the environment of the current
 * loop and once in the environment of the containing loop.  If the
 * containing loop puts (say) the inner loop's index variable in register
 * but the inner loop does not, havoc results.
 * 
 * Revision 2.6  85/02/14  23:21:45  donn
 * Don't permit variable references of the form 'a(i)' to be put in register
 * if array 'a' is in common.  This is because there is no good way to
 * identify instances of this sort without getting confused with other
 * variables in the same common block which are in register.  Sigh.
 * 
 * Revision 2.5  85/01/11  21:08:00  donn
 * Made changes so that we pay attention to SAVE statements.  Added a new
 * gensetreturn() function to implement this.
 * 
 * Revision 2.4  84/09/03  22:37:28  donn
 * Changed the treatment of SKRETURN in alreg() so that all variables in
 * register, not just COMMON variables, get written out to memory before a
 * RETURN.  This was causing the return value of a function to get lost when
 * a RETURN was done from inside a loop (among other problems).
 * 
 * Revision 2.3  84/08/04  20:52:42  donn
 * Added fixes for EXTERNAL parameters from Jerry Berkman.
 * 
 * Revision 2.2  84/08/04  20:34:29  donn
 * Fixed a stupidity pointed out by Jerry Berkman -- the 'floats in register'
 * stuff applies if the TARGET is a VAX, not if the local machine is a VAX.
 * 
 * Revision 2.1  84/07/19  12:04:47  donn
 * Changed comment headers for UofU.
 * 
 * Revision 1.5  83/11/27  19:25:41  donn
 * Added REAL to the list of types which may appear in registers (VAXen only).
 * 
 * Revision 1.4  83/11/13  02:38:39  donn
 * Bug fixed in alreg()'s handling of computed goto's.  A '<=' in place of a
 * '<' led to core dumps when we walked off the end of the list of labels...
 * 
 * Revision 1.3  83/11/12  01:25:57  donn
 * Bug in redundant register assignment code, mistakenly carried over some old
 * code that sometimes rewound a slot pointer even when a redundant slot wasn't
 * deleted; this caused an infinite loop...  Seems to work now.
 * 
 * Revision 1.2  83/11/09  14:58:12  donn
 * Took out broken code dealing with redundant register initializations.
 * Couldn't see what to do about redundantly initializing a DO variable but
 * I did fix things so that an assignment from a register into the same
 * register is always deleted.
 * 
 */

#include "defs.h"
#include "optim.h"

#define LABTABSIZE 101
#define VARTABSIZE 1009
#define TABLELIMIT 12

#if TARGET==VAX || TARGET==TAHOE
#define MSKREGTYPES M(TYLOGICAL) | M(TYADDR) | M(TYSHORT) | M(TYLONG) | M(TYREAL)
#if TARGET==TAHOE
#define BUMPREALS	/* put floats last */
#endif
#else
#define MSKREGTYPES M(TYLOGICAL) | M(TYADDR) | M(TYSHORT) | M(TYLONG)
#endif


#define ISREGTYPE(x) ONEOF(x, MSKREGTYPES)

#define MSKVARS M(STGAUTO) | M(STGBSS) | M(STGINIT) | M(STGCONST) |\
		M(STGEQUIV) | M(STGARG) | M(STGCOMMON)

#define ISVAR(x) ((((expptr) x)->headblock.vclass == CLVAR || \
			((expptr) x)->headblock.vclass == CLUNKNOWN) \
                  && ONEOF(((expptr) x)->headblock.vstg, MSKVARS))


typedef
  struct regdata
    {
      field vstg;
      field vtype;
      int memno;
      int memoffset;
      int refs;
      Addrp stgp;
      unsigned isarrayarg : 1;
      unsigned istemp : 1;
      unsigned isset : 1;
      unsigned setfirst : 1;
    } REGDATA;


typedef
  struct labelnode
    {
      struct labelnode *link;
      int labelno;
    } LABELNODE;



typedef
  struct varnode
    {
      struct varnode *link;
      int memoffset;
      unsigned isset : 1;
      unsigned isused : 1;
      unsigned setfirst : 1;
      unsigned unusable : 1;
      int refs;
      Addrp stgp;
    } VARNODE;


typedef
  struct addrnode
    {
      struct addrnode *link;
      field vtype;
      field vstg;
      int memno;
      unsigned istemp : 1;
      unsigned isset : 1;
      unsigned loopset :1;
      unsigned freeuse : 1;
      unsigned mixedtype : 1;
      unsigned fixed : 1;
      int refs;
      struct addrnode *commonlink;
      VARNODE *varlist;
    } ADDRNODE;


typedef
  struct setnode
    {
      struct setnode *link;
      field vstg;
      int memno;
      int memoffset;
    } SETNODE;


typedef
  struct doqueue
    {
      struct doqueue *up, *down;
      Slotp dohead, doend;
      int nregvars;
      REGNODE *reg[MAXREGVAR];
    }  DOQUEUE;

LOCAL DOQUEUE *dqptr, *dqtop, *dqbottom;


LOCAL Slotp dohead;
LOCAL Slotp doend;
LOCAL Slotp newcode;



LOCAL LABELNODE *labeltable[LABTABSIZE];
LOCAL ADDRNODE *vartable[VARTABSIZE];
LOCAL ADDRNODE *commonvars;
LOCAL SETNODE *setlist;
LOCAL int topregvar;
LOCAL int toplcv;
LOCAL int allset;
LOCAL ADDRNODE *currentaddr;
LOCAL int loopcost;
LOCAL REGDATA *regtab[MAXREGVAR];
LOCAL REGDATA *rt[TABLELIMIT];
LOCAL int tabletop;
LOCAL int linearcode;
LOCAL int docount;
LOCAL int globalbranch;
LOCAL int commonunusable;
LOCAL int regdefined[MAXREGVAR];
LOCAL int memdefined[MAXREGVAR];
LOCAL int regaltered[MAXREGVAR];



LOCAL insertlabel(l)
int l;

{
  int key;
  LABELNODE *p;

  key = l % LABTABSIZE;
  for (p = labeltable[key]; p; p = p->link)
    if (p->labelno == l) return;
  p = labeltable[key];
  labeltable[key] = ALLOC(labelnode);
  labeltable[key]->link = p;
  labeltable[key]->labelno = l;
  return;
}



LOCAL int locallabel(l)
int l;

{
  int key;
  LABELNODE *p;

  key = l % LABTABSIZE;
  for (p = labeltable[key]; p; p = p->link)
    if (p->labelno == l) return YES;

  return NO;
}



LOCAL freelabtab()

{
  int i;
  LABELNODE *p, *q;

  for (i = 0; i < LABTABSIZE; i++)
    if (labeltable[i])
      {
	p = labeltable[i];
	labeltable[i] = NULL;
	while (p)
	  {
	    q = p->link;
	    free(p);
	    p = q;
	  }
      }
  return;
}



LOCAL ADDRNODE *getaddr(ap)
Addrp ap;

{
  int key;
  field vstg;
  int memno;
  register ADDRNODE *q;
  ADDRNODE *q1;

  if (!ISVAR(ap))
    fatal("regalloc: bad data sent to getaddr");
  vstg = ap->vstg;
  memno = ap->memno;
  key = (256*vstg + memno) % VARTABSIZE;

  for (q = vartable[key]; q; q = q->link)
    if ((q->vstg == vstg) && (q->memno == memno)) 
      {
	if (ap->istemp) q->istemp = YES;
	if (ap->vtype != q->vtype)
	  q->mixedtype = YES;
	if (!fixedaddress(ap))
	  q->fixed = NO;
	return q;
      }

  q1 = vartable[key];
  vartable[key] = q = ALLOC(addrnode);
  q->link = q1;
  q->vstg = vstg;
  q->memno = memno;
  if (ap->istemp) q->istemp = YES;
  if (fixedaddress(ap)) q->fixed = YES;
  q->vtype = ap->vtype;
  q->varlist = NULL;
  if (vstg == STGCOMMON)
    {
      q->commonlink = commonvars;
      commonvars = q;
    }
  return q;
}



LOCAL VARNODE *getvar(ainfo, ap)
ADDRNODE *ainfo;
Addrp ap;

{
  register VARNODE *q;
  register VARNODE *q1;

  int memoffset;

  if (!ISVAR(ap))
    fatal("regalloc:  bad data sent to getvar");

  memoffset = ap->memoffset->constblock.const.ci;

  for (q = ainfo->varlist; q; q = q->link)
    if (q->memoffset == memoffset)
      return q;

  q1 = ainfo->varlist;
  ainfo->varlist = q = ALLOC(varnode);
  q->link = q1;
  q->memoffset = memoffset;
  q->stgp = (Addrp) cpexpr(ap);
  return q;
}


LOCAL ADDRNODE *lookupaddr(vstg, memno)
field vstg;
int memno;

{
  int key;
  register ADDRNODE *q;

  key = (256*vstg + memno) % VARTABSIZE;

  for (q = vartable[key]; q; q = q->link)
    if ((q->vstg == vstg) && (q->memno == memno))
      return q;

  fatal("regalloc:  lookupaddr");
}


LOCAL VARNODE *lookupvar(ainfo, memoffset)
ADDRNODE *ainfo;
int memoffset;

{
  register VARNODE *q;

  for (q = ainfo->varlist; q; q = q->link)
    if (q->memoffset == memoffset)
      return q;

  fatal("regalloc:  lookupvar");
}



LOCAL int invartable(p)
REGNODE *p;

{
  field vstg;
  int memno;
  int key;
  register ADDRNODE *q;

  vstg = p->vstg;
  memno = p->memno;
  key = (256*vstg + memno) % VARTABSIZE;

  for (q = vartable[key]; q; q = q->link)
    if ((q->vstg == vstg) && (q->memno == memno))
      return YES;

  return NO;
}



LOCAL freevartab()

{
  register ADDRNODE *p;
  ADDRNODE *p1;
  register VARNODE *q;
  VARNODE *q1;
  register int i;

  for (i = 0; i < VARTABSIZE; i++)
    if (vartable[i])
      {
	p = vartable[i];
	vartable[i] = NULL;

	while (p)
	  {
	    for (q = p->varlist; q; q = q1)
	      {
		q1 = q->link;
		frexpr(q->stgp);
		free ((char *) q);
	      }
	    p1 = p->link;
	    free((char *) p);
	    p = p1;
	  }
      }
}



LOCAL insertset(vstg, memno, memoffset)
field vstg;
int memno;
int memoffset;

{
  register SETNODE *p;
  SETNODE *q;

  if (allset) return;
  for (p = setlist; p; p = p->link)
    if((p->vstg == vstg) && (p->memno == memno) && (p->memoffset == memoffset))
      return;

  q = p;
  setlist = p = ALLOC(setnode);
  p->link = q;
  p->vstg = vstg;
  p->memno = memno;
  p->memoffset = memoffset;
  return;
}



LOCAL int insetlist(vstg, memno, memoffset)
field vstg;
int memno;
int memoffset;

{
  register SETNODE *p;

  if (allset) return YES;
  for (p = setlist; p; p = p->link)
    if((p->vstg == vstg) && (p->memno == memno) && (p->memoffset == memoffset))
      return YES;

  return NO;
}



LOCAL clearsets()

{
  register SETNODE *p, *q;

  allset = NO;

  p = setlist;
  while (p)
    {
      q = p->link;
      free ((char *) p);
      p = q;
    }
  setlist = NULL;
  return;
}



LOCAL alreg()

{
  register Slotp sp;
  register int i;
  register ADDRNODE *p;
  register VARNODE *q;
  Slotp sp1, sp2;
  ADDRNODE *addrinfo;
  VARNODE *varinfo;
  struct Labelblock **lp;
  int toptrack;
  int track[MAXREGVAR];
  Addrp ap, ap1;
  DOQUEUE *dqp;
  REGDATA *rp;
  REGNODE *regp;

  if (nregvar >= maxregvar) return;

  commonvars = NULL;
  docount = 0;

  for (sp = dohead; sp != doend->next; sp = sp->next)
    switch (sp->type)
      {
      case SKLABEL:
	insertlabel(sp->label);
	break;

      case SKARIF:
      case SKASGOTO:
      case SKCALL:
      case SKCMGOTO:
      case SKEQ:
      case SKIFN:
      case SKIOIFN:
      case SKSTOP:
      case SKPAUSE:
      case SKRETURN:
	scanvars(sp->expr);
	break;

      case SKDOHEAD:
	++docount;
	break;

      case SKENDDO:
	--docount;
	break;
     
      case SKNULL:
      case SKGOTO:
      case SKASSIGN:
	break;

      default:
	badthing ("SKtype", "alreg-1", sp->type);
      }

  loopcost = 0;
  docount = 1;
  commonunusable = NO;
  if (! dohead->nullslot) fatal ("missing dohead->nullslot -cbb");
  for (sp = dohead->next, globalbranch = NO;
       docount;
       sp = sp->next, clearsets(), globalbranch = NO)
    if (docount > 1)
      switch (sp->type)
	{
	case SKDOHEAD:
	  docount++;
	  break;

	case SKENDDO:
	  docount--;

	default:
	  break;
	}
    else
      switch (sp->type)
	{
	case SKARIF:
#define LM   ((struct Labelblock * *)sp->ctlinfo)[0]->labelno 
#define LZ   ((struct Labelblock * *)sp->ctlinfo)[1]->labelno 
#define LP   ((struct Labelblock * *)sp->ctlinfo)[2]->labelno 

	  if (!locallabel(LM) || !locallabel(LZ) || !locallabel(LP))
	    {
	      setall();
	      globalbranch = YES;
	    }
	  countrefs(sp->expr);
	  break;

	case SKASGOTO:
	  setall();
	  globalbranch = YES;
	  countrefs(sp->expr);
	  break;

	case SKCMGOTO:
	  lp = (struct Labelblock **) sp->ctlinfo;
	  for (i = 0; i < sp->label; i++, lp++)
	    if (!locallabel((*lp)->labelno))
	      {
		setall();
		globalbranch = YES;
		break;
	      }
	  countrefs(sp->expr);
	  break;

	case SKDOHEAD:
	  globalbranch = YES;
	  loopcost = 2;
	  docount++;
	  break;

	case SKENDDO:
	  docount = 0;
	  break;

	case SKGOTO:
	  if (!locallabel(sp->label))
	    {
	      setall();
	      globalbranch = YES;
	    }
	  break;

	case SKIFN:
	case SKIOIFN:
	  if (!locallabel(sp->label))
	    {
	      setall();
	      globalbranch = YES;
	    }
	  countrefs(sp->expr);
	  break;

	case SKEQ:
	case SKCALL:
	case SKSTOP:
	case SKPAUSE:
	  linearcode = YES;
	  countrefs(sp->expr);
	  linearcode = NO;
	  break;
	}

  topregvar = toplcv = nregvar - 1;

  for (i = 0; i < nregvar; i++)
    {
      ap = memversion(regnamep[i]);
      regtab[i] = rp = ALLOC(regdata);
      rp->vstg = ap->vstg;
      rp->vtype = ap->vtype;
      rp->memno = ap->memno;
      rp->memoffset = ap->memoffset->constblock.const.ci;
      rp->isarrayarg = NO;
      rp->stgp = ap;
    }

  for (i = 0; i < MAXREGVAR; i++)
    track[i] = YES;

  for (dqp = dqptr->down; dqp; dqp = dqp->down)
    {
      if (dqp->nregvars - 1 > topregvar)
	topregvar = dqp->nregvars -1;
      for (i = toplcv + 1; i < dqp->nregvars; i++)
	if (track[i])
	  if (regp = dqp->reg[i])
	    if (rp = regtab[i])
	      {
		if (!samevar(rp, regp))
		  track[i] = NO;
	      }
	    else if (invartable(regp))
	      {
		regtab[i] = rp = ALLOC(regdata);
		rp->vstg = regp->vstg;
		rp->vtype = regp->vtype;
		rp->memno = regp->memno;
		rp->memoffset = regp->memoffset;
		addrinfo = lookupaddr(rp->vstg, rp->memno);
		if (regp->isarrayarg)
		  {
		    rp->isarrayarg = YES;
		    rp->refs = addrinfo->refs;
		  }
		else
		  {
		    varinfo = lookupvar(addrinfo, regp->memoffset);
		    rp->refs = varinfo->refs;
		    rp->stgp = (Addrp) cpexpr(varinfo->stgp);
		    rp->istemp = addrinfo->istemp;
		    rp->isset = varinfo->isset;
		    rp->setfirst = varinfo->setfirst;
		  }
	      }
	    else
              track[i] = NO;
	  else
	    track[i] = NO;
    }

  toptrack = topregvar;

  for (i = toplcv + 1; i <= topregvar; i++)
    if (regtab[i])
      if ((track[i] == NO) || (regtab[i]->refs <= 0))
        {
	  free((char *) regtab[i]);
	  regtab[i] = NULL;
        }

  tabletop = -1;
  if (topregvar < maxregvar - 1)
    for (i = 0; i < VARTABSIZE; i++)
      for (p = vartable[i]; p; p = p->link)
	{
	  entableaddr(p);
	  if ((!p->loopset) &&
	      (!p->mixedtype) &&
	      (p->vstg != STGARG) &&
	      !((p->vstg == STGCOMMON) && ((!p->fixed) || commonunusable)))
	    for (q = p->varlist; q; q = q->link)
	      entablevar(q);
	}

  for (i = 0; (i <= tabletop) && (topregvar + 1 < maxregvar); i++)
    {
      if (inregtab(rt[i]) || (loopcost && rt[i]->isset))
	continue;
      topregvar++;
      regtab[topregvar] = rp = ALLOC(regdata);
      rp->vstg = rt[i]->vstg;
      rp->vtype = rt[i]->vtype;
      rp->memno = rt[i]->memno;
      rp->memoffset = rt[i]->memoffset;
      rp->refs = rt[i]->refs;
      rp->stgp = (Addrp) cpexpr(rt[i]->stgp);
      rp->isarrayarg = rt[i]->isarrayarg;
      rp->istemp = rt[i]->istemp;
      rp->isset = rt[i]->isset;
      rp->setfirst = rt[i]->setfirst;
    }

  for (i = toplcv + 1; i <= topregvar; i++)
    {
      if (rp = regtab[i])
	if (rp->isarrayarg)
	  {
	    ap = ALLOC(Addrblock);
	    ap->tag = TADDR;
	    ap->vstg = STGREG;
	    ap->vtype = TYADDR;
	    ap->vclass = CLVAR;
	    ap->memno = regnum[i];
	    ap->memoffset = ICON(0);

	    ap1 = ALLOC(Addrblock);
	    ap1->tag = TADDR;
	    ap1->vstg = rp->vstg;
	    ap1->vtype = rp->vtype;
	    ap1->vclass = CLVAR;
	    ap1->memno = rp->memno;
	    ap1->memoffset = ICON(0);

	    insertassign(dohead, ap, addrof(ap1));
	  }
        else if (!(rp->setfirst && rp->istemp))
	  {
	    if (rp->istemp)
	      for (sp = newcode; sp && sp != dohead; sp = sp->next)
	        if (sp->type == SKEQ)
		  {
		    ap = (Addrp) sp->expr->exprblock.leftp;
		    if ((ap->vstg == rp->vstg) && (ap->memno == rp->memno) &&
			fixedaddress(ap) &&
			(ap->memoffset->constblock.const.ci == rp->memoffset))
		      {
			changetoreg(ap, i);
			goto L1;
		      }
		  }
	    ap = (Addrp) cpexpr(rp->stgp);
	    changetoreg(ap, i);
	    insertassign(dohead, ap, cpexpr(rp->stgp));
	  }
L1:;
    }

  for (i = toplcv + 1; i <= topregvar; i++)
    if (rp = regtab[i])
      if (rp->isset && !(rp->istemp || rp->isarrayarg))
	{
	  ap = (Addrp) cpexpr(rp->stgp);
	  changetoreg(ap, i);
	  appendassign(doend, cpexpr(rp->stgp), ap);
	}

  docount = 1;
  clearmems();
  setregs();
  sp = dohead->next;
  if (loopcost)
    for (i = toptrack + 1; i <= topregvar; i++)
      if ((rp = regtab[i]) && !rp->isarrayarg)
	{
	  ap = (Addrp) cpexpr(rp->stgp);
	  changetoreg(ap, i);
	  insertassign(sp, cpexpr(rp->stgp), ap);
	}

  for ( sp = dohead->next;
	docount || sp->type != SKNULL;
	sp = sp->next)
    if (docount > 1)
      switch (sp->type)
	{
	case SKDOHEAD:
	  docount++;
	  break;

	case SKENDDO:
	  if (--docount == 1)
	    {
	      /*
	       * Remove redundant stores to memory.
	       */
	      sp1 = sp->nullslot->next;
	      while (sp1)
		{
		  if (regtomem(sp1))
		    {
		      ap = (Addrp) sp1->expr->exprblock.rightp;
		      sp2 = sp1->next;
		      for (i = toplcv + 2; i <= toptrack; i++)
			if (regtab[i] && (regnum[i] == ap->memno))
			  {
			    deleteslot(sp1);
			    break;
			  }
		      sp1 = sp2;
		    }
		  else
		    sp1 = NULL;
		}

	      /*
	       * Restore register variables (complement to DOHEAD code).
	       */
	      sp1 = sp->nullslot->next;
	      for (i = toplcv + 1; i <= topregvar; i++)
		if (rp = regtab[i])
		  if (!regdefined[i])
		    if (i >= dqp->nregvars || !samevar(rp, dqp->reg[i]))
		      {
			ap = (Addrp) cpexpr(rp->stgp);
			changetoreg(ap, i);
			insertassign(sp1, ap, cpexpr(rp->stgp));
			regdefined[i] = YES;
		      }

	      clearmems();
	      if (toplcv + 1 < maxregvar)
		memdefined[toplcv + 1] = YES;
	      sp = sp1->prev;
	    }
	  break;
	}
      else
	{
	  setregs();
	  for (i = 0; i <= MAXREGVAR; i++)
	    regaltered[i] = NO;
	  globalbranch = NO;

	  switch (sp->type)
	    {
	    case SKLABEL:
	      clearmems();
	      break;

	    case SKGOTO:
	      if (!locallabel(sp->label))
		gensetall(sp);
	      break;

	    case SKENDDO:
	      docount = 0;
	      break;

	    case SKRETURN:
	      gensetreturn(sp);
	      linearcode = YES;
	      regwrite(sp, sp->expr);
	      linearcode = NO;
	      break;

	    case SKDOHEAD:
	      /*
	       * If one of the current loop's register variables is not in
	       * register in an inner loop, we must save it.  It's a pity
	       * we don't save enough info to optimize this properly...
	       */
	      for (dqp = dqptr->down; dqp; dqp = dqp->down)
		if (dqp->dohead == sp)
		  break;
	      if (dqp == NULL)
		fatal("confused in alreg loop analysis");
	      for (i = toplcv + 1; i <= topregvar; i++)
		if (rp = regtab[i])
		  if (!memdefined[i])
		    if (i >= dqp->nregvars || !samevar(rp, dqp->reg[i]))
		      {
			ap = (Addrp) cpexpr(rp->stgp);
			changetoreg(ap, i);
			insertassign(sp, cpexpr(rp->stgp), ap);
			memdefined[i] = YES;
			regdefined[i] = NO;
		      }

	      docount++;
	      globalbranch = YES;
	      break;

	    case SKEQ:
	    case SKCALL:
	    case SKSTOP:
	    case SKPAUSE:
	      linearcode = YES;
	      regwrite(sp, sp->expr);
	      for (i = toplcv + 1; i <= topregvar; i++)
		if (!regdefined[i] && ((rp = regtab[i]) && rp->isset))
		  {
		    ap = (Addrp) cpexpr(rp->stgp);
		    changetoreg(ap, i);
		    appendassign(sp, ap, cpexpr(rp->stgp));
		    sp = sp->next;
		    regdefined[i] = YES;
		  }
	      linearcode = NO;

	      /*
	       * Eliminate redundant register moves.
	       */
	      if (regtoreg(sp))
		{
		  ap = (Addrp) sp->expr->exprblock.leftp;
	          sp1 = sp->prev;
		  for (i = toplcv + 1; i <= toptrack; i++)
		    if (regtab[i] && (regnum[i] == ap->memno))
		      {
			deleteslot(sp);
			sp = sp1;
			break;
		      }
		}
	      break;

	    case SKARIF:
	      if (!locallabel(LM) || !locallabel(LZ) || !locallabel(LP))
		{
		  gensetall(sp);
		  globalbranch = YES;
		}
	      regwrite(sp, sp->expr);
	      break;

	    case SKASGOTO:
	      gensetall(sp);
	      globalbranch = YES;
	      regwrite(sp, sp->expr);
	      break;

	    case SKCMGOTO:
	      lp = (struct Labelblock **) sp->ctlinfo;
	      for (i = 0; i < sp->label; i++, lp++)
		if (!locallabel((*lp)->labelno))
		  {
		    gensetall(sp);
		    globalbranch = YES;
		    break;
		  }
	      regwrite(sp, sp->expr);
	      break;

	    case SKIFN:
	    case SKIOIFN:
	      if (!locallabel(sp->label))
		{
		  gensetall(sp);
		  globalbranch = YES;
		}
	      regwrite(sp, sp->expr);
	      break;

	    case SKNULL:
	    case SKASSIGN:
	      break;

	    default:
	      badthing ("SKtype","alreg-3",sp->type);
	    }
	  
	  for (i = toplcv + 1; i <= topregvar; i++)
	    if (regaltered[i])
	      memdefined[i] = NO;
	}

  if (topregvar + 1 > highregvar)
    highregvar = topregvar + 1;
  dqptr->nregvars = topregvar + 1;
  for (i = 0; i <= topregvar; i++)
    if (rp = regtab[i])
      {
	dqptr->reg[i] = regp = ALLOC(regnode);
	regp->vstg = rp->vstg;
	regp->vtype = rp->vtype;
	regp->memno = rp->memno;
	regp->memoffset = rp->memoffset;
	regp->isarrayarg = rp->isarrayarg;
	frexpr(rp->stgp);
	free((char *) rp);
	regtab[i] = NULL;
      }

  while (tabletop >= 0)
    free((char *) rt[tabletop--]);
  freelabtab();
  freevartab();
  return;
}



LOCAL scanvars(p)
expptr p;

{
  Addrp ap;
  ADDRNODE *addrinfo;
  VARNODE *varinfo;
  chainp args;
  VARNODE *q;

  if (p == NULL) return;

  switch (p->tag)
    {
    case TCONST:
      return;

    case TEXPR:
      switch (p->exprblock.opcode)
	{
	case OPASSIGN:
	  scanassign(p);
	  return;

	case OPPLUSEQ:
	case OPSTAREQ:
	  scanopeq(p);
	  return;

	case OPCALL:
	  scancall(p);
	  return;

	default:
	  scanvars(p->exprblock.vleng);
	  scanvars(p->exprblock.leftp);
	  scanvars(p->exprblock.rightp);
	  return;
	}

    case TADDR:
      ap = (Addrp) p;
      scanvars(ap->vleng);
      scanvars(ap->memoffset);
      if (!ISVAR(ap)) return;

      addrinfo = getaddr(ap);
      if (fixedaddress(ap))
	{
	  if (ISREGTYPE(ap->vtype))
	    {
	      varinfo = getvar(addrinfo, ap);
	      varinfo->isused = YES;
	    }
	}
      else
	{
	  addrinfo->freeuse = YES;
	  for (q = addrinfo->varlist; q; q = q->link)
	    q->isused = YES;
	}
      return;

    case TLIST:
      for (args = p->listblock.listp; args; args = args->nextp)
	scanvars(args->datap);
      return;

    default:
      badtag ("regalloc:scanvars", p->tag);
    }
}



LOCAL scanassign(ep)
Exprp ep;

{
  Addrp lhs;
  VARNODE *varinfo;
  ADDRNODE *addrinfo;

  scanvars(ep->rightp);
  if (ep->leftp->tag == TADDR)
    {
      lhs = (Addrp) ep->leftp;
      scanvars(lhs->vleng);
      scanvars(lhs->memoffset);
      if ((lhs->vstg == STGREG) || (lhs->vstg == STGPREG))
	return;
      if (ISVAR(lhs))
	{
          addrinfo = getaddr(lhs);
          addrinfo->isset = YES;
	  if (docount > 1)
		addrinfo->loopset = YES;
          if (fixedaddress(lhs) && ISREGTYPE(lhs->vtype))
	    {
	      varinfo = getvar(addrinfo, lhs);
	      if (addrinfo->freeuse) varinfo->isused = YES;
	      varinfo->isset = YES;
	      if (!addrinfo->freeuse && !varinfo->isused)
	        varinfo->setfirst = YES;
	    }
        }
    }
  else
    badtag ("regalloc:scanassign", ep->leftp->tag);
}



LOCAL scanopeq(ep)
Exprp ep;

{
  Addrp lhs;
  ADDRNODE *addrinfo;
  VARNODE *varinfo;

  scanvars(ep->rightp);
  if (ep->leftp->tag == TADDR)
    {
      lhs = (Addrp) ep->leftp;
      scanvars(lhs->vleng);
      scanvars(lhs->memoffset);
      if ((lhs->vstg == STGREG) || (lhs->vstg == STGPREG))
	return;
      if (ISVAR(lhs))
	{
          addrinfo = getaddr(lhs);
          addrinfo->isset = YES;
	  if (docount > 1)
		addrinfo->loopset = YES;
          if (fixedaddress(lhs))
	    {
	      if (ISREGTYPE(lhs->vtype))
	        {
	          varinfo = getvar(addrinfo, lhs);
	          varinfo->isused = YES;
	          varinfo->isset = YES;
	        }
	    }
        }
      else
	addrinfo->freeuse = YES;
    }
  else
    badtag ("regalloc:scanopeq", ep->leftp->tag);
}



LOCAL scancall(ep)
Exprp ep;

{
  Addrp lhs;
  chainp args;
  Addrp ap;
  VARNODE *varinfo;
  ADDRNODE *addrinfo;

  lhs = (Addrp) ep->leftp;
  scanvars(lhs->vleng);
  scanvars(lhs->memoffset);

  if (ep->rightp == NULL) return;

  if (lhs->vstg != STGINTR)
    {
      args = ep->rightp->listblock.listp;
      for (; args; args = args->nextp)
	{
	  if (args->datap->tag == TADDR)
	    {
	      ap = (Addrp) args->datap;
	      scanvars(ap->vleng);
	      scanvars(ap->memoffset);
	      if (!ISVAR(ap)) continue;

	      addrinfo = getaddr(ap);
	      addrinfo->isset = YES;
	      if (docount > 1)
		addrinfo->loopset = YES;
	      if (fixedaddress(ap))
		{
		  varinfo = getvar(addrinfo, ap);
		  if (ap->vstg != STGCONST)
		    varinfo->isset = YES;
		  varinfo->isused = YES;
		}
	      else
		addrinfo->freeuse = YES;
	    }
	  else
	    scanvars(args->datap);
	}
    }
  else
    scanvars(ep->rightp);

  return;
}



LOCAL int fixedaddress(ap)
Addrp ap;

{
  if (!ap->memoffset)
    return NO;
  return (ISCONST(ap->memoffset) && ISINT(ap->memoffset->headblock.vtype));
}



LOCAL countrefs(p)
expptr p;

{
  Addrp ap;
  ADDRNODE *addrinfo;
  VARNODE *varinfo;
  VARNODE *vp;
  chainp args;

  if (p == NULL) return;

  switch (p->tag)
    {
    case TCONST:
      return;

    case TEXPR:
      switch (p->exprblock.opcode)
	{
	case OPCALL:
	  if (p->exprblock.leftp->tag != TADDR)
	    badtag ("regalloc:countrefs", p->exprblock.leftp->tag);
	  countrefs(p->exprblock.leftp->addrblock.vleng);
	  countrefs(p->exprblock.leftp->addrblock.memoffset);

	  if (p->exprblock.leftp->addrblock.vstg != STGINTR)
	    {
	      if (!commonunusable)
		if (linearcode)
		  setcommon();
	        else
		  commonunusable = YES;
	      if (p->exprblock.rightp == NULL) return;
	      args = p->exprblock.rightp->listblock.listp;
	      for (; args; args = args->nextp)
		if (args->datap->tag == TADDR)
		  {
		    ap = (Addrp) args->datap;
		    countrefs(ap->vleng);
		    countrefs(ap->memoffset);
		    if (!ISVAR(ap) || ap->vstg == STGCONST) continue;
		    addrinfo = lookupaddr(ap->vstg, ap->memno);
		    if (ap->vstg == STGARG)
		      addrinfo->refs++;
		    for (vp = addrinfo->varlist; vp; vp = vp->link)
		      if (linearcode)
		        if (!insetlist(ap->vstg, ap->memno, vp->memoffset))
			  if (addrinfo->istemp)
			    vp->refs--;
			  else
			    {
			      vp->refs -= 2;
			      insertset(ap->vstg, ap->memno, vp->memoffset);
			    }
		        else
			  vp->refs--;
		      else
			{
			  if (!addrinfo->istemp)
			    vp->unusable = YES;
			}
		  }
		else
		  countrefs(args->datap);
            }
	  else
	    {
	      if (p->exprblock.rightp == NULL) return;
	      args = p->exprblock.rightp->listblock.listp;
	      for (; args; args = args->nextp)
		if (args->datap->tag == TADDR)
		  {
		    ap = (Addrp) args->datap;
		    countrefs(ap->vleng);
		    countrefs(ap->memoffset);
		    if (!ISVAR(ap) || ap->vstg == STGCONST) continue;
		    addrinfo = lookupaddr(ap->vstg, ap->memno);
		    addrinfo->refs++;
		    for (vp = addrinfo->varlist; vp; vp = vp->link)
		      if (!insetlist(ap->vstg, ap->memno, vp->memoffset))
			{
			  vp->refs--;
			  insertset(ap->vstg, ap->memno, vp->memoffset);
			}
		  }
		else
		  countrefs(args->datap);
	    }
	  return;

	case OPASSIGN:
	case OPPLUSEQ:
	case OPSTAREQ:
	  countrefs(p->exprblock.vleng);
	  countrefs(p->exprblock.rightp);
	  ap = (Addrp) p->exprblock.leftp;
	  if (fixedaddress(ap))
	    if (globalbranch)
	      {
		countrefs(ap->vleng);
		countrefs(ap->memoffset);
	      }
	    else
	      countrefs(ap);
	  else if (linearcode)
	    {
	      countrefs(ap);
	      for (vp = lookupaddr(ap->vstg, ap->memno)->varlist;
		   vp;
		   vp = vp->link)
		vp->refs--;
	    }
	  else
	    {
	      countrefs(ap);
	      for (vp = lookupaddr(ap->vstg, ap->memno)->varlist;
		   vp;
		   vp = vp->link)
		vp->unusable = YES;
	    }
	  return;

	default:
	  countrefs(p->exprblock.vleng);
	  countrefs(p->exprblock.leftp);
	  countrefs(p->exprblock.rightp);
	  return;
	}

    case TADDR:
      ap = (Addrp) p;
      countrefs(ap->vleng);
      countrefs(ap->memoffset);
      if (!ISVAR(ap)) return;

      addrinfo = lookupaddr(ap->vstg, ap->memno);
      if (ap->vstg == STGARG)
	addrinfo->refs++;

      if (fixedaddress(ap))
	{
	  if (ISREGTYPE(ap->vtype))
	    {
	      varinfo = lookupvar(addrinfo, ap->memoffset->constblock.const.ci);
	      varinfo->refs++;
	    }
	}
      else
	for (vp = addrinfo->varlist; vp; vp = vp->link)
	  if (!insetlist(ap->vstg, ap->memno, vp->memoffset))
	    {
	      vp->refs--;
	      insertset(ap->vstg, ap->memno, vp->memoffset);
	    }
      return;

    case TLIST:
      args = p->listblock.listp;
      for (; args; args = args->nextp)
	countrefs(args->datap);
      return;

    default:
      badtag ("regalloc:countrefs", p->tag);
    }
}



LOCAL regwrite(sp, p)
Slotp sp;
expptr p;

{
  register int i;
  REGDATA *rp;
  chainp args;
  Addrp ap, ap1;
  int memoffset;

  if (p == NULL) return;

  switch (p->tag)
    {
    case TCONST:
      return;

    case TEXPR:
      switch (p->exprblock.opcode)
	{
	case OPCALL:
	  ap = (Addrp) p->exprblock.leftp;
	  regwrite(sp, ap->vleng);
	  regwrite(sp, ap->memoffset);
	  if (ap->vstg != STGINTR)
	    {
	      if (linearcode)
		{
		  gensetcommon(sp);
		  for (i = toplcv + 1; i <= topregvar; i++)
		    if ((rp = regtab[i]) && (rp->vstg == STGCOMMON))
		      regdefined[i] = NO;
		}
	      if (p->exprblock.rightp == NULL) return;
	      args = p->exprblock.rightp->listblock.listp;
	      for (; args; args = args->nextp)
		if (args->datap->tag == TADDR)
		  {
		    ap = (Addrp) args->datap;
		    regwrite(sp, ap->vleng);
		    regwrite(sp, ap->memoffset);
		    for (i = toplcv + 1; i <= topregvar; i++)
		      if ((rp = regtab[i]) &&
			  !rp->isarrayarg &&
			  !rp->istemp &&
			  (rp->vstg == ap->vstg) &&
			  (rp->memno == ap->memno))
			{
			  regdefined[i] = NO;
			  if (!memdefined[i])
			    {
			      ap1 = (Addrp) cpexpr(rp->stgp);
			      changetoreg(ap1, i);
			      insertassign(sp, cpexpr(rp->stgp), ap1);
			      memdefined[i] = YES;
			    }
			}
		      else if (rp->isarrayarg &&
			       (ap->vstg == STGARG) &&
			       (ap->memno == rp->memno))
			{
			  ap->vstg = STGPREG;
			  ap->memno = regnum[i];
			}
		  }
		else
		  regwrite(sp, args->datap);
	    }
	  else
	    {
	      if (p->exprblock.rightp == NULL) return;
	      args = p->exprblock.rightp->listblock.listp;
	      for (; args; args = args->nextp)
		if (args->datap->tag == TADDR)
		  {
		    ap = (Addrp) args->datap;
		    regwrite(sp, ap->vleng);
		    regwrite(sp, ap->memoffset);
		    for (i = toplcv + 1; i <= topregvar; i++)
		      if ((rp = regtab[i]) &&
			  !rp->isarrayarg &&
			  !rp->istemp &&
			  (rp->vstg == ap->vstg) &&
			  (rp->memno == ap->memno) &&
			  !memdefined[i])
			{
			  ap1 = (Addrp) cpexpr(rp->stgp);
			  changetoreg(ap1, i);
			  insertassign(sp, cpexpr(rp->stgp), ap1);
			  memdefined[i] = YES;
			}
		      else if (rp->isarrayarg &&
			       (ap->vstg == STGARG) &&
			       (rp->memno == ap->memno))
			{
			  ap->vstg = STGPREG;
			  ap->memno = regnum[i];
			}
		  }
		else
		  {
		    regwrite(sp, args->datap);
		  }
	    }
	  return;

	case OPASSIGN:
	case OPPLUSEQ:
	case OPSTAREQ:
	  regwrite(sp, p->exprblock.vleng);
	  regwrite(sp, p->exprblock.rightp);
	  ap = (Addrp) p->exprblock.leftp;
	  regwrite(sp, ap->vleng);
	  regwrite(sp, ap->memoffset);

	  if (ap->vstg == STGARG)
	    for (i = toplcv + 1; i<=topregvar; i++)
	      if ((rp = regtab[i]) &&
		  rp->isarrayarg &&
		  (rp->memno == ap->memno))
		{
		  ap->vstg = STGPREG;
		  ap->memno = regnum[i];
		  return;
		}

	  if (fixedaddress(ap))
	    {
	      memoffset = ap->memoffset->constblock.const.ci;
	      for (i = toplcv + 1; i <= topregvar; i++)
		if ((rp = regtab[i]) &&
		    !rp->isarrayarg &&
		    (rp->vstg == ap->vstg) &&
		    (rp->memno == ap->memno) &&
		    (rp->memoffset == memoffset))
		  {
		    changetoreg(ap, i);
		    if (globalbranch)
		      {
			p->exprblock.rightp = (expptr) cpexpr(p);
			p->exprblock.leftp = (expptr) cpexpr(rp->stgp);
			p->exprblock.opcode = OPASSIGN;
			memdefined[i] = YES;
		      }
		    else
		      {
			regaltered[i] = YES;
			regdefined[i] = YES;
		      }
		  }
	      return;
	    }

	  if (linearcode)
	    for (i = toplcv + 1; i <= topregvar; i++)
	      if ((rp = regtab[i]) &&
		  !rp->isarrayarg &&
		  (rp->vstg == ap->vstg) &&
		  (rp->memno == ap->memno))
		regdefined[i] = NO;
	  return;

	default:
	  regwrite(sp, p->exprblock.vleng);
	  regwrite(sp, p->exprblock.leftp);
	  regwrite(sp, p->exprblock.rightp);
	  return;
	}

    case TADDR:
      ap = (Addrp) p;
      regwrite(sp, ap->vleng);
      regwrite(sp, ap->memoffset);

      if (ap->vstg == STGARG)
	for (i = toplcv + 1; i <= topregvar; i++)
	  if ((rp = regtab[i]) &&
	      rp->isarrayarg &&
	      (rp->memno == ap->memno))
	    {
	      ap->vstg = STGPREG;
	      ap->memno = regnum[i];
	      return;
	    }

      if (fixedaddress(ap))
	{
          memoffset = ap->memoffset->constblock.const.ci;
	  for (i = toplcv + 1; i <= topregvar; i++)
	    if ((rp = regtab[i]) &&
		!rp->isarrayarg &&
		(rp->vstg == ap->vstg) &&
		(rp->memno == ap->memno) &&
		(rp->memoffset == memoffset))
	      {
		changetoreg(ap, i);
		return;
	      }
	}
      else
	{
	  for (i = toplcv + 1; i <= topregvar; i++)
	    if ((rp = regtab[i]) &&
		!rp->isarrayarg &&
		(rp->vstg == ap->vstg) &&
		(rp->memno == ap->memno) &&
		!memdefined[i])
	      {
		ap1 = (Addrp) cpexpr(rp->stgp);
		changetoreg(ap1, i);
		insertassign(sp, cpexpr(rp->stgp), ap1);
		memdefined[i] = YES;
	      }
	}
      return;

    case TLIST:
      for (args = p->listblock.listp; args; args = args->nextp)
	regwrite(sp, args->datap);
      return;

    default:
      badtag ("regalloc:regwrite", p->tag);
    }
}



LOCAL setcommon()

{
  ADDRNODE *ap;
  VARNODE *vp;

  for (ap = commonvars; ap; ap = ap->commonlink)
    for (vp = ap->varlist; vp; vp = vp->link)
      if (!insetlist(ap->vstg, ap->memno, vp->memoffset))
	{
	  vp->refs -= 2;
	  insertset(ap->vstg, ap->memno, vp->memoffset);
	}
      else
	vp->refs--;

  return;
}

 

LOCAL setall()

{
  register int i;
  register ADDRNODE *p;
  register VARNODE *q;

  for (i = 0; i < VARTABSIZE; i++)
    for (p = vartable[i]; p; p = p->link)
      if (p->istemp || !p->isset)
	break;
      else
	for (q = p->varlist; q; q = q->link)
	  if (q->isset && !insetlist(p->vstg, p->memno, q->memoffset))
	    q->refs--;

  allset = YES;
  return;
}



LOCAL int samevar(r1, r2)
register REGDATA *r1;
register REGNODE *r2;

{
  if ((r1->vstg != r2->vstg) ||
      (r1->memno != r2->memno) ||
      (r1->isarrayarg != r2->isarrayarg))
    return NO;
  if (r1->isarrayarg)
    return YES;
  return (r1->memoffset == r2->memoffset);
}



LOCAL entableaddr(p)
ADDRNODE *p;

{
  int refs;
  Addrp ap;
  register int i;

  if (p->vstg != STGARG)
    {
      currentaddr = p;
      return;
    }

  refs = p->refs;
  if (refs <= 0) return;

  if (tabletop < 0)
    tabletop = i = 0;
  else if (refs > rt[tabletop]->refs)
    {
      if (tabletop + 1 < TABLELIMIT)
	tabletop++;
      else
	{
	  frexpr(rt[tabletop]->stgp);
	  free((char *) rt[tabletop]);
	}

      for (i = tabletop; i > 0; i--)
	if (refs > rt[i - 1]->refs)
	  rt[i] = rt[i - 1];
	else
	  break;
    }
  else if (tabletop + 1 < TABLELIMIT)
    i = ++tabletop;
  else
    return;

  rt[i] = ALLOC(regdata);
  rt[i]->vstg = p->vstg;
  rt[i]->vtype = p->vtype;
  rt[i]->memno = p->memno;
  rt[i]->refs = refs;
  rt[i]->isarrayarg = YES;

  return;
}




LOCAL entablevar(p)
VARNODE *p;

{
  int refs;
  register int i;

  if (p->unusable) return;
  refs = p->refs - loopcost;
  if (refs <= 0) return;

  if (tabletop < 0)
    tabletop = i = 0;
  else if (refs > rt[tabletop]->refs
#ifdef BUMPREALS	/* put floats last */
    || currentaddr->vtype!=TYREAL && rt[tabletop]->vtype==TYREAL && !rt[tabletop]->isarrayarg
#endif
    ){
      if (tabletop + 1 < TABLELIMIT)
        tabletop++;
      else
	{
	  frexpr(rt[tabletop]->stgp);
          free((char *) rt[tabletop]);
	}

      for (i = tabletop; i > 0; i--)
        if (refs > rt[i - 1]->refs
#ifdef BUMPREALS	/* put floats last */
         || currentaddr->vtype!=TYREAL && rt[i-1]->vtype==TYREAL && !rt[i-1]->isarrayarg
#endif
	)
          rt[i] = rt[i - 1];
        else
          break;
    }
  else if (tabletop + 1 < TABLELIMIT)
    i = ++tabletop;
  else
    return;

  rt[i] = ALLOC(regdata);
  rt[i]->vstg = currentaddr->vstg;
  rt[i]->vtype = currentaddr->vtype;
  rt[i]->memno = currentaddr->memno;
  rt[i]->memoffset = p->memoffset;
  rt[i]->refs = refs;
  rt[i]->stgp = (Addrp) cpexpr(p->stgp);
  rt[i]->isarrayarg = NO;
  rt[i]->istemp = currentaddr->istemp;
  rt[i]->isset = p->isset;
  rt[i]->setfirst = p->setfirst;

  return;
}



LOCAL int inregtab(p)
register REGDATA *p;

{
  register REGDATA *rp;
  register int i;

  for (i = 0; i <= topregvar; i++)
    if (rp = regtab[i])
      if ((rp->vstg == p->vstg) &&
	  (rp->memno == p->memno) &&
	  (rp->isarrayarg == p->isarrayarg))
	if (rp->isarrayarg)
          return YES;
        else if (rp->memoffset == p->memoffset)
          return YES;

  return NO;
}



LOCAL changetoreg(ap, i)
register Addrp ap;
int i;

{
  ap->vstg = STGREG;
  ap->memno = regnum[i];
  frexpr(ap->memoffset);
  ap->memoffset = ICON(0);
  ap->istemp = NO;
  return;
}



LOCAL insertassign(sp, dest, src)
Slotp sp;
Addrp dest;
expptr src;

{
  Slotp newslot;
  expptr p;

  p = mkexpr(OPASSIGN, dest, src);
  newslot = optinsert (SKEQ,p,0,0,sp);

  if (sp == dohead)
    if (!newcode)
      newcode = newslot;

  return;
}


LOCAL appendassign(sp, dest, src)
Slotp sp;
Addrp dest;
expptr src;

{
  Slotp newslot;
  expptr p;

  if (!sp)
    fatal ("regalloc:appendassign");

  p = mkexpr(OPASSIGN, dest, src);
  newslot = optinsert (SKEQ,p,0,0,sp->next);

  return;
}



LOCAL int regtomem(sp)
Slotp sp;

{
  expptr p, l, r;
  int i;

  if (sp->type != SKEQ) return NO;

  p = sp->expr;
  if ((p->tag != TEXPR) || (p->exprblock.opcode != OPASSIGN))
    return NO;

  r = p->exprblock.rightp;
  if ((r->tag != TADDR) || (r->addrblock.vstg != STGREG))
    return NO;

  l = p->exprblock.leftp;
  if (l->tag != TADDR)
    return NO;
  i = r->addrblock.memno;
  if (regtab[i] &&
      (l->addrblock.vstg == regtab[i]->vstg) &&
      (l->addrblock.memno == regtab[i]->memno) &&
      fixedaddress(l) &&
      (l->addrblock.memoffset->constblock.const.ci == regtab[i]->memoffset))
    return YES;

  return NO;
}



LOCAL int regtoreg(sp)
Slotp sp;

{
  expptr p, l, r;

  if (sp->type != SKEQ) return NO;

  p = sp->expr;
  if ((p->tag != TEXPR) || (p->exprblock.opcode != OPASSIGN))
    return NO;

  l = p->exprblock.leftp;
  if ((l->tag != TADDR) || (l->addrblock.vstg != STGREG))
    return NO;

  r = p->exprblock.rightp;
  if ((r->tag == TADDR) &&
      (r->addrblock.vstg == STGREG) &&
      (r->addrblock.memno == l->addrblock.memno))
    return YES;

  if ((r->tag == TEXPR) && 
      (r->exprblock.opcode == OPADDR) &&
      (r->exprblock.leftp->tag == TADDR) && 
      (r->exprblock.leftp->addrblock.vstg == STGPREG) &&
      (r->exprblock.leftp->addrblock.memno == l->addrblock.memno))
      return YES;

  return NO;
}



LOCAL deleteslot(sp)
Slotp sp;

{
  if (newcode == sp)
    {
      newcode = sp->next;
      if (newcode == dohead)
	newcode = NULL;
    }

  delslot (sp);
  return;
}



LOCAL gensetall(sp)
Slotp sp;

{
  register int i;
  register REGDATA *rp;
  register Addrp ap;

  for (i = toplcv + 1; i <= topregvar; i++)
    if (rp = regtab[i])
      if (rp->isset && !(rp->istemp || rp->isarrayarg))
	if (!memdefined[i])
	  {
	    ap = (Addrp) cpexpr(rp->stgp);
	    changetoreg(ap, i);
	    insertassign(sp, cpexpr(rp->stgp), ap);
	    memdefined[i] = YES;
	  }

  return;
}


LOCAL gensetcommon(sp)
Slotp sp;

{
  register int i;
  register REGDATA *rp;
  register Addrp ap;

  for (i = toplcv + 1; i <= topregvar; i++)
    if (rp = regtab[i])
      if ((rp->vstg == STGCOMMON) && !rp->isarrayarg)
	if (!memdefined[i])
	  {
	    ap = (Addrp) cpexpr(rp->stgp);
	    changetoreg(ap, i);
	    insertassign(sp, cpexpr(rp->stgp), ap);
	    memdefined[i] = YES;
	  }

  return;
}


LOCAL gensetreturn(sp)
Slotp sp;

{
  register int i;
  register REGDATA *rp;
  register Addrp ap;

  for (i = toplcv + 1; i <= topregvar; i++)
    if (rp = regtab[i])
      if (((rp->vstg == STGCOMMON) && !rp->isarrayarg)
      || (rp->isset && (saveall || rp->stgp->issaved) && !(rp->istemp || rp->isarrayarg)))
	if (!memdefined[i])
	  {
	    ap = (Addrp) cpexpr(rp->stgp);
	    changetoreg(ap, i);
	    insertassign(sp, cpexpr(rp->stgp), ap);
	    memdefined[i] = YES;
	  }

  return;
}



LOCAL clearmems()

{
  REGDATA *rp;
  register int i;

  for (i = 0; i <= toplcv; i++)
    memdefined[i] = YES;
  for (; i <= topregvar; i++)
    if ((rp = regtab[i]) && rp->isset)
      memdefined[i] = NO;
    else
      memdefined[i] = YES;
  return;
}


LOCAL setregs()

{
  register int i;

  for (i = 0; i <= topregvar; i++)
    regdefined[i] = YES;
  return;
}



regalloc()

{
int	match;
Slotp	nextslot;
Slotp	sl1,sl2;
Slotp	lastlabslot;

if (! optimflag) return;

docount = 0;
lastlabslot = NULL;
for (sl1 = firstslot; sl1; sl1 = nextslot)
	{
	nextslot = sl1->next;
	switch (sl1->type)
	    {

/* temporarily commented out -----
	    case SKLABEL:
		lastlabslot = sl1;
		break;

	    case SKGOTO:
		if (lastlabslot && sl1->label == lastlabslot->label)
			{
			dohead = lastlabslot;
			doend = sl1;
			alreg ();
			}
		break;
----- */

	    case SKDOHEAD:
		++docount;
		pushq (sl1);
		break;

	    case SKENDDO:
		--docount;
		match = 0;
		for (sl2 = sl1; sl2; sl2 = sl2->prev)
			{
			if (sl2->type == SKDOHEAD) match++;
			else if (sl2->type == SKENDDO) match--;
			if (match == 0) break;
			}
		if (sl2)
			dohead = sl2;
		else
			fatal ("unmatched enddo in code buffer");
		if (sl2->type != SKDOHEAD)
			fatal ("internal error in regalloc");

		for (dqptr = dqbottom; dqptr; dqptr = dqptr->up)
			{
			if (dqptr->dohead == dohead)
				break;
			}

		if (!dqptr)
			fatal ("garbled doqueue in regalloc");

		/*  sl1 now points to the SKENDDO slot; the SKNULL slot
		 *  is reached through sl1->nullslot
		 */
		dqptr->doend = (Slotp) sl1->nullslot;
		if (docount == 0)
			{
			for (dqptr = dqbottom; dqptr; dqptr = dqptr->up)
				{
				dohead = dqptr->dohead;
				doend = dqptr->doend;
				alreg();
				}
			while (dqtop)
				popq(dqtop->dohead);
			docount = 0;
			}
		break;

	    default:
		break;
	    }
	}

return;
}



LOCAL pushq(sp)
Slotp sp;

{
  DOQUEUE *t;

  if (sp->type != SKDOHEAD)
    fatal("regalloc:pushq:  DO statement expected");

  if (dqbottom)
    {
      t = ALLOC(doqueue);
      t->up = dqbottom;
      dqbottom->down = t;
      dqbottom = t;
    }
  else
    dqtop = dqbottom = ALLOC(doqueue);

  dqbottom->dohead = sp;
}


LOCAL popq(sp)
Slotp sp;

{
  DOQUEUE *t;
  register int i;

  if (!dqtop)
    fatal("regalloc:popq:  empty DO queue");
  if (dqtop->dohead != sp)
    fatal("regalloc:popq:  garbled DO queue");

  t = dqtop;
  
  dqtop = t->down;
  if (dqtop)
    dqtop->up = NULL;
  else
    dqbottom = NULL;
  for (i = 0; i < MAXREGVAR; i++)
    if (t->reg[i])
      free((char *) t->reg[i]);
  free(t);
}
