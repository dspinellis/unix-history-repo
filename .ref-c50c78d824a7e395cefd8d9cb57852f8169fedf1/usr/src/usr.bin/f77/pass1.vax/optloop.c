/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)optloop.c	5.2 (Berkeley) %G%";
#endif not lint

/*
 * optloop.c
 *
 * Loop optimizations, f77 compiler pass 1, 4.2 BSD.
 *
 * University of Utah CS Dept. modification history:
 *
 * $Log:	optloop.c,v $
 * Revision 1.4  84/10/25  01:27:29  donn
 * Fixed a subtle bug in removesafe().  When the moved code is an assignment
 * into a temporary, we use the lhs to substitute for the expression inside
 * the loop.  Previously the data structure for the temporary was not copied,
 * so later on when the lhs was freed, the substitute was too, turning it
 * into garbage.
 * 
 * Revision 1.3  84/08/05  17:04:03  donn
 * Changed worthcost() so that it skips variable length strings -- we can't
 * make temporaries for these...
 * 
 * Revision 1.2  84/07/19  11:50:39  donn
 * Installed changes to force non-intrinsic subroutines and functions to define
 * their arguments (make them ineligible for optimization), function setsuses.
 * Fix from A.F.
 * 
 */

#include "defs.h"
#include "optim.h"


#define SCFREE   0
#define SCSAFE   1



typedef
  struct varblock
    {
      struct varblock *next;
      field vstg;
      int memno;	/* holds memalloc for TTEMP */
      short sets;
      short uses;
      field setfirst;
    } VARBLOCK;

typedef VARBLOCK *Varp;

#define TABLESIZE 59

LOCAL Varp table[TABLESIZE];



LOCAL Varp mkbucket(vstg,memno)
field vstg;
int memno;

{
  Varp q;

  q = ALLOC(varblock);
  q->vstg = vstg;
  q->memno = memno;
  return q;
}



LOCAL Varp lookup(p)
tagptr p;

{
int vstg, memno;
int key;
Varp q, r;

switch (p->tag)
	{
	case TTEMP:
		vstg = 0;
		memno = (int) p->tempblock.memalloc;
		break;

	case TADDR:
		vstg = p->addrblock.vstg;
		memno = p->addrblock.memno;
		break;

	default:
		badtag ("lookup",p->tag);
	}
key = memno % TABLESIZE;
q = table[key];

if (q)
	{
	for (; q; r = q, q = q->next)
		if ((q->vstg == vstg) && (q->memno == memno))
			return q;
	return r->next = mkbucket(vstg,memno);
	}
else
	return table[key] = mkbucket(vstg,memno);
}



LOCAL freetable()

{
  int i;
  Varp p, q;

  for (i = 0; i < TABLESIZE; i++)
    if (table[i])
      {
	p = table[i];
	table[i] = NULL;

	while (p)
	  {
	    q = p->next;
	    free((char *) p);
	    p = q;
	  }
      }
}



Slotp newcode;
Slotp dohead, doend;
LOCAL Slotp first, last;
LOCAL commonset;
LOCAL int comocount;	/* count of number of code motions done */


optloops()

{
int	match;
Slotp	nextslot;
Slotp	sl1,sl2;
Slotp	lastlabslot;
int	lab;

if (! optimflag) return;
if (debugflag[6]) return;

lastlabslot = NULL;
comocount = 0;
for (sl1 = firstslot; sl1; sl1 = nextslot)
	{
	nextslot = sl1->next;
	switch (sl1->type)
	    {
	    case SKLABEL:
		lastlabslot = sl1;
		break;

	    case SKGOTO:
		if (lastlabslot && sl1->label == lastlabslot->label)
			{
			lab = newlabel ();
			first = optinsert (SKLABEL,0,lab,0,lastlabslot->next);
			last = sl1;
			last->label = lab;
			optloop ();
			}
		break;

	    case SKDOHEAD:
		match = 0;
		for (sl2 = sl1; sl2; sl2 = sl2->next)
			{
			if (sl2->type == SKDOHEAD) match++;
			else if (sl2->type == SKENDDO) match--;
			if (match == 0) break;
			}
		if (sl2)
			last = sl2;
		else
			fatal ("unmatched do in code buffer");
		if (sl2->type != SKENDDO)
			fatal ("internal error in optloops");

		/*  last now points to the SKENDDO slot; the SKNULL slot
		 *  is reached through last->nullslot
		 */
		last = (Slotp) last->nullslot;

		first = sl1;

		optloop ();
		break;

	    default:
		break;
	    }
	}

if (debugflag[0])
	fprintf (diagfile,"%d code motion%s performed\n",comocount,
		(comocount==1 ? "" : "s") );
return;
}



optloop()

{
newcode = NULL;

modify();

return;
}


LOCAL modify()

{
  Slotp sp;
  int s;

  scanvars();

  for (sp = first; sp != last->next; sp = sp->next)
    switch (sp->type)
      {
      case SKEQ:
	s = anex(sp->expr);
	if (s == SCSAFE)
	  removesafe (&sp->expr);
	break;

      case SKARIF:
      case SKASGOTO:
      case SKCALL:
      case SKCMGOTO:
      case SKIFN:
      case SKSTOP:
      case SKRETURN:
      case SKPAUSE:
      case SKIOIFN:
	s = anex(sp->expr);
	if (s == SCSAFE)
	  removesafe(&sp->expr);
	break;

      default:
	break;
      }

  freetable();
  return;
}


LOCAL scanvars()

{
  Slotp sp;
  Varp varinfo;
  int i;
  Varp p;

  commonset = NO;

  for (sp = first; sp != last->next; sp = sp->next)
    {
      switch (sp->type)
	{
	case SKARIF:
	case SKASGOTO:
	case SKCALL:
	case SKCMGOTO:
	case SKIFN:
	case SKSTOP:
	case SKRETURN:
	case SKPAUSE:
	case SKIOIFN:
	case SKEQ:
	  setsuses(sp->expr);
	  break;

	default:
	  break;
	}
    }

  if (commonset)
    for (i = 0; i < TABLESIZE; i++)
      for (p = table[i]; p; p = p->next)
	if (p->vstg == STGCOMMON)
	  {
	    p->sets++;
	    p->setfirst = NO;
	  }
}


LOCAL setsuses(p)
expptr p;

{
  Addrp lhs;
  Varp varinfo;
  chainp args;

  if (!p) return;

  switch (p->tag)
    {
    case TEXPR:
      switch (p->exprblock.opcode)
	{
	default:
	  setsuses(p->exprblock.leftp);
	  setsuses(p->exprblock.rightp);
	  setsuses(p->exprblock.vleng);
	  break;

	case OPASSIGN:
	  switch (p->exprblock.leftp->tag)
	    {
	    case TTEMP:
	      lhs = (Addrp) p->exprblock.leftp;
	      goto taddr;

	    case TADDR:
	      lhs = (Addrp) p->exprblock.leftp;
	      setsuses(lhs->memoffset);
	      setsuses(lhs->vleng);
	    taddr:
	      setsuses(p->exprblock.rightp);
	      setsuses(p->exprblock.vleng);
	      varinfo = lookup(lhs);
	      varinfo->sets++;
              if (varinfo->uses == 0)
	        varinfo->setfirst = YES;
	      break;

	    default:
	      fatal("O6:  l-value expected");
	    }
	  break;

	case OPSTAREQ:
	case OPPLUSEQ:
	  switch (p->exprblock.leftp->tag)
	    {
	    case TADDR:
	      lhs = (Addrp) p->exprblock.leftp;
	      break;
	    case TTEMP:
	      lhs = (Addrp) p->exprblock.leftp;
	      break;
	    default:
	      fatal("O7:  l-value expected");
	    }
	  setsuses(p->exprblock.leftp);
	  setsuses(p->exprblock.rightp);
	  setsuses(p->exprblock.vleng);
	  varinfo = lookup(lhs);
	  varinfo->sets++;
	  break;

	case OPCALL:
	  if (p->exprblock.leftp->tag != TADDR)
	    fatal("O8:  subprogram expected");
	  setsuses(p->exprblock.rightp);
	  setsuses(p->exprblock.vleng);
	  if (p->exprblock.leftp->addrblock.vstg == STGINTR) break;
	  commonset = YES;
	  if (p->exprblock.rightp == NULL) break;
	  args = p->exprblock.rightp->listblock.listp;
	  for (; args; args = args->nextp)
	    if (args->datap->tag == TADDR)
	      {
		lhs = (Addrp) args->datap;
		switch (lhs->vstg)
		  {
		  case STGARG:
		  case STGAUTO:
		  case STGBSS:
		  case STGINIT:
		  case STGCOMMON:
		  case STGEQUIV:
		  case STGREG:
		  case STGPREG:
		    varinfo = lookup(lhs);
		    varinfo->sets++;
		  }
	      }
	    else if (args->datap->tag == TTEMP)
	      {
		lhs = (Addrp) args->datap;
		varinfo = lookup (lhs);
		varinfo->sets++;
	      }
	  break;
        }

      return;

    case TTEMP:
      varinfo = lookup((Addrp) p);
      varinfo->uses++;
      return;

    case TADDR:
      setsuses(p->addrblock.memoffset);
      setsuses(p->addrblock.vleng);
      varinfo = lookup((Addrp) p);
      varinfo->uses++;
      return;

    case TLIST:
      for (args = p->listblock.listp; args; args = args->nextp)
	setsuses(args->datap);

    case TCONST:
    case TERROR:
      return;

    default:
      fatal("O9:  bad tag value");
    }
}


LOCAL int anex(p)
expptr p;

{
  int s1, s2, s3;
  expptr q;
  Varp varinfo;
  chainp ch;
  int setfirst;
  expptr expr;


  if (p == ENULL)
    return SCSAFE;

  switch (p->tag)
    {
    case TCONST:
      return SCSAFE;

    case TLIST:
      for (ch = p->listblock.listp; ch; ch = ch->nextp)
	{
	  s1 = anex (ch->datap);
	  if (s1 == SCSAFE)
	    removesafe (&ch->datap);
	}
      return SCFREE;

    case TEXPR:
      s1 = anex(p->exprblock.leftp);
      s2 = anex(p->exprblock.rightp);
      s3 = anex(p->exprblock.vleng);

      switch (p->exprblock.opcode)
	{
	case OPASSIGN:
	  expr = p->exprblock.leftp;
	  varinfo = lookup(expr);
	  setfirst = varinfo->setfirst && (varinfo->sets == 1);
	  if (expr->tag == TTEMP && setfirst &&
		s2 == SCSAFE && s3 == SCSAFE)
	    {
	      movefrtemp (expr);
	      return SCSAFE;
	    }
	  else
	    {
	      if (s2 == SCSAFE) removesafe (&p->exprblock.rightp);
	      if (s3 == SCSAFE) removesafe (&p->exprblock.vleng);
	      return SCFREE;
	    }

	case OPNEG:
	case OPNOT:
	case OPABS:
	case OPADDR:
	case OPBITNOT:
	  if ((s2 == SCSAFE) && (s3 == SCSAFE))
	    return s1;
	  else
	    return SCFREE;

	case OPCONV:
	  if ((s2 != SCSAFE) || (s3 != SCSAFE))
	    return SCFREE;

	  if (ISINT(p->exprblock.vtype))
	    return s1;
	  if (ISINT(p->exprblock.leftp->headblock.vtype))
	    return s1;

	  return SCFREE;


	case OPSTAR:
	  if (ISINT(p->exprblock.vtype))
	    goto safeop;

	  if (safefactor(p->exprblock.leftp) ||
	      safefactor(p->exprblock.rightp))
	    goto safeop;

	  goto floatop;


	case OPPLUS:
	case OPMINUS:
	  if (ISINT(p->exprblock.vtype))
	    goto safeop;

	floatop:
	  if (!(ISREAL(p->exprblock.vtype) || ISCOMPLEX(p->exprblock.vtype)))
	    return SCFREE;

	  switch (s1)
	    {
	    case SCSAFE:
	      removesafe(&p->exprblock.leftp);
	      if (s2 == SCSAFE)
		removesafe(&p->exprblock.leftp);
	      return SCFREE;

	    case SCFREE:
	      if (s2 == SCSAFE)
		removesafe(&p->exprblock.rightp);
	      return SCFREE;
	    }

	case OPOR:
	case OPAND:
	case OPEQV:
	case OPNEQV:
	case OPLT:
	case OPEQ:
	case OPGT:
	case OPLE:
	case OPNE:
	case OPGE:
	case OPLSHIFT:
	case OPMIN:
	case OPMAX:
	case OPBITOR:
	case OPBITAND:
	case OPBITXOR:
	case OPRSHIFT:
	safeop:
	  if ((p->exprblock.vleng != ENULL) && ( ! ISCONST(p->exprblock.vleng)))
	    return SCFREE;

	  switch (s1)
	    {
	    case SCSAFE:
		if (s2 == SCFREE) removesafe (&p->exprblock.leftp);
		return s2;

	    case SCFREE:
		if (s2 == SCSAFE) removesafe (&p->exprblock.rightp);
		return SCFREE;
	    }

	default:
	  if (s1 == SCSAFE) removesafe(&p->exprblock.leftp);
	  if (s2 == SCSAFE) removesafe(&p->exprblock.rightp);
	  if (s3 == SCSAFE) removesafe(&p->exprblock.vleng);
	  return SCFREE;
	}


    case TTEMP:
      varinfo = lookup(p);
      if (varinfo->sets == 0)
	return SCSAFE;
      else
	return SCFREE;

    case TADDR:
      s1 = anex(p->addrblock.memoffset);
      s2 = anex(p->addrblock.vleng);

      varinfo = lookup(p);

      if (varinfo->sets == 0)
	switch (s1)
	  {
	  case SCSAFE:
		if (s2 == SCFREE) removesafe(&p->addrblock.memoffset);
		return s2;

	  case SCFREE:
		if (s2 == SCSAFE) removesafe(&p->addrblock.vleng);
		return SCFREE;
	  }

      if (s1 == SCSAFE) removesafe(&p->addrblock.memoffset);
      if (s2 == SCSAFE) removesafe(&p->addrblock.vleng);
      return SCFREE;
    
	
    default:
      return SCFREE;
    }
}


LOCAL safefactor(p)
expptr p;

{
  if ( ! ISCONST(p))
    return NO;

  if (ISINT(p->constblock.vtype))
    if (abs(p->constblock.constant.ci) <= 1)
      return YES;

  if (ISREAL(p->constblock.vtype))
    if (abs(p->constblock.constant.cd[0]) <= 1.0)
      return YES;

  return NO;
}


LOCAL int worthcost(p)
expptr p;

{
  int cost;
  chainp q;
  expptr memoffset,vleng;

  if (p == ENULL)
    return NO;

  switch (p->tag)
    {
    case TCONST:
      return NO;

    case TTEMP:
      return NO;

    case TADDR:
      if ((vleng = p->addrblock.vleng) && ! ISCONST(vleng))
	return NO;	/* Can't make variable length temporaries */
      if ((memoffset = p->addrblock.memoffset) && ! ISCONST(memoffset))
	return YES;
      else
	return NO;

    case TEXPR:
      return YES;

    case TLIST:
      cost = 0;
      for (q = p->listblock.listp; q; q = q->nextp)
	{
	if (worthcost ((expptr) q->datap))
	  return YES;
	cost++;
	}
      return (cost>2 ? YES : NO);

    default:
      return NO;
    }
}


LOCAL removesafe(refexpr)
expptr *refexpr;

{
  expptr ep;
  Tempp ap;
  Slotp newslot;

  extern Addrp gettemp();

  ep = *refexpr;
  if (! worthcost(ep))
    return;

  if (ep->tag == TEXPR && ep->exprblock.opcode == OPASSIGN)
    {
      if (ep->exprblock.leftp->tag != TTEMP)
	fatal ("non-TEMP in assignment to be moved in optloop");

      newslot = optinsert (SKEQ, ep, 0, 0, first);
      *refexpr = (expptr) cpexpr (ep->exprblock.leftp);
    }
  else
    {
      ap = (Tempp) gettemp(ep);
      newslot = optinsert (SKEQ, mkexpr(OPASSIGN,cpexpr(ap),ep), 0, 0, first);
      *refexpr = (expptr) ap;
      optinsert (SKFRTEMP,ap->memalloc,0,0,last->next);
    }

  comocount++;
  if (!newcode)
    newcode = newslot;

  return;
}


LOCAL Addrp gettemp(p)
expptr p;

{
  return mktemp(p->headblock.vtype, p->headblock.vleng);
}



LOCAL movefrtemp (expr)
Tempp	expr;

{
  Slotp	s;

  if (expr->tag != TTEMP)
    badtag ("movefrtemp",expr->tag);

  for (s = first; s; s = s->next)
    if (s->type == SKFRTEMP && s->expr == (expptr) expr->memalloc)
      {
	removeslot (s);
	insertslot (s,last->next);
	return;
      }
}
