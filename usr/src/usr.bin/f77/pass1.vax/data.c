/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)data.c	5.2 (Berkeley) %G%";
#endif not lint

/*
 * data.c
 *
 * Routines for handling DATA statements, f77 compiler, 4.2 BSD.
 *
 * University of Utah CS Dept modification history:
 *
 * Revision 3.1  84/10/13  01:09:50  donn
 * Installed Jerry Berkman's version; added UofU comment header.
 * 
 */

#include "defs.h"
#include "data.h"


/*  global variables  */

flag overlapflag;



/*  local variables  */

LOCAL char rstatus;
LOCAL ftnint rvalue;
LOCAL dovars *dvlist;
LOCAL int dataerror;
LOCAL vallist *grvals;
LOCAL int datafile;
LOCAL int chkfile;
LOCAL long base;



/*  Copied from expr.c  */

LOCAL letter(c)
register int c;
{
if( isupper(c) )
	c = tolower(c);
return(c - 'a');
}



vexpr *
cpdvalue(dp)
vexpr *dp;
{
  register dvalue *p;

  if (dp->tag != DVALUE)
    badtag("cpdvalue", dp->tag);

  p = ALLOC(Dvalue);
  p->tag = DVALUE;
  p->status = dp->dvalue.status;
  p->value = dp->dvalue.value;

  return ((vexpr *) p);
}



frvexpr(vp)
register vexpr *vp;
{
  if (vp != NULL)
    {
      if (vp->tag == DNAME)
	free(vp->dname.repr);
      else if (vp->tag == DEXPR)
	{
	  frvexpr(vp->dexpr.left);
	  frvexpr(vp->dexpr.right);
	}

      free((char *) vp);
    }

  return;
}



frvlist(vp)
register vlist *vp;
{
  register vlist *t;

  while (vp)
    {
      t = vp->next;
      frvexpr(vp->val);
      free((char *) vp);
      vp = t;
    }

  return;
}



frelist(ep)
elist *ep;
{
  register elist *p;
  register elist *t;
  register aelt *ap;
  register dolist *dp;

  p = ep;

  while (p != NULL)
    {
      if (p->elt->tag == SIMPLE)
	{
	  ap = (aelt *) p->elt;
	  frvlist(ap->subs);
	  if (ap->range != NULL)
	    {
	      frvexpr(ap->range->low);
	      frvexpr(ap->range->high);
	      free((char *) ap->range);
	    }
	  free((char *) ap);
	}
      else
	{
	  dp = (dolist *) p->elt;
	  frvexpr(dp->dovar);
	  frvexpr(dp->init);
	  frvexpr(dp->limit);
	  frvexpr(dp->step);
	  frelist(dp->elts);
	  free((char *) dp);
	}

      t = p;
      p = p->next;
      free((char *) t);
    }

  return;
}



frvallist(vp)
vallist *vp;
{
  register vallist *p;
  register vallist *t;

  p = vp;
  while (p != NULL)
    {
      frexpr((tagptr) p->value);
      t = p;
      p = p->next;
      free((char *) t);
    }

  return;
}



elist *revelist(ep)
register elist *ep;
{
  register elist *next;
  register elist *t;

  if (ep != NULL)
    {
      next = ep->next;
      ep->next = NULL;

      while (next)
	{
	  t = next->next;
	  next->next = ep;
	  ep = next;
	  next = t;
	}
    }

  return (ep);
}



vlist *revvlist(vp)
vlist *vp;
{
  register vlist *p;
  register vlist *next;
  register vlist *t;

  if (vp == NULL)
    p = NULL;
  else
    {
      p = vp;
      next = p->next;
      p->next = NULL;

      while (next)
	{
	  t = next->next;
	  next->next = p;
	  p = next;
	  next = t;
	}
    }

  return (p);
}



vallist *
revrvals(vp)
vallist *vp;
{
  register vallist *p;
  register vallist *next;
  register vallist *t;

  if (vp == NULL)
    p = NULL;
  else
    {
      p = vp;
      next = p->next;
      p->next = NULL;
      while (next)
	{
	  t = next->next;
	  next->next = p;
	  p = next;
	  next = t;
	}
    }

  return (p);
}



vlist *prepvexpr(tail, head)
vlist *tail;
vexpr *head;
{
  register vlist *p;

  p = ALLOC(Vlist);
  p->next = tail;
  p->val = head;

  return (p);
}



elist *preplval(tail, head)
elist *tail;
delt* head;
{
  register elist *p;
  p = ALLOC(Elist);
  p->next = tail;
  p->elt = head;

  return (p);
}



delt *mkdlval(name, subs, range)
vexpr *name;
vlist *subs;
rpair *range;
{
  register aelt *p;

  p = ALLOC(Aelt);
  p->tag = SIMPLE;
  p->var = mkname(name->dname.len, name->dname.repr);
  p->subs = subs;
  p->range = range;

  return ((delt *) p);
}



delt *mkdatado(lvals, dovar, params)
elist *lvals;
vexpr *dovar;
vlist *params;
{
  static char *toofew = "missing loop parameters";
  static char *toomany = "too many loop parameters";

  register dolist *p;
  register vlist *vp;
  register int pcnt;
  register dvalue *one;

  p = ALLOC(DoList);
  p->tag = NESTED;
  p->elts = revelist(lvals);
  p->dovar = dovar;

  vp = params;
  pcnt = 0;
  while (vp)
    {
      pcnt++;
      vp = vp->next;
    }

  if (pcnt != 2 && pcnt != 3)
    {
      if (pcnt < 2)
	err(toofew);
      else
	err(toomany);

      p->init = (vexpr *) ALLOC(Derror);
      p->init->tag = DERROR;

      p->limit = (vexpr *) ALLOC(Derror);
      p->limit->tag = DERROR;

      p->step = (vexpr *) ALLOC(Derror);
      p->step->tag = DERROR;
    }
  else
    {
      vp = params;

      if (pcnt == 2)
	{
	  one = ALLOC(Dvalue);
	  one->tag = DVALUE;
	  one->status = NORMAL;
	  one->value = 1;
	  p->step = (vexpr *) one;
	}
      else
	{
	  p->step = vp->val;
	  vp->val = NULL;
	  vp = vp->next;
	}

      p->limit = vp->val;
      vp->val = NULL;
      vp = vp->next;

      p->init = vp->val;
      vp->val = NULL;
    }

  frvlist(params);
  return ((delt *) p);
}



rpair *mkdrange(lb, ub)
vexpr *lb, *ub;
{
  register rpair *p;

  p = ALLOC(Rpair);
  p->low = lb;
  p->high = ub;

  return (p);
}



vallist *mkdrval(repl, val)
vexpr *repl;
expptr val;
{
  static char *badtag = "bad tag in mkdrval";
  static char *negrepl = "negative replicator";
  static char *zerorepl = "zero replicator";
  static char *toobig = "replicator too large";
  static char *nonconst = "%s is not a constant";

  register vexpr *vp;
  register vallist *p;
  register int status;
  register ftnint value;
  register int copied;

  copied = 0;

  if (repl->tag == DNAME)
    {
      vp = evaldname(repl);
      copied = 1;
    }
  else
    vp = repl;

  p = ALLOC(ValList);
  p->next = NULL;
  p->value = (Constp) val;

  if (vp->tag == DVALUE)
    {
      status = vp->dvalue.status;
      value = vp->dvalue.value;

      if ((status == NORMAL && value < 0) || status == MINLESS1)
	{
	  err(negrepl);
	  p->status = ERRVAL;
	}
      else if (status == NORMAL)
	{
	  if (value == 0)
	    warn(zerorepl);
	  p->status = NORMAL;
	  p->repl = value;
	}
      else if (status == MAXPLUS1)
	{
	  err(toobig);
	  p->status = ERRVAL;
	}
      else
	p->status = ERRVAL;
    }
  else if (vp->tag == DNAME)
    {
      errnm(nonconst, vp->dname.len, vp->dname.repr);
      p->status = ERRVAL;
    }
  else if (vp->tag == DERROR)
    p->status = ERRVAL;
  else
    fatal(badtag);

  if (copied) frvexpr(vp);
  return (p);
}



/*  Evicon returns the value of the integer constant  */
/*  pointed to by token.                              */

vexpr *evicon(len, token)
register int len;
register char *token;
{
  static char *badconst = "bad integer constant";
  static char *overflow = "integer constant too large";

  register int i;
  register ftnint val;
  register int digit;
  register dvalue *p;

  if (len <= 0)
    fatal(badconst);

  p = ALLOC(Dvalue);
  p->tag = DVALUE;

  i = 0;
  val = 0;
  while (i < len)
    {
      if (val > MAXINT/10)
	{
	  err(overflow);
	  p->status = ERRVAL;
	  goto ret;
	}
      val = 10*val;
      digit = token[i++];
      if (!isdigit(digit))
	fatal(badconst);
      digit = digit - '0';
      if (MAXINT - val >= digit)
	val = val + digit;
      else
	if (i == len && MAXINT - val + 1 == digit)
	  {
	    p->status = MAXPLUS1;
	    goto ret;
	  }
	else
	  {
	    err(overflow);
	    p->status = ERRVAL;
	    goto ret;
	  }
    }

  p->status = NORMAL;
  p->value = val;

ret:
  return ((vexpr *) p);
}



/*  Ivaltoicon converts a dvalue into a constant block.  */

expptr ivaltoicon(vp)
register vexpr *vp;
{
  static char *badtag = "bad tag in ivaltoicon";
  static char *overflow = "integer constant too large";

  register int vs;
  register expptr p;

  if (vp->tag == DERROR)
    return(errnode());
  else if (vp->tag != DVALUE)
    fatal(badtag);

  vs = vp->dvalue.status;
  if (vs == NORMAL)
    p = mkintcon(vp->dvalue.value);
  else if ((MAXINT + MININT == -1) && vs == MINLESS1)
    p = mkintcon(MININT);
  else if (vs == MAXPLUS1 || vs == MINLESS1)
    {
      err(overflow);
      p = errnode();
    }
  else
    p = errnode();

  return (p);
}



/*  Mkdname stores an identifier as a dname  */

vexpr *mkdname(len, str)
int len;
register char *str;
{
  register dname *p;
  register int i;
  register char *s;

  s = (char *) ckalloc(len + 1);
  i = len;
  s[i] = '\0';

  while (--i >= 0)
    s[i] = str[i];

  p = ALLOC(Dname);
  p->tag = DNAME;
  p->len = len;
  p->repr = s;

  return ((vexpr *) p);
}



/*  Getname gets the symbol table information associated with  */
/*  a name.  Getname differs from mkname in that it will not   */
/*  add the name to the symbol table if it is not already      */
/*  present.                                                   */

Namep getname(l, s)
int l;
register char *s;
{
  struct Hashentry *hp;
  int hash;
  register Namep q;
  register int i;
  char n[VL];

  hash = 0;
  for (i = 0; i < l && *s != '\0'; ++i)
    {
      hash += *s;
      n[i] = *s++;
    }

  while (i < VL)
    n[i++] = ' ';

  hash %= maxhash;
  hp = hashtab + hash;

  while (q = hp->varp)
    if (hash == hp->hashval
	&& eqn(VL, n, q->varname))
      goto ret;
    else if (++hp >= lasthash)
      hp = hashtab;

ret:
  return (q);
}



/*  Evparam returns the value of the constant named by name.  */

expptr evparam(np)
register vexpr *np;
{
  static char *badtag = "bad tag in evparam";
  static char *undefined = "%s is undefined";
  static char *nonconst = "%s is not constant";

  register struct Paramblock *tp;
  register expptr p;
  register int len;
  register char *repr;

  if (np->tag != DNAME)
    fatal(badtag);

  len = np->dname.len;
  repr = np->dname.repr;

  tp = (struct Paramblock *) getname(len, repr);

  if (tp == NULL)
    {
      errnm(undefined, len, repr);
      p = errnode();
    }
  else if (tp->vclass != CLPARAM || !ISCONST(tp->paramval))
    {
      if (tp->paramval->tag != TERROR)
        errnm(nonconst, len, repr);
      p = errnode();
    }
  else
    p = (expptr) cpexpr(tp->paramval);

  return (p);
}



vexpr *evaldname(dp)
vexpr *dp;
{
  static char *undefined = "%s is undefined";
  static char *nonconst = "%s is not a constant";
  static char *nonint = "%s is not an integer";

  register dvalue *p;
  register struct Paramblock *tp;
  register int len;
  register char *repr;

  p = ALLOC(Dvalue);
  p->tag = DVALUE;

  len = dp->dname.len;
  repr = dp->dname.repr;

  tp = (struct Paramblock *) getname(len, repr);

  if (tp == NULL)
    {
      errnm(undefined, len, repr);
      p->status = ERRVAL;
    }
  else if (tp->vclass != CLPARAM || !ISCONST(tp->paramval))
    {
      if (tp->paramval->tag != TERROR)
        errnm(nonconst, len, repr);
      p->status = ERRVAL;
    }
  else if (!ISINT(tp->paramval->constblock.vtype))
    {
      errnm(nonint, len, repr);
      p->status = ERRVAL;
    }
  else
    {
      if ((MAXINT + MININT == -1)
	  && tp->paramval->constblock.constant.ci == MININT)
	p->status = MINLESS1;
      else
	{
	  p->status = NORMAL;
          p->value = tp->paramval->constblock.constant.ci;
	}
    }

  return ((vexpr *) p);
}



vexpr *mkdexpr(op, l, r)
register int op;
register vexpr *l;
register vexpr *r;
{
  static char *badop = "bad operator in mkdexpr";

  register vexpr *p;

  switch (op)
    {
    default:
      fatal(badop);

    case OPNEG:
    case OPPLUS:
    case OPMINUS:
    case OPSTAR:
    case OPSLASH:
    case OPPOWER:
      break;
    }

  if ((l != NULL && l->tag == DERROR) || r->tag == DERROR)
    {
      frvexpr(l);
      frvexpr(r);
      p = (vexpr *) ALLOC(Derror);
      p->tag = DERROR;
    }
  else if (op == OPNEG && r->tag == DVALUE)
    {
      p = negival(r);
      frvexpr(r);
    }
  else if (op != OPNEG && l->tag == DVALUE && r->tag == DVALUE)
    {
      switch (op)
	{
	case OPPLUS:
	  p = addivals(l, r);
	  break;

	case OPMINUS:
	  p = subivals(l, r);
	  break;

	case OPSTAR:
	  p = mulivals(l, r);
	  break;

	case OPSLASH:
	  p = divivals(l, r);
	  break;

	case OPPOWER:
	  p = powivals(l, r);
	  break;
	}

      frvexpr(l);
      frvexpr(r);
    }
  else
    {
      p = (vexpr *) ALLOC(Dexpr);
      p->tag = DEXPR;
      p->dexpr.opcode = op;
      p->dexpr.left = l;
      p->dexpr.right = r;
    }

  return (p);
}



vexpr *addivals(l, r)
vexpr *l;
vexpr *r;
{
  static char *badtag = "bad tag in addivals";
  static char *overflow = "integer value too large";

  register int ls, rs;
  register ftnint lv, rv;
  register dvalue *p;
  register ftnint k;

  if (l->tag != DVALUE || r->tag != DVALUE)
    fatal(badtag);

  ls = l->dvalue.status;
  lv = l->dvalue.value;
  rs = r->dvalue.status;
  rv = r->dvalue.value;

  p = ALLOC(Dvalue);
  p->tag = DVALUE;

  if (ls == ERRVAL || rs == ERRVAL)
    p->status = ERRVAL;

  else if (ls == NORMAL && rs == NORMAL)
    {
      addints(lv, rv);
      if (rstatus == ERRVAL)
	err(overflow);
      p->status = rstatus;
      p->value = rvalue;
    }

  else
    {
      if (rs == MAXPLUS1 || rs == MINLESS1)
	{
	  rs = ls;
	  rv = lv;
	  ls = r->dvalue.status;
	}

      if (rs == NORMAL && rv == 0)
	p->status = ls;
      else if (ls == MAXPLUS1)
	{
	  if (rs == NORMAL && rv < 0)
	    {
	      p->status = NORMAL;
	      k = MAXINT + rv;
	      p->value = k + 1;
	    }
	  else if (rs == MINLESS1)
	    {
	      p->status = NORMAL;
	      p->value = 0;
	    }
	  else
	    {
	      err(overflow);
	      p->status = ERRVAL;
	    }
	}
      else
	{
	  if (rs == NORMAL && rv > 0)
	    {
	      p->status = NORMAL;
	      k = ( -MAXINT ) + rv;
	      p->value = k - 1;
	    }
	  else if (rs == MAXPLUS1)
	    {
	      p->status = NORMAL;
	      p->value = 0;
	    }
	  else
	    {
	      err(overflow);
	      p->status = ERRVAL;
	    }
	}
    }

  return ((vexpr *) p);
}



vexpr *negival(vp)
vexpr *vp;
{
  static char *badtag = "bad tag in negival";

  register int vs;
  register dvalue *p;

  if (vp->tag != DVALUE)
    fatal(badtag);

  vs = vp->dvalue.status;

  p = ALLOC(Dvalue);
  p->tag = DVALUE;

  if (vs == ERRVAL)
    p->status = ERRVAL;
  else if (vs == NORMAL)
    {
      p->status = NORMAL;
      p->value = -(vp->dvalue.value);
    }
  else if (vs == MAXPLUS1)
    p->status = MINLESS1;
  else
    p->status = MAXPLUS1;

  return ((vexpr *) p);
}



vexpr *subivals(l, r)
vexpr *l;
vexpr *r;
{
  static char *badtag = "bad tag in subivals";

  register vexpr *p;
  register vexpr *t;

  if (l->tag != DVALUE || r->tag != DVALUE)
    fatal(badtag);

  t = negival(r);
  p = addivals(l, t);
  frvexpr(t);

  return (p);
}



vexpr *mulivals(l, r)
vexpr *l;
vexpr *r;
{
  static char *badtag = "bad tag in mulivals";
  static char *overflow = "integer value too large";

  register int ls, rs;
  register ftnint lv, rv;
  register dvalue *p;

  if (l->tag != DVALUE || r->tag != DVALUE)
    fatal(badtag);

  ls = l->dvalue.status;
  lv = l->dvalue.value;
  rs = r->dvalue.status;
  rv = r->dvalue.value;

  p = ALLOC(Dvalue);
  p->tag = DVALUE;

  if (ls == ERRVAL || rs == ERRVAL)
    p->status = ERRVAL;

  else if (ls == NORMAL && rs == NORMAL)
    {
      mulints(lv, rv);
      if (rstatus == ERRVAL)
	err(overflow);
      p->status = rstatus;
      p->value = rvalue;
    }
  else
    {
      if (rs == MAXPLUS1 || rs == MINLESS1)
	{
	  rs = ls;
	  rv = lv;
	  ls = r->dvalue.status;
	}

      if (rs == NORMAL && rv == 0)
	{
	  p->status = NORMAL;
	  p->value = 0;
	}
      else if (rs == NORMAL && rv == 1)
	p->status = ls;
      else if (rs == NORMAL && rv == -1)
	if (ls == MAXPLUS1)
	  p->status = MINLESS1;
	else
	  p->status = MAXPLUS1;
      else
	{
	  err(overflow);
	  p->status = ERRVAL;
	}
    }

  return ((vexpr *) p);
}



vexpr *divivals(l, r)
vexpr *l;
vexpr *r;
{
  static char *badtag = "bad tag in divivals";
  static char *zerodivide = "division by zero";

  register int ls, rs;
  register ftnint lv, rv;
  register dvalue *p;
  register ftnint k;
  register int sign;

  if (l->tag != DVALUE && r->tag != DVALUE)
    fatal(badtag);

  ls = l->dvalue.status;
  lv = l->dvalue.value;
  rs = r->dvalue.status;
  rv = r->dvalue.value;

  p = ALLOC(Dvalue);
  p->tag = DVALUE;

  if (ls == ERRVAL || rs == ERRVAL)
    p->status = ERRVAL;
  else if (rs == NORMAL)
    {
      if (rv == 0)
	{
	  err(zerodivide);
	  p->status = ERRVAL;
	}
      else if (ls == NORMAL)
	{
	  p->status = NORMAL;
	  p->value = lv / rv;
	}
      else if (rv == 1)
	p->status = ls;
      else if (rv == -1)
	if (ls == MAXPLUS1)
	  p->status = MINLESS1;
	else
	  p->status = MAXPLUS1;
      else
	{
	  p->status = NORMAL;

	  if (ls == MAXPLUS1)
	    sign = 1;
	  else
	    sign = -1;

	  if (rv < 0)
	    {
	      rv = -rv;
	      sign = -sign;
	    }
	
	  k = MAXINT - rv;
	  p->value = sign * ((k + 1)/rv + 1);
	}
    }
  else
    {
      p->status = NORMAL;
      if (ls == NORMAL)
	p->value = 0;
      else if ((ls == MAXPLUS1 && rs == MAXPLUS1)
		|| (ls == MINLESS1 && rs == MINLESS1))
	p->value = 1;
      else
	p->value = -1;
    }

  return ((vexpr *) p);
}



vexpr *powivals(l, r)
vexpr *l;
vexpr *r;
{
  static char *badtag = "bad tag in powivals";
  static char *zerozero = "zero raised to the zero-th power";
  static char *zeroneg = "zero raised to a negative power";
  static char *overflow = "integer value too large";

  register int ls, rs;
  register ftnint lv, rv;
  register dvalue *p;

  if (l->tag != DVALUE || r->tag != DVALUE)
    fatal(badtag);

  ls = l->dvalue.status;
  lv = l->dvalue.value;
  rs = r->dvalue.status;
  rv = r->dvalue.value;

  p = ALLOC(Dvalue);
  p->tag = DVALUE;

  if (ls == ERRVAL || rs == ERRVAL)
    p->status = ERRVAL;

  else if (ls == NORMAL)
    {
      if (lv == 1)
	{
	  p->status = NORMAL;
	  p->value = 1;
	}
      else if (lv == 0)
	{
	  if (rs == MAXPLUS1 || (rs == NORMAL && rv > 0))
	    {
	      p->status = NORMAL;
	      p->value = 0;
	    }
	  else if (rs == NORMAL && rv == 0)
	    {
	      warn(zerozero);
	      p->status = NORMAL;
	      p->value = 1;
	    }
	  else
	    {
	      err(zeroneg);
	      p->status = ERRVAL;
	    }
	}
      else if (lv == -1)
	{
	  p->status = NORMAL;
	  if (rs == NORMAL)
	    {
	      if (rv < 0) rv = -rv;
	      if (rv % 2 == 0)
		p->value = 1;
	      else
		p->value = -1;
	    }
	  else
#	    if (MAXINT % 2 == 1)
	      p->value = 1;
#	    else
	      p->value = -1;
#	    endif
	}
      else
	{
	  if (rs == NORMAL && rv > 0)
	    {
	      rstatus = NORMAL;
	      rvalue = lv;
	      while (--rv && rstatus == NORMAL)
		mulints(rvalue, lv);
	      if (rv == 0 && rstatus != ERRVAL)
		{
		  p->status = rstatus;
		  p->value = rvalue;
		}
	      else
		{
		  err(overflow);
		  p->status = ERRVAL;
		}
	    }
	  else if (rs == MAXPLUS1)
	    {
	      err(overflow);
	      p->status = ERRVAL;
	    }
	  else if (rs == NORMAL && rv == 0)
	    {
	      p->status = NORMAL;
	      p->value = 1;
	    }
	  else
	    {
	      p->status = NORMAL;
	      p->value = 0;
	    }
	}
    }

  else
    {
      if (rs == MAXPLUS1 || (rs == NORMAL && rv > 1))
	{
	  err(overflow);
	  p->status = ERRVAL;
	}
      else if (rs == NORMAL && rv == 1)
	p->status = ls;
      else if (rs == NORMAL && rv == 0)
	{
	  p->status = NORMAL;
	  p->value = 1;
	}
      else
	{
	  p->status = NORMAL;
	  p->value = 0;
	}
    }

  return ((vexpr *) p);
}



/*  Addints adds two integer values.  */

addints(i, j)
register ftnint i, j;
{
  register ftnint margin;

  if (i == 0)
    {
      rstatus = NORMAL;
      rvalue = j;
    }
  else if (i > 0)
    {
      margin = MAXINT - i;
      if (j <= margin)
	{
	  rstatus = NORMAL;
	  rvalue = i + j;
	}
      else if (j == margin + 1)
	rstatus = MAXPLUS1;
      else
	rstatus = ERRVAL;
    }
  else
    {
      margin = ( -MAXINT ) - i;
      if (j >= margin)
	{
	  rstatus = NORMAL;
	  rvalue = i + j;
	}
      else if (j == margin - 1)
	rstatus = MINLESS1;
      else
	rstatus = ERRVAL;
    }

   return;
}



/*  Mulints multiplies two integer values  */

mulints(i, j)
register ftnint i, j;
{
  register ftnint sign;
  register ftnint margin;

  if (i == 0 || j == 0)
    {
      rstatus = NORMAL;
      rvalue = 0;
    }
  else
    {
      if ((i > 0 && j > 0) || (i < 0 && j < 0))
	sign = 1;
      else
	sign = -1;

      if (i < 0) i = -i;
      if (j < 0) j = -j;

      margin = MAXINT - i;
      margin = (margin + 1) / i;

      if (j <= margin)
	{
	  rstatus = NORMAL;
	  rvalue = i * j * sign;
	}
      else if (j - 1 == margin)
	{
	  margin = i*margin - 1;
	  if (margin == MAXINT - i)
	    if (sign > 0)
	      rstatus = MAXPLUS1;
	    else
	      rstatus = MINLESS1;
	  else
	    {
	      rstatus = NORMAL;
	      rvalue = i * j * sign;
	    }
	}
      else
	rstatus = ERRVAL;
    }

  return;
}



vexpr *
evalvexpr(ep)
vexpr *ep;
{
  register vexpr *p;
  register vexpr *l, *r;

  switch (ep->tag)
    {
    case DVALUE:
      p = cpdvalue(ep);
      break;

    case DVAR:
      p = cpdvalue((vexpr *) ep->dvar.valp);
      break;

    case DNAME:
      p = evaldname(ep);
      break;

    case DEXPR:
      if (ep->dexpr.left == NULL)
	l = NULL;
      else
	l = evalvexpr(ep->dexpr.left);

      if (ep->dexpr.right == NULL)
	r = NULL;
      else
	r = evalvexpr(ep->dexpr.right);

      switch (ep->dexpr.opcode)
	{
	case OPNEG:
	  p = negival(r);
	  break;

	case OPPLUS:
	  p = addivals(l, r);
	  break;

	case OPMINUS:
	  p = subivals(l, r);
	  break;

	case OPSTAR:
	  p = mulivals(l, r);
	  break;

	case OPSLASH:
	  p = divivals(l, r);
	  break;

	case OPPOWER:
	  p = powivals(l, r);
	  break;
	}

      frvexpr(l);
      frvexpr(r);
      break;

    case DERROR:
      p = (vexpr *) ALLOC(Dvalue);
      p->tag = DVALUE;
      p->dvalue.status = ERRVAL;
      break;
    }

  return (p);
}



vexpr *
refrigdname(vp)
vexpr *vp;
{
  register vexpr *p;
  register int len;
  register char *repr;
  register int found;
  register dovars *dvp;

  len = vp->dname.len;
  repr = vp->dname.repr;

  found = NO;
  dvp = dvlist;
  while (found == NO && dvp != NULL)
    {
      if (len == dvp->len && eqn(len, repr, dvp->repr))
	found = YES;
      else
	dvp = dvp->next;
    }

  if (found == YES)
    {
      p = (vexpr *) ALLOC(Dvar);
      p->tag = DVAR;
      p->dvar.valp = dvp->valp;
    }
  else
    {
      p = evaldname(vp);
      if (p->dvalue.status == ERRVAL)
	dataerror = YES;
    }

  return (p);
}



refrigvexpr(vpp)
vexpr **vpp;
{
  register vexpr *vp;

  vp = *vpp;

  switch (vp->tag)
    {
    case DVALUE:
    case DVAR:
    case DERROR:
      break;

    case DEXPR:
      refrigvexpr( &(vp->dexpr.left) );
      refrigvexpr( &(vp->dexpr.right) );
      break;

    case DNAME:
      *(vpp) = refrigdname(vp);
      frvexpr(vp);
      break;
    }

  return;
}



int
chkvar(np, sname)
Namep np;
char *sname;
{
  static char *nonvar = "%s is not a variable";
  static char *arginit = "attempt to initialize a dummy argument: %s";
  static char *autoinit = "attempt to initialize an automatic variable: %s";
  static char *badclass = "bad class in chkvar";

  register int status;
  register struct Dimblock *dp;
  register int i;

  status = YES;

  if (np->vclass == CLUNKNOWN
      || (np->vclass == CLVAR && !np->vdcldone))
    vardcl(np);

  if (np->vstg == STGARG)
    {
      errstr(arginit, sname);
      dataerror = YES;
      status = NO;
    }
  else if (np->vclass != CLVAR)
    {
      errstr(nonvar, sname);
      dataerror = YES;
      status = NO;
    }
  else if (np->vstg == STGAUTO)
    {
      errstr(autoinit, sname);
      dataerror = YES;
      status = NO;
    }
  else if (np->vstg != STGBSS && np->vstg != STGINIT
	    && np->vstg != STGCOMMON && np->vstg != STGEQUIV)
    {
      fatal(badclass);
    }
  else
    {
      switch (np->vtype)
	{
	case TYERROR:
	  status = NO;
	  dataerror = YES;
	  break;

	case TYSHORT:
	case TYLONG:
	case TYREAL:
	case TYDREAL:
	case TYCOMPLEX:
	case TYDCOMPLEX:
	case TYLOGICAL:
	case TYCHAR:
	  dp = np->vdim;
	  if (dp != NULL)
	    {
	      if (dp->nelt == NULL || !ISICON(dp->nelt))
	        {
	          status = NO;
	          dataerror = YES;
	        }
	    }
	  break;

	default:
	  badtype("chkvar", np->vtype);
	}
    }

  return (status);
}



refrigsubs(ap, sname)
aelt *ap;
char *sname;
{
  static char *nonarray = "subscripts on a simple variable:  %s";
  static char *toofew = "not enough subscripts on %s";
  static char *toomany = "too many subscripts on %s";

  register vlist *subp;
  register int nsubs;
  register Namep np;
  register struct Dimblock *dp;
  register int i;

  np = ap->var;
  dp = np->vdim;

  if (ap->subs != NULL)
    {
      if (np->vdim == NULL)
	{
	  errstr(nonarray, sname);
	  dataerror = YES;
	}
      else
	{
	  nsubs = 0;
	  subp = ap->subs;
	  while (subp != NULL)
	    {
	      nsubs++;
	      refrigvexpr( &(subp->val) );
	      subp = subp->next;
	    }

	  if (dp->ndim != nsubs)
	    {
	      if (np->vdim->ndim > nsubs)
		errstr(toofew, sname);
	      else
		errstr(toomany, sname);
	      dataerror = YES;
	    }
	  else if (dp->baseoffset == NULL || !ISICON(dp->baseoffset))
	    dataerror = YES;
	  else
	    {
	      i = dp->ndim;
	      while (i-- > 0)
		{
		  if (dp->dims[i].dimsize == NULL
		      || !ISICON(dp->dims[i].dimsize))
		    dataerror = YES;
		}
	    }
	}
    }

  return;
}



refrigrange(ap, sname)
aelt *ap;
char *sname;
{
  static char *nonstr = "substring of a noncharacter variable:  %s";
  static char *array = "substring applied to an array:  %s";

  register Namep np;
  register dvalue *t;
  register rpair *rp;

  if (ap->range != NULL)
    {
      np = ap->var;
      if (np->vtype != TYCHAR)
	{
	  errstr(nonstr, sname);
	  dataerror = YES;
	}
      else if (ap->subs == NULL && np->vdim != NULL)
	{
	  errstr(array, sname);
	  dataerror = YES;
	}
      else
	{
	  rp = ap->range;

	  if (rp->low != NULL)
	    refrigvexpr( &(rp->low) );
	  else
	    {
	      t = ALLOC(Dvalue);
	      t->tag = DVALUE;
	      t->status = NORMAL;
	      t->value = 1;
	      rp->low = (vexpr *) t;
	    }

	  if (rp->high != NULL)
	    refrigvexpr( &(rp->high) );
	  else
	    {
	      if (!ISICON(np->vleng))
		{
		  rp->high = (vexpr *) ALLOC(Derror);
		  rp->high->tag = DERROR;
		}
	      else
		{
		  t = ALLOC(Dvalue);
		  t->tag = DVALUE;
		  t->status = NORMAL;
		  t->value = np->vleng->constblock.constant.ci;
		  rp->high = (vexpr *) t;
		}
	    }
	}
    }

  return;
}



refrigaelt(ap)
aelt *ap;
{
  register Namep np;
  register char *bp, *sp;
  register int len;
  char buff[VL+1];

  np = ap->var;

  len = 0;
  bp = buff;
  sp = np->varname;
  while (len < VL && *sp != ' ' && *sp != '\0')
    {
      *bp++ = *sp++;
      len++;
    }
  *bp = '\0';

  if (chkvar(np, buff))
    {
      refrigsubs(ap, buff);
      refrigrange(ap, buff);
    }

  return;
}



refrigdo(dp)
dolist *dp;
{
  static char *duplicates = "implied DO variable %s redefined";
  static char *nonvar = "%s is not a variable";
  static char *nonint = "%s is not integer";

  register int len;
  register char *repr;
  register int found;
  register dovars *dvp;
  register Namep np;
  register dovars *t;

  refrigvexpr( &(dp->init) );
  refrigvexpr( &(dp->limit) );
  refrigvexpr( &(dp->step) );

  len = dp->dovar->dname.len;
  repr = dp->dovar->dname.repr;

  found = NO;
  dvp = dvlist;
  while (found == NO && dvp != NULL)
    if (len == dvp->len && eqn(len, repr, dvp->repr))
      found = YES;
    else
      dvp = dvp->next;

  if (found == YES)
    {
      errnm(duplicates, len, repr);
      dataerror = YES;
    }
  else
    {
      np = getname(len, repr);
      if (np == NULL)
	{
	  if (!ISINT(impltype[letter(*repr)]))
	    warnnm(nonint, len, repr);
	}
      else
	{
	  if (np->vclass == CLUNKNOWN)
	    vardcl(np);
	  if (np->vclass != CLVAR)
	    warnnm(nonvar, len, repr);
	  else if (!ISINT(np->vtype))
	    warnnm(nonint, len, repr);
	}
    }

  t = ALLOC(DoVars);
  t->next = dvlist;
  t->len = len;
  t->repr = repr;
  t->valp = ALLOC(Dvalue);
  t->valp->tag = DVALUE;
  dp->dovar = (vexpr *) t->valp;

  dvlist = t;

  refriglvals(dp->elts);

  dvlist = t->next;
  free((char *) t);

  return;
}



refriglvals(lvals)
elist *lvals;
{
  register elist *top;

  top = lvals;

  while (top != NULL)
    {
      if (top->elt->tag == SIMPLE)
	refrigaelt((aelt *) top->elt);
      else
	refrigdo((dolist *) top->elt);

      top = top->next;
    }

  return;
}



/*  Refrig freezes name/value bindings in the DATA name list  */


refrig(lvals)
elist *lvals;
{
  dvlist = NULL;
  refriglvals(lvals);
  return;
}



ftnint
indexer(ap)
aelt *ap;
{
  static char *badvar = "bad variable in indexer";
  static char *boundserror = "subscript out of bounds";

  register ftnint index;
  register vlist *sp;
  register Namep np;
  register struct Dimblock *dp;
  register int i;
  register dvalue *vp;
  register ftnint size;
  ftnint sub[MAXDIM];

  sp = ap->subs;
  if (sp == NULL) return (0);

  np = ap->var;
  dp = np->vdim;

  if (dp == NULL)
    fatal(badvar);

  i = 0;
  while (sp != NULL)
    {
      vp = (dvalue *) evalvexpr(sp->val);

      if (vp->status == NORMAL)
	sub[i++] = vp->value;
      else if ((MININT + MAXINT == -1) && vp->status == MINLESS1)
	sub[i++] = MININT;
      else
	{
	  frvexpr((vexpr *) vp);
	  return (-1);
	}

      frvexpr((vexpr *) vp);
      sp = sp->next;
    }

  index = sub[--i];
  while (i-- > 0)
    {
      size = dp->dims[i].dimsize->constblock.constant.ci;
      index = sub[i] + index * size;
    }

  index -= dp->baseoffset->constblock.constant.ci;

  if (index < 0 || index >= dp->nelt->constblock.constant.ci)
    {
      err(boundserror);
      return (-1);
    }

  return (index);
}



savedata(lvals, rvals)
elist *lvals;
vallist *rvals;
{
  static char *toomany = "more data values than data items";

  register elist *top;

  dataerror = NO;
  badvalue = NO;

  lvals = revelist(lvals);
  grvals = revrvals(rvals);

  refrig(lvals);

  if (!dataerror)
    outdata(lvals);

  frelist(lvals);

  while (grvals != NULL && dataerror == NO)
    {
      if (grvals->status != NORMAL)
	dataerror = YES;
      else if (grvals->repl <= 0)
        grvals = grvals->next;
      else
	{
	  err(toomany);
	  dataerror = YES;
	}
    }
    
  frvallist(grvals);

  return;
}



setdfiles(np)
register Namep np;
{
  register struct Extsym *cp;
  register struct Equivblock *ep;
  register int stg;
  register int type;
  register ftnint typelen;
  register ftnint nelt;
  register ftnint varsize;

  stg = np->vstg;

  if (stg == STGBSS || stg == STGINIT)
    {
      datafile = vdatafile;
      chkfile = vchkfile;
      if (np->init == YES)
	base = np->initoffset;
      else
	{
	  np->init = YES;
	  np->initoffset = base = vdatahwm;
	  if (np->vdim != NULL)
	    nelt = np->vdim->nelt->constblock.constant.ci;
	  else
	    nelt = 1;
	  type = np->vtype;
	  if (type == TYCHAR)
	    typelen = np->vleng->constblock.constant.ci;
	  else if (type == TYLOGICAL)
	    typelen = typesize[tylogical];
	  else
	    typelen = typesize[type];
	  varsize = nelt * typelen;
	  vdatahwm += varsize;
	}
    }
  else if (stg == STGEQUIV)
    {
      datafile = vdatafile;
      chkfile = vchkfile;
      ep = &eqvclass[np->vardesc.varno];
      if (ep->init == YES)
	base = ep->initoffset;
      else
	{
	  ep->init = YES;
	  ep->initoffset = base = vdatahwm;
	  vdatahwm += ep->eqvleng;
	}
      base += np->voffset;
    }
  else if (stg == STGCOMMON)
    {
      datafile = cdatafile;
      chkfile = cchkfile;
      cp = &extsymtab[np->vardesc.varno];
      if (cp->init == YES)
	base = cp->initoffset;
      else
	{
	  cp->init = YES;
	  cp->initoffset = base = cdatahwm;
	  cdatahwm += cp->maxleng;
	}
      base += np->voffset;
    }

  return;
}



wrtdata(offset, repl, len, constant)
long offset;
ftnint repl;
ftnint len;
char *constant;
{
  static char *badoffset = "bad offset in wrtdata";
  static char *toomuch = "too much data";
  static char *readerror = "read error on tmp file";
  static char *writeerror = "write error on tmp file";
  static char *seekerror = "seek error on tmp file";

  register ftnint k;
  long lastbyte;
  int bitpos;
  long chkoff;
  long lastoff;
  long chklen;
  long pos;
  int n;
  ftnint nbytes;
  int mask;
  register int i;
  char overlap;
  char allzero;
  char buff[BUFSIZ];

  if (offset < 0)
    fatal(badoffset);

  overlap = NO;

  k = repl * len;
  lastbyte = offset + k - 1;
  if (lastbyte < 0)
    {
      err(toomuch);
      dataerror = YES;
      return;
    }

  bitpos = offset % BYTESIZE;
  chkoff = offset/BYTESIZE;
  lastoff = lastbyte/BYTESIZE;
  chklen = lastoff - chkoff + 1;

  pos = lseek(chkfile, chkoff, 0);
  if (pos == -1)
    {
      err(seekerror);
      done(1);
    }

  while (k > 0)
    {
      if (chklen <= BUFSIZ)
	n = chklen;
      else
	{
	  n = BUFSIZ;
	  chklen -= BUFSIZ;
	}

      nbytes = read(chkfile, buff, n);
      if (nbytes < 0)
	{
	  err(readerror);
	  done(1);
	}

      if (nbytes == 0)
	buff[0] = '\0';

      if (nbytes < n)
	buff[ n-1 ] = '\0';

      i = 0;

      if (bitpos > 0)
	{
	  while (k > 0 && bitpos < BYTESIZE)
	    {
	      mask = 1 << bitpos;

	      if (mask & buff[0])
		overlap = YES;
	      else
		buff[0] |= mask;

	      k--;
	      bitpos++;
	    }

	  if (bitpos == BYTESIZE)
	    {
	      bitpos = 0;
	      i++;
	    }
	}

      while (i < nbytes && overlap == NO)
	{
	  if (buff[i] == 0 && k >= BYTESIZE)
	    {
	      buff[i++] = MAXBYTE;
	      k -= BYTESIZE;
	    }
	  else if (k < BYTESIZE)
	    {
	      while (k-- > 0)
		{
		  mask = 1 << k;
		  if (mask & buff[i])
		    overlap = YES;
		  else
		    buff[i] |= mask;
		}
	      i++;
	    }
	  else
	    {
	      overlap = YES;
	      buff[i++] = MAXBYTE;
	      k -= BYTESIZE;
	    }
	}

      while (i < n)
	{
	  if (k >= BYTESIZE)
	    {
	      buff[i++] = MAXBYTE;
	      k -= BYTESIZE;
	    }
	  else
	    {
	      while (k-- > 0)
		{
		  mask = 1 << k;
		  buff[i] |= mask;
		}
	      i++;
	    }
	}

      pos = lseek(chkfile, -nbytes, 1);
      if (pos == -1)
	{
	  err(seekerror);
	  done(1);
	}

      nbytes = write(chkfile, buff, n);
      if (nbytes != n)
	{
	  err(writeerror);
	  done(1);
	}
    }

  if (overlap == NO)
    {
      allzero = YES;
      k = len;

      while (k > 0 && allzero != NO)
	if (constant[--k] != 0) allzero = NO;

      if (allzero == YES)
	return;
    }

  pos = lseek(datafile, offset, 0);
  if (pos == -1)
    {
      err(seekerror);
      done(1);
    }

  k = repl;
  while (k-- > 0)
    {
      nbytes = write(datafile, constant, len);
      if (nbytes != len)
	{
	  err(writeerror);
	  done(1);
	}
    }

  if (overlap) overlapflag = YES;

  return;
}



Constp
getdatum()
{
  static char *toofew = "more data items than data values";

  register vallist *t;

  while (grvals != NULL)
    {
      if (grvals->status != NORMAL)
	{
	  dataerror = YES;
	  return (NULL);
	}
      else if (grvals->repl > 0)
	{
	  grvals->repl--;
	  return (grvals->value);
	}
      else
	{
	  badvalue = 0;
	  frexpr ((tagptr) grvals->value);
	  t = grvals;
	  grvals = t->next;
	  free((char *) t);
	}
    }

  err(toofew);
  dataerror = YES;
  return (NULL);
}



outdata(lvals)
elist *lvals;
{
  register elist *top;

  top = lvals;

  while (top != NULL && dataerror == NO)
    {
      if (top->elt->tag == SIMPLE)
	outaelt((aelt *) top->elt);
      else
	outdolist((dolist *) top->elt);

      top = top->next;
    }

  return;
}



outaelt(ap)
aelt *ap;
{
  static char *toofew = "more data items than data values";
  static char *boundserror = "substring expression out of bounds";
  static char *order = "substring expressions out of order";

  register Namep np;
  register long soffset;
  register dvalue *lwb;
  register dvalue *upb;
  register Constp constant;
  register int k;
  register vallist *t;
  register int type;
  register ftnint typelen;
  register ftnint repl;

  extern char *packbytes();

  np = ap->var;
  setdfiles(np);

  type = np->vtype;

  if (type == TYCHAR)
    typelen = np->vleng->constblock.constant.ci;
  else if (type == TYLOGICAL)
    typelen = typesize[tylogical];
  else
    typelen = typesize[type];

  if (ap->subs != NULL || np->vdim == NULL)
    {
      soffset = indexer(ap);
      if (soffset == -1)
	{
	  dataerror = YES;
	  return;
	}

      soffset = soffset * typelen;

      if (ap->range != NULL)
	{
	  lwb = (dvalue *) evalvexpr(ap->range->low);
	  upb = (dvalue *) evalvexpr(ap->range->high);
	  if (lwb->status == ERRVAL || upb->status == ERRVAL)
	    {
	      frvexpr((vexpr *) lwb);
	      frvexpr((vexpr *) upb);
	      dataerror = YES;
	      return;
	    }

	  if (lwb->status != NORMAL ||
	      lwb->value < 1 ||
	      lwb->value > typelen ||
	      upb->status != NORMAL ||
	      upb->value < 1 ||
	      upb->value > typelen)
	    {
	      err(boundserror);
	      frvexpr((vexpr *) lwb);
	      frvexpr((vexpr *) upb);
	      dataerror = YES;
	      return;
	    }

	  if (lwb->value > upb->value)
	    {
	      err(order);
	      frvexpr((vexpr *) lwb);
	      frvexpr((vexpr *) upb);
	      dataerror = YES;
	      return;
	    }

	  soffset = soffset + lwb->value - 1;
	  typelen = upb->value - lwb->value + 1;
	  frvexpr((vexpr *) lwb);
	  frvexpr((vexpr *) upb);
	}

      constant = getdatum();
      if (constant == NULL || !ISCONST(constant))
	return;

      constant = (Constp) convconst(type, typelen, constant);
      if (constant == NULL || !ISCONST(constant))
	{
	  frexpr((tagptr) constant);
	  return;
	}

      if (type == TYCHAR)
	wrtdata(base + soffset, 1, typelen, constant->constant.ccp);
      else
	wrtdata(base + soffset, 1, typelen, packbytes(constant));

      frexpr((tagptr) constant);
    }
  else
    {
      soffset = 0;
      k = np->vdim->nelt->constblock.constant.ci;
      while (k > 0 && dataerror == NO)
	{
	  if (grvals == NULL)
	    {
	      err(toofew);
	      dataerror = YES;
	    }
	  else if (grvals->status != NORMAL)
	    dataerror = YES;
	  else if (grvals-> repl <= 0)
	    {
	      badvalue = 0;
	      frexpr((tagptr) grvals->value);
	      t = grvals;
	      grvals = t->next;
	      free((char *) t);
	    }
	  else
	    {
	      constant = grvals->value;
	      if (constant == NULL || !ISCONST(constant))
		{
		  dataerror = YES;
		}
	      else
		{
		  constant = (Constp) convconst(type, typelen, constant);
		  if (constant == NULL || !ISCONST(constant))
		    {
		      dataerror = YES;
		      frexpr((tagptr) constant);
		    }
		  else
		    {
		      if (k > grvals->repl)
			repl = grvals->repl;
		      else
			repl = k;

		      grvals->repl -= repl;
		      k -= repl;

		      if (type == TYCHAR)
			wrtdata(base+soffset, repl, typelen, constant->constant.ccp);
		      else
			wrtdata(base+soffset, repl, typelen, packbytes(constant));

		      soffset = soffset + repl * typelen;

		      frexpr((tagptr) constant);
		    }
		}
	    }
	}
    }

  return;
}



outdolist(dp)
dolist *dp;
{
  static char *zerostep = "zero step in implied-DO";
  static char *order = "zero iteration count in implied-DO";

  register dvalue *e1, *e2, *e3;
  register int direction;
  register dvalue *dv;
  register int done;
  register int addin;
  register int ts;
  register ftnint tv;

  e1 = (dvalue *) evalvexpr(dp->init);
  e2 = (dvalue *) evalvexpr(dp->limit);
  e3 = (dvalue *) evalvexpr(dp->step);

  if (e1->status == ERRVAL ||
      e2->status == ERRVAL ||
      e3->status == ERRVAL)
    {
      dataerror = YES;
      goto ret;
    }

  if (e1->status == NORMAL)
    {
      if (e2->status == NORMAL)
	{
	  if (e1->value < e2->value)
	    direction = 1;
	  else if (e1->value > e2->value)
	    direction = -1;
	  else
	    direction = 0;
	}
      else if (e2->status == MAXPLUS1)
	direction = 1;
      else
	direction = -1;
    }
  else if (e1->status == MAXPLUS1)
    {
      if (e2->status == MAXPLUS1)
	direction = 0;
      else
	direction = -1;
    }
  else
    {
      if (e2->status == MINLESS1)
	direction = 0;
      else
	direction = 1;
    }

  if (e3->status == NORMAL && e3->value == 0)
    {
      err(zerostep);
      dataerror = YES;
      goto ret;
    }
  else if (e3->status == MAXPLUS1 ||
	   (e3->status == NORMAL && e3->value > 0))
    {
      if (direction == -1)
	{
	  warn(order);
	  goto ret;
	}
    }
  else
    {
      if (direction == 1)
	{
	  warn(order);
	  goto ret;
	}
    }

  dv = (dvalue *) dp->dovar;
  dv->status = e1->status;
  dv->value = e1->value;

  done = NO;
  while (done == NO && dataerror == NO)
    {
      outdata(dp->elts);

      if (e3->status == NORMAL && dv->status == NORMAL)
	{
	  addints(e3->value, dv->value);
	  dv->status = rstatus;
	  dv->value = rvalue;
	}
      else
	{
	  if (e3->status != NORMAL)
	    {
	      if (e3->status == MAXPLUS1)
		addin = MAXPLUS1;
	      else
		addin = MINLESS1;
	      ts = dv->status;
	      tv = dv->value;
	    }
	  else
	    {
	      if (dv->status == MAXPLUS1)
		addin = MAXPLUS1;
	      else
		addin = MINLESS1;
	      ts = e3->status;
	      tv = e3->value;
	    }

	  if (addin == MAXPLUS1)
	    {
	      if (ts == MAXPLUS1 || (ts == NORMAL && tv > 0))
		dv->status = ERRVAL;
	      else if (ts == NORMAL && tv == 0)
		dv->status = MAXPLUS1;
	      else if (ts == NORMAL)
		{
		  dv->status = NORMAL;
		  dv->value = tv + MAXINT;
		  dv->value++;
		}
	      else
		{
		  dv->status = NORMAL;
		  dv->value = 0;
		}
	    }
	  else
	    {
	      if (ts == MINLESS1 || (ts == NORMAL && tv < 0))
		dv->status = ERRVAL;
	      else if (ts == NORMAL && tv == 0)
		dv->status = MINLESS1;
	      else if (ts == NORMAL)
		{
		  dv->status = NORMAL;
		  dv->value = tv - MAXINT;
		  dv->value--;
		}
	      else
		{
		  dv->status = NORMAL;
		  dv->value = 0;
		}
	    }
	}

      if (dv->status == ERRVAL)
	done = YES;
      else if (direction > 0)
	{
	  if (e2->status == NORMAL)
	    {
	      if (dv->status == MAXPLUS1 ||
		  (dv->status == NORMAL && dv->value > e2->value))
		done = YES;
	    }
	}
      else if (direction < 0)
	{
	  if (e2->status == NORMAL)
	    {
	      if (dv->status == MINLESS1 ||
		  (dv->status == NORMAL && dv->value < e2->value))
		done = YES;
	    }
	}
      else
	done = YES;
    }

ret:
  frvexpr((vexpr *) e1);
  frvexpr((vexpr *) e2);
  frvexpr((vexpr *) e3);
  return;
}
