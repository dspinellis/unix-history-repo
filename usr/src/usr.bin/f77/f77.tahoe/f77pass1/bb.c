/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)bb.c	5.1 (Berkeley) 6/7/85";
#endif not lint

/*
 * bb.c
 *
 * Basic block optimizations.
 *
 * University of Utah CS Dept modification history:
 *
 * Revision 2.1  84/07/19  12:01:20  donn
 * Changed comment headers for UofU.
 * 
 * Revision 1.2  84/04/02  14:22:49  donn
 * Bug in copy propagation missed places where temporaries are assigned to
 * by OPSTAREQ or OPPLUSEQ, e.g. exponentiation with an integer constant
 * power, expanded inline.
 * 
 */

#include "defs.h"
#include "optim.h"

/*
 *  This file contains code for determination of basic blocks,
 *  as well as some other optimization supporting routines
 *  [including the main routine 'optimize()'].
 *
 *  The compiler's general debugging flag ['debugflag'] has been
 *  extended to provide the capability of having multiple flags
 *  which are contained in an array.  If the option -d is used,
 *  then the flag debugflag[0] is set.  If a sequence of one or more
 *  numbers are given (e.g, -d3,7,12), then the flags debugflag[3],
 *  debugflag[7], and debugflag[12] are set.  The maximum number of
 *  flags available is specified in the defines.h file.
 */


Bblockp	firstblock = NULL;		/* first block in buffer */
Bblockp	lastblock = NULL;		/* last block in buffer */

expptr	tempalloc();


optimize ()

{
Bblockp bb;
Slotp	sl,nextsl;

if (debugflag[2]) showbuffer ();

optloops ();

if (debugflag[3]) showbuffer ();

formbblock ();
optcse ();

if (debugflag[4]) showbuffer ();

if (! debugflag[7])
	copyprop ();

if (debugflag[9]) showbuffer ();

for (sl = firstslot; sl; sl = nextsl)
	{
	nextsl = sl->next;
	if (sl->type == SKFRTEMP)
		{
		templist = mkchain (sl->expr,templist);
		sl->expr = NULL;
		delslot (sl);
		}
	else
		sl->expr = tempalloc (sl->expr);
	}

if (! debugflag[10])
	regalloc ();

flushopt ();
}



/*
 *  creates a new basic block record
 */

LOCAL Bblockp newblock (sl)
Slotp	sl;

{
register Bblockp bb;

bb = ALLOC( bblock );
bb->next = NULL ;
if (lastblock)
	{
	bb->prev = lastblock;
	lastblock->next = bb;
	lastblock = bb;
	}
else
	{
	firstblock = lastblock = bb;
	bb->prev = NULL;
	}

bb->first = sl;
return (bb);
}



/*
 *  scans slot buffer, creating basic block records
 */

formbblock ()

{
Slotp	sl;
field	type;
Bblockp	newbb;

newbb = NULL;
for (sl = firstslot; sl; sl = sl->next)
	{
	type = sl->type;
	switch (type)
		{
		case SKEQ:
			if (!newbb)
				newbb = newblock(sl);
			if (containscall(sl->expr))
				{
				newbb->last = sl;
				newbb = NULL;
				}
			break;
		case SKNULL:
		case SKASSIGN:
		case SKFRTEMP:
			if (!newbb)
				newbb = newblock(sl);
			break;
		case SKPAUSE:
		case SKSTOP:
		case SKIFN:
		case SKGOTO:
		case SKCMGOTO:
		case SKARIF:
		case SKASGOTO:
		case SKIOIFN:
		case SKCALL:
		case SKRETURN:
			if (!newbb)
				newbb = newblock(sl);
			newbb->last = sl;
			newbb = NULL;
			break;
		case SKLABEL:
			if (newbb)
				newbb->last = sl->prev;
			newbb = newblock(sl);
			break;
		case SKDOHEAD:
		case SKENDDO:
			if (!newbb)
				newbb = newblock(sl);
			break;
		default:
			badthing("SKtype", "formbblock", type);
			break;
		}
	}
if (newbb)
	newbb->last = lastslot;
}



/*
 *  frees all basic block records
 *  as well as the id and value node chains hanging off the bb and their
 *  respective cross link chains (IDlist, DUPlist and NODElist structs) 
 */

clearbb ()
{
Bblockp	bb,next;

for (bb = firstblock; bb; bb = next)
	{
	next = bb->next;
	   { 
	     idptr idp,next;
	     for(idp = bb->headid; idp; idp = next) 
		{ 
		 next = idp->next;
		      {
		      nodelptr nodelp, next;
	              for(nodelp = idp->headnodelist; nodelp; nodelp = next)
			 {
			    next = nodelp->next;
		            free( (charptr) nodelp);
		         }	
		      }
                 free( (charptr) idp);
	        }	
           }
	   { 
	     valuen vp,next;
	     for(vp = bb->headnode; vp; vp = next) 
		{ 
		 next = vp->next;
		      {
		      idlptr idlp, next;
	              for(idlp = vp->headdeplist; idlp; idlp = next)
			 {
			    next = idlp->next;
		            free( (charptr) idlp);
		         }	
		      }
		      {
		      duplptr duplp, next;
	              for(duplp = vp->headduplist; duplp; duplp = next)
			 {
			    next = duplp->next;
		            free( (charptr) duplp);
		         }	
		      }
                 free( (charptr) vp);
	        }	
           }
	free ( (charptr) bb);
	}
firstblock = lastblock = NULL;
}


/* structure for maintaining records on copy statements */

typedef struct Subrec {
	Addrp	lmem;
	Addrp	rmem;
	int	sets;
} *Subrecp;


LOCAL chainp sublist;	/* list of copy statements */
LOCAL int prop1count;	/* count of number of temporaries eliminated */
LOCAL int prop2count;	/* count of number of uses of temporaries replaced */

expptr rmcommaop();
Addrp subfor();



/*
 *  eliminates copy statements of the form T1 = T2 from the intermediate
 *  code, where T1 and T2 are temporary variables which are each
 *  set only once;  eliminates the copy statement and replaces each
 *  use of T1 by T2 (T1 is therefore totally eliminated).
 */

LOCAL copyprop ()

{
Slotp	sl,nextsl;
expptr	expr;
Tempp	lp,rp;

for (sl = firstslot; sl; sl = sl->next)
	sl->expr = rmcommaop (sl->expr,sl);

prop1count = prop2count = 0;
findcopies ();

for (sl = firstslot; sl; sl = nextsl)
	{
	nextsl = sl->next;
	expr = sl->expr;

	if ((sl->type == SKFRTEMP) && subfor (expr))
		{
		delslot (sl);
		expr = ENULL;
		}
	else if (expr && expr->tag == TEXPR &&
			expr->exprblock.opcode == OPASSIGN)
		{
		lp = (Tempp) expr->exprblock.leftp;
		rp = (Tempp) expr->exprblock.rightp;
		if (lp->tag == TTEMP && rp->tag == TTEMP)
			if (subfor(lp->memalloc) == rp->memalloc
					&& !subfor (rp->memalloc))
				{
				frexpr (expr);
				expr = sl->expr = ENULL;
				prop1count++;
				}
		}

	propagate (expr);
	}

if (debugflag[0])
	fprintf (diagfile,
	    "%d temporarie%s replaced by copy propagation (%d use%s)\n",
		prop1count,(prop1count==1 ? "" : "s"),
		prop2count,(prop2count==1 ? "" : "s") );
}



/*
 *  finds copy statements and enters information in table
 */

LOCAL findcopies ()

{
Slotp	sl;
expptr	expr;
chainp	cp;

for (sl = firstslot; sl; sl = sl->next)
	{
	expr = sl->expr;
	if (expr) switch (expr->tag)
	    {
	    case TEXPR:
		ckexpr (expr);
		break;

	    case TLIST:
		for (cp = expr->listblock.listp; cp; cp = cp->nextp)
			{
			expr = (expptr) cp->datap;
			ckexpr (expr);
			}
		break;

	    default:
		break;
	    }
	}
}



/*
 *  checks an individual expression
 */

ckexpr (expr)
expptr	expr;

{
Tempp	lp,rp;
int	oc = expr->exprblock.opcode;

if (oc == OPASSIGN || oc == OPPLUSEQ || oc == OPSTAREQ)
	{
	lp = (Tempp) expr->exprblock.leftp;
	rp = (Tempp) expr->exprblock.rightp;
	if (lp->tag == TTEMP)
		if (rp->tag == TTEMP && oc == OPASSIGN)
			enter (lp->memalloc, rp->memalloc);
		else
			enter (lp->memalloc, ENULL);
	}
}



/*
 *  Enters the given memalloc values in the table (or update if they
 *  are already there), for the assignment statement m1 = m2.
 *  If m2 is NULL, this indicates that the assignment is not a copy
 *  statement.
 */

LOCAL enter (m1,m2)
Addrp	m1,m2;

{
chainp	cp;
Subrecp old,new;

for (cp = sublist; cp; cp = cp->nextp)
	{
	old = (Subrecp) cp->datap;
	if (old->lmem == m1)
		{
		old->sets++;
		return;
		}
	}

new = ALLOC (Subrec);
new->lmem = m1;
new->rmem = m2;
new->sets = 1;
sublist = mkchain (new, sublist);
}



/*
 *  looks for record for the given memalloc value
 */

LOCAL Subrecp lookup (mem)
Addrp	mem;

{
chainp	cp;
Subrecp rec;

for (cp = sublist; cp; cp = cp->nextp)
	{
	rec = (Subrecp) cp->datap;
	if (rec->lmem == mem)
		return rec;
	}

return NULL;
}



/*
 *  checks to see if there is a substitute for given memalloc value
 */

LOCAL Addrp subfor (mem)
Addrp	mem;

{
Subrecp rec,rec2;
Addrp	sub;

rec = lookup (mem);
if (rec && rec->sets == 1)
	{
	sub = rec->rmem;
	rec2 = lookup(sub);
	if (rec2 && rec2->sets == 1)
		return sub;
	}

return NULL;
}



/*
 *  actually propagates the information
 */

LOCAL propagate (expr)
expptr	expr;

{
chainp	t;
Addrp	new;

if (! expr) return;

switch (expr->tag)
	{
	case TEXPR:
		propagate (expr->exprblock.leftp);
		propagate (expr->exprblock.rightp);
		break;

	case TADDR:
		propagate (expr->addrblock.vleng);
		propagate (expr->addrblock.memoffset);
		break;

	case TLIST:
		for (t = expr->listblock.listp; t; t = t->nextp)
			propagate (t->datap);
		break;

	case TTEMP:
		new = subfor (expr->tempblock.memalloc);
		if (new)
			{
			expr->tempblock.memalloc = new;
			prop2count++;
			}
		break;

	default:
		break;
	}
}



/*
 *  allocates ADDR blocks for each TEMP in the expression
 */

LOCAL expptr tempalloc (expr)
expptr	expr;

{
chainp	t;

if (! expr)
	return NULL;

switch (expr->tag)
    {
    case TEXPR:
	expr->exprblock.leftp = tempalloc (expr->exprblock.leftp);
	expr->exprblock.rightp = tempalloc (expr->exprblock.rightp);
	break;

    case TADDR:
	expr->addrblock.vleng = tempalloc (expr->addrblock.vleng);
	expr->addrblock.memoffset = tempalloc (expr->addrblock.memoffset);
	break;

    case TLIST:
	for (t = expr->listblock.listp; t; t = t->nextp)
		t->datap = (tagptr) tempalloc (t->datap);
	break;

    case TTEMP:
	return (expptr) cpexpr (altmpn (expr));
	break;

    default:
	break;
    }
return expr;
}


/********************* debugging routines *********************/



Announce (s,q)
char *s;
expptr q;

{
fprintf (diagfile,"\nAn expression [%s]----->\n",s);
showexpr(q,0);
fprintf (diagfile,"\n-------------end of expr--------------\n");
}



/*
 *  dump the basic block buffer, including expressions, mnemonically
 */

showbuffer ()

{
Slotp	sl;
Bblockp	bb;
int	i;

fprintf (diagfile,"Basic blocks with first and last slots ----------\n");
for (i=1, bb = firstblock; bb; i++, bb = bb->next)
	fprintf (diagfile,"%2d.  %d  %d\n",i,bb->first,bb->last);
fprintf (diagfile,"\n");

fprintf (diagfile,"Slots and expressions ----------\n");

fprintf (diagfile,"tag pointer vtype vclass vstg vleng\n");
fprintf (diagfile,"          ADDR memno memoffset istemp ntempelt varleng\n");
fprintf (diagfile,"          TEMP memalloc istemp ntempelt varleng\n");
fprintf (diagfile,"          EXPR opcode leftp rightp\n");
fprintf (diagfile,"          LIST type listp\n");
fprintf (diagfile,"\n");

for (i=1, sl = firstslot; sl; i++, sl = sl->next)
	{
	fprintf (diagfile,"%2d.  ",i);
	showslt (sl);
	}
fprintf (diagfile,"---------- End of showbuffer ----------\n");
}



/*
 *  dumps a single slot in the code buffer
 */

LOCAL charptr Zslot[] = {"NULL",
	"IFN","GOTO","LABEL","EQ","CALL","CMGOTO","STOP","DOHEAD",
	"ENDDO","ARIF","RETURN","ASGOTO","PAUSE","ASSIGN","IOIFN","FRTEMP"};



showslt (sl)
Slotp sl;

{
fprintf (diagfile,"(%2d)  %d  %s  %d\n",
	sl->lineno,sl,Zslot[sl->type],sl->label);
showexpr (sl->expr,0);
fprintf (diagfile,"\n");
}



showslottype (type)
int type;

{
fprintf (diagfile,"%s\n",Zslot[type]);
}



/*
 *  displays the given expression at the given indentation, showing
 *  its subexpressions at further indentations
 */

LOCAL charptr Ztag[] = {"----",
	"NAME","CONST","EXPR","ADDR","TEMP","PRIM","LIST","IMPLDO","ERROR"};
LOCAL charptr Zstg[] = {"unk",
	"ARG","AUTO","BSS","INIT","CONST","EXT","INTR","STFUNCT",
	"COMMON","EQUIV","REG","LENG","NULL","PREG"};
LOCAL charptr Zclass[] = {"unk",
	"PARAM","VAR","ENTRY","MAIN","BLOCK","PROC","NAMELIST"};
LOCAL charptr Zop[] = {"----",
	"PLUS","MINUS","STAR","SLASH","POWER","NEG","OR","AND","EQV",
	"NEQV","NOT","CONCAT","LT","EQ","GT","LE","NE","GE","CALL",
	"CCALL","ASSIGN","PLUSEQ","STAREQ","CONV","LSHIFT","MOD",
	"COMMA","QUEST","COLON","ABS","MIN","MAX","ADDR","INDIRECT",
	"BITOR","BITAND","BITXOR","BITNOT","RSHIFT","PAREN"};
LOCAL charptr Ztype[] = {"unk",
	"ADDR","SHORT","LONG","REAL","DREAL","COMPLEX","DCOMPLEX",
	"LOGICAL","CHAR","SUBR","ERROR"};


showexpr(p,indent)
tagptr p;
int indent;

{
int i;
int type;
chainp q;

#define PRHEAD(q) fprintf(diagfile,"%s %d %s %s %s %d", \
	Ztag[q->tag], q, Ztype[q->headblock.vtype], \
	Zclass[q->headblock.vclass], Zstg[q->headblock.vstg], \
	q->headblock.vleng);
#define SHOWEXPR(p) showexpr(p,indent+2)



if(p == NULL)
	return;

for (i=0; i<indent; i++)
	putc(' ',diagfile);

switch(p->tag)
         {
         case TCONST:
              PRHEAD(p);

              type=p->constblock.vtype;
              if (ISCHAR(p))
                 {
                      fprintf(diagfile," ISCHAR ccp= %d\n",
                                                   p->constblock.const.ccp);
                      SHOWEXPR(p->constblock.vleng);
                 }
              else  if( ISINT(type) )
                   fprintf(diagfile," ci= %d\n",p->constblock.const.ci); 
              else if( ISREAL(type) )
                   fprintf(diagfile," cd[0]= %e\n",p->constblock.const.cd[0]);
              else fprintf(diagfile," cd[0]= %e  cd[1]= %e\n",
                            p->constblock.const.cd[0],
                            p->constblock.const.cd[1] ); 
              break;

         case TADDR:
              PRHEAD(p);
              fprintf(diagfile,
              " memno= %d %d %d %d %d\n",
              p->addrblock.memno,p->addrblock.memoffset,p->addrblock.istemp,
              p->addrblock.ntempelt,p->addrblock.varleng);
              SHOWEXPR(p->addrblock.vleng);
              SHOWEXPR(p->addrblock.memoffset);
              break;

         case TTEMP:
	      fprintf(diagfile,"%s %d %s %s %d",
			Ztag[p->tag], p, Ztype[p->headblock.vtype],
			Zclass[p->headblock.vclass],
			p->headblock.vleng);
              fprintf(diagfile,
		" memalloc= %d %d %d %d\n",
		p->tempblock.memalloc,p->tempblock.istemp,
		p->tempblock.ntempelt,p->tempblock.varleng);
              SHOWEXPR(p->tempblock.vleng);
	      SHOWEXPR(p->tempblock.memalloc);
              break;

         case TERROR:
              fprintf(diagfile,"ERROR %d\n",p);
              break;
          
         case TNAME:
              fprintf(diagfile,"NAME %d\n",p);
              return;

         case TPRIM:
              fprintf(diagfile,"PRIM %d --- not implemented\n",p);
              break;

         case TEXPR:
              PRHEAD(p);
              fprintf(diagfile," opcode= %s %d %d\n",
                     Zop[p->exprblock.opcode],p->exprblock.leftp,
                     p->exprblock.rightp);
              SHOWEXPR(p->exprblock.leftp);
              if(p->exprblock.rightp)
                    SHOWEXPR(p->exprblock.rightp);
              break;

         case TLIST:
              fprintf(diagfile,"LIST %d %s %d\n",p,
                      Ztype[p->listblock.vtype],p->listblock.listp);
              for(q= p->listblock.listp ; q ; q = q->nextp)
                      SHOWEXPR(q->datap);
	      for (i=0; i<indent; i++)
		putc (' ',diagfile);
              fprintf(diagfile,"END LIST %d\n",p);
              break;

         default:
              fprintf(diagfile,"showexpr BAD TAG= %d at %d \n",p->tag,p);
           }
}



selective()/************************************/
{
int i;
Slotp sl;

i=0;
fprintf (stderr,"SELECTIVE OUTPUT\n");
for (sl=firstslot;sl;sl=sl->next)
	{
	i++;
/*
	if (i>=176 && i<184)
*/
		{
		fprintf (stderr,"%d.  ",i);
		showslt(sl);
		}
	}
}




LOCAL containscall(p)
expptr p;
{
  chainp cp;

  if (p == NULL)
    return NO;

  switch (p->tag)
    {
    case TADDR:
      if (containscall(p->addrblock.vleng)
	  || containscall(p->addrblock.memoffset))
	return YES;
      else
        return NO;

    case TCONST:
      return NO;

    case TERROR:
      return NO;

    case TEXPR:
      if (p->exprblock.opcode == OPCALL ||
	  p->exprblock.opcode == OPCCALL)
	return YES;
      if (containscall(p->exprblock.vleng) ||
	  containscall(p->exprblock.leftp) ||
	  containscall(p->exprblock.rightp))
	return YES;
      else
	return NO;

    case TLIST:
      cp = p->listblock.listp;
      while (cp)
	{
	  if (containscall(cp->datap))
	    return YES;
	  cp = cp->nextp;
	}
      return NO;

    default:
      return YES;
    }
}
