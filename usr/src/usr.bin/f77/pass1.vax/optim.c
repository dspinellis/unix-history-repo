/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)optim.c	5.5 (Berkeley) %G%";
#endif /* not lint */

/*
 * optim.c
 *
 * Miscellaneous optimizer routines, f77 compiler pass 1.
 *
 * UCSD Chemistry modification history:
 *
 * $Log:	optim.c,v $
 * Revision 5.2  86/03/04  17:47:08  donn
 * Change buffcat() and buffct1() analogously to putcat and putct1() --
 * ensure that memoffset is evaluated before vleng.  Take care not to
 * screw up and return something other than an expression.
 * 
 * Revision 5.1  85/08/10  03:48:42  donn
 * 4.3 alpha
 * 
 * Revision 2.12  85/06/08  22:57:01  donn
 * Prevent core dumps -- bug in optinsert was causing lastslot to be wrong
 * when a slot was inserted at the end of the buffer.
 * 
 * Revision 2.11  85/03/18  08:05:05  donn
 * Prevent warnings about implicit conversions.
 * 
 * Revision 2.10  85/02/12  20:13:00  donn
 * Resurrected the hack in 2.6.1.1 to avoid creating a temporary when
 * there is a concatenation on the rhs of an assignment, and threw out
 * all the code dealing with starcat().  It seems that we can't use a
 * temporary because the lhs as well as the rhs may have nonconstant length.
 * 
 * Revision 2.9  85/01/18  00:53:52  donn
 * Missed a call to free() in the last change...
 * 
 * Revision 2.8  85/01/18  00:50:03  donn
 * Fixed goof made when modifying buffmnmx() to explicitly call expand().
 * 
 * Revision 2.7  85/01/15  18:47:35  donn
 * Changes to allow character*(*) variables to appear in concatenations in
 * the rhs of an assignment statement.
 * 
 * Revision 2.6  84/12/16  21:46:27  donn
 * Fixed bug that prevented concatenations from being run together.  Changed
 * buffpower() to not touch exponents greater than 64 -- let putpower do them.
 * 
 * Revision 2.5  84/10/29  08:41:45  donn
 * Added hack to flushopt() to prevent the compiler from trying to generate
 * intermediate code after an error.
 * 
 * Revision 2.4  84/08/07  21:28:00  donn
 * Removed call to p2flush() in putopt() -- this allows us to make better use
 * of the buffering on the intermediate code file.
 * 
 * Revision 2.3  84/08/01  16:06:24  donn
 * Forced expand() to expand subscripts.
 * 
 * Revision 2.2  84/07/19  20:21:55  donn
 * Decided I liked the expression tree algorithm after all.  The algorithm
 * which repeatedly squares temporaries is now checked in as rev. 2.1.
 * 
 * Revision 1.3.1.1  84/07/10  14:18:18  donn
 * I'm taking this branch off the trunk -- it works but it's not as good as
 * the old version would be if it worked right.
 * 
 * Revision 1.5  84/07/09  22:28:50  donn
 * Added fix to buffpower() to prevent it chasing after huge exponents.
 * 
 * Revision 1.4  84/07/09  20:13:59  donn
 * Replaced buffpower() routine with a new one that generates trees which can
 * be handled by CSE later on.  
 * 
 * Revision 1.3  84/05/04  21:02:07  donn
 * Added fix for a bug in buffpower() that caused func(x)**2 to turn into
 * func(x) * func(x).  This bug had already been fixed in putpower()...
 * 
 * Revision 1.2  84/03/23  22:47:21  donn
 * The subroutine argument temporary fixes from Bob Corbett didn't take into
 * account the fact that the code generator collects all the assignments to
 * temporaries at the start of a statement -- hence the temporaries need to
 * be initialized once per statement instead of once per call.
 * 
 */

#include "defs.h"
#include "optim.h"



/*
 *		Information buffered for each slot type
 *
 *  slot type	       expptr	       integer		pointer
 *
 *  IFN			expr		label		-
 *  GOTO		-		label		-
 *  LABEL		-		label		-
 *  EQ			expr		-		-
 *  CALL		expr		-		-
 *  CMGOTO		expr		num		labellist*
 *  STOP		expr		-		-
 *  DOHEAD		[1]		-		ctlframe*
 *  ENDDO		[1]		-		ctlframe*
 *  ARIF		expr		-		labellist*
 *  RETURN		expr		label		-
 *  ASGOTO		expr		-		labellist*
 *  PAUSE		expr		-		-
 *  ASSIGN		expr		label		-
 *  SKIOIFN		expr		label		-
 *  SKFRTEMP		expr		-		-
 *
 *     Note [1]:  the nullslot field is a pointer to a fake slot which is
 *     at the end of the slots which may be replaced by this slot.  In
 *     other words, it looks like this:
 *		DOHEAD slot
 *		slot   \
 *		slot    > ordinary IF, GOTO, LABEL slots which implement the DO
 *		slot   /
 *		NULL slot
 */


expptr expand();

Slotp	firstslot = NULL;
Slotp	lastslot = NULL;
int	numslots = 0;


/*
 *  turns off optimization option
 */

optoff()

{
flushopt();
optimflag = 0;
}



/*
 *  initializes the code buffer for optimization
 */

setopt()

{
register Slotp sp;

for (sp = firstslot; sp; sp = sp->next)
	free ( (charptr) sp);
firstslot = lastslot = NULL;
numslots = 0;
}



/*
 *  flushes the code buffer
 */

LOCAL int alreadycalled = 0;

flushopt()
{
register Slotp sp;
int savelineno;

if (alreadycalled) return;	/* to prevent recursive call during errors */
alreadycalled = 1;

if (debugflag[1])
	showbuffer ();

frtempbuff ();

savelineno = lineno;
for (sp = firstslot; sp; sp = sp->next)
	{
	if (nerr == 0)
		putopt (sp);
	else
		frexpr (sp->expr);
        if(sp->ctlinfo) free ( (charptr) sp->ctlinfo);
        free ( (charptr) sp);
        numslots--;
	}
firstslot = lastslot = NULL;
numslots = 0;
clearbb();
lineno = savelineno;

alreadycalled = 0;
}



/*
 *  puts out code for the given slot (from the code buffer)
 */

LOCAL putopt (sp)
register Slotp sp;
{
	lineno = sp->lineno;
	switch (sp->type) {
	    case SKNULL:
		break;
	    case SKIFN:
	    case SKIOIFN:
		putif(sp->expr, sp->label);
		break;
	    case SKGOTO:
		putgoto(sp->label);
		break;
	    case SKCMGOTO:
		putcmgo(sp->expr, sp->label, sp->ctlinfo);
		break;
	    case SKCALL:
		putexpr(sp->expr);
		break;
	    case SKSTOP:
		putexpr (call1 (TYSUBR, "s_stop", sp->expr));
		break;
	    case SKPAUSE:
		putexpr (call1 (TYSUBR, "s_paus", sp->expr));
		break;
	    case SKASSIGN:
		puteq (sp->expr,
		    intrconv(sp->expr->headblock.vtype, mkaddcon(sp->label)));
		break;
	    case SKDOHEAD:
	    case SKENDDO:
		break;
	    case SKEQ:
		putexpr(sp->expr);
		break;
	    case SKARIF:
#define LM   ((struct Labelblock * *)sp->ctlinfo)[0]->labelno 
#define LZ   ((struct Labelblock * *)sp->ctlinfo)[1]->labelno 
#define LP   ((struct Labelblock * *)sp->ctlinfo)[2]->labelno 
       		prarif(sp->expr, LM, LZ, LP);
		break;
	    case SKASGOTO:
		putbranch((Addrp) sp->expr);
		break;
	    case SKLABEL:
		putlabel(sp->label);
		break;
	    case SKRETURN:
		if (sp->expr)
			{
			putforce(TYINT, sp->expr);
			putgoto(sp->label);
			}
		else
			putgoto(sp->label);
		break;
	    case SKFRTEMP:
		templist = mkchain (sp->expr,templist);
		break;
	    default:
		badthing("SKtype", "putopt", sp->type);
		break;
	}

	/*
	 * Recycle argument temporaries here.  This must get done on a
	 *	statement-by-statement basis because the code generator
	 *	makes side effects happen at the start of a statement.
	 */
	argtemplist = hookup(argtemplist, activearglist);
	activearglist = CHNULL;
}



/*
 *  copies one element of the control stack
 */

LOCAL struct Ctlframe *cpframe(p)
register char *p;
{
static int size =  sizeof (struct Ctlframe);
register int n;
register char *q;
struct Ctlframe *q0;

q0 = ALLOC(Ctlframe);
q = (char *) q0;
n = size;
while(n-- > 0)
	*q++ = *p++;
return( q0);
}



/*
 *  copies an array of labelblock pointers
 */

LOCAL struct Labelblock **cplabarr(n,arr)
struct Labelblock *arr[];
int n;
{
struct Labelblock **newarr;
register char *in, *out;
register int i,j;

newarr = (struct Labelblock **) ckalloc (n * sizeof (char *));
for (i = 0; i < n; i++)
	{
	newarr[i] = ALLOC (Labelblock);
	out = (char *) newarr[i];
	in = (char *) arr[i];
	j = sizeof (struct Labelblock);
	while (j-- > 0)
		*out++ = *in++;
	}
return (newarr);
}



/*
 *  creates a new slot in the code buffer
 */

LOCAL Slotp newslot()
{
register Slotp sp;

++numslots;
sp = ALLOC( slt );
sp->next = NULL ;
if (lastslot)
	{
	sp->prev = lastslot;
	lastslot = lastslot->next = sp;
	}
else
	{
	firstslot = lastslot = sp;
	sp->prev = NULL;
	}
sp->lineno = lineno;
return (sp);
}



/*
 *  removes (but not deletes) the specified slot from the code buffer
 */

removeslot (sl)
Slotp	sl;

{
if (sl->next)
	sl->next->prev = sl->prev;
else
	lastslot = sl->prev;
if (sl->prev)
	sl->prev->next = sl->next;
else
	firstslot = sl->next;
sl->next = sl->prev = NULL;

--numslots;
}



/*
 *  inserts slot s1 before existing slot s2 in the code buffer;
 *  appends to end of list if s2 is NULL.
 */

insertslot (s1,s2)
Slotp	s1,s2;

{
if (s2)
	{
	if (s2->prev)
		s2->prev->next = s1;
	else
		firstslot = s1;
	s1->prev = s2->prev;
	s2->prev = s1;
	}
else
	{
	s1->prev = lastslot;
	lastslot->next = s1;
	lastslot = s1;
	}
s1->next = s2;

++numslots;
}



/*
 *  deletes the specified slot from the code buffer
 */

delslot (sl)
Slotp	sl;

{
removeslot (sl);

if (sl->ctlinfo)
	free ((charptr) sl->ctlinfo);
frexpr (sl->expr);
free ((charptr) sl);
numslots--;
}



/*
 *  inserts a slot before the specified slot; if given NULL, it is
 *  inserted at the end of the buffer
 */

Slotp optinsert (type,p,l,c,currslot)
int	type;
expptr	p;
int	l;
int	*c;
Slotp	currslot;

{
Slotp	savelast,new;

savelast = lastslot;
if (currslot)
	lastslot = currslot->prev;
new = optbuff (type,p,l,c);
new->next = currslot;
if (currslot)
	currslot->prev = new;
new->lineno = -1;	/* who knows what the line number should be ??!! */
if (currslot)
	lastslot = savelast;
return (new);
}



/*
 *  buffers the FRTEMP slots which have been waiting
 */

frtempbuff ()

{
chainp ht;
register Slotp sp;

for (ht = holdtemps; ht; ht = ht->nextp)
	{
	sp = newslot();
		/* this slot actually belongs to some previous source line */
	sp->lineno = sp->lineno - 1;
	sp->type = SKFRTEMP;
	sp->expr = (expptr) ht->datap;
	sp->label = 0;
	sp->ctlinfo = NULL;
	}
holdtemps = NULL;
}



/*
 *  puts the given information into a slot at the end of the code buffer
 */

Slotp optbuff (type,p,l,c)
int	type;
expptr	p;
int	l;
int	*c;

{
register Slotp sp;

if (debugflag[1])
	{
	fprintf (diagfile,"-----optbuff-----"); showslottype (type);
	showexpr (p,0); fprintf (diagfile,"\n");
	}

p = expand (p);
sp = newslot();
sp->type = type;
sp->expr = p;
sp->label = l;
sp->ctlinfo = NULL;
switch (type)
	{
	case SKCMGOTO:
		sp->ctlinfo = (int*) cplabarr (l, (struct Labelblock**) c);
		break;
	case SKARIF:
		sp->ctlinfo = (int*) cplabarr (3, (struct Labelblock**) c);
		break;
	case SKDOHEAD:
	case SKENDDO:
		sp->ctlinfo = (int*) cpframe ((struct Ctlframe*) c);
		break;
	default:
		break;
	}

frtempbuff ();

return (sp);
}



/*
 *  expands the given expression, if possible (e.g., concat, min, max, etc.);
 *  also frees temporaries when they are indicated as being the last use
 */

#define APPEND(z)	\
	res = res->exprblock.rightp = mkexpr (OPCOMMA, z, newtemp)

LOCAL expptr expand (p)
tagptr p;

{
Addrp t;
expptr q;
expptr buffmnmx(), buffpower(), buffcat();

if (!p)
	return (ENULL);
switch (p->tag)
	{
	case TEXPR:
		switch (p->exprblock.opcode)
			{
			case OPASSIGN: /* handle a = b // c */
				if (p->exprblock.vtype != TYCHAR)
					goto standard;
				q = p->exprblock.rightp;
				if (!(q->tag == TEXPR &&
				      q->exprblock.opcode == OPCONCAT))
					goto standard;
				t = (Addrp) expand(p->exprblock.leftp);
				frexpr(p->exprblock.vleng);
				free( (charptr) p );
				p = (tagptr) q;
				goto cat;
			case OPCONCAT:
				t = mktemp (TYCHAR, ICON(lencat(p)));
			cat:
				q = (expptr) cpexpr (p->exprblock.vleng);
				p = (tagptr) buffcat (t, p);
				frexpr (p->headblock.vleng);
				p->headblock.vleng = q;
				break;
			case OPMIN:
			case OPMAX:
				p = (tagptr) buffmnmx (p);
				break;
			case OPPOWER:
				p = (tagptr) buffpower (p);
				break;
			default:
			standard:
				p->exprblock.leftp =
					expand (p->exprblock.leftp);
				if (p->exprblock.rightp)
					p->exprblock.rightp =
						expand (p->exprblock.rightp);
				break;
			}
		break;

	case TLIST:
		{
		chainp t;
		for (t = p->listblock.listp; t; t = t->nextp)
			t->datap = (tagptr) expand (t->datap);
		}
		break;

	case TTEMP:
		if (p->tempblock.istemp)
			frtemp(p);
		break;

	case TADDR:
		p->addrblock.memoffset = expand( p->addrblock.memoffset );
		break;

	default:
		break;
	}
return ((expptr) p);
}



/*
 *  local version of routine putcat in putpcc.c, called by expand
 */

LOCAL expptr buffcat(lhs, rhs)
register Addrp lhs;
register expptr rhs;
{
int n;
Addrp lp, cp;
expptr ep, buffct1();

n = ncat(rhs);
lp = (Addrp) mkaltmpn(n, TYLENG, PNULL);
cp = (Addrp) mkaltmpn(n, TYADDR, PNULL);

n = 0;
ep = buffct1(rhs, lp, cp, &n);

ep = mkexpr(OPCOMMA, ep,
	call4(TYSUBR, "s_cat", lhs, cp, lp, mkconv(TYLONG, ICON(n))));

return (ep);
}



/*
 *  local version of routine putct1 in putpcc.c, called by expand
 */

LOCAL expptr buffct1(q, lp, cp, ip)
register expptr q;
register Addrp lp, cp;
int *ip;
{
int i;
Addrp lp1, cp1;
expptr eleft, eright;

if(q->tag==TEXPR && q->exprblock.opcode==OPCONCAT)
	{
	eleft = buffct1(q->exprblock.leftp, lp, cp, ip);
	eright = buffct1(q->exprblock.rightp, lp, cp, ip);
	frexpr(q->exprblock.vleng);
	free( (charptr) q );
	}
else
	{
	i = (*ip)++;
	cp1 = (Addrp) cpexpr(cp);
	cp1->memoffset = mkexpr(OPPLUS, cp1->memoffset, ICON(i*SZADDR));
	lp1 = (Addrp) cpexpr(lp);
	lp1->memoffset = mkexpr(OPPLUS, lp1->memoffset, ICON(i*SZLENG));
	eleft = mkexpr(OPASSIGN, cp1, addrof(expand(cpexpr(q))));
	eright = mkexpr(OPASSIGN, lp1, cpexpr(q->headblock.vleng));
	frexpr(q);
	}
return (mkexpr(OPCOMMA, eleft, eright));
}



/*
 *  local version of routine putmnmx in putpcc.c, called by expand
 */

LOCAL expptr buffmnmx(p)
register expptr p;
{
int op, type;
expptr qp;
chainp p0, p1;
Addrp sp, tp;
Addrp newtemp;
expptr result, res;

if(p->tag != TEXPR)
	badtag("buffmnmx", p->tag);

type = p->exprblock.vtype;
op = (p->exprblock.opcode==OPMIN ? OPLT : OPGT );
qp = expand(p->exprblock.leftp);
if(qp->tag != TLIST)
	badtag("buffmnmx list", qp->tag);
p0 = qp->listblock.listp;
free( (charptr) qp );
free( (charptr) p );

sp = mktemp(type, PNULL);
tp = mktemp(type, PNULL);
qp = mkexpr(OPCOLON, cpexpr(tp), cpexpr(sp));
qp = mkexpr(OPQUEST, mkexpr(op, cpexpr(tp),cpexpr(sp)), qp);
qp = fixexpr(qp);

newtemp = mktemp (type,PNULL);

result = res = mkexpr (OPCOMMA,
	mkexpr( OPASSIGN, cpexpr(sp), p0->datap ), cpexpr(newtemp));

for(p1 = p0->nextp ; p1 ; p1 = p1->nextp)
	{
	APPEND (mkexpr( OPASSIGN, cpexpr(tp), p1->datap ));
	if(p1->nextp)
		APPEND (mkexpr (OPASSIGN, cpexpr(sp), cpexpr(qp)) );
	else
		APPEND (mkexpr (OPASSIGN, cpexpr(newtemp), qp));
	}

frtemp(sp);
frtemp(tp);
frtemp(newtemp);
frchain( &p0 );

return (result);
}



/*
 * Called by expand() to eliminate exponentiations to integer constants.
 */
LOCAL expptr buffpower( p )
	expptr p;
{
	expptr base;
	Addrp newtemp;
	expptr storetemp = ENULL;
	expptr powtree();
	expptr result;
	ftnint exp;

	if ( ! ISICON( p->exprblock.rightp ) )
		fatal( "buffpower: bad non-integer exponent" );

	base = expand(p->exprblock.leftp);
	exp = p->exprblock.rightp->constblock.constant.ci;
	if ( exp < 2 )
		fatal( "buffpower: bad exponent less than 2" );

	if ( exp > 64 ) {
		/*
		 * Let's be reasonable, here...  Let putpower() do the job.
		 */
		p->exprblock.leftp = base;
		return ( p );
	}

	/*
	 * If the base is not a simple variable, evaluate it and copy the
	 *	result into a temporary.
	 */
	if ( ! (base->tag == TADDR && ISCONST( base->addrblock.memoffset )) ) {
		newtemp = mktemp( base->headblock.vtype, PNULL );
		storetemp = mkexpr( OPASSIGN,
			      cpexpr( (expptr) newtemp ),
			      cpexpr( base ) );
		base = (expptr) newtemp;
	}

	result = powtree( base, exp );

	if ( storetemp != ENULL )
		result = mkexpr( OPCOMMA, storetemp, result );
	frexpr( p );

	return ( result );
}



/*
 * powtree( base, exp ) -- Create a tree of multiplications which computes
 *	base ** exp.  The tree is built so that CSE will compact it if
 *	possible.  The routine works by creating subtrees that compute
 *	exponents which are powers of two, then multiplying these
 *	together to get the result; this gives a log2( exp ) tree depth
 *	and lots of subexpressions which can be eliminated.
 */
LOCAL expptr powtree( base, exp )
	expptr base;
	register ftnint exp;
{
	register expptr r = ENULL, r1;
	register int i;

	for ( i = 0; exp; ++i, exp >>= 1 )
		if ( exp & 1 )
			if ( i == 0 )
				r = (expptr) cpexpr( base );
			else {
				r1 = powtree( base, 1 << (i - 1) );
				r1 = mkexpr( OPSTAR, r1, cpexpr( r1 ) );
				r = (r ? mkexpr( OPSTAR, r1, r ) : r1);
			}

	return ( r );
}
