/* %W% (Berkeley) %G% */
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
	putopt(sp);
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
	p2flush();
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
		puteq (sp->expr, mkaddcon(sp->label));
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
Addrp buffmnmx(), buffpower();

if (!p)
	return (ENULL);
switch (p->tag)
	{
	case TEXPR:
		p->exprblock.leftp = expand (p->exprblock.leftp);
		if (p->exprblock.rightp)
			p->exprblock.rightp = expand (p->exprblock.rightp);
		switch (p->exprblock.opcode)
			{
			expptr temp;
			case OPCONCAT:
				t = mktemp (TYCHAR, ICON(lencat(p)));
				q = (expptr) cpexpr (p->exprblock.vleng);
				buffcat (cpexpr(t),p);
				frexpr (t->vleng);
				t->vleng = q;
				p = (tagptr) t;
				break;
			case OPMIN:
			case OPMAX:
				p = (tagptr) buffmnmx (p);
				break;
			case OPPOWER:
				p = (tagptr) buffpower (p);
				break;
			default:
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

	default:
		break;
	}
return ((expptr) p);
}



/*
 *  local version of routine putcat in putpcc.c, called by expand
 */

LOCAL buffcat(lhs, rhs)
register Addrp lhs;
register expptr rhs;
{
int n;
Addrp lp, cp;

n = ncat(rhs);
lp = (Addrp) mkaltmpn(n, TYLENG, PNULL);
cp = (Addrp) mkaltmpn(n, TYADDR, PNULL);

n = 0;
buffct1(rhs, lp, cp, &n);

optbuff (SKCALL, call4(TYSUBR, "s_cat", lhs, cp, lp, mkconv(TYLONG, ICON(n))),
	0, 0);
}



/*
 *  local version of routine putct1 in putpcc.c, called by expand
 */

LOCAL buffct1(q, lp, cp, ip)
register expptr q;
register Addrp lp, cp;
int *ip;
{
int i;
Addrp lp1, cp1;

if(q->tag==TEXPR && q->exprblock.opcode==OPCONCAT)
	{
	buffct1(q->exprblock.leftp, lp, cp, ip);
	buffct1(q->exprblock.rightp, lp, cp, ip);
	frexpr(q->exprblock.vleng);
	free( (charptr) q );
	}
else
	{
	i = (*ip)++;
	lp1 = (Addrp) cpexpr(lp);
	lp1->memoffset = mkexpr(OPPLUS,lp1->memoffset, ICON(i*SZLENG));
	cp1 = (Addrp) cpexpr(cp);
	cp1->memoffset = mkexpr(OPPLUS, cp1->memoffset, ICON(i*SZADDR));
	optbuff (SKEQ, (mkexpr(OPASSIGN, lp1, cpexpr(q->headblock.vleng))),
		0,0);
	optbuff (SKEQ, (mkexpr(OPASSIGN, cp1, addrof(expand (q)))), 0, 0);
	}
}



/*
 *  local version of routine putmnmx in putpcc.c, called by expand
 */

LOCAL Addrp buffmnmx(p)
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
p0 = p->exprblock.leftp->listblock.listp;
free( (charptr) (p->exprblock.leftp) );
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

return ( (Addrp) result);
}



/*
 *  Local version of putpower routine from putpcc.c, used by expand().
 */

LOCAL Addrp buffpower (p)
expptr p;
{
expptr base;
Addrp t1, t2;
ftnint k;
int type;
Addrp newtemp;
expptr res,result;

if(!ISICON(p->exprblock.rightp) ||
    (k = p->exprblock.rightp->constblock.const.ci)<2)
	fatal("buffpower: bad call");
base = p->exprblock.leftp;

if (k == 2)
{
	expptr prod;
	prod = mkexpr (OPSTAR,cpexpr(base),cpexpr(base));
	return ( (Addrp) prod);
}

type = base->headblock.vtype;
newtemp = mktemp (type, PNULL);

t1 = mktemp(type, PNULL);
t2 = NULL;
result = res =
	mkexpr (OPCOMMA, mkexpr (OPASSIGN,cpexpr(t1),cpexpr(base)),
	cpexpr (newtemp));

for( ; (k&1)==0 && k>2 ; k>>=1 )
	APPEND (mkexpr (OPSTAREQ,cpexpr(t1),cpexpr(t1)));

if(k == 2)
	{
	expptr prod;
	prod = mkexpr(OPSTAR, cpexpr(t1), cpexpr(t1));
	APPEND (mkexpr (OPASSIGN,cpexpr(newtemp),cpexpr(prod)));
	}
else
	{
	t2 = mktemp(type, PNULL);
	APPEND (mkexpr (OPASSIGN,cpexpr(t2),cpexpr(t1)));

	for(k>>=1 ; k>1 ; k>>=1)
		{
		APPEND (mkexpr (OPSTAREQ,cpexpr(t1),cpexpr(t1)));
		if(k & 1)
			APPEND (mkexpr (OPSTAREQ,cpexpr(t2),cpexpr(t1)));
		}
	APPEND (mkexpr (OPASSIGN, cpexpr(newtemp), mkexpr(OPSTAR, cpexpr(t2),
			mkexpr(OPSTAR, cpexpr(t1), cpexpr(t1)) )));
	}
frexpr(t1);
if(t2)
	frexpr(t2);
frexpr(p);

return ( (Addrp) result);
}
