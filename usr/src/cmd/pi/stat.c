/* Copyright (c) 1979 Regents of the University of California */

static	char sccsid[] = "@(#)stat.c 1.1 8/27/80";

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "objfmt.h"
#ifdef PC
#   include "pcops.h"
#   include "pc.h"
#endif PC

int cntstat;
short cnts = 3;
#include "opcode.h"

/*
 * Statement list
 */
statlist(r)
	int *r;
{
	register *sl;

	for (sl=r; sl != NIL; sl=sl[2])
		statement(sl[1]);
}

/*
 * Statement
 */
statement(r)
	int *r;
{
	register *s;
	register struct nl *snlp;
	long	soffset;

	s = r;
	snlp = nlp;
	soffset = sizes[ cbn ].om_off;
top:
	if (cntstat) {
		cntstat = 0;
		putcnt();
	}
	if (s == NIL)
		return;
	line = s[1];
	if (s[0] == T_LABEL) {
		labeled(s[2]);
		s = s[3];
		noreach = 0;
		cntstat = 1;
		goto top;
	}
	if (noreach) {
		noreach = 0;
		warning();
		error("Unreachable statement");
	}
	switch (s[0]) {
		case T_PCALL:
			putline();
#			ifdef OBJ
			    proc(s);
#			endif OBJ
#			ifdef PC
			    pcproc( s );
#			endif PC
			break;
		case T_ASGN:
			putline();
			asgnop(s);
			break;
		case T_GOTO:
			putline();
			gotoop(s[2]);
			noreach = 1;
			cntstat = 1;
			break;
		default:
			level++;
			switch (s[0]) {
				default:
					panic("stat");
				case T_IF:
				case T_IFEL:
					ifop(s);
					break;
				case T_WHILE:
					whilop(s);
					noreach = 0;
					break;
				case T_REPEAT:
					repop(s);
					break;
				case T_FORU:
				case T_FORD:
#					ifdef OBJ
					    forop(s);
#					endif OBJ
#					ifdef PC
					    pcforop( s );
#					endif PC
					noreach = 0;
					break;
				case T_BLOCK:
					statlist(s[2]);
					break;
				case T_CASE:
					putline();
#					ifdef OBJ
					    caseop(s);
#					endif OBJ
#					ifdef PC
					    pccaseop( s );
#					endif PC
					break;
				case T_WITH:
					withop(s);
					break;
				case T_ASRT:
					putline();
					asrtop(s);
					break;
			}
			--level;
			if (gotos[cbn])
				ungoto();
			break;
	}
	/*
	 * Free the temporary name list entries defined in
	 * expressions, e.g. STRs, and WITHPTRs from withs.
	 */
	nlfree(snlp);
	    /*
	     *	free any temporaries allocated for this statement
	     *	these come from strings and sets.
	     */
	if ( soffset != sizes[ cbn ].om_off ) {
	    sizes[ cbn ].om_off = soffset;
#	    ifdef PC
		putlbracket( ftnno , -sizes[cbn].om_off );
#	    endif PC
	}
}

ungoto()
{
	register struct nl *p;

	for (p = gotos[cbn]; p != NIL; p = p->chain)
		if ((p->nl_flags & NFORWD) != 0) {
			if (p->value[NL_GOLEV] != NOTYET)
				if (p->value[NL_GOLEV] > level)
					p->value[NL_GOLEV] = level;
		} else
			if (p->value[NL_GOLEV] != DEAD)
				if (p->value[NL_GOLEV] > level)
					p->value[NL_GOLEV] = DEAD;
}

putcnt()
{

	if (monflg == 0) {
		return;
	}
	inccnt( getcnt() );
}

int
getcnt()
    {
	
	return ++cnts;
    }

inccnt( counter )
    int	counter;
    {

#	ifdef OBJ
	    put2(O_COUNT, counter );
#	endif OBJ
#	ifdef PC
	    putRV( PCPCOUNT , 0 , counter * sizeof (long) , P2INT );
	    putleaf( P2ICON , 1 , 0 , P2INT , 0 );
	    putop( P2ASG P2PLUS , P2INT );
	    putdot( filename , line );
#	endif PC
    }

putline()
{

#	ifdef OBJ
	    if (opt('p') != 0)
		    put2(O_LINO, line);
#	endif OBJ
#	ifdef PC
	    static lastline;

	    if ( line != lastline ) {
		stabline( line );
		lastline = line;
	    }
	    if ( opt( 'p' ) ) {
		if ( opt('t') ) {
		    putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
			    , "_LINO" );
		    putop( P2UNARY P2CALL , P2INT );
		    putdot( filename , line );
		} else {
		    putRV( STMTCOUNT , 0 , 0 , P2INT );
		    putleaf( P2ICON , 1 , 0 , P2INT , 0 );
		    putop( P2ASG P2PLUS , P2INT );
		    putdot( filename , line );
		}
	    }
#	endif PC
}

/*
 * With varlist do stat
 *
 * With statement requires an extra word
 * in automatic storage for each level of withing.
 * These indirect pointers are initialized here, and
 * the scoping effect of the with statement occurs
 * because lookup examines the field names of the records
 * associated with the WITHPTRs on the withlist.
 */
withop(s)
	int *s;
{
	register *p;
	register struct nl *r;
	int i;
	int *swl;
	long soffset;

	putline();
	swl = withlist;
	soffset = sizes[cbn].om_off;
	for (p = s[2]; p != NIL; p = p[2]) {
		i = sizes[cbn].om_off -= sizeof ( int * );
		if (sizes[cbn].om_off < sizes[cbn].om_max)
			sizes[cbn].om_max = sizes[cbn].om_off;
#		ifdef OBJ
		    put2(O_LV | cbn <<8+INDX, i );
#		endif OBJ
#		ifdef PC
		    putlbracket( ftnno , -sizes[cbn].om_off );
		    putRV( 0 , cbn , i , P2PTR|P2STRTY );
#		endif PC
		r = lvalue(p[1], MOD , LREQ );
		if (r == NIL)
			continue;
		if (r->class != RECORD) {
			error("Variable in with statement refers to %s, not to a record", nameof(r));
			continue;
		}
		r = defnl(0, WITHPTR, r, i);
		r->nl_next = withlist;
		withlist = r;
#		ifdef OBJ
		    put(1, PTR_AS);
#		endif OBJ
#		ifdef PC
		    putop( P2ASSIGN , P2PTR|P2STRTY );
		    putdot( filename , line );
#		endif PC
	}
	statement(s[3]);
	sizes[cbn].om_off = soffset;
#	ifdef PC
	    putlbracket( ftnno , -sizes[cbn].om_off );
#	endif PC
	withlist = swl;
}

extern	flagwas;
/*
 * var := expr
 */
asgnop(r)
	int *r;
{
	register struct nl *p;
	register *av;

	if (r == NIL)
		return (NIL);
	/*
	 * Asgnop's only function is
	 * to handle function variable
	 * assignments.  All other assignment
	 * stuff is handled by asgnop1.
	 * the if below checks for unqualified lefthandside:
	 * necessary for fvars.
	 */
	av = r[2];
	if (av != NIL && av[0] == T_VAR && av[3] == NIL) {
		p = lookup1(av[2]);
		if (p != NIL)
			p->nl_flags = flagwas;
		if (p != NIL && p->class == FVAR) {
			/*
			 * Give asgnop1 the func
			 * which is the chain of
			 * the FVAR.
			 */
			p->nl_flags |= NUSED|NMOD;
			p = p->chain;
			if (p == NIL) {
				rvalue(r[3], NIL , RREQ );
				return;
			}
#			ifdef OBJ
			    put2(O_LV | bn << 8+INDX, p->value[NL_OFFS]);
			    if (isa(p->type, "i") && width(p->type) == 1)
				    asgnop1(r, nl+T2INT);
			    else
				    asgnop1(r, p->type);
#			endif OBJ
#			ifdef PC
				/*
				 * this should be the lvalue of the fvar,
				 * but since the second pass knows to use
				 * the address of the left operand of an
				 * assignment, what i want here is an rvalue.
				 * see note in funchdr about fvar allocation.
				 */
			    p = p -> ptr[ NL_FVAR ];
			    putRV( p -> symbol , bn , p -> value[ NL_OFFS ]
					, p2type( p -> type ) );
			    asgnop1( r , p -> type );
#			endif PC
			return;
		}
	}
	asgnop1(r, NIL);
}

/*
 * Asgnop1 handles all assignments.
 * If p is not nil then we are assigning
 * to a function variable, otherwise
 * we look the variable up ourselves.
 */
struct nl *
asgnop1(r, p)
	int *r;
	register struct nl *p;
{
	register struct nl *p1;

	if (r == NIL)
		return (NIL);
	if (p == NIL) {
#	    ifdef OBJ
		p = lvalue(r[2], MOD|ASGN|NOUSE , LREQ );
#	    endif OBJ
#	    ifdef PC
		    /*
		     * since the second pass knows that it should reference
		     * the lefthandside of asignments, what i need here is
		     * an rvalue.
		     */
		p = lvalue( r[2] , MOD|ASGN|NOUSE , RREQ );
#	    endif PC
	    if ( p == NIL ) {
		rvalue( r[3] , NIL , RREQ );
		return NIL;
	    }
	}
#	ifdef OBJ
	    p1 = rvalue(r[3], p , RREQ );
#	endif OBJ
#	ifdef PC
		/*
		 *	if this is a scalar assignment,
		 *	    then i want to rvalue the righthandside.
		 *	if this is a structure assignment,
		 *	    then i want an lvalue to the righthandside.
		 *  that's what the intermediate form sez.
		 */
	    switch ( classify( p ) ) {
		case TINT:
		case TCHAR:
		case TBOOL:
		case TSCAL:
		    precheck( p , "_RANG4" , "_RSNG4" );
		case TDOUBLE:
		case TPTR:
		    p1 = rvalue( r[3] , p , RREQ );
		    break;
		default:
		    p1 = rvalue( r[3] , p , LREQ );
		    break;
	    }
#	endif PC
	if (p1 == NIL)
		return (NIL);
	if (incompat(p1, p, r[3])) {
		cerror("Type of expression clashed with type of variable in assignment");
		return (NIL);
	}
	switch (classify(p)) {
		case TINT:
		case TBOOL:
		case TCHAR:
		case TSCAL:
#			ifdef OBJ
			    rangechk(p, p1);
#			endif OBJ
#			ifdef PC
			    postcheck( p );
#			endif PC
		case TDOUBLE:
		case TPTR:
#			ifdef OBJ
			    gen(O_AS2, O_AS2, width(p), width(p1));
#			endif OBJ
#			ifdef PC
			    putop( P2ASSIGN , p2type( p ) );
			    putdot( filename , line );
#			endif PC
			break;
		default:
#			ifdef OBJ
			    put2(O_AS, width(p));
#			endif OBJ
#			ifdef PC
			    putstrop( P2STASG , p2type( p )
					, lwidth( p ) , align( p ) );
			    putdot( filename , line );
#			endif PC
	}
	return (p);	/* Used by for statement */
}

#ifdef OBJ
/*
 * for var := expr [down]to expr do stat
 */
forop(r)
	int *r;
{
	register struct nl *t1, *t2;
	int l1, l2, l3;
	long soffset;
	register op;
	struct nl *p;
	int *rr, goc, i;

	p = NIL;
	goc = gocnt;
	if (r == NIL)
		goto aloha;
	putline();
	/*
	 * Start with assignment
	 * of initial value to for variable
	 */
	t1 = asgnop1(r[2], NIL);
	if (t1 == NIL) {
		rvalue(r[3], NIL , RREQ );
		statement(r[4]);
		goto aloha;
	}
	rr = r[2];		/* Assignment */
	rr = rr[2];		/* Lhs variable */
	if (rr[3] != NIL) {
		error("For variable must be unqualified");
		rvalue(r[3], NIL , RREQ );
		statement(r[4]);
		goto aloha;
	}
	p = lookup(rr[2]);
	p->value[NL_FORV] = 1;
	if (isnta(t1, "bcis")) {
		error("For variables cannot be %ss", nameof(t1));
		statement(r[4]);
		goto aloha;
	}
	/*
	 * Allocate automatic
	 * space for limit variable
	 */
	sizes[cbn].om_off -= 4;
	if (sizes[cbn].om_off < sizes[cbn].om_max)
		sizes[cbn].om_max = sizes[cbn].om_off;
	i = sizes[cbn].om_off;
	/*
	 * Initialize the limit variable
	 */
	put2(O_LV | cbn<<8+INDX, i);
	t2 = rvalue(r[3], NIL , RREQ );
	if (incompat(t2, t1, r[3])) {
		cerror("Limit type clashed with index type in 'for' statement");
		statement(r[4]);
		goto aloha;
	}
	put1(width(t2) <= 2 ? O_AS24 : O_AS4);
	/*
	 * See if we can skip the loop altogether
	 */
	rr = r[2];
	if (rr != NIL)
		rvalue(rr[2], NIL , RREQ );
	put2(O_RV4 | cbn<<8+INDX, i);
	gen(NIL, r[0] == T_FORU ? T_LE : T_GE, width(t1), 4);
	/*
	 * L1 will be patched to skip the body of the loop.
	 * L2 marks the top of the loop when we go around.
	 */
	put2(O_IF, (l1 = getlab()));
	putlab(l2 = getlab());
	putcnt();
	statement(r[4]);
	/*
	 * now we see if we get to go again
	 */
	if (opt('t') == 0) {
		/*
		 * Easy if we dont have to test
		 */
		put2(O_RV4 | cbn<<8+INDX, i);
		if (rr != NIL)
			lvalue(rr[2], MOD , RREQ );
		put2((r[0] == T_FORU ? O_FOR1U : O_FOR1D) + (width(t1) >> 1), l2);
	} else {
		line = r[1];
		putline();
		if (rr != NIL)
			rvalue(rr[2], NIL , RREQ );
		put2(O_RV4 | cbn << 8+INDX, i);
		gen(NIL, (r[0] == T_FORU ? T_LT : T_GT), width(t1), 4);
		l3 = put2(O_IF, getlab());
		lvalue((int *) rr[2], MOD , RREQ );
		rvalue(rr[2], NIL , RREQ );
		put2(O_CON2, 1);
		t2 = gen(NIL, r[0] == T_FORU ? T_ADD: T_SUB, width(t1), 2);
		rangechk(t1, t2);	/* The point of all this */
		gen(O_AS2, O_AS2, width(t1), width(t2));
		put2(O_TRA, l2);
		patch(l3);
	}
	sizes[cbn].om_off += 4;
	patch(l1);
aloha:
	noreach = 0;
	if (p != NIL)
		p->value[NL_FORV] = 0;
	if (goc != gocnt)
		putcnt();
}
#endif OBJ

/*
 * if expr then stat [ else stat ]
 */
ifop(r)
	int *r;
{
	register struct nl *p;
	register l1, l2;	/* l1 is start of else, l2 is end of else */
	int nr, goc;

	goc = gocnt;
	if (r == NIL)
		return;
	putline();
	p = rvalue(r[2], NIL , RREQ );
	if (p == NIL) {
		statement(r[3]);
		noreach = 0;
		statement(r[4]);
		noreach = 0;
		return;
	}
	if (isnta(p, "b")) {
		error("Type of expression in if statement must be Boolean, not %s", nameof(p));
		statement(r[3]);
		noreach = 0;
		statement(r[4]);
		noreach = 0;
		return;
	}
#	ifdef OBJ
	    l1 = put2(O_IF, getlab());
#	endif OBJ
#	ifdef PC
	    l1 = getlab();
	    putleaf( P2ICON , l1 , 0 , P2INT , 0 );
	    putop( P2CBRANCH , P2INT );
	    putdot( filename , line );
#	endif PC
	putcnt();
	statement(r[3]);
	nr = noreach;
	if (r[4] != NIL) {
		/*
		 * else stat
		 */
		--level;
		ungoto();
		++level;
#		ifdef OBJ
		    l2 = put2(O_TRA, getlab());
#		endif OBJ
#		ifdef PC
		    l2 = getlab();
		    putjbr( l2 );
#		endif PC
		patch(l1);
		noreach = 0;
		statement(r[4]);
		noreach &= nr;
		l1 = l2;
	} else
		noreach = 0;
	patch(l1);
	if (goc != gocnt)
		putcnt();
}

/*
 * while expr do stat
 */
whilop(r)
	int *r;
{
	register struct nl *p;
	register l1, l2;
	int goc;

	goc = gocnt;
	if (r == NIL)
		return;
	putlab(l1 = getlab());
	putline();
	p = rvalue(r[2], NIL , RREQ );
	if (p == NIL) {
		statement(r[3]);
		noreach = 0;
		return;
	}
	if (isnta(p, "b")) {
		error("Type of expression in while statement must be Boolean, not %s", nameof(p));
		statement(r[3]);
		noreach = 0;
		return;
	}
	l2 = getlab();
#	ifdef OBJ
	    put2(O_IF, l2);
#	endif OBJ
#	ifdef PC
	    putleaf( P2ICON , l2 , 0 , P2INT , 0 );
	    putop( P2CBRANCH , P2INT );
	    putdot( filename , line );
#	endif PC
	putcnt();
	statement(r[3]);
#	ifdef OBJ
	    put2(O_TRA, l1);
#	endif OBJ
#	ifdef PC
	    putjbr( l1 );
#	endif PC
	patch(l2);
	if (goc != gocnt)
		putcnt();
}

/*
 * repeat stat* until expr
 */
repop(r)
	int *r;
{
	register struct nl *p;
	register l;
	int goc;

	goc = gocnt;
	if (r == NIL)
		return;
	l = putlab(getlab());
	putcnt();
	statlist(r[2]);
	line = r[1];
	p = rvalue(r[3], NIL , RREQ );
	if (p == NIL)
		return;
	if (isnta(p,"b")) {
		error("Until expression type must be Boolean, not %s, in repeat statement", nameof(p));
		return;
	}
#	ifdef OBJ
	    put2(O_IF, l);
#	endif OBJ
#	ifdef PC
	    putleaf( P2ICON , l , 0 , P2INT , 0 );
	    putop( P2CBRANCH , P2INT );
	    putdot( filename , line );
#	endif PC
	if (goc != gocnt)
		putcnt();
}

/*
 * assert expr
 */
asrtop(r)
	register int *r;
{
	register struct nl *q;

	if (opt('s')) {
		standard();
		error("Assert statement is non-standard");
	}
	if (!opt('t'))
		return;
	r = r[2];
#	ifdef OBJ
	    q = rvalue((int *) r, NLNIL , RREQ );
#	endif OBJ
#	ifdef PC
	    putleaf( P2ICON , 0 , 0
		    , ADDTYPE( P2FTN | P2INT , P2PTR ) , "_ASRT" );
	    q = stkrval( r , NLNIL , RREQ );
#	endif PC
	if (q == NIL)
		return;
	if (isnta(q, "b"))
		error("Assert expression must be Boolean, not %ss", nameof(q));
#	ifdef OBJ
	    put1(O_ASRT);
#	endif OBJ
#	ifdef PC
	    putop( P2CALL , P2INT );
	    putdot( filename , line );
#	endif PC
}
