/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)stat.c	5.1 (Berkeley) %G%";
#endif not lint

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "objfmt.h"
#ifdef PC
#   include <pcc.h>
#   include "pc.h"
#endif PC
#include "tmps.h"

int cntstat;
short cnts = 3;
#include "opcode.h"
#include "tree_ty.h"

/*
 * Statement list
 */
statlist(r)
	struct tnode *r;
{
	register struct tnode *sl;

	for (sl=r; sl != TR_NIL; sl=sl->list_node.next)
		statement(sl->list_node.list);
}

/*
 * Statement
 */
statement(r)
	struct tnode *r;
{
	register struct tnode *tree_node;
	register struct nl *snlp;
	struct tmps soffset;

	tree_node = r;
	snlp = nlp;
	soffset = sizes[cbn].curtmps;
top:
	if (cntstat) {
		cntstat = 0;
		putcnt();
	}
	if (tree_node == TR_NIL)
		return;
	line = tree_node->lined.line_no; 
	if (tree_node->tag == T_LABEL) {
		labeled(tree_node->label_node.lbl_ptr);
		tree_node = tree_node->label_node.stmnt;
		noreach = FALSE;
		cntstat = 1;
		goto top;
	}
	if (noreach) {
		noreach = FALSE;
		warning();
		error("Unreachable statement");
	}
	switch (tree_node->tag) {
		case T_PCALL:
			putline();
#			ifdef OBJ
			    proc(tree_node);
#			endif OBJ
#			ifdef PC
			    pcproc( tree_node );
#			endif PC
			break;
		case T_ASGN:
			putline();
			asgnop(&(tree_node->asg_node));
			break;
		case T_GOTO:
			putline();
			gotoop(tree_node->goto_node.lbl_ptr);
			noreach = TRUE;
			cntstat = 1;
			break;
		default:
			level++;
			switch (tree_node->tag) {
				default:
					panic("stat");
				case T_IF:
				case T_IFEL:
					ifop(&(tree_node->if_node));
					break;
				case T_WHILE:
					whilop(&(tree_node->whi_cas));
					noreach = FALSE;
					break;
				case T_REPEAT:
					repop(&(tree_node->repeat));
					break;
				case T_FORU:
				case T_FORD:
				        forop(tree_node);
					noreach = FALSE;
					break;
				case T_BLOCK:
					statlist(tree_node->stmnt_blck.stmnt_list);
					break;
				case T_CASE:
					putline();
#					ifdef OBJ
					    caseop(&(tree_node->whi_cas));
#					endif OBJ
#					ifdef PC
					    pccaseop(&(tree_node->whi_cas));
#					endif PC
					break;
				case T_WITH:
					withop(&(tree_node->with_node));
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
	tmpfree(&soffset);
}

ungoto()
{
	register struct nl *p;

	for (p = gotos[cbn]; p != NLNIL; p = p->chain)
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

	if (monflg == FALSE) {
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
	    (void) put(2, O_COUNT, counter );
#	endif OBJ
#	ifdef PC
	    putRV( PCPCOUNT , 0 , counter * sizeof (long) , NGLOBAL , PCCT_INT );
	    putleaf( PCC_ICON , 1 , 0 , PCCT_INT , (char *) 0 );
	    putop( PCCOM_ASG PCC_PLUS , PCCT_INT );
	    putdot( filename , line );
#	endif PC
    }

putline()
{

#	ifdef OBJ
	    if (opt('p') != 0)
		    (void) put(2, O_LINO, line);

	    /*
	     * put out line number information for pdx
	     */
	    lineno(line);

#	endif OBJ
#	ifdef PC
	    static lastline;

	    if ( line != lastline ) {
		stabline( line );
		lastline = line;
	    }
	    if ( opt( 'p' ) ) {
		if ( opt('t') ) {
		    putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			    , "_LINO" );
		    putop( PCCOM_UNARY PCC_CALL , PCCT_INT );
		    putdot( filename , line );
		} else {
		    putRV( STMTCOUNT , 0 , 0 , NGLOBAL , PCCT_INT );
		    putleaf( PCC_ICON , 1 , 0 , PCCT_INT , (char *) 0 );
		    putop( PCCOM_ASG PCC_PLUS , PCCT_INT );
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
	WITH_NODE *s;
{
	register struct tnode *p;
	register struct nl *r;
	struct nl	*tempnlp;
	struct nl *swl;

	putline();
	swl = withlist;
		    /*
		     *	no one uses the allocated temporary namelist entry,
		     *	since we have to use it before we know its type;
		     *	but we use its runtime location for the with pointer.
		     */
#		ifdef OBJ
		    (void) put(2, O_LV | cbn <<8+INDX, tempnlp -> value[ NL_OFFS ] );
#		endif OBJ
#		ifdef PC
		    putRV( (char *) 0 , cbn , tempnlp -> value[ NL_OFFS ] ,
			    tempnlp -> extra_flags , PCCTM_PTR|PCCT_STRTY );
#		endif PC
		r = lvalue(p->list_node.list, MOD , LREQ );
		if (r == NLNIL)
			continue;
		if (r->class != RECORD) {
			error("Variable in with statement refers to %s, not to a record", nameof(r));
			continue;
		}
		r = defnl((char *) 0, WITHPTR, r, tempnlp -> value[ NL_OFFS ] );
#		ifdef PC
		    r -> extra_flags |= tempnlp -> extra_flags;
#		endif PC
		r->nl_next = withlist;
		withlist = r;
#		ifdef OBJ
		    (void) put(1, PTR_AS);
#		endif OBJ
#		ifdef PC
		    putop( PCC_ASSIGN , PCCTM_PTR|PCCT_STRTY );
		    putdot( filename , line );
#		endif PC
	}
	statement(s->stmnt);
	withlist = swl;
}

extern	flagwas;
/*
 * var := expr
 */
asgnop(r)
	ASG_NODE *r;
{
	register struct nl *p;
	register struct tnode *av;

	/*
	 * Asgnop's only function is
	 * to handle function variable
	 * assignments.  All other assignment
	 * stuff is handled by asgnop1.
	 * the if below checks for unqualified lefthandside:
	 * necessary for fvars.
	 */
	av = r->lhs_var;
	if (av != TR_NIL && av->tag == T_VAR && av->var_node.qual == TR_NIL) {
		p = lookup1(av->var_node.cptr);
		if (p != NLNIL)
			p->nl_flags = flagwas;
		if (p != NLNIL && p->class == FVAR) {
			/*
			 * Give asgnop1 the func
			 * which is the chain of
			 * the FVAR.
			 */
			p->nl_flags |= NUSED|NMOD;
			p = p->chain;
			if (p == NLNIL) {
				p = rvalue(r->rhs_expr, NLNIL , RREQ );
				return;
			}
#			ifdef OBJ
			    (void) put(2, O_LV | bn << 8+INDX, (int)p->value[NL_OFFS]);
			    if (isa(p->type, "i") && width(p->type) == 1)
				    (void) asgnop1(r, nl+T2INT);
			    else
				    (void) asgnop1(r, p->type);
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
			    putRV( p -> symbol , bn , p -> value[ NL_OFFS ] ,
				    p -> extra_flags , p2type( p -> type ) );
			    (void) asgnop1( r , p -> type );
#			endif PC
			return;
		}
	}
	(void) asgnop1(r, NLNIL);
}

/*
 * Asgnop1 handles all assignments.
 * If p is not nil then we are assigning
 * to a function variable, otherwise
 * we look the variable up ourselves.
 */
struct nl *
asgnop1(r, p)
	ASG_NODE *r;
	register struct nl *p;
{
	register struct nl *p1;
	int	clas;
#ifdef OBJ
	int w;
#endif OBJ

#ifdef OBJ
	if (p == NLNIL) {
	    p = lvalue(r->lhs_var, MOD|ASGN|NOUSE , LREQ );
	    if ( p == NLNIL ) {
		(void) rvalue( r->rhs_expr , NLNIL , RREQ );
		return NLNIL;
	    }
	    w = width(p);
	} else {
	    /*
	     * assigning to the return value, which is at least
	     * of width two since it resides on the stack
	     */
	    w = width(p);
	    if (w < 2)
		w = 2;
	}
	clas = classify(p);
	if ((clas == TARY || clas == TSTR) && p->chain->class == CRANGE) {
	    p1 = lvalue(r->rhs_expr, p , LREQ ); /* SHOULD THIS BE rvalue? */
	} else {
	    p1 = rvalue(r->rhs_expr, p , RREQ );
	}
#   endif OBJ
#   ifdef PC
	if (p == NLNIL) {
	    /* check for conformant array type */
	    codeoff();
	    p = rvalue(r->lhs_var, MOD|ASGN|NOUSE, LREQ);
	    codeon();
	    if (p == NLNIL) {
		(void) rvalue(r->rhs_expr, NLNIL, RREQ);
		return NLNIL;
	    }
	    clas = classify(p);
	    if ((clas == TARY || clas == TSTR) && p->chain->class == CRANGE) {
		return pcasgconf(r, p);
	    } else {
		/*
		 * since the second pass knows that it should reference
		 * the lefthandside of asignments, what i need here is
		 * an rvalue.
		 */
		p = lvalue( r->lhs_var , MOD|ASGN|NOUSE , RREQ );
	    }
	    if ( p == NLNIL ) {
		(void) rvalue( r->rhs_expr , NLNIL , RREQ );
		return NLNIL;
	    }
	}
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
		/* and fall through */
	    case TDOUBLE:
	    case TPTR:
		p1 = rvalue( r->rhs_expr , p , RREQ );
		break;
	    default:
		p1 = rvalue( r->rhs_expr , p , LREQ );
		break;
	}
#	endif PC
	if (p1 == NLNIL)
		return (NLNIL);
	if (incompat(p1, p, r->rhs_expr)) {
		cerror("Type of expression clashed with type of variable in assignment");
		return (NLNIL);
	}
#	ifdef OBJ
	    switch (classify(p)) {
		    case TINT:
		    case TBOOL:
		    case TCHAR:
		    case TSCAL:
			    rangechk(p, p1);
			    (void) gen(O_AS2, O_AS2, w, width(p1));
			    break;
		    case TDOUBLE:
		    case TPTR:
			    (void) gen(O_AS2, O_AS2, w, width(p1));
			    break;
		    case TARY:
		    case TSTR:
			    if (p->chain->class == CRANGE) {
				/* conformant array assignment */
				p1 = p->chain;
				w = width(p1->type);
				putcbnds(p1, 1);
				putcbnds(p1, 0);
				gen(NIL, T_SUB, w, w);
				put(2, w > 2? O_CON24: O_CON2, 1);
				gen(NIL, T_ADD, w, w);
				putcbnds(p1, 2);
				gen(NIL, T_MULT, w, w);
				put(1, O_VAS);
				break;
			    }
			    /* else fall through */
		    default:
			    (void) put(2, O_AS, w);
			    break;
	    }
#	endif OBJ
#	ifdef PC
	    switch (classify(p)) {
		    case TINT:
		    case TBOOL:
		    case TCHAR:
		    case TSCAL:
			    postcheck(p, p1);
			    sconv(p2type(p1), p2type(p));
			    putop( PCC_ASSIGN , p2type( p ) );
			    putdot( filename , line );
			    break;
		    case TPTR:
			    putop( PCC_ASSIGN , p2type( p ) );
			    putdot( filename , line );
			    break;
		    case TDOUBLE:
			    sconv(p2type(p1), p2type(p));
			    putop( PCC_ASSIGN , p2type( p ) );
			    putdot( filename , line );
			    break;
		    default:
			    putstrop(PCC_STASG, PCCM_ADDTYPE(p2type(p), PCCTM_PTR),
					(int) lwidth(p), align(p));
			    putdot( filename , line );
			    break;
	    }
#	endif PC
	return (p);	/* Used by for statement */
}

#ifdef PC
/*
 * assignment to conformant arrays.  Since these are variable length,
 *	we use blkcpy() to perform the assignment.
 *	blkcpy(rhs, lhs, (upper - lower + 1) * width)
 */
struct nl *
pcasgconf(r, p)
	register ASG_NODE *r;
	struct nl *p;
{
	struct nl *p1;

	if (r == (ASG_NODE *) TR_NIL || p == NLNIL)
		return NLNIL;
	putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR) , "_blkcpy" );
	p1 = rvalue( r->rhs_expr , p , LREQ );
	if (p1 == NLNIL)
		return NLNIL;
	p = lvalue( r->lhs_var , MOD|ASGN|NOUSE , LREQ );
	if (p == NLNIL)
		return NLNIL;
	putop(PCC_CM, PCCT_INT);
		/* upper bound */
	p1 = p->chain->nptr[1];
	putRV(p1->symbol, (p1->nl_block & 037), p1->value[0],
	    p1->extra_flags, p2type( p1 ) );
		/* minus lower bound */
	p1 = p->chain->nptr[0];
	putRV(p1->symbol, (p1->nl_block & 037), p1->value[0],
	    p1->extra_flags, p2type( p1 ) );
	putop( PCC_MINUS, PCCT_INT );
		/* add one */
	putleaf(PCC_ICON, 1, 0, PCCT_INT, 0);
	putop( PCC_PLUS, PCCT_INT );
		/* and multiply by the width */
	p1 = p->chain->nptr[2];
	putRV(p1->symbol, (p1->nl_block & 037), p1->value[0],
	    p1->extra_flags, p2type( p1 ) );
	putop( PCC_MUL , PCCT_INT );
	putop(PCC_CM, PCCT_INT);
	putop(PCC_CALL, PCCT_INT);
	putdot( filename , line);
	return p;
}
#endif PC

/*
 * if expr then stat [ else stat ]
 */
ifop(if_n)
	IF_NODE *if_n;
{
	register struct nl *p;
	register l1, l2;	/* l1 is start of else, l2 is end of else */
	int goc;
	bool nr;

	goc = gocnt;
	putline();
	p = rvalue(if_n->cond_expr, NLNIL , RREQ );
	if (p == NIL) {
		statement(if_n->then_stmnt);
		noreach = FALSE;
		statement(if_n->else_stmnt);
		noreach = FALSE;
		return;
	}
	if (isnta(p, "b")) {
		error("Type of expression in if statement must be Boolean, not %s", nameof(p));
		statement(if_n->then_stmnt);
		noreach = FALSE;
		statement(if_n->else_stmnt);
		noreach = FALSE;
		return;
	}
#	ifdef OBJ
	    l1 = put(2, O_IF, getlab());
#	endif OBJ
#	ifdef PC
	    l1 = (int) getlab();
	    putleaf( PCC_ICON , l1 , 0 , PCCT_INT , (char *) 0 );
	    putop( PCC_CBRANCH , PCCT_INT );
	    putdot( filename , line );
#	endif PC
	putcnt();
	statement(if_n->then_stmnt);
	nr = noreach;
	if (if_n->else_stmnt != TR_NIL) {
		/*
		 * else stat
		 */
		--level;
		ungoto();
		++level;
#		ifdef OBJ
		    l2 = put(2, O_TRA, getlab());
#		endif OBJ
#		ifdef PC
		    l2 = (int) getlab();
		    putjbr( (long) l2 );
#		endif PC
		patch((PTR_DCL)l1);
		noreach = FALSE;
		statement(if_n->else_stmnt);
		noreach = (noreach && nr)?TRUE:FALSE;
		l1 = l2;
	} else
		noreach = FALSE;
	patch((PTR_DCL)l1);
	if (goc != gocnt)
		putcnt();
}

/*
 * while expr do stat
 */
whilop(w_node)
	WHI_CAS *w_node;
{
	register struct nl *p;
	register char *l1, *l2;
	int goc;

	goc = gocnt;
	l1 = getlab();
	(void) putlab(l1);
	putline();
	p = rvalue(w_node->expr, NLNIL , RREQ );
	if (p == NLNIL) {
		statement(w_node->stmnt_list);
		noreach = FALSE;
		return;
	}
	if (isnta(p, "b")) {
		error("Type of expression in while statement must be Boolean, not %s", nameof(p));
		statement(w_node->stmnt_list);
		noreach = FALSE;
		return;
	}
	l2 = getlab();
#	ifdef OBJ
	    (void) put(2, O_IF, l2);
#	endif OBJ
#	ifdef PC
	    putleaf( PCC_ICON , (int) l2 , 0 , PCCT_INT , (char *) 0 );
	    putop( PCC_CBRANCH , PCCT_INT );
	    putdot( filename , line );
#	endif PC
	putcnt();
	statement(w_node->stmnt_list);
#	ifdef OBJ
	    (void) put(2, O_TRA, l1);
#	endif OBJ
#	ifdef PC
	    putjbr( (long) l1 );
#	endif PC
	patch((PTR_DCL) l2);
	if (goc != gocnt)
		putcnt();
}

/*
 * repeat stat* until expr
 */
repop(r)
	REPEAT *r;
{
	register struct nl *p;
	register l;
	int goc;

	goc = gocnt;
	l = (int) putlab(getlab());
	putcnt();
	statlist(r->stmnt_list);
	line = r->line_no;
	p = rvalue(r->term_expr, NLNIL , RREQ );
	if (p == NLNIL)
		return;
	if (isnta(p,"b")) {
		error("Until expression type must be Boolean, not %s, in repeat statement", nameof(p));
		return;
	}
#	ifdef OBJ
	    (void) put(2, O_IF, l);
#	endif OBJ
#	ifdef PC
	    putleaf( PCC_ICON , l , 0 , PCCT_INT , (char *) 0 );
	    putop( PCC_CBRANCH , PCCT_INT );
	    putdot( filename , line );
#	endif PC
	if (goc != gocnt)
		putcnt();
}
