/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)parse.c	5.1 (Berkeley) %G%";
#endif not lint

/*

			  Copyright (C) 1976
				by the
			  Board of Trustees
				of the
			University of Illinois

			 All rights reserved


FILE NAME:
	parse.c

PURPOSE:
	Contains the routines which keep track of the parse stack.

GLOBALS:
	p_stack =	The parse stack, set by both routines
	il =		Stack of indentation levels, set by parse
	cstk =		Stack of case statement indentation levels, set by parse
	tos =		Pointer to top of stack, set by both routines.

FUNCTIONS:
	parse
	reduce
*/
/*

			  Copyright (C) 1976
				by the
			  Board of Trustees
				of the
			University of Illinois

			 All rights reserved


NAME:
	parse

FUNCTION:
	Parse is given one input which is a "maxi token" just scanned from
	input.  Maxi tokens are signifigant constructs such as else, {, do,
	if (...), etc.  Parse works with reduce to maintain a parse stack
	of these constructs.  Parse is responsible for the "shift" portion
	of the parse algorithm, and reduce handles the "reduce" portion.

ALGORITHM:
	1) If there is "ifstmt" on the stack and input is anything other than
	   an else, then change the top of stack (TOS) to <stmt>.  Do a reduce.
	2) Use a switch statement to implement the following shift operations:

	   TOS___		Input_____		Stack_____		Note____
	decl		decl		nothing
	anything else	decl		decl
	"dostmt"	while (..)			Change TOS to <stmt>
	anything else	while (..)	while
	"ifstmt"	else				Change TOS to "ifelse"
	{ <stmtl>	}				Change { <stmtl> 
								to <stmtl>
			switch (..)	switch
			do		do
			for(..)		for
			;		<stmt>
			{		{ <stmt>

PARAMETERS:
	tk	An integer code for the maxi token scanned

RETURNS:
	Nothing

GLOBALS:
	break_comma =	Set to true when in a declaration but not initialization
	btype_2
	case_ind =
	cstk =
	i_l_follow =
	il =		Stack of indentation levels
	ind_level =
	p_stack =	Stack of token codes
	search_brace =	Set to true if we must look for possibility of moving a
			brace
	tos =		Pointer to top of p_stack, il, and cstk

CALLS:
	printf (lib)
	reduce

CALLED BY:
	main

HISTORY:
	initial coding 	November 1976	D A Willcox of CAC

*/

#include "./indent_globs.h";
#include "./indent_codes.h";


int     p_stack[50] = stmt;
 /* this is the parser's stack */
int     il[50];    /* this stack stores indentation levels */
int     cstk[50];  /* used to store case stmt indentation levels */
int     tos = 0;   /* pointer to top of stack */


parse (tk)
int     tk;	   /* the code for the construct scanned */
{
    int     i;

#ifdef debug
    printf ("%2d - %s\n", tk, token);
#endif
    while (p_stack[tos] == ifhead && tk != elselit) {
    /* true if we have an if without an else */
	p_stack[tos] = stmt;   /* apply the if(..) stmt ::= stmt reduction */
	reduce ();	       /* see if this allows any reduction */
    }


    switch (tk) {	       /* go on and figure out what to do with the
			          input */

	case decl: 	       /* scanned a declaration word */
	    search_brace = btype_2;
	/* indicate that following brace should be on same line */
	    if (p_stack[tos] != decl) {
	    /* only put one declaration onto stack */
		break_comma = true;
	    /* while in declaration, newline should be forced after comma */
		p_stack[++tos] = decl;
		il[tos] = i_l_follow;

		if (ljust_decl) {
		/* only do if we want left justified declarations */
		    ind_level = 0;
		    for (i = tos - 1; i > 0; --i)
			if (p_stack[i] == decl)
			    ++ind_level;
		/* indentation is number of declaration levels deep we are */
		    i_l_follow = ind_level;
		}
	    }
	    break;

	case ifstmt: 	       /* scanned if (...) */
	case dolit: 	       /* 'do' */
	case forstmt: 	       /* for (...) */
	    p_stack[++tos] = tk;
	    il[tos] = ind_level = i_l_follow;
	    ++i_l_follow;      /* subsequent statements should be indented 1 */
	    search_brace = btype_2;
	    break;

	case lbrace: 	       /* scanned { */
	    break_comma = false;
	/* don't break comma in an initial list */
	    if (p_stack[tos] == stmt || p_stack[tos] == decl
				     || p_stack[tos] == stmtl)
		++i_l_follow;  /* it is a random, isolated stmt group or a
			          declaration */
	    else {
		if (s_code == e_code) {
		/* only do this if there is nothing on the line */
		    --ind_level;
		/* it is a group as part of a while, for, etc. */
		    if (p_stack[tos] == swstmt)
			--ind_level;
		/* for a switch, brace should be two levels out from the code 
		*/
		}
	    }

	    p_stack[++tos] = lbrace;
	    il[tos] = ind_level;
	    p_stack[++tos] = stmt;
	/* allow null stmt between braces */
	    il[tos] = i_l_follow;
	    break;

	case whilestmt:        /* scanned while (...) */
	    if (p_stack[tos] == dohead) {
	    /* it is matched with do stmt */
		ind_level = i_l_follow = il[tos];
		p_stack[++tos] = whilestmt;
		il[tos] = ind_level = i_l_follow;
	    }
	    else {	       /* it is a while loop */
		p_stack[++tos] = whilestmt;
		il[tos] = i_l_follow;
		++i_l_follow;
		search_brace = btype_2;
	    }

	    break;

	case elselit: 	       /* scanned an else */

	    if (p_stack[tos] != ifhead) {
		printf ("%d: Unmatched else\n", line_no);
	    }
	    else {
		ind_level = il[tos];
	    /* indentation for else should be same as for if */
		i_l_follow = ind_level + 1;
	    /* everything following should be in 1 level */
		p_stack[tos] = elsehead;
	    /* remember if with else */
		search_brace = btype_2;
	    }

	    break;

	case rbrace: 	       /* scanned a } */
	/* stack should have <lbrace> <stmt> or <lbrace> <stmtl> */
	    if (p_stack[tos - 1] == lbrace) {
		ind_level = i_l_follow = il[--tos];
		p_stack[tos] = stmt;
	    }
	    else {
		printf ("%d: Stmt nesting error\n", line_no);
	    }

	    break;

	case swstmt: 	       /* had switch (...) */
	    p_stack[++tos] = swstmt;
	    cstk[tos] = case_ind;
	/* save current case indent level */
	    il[tos] = i_l_follow;
	    case_ind = i_l_follow + 1;
	/* cases should be one level down from switch */
	    i_l_follow + = 2;  /* statements should be two levels in */
	    search_brace = btype_2;
	    break;

	case semicolon:        /* this indicates a simple stmt */
	    break_comma = false;
	/* turn off flag to break after commas in a declaration */
	    p_stack[++tos] = stmt;
	    il[tos] = ind_level;
	    break;

	default: 	       /* this is an error */
	    printf ("%d: Unknown code to parser - %d\n", line_no, tk);
	    return;


    }			       /* end of switch */

    reduce ();		       /* see if any reduction can be done */
#ifdef debug
    for (i = 1; i <= tos; ++i)
	printf ("(%d %d)", p_stack[i], il[i]);
    printf ("\n");
#endif
    return;
}
/*

			  Copyright (C) 1976
				by the
			  Board of Trustees
				of the
			University of Illinois

			 All rights reserved


NAME:
	reduce

FUNCTION:
	Implements the reduce part of the parsing algorithm

ALGORITHM:
	The following reductions are done.  Reductions are repeated until no
	more are possible.

	Old___ TOS___		New___ TOS___
	<stmt> <stmt>	<stmtl>
	<stmtl> <stmt>	<stmtl>
	do <stmt>	"dostmt"
	if <stmt>	"ifstmt"
	switch <stmt>	<stmt>
	decl <stmt>	<stmt>
	"ifelse" <stmt>	<stmt>
	for <stmt>	<stmt>
	while <stmt>	<stmt>
	"dostmt" while	<stmt>

	On each reduction, i_l_follow (the indentation for the following line)
	is set to the indentation level associated with the old TOS.

PARAMETERS:
	None

RETURNS:
	Nothing

GLOBALS:
	cstk
	i_l_follow =
	il
	p_stack =
	tos =

CALLS:
	None

CALLED BY:
	parse

HISTORY:
	initial coding 	November 1976	D A Willcox of CAC

*/
/*----------------------------------------------*\
|   REDUCTION PHASE
\*----------------------------------------------*/
reduce () {

    register int    i;
 /* local looping variable */

    for (;;) {		       /* keep looping until there is nothing left to
			          reduce */

	switch (p_stack[tos]) {

	    case stmt: 
		switch (p_stack[tos - 1]) {

		    case stmt: 
		    case stmtl: 
		    /* stmtl stmt or stmt stmt */
			p_stack[--tos] = stmtl;
			break;

		    case dolit: 
		    /* <do> <stmt> */
			p_stack[--tos] = dohead;
			i_l_follow = il[tos];
			break;

		    case ifstmt: 
		    /* <if> <stmt> */
			p_stack[--tos] = ifhead;
			for (i = tos - 1;
				(
				    p_stack[i] != stmt
				    &&
				    p_stack[i] != stmtl
				    &&
				    p_stack[i] != lbrace
				);
				--i);
			i_l_follow = il[i];
		    /* for the time being, we will assume that there is no else
		       on this if, and set the indentation level accordingly.
		       If an else is scanned, it will be fixed up later */
			break;

		    case swstmt: 
		    /* <switch> <stmt> */
			case_ind = cstk[tos - 1];

		    case decl: /* finish of a declaration */
		    case elsehead: 
		    /* <<if> <stmt> else> <stmt> */
		    case forstmt: 
		    /* <for> <stmt> */
		    case whilestmt: 
		    /* <while> <stmt> */
			p_stack[--tos] = stmt;
			i_l_follow = il[tos];
			break;

		    default:   /* <anything else> <stmt> */
			return;

		}	       /* end of section for <stmt> on top of stack */
		break;

	    case whilestmt:    /* while (...) on top */
		if (p_stack[tos - 1] == dohead) {
		/* it is termination of a do while */
		    p_stack[--tos] = stmt;
		    break;
		}
		else
		    return;

	    default: 	       /* anything else on top */
		return;

	}		       /* end of big switch */

    }			       /* end of reduction phase for (;;) */
}
