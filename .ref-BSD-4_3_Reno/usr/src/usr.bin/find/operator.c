/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Cimarron D. Taylor of the University of California, Berkeley.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)operator.c	5.1 (Berkeley) 4/16/90";
#endif /* not lint */

#include <sys/types.h>
#include <stdio.h>
#include "find.h"
    
/*
 * find_yanknode --
 *	destructively removes the top from the plan
 */
PLAN *
find_yanknode(planp)    
	PLAN **planp;		/* pointer to top of plan (modified) */
{
	PLAN *node;		/* top node removed from the plan */
    
	if ((node = (*planp)) == NULL)
		return(NULL);
	(*planp) = (*planp)->next;
	node->next = NULL;
	return(node);
}
 
/*
 * find_yankexpr --
 *	Removes one expression from the plan.  This is used mainly by
 *	find_squish_paren.  In comments below, an expression is either
 *	a simple node or a T_EXPR node containing a list of simple nodes.
 */
PLAN *
find_yankexpr(planp)    
	PLAN **planp;		/* pointer to top of plan (modified) */
{
	register PLAN *next;	/* temp node holding subexpression results */
	PLAN *node;		/* pointer to returned node or expression */
	PLAN *tail;		/* pointer to tail of subplan */
	PLAN *subplan;		/* pointer to head of ( ) expression */
	int f_expr();
    
	/* first pull the top node from the plan */
	if ((node = find_yanknode(planp)) == NULL)
		return(NULL);
    
	/*
	 * If the node is an '(' then we recursively slurp up expressions
	 * until we find its associated ')'.  If it's a closing paren we
	 * just return it and unwind our recursion; all other nodes are
	 * complete expressions, so just return them.
	 */
	if (node->type == T_OPENPAREN)
		for (tail = subplan = NULL;;) {
			if ((next = find_yankexpr(planp)) == NULL)
				bad_arg("(", "missing closing ')'");
			/*
			 * If we find a closing ')' we store the collected
			 * subplan in our '(' node and convert the node to
			 * a T_EXPR.  The ')' we found is ignored.  Otherwise,
			 * we just continue to add whatever we get to our
			 * subplan.
			 */
			if (next->type == T_CLOSEPAREN) {
				if (subplan == NULL)
					bad_arg("()", "empty inner expression");
				node->p_data[0] = subplan;
				node->type = T_EXPR;
				node->eval = f_expr;
				break;
			} else {
				if (subplan == NULL)
					tail = subplan = next;
				else {
					tail->next = next;
					tail = next;
				}
				tail->next = NULL;
			}
		}
	return(node);
}
 
/*
 * find_squish_paren --
 *	replaces "parentheisized" plans in our search plan with "expr" nodes.
 */
PLAN *
find_squish_paren(plan)
	PLAN *plan;		/* plan with ( ) nodes */
{
	register PLAN *expr;	/* pointer to next expression */
	register PLAN *tail;	/* pointer to tail of result plan */
	PLAN *result;		/* pointer to head of result plan */
    
	result = tail = NULL;

	/*
	 * the basic idea is to have find_yankexpr do all our work and just
	 * collect it's results together.
	 */
	while ((expr = find_yankexpr(&plan)) != NULL) {
		/*
		 * if we find an unclaimed ')' it means there is a missing
		 * '(' someplace.
		 */
		if (expr->type == T_CLOSEPAREN)
			bad_arg(")", "no beginning '('");

		/* add the expression to our result plan */
		if (result == NULL)
			tail = result = expr;
		else {
			tail->next = expr;
			tail = expr;
		}
		tail->next = NULL;
	}
	return(result);
}
 
/*
 * find_squish_not --
 *	compresses "!" expressions in our search plan.
 */
PLAN *
find_squish_not(plan)
	PLAN *plan;		/* plan to process */
{
	register PLAN *next;	/* next node being processed */
	register PLAN *node;	/* temporary node used in T_NOT processing */
	register PLAN *tail;	/* pointer to tail of result plan */
	PLAN *result;		/* pointer to head of result plan */
    
	tail = result = next = NULL;
    
	while ((next = find_yanknode(&plan)) != NULL) {
		/*
		 * if we encounter a ( expression ) then look for nots in
		 * the expr subplan.
		 */
		if (next->type == T_EXPR)
			next->p_data[0] = find_squish_not(next->p_data[0]);

		/*
		 * if we encounter a not, then snag the next node and place
		 * it in the not's subplan.  As an optimization we compress
		 * several not's to zero or one not.
		 */
		if (next->type == T_NOT) {
			int notlevel = 1;

			node = find_yanknode(&plan);
			while (node->type == T_NOT) {
				++notlevel;
				node = find_yanknode(&plan);
			}
			if (node == NULL)
				bad_arg("!", "no following expression");
			if (node->type == T_OR)
				bad_arg("!", "nothing between ! and -o");
			if (notlevel % 2 != 1)
				next = node;
			else
				next->p_data[0] = node;
		}

		/* add the node to our result plan */
		if (result == NULL)
			tail = result = next;
		else {
			tail->next = next;
			tail = next;
		}
		tail->next = NULL;
	}
	return(result);
}
 
/*
 * find_squish_or --
 *	compresses -o expressions in our search plan.
 */
PLAN *
find_squish_or(plan)
	PLAN *plan;		/* plan with ors to be squished */
{
	register PLAN *next;	/* next node being processed */
	register PLAN *tail;	/* pointer to tail of result plan */
	PLAN *result;		/* pointer to head of result plan */
    
	tail = result = next = NULL;
    
	while ((next = find_yanknode(&plan)) != NULL) {
		/*
		 * if we encounter a ( expression ) then look for or's in
		 * the expr subplan.
		 */
		if (next->type == T_EXPR)
			next->p_data[0] = find_squish_or(next->p_data[0]);

		/* if we encounter a not then look for not's in the subplan */
		if (next->type == T_NOT)
			next->p_data[0] = find_squish_or(next->p_data[0]);

		/*
		 * if we encounter an or, then place our collected plan in the
		 * or's first subplan and then recursively collect the
		 * remaining stuff into the second subplan and return the or.
		 */
		if (next->type == T_OR) {
			if (result == NULL)
				bad_arg("-o", "no expression before -o");
			next->p_data[0] = result;
			next->p_data[1] = find_squish_or(plan);
			if (next->p_data[1] == NULL)
				bad_arg("-o", "no expression after -o");
			return(next);
		}

		/* add the node to our result plan */
		if (result == NULL)
			tail = result = next;
		else {
			tail->next = next;
			tail = next;
		}
		tail->next = NULL;
	}
	return(result);
}
