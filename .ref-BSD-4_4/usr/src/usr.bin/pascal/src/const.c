/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)const.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "tree_ty.h"

/*
 * Const enters the definitions
 * of the constant declaration
 * part into the namelist.
 */
#ifndef PI1
constbeg( lineofyconst , linenum )
    int	lineofyconst, linenum;
{
    static bool	const_order = FALSE;
    static bool	const_seen = FALSE;

/*
 * this allows for multiple declaration
 * parts, unless the "standard" option
 * has been specified.
 * If a routine segment is being compiled,
 * do level one processing.
 */

	if (!progseen)
		level1();
	line = lineofyconst;
	if (parts[ cbn ] & (TPRT|VPRT|RPRT)) {
	    if ( opt( 's' ) ) {
		standard();
		error("Constant declarations should precede type, var and routine declarations");
	    } else {
		if ( !const_order ) {
		    const_order = TRUE;
		    warning();
		    error("Constant declarations should precede type, var and routine declarations");
		}
	    }
	}
	if (parts[ cbn ] & CPRT) {
	    if ( opt( 's' ) ) {
		standard();
		error("All constants should be declared in one const part");
	    } else {
		if ( !const_seen ) {
		    const_seen = TRUE;
		    warning();
		    error("All constants should be declared in one const part");
		}
	    }
	}
	parts[ cbn ] |= CPRT;
}
#endif PI1

constant(cline, cid, cdecl)
	int cline;
	register char *cid;
	register struct tnode *cdecl;
{
	register struct nl *np;

#ifdef PI0
	send(REVCNST, cline, cid, cdecl);
#endif
	line = cline;
	gconst(cdecl);
	np = enter(defnl(cid, CONST, con.ctype, con.cival));
#ifndef PI0
	np->nl_flags |= NMOD;
#endif

#ifdef PC
	if (cbn == 1) {
	    stabgconst( cid , line );
	}
#endif PC

#	ifdef PTREE
	    {
		pPointer	Const = ConstDecl( cid , cdecl );
		pPointer	*Consts;

		pSeize( PorFHeader[ nesting ] );
		Consts = &( pDEF( PorFHeader[ nesting ] ).PorFConsts );
		*Consts = ListAppend( *Consts , Const );
		pRelease( PorFHeader[ nesting ] );
	    }
#	endif
	if (con.ctype == NIL)
		return;
	if ( con.ctype == nl + TSTR )
		np->ptr[0] = (struct nl *) con.cpval;
	if (isa(con.ctype, "i"))
		np->range[0] = con.crval;
	else if (isa(con.ctype, "d"))
		np->real = con.crval;
#       ifdef PC
	    if (cbn == 1 && con.ctype != NIL) {
		    stabconst(np);
	    }
#       endif
}

#ifndef PI0
#ifndef PI1
constend()
{

}
#endif
#endif

/*
 * Gconst extracts
 * a constant declaration
 * from the tree for it.
 * only types of constants
 * are integer, reals, strings
 * and scalars, the first two
 * being possibly signed.
 */
gconst(c_node)
	struct tnode *c_node;
{
	register struct nl *np;
	register struct tnode *cn;
	char *cp;
	int negd, sgnd;
	long ci;

	con.ctype = NIL;
	cn = c_node;
	negd = sgnd = 0;
loop:
	if (cn == TR_NIL || cn->sign_const.number == TR_NIL)
		return;
	switch (cn->tag) {
		default:
			panic("gconst");
		case T_MINUSC:
			negd = 1 - negd;
		case T_PLUSC:
			sgnd++;
			cn = cn->sign_const.number;
			goto loop;
		case T_ID:
			np = lookup(cn->char_const.cptr);
			if (np == NLNIL)
				return;
			if (np->class != CONST) {
				derror("%s is a %s, not a constant as required", cn->char_const.cptr, classes[np->class]);
				return;
			}
			con.ctype = np->type;
			switch (classify(np->type)) {
				case TINT:
					con.crval = np->range[0];
					break;
				case TDOUBLE:
					con.crval = np->real;
					break;
				case TBOOL:
				case TCHAR:
				case TSCAL:
					con.cival = np->value[0];
					con.crval = con.cival;
					break;
				case TSTR:
					con.cpval = (char *) np->ptr[0];
					break;
				case NIL:
					con.ctype = NIL;
					return;
				default:
					panic("gconst2");
			}
			break;
		case T_CBINT:
			con.crval = a8tol(cn->char_const.cptr);
			goto restcon;
		case T_CINT:
			con.crval = atof(cn->char_const.cptr);
			if (con.crval > MAXINT || con.crval < MININT) {
				derror("Constant too large for this implementation");
				con.crval = 0;
			}
restcon:
			ci = con.crval;
#ifndef PI0
			if (bytes(ci, ci) <= 2)
				con.ctype = nl+T2INT;
			else	
#endif
				con.ctype = nl+T4INT;
			break;
		case T_CFINT:
			con.ctype = nl+TDOUBLE;
			con.crval = atof(cn->char_const.cptr);
			break;
		case T_CSTRNG:
			cp = cn->char_const.cptr;
			if (cp[1] == 0) {
				con.ctype = nl+T1CHAR;
				con.cival = cp[0];
				con.crval = con.cival;
				break;
			}
			con.ctype = nl+TSTR;
			con.cpval = savestr(cp);
			break;
	}
	if (sgnd) {
		if (isnta((struct nl *) con.ctype, "id"))
			derror("%s constants cannot be signed",
				nameof((struct nl *) con.ctype));
		else {
			if (negd)
				con.crval = -con.crval;
			ci = con.crval;
		}
	}
}

#ifndef PI0
isconst(cn)
	register struct tnode *cn;
{

	if (cn == TR_NIL)
		return (1);
	switch (cn->tag) {
		case T_MINUS:
			cn->tag = T_MINUSC;
			cn->sign_const.number = 
					 cn->un_expr.expr;
			return (isconst(cn->sign_const.number));
		case T_PLUS:
			cn->tag = T_PLUSC;
			cn->sign_const.number = 
					 cn->un_expr.expr;
			return (isconst(cn->sign_const.number));
		case T_VAR:
			if (cn->var_node.qual != TR_NIL)
				return (0);
			cn->tag = T_ID;
			cn->char_const.cptr = 
					cn->var_node.cptr;
			return (1);
		case T_BINT:
			cn->tag = T_CBINT;
			cn->char_const.cptr = 
				cn->const_node.cptr;
			return (1);
		case T_INT:
			cn->tag = T_CINT;
			cn->char_const.cptr = 
				cn->const_node.cptr;
			return (1);
		case T_FINT:
			cn->tag = T_CFINT;
			cn->char_const.cptr = 
				cn->const_node.cptr;
			return (1);
		case T_STRNG:
			cn->tag = T_CSTRNG;
			cn->char_const.cptr = 
				cn->const_node.cptr;
			return (1);
	}
	return (0);
}
#endif
