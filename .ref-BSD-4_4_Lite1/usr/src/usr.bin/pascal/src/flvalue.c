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
static char sccsid[] = "@(#)flvalue.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#include "tree_ty.h"
#ifdef PC
#   include "pc.h"
#   include <pcc.h>
#endif PC
#include "tmps.h"

    /*
     *	flvalue generates the code to either pass on a formal routine,
     *	or construct the structure which is the environment for passing.
     *	it tells the difference by looking at the tree it's given.
     */
struct nl *
flvalue( r , formalp )
    struct tnode *r; 	/* T_VAR */
    struct nl	*formalp;
    {
	struct nl	*p;
	struct nl	*tempnlp;
	char		*typename;
#ifdef PC
	char		extname[ BUFSIZ ];
#endif PC

	if ( r == TR_NIL ) {
	    return NLNIL;
	}
	typename = formalp -> class == FFUNC ? "function":"procedure";
	if ( r->tag != T_VAR ) {
	    error("Expression given, %s required for %s parameter %s" ,
		    typename , typename , formalp -> symbol );
	    return NLNIL;
	}
	p = lookup(r->var_node.cptr);
	if (p == NLNIL) {
	    return NLNIL;
	}
	switch ( p -> class ) {
	    case FFUNC:
	    case FPROC:
		    if ( r->var_node.qual != TR_NIL ) {
			error("Formal %s %s cannot be qualified" ,
				typename , p -> symbol );
			return NLNIL;
		    }
#		    ifdef OBJ
			(void) put(2, PTR_RV | bn << 8+INDX, (int)p->value[NL_OFFS]);
#		    endif OBJ
#		    ifdef PC
			putRV( p -> symbol , bn , p -> value[ NL_OFFS ] , 
				p -> extra_flags ,
				p2type( p ) );
#		    endif PC
		    return p;
	    case FUNC:
	    case PROC:
		    if ( r->var_node.qual != TR_NIL ) {
			error("%s %s cannot be qualified" , typename ,
				p -> symbol );
			return NLNIL;
		    }
		    if (bn == 0) {
			error("Built-in %s %s cannot be passed as a parameter" ,
				typename , p -> symbol );
			return NLNIL;
		    }
			/*
			 *	allocate space for the thunk
			 */
		    tempnlp = tmpalloc((long) (sizeof(struct formalrtn)), NLNIL, NOREG);
#		    ifdef OBJ
			(void) put(2 , O_LV | cbn << 8 + INDX ,
				(int)tempnlp -> value[ NL_OFFS ] );
			(void) put(2, O_FSAV | bn << 8, (long)p->value[NL_ENTLOC]);
#		    endif OBJ
#		    ifdef PC
			putleaf( PCC_ICON , 0 , 0 ,
			    PCCM_ADDTYPE( PCCTM_PTR , PCCM_ADDTYPE( PCCTM_FTN , PCCTM_PTR|PCCT_STRTY ) ) ,
			    "_FSAV" );
			sprintf( extname , "%s" , FORMALPREFIX );
			sextname( &extname[ strlen( extname ) ] ,
				    p -> symbol , bn );
			putleaf( PCC_ICON , 0 , 0 , p2type( p ) , extname );
			putleaf( PCC_ICON , bn , 0 , PCCT_INT , (char *) 0 );
			putop( PCC_CM , PCCT_INT );
			putLV( (char *) 0 , cbn , tempnlp -> value[NL_OFFS] ,
				tempnlp -> extra_flags , PCCT_STRTY );
			putop( PCC_CM , PCCT_INT );
			putop( PCC_CALL , PCCTM_PTR | PCCT_STRTY );
#		    endif PC
		    return p;
	    default:
		    error("Variable given, %s required for %s parameter %s" ,
			    typename , typename , formalp -> symbol );
		    return NLNIL;
	}
    }
