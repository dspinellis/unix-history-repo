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
static char sccsid[] = "@(#)lab.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#ifdef PC
#   include	"pc.h"
#   include	<pcc.h>
#endif PC
#include "tree_ty.h"

/*
 * Label enters the definitions
 * of the label declaration part
 * into the namelist.
 */
label(r, l)
	struct tnode *r;
	int l;
{
    static bool	label_order = FALSE;
    static bool	label_seen = FALSE;
#ifdef PC
	char	extname[ BUFSIZ ];
#endif PC
#ifndef PI0
	register struct tnode *ll;
	register struct nl *p, *lp;

	lp = NIL;
#else
	send(REVLAB, r);
#endif
	if ( ! progseen ) {
	    level1();
	}
	line = l;
#ifndef PI1
	if (parts[ cbn ] & (CPRT|TPRT|VPRT|RPRT)){
	    if ( opt( 's' ) ) {
		standard();
		error("Label declarations should precede const, type, var and routine declarations");
	    } else {
		if ( !label_order ) {
		    label_order = TRUE;
		    warning();
		    error("Label declarations should precede const, type, var and routine declarations");
		}
	    }
	}
	if (parts[ cbn ] & LPRT) {
	    if ( opt( 's' ) ) {
		standard();
		error("All labels should be declared in one label part");
	    } else {
		if ( !label_seen ) {
		    label_seen = TRUE;
		    warning();
		    error("All labels should be declared in one label part");
		}
	    }
	}
	parts[ cbn ] |= LPRT;
#endif
#ifndef PI0
	for (ll = r; ll != TR_NIL; ll = ll->list_node.next) {
		l = (int) getlab();
		p = enter(defnl((char *) ll->list_node.list, LABEL, NLNIL,
				(int) l));
		/*
		 * Get the label for the eventual target
		 */
		p->value[1] = (int) getlab();
		p->chain = lp;
		p->nl_flags |= (NFORWD|NMOD);
		p->value[NL_GOLEV] = NOTYET;
		p->value[NL_ENTLOC] = l;
		lp = p;
#		ifdef OBJ
		    /*
		     * This operator is between
		     * the bodies of two procedures
		     * and provides a target for
		     * gotos for this label via TRA.
		     */
		    (void) putlab((char *) l);
		    (void) put(2, O_GOTO | cbn<<8, (long)p->value[1]);
#		endif OBJ
#		ifdef PC
		    /*
		     *	labels have to be .globl otherwise /lib/c2 may
		     *	throw them away if they aren't used in the function
		     *	which defines them.
		     */
		    extlabname( extname , p -> symbol , cbn );
		    putprintf("	.globl	%s", 0, (int) extname);
		    if ( cbn == 1 ) {
			stabglabel( extname , line );
		    }
#		endif PC
	}
	gotos[cbn] = lp;
#	ifdef PTREE
	    {
		pPointer	Labels = LabelDCopy( r );

		pDEF( PorFHeader[ nesting ] ).PorFLabels = Labels;
	    }
#	endif PTREE
#endif
}

#ifndef PI0
/*
 * Gotoop is called when
 * we get a statement "goto label"
 * and generates the needed tra.
 */
gotoop(s)
	char *s;
{
	register struct nl *p;
#ifdef PC
	char	extname[ BUFSIZ ];
#endif PC

	gocnt++;
	p = lookup(s);
	if (p == NIL)
		return;
#	ifdef OBJ
	    (void) put(2, O_TRA4, (long)p->value[NL_ENTLOC]);
#	endif OBJ
#	ifdef PC
	    if ( cbn == bn ) {
		    /*
		     *	local goto.
		     */
		extlabname( extname , p -> symbol , bn );
		    /*
		     * this is a funny jump because it's to a label that
		     * has been declared global.
		     * Although this branch is within this module
		     * the assembler will complain that the destination
		     * is a global symbol.
		     * The complaint arises because the assembler
		     * doesn't change relative jumps into absolute jumps.
		     * and this  may cause a branch displacement overflow
		     * when the module is subsequently linked with
		     * the rest of the program.
		     */
#		if defined(vax) || defined(tahoe)
		    putprintf("	jmp	%s", 0, (int) extname);
#		endif vax || tahoe
#		ifdef mc68000
		    putprintf("	jra	%s", 0, (int) extname);
#		endif mc68000
	    } else {
		    /*
		     *	Non-local goto.
		     *
		     *  Close all active files between top of stack and
		     *  frame at the destination level.	Then call longjmp
		     *	to unwind the stack to the destination level.
		     *
		     *	For nested routines the end of the frame
		     *	is calculated as:
		     *	    __disply[bn].fp + sizeof(local frame)
		     *	(adjusted by (sizeof int) to get just past the end).
		     *	The size of the local frame is dumped out by
		     *	the second pass as an assembler constant.
		     *	The main routine may not be compiled in this
		     *	module, so its size may not be available.
		     * 	However all of its variables will be globally
		     *	declared, so only the known runtime temporaries
		     *	will be in its stack frame.
		     */
		parts[ bn ] |= NONLOCALGOTO;
		putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			, "_PCLOSE" );
		if ( bn > 1 ) {
		    p = lookup( enclosing[ bn - 1 ] );
		    sprintf( extname, "%s%d+%d",
			FRAME_SIZE_LABEL, p -> value[NL_ENTLOC], sizeof(int));
		    p = lookup(s);
		    putLV( extname , bn , 0 , NNLOCAL , PCCTM_PTR | PCCT_CHAR );
		} else {
		    putLV((char *) 0 , bn , -( DPOFF1 + sizeof( int ) ) , LOCALVAR ,
			PCCTM_PTR | PCCT_CHAR );
		}
		putop( PCC_CALL , PCCT_INT );
		putdot( filename , line );
		putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			, "_longjmp" );
		putLV((char *) 0 , bn , GOTOENVOFFSET , NLOCAL , PCCTM_PTR|PCCT_STRTY );
		extlabname( extname , p -> symbol , bn );
		putLV( extname , 0 , 0 , NGLOBAL , PCCTM_PTR|PCCT_STRTY );
		putop( PCC_CM , PCCT_INT );
		putop( PCC_CALL , PCCT_INT );
		putdot( filename , line );
	    }
#	endif PC
	if (bn == cbn)
		if (p->nl_flags & NFORWD) {
			if (p->value[NL_GOLEV] == NOTYET) {
				p->value[NL_GOLEV] = level;
				p->value[NL_GOLINE] = line;
			}
		} else
			if (p->value[NL_GOLEV] == DEAD) {
				recovered();
				error("Goto %s is into a structured statement", p->symbol);
			}
}

/*
 * Labeled is called when a label
 * definition is encountered, and
 * marks that it has been found and
 * patches the associated GOTO generated
 * by gotoop.
 */
labeled(s)
	char *s;
{
	register struct nl *p;
#ifdef PC
	char	extname[ BUFSIZ ];
#endif PC

	p = lookup(s);
	if (p == NIL)
		return;
	if (bn != cbn) {
		error("Label %s not defined in correct block", s);
		return;
	}
	if ((p->nl_flags & NFORWD) == 0) {
		error("Label %s redefined", s);
		return;
	}
	p->nl_flags &= ~NFORWD;
#	ifdef OBJ
	    patch4((PTR_DCL) p->value[NL_ENTLOC]);
#	endif OBJ
#	ifdef PC
	    extlabname( extname , p -> symbol , bn );
	    putprintf( "%s:" , 0 , (int) extname );
#	endif PC
	if (p->value[NL_GOLEV] != NOTYET)
		if (p->value[NL_GOLEV] < level) {
			recovered();
			error("Goto %s from line %d is into a structured statement", s, (char *) p->value[NL_GOLINE]);
		}
	p->value[NL_GOLEV] = level;
}
#endif

#ifdef PC
    /*
     *	construct the long name of a label based on it's static nesting.
     *	into a caller-supplied buffer (that should be about BUFSIZ big).
     */
extlabname( buffer , name , level )
    char	buffer[];
    char	*name;
    int		level;
{
    char	*starthere;
    int		i;

    starthere = &buffer[0];
    for ( i = 1 ; i < level ; i++ ) {
	sprintf( starthere , EXTFORMAT , enclosing[ i ] );
	starthere += strlen( enclosing[ i ] ) + 1;
    }
    sprintf( starthere , EXTFORMAT , "" );
    starthere += 1;
    sprintf( starthere , LABELFORMAT , name );
    starthere += strlen( name ) + 1;
    if ( starthere >= &buffer[ BUFSIZ ] ) {
	panic( "extlabname" );
    }
}
#endif PC
