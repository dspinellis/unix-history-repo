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
static char sccsid[] = "@(#)pccaseop.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include "whoami.h"
#ifdef PC
    /*
     *	and the rest of the file
     */
#include "0.h"
#include "tree.h"
#include "objfmt.h"
#include <pcc.h>
#include "pc.h"
#include "tmps.h"
#include "tree_ty.h"

    /*
     *	structure for a case: 
     *	    its constant label, line number (for errors), and location label.
     */
struct ct {
    long	cconst;
    int		cline;
    int		clabel;
};

    /*
     *	the PCC_FORCE operator puts its operand into a register.
     *	these to keep from thinking of it as r0 all over.
     */
#if defined(vax) || defined(tahoe)
#   define	FORCENAME	"r0"
#endif vax || tahoe
#ifdef mc68000
#   define	FORCENAME	"d0"
#   define	ADDRTEMP	"a0"
#endif mc68000

    /*
     *	given a tree for a case statement, generate code for it.
     *	this computes the expression into a register,
     *	puts down the code for each of the cases,
     *	and then decides how to do the case switching.
     *	tcase	[0]	T_CASE
     *		[1]	lineof "case"
     *		[2]	expression
     *		[3]	list of cased statements:
     *			cstat	[0]	T_CSTAT
     *				[1]	lineof ":"
     *				[2]	list of constant labels
     *				[3]	statement
     */
pccaseop( tcase )
    WHI_CAS *tcase;
{
    struct nl	*exprtype;
    struct nl	*exprnlp;
    struct nl	*rangetype;
    long	low;
    long	high;
    long	exprctype;
    char 	*swlabel;
    char	*endlabel;
    char	*label;
    int		count;
    struct tnode *cstatlp;
    struct tnode *cstatp;
    struct tnode *casep;
    struct ct	*ctab;
    struct ct	*ctp;
    bool	nr;
    long	goc;
    int		casecmp();
    bool	dupcases;

    goc = gocnt;
	/*
	 *  find out the type of the case expression
	 *  even if the expression has errors (exprtype == NIL), continue.
	 */
    line = tcase->line_no;
    codeoff();
    exprtype = rvalue( tcase->expr , NLNIL  , RREQ );
    codeon();
    if ( exprtype != NLNIL ) {
	if ( isnta( exprtype , "bcsi" ) ) {
	    error("Case selectors cannot be %ss" , nameof( exprtype ) );
	    exprtype = NLNIL;
	} else {
	    if ( exprtype -> class != RANGE ) {
		rangetype = exprtype -> type;
	    } else {
		rangetype = exprtype;
	    }
	    if ( rangetype == NLNIL ) {
		exprtype = NLNIL;
	    } else {
		low = rangetype -> range[0];
		high = rangetype -> range[1];
	    }
	}
    }
    if ( exprtype != NLNIL ) {
	    /*
	     *	compute and save the case expression.
	     *	also, put expression into a register
	     *	save its c-type and jump to the code to do the switch.
	     */
	exprctype = p2type( exprtype );
	exprnlp = tmpalloc( (long) (sizeof (long)), nl + T4INT , NOREG );
	putRV((char *) 0 , cbn , exprnlp -> value[ NL_OFFS ] ,
			exprnlp -> extra_flags , PCCT_INT );
	(void) rvalue( tcase->expr , NLNIL , RREQ );
	sconv((int) exprctype, (int) PCCT_INT);
	putop( PCC_ASSIGN , PCCT_INT );
	putop( PCC_FORCE , PCCT_INT );
	putdot( filename , line );
	swlabel = getlab();
	putjbr( (long) swlabel );
    }
	/*
	 *  count the number of cases
	 *  and allocate table for cases, lines, and labels
	 *  default case goes in ctab[0].
	 */
    count = 1;
    for ( cstatlp = tcase->stmnt_list ; cstatlp != TR_NIL ;
		cstatlp = cstatlp->list_node.next ) {
	cstatp = cstatlp->list_node.list;
	if ( cstatp == TR_NIL ) {
	    continue;
	}
	for ( casep = cstatp->c_stmnt.const_list ; casep != TR_NIL ;
			casep = casep->list_node.next ) {
	    count++;
	}
    }
	/*
	 */
    ctab = (struct ct *) malloc( count * sizeof( struct ct ) );
    if ( ctab == (struct ct *) 0 ) {
	error("Ran out of memory (case)");
	pexit( DIED );
    }
	/*
	 *  pick up default label and label for after case statement.
	 */
    ctab[0].clabel = (int) getlab();
    endlabel = getlab();
	/*
	 *  generate code for each case
	 *  filling in ctab for each.
	 *  nr is for error if no case falls out bottom.
	 */
    nr = TRUE;;
    count = 0;
    for ( cstatlp = tcase->stmnt_list ; cstatlp != TR_NIL ;
		cstatlp = cstatlp->list_node.next ) {
	cstatp = cstatlp->list_node.list;
	if ( cstatp == TR_NIL ) {
	    continue;
	}
	line = cstatp->c_stmnt.line_no;
	label = getlab();
	for ( casep = cstatp->c_stmnt.const_list ; casep != TR_NIL ;
			casep = casep->list_node.next ) {
	    gconst( casep->list_node.list );
	    if( exprtype == NLNIL || con.ctype == NIL ) {
		continue;
	    }
	    if ( incompat( con.ctype , exprtype , TR_NIL ) ) {
		cerror("Case label type clashed with case selector expression type");
		continue;
	    }
	    if ( con.crval < low || con.crval > high ) {
		error("Case label out of range");
		continue;
	    }
	    count++;
	    ctab[ count ].cconst = con.crval;
	    ctab[ count ].cline = line;
	    ctab[ count ].clabel = (int) label;
	}
	    /*
	     *	put out the statement
	     */
	(void) putlab( label );
	putcnt();
	level++;
	statement( cstatp->c_stmnt.stmnt );
	nr = (nr && noreach)?TRUE:FALSE;
	noreach = FALSE;
	level--;
	if (gotos[cbn]) {
		ungoto();
	}
	putjbr( (long) endlabel );
    }
    noreach = nr;
	/*
	 *	default action is to call error
	 */
    (void) putlab( (char *) ctab[0].clabel );
    putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR ) , "_CASERNG" );
    putRV((char *) 0 , cbn , exprnlp -> value[ NL_OFFS ] ,
		    exprnlp -> extra_flags , PCCT_INT );
    putop( PCC_CALL , PCCT_INT );
    putdot( filename , line );
	/*
	 *  sort the cases
	 */
    qsort( &ctab[1] , count , sizeof (struct ct) , casecmp );
	/*
	 *  check for duplicates
	 */
    dupcases = FALSE;
    for ( ctp = &ctab[1] ; ctp < &ctab[ count ] ; ctp++ ) {
	if ( ctp[0].cconst == ctp[1].cconst ) {
	    error("Multiply defined label in case, lines %d and %d" ,
		    (char *) ctp[0].cline , (char *) ctp[1].cline );
	    dupcases = TRUE;
	}
    }
    if ( dupcases ) {
	return;
    }
	/*
	 *  choose a switch algorithm and implement it:
	 *	direct switch	>= 1/3 full and >= 4 cases.
	 *	binary switch	not direct switch and > 8 cases.
	 *	ifthenelse	not direct or binary switch.
	 */
    (void) putlab( swlabel );
    if ( ctab[ count ].cconst - ctab[1].cconst < 3 * count && count >= 4 ) {
	directsw( ctab , count );
    } else if ( count > 8 ) {
	binarysw( ctab , count );
    } else {
	itesw( ctab , count );
    }
    (void) putlab( endlabel );
    if ( goc != gocnt ) {
	    putcnt();
    }
}

    /*
     *	direct switch
     */
directsw( ctab , count )
    struct ct	*ctab;
    int		count;
{
    int		fromlabel = (int) getlab();
    long	i;
    long	j;

#   ifdef vax
	if (opt('J')) {
	    /*
	     *	We have a table of absolute addresses.
	     *
	     *	subl2	to make r0 a 0-origin byte offset.
	     *	cmpl	check against upper limit.
	     *	jlssu	error if out of bounds.
	     *	ashl	to make r0 a 0-origin long offset,
	     *	jmp	and indirect through it.
	     */
	    putprintf("	subl2	$%d,%s", 0, (int) ctab[1].cconst, (int) FORCENAME);
	    putprintf("	cmpl	$%d,%s", 0,
			(int) (ctab[count].cconst - ctab[1].cconst),
			(int) FORCENAME);
	    putprintf("	jlssu	%s%d", 0, (int) LABELPREFIX, ctab[0].clabel);
	    putprintf("	ashl	$2,%s,%s", 0, (int) FORCENAME, (int) FORCENAME);
	    putprintf("	jmp	*%s%d(%s)", 0,
		    (int) LABELPREFIX, fromlabel, (int) FORCENAME);
	} else {
	    /*
	     *	We can use the VAX casel instruction with a table
	     *	of short relative offsets.
	     */
	    putprintf("	casel	%s,$%d,$%d" , 0 , (int) FORCENAME ,
		    (int) ctab[1].cconst ,
		    (int) (ctab[ count ].cconst - ctab[1].cconst ));
	}
#   endif vax

#   ifdef tahoe
	if (opt('J')) {
	    /*
	     *	We have a table of absolute addresses.
	     *
	     *	subl2	to make r0 a 0-origin byte offset.
	     *	cmpl	check against upper limit.
	     *	jlssu	error if out of bounds.
	     *	shal	to make r0 a 0-origin long offset,
	     *	jmp	and indirect through it.
	     */
	    putprintf("	subl2	$%d,%s", 0, (int) ctab[1].cconst, (int) FORCENAME);
	    putprintf("	cmpl	$%d,%s", 0,
			(int) (ctab[count].cconst - ctab[1].cconst),
			(int) FORCENAME);
	    putprintf("	jlssu	%s%d", 0, (int) LABELPREFIX, ctab[0].clabel);
	    putprintf("	shal	$2,%s,%s", 0, (int) FORCENAME, (int) FORCENAME);
	    putprintf("	jmp	*%s%d(%s)", 0,
		    (int) LABELPREFIX, fromlabel, (int) FORCENAME);
	} else {
	    /*
	     *	We can use the TAHOE casel instruction with a table
	     *	of short relative offsets.
	     */
	    putprintf("	casel	%s,$%d,$%d" , 0 , (int) FORCENAME ,
		    (int) ctab[1].cconst ,
		    (int) (ctab[ count ].cconst - ctab[1].cconst ));
	    putprintf("	.align 1", 0);
	}
#   endif tahoe

#   ifdef mc68000
	/*
	 *	subl	to make d0 a 0-origin byte offset.
	 *	cmpl	check against upper limit.
	 *	bhi	error if out of bounds.
	 */
	putprintf("	subl	#%d,%s", 0, ctab[1].cconst, FORCENAME);
	putprintf("	cmpl	#%d,%s", 0,
		ctab[count].cconst - ctab[1].cconst, FORCENAME);
	putprintf("	bhi	%s%d", 0, LABELPREFIX, ctab[0].clabel);
	if (opt('J')) {
	    /*
	     *	We have a table of absolute addresses.
	     *
	     *	asll	to make d0 a 0-origin long offset.
	     *	movl	pick up a jump-table entry
	     *	jmp	and indirect through it.
	     */
	    putprintf("	asll	#2,%s", 0, FORCENAME, FORCENAME);
	    putprintf("	movl	pc@(4,%s:l),%s", 0, FORCENAME, ADDRTEMP);
	    putprintf("	jmp	%s@", 0, ADDRTEMP);
	} else {
	    /*
	     *	We have a table of relative addresses.
	     *
	     *	addw	to make d0 a 0-origin word offset.
	     *	movw	pick up a jump-table entry
	     *	jmp	and indirect through it.
	     */
	    putprintf("	addw	%s,%s", 0, FORCENAME, FORCENAME);
	    putprintf("	movw	pc@(6,%s:w),%s", 0, FORCENAME, FORCENAME);
	    putprintf("	jmp	pc@(2,%s:w)", 0, FORCENAME);
	}
#   endif mc68000
    (void) putlab( (char *) fromlabel );
    i = 1;
    j = ctab[1].cconst;
    while ( i <= count ) {
	if ( j == ctab[ i ].cconst ) {
	    if (opt('J')) {
		putprintf( "	.long	" , 1 );
		putprintf( PREFIXFORMAT , 0 , (int) LABELPREFIX , ctab[ i ].clabel );
	    } else {
		putprintf( "	.word	" , 1 );
		putprintf( PREFIXFORMAT , 1 , (int) LABELPREFIX , ctab[ i ].clabel );
		putprintf( "-" , 1 );
		putprintf( PREFIXFORMAT , 0 , (int) LABELPREFIX , fromlabel );
	    }
	    i++;
	} else {
	    if (opt('J')) {
		putprintf( "	.long	" , 1 );
		putprintf( PREFIXFORMAT , 0 , (int) LABELPREFIX , ctab[ 0 ].clabel );
	    } else {
		putprintf( "	.word	" , 1 );
		putprintf( PREFIXFORMAT , 1 , (int) LABELPREFIX , ctab[ 0 ].clabel );
		putprintf( "-" , 1 );
		putprintf( PREFIXFORMAT , 0 , (int) LABELPREFIX , fromlabel );
	    }
	}
	j++;
    }
#   if defined(vax) || defined(tahoe)
	    /*
	     *	execution continues here if value not in range of case.
	     */
	if (!opt('J'))
	    putjbr( (long) ctab[0].clabel );
#   endif vax || tahoe
}

    /*
     *	binary switch
     *	special case out default label and start recursion.
     */
binarysw( ctab , count )
    struct ct	*ctab;
    int		count;
{
    
    bsrecur( ctab[0].clabel , &ctab[0] , count );
}

    /*
     *	recursive log( count ) search.
     */
bsrecur( deflabel , ctab , count )
    int		deflabel;
    struct ct	*ctab;
    int		count;
{

    if ( count <= 0 ) {
	putjbr((long) deflabel);
	return;
    } else if ( count == 1 ) {
#	if defined(vax) || defined(tahoe)
	    putprintf("	cmpl	%s,$%d", 0, (int) FORCENAME, (int) ctab[1].cconst);
	    putprintf("	jeql	%s%d", 0, (int) LABELPREFIX, ctab[1].clabel);
	    putjbr((long) deflabel);
#	endif vax || tahoe
#	ifdef mc68000
	    putprintf("	cmpl	#%d,%s", 0, ctab[1].cconst, (int) FORCENAME);
	    putprintf("	jeq	L%d", 0, (int) LABELPREFIX, ctab[1].clabel);
	    putjbr((long) deflabel);
#	endif mc68000
	return;
    } else {
	int	half = ( count + 1 ) / 2;
	int	gtrlabel = (int) getlab();

#	if defined(vax) || defined(tahoe)
	    putprintf("	cmpl	%s,$%d", 0, (int) FORCENAME, (int) ctab[half].cconst);
	    putprintf("	jgtr	%s%d", 0, (int) LABELPREFIX, gtrlabel);
	    putprintf("	jeql	%s%d", 0, (int) LABELPREFIX, ctab[half].clabel);
#	endif vax || tahoe
#	ifdef mc68000
	    putprintf("	cmpl	#%d,%s", 0, (int) ctab[half].cconst, (int) FORCENAME);
	    putprintf("	jgt	%s%d", 0, (int) LABELPREFIX, gtrlabel);
	    putprintf("	jeq	%s%d", 0, (int) LABELPREFIX, ctab[half].clabel);
#	endif mc68000
	bsrecur( deflabel , &ctab[0] , half - 1 );
	(void) putlab((char *) gtrlabel);
	bsrecur( deflabel , &ctab[ half ] , count - half );
	return;
    }
}

itesw( ctab , count )
    struct ct	*ctab;
    int		count;
{
    int	i;

    for ( i = 1 ; i <= count ; i++ ) {
#	if defined(vax) || defined(tahoe)
	    putprintf("	cmpl	%s,$%d", 0, (int) FORCENAME, (int) ctab[i].cconst);
	    putprintf("	jeql	%s%d", 0, (int) LABELPREFIX, ctab[i].clabel);
#	endif vax || tahoe
#	ifdef mc68000
	    putprintf("	cmpl	#%d,%s", 0, (int) ctab[i].cconst, (int) FORCENAME);
	    putprintf("	jeq	%s%d", 0, (int) LABELPREFIX, ctab[i].clabel);
#	endif mc68000
    }
    putjbr((long) ctab[0].clabel);
    return;
}
int
casecmp( this , that )
    struct ct 	*this;
    struct ct 	*that;
{
    if ( this -> cconst < that -> cconst ) {
	return -1;
    } else if ( this -> cconst > that -> cconst ) {
	return 1;
    } else {
	return 0;
    }
}
#endif PC
