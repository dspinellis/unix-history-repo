/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)fend.c	5.4 (Berkeley) %G%";
#endif not lint

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#include "align.h"
#include "tmps.h"

/*
 * this array keeps the pxp counters associated with
 * functions and procedures, so that they can be output
 * when their bodies are encountered
 */
int	bodycnts[ DSPLYSZ ];

#ifdef PC
#   include "pc.h"
#   include <pcc.h>
#endif PC

#ifdef OBJ
int	cntpatch;
int	nfppatch;
#endif OBJ

#include "tree_ty.h"

struct	nl *Fp;
int	pnumcnt;
/*
 * Funcend is called to
 * finish a block by generating
 * the code for the statements.
 * It then looks for unresolved declarations
 * of labels, procedures and functions,
 * and cleans up the name list.
 * For the program, it checks the
 * semantics of the program
 * statement (yuchh).
 */
funcend(fp, bundle, endline)
	struct nl *fp;
	struct tnode *bundle;
	int endline;
{
	register struct nl *p;
	register int i, b;
	int inp, out;
	struct tnode *blk;
	bool chkref;
	struct nl *iop;
	char *cp;
	extern int cntstat;
#	ifdef PC
	    struct entry_exit_cookie	eecookie;
#	endif PC
#	ifndef PC
	int var;
#	endif PC

	cntstat = 0;
/*
 *	yyoutline();
 */
	if (program != NIL)
		line = program->value[3];
	blk = bundle->stmnt_blck.stmnt_list;
	if (fp == NIL) {
		cbn--;
#		ifdef PTREE
		    nesting--;
#		endif PTREE
		return;
	}
#ifdef OBJ
	/*
	 * Patch the branch to the entry point of the function.
	 * Assure alignment of O_BEG structure.
	 */
	if (((int)lc & 02) == 0)
		word(0);
	patch4((PTR_DCL) fp->value[NL_ENTLOC]);
	/*
	 * Put out the block entrance code and the block name.
	 * HDRSZE is the number of bytes of info in the static
	 * BEG data area exclusive of the proc name. It is
	 * currently defined as:
	/*	struct hdr {
	/*		long framesze;	/* number of bytes of local vars */
	/*		long nargs;	/* number of bytes of arguments */
	/*		bool tests;	/* TRUE => perform runtime tests */
	/*		short offset;	/* offset of procedure in source file */
	/*		char name[1];	/* name of active procedure */
	/*	};
	 */
#	define HDRSZE (2 * sizeof(long) + sizeof(short) + sizeof(bool))
	var = put(2, ((lenstr(fp->symbol,0) + HDRSZE) << 8)
		| (cbn == 1 && opt('p') == 0 ? O_NODUMP: O_BEG), (long)0);
	    /*
	     *  output the number of bytes of arguments
	     *  this is only checked on formal calls.
	     */
	(void) put(2, O_CASE4, cbn == 1 ? (long)0 : (long)(fp->value[NL_OFFS]-DPOFF2));
	    /*
	     *	Output the runtime test mode for the routine
	     */
	(void) put(2, sizeof(bool) == 2 ? O_CASE2 : O_CASE4, opt('t') ? TRUE : FALSE);
	    /*
	     *	Output line number and routine name
	     */
	(void) put(2, O_CASE2, bundle->stmnt_blck.line_no);
	putstr(fp->symbol, 0);
#endif OBJ
#ifdef PC
	/*
	 * put out the procedure entry code
	 */
	eecookie.nlp = fp;
	if ( fp -> class == PROG ) {
		/*
		 *	If there is a label declaration in the main routine
		 *	then there may be a non-local goto to it that does
		 *	not appear in this module. We have to assume that
		 *	such a reference may occur and generate code to
		 *	prepare for it.
		 */
	    if ( parts[ cbn ] & LPRT ) {
		parts[ cbn ] |= ( NONLOCALVAR | NONLOCALGOTO );
	    }
	    codeformain();
	    ftnno = fp -> value[NL_ENTLOC];
	    prog_prologue(&eecookie);
	    stabline(bundle->stmnt_blck.line_no);
	    stabfunc(fp, "program", bundle->stmnt_blck.line_no , (long) 0 );
	} else {
	    ftnno = fp -> value[NL_ENTLOC];
	    fp_prologue(&eecookie);
	    stabline(bundle->stmnt_blck.line_no);
	    stabfunc(fp, fp->symbol, bundle->stmnt_blck.line_no,
		(long)(cbn - 1));
	    for ( p = fp -> chain ; p != NIL ; p = p -> chain ) {
		stabparam( p , p -> value[ NL_OFFS ] , (int) lwidth(p->type));
	    }
	    if ( fp -> class == FUNC ) {
		    /*
		     *	stab the function variable
		     */
		p = fp -> ptr[ NL_FVAR ];
		stablvar( p , p -> value[ NL_OFFS ] , (int) lwidth( p -> type));
	    }
		/*
		 *	stab local variables
		 *	rummage down hash chain links.
		 */
	    for ( i = 0 ; i <= 077 ; i++ ) {
		for ( p = disptab[ i ] ; p != NIL ; p = p->nl_next) {
		    if ( ( p -> nl_block & 037 ) != cbn ) {
			break;
		    }
		    /*
		     *	stab locals (not parameters)
		     */
		    }
		}
	    }
	}
	stablbrac( cbn );
	    /*
	     *	ask second pass to allocate known locals
	     */
	putlbracket(ftnno, &sizes[cbn]);
	fp_entrycode(&eecookie);
#endif PC
	if ( monflg ) {
		if ( fp -> value[ NL_CNTR ] != 0 ) {
			inccnt( fp -> value [ NL_CNTR ] );
		}
		inccnt( bodycnts[ fp -> nl_block & 037 ] );
	}
	if (fp->class == PROG) {
		/*
		 * The glorious buffers option.
		 *          0 = don't buffer output
		 *          1 = line buffer output
		 *          2 = 512 byte buffer output
		 */
#		ifdef OBJ
		    if (opt('b') != 1)
			    (void) put(1, O_BUFF | opt('b') << 8);
#		endif OBJ
#		ifdef PC
		    if ( opt( 'b' ) != 1 ) {
			putleaf( PCC_ICON , 0 , 0
				, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR ) , "_BUFF" );
			putleaf( PCC_ICON , opt( 'b' ) , 0 , PCCT_INT , (char *) 0 );
			putop( PCC_CALL , PCCT_INT );
			putdot( filename , line );
		    }
#		endif PC
		inp = 0;
		out = 0;
		for (p = fp->chain; p != NIL; p = p->chain) {
			if (pstrcmp(p->symbol, input->symbol) == 0) {
				inp++;
				continue;
			}
			if (pstrcmp(p->symbol, output->symbol) == 0) {
				out++;
				continue;
			}
			iop = lookup1(p->symbol);
			if (iop == NIL || bn != cbn) {
				error("File %s listed in program statement but not declared", p->symbol);
				continue;
			}
			if (iop->class != VAR) {
				error("File %s listed in program statement but declared as a %s", p->symbol, classes[iop->class]);
				continue;
			}
			if (iop->type == NIL)
				continue;
			if (iop->type->class != FILET) {
				error("File %s listed in program statement but defined as %s",
					p->symbol, nameof(iop->type));
				continue;
			}
#			ifdef OBJ
			    (void) put(2, O_CON24, text(iop->type) ? 0 : width(iop->type->type));
			    i = lenstr(p->symbol,0);
			    (void) put(2, O_CON24, i);
			    (void) put(2, O_LVCON, i);
			    putstr(p->symbol, 0);
			    (void) put(2, O_LV | bn<<8+INDX, (int)iop->value[NL_OFFS]);
			    (void) put(1, O_DEFNAME);
#			endif OBJ
#			ifdef PC
			    putleaf( PCC_ICON , 0 , 0
				    , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
				    , "_DEFNAME" );
			    putLV( p -> symbol , bn , iop -> value[NL_OFFS] ,
				    iop -> extra_flags , p2type( iop ) );
			    putCONG( p -> symbol , strlen( p -> symbol )
				    , LREQ );
			    putop( PCC_CM , PCCT_INT );
			    putleaf( PCC_ICON , strlen( p -> symbol )
				    , 0 , PCCT_INT , (char *) 0 );
			    putop( PCC_CM , PCCT_INT );
			    putleaf( PCC_ICON
				, text(iop->type) ? 0 : width(iop->type->type)
				, 0 , PCCT_INT , (char *) 0 );
			    putop( PCC_CM , PCCT_INT );
			    putop( PCC_CALL , PCCT_INT );
			    putdot( filename , line );
#			endif PC
		}
	}
	/*
	 * Process the prog/proc/func body
	 */
	noreach = FALSE;
	line = bundle->stmnt_blck.line_no;
	statlist(blk);
#	ifdef PTREE
	    {
		pPointer Body = tCopy( blk );

		pDEF( PorFHeader[ nesting -- ] ).PorFBody = Body;
	    }
#	endif PTREE
#	ifdef OBJ
	    if (cbn== 1 && monflg != FALSE) {
		    patchfil((PTR_DCL) (cntpatch - 2), (long)cnts, 2);
		    patchfil((PTR_DCL) (nfppatch - 2), (long)pfcnt, 2);
	    }
#	endif OBJ
#	ifdef PC
	    if ( fp -> class == PROG && monflg ) {
		putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			, "_PMFLUSH" );
		putleaf( PCC_ICON , cnts , 0 , PCCT_INT , (char *) 0 );
		putleaf( PCC_ICON , pfcnt , 0 , PCCT_INT , (char *) 0 );
		putop( PCC_CM , PCCT_INT );
		putLV( PCPCOUNT , 0 , 0 , NGLOBAL , PCCT_INT );
		putop( PCC_CM , PCCT_INT );
		putop( PCC_CALL , PCCT_INT );
		putdot( filename , line );
	    }
#	endif PC
	/*
	 * Clean up the symbol table displays and check for unresolves
	 */
	line = endline;
	if (fp->class == PROG && inp == 0 && (input->nl_flags & (NUSED|NMOD)) != 0) {
		recovered();
		error("Input is used but not defined in the program statement");
	}
	if (fp->class == PROG && out == 0 && (output->nl_flags & (NUSED|NMOD)) != 0) {
		recovered();
		error("Output is used but not defined in the program statement");
	}
	b = cbn;
	Fp = fp;
	chkref = (syneflg == errcnt[cbn] && opt('w') == 0)?TRUE:FALSE;
	for (i = 0; i <= 077; i++) {
		for (p = disptab[i]; p != NIL && (p->nl_block & 037) == b; p = p->nl_next) {
			/*
			 * Check for variables defined
			 * but not referenced 
			 */
			if (chkref && p->symbol != NIL)
			switch (p->class) {
				case FIELD:
					/*
					 * If the corresponding record is
					 * unused, we shouldn't complain about
					 * the fields.
					 */
				default:
					if ((p->nl_flags & (NUSED|NMOD)) == 0) {
						warning();
						nerror("%s %s is neither used nor set", classes[p->class], p->symbol);
						break;
					}
					/*
					 * If a var parameter is either
					 * modified or used that is enough.
					 */
					if (p->class == REF)
						continue;
#					ifdef OBJ
					    if ((p->nl_flags & NUSED) == 0) {
						warning();
						nerror("%s %s is never used", classes[p->class], p->symbol);
						break;
					    }
#					endif OBJ
#					ifdef PC
					    if (((p->nl_flags & NUSED) == 0) && ((p->extra_flags & NEXTERN) == 0)) {
						warning();
						nerror("%s %s is never used", classes[p->class], p->symbol);
						break;
					    }
#					endif PC
					if ((p->nl_flags & NMOD) == 0) {
						warning();
						nerror("%s %s is used but never set", classes[p->class], p->symbol);
						break;
					}
				case LABEL:
				case FVAR:
				case BADUSE:
					break;
			}
			switch (p->class) {
				case BADUSE:
					cp = "s";
					/* This used to say ud_next
					   that is not a member of nl so
					   i changed it to nl_next,
					   which may be wrong */
					if (p->chain->nl_next == NIL)
						cp++;
					eholdnl();
					if (p->value[NL_KINDS] & ISUNDEF)
						nerror("%s undefined on line%s", p->symbol, cp);
					else
						nerror("%s improperly used on line%s", p->symbol, cp);
					pnumcnt = 10;
					pnums((struct udinfo *) p->chain);
					pchr('\n');
					break;

				case FUNC:
				case PROC:
#					ifdef OBJ
					    if ((p->nl_flags & NFORWD))
						nerror("Unresolved forward declaration of %s %s", classes[p->class], p->symbol);
#					endif OBJ
#					ifdef PC
					    if ((p->nl_flags & NFORWD) && ((p->extra_flags & NEXTERN) == 0))
						nerror("Unresolved forward declaration of %s %s", classes[p->class], p->symbol);
#					endif PC
					break;

				case LABEL:
					if (p->nl_flags & NFORWD)
						nerror("label %s was declared but not defined", p->symbol);
					break;
				case FVAR:
					if ((p->nl_flags & NMOD) == 0)
						nerror("No assignment to the function variable");
					break;
			}
		}
		/*
		 * Pop this symbol
		 * table slot
		 */
		disptab[i] = p;
	}

#	ifdef OBJ
	    (void) put(1, O_END);
#	endif OBJ
#	ifdef PC
	    fp_exitcode(&eecookie);
	    stabrbrac(cbn);
	    putrbracket(ftnno);
	    fp_epilogue(&eecookie);
	    if (fp -> class != PROG) {
		fp_formalentry(&eecookie);
	    }
		/*
		 *	declare pcp counters, if any
		 */
	    if ( monflg && fp -> class == PROG ) {
		putprintf( "	.data" , 0 );
		aligndot(PCCT_INT);
		putprintf( "	.comm	" , 1 );
		putprintf( PCPCOUNT , 1 );
		putprintf( ",%d" , 0 , ( cnts + 1 ) * sizeof (long) );
		putprintf( "	.text" , 0 );
	    }
#	endif PC
#ifdef DEBUG
	dumpnl(fp->ptr[2], (int) fp->symbol);
#endif

#ifdef OBJ
	/*
	 * save the namelist for the debugger pdx
	 */

	savenl(fp->ptr[2], (int) fp->symbol);
#endif

	/*
	 * Restore the
	 * (virtual) name list
	 * position
	 */
	nlfree(fp->ptr[2]);
	/*
	 * Proc/func has been
	 * resolved
	 */
	fp->nl_flags &= ~NFORWD;
	/*
	 * Patch the beg
	 * of the proc/func to
	 * the proper variable size
	 */
	if (Fp == NIL)
		elineon();
#	ifdef OBJ
	    patchfil((PTR_DCL) var,
		roundup(-sizes[cbn].om_max, (long) A_STACK), 2);
#	endif OBJ
	cbn--;
	if (inpflist(fp->symbol)) {
		opop('l');
	}
}

#ifdef PC
    /*
     *	construct the long name of a function based on it's static nesting.
     *	into a caller-supplied buffer (that should be about BUFSIZ big).
     */
sextname( buffer , name , level )
    char	buffer[];
    char	*name;
    int		level;
{
    char	*starthere;
    int	i;

    starthere = &buffer[0];
    for ( i = 1 ; i < level ; i++ ) {
	sprintf( starthere , EXTFORMAT , enclosing[ i ] );
	starthere += strlen( enclosing[ i ] ) + 1;
    }
    sprintf( starthere , EXTFORMAT , name );
    starthere += strlen( name ) + 1;
    if ( starthere >= &buffer[ BUFSIZ ] ) {
	panic( "sextname" );
    }
}

    /*
     *	code for main()
     */
#ifdef vax

codeformain()
{
    putprintf("	.text" , 0 );
    putprintf("	.align	1" , 0 );
    putprintf("	.globl	_main" , 0 );
    putprintf("_main:" , 0 );
    putprintf("	.word	0" , 0 );
    if ( opt ( 't' ) ) {
	putprintf("	pushl	$1" , 0 );
    } else {
	putprintf("	pushl	$0" , 0 );
    }
    putprintf("	calls	$1,_PCSTART" , 0 );
    putprintf("	movl	4(ap),__argc" , 0 );
    putprintf("	movl	8(ap),__argv" , 0 );
    putprintf("	calls	$0,_program" , 0 );
    putprintf("	pushl	$0" , 0 );
    putprintf("	calls	$1,_PCEXIT" , 0 );
}
#endif vax

#ifdef tahoe
codeformain()
{
    putprintf("	.text" , 0 );
    putprintf("	.align	1" , 0 );
    putprintf("	.globl	_main" , 0 );
    putprintf("_main:" , 0 );
    putprintf("	.word	0" , 0 );
    if ( opt ( 't' ) ) {
	putprintf("	pushl	$1" , 0 );
    } else {
	putprintf("	pushl	$0" , 0 );
    }
    putprintf("	callf	$8,_PCSTART" , 0 );
    putprintf("	movl	4(fp),__argc" , 0 );
    putprintf("	movl	8(fp),__argv" , 0 );
    putprintf("	callf	$4,_program" , 0 );
    putprintf("	pushl	$0" , 0 );
    putprintf("	callf	$8,_PCEXIT" , 0 );
}
#endif tahoe

    /*
     *	prologue for the program.
     *	different because it
     *		doesn't have formal entry point
     */
#if defined(vax) || defined(tahoe) 
prog_prologue(eecookiep)
    struct entry_exit_cookie	*eecookiep;
{
    putprintf("	.text" , 0 );
    putprintf("	.align	1" , 0 );
    putprintf("	.globl	_program" , 0 );
    putprintf("_program:" , 0 );
	/*
	 *	register save mask
	 */
    eecookiep -> savlabel = (int) getlab();
    putprintf("	.word	%s%d", 0, (int) SAVE_MASK_LABEL , eecookiep -> savlabel );
}

fp_prologue(eecookiep)
    struct entry_exit_cookie	*eecookiep;
{

    sextname( eecookiep -> extname, eecookiep -> nlp -> symbol , cbn - 1 );
    putprintf( "	.text" , 0 );
    putprintf( "	.align	1" , 0 );
    putprintf( "	.globl	%s%s", 0, (int) FORMALPREFIX, (int) eecookiep -> extname );
    putprintf( "	.globl	%s" , 0 , (int) eecookiep -> extname );
    putprintf( "%s:" , 0 , (int) eecookiep -> extname );
	/*
	 *	register save mask
	 */
    eecookiep -> savlabel = (int) getlab();
    putprintf("	.word	%s%d", 0, (int) SAVE_MASK_LABEL , eecookiep -> savlabel );
}
#endif vax || tahoe

    /*
     *	code before any user code.
     *	or code that is machine dependent.
     */
#ifdef vax
fp_entrycode(eecookiep)
    struct entry_exit_cookie	*eecookiep;
{
    int	ftnno = eecookiep -> nlp -> value[NL_ENTLOC];
    int	proflabel = (int) getlab();
    int	setjmp0 = (int) getlab();

	/*
	 *	top of code;  destination of jump from formal entry code.
	 */
    eecookiep -> toplabel = (int) getlab();
    (void) putlab( (char *) eecookiep -> toplabel );
    putprintf("	subl2	$%s%d,sp" , 0 , (int) FRAME_SIZE_LABEL, ftnno );
    if ( profflag ) {
	    /*
	     *	call mcount for profiling
	     */
	putprintf( "	moval	" , 1 );
	putprintf( PREFIXFORMAT , 1 , (int) LABELPREFIX , proflabel );
	putprintf( ",r0" , 0 );
	putprintf( "	jsb	mcount" , 0 );
	putprintf( "	.data" , 0 );
	putprintf( "	.align	2" , 0 );
	(void) putlab( (char *) proflabel );
	putprintf( "	.long	0" , 0 );
	putprintf( "	.text" , 0 );
    }
	/*
	 *	if there are nested procedures that access our variables
	 *	we must save the display.
	 */
    if ( parts[ cbn ] & NONLOCALVAR ) {
	    /*
	     *	save old display 
	     */
	putprintf( "	movq	%s+%d,%d(%s)" , 0
		, (int) DISPLAYNAME , cbn * sizeof(struct dispsave)
		, DSAVEOFFSET , (int) P2FPNAME );
	    /*
	     *	set up new display by saving AP and FP in appropriate
	     *	slot in display structure.
	     */
	putprintf( "	movq	%s,%s+%d" , 0
		, (int) P2APNAME , (int) DISPLAYNAME , cbn * sizeof(struct dispsave) );
    }
	/*
	 *	set underflow checking if runtime tests
	 */
    if ( opt( 't' ) ) {
	putprintf( "	bispsw	$0xe0" , 0 );
    }
	/*
	 *	zero local variables if checking is on
	 *	by calling blkclr( bytes of locals , starting local address );
	 */
    if ( opt( 't' ) && ( -sizes[ cbn ].om_max ) > DPOFF1 ) {
	putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
		, "_blkclr" );
	putLV((char *) 0 , cbn , (int) sizes[ cbn ].om_max , NLOCAL , PCCT_CHAR );
	putleaf( PCC_ICON ,  (int) (( -sizes[ cbn ].om_max ) - DPOFF1)
		, 0 , PCCT_INT ,(char *) 0 );
	putop( PCC_CM , PCCT_INT );
	putop( PCC_CALL , PCCT_INT );
	putdot( filename , line );
    }
	/*
	 *  set up goto vector if non-local goto to this frame
	 */
    if ( parts[ cbn ] & NONLOCALGOTO ) {
	putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
		, "_setjmp" );
	putLV( (char *) 0 , cbn , GOTOENVOFFSET , NLOCAL , PCCTM_PTR|PCCT_STRTY );
	putop( PCC_CALL , PCCT_INT );
	putleaf( PCC_ICON , 0 , 0 , PCCT_INT , (char *) 0 );
	putop( PCC_NE , PCCT_INT );
	putleaf( PCC_ICON , setjmp0 , 0 , PCCT_INT , (char *) 0 );
	putop( PCC_CBRANCH , PCCT_INT );
	putdot( filename , line );
	    /*
	     *	on non-local goto, setjmp returns with address to
	     *	be branched to.
	     */
	putprintf( "	jmp	(r0)" , 0 );
	(void) putlab((char *) setjmp0);
    }
}
#endif vax

#ifdef tahoe
fp_entrycode(eecookiep)
    struct entry_exit_cookie	*eecookiep;
{
    int	ftnno = eecookiep -> nlp -> value[NL_ENTLOC];
    int	proflabel = (int) getlab();
    int	setjmp0 = (int) getlab();

	/*
	 *	top of code;  destination of jump from formal entry code.
	 */
    eecookiep -> toplabel = (int) getlab();
    (void) putlab( (char *) eecookiep -> toplabel );
    putprintf("	subl3	$%s%d,fp,sp" , 0 , (int) FRAME_SIZE_LABEL, ftnno );
    if ( profflag ) {
	    /*
	     *	call mcount for profiling
	     */
	putprintf( "	pushal	" , 1 );
	putprintf( PREFIXFORMAT , 0 , (int) LABELPREFIX , proflabel );
	putprintf( "	callf	$8,mcount" , 0 );
	putprintf( "	.data" , 0 );
	putprintf( "	.align	2" , 0 );
	(void) putlab( (char *) proflabel );
	putprintf( "	.long	0" , 0 );
	putprintf( "	.text" , 0 );
    }
	/*
	 *	if there are nested procedures that access our variables
	 *	we must save the display.
	 */
    if ( parts[ cbn ] & NONLOCALVAR ) {
	    /*
	     *	save old display 
	     */
	putprintf( "	movl	%s+%d,%d(%s)" , 0
		, (int) DISPLAYNAME , cbn * sizeof(struct dispsave)
		, DSAVEOFFSET , (int) P2FPNAME );
	    /*
	     *	set up new display by saving FP in appropriate
	     *	slot in display structure.
	     */
	putprintf( "	movl	%s,%s+%d" , 0
		, (int) P2FPNAME , (int) DISPLAYNAME , cbn * sizeof(struct dispsave) );
    }
	/*
	 *	set underflow checking if runtime tests
	 */
    if ( opt( 't' ) ) {
	putprintf( "	bicpsw	$0x20" , 0 );
    }
	/*
	 *	zero local variables if checking is on
	 *	by calling blkclr( bytes of locals , starting local address );
	 */
    if ( opt( 't' ) && ( -sizes[ cbn ].om_max ) > DPOFF1 ) {
	putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
		, "_blkclr" );
	putLV((char *) 0 , cbn , (int) sizes[ cbn ].om_max , NLOCAL , PCCT_CHAR );
	putleaf( PCC_ICON ,  (int) (( -sizes[ cbn ].om_max ) - DPOFF1)
		, 0 , PCCT_INT ,(char *) 0 );
	putop( PCC_CM , PCCT_INT );
	putop( PCC_CALL , PCCT_INT );
	putdot( filename , line );
    }
	/*
	 *  set up goto vector if non-local goto to this frame
	 */
    if ( parts[ cbn ] & NONLOCALGOTO ) {
	putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
		, "_setjmp" );
	putLV( (char *) 0 , cbn , GOTOENVOFFSET , NLOCAL , PCCTM_PTR|PCCT_STRTY );
	putop( PCC_CALL , PCCT_INT );
	putleaf( PCC_ICON , 0 , 0 , PCCT_INT , (char *) 0 );
	putop( PCC_NE , PCCT_INT );
	putleaf( PCC_ICON , setjmp0 , 0 , PCCT_INT , (char *) 0 );
	putop( PCC_CBRANCH , PCCT_INT );
	putdot( filename , line );
	    /*
	     *	on non-local goto, setjmp returns with address to
	     *	be branched to.
	     */
	putprintf( "	jmp	(r0)" , 0 );
	(void) putlab((char *) setjmp0);
    }
}
#endif tahoe

#if defined(vax) || defined(tahoe)
fp_exitcode(eecookiep)
    struct entry_exit_cookie	*eecookiep;
{
	/*
	 *	if there were file variables declared at this level
	 *	call PCLOSE( ap ) to clean them up.
	 */
    if ( dfiles[ cbn ] ) {
	putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
		, "_PCLOSE" );
	putleaf( PCC_REG , 0 , P2AP , PCCM_ADDTYPE( PCCT_CHAR , PCCTM_PTR ) , (char *) 0 );
	putop( PCC_CALL , PCCT_INT );
	putdot( filename , line );
    }
	/*
	 *	if this is a function,
	 *	the function variable is the return value.
	 *	if it's a scalar valued function, return scalar,
	 *	else, return a pointer to the structure value.
	 */
    if ( eecookiep-> nlp -> class == FUNC ) {
	struct nl	*fvar = eecookiep-> nlp -> ptr[ NL_FVAR ];
	long		fvartype = p2type( fvar -> type );
	long		label;
	char		labelname[ BUFSIZ ];

	switch ( classify( fvar -> type ) ) {
	    case TBOOL:
	    case TCHAR:
	    case TINT:
	    case TSCAL:
	    case TDOUBLE:
	    case TPTR:
		putRV( fvar -> symbol , ( fvar -> nl_block ) & 037 ,
			fvar -> value[ NL_OFFS ] ,
			fvar -> extra_flags ,
			(int) fvartype );
		putop( PCC_FORCE , (int) fvartype );
		break;
	    default:
		label = (int) getlab();
		sprintf( labelname , PREFIXFORMAT , LABELPREFIX , label );
		putprintf( "	.data" , 0 );
		aligndot(A_STRUCT);
		putprintf( "	.lcomm	%s,%d" , 0 ,
			    (int) labelname , (int) lwidth( fvar -> type ) );
		putprintf( "	.text" , 0 );
		putleaf( PCC_NAME , 0 , 0 , (int) fvartype , labelname );
		putLV( fvar -> symbol , ( fvar -> nl_block ) & 037 ,
			fvar -> value[ NL_OFFS ] ,
			fvar -> extra_flags ,
			(int) fvartype );
		putstrop( PCC_STASG , (int) PCCM_ADDTYPE(fvartype, PCCTM_PTR) ,
			(int) lwidth( fvar -> type ) ,
			align( fvar -> type ) );
		putdot( filename , line );
		putleaf( PCC_ICON , 0 , 0 , (int) PCCM_ADDTYPE(fvartype, PCCTM_PTR), labelname );
		putop( PCC_FORCE , (int) PCCM_ADDTYPE(fvartype, PCCTM_PTR) );
		break;
	}
	putdot( filename , line );
    }
	/*
	 *	if there are nested procedures we must save the display.
	 */
    if ( parts[ cbn ] & NONLOCALVAR ) {
	    /*
	     *	restore old display entry from save area
	     */
#ifdef vax
	putprintf( "	movq	%d(%s),%s+%d" , 0
	    , DSAVEOFFSET , (int) P2FPNAME
	    , (int) DISPLAYNAME , cbn * sizeof(struct dispsave) );
#endif
#ifdef tahoe
	putprintf( "	movl	%d(%s),%s+%d" , 0
	    , DSAVEOFFSET , (int) P2FPNAME
	    , (int) DISPLAYNAME , cbn * sizeof(struct dispsave) );
#endif
    }
}
#endif vax || tahoe

#if defined(vax) || defined(tahoe)
fp_epilogue(eecookiep)
    struct entry_exit_cookie	*eecookiep;
{
    stabline(line);
    putprintf("	ret" , 0 );
	/*
	 *	set the register save mask.
	 */
    putprintf("	.set	%s%d,0x%x", 0,
		(int) SAVE_MASK_LABEL, eecookiep -> savlabel, savmask());
}
#endif vax || tahoe

#if defined(vax) || defined(tahoe)
fp_formalentry(eecookiep)
    struct entry_exit_cookie	*eecookiep;
{

    putprintf("	.align 1", 0);
    putprintf("%s%s:" , 0 , (int) FORMALPREFIX , (int) eecookiep -> extname );
    putprintf("	.word	%s%d", 0, (int) SAVE_MASK_LABEL, eecookiep -> savlabel );
    putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR ) , "_FCALL" );
    putRV((char *) 0 , cbn ,
	eecookiep -> nlp -> value[ NL_OFFS ] + sizeof( struct formalrtn * ) ,
	NPARAM , PCCTM_PTR | PCCT_STRTY );
    putRV((char *) 0, cbn, eecookiep -> nlp -> value[NL_OFFS], NPARAM, PCCTM_PTR|PCCT_STRTY);
    putop( PCC_CM , PCCT_INT );
    putop( PCC_CALL , PCCT_INT );
    putdot( filename , line );
    putjbr( (long) eecookiep -> toplabel );
}
#endif vax || tahoe

#ifdef mc68000

codeformain()
{
    putprintf("	.text", 0);
    putprintf("	.globl	_main", 0);
    putprintf("_main:", 0);
    putprintf("	link	%s,#0", 0, P2FPNAME);
    if (opt('t')) {
	putprintf("	pea	1", 0);
    } else {
	putprintf("	pea	0", 0);
    }
    putprintf("	jbsr	_PCSTART", 0);
    putprintf("	addql	#4,sp", 0);
    putprintf("	movl	%s@(8),__argc", 0, P2FPNAME);
    putprintf("	movl	%s@(12),__argv", 0, P2FPNAME);
    putprintf("	jbsr	_program", 0);
    putprintf("	pea	0", 0);
    putprintf("	jbsr	_PCEXIT", 0);
}

prog_prologue(eecookiep)
    struct entry_exit_cookie	*eecookiep;
{
    int	ftnno = eecookiep -> nlp -> value[NL_ENTLOC];

    putprintf("	.text", 0);
    putprintf("	.globl	_program", 0);
    putprintf("_program:", 0);
    putprintf("	link	%s,#0", 0, P2FPNAME);
    putprintf("	addl	#-%s%d,sp", 0, FRAME_SIZE_LABEL, ftnno);
	/* touch new end of stack, to break more stack space */
    putprintf("	tstb	sp@(-%s%d)", 0, PAGE_BREAK_LABEL, ftnno);
    putprintf("	moveml	#%s%d,sp@", 0, SAVE_MASK_LABEL, ftnno);
}

fp_prologue(eecookiep)
    struct entry_exit_cookie	*eecookiep;
{
    int		ftnno = eecookiep -> nlp -> value[NL_ENTLOC];

    sextname(eecookiep -> extname, eecookiep -> nlp -> symbol, cbn - 1);
    putprintf("	.text", 0);
    putprintf("	.globl	%s%s", 0, FORMALPREFIX, eecookiep -> extname);
    putprintf("	.globl	%s", 0, eecookiep -> extname);
    putprintf("%s:", 0, eecookiep -> extname);
    putprintf("	link	%s,#0", 0, P2FPNAME);
    putprintf("	addl	#-%s%d,sp", 0, FRAME_SIZE_LABEL, ftnno);
	/* touch new end of stack, to break more stack space */
    putprintf("	tstb	sp@(-%s%d)", 0, PAGE_BREAK_LABEL, ftnno);
    putprintf("	moveml	#%s%d,sp@", 0, SAVE_MASK_LABEL, ftnno);
}

fp_entrycode(eecookiep)
    struct entry_exit_cookie	*eecookiep;
{
    char *proflabel = getlab();
    char *setjmp0 = getlab();

	/*
	 *	fill in the label cookie
	 */
    eecookiep -> toplabel = getlab();
    (void) putlab(eecookiep -> toplabel);
	/*
	 *	call mcount if we are profiling.
	 */
    if ( profflag ) {
	putprintf("	movl	#%s%d,a0", 0, LABELPREFIX,  proflabel);
	putprintf("	jsr	mcount", 0);
	putprintf("	.data", 0);
	putprintf("	.even", 0);
	(void) putlab(proflabel);
	putprintf("	.long	0", 0);
	putprintf("	.text", 0);
    }
	/*
	 *	if there are nested procedures that access our variables
	 *	we must save the display
	 */
    if (parts[cbn] & NONLOCALVAR) {
	    /*
	     *	save the old display
	     */
	putprintf("	movl	%s+%d,%s@(%d)", 0,
		    DISPLAYNAME, cbn * sizeof(struct dispsave),
		    P2FPNAME, DSAVEOFFSET);
	    /*
	     *	set up the new display by saving the framepointer
	     *	in the display structure.
	     */
	putprintf("	movl	%s,%s+%d", 0,
		    P2FPNAME, DISPLAYNAME, cbn * sizeof(struct dispsave));
    }
	/*
	 *	zero local variables if checking is on
	 *	by calling blkclr( bytes of locals , starting local address );
	 */
    if ( opt( 't' ) && ( -sizes[ cbn ].om_max ) > DPOFF1 ) {
	putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
		, "_blkclr" );
	putLV( 0 , cbn , sizes[ cbn ].om_max , NLOCAL , PCCT_CHAR );
	putleaf( PCC_ICON ,  ( -sizes[ cbn ].om_max ) - DPOFF1
		, 0 , PCCT_INT , 0 );
	putop( PCC_CM , PCCT_INT );
	putop( PCC_CALL , PCCT_INT );
	putdot( filename , line );
    }
	/*
	 *  set up goto vector if non-local goto to this frame
	 */
    if ( parts[ cbn ] & NONLOCALGOTO ) {
	putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
		, "_setjmp" );
	putLV( 0 , cbn , GOTOENVOFFSET , NLOCAL , PCCTM_PTR|PCCT_STRTY );
	putop( PCC_CALL , PCCT_INT );
	putleaf( PCC_ICON , 0 , 0 , PCCT_INT , 0 );
	putop( PCC_NE , PCCT_INT );
	putleaf( PCC_ICON , setjmp0 , 0 , PCCT_INT , 0 );
	putop( PCC_CBRANCH , PCCT_INT );
	putdot( filename , line );
	    /*
	     *	on non-local goto, setjmp returns with address to
	     *	be branched to.
	     */
	putprintf("	movl	d0,a0", 0);
	putprintf("	jmp	a0@", 0);
	(void) putlab(setjmp0);
    }
}

fp_exitcode(eecookiep)
    struct entry_exit_cookie	*eecookiep;
{
	/*
	 *	if there were file variables declared at this level
	 *	call PCLOSE( ap ) to clean them up.
	 */
    if ( dfiles[ cbn ] ) {
	putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
		, "_PCLOSE" );
	putleaf( PCC_REG , 0 , P2AP , PCCM_ADDTYPE( PCCT_CHAR , PCCTM_PTR ) , 0 );
	putop( PCC_CALL , PCCT_INT );
	putdot( filename , line );
    }
	/*
	 *	if this is a function,
	 *	the function variable is the return value.
	 *	if it's a scalar valued function, return scalar,
	 *	else, return a pointer to the structure value.
	 */
    if ( eecookiep -> nlp -> class == FUNC ) {
	struct nl	*fvar = eecookiep -> nlp -> ptr[ NL_FVAR ];
	long		fvartype = p2type( fvar -> type );
	char		*label;
	char		labelname[ BUFSIZ ];

	switch ( classify( fvar -> type ) ) {
	    case TBOOL:
	    case TCHAR:
	    case TINT:
	    case TSCAL:
	    case TDOUBLE:
	    case TPTR:
		putRV( fvar -> symbol , ( fvar -> nl_block ) & 037 ,
			fvar -> value[ NL_OFFS ] ,
			fvar -> extra_flags ,
			fvartype );
		putop( PCC_FORCE , fvartype );
		break;
	    default:
		label = getlab();
		sprintf( labelname , PREFIXFORMAT , LABELPREFIX , label );
		putprintf("	.lcomm	%s,%d", 0,
			labelname, lwidth(fvar -> type));
		putleaf( PCC_NAME , 0 , 0 , fvartype , labelname );
		putLV( fvar -> symbol , ( fvar -> nl_block ) & 037 ,
			fvar -> value[ NL_OFFS ] ,
			fvar -> extra_flags ,
			fvartype );
		putstrop( PCC_STASG , PCCM_ADDTYPE(fvartype, PCCTM_PTR) ,
			lwidth( fvar -> type ) ,
			align( fvar -> type ) );
		putdot( filename , line );
		putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE(fvartype, PCCTM_PTR), labelname );
		putop( PCC_FORCE , PCCM_ADDTYPE(fvartype, PCCTM_PTR) );
		break;
	}
	putdot( filename , line );
    }
	/*
	 *	if we saved a display, we must restore it.
	 */
    if ( parts[ cbn ] & NONLOCALVAR ) {
	    /*
	     *	restore old display entry from save area
	     */
	putprintf("	movl	%s@(%d),%s+%d", 0,
		    P2FPNAME, DSAVEOFFSET,
		    DISPLAYNAME, cbn * sizeof(struct dispsave));
    }
}

fp_epilogue(eecookiep)
    struct entry_exit_cookie	*eecookiep;
{
    /*
     *	all done by the second pass.
     */
}

fp_formalentry(eecookiep)
    struct entry_exit_cookie	*eecookiep;
{
    putprintf( "%s%s:" , 0 , FORMALPREFIX , eecookiep -> extname );
    putprintf("	link	%s,#0", 0, P2FPNAME);
    putprintf("	addl	#-%s%d,sp", 0, FRAME_SIZE_LABEL, ftnno);
	/* touch new end of stack, to break more stack space */
    putprintf("	tstb	sp@(-%s%d)", 0, PAGE_BREAK_LABEL, ftnno);
    putprintf("	moveml	#%s%d,sp@", 0, SAVE_MASK_LABEL, ftnno);
    putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR ) , "_FCALL" );
    putRV( 0 , cbn ,
	eecookiep -> nlp -> value[ NL_OFFS ] + sizeof( struct formalrtn * ) ,
	NPARAM , PCCTM_PTR | PCCT_STRTY );
    putRV(0, cbn, eecookiep -> nlp -> value[NL_OFFS], NPARAM, PCCTM_PTR|PCCT_STRTY);
    putop( PCC_CM , PCCT_INT );
    putop( PCC_CALL , PCCT_INT );
    putdot( filename , line );
    putjbr( eecookiep -> toplabel );
}
#endif mc68000
#endif PC
