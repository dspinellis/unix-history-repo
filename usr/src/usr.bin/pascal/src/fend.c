/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)fend.c 1.16 %G%";

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"
#include "align.h"

/*
 * this array keeps the pxp counters associated with
 * functions and procedures, so that they can be output
 * when their bodies are encountered
 */
int	bodycnts[ DSPLYSZ ];

#ifdef PC
#   include "pc.h"
#   include "pcops.h"
#endif PC

#ifdef OBJ
int	cntpatch;
int	nfppatch;
#endif OBJ

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
	int *bundle;
	int endline;
{
	register struct nl *p;
	register int i, b;
	int var, inp, out, *blk;
	bool chkref;
	struct nl *iop;
	char *cp;
	extern int cntstat;
#	ifdef PC
	    int		savlabel = getlab();
	    int		toplabel = getlab();
	    int		proflabel = getlab();
	    int		skip = getlab();
	    char	extname[ BUFSIZ ];
#	endif PC

	cntstat = 0;
/*
 *	yyoutline();
 */
	if (program != NIL)
		line = program->value[3];
	blk = bundle[2];
	if (fp == NIL) {
		cbn--;
#		ifdef PTREE
		    nesting--;
#		endif PTREE
		return;
	}
#ifdef OBJ
	/*
	 * Patch the branch to the
	 * entry point of the function
	 */
	patch4(fp->value[NL_ENTLOC]);
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
	put(2, O_CASE4, cbn == 1 ? (long)0 : (long)(fp->value[NL_OFFS]-DPOFF2));
	    /*
	     *	Output the runtime test mode for the routine
	     */
	put(2, sizeof(bool) == 2 ? O_CASE2 : O_CASE4, opt('t') ? TRUE : FALSE);
	    /*
	     *	Output line number and routine name
	     */
	put(2, O_CASE2, bundle[1]);
	putstr(fp->symbol, 0);
#endif OBJ
#ifdef PC
	/*
	 * put out the procedure entry code
	 */
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
	    putprintf( "	.text" , 0 );
	    putprintf( "	.align	1" , 0 );
	    putprintf( "	.globl	_main" , 0 );
	    putprintf( "_main:" , 0 );
	    putprintf( "	.word	0" , 0 );
	    if ( opt ( 't' ) ) {
	        putprintf( "	pushl	$1" , 0 );
	    } else {
	        putprintf( "	pushl	$0" , 0 );
	    }
	    putprintf( "	calls	$1,_PCSTART" , 0 );
	    putprintf( "	movl	4(ap),__argc" , 0 );
	    putprintf( "	movl	8(ap),__argv" , 0 );
	    putprintf( "	calls	$0,_program" , 0 );
	    putprintf( "	pushl	$0" , 0 );
	    putprintf( "	calls	$1,_PCEXIT" , 0 );
	    ftnno = fp -> value[NL_ENTLOC];
	    putprintf( "	.text" , 0 );
	    putprintf( "	.align	1" , 0 );
	    putprintf( "	.globl	_program" , 0 );
	    putprintf( "_program:" , 0 );
	    stabfunc( "program" , fp -> class , bundle[1] , 0 );
	} else {
	    ftnno = fp -> value[NL_ENTLOC];
	    putprintf( "	.text" , 0 );
	    putprintf( "	.align	1" , 0 );
	    sextname( extname , fp -> symbol , cbn - 1 );
	    putprintf( "	.globl	%s%s" , 0 , FORMALPREFIX , extname );
	    putprintf( "	.globl	%s" , 0 , extname );
	    putprintf( "%s:" , 0 , extname );
	    stabfunc( fp -> symbol , fp -> class , bundle[1] , cbn - 1 );
	    for ( p = fp -> chain ; p != NIL ; p = p -> chain ) {
		stabparam( p -> symbol , p2type( p -> type )
			    , p -> value[ NL_OFFS ] , lwidth( p -> type ) );
	    }
	    if ( fp -> class == FUNC ) {
		    /*
		     *	stab the function variable
		     */
		p = fp -> ptr[ NL_FVAR ];
		stablvar( p -> symbol , p2type( p -> type ) , cbn 
			, p -> value[ NL_OFFS ] , lwidth( p -> type ) );
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
		     *	stab local variables
		     *	that's named variables, but not params
		     */
		    if (   ( p -> symbol != NIL ) 
			&& ( p -> class == VAR )
			&& ( p -> value[ NL_OFFS ] < 0 ) ) {
			stablvar( p -> symbol , p2type( p -> type ) , cbn 
			    , p -> value[ NL_OFFS ] , lwidth( p -> type ) );
		    }
		}
	    }
	}
	stablbrac( cbn );
	    /*
	     *	register save mask
	     */
	putprintf( "	.word	" , 1 );
        putprintf( PREFIXFORMAT , 0 , LABELPREFIX , savlabel );
	putlab( toplabel );
	putprintf( "	subl2	$LF%d,sp" , 0 , ftnno );
	if ( profflag ) {
		/*
		 *	call mcount for profiling
		 */
	    putprintf( "	moval	" , 1 );
	    putprintf( PREFIXFORMAT , 1 , LABELPREFIX , proflabel );
	    putprintf( ",r0" , 0 );
	    putprintf( "	jsb	mcount" , 0 );
	    putprintf( "	.data" , 0 );
	    putprintf( "	.align	2" , 0 );
	    putlab( proflabel );
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
		    , DISPLAYNAME , cbn * sizeof(struct dispsave)
		    , DSAVEOFFSET , P2FPNAME );
		/*
		 *	set up new display by saving AP and FP in appropriate
		 *	slot in display structure.
		 */
	    putprintf( "	movq	%s,%s+%d" , 0
		    , P2APNAME , DISPLAYNAME , cbn * sizeof(struct dispsave) );
	}
	    /*
	     *	set underflow checking if runtime tests
	     */
	if ( opt( 't' ) ) {
	    putprintf( "	bispsw	$0xe0" , 0 );
	}
	    /*
	     *	ask second pass to allocate known locals
	     */
	putlbracket( ftnno , -sizes[ cbn ].om_max );
	    /*
	     *	and zero them if checking is on
	     *	by calling blkclr( bytes of locals , starting local address );
	     */
	if ( opt( 't' ) && ( -sizes[ cbn ].om_max ) > DPOFF1 ) {
	    putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
		    , "_blkclr" );
	    putleaf( P2ICON ,  ( -sizes[ cbn ].om_max ) - DPOFF1
		    , 0 , P2INT , 0 );
	    putLV( 0 , cbn , sizes[ cbn ].om_max , NLOCAL , P2CHAR );
	    putop( P2LISTOP , P2INT );
	    putop( P2CALL , P2INT );
	    putdot( filename , line );
	}
	    /*
	     *  set up goto vector if non-local goto to this frame
	     */
	if ( parts[ cbn ] & NONLOCALGOTO ) {
	    putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
		    , "_setjmp" );
	    putLV( 0 , cbn , GOTOENVOFFSET , NLOCAL , P2PTR|P2STRTY );
	    putop( P2CALL , P2INT );
	    putleaf( P2ICON , 0 , 0 , P2INT , 0 );
	    putop( P2NE , P2INT );
	    putleaf( P2ICON , skip , 0 , P2INT , 0 );
	    putop( P2CBRANCH , P2INT );
	    putdot( filename , line );
		/*
		 *	on non-local goto, setjmp returns with address to
		 *	be branched to.
		 */
	    putprintf( "	jmp	(r0)" , 0 );
	    putlab(skip);
	}
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
			    put(1, O_BUFF | opt('b') << 8);
#		endif OBJ
#		ifdef PC
		    if ( opt( 'b' ) != 1 ) {
			putleaf( P2ICON , 0 , 0
				, ADDTYPE( P2FTN | P2INT , P2PTR ) , "_BUFF" );
			putleaf( P2ICON , opt( 'b' ) , 0 , P2INT , 0 );
			putop( P2CALL , P2INT );
			putdot( filename , line );
		    }
#		endif PC
		inp = 0;
		out = 0;
		for (p = fp->chain; p != NIL; p = p->chain) {
			if (strcmp(p->symbol, input->symbol) == 0) {
				inp++;
				continue;
			}
			if (strcmp(p->symbol, output->symbol) == 0) {
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
			    put(2, O_CON24, text(iop->type) ? 0 : width(iop->type->type));
			    i = lenstr(p->symbol,0);
			    put(2, O_CON24, i);
			    put(2, O_LVCON, i);
			    putstr(p->symbol, 0);
			    put(2, O_LV | bn<<8+INDX, (int)iop->value[NL_OFFS]);
			    put(1, O_DEFNAME);
#			endif OBJ
#			ifdef PC
			    putleaf( P2ICON , 0 , 0
				    , ADDTYPE( P2FTN | P2INT , P2PTR )
				    , "_DEFNAME" );
			    putLV( p -> symbol , bn , iop -> value[NL_OFFS] ,
				    iop -> extra_flags , p2type( iop ) );
			    putCONG( p -> symbol , strlen( p -> symbol )
				    , LREQ );
			    putop( P2LISTOP , P2INT );
			    putleaf( P2ICON , strlen( p -> symbol )
				    , 0 , P2INT , 0 );
			    putop( P2LISTOP , P2INT );
			    putleaf( P2ICON
				, text(iop->type) ? 0 : width(iop->type->type)
				, 0 , P2INT , 0 );
			    putop( P2LISTOP , P2INT );
			    putop( P2CALL , P2INT );
			    putdot( filename , line );
#			endif PC
		}
	}
	/*
	 * Process the prog/proc/func body
	 */
	noreach = 0;
	line = bundle[1];
	statlist(blk);
#	ifdef PTREE
	    {
		pPointer Body = tCopy( blk );

		pDEF( PorFHeader[ nesting -- ] ).PorFBody = Body;
	    }
#	endif PTREE
#	ifdef OBJ
	    if (cbn== 1 && monflg != 0) {
		    patchfil(cntpatch - 2, (long)cnts, 2);
		    patchfil(nfppatch - 2, (long)pfcnt, 2);
	    }
#	endif OBJ
#	ifdef PC
	    if ( fp -> class == PROG && monflg ) {
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
			, "_PMFLUSH" );
		putleaf( P2ICON , cnts , 0 , P2INT , 0 );
		putleaf( P2ICON , pfcnt , 0 , P2INT , 0 );
		putop( P2LISTOP , P2INT );
		putLV( PCPCOUNT , 0 , 0 , NGLOBAL , P2INT );
		putop( P2LISTOP , P2INT );
		putop( P2CALL , P2INT );
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
	chkref = syneflg == errcnt[cbn] && opt('w') == 0;
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
					if (p->chain->ud_next == NIL)
						cp++;
					eholdnl();
					if (p->value[NL_KINDS] & ISUNDEF)
						nerror("%s undefined on line%s", p->symbol, cp);
					else
						nerror("%s improperly used on line%s", p->symbol, cp);
					pnumcnt = 10;
					pnums(p->chain);
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
	    put(1, O_END);
#	endif OBJ
#	ifdef PC
		/*
		 *	if there were file variables declared at this level
		 *	call PCLOSE( ap ) to clean them up.
		 */
	    if ( dfiles[ cbn ] ) {
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
			, "_PCLOSE" );
		putleaf( P2REG , 0 , P2AP , ADDTYPE( P2CHAR , P2PTR ) , 0 );
		putop( P2CALL , P2INT );
		putdot( filename , line );
	    }
		/*
		 *	if this is a function,
		 *	the function variable is the return value.
		 *	if it's a scalar valued function, return scalar,
		 *	else, return a pointer to the structure value.
		 */
	    if ( fp -> class == FUNC ) {
		struct nl	*fvar = fp -> ptr[ NL_FVAR ];
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
				fvartype );
			break;
		    default:
			label = getlab();
			sprintf( labelname , PREFIXFORMAT ,
				LABELPREFIX , label );
			putprintf( "	.data" , 0 );
			putprintf( "	.lcomm	%s,%d" , 0 ,
				    labelname , lwidth( fvar -> type ) );
			putprintf( "	.text" , 0 );
			putleaf( P2NAME , 0 , 0 , fvartype , labelname );
			putLV( fvar -> symbol , ( fvar -> nl_block ) & 037 ,
				fvar -> value[ NL_OFFS ] ,
				fvar -> extra_flags ,
				fvartype );
			putstrop( P2STASG , fvartype , lwidth( fvar -> type ) ,
				align( fvar -> type ) );
			putdot( filename , line );
			putleaf( P2ICON , 0 , 0 , fvartype , labelname );
			break;
		}
		putop( P2FORCE , fvartype );
		putdot( filename , line );
	    }
		/*
		 *	if there are nested procedures we must save the display.
		 */
	    if ( parts[ cbn ] & NONLOCALVAR ) {
		    /*
		     *	restore old display entry from save area
		     */
		putprintf( "	movq	%d(%s),%s+%d" , 0
		    , DSAVEOFFSET , P2FPNAME
		    , DISPLAYNAME , cbn * sizeof(struct dispsave) );
	    }
	    stabrbrac( cbn );
	    putprintf( "	ret" , 0 );
		/*
		 *	let the second pass allocate locals
		 * 	and registers
		 */
	    putprintf( "	.set	" , 1 );	
	    putprintf( PREFIXFORMAT , 1 , LABELPREFIX , savlabel );
	    putprintf( ", 0x%x" , 0 , savmask() );
	    putrbracket( ftnno );
		/*
		 *  put down the entry point for formal calls
		 *  the arguments for FCALL have been passed to us
		 *  as hidden parameters after the regular arguments.
		 */
	    if ( fp -> class != PROG ) {
		putprintf( "%s%s:" , 0 , FORMALPREFIX , extname );
		putprintf( "	.word	" , 1 );
		putprintf( PREFIXFORMAT , 0 , LABELPREFIX , savlabel );
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR ) ,
			"_FCALL" );
		putRV( 0 , cbn ,
		    fp -> value[ NL_OFFS ] + sizeof( struct formalrtn * ) ,
		    NPARAM ,
		    P2PTR | P2STRTY );
		putRV( 0 , cbn , fp -> value[ NL_OFFS ] ,
			NPARAM , P2PTR|P2STRTY );
		putop( P2LISTOP , P2INT );
		putop( P2CALL , P2INT );
		putdot( filename , line );
		putjbr( toplabel );
	    }
		/*
		 *	declare pcp counters, if any
		 */
	    if ( monflg && fp -> class == PROG ) {
		putprintf( "	.data" , 0 );
		putprintf( "	.comm	" , 1 );
		putprintf( PCPCOUNT , 1 );
		putprintf( ",%d" , 0 , ( cnts + 1 ) * sizeof (long) );
		putprintf( "	.text" , 0 );
	    }
#	endif PC
#ifdef DEBUG
	dumpnl(fp->ptr[2], fp->symbol);
#endif

#ifdef OBJ
	/*
	 * save the namelist for the debugger pdx
	 */

	savenl(fp->ptr[2], fp->symbol);
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
	    patchfil(var, (long)(-sizes[cbn].om_max), 2);
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
#endif PC
