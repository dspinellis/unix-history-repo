/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)pcproc.c	5.1 (Berkeley) 6/5/85";
#endif not lint

#include "whoami.h"
#ifdef PC
    /*
     * and to the end of the file
     */
#include "0.h"
#include "tree.h"
#include "objfmt.h"
#include "opcode.h"
#include "pc.h"
#include <pcc.h>
#include "tmps.h"
#include "tree_ty.h"

/*
 * The constant EXPOSIZE specifies the number of digits in the exponent
 * of real numbers.
 *
 * The constant REALSPC defines the amount of forced padding preceeding
 * real numbers when they are printed. If REALSPC == 0, then no padding
 * is added, REALSPC == 1 adds one extra blank irregardless of the width
 * specified by the user.
 *
 * N.B. - Values greater than one require program mods.
 */
#define EXPOSIZE	2
#define	REALSPC		0

/*
 * The following array is used to determine which classes may be read
 * from textfiles. It is indexed by the return value from classify.
 */
#define rdops(x) rdxxxx[(x)-(TFIRST)]

int rdxxxx[] = {
	0,		/* -7 file types */
	0,		/* -6 record types */
	0,		/* -5 array types */
	O_READE,	/* -4 scalar types */
	0,		/* -3 pointer types */
	0,		/* -2 set types */
	0,		/* -1 string types */
	0,		/*  0 nil, no type */
	O_READE,	/*  1 boolean */
	O_READC,	/*  2 character */
	O_READ4,	/*  3 integer */
	O_READ8		/*  4 real */
};

/*
 * Proc handles procedure calls.
 * Non-builtin procedures are "buck-passed" to func (with a flag
 * indicating that they are actually procedures.
 * builtin procedures are handled here.
 */
pcproc(r)
	struct tnode *r;	/* T_PCALL */
{
	register struct nl *p;
	register struct tnode *alv, *al;
	register op;
	struct nl *filetype, *ap;
	int argc, typ, fmtspec, strfmt;
	struct tnode *argv, *file;
	char fmt, format[20], *strptr, *cmd;
	int prec, field, strnglen, fmtstart;
	char *pu;
	struct tnode *pua, *pui, *puz;
	int i, j, k;
	int itemwidth;
	char		*readname;
	struct nl	*tempnlp;
	long		readtype;
	struct tmps	soffset;
	bool		soffset_flag;

#define	CONPREC 4
#define	VARPREC 8
#define	CONWIDTH 1
#define	VARWIDTH 2
#define SKIP 16

	/*
	 * Verify that the name is
	 * defined and is that of a
	 * procedure.
	 */
	p = lookup(r->pcall_node.proc_id);
	if (p == NLNIL) {
		rvlist(r->pcall_node.arg);
		return;
	}
	if (p->class != PROC && p->class != FPROC) {
		error("Can't call %s, its %s not a procedure", p->symbol, classes[p->class]);
		rvlist(r->pcall_node.arg);
		return;
	}
	argv = r->pcall_node.arg;

	/*
	 * Call handles user defined
	 * procedures and functions.
	 */
	if (bn != 0) {
		(void) call(p, argv, PROC, bn);
		return;
	}

	/*
	 * Call to built-in procedure.
	 * Count the arguments.
	 */
	argc = 0;
	for (al = argv; al != TR_NIL; al = al->list_node.next)
		argc++;

	/*
	 * Switch on the operator
	 * associated with the built-in
	 * procedure in the namelist
	 */
	op = p->value[0] &~ NSTAND;
	if (opt('s') && (p->value[0] & NSTAND)) {
		standard();
		error("%s is a nonstandard procedure", p->symbol);
	}
	switch (op) {

	case O_ABORT:
		if (argc != 0)
			error("null takes no arguments");
		return;

	case O_FLUSH:
		if (argc == 0) {
			putleaf( PCC_ICON , 0 , 0 , PCCT_INT , "_PFLUSH" );
			putop( PCCOM_UNARY PCC_CALL , PCCT_INT );
			putdot( filename , line );
			return;
		}
		if (argc != 1) {
			error("flush takes at most one argument");
			return;
		}
		putleaf( PCC_ICON , 0 , 0
			, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			, "_FLUSH" );
		ap = stklval(argv->list_node.list, NOFLAGS);
		if (ap == NLNIL)
			return;
		if (ap->class != FILET) {
			error("flush's argument must be a file, not %s", nameof(ap));
			return;
		}
		putop( PCC_CALL , PCCT_INT );
		putdot( filename , line );
		return;

	case O_MESSAGE:
	case O_WRITEF:
	case O_WRITLN:
		/*
		 * Set up default file "output"'s type
		 */
		file = NIL;
		filetype = nl+T1CHAR;
		/*
		 * Determine the file implied
		 * for the write and generate
		 * code to make it the active file.
		 */
		if (op == O_MESSAGE) {
			/*
			 * For message, all that matters
			 * is that the filetype is
			 * a character file.
			 * Thus "output" will suit us fine.
			 */
			putleaf( PCC_ICON , 0 , 0 , PCCT_INT , "_PFLUSH" );
			putop( PCCOM_UNARY PCC_CALL , PCCT_INT );
			putdot( filename , line );
			putRV( (char *) 0 , cbn , CURFILEOFFSET , NLOCAL ,
				PCCTM_PTR|PCCT_STRTY );
			putLV( "__err" , 0 , 0 , NGLOBAL , PCCTM_PTR|PCCT_STRTY );
			putop( PCC_ASSIGN , PCCTM_PTR|PCCT_STRTY );
			putdot( filename , line );
		} else if (argv != TR_NIL && (al = argv->list_node.list)->tag !=
					T_WEXP) {
			/*
			 * If there is a first argument which has
			 * no write widths, then it is potentially
			 * a file name.
			 */
			codeoff();
			ap = stkrval(argv->list_node.list, NLNIL, (long) RREQ );
			codeon();
			if (ap == NLNIL)
				argv = argv->list_node.next;
			if (ap != NIL && ap->class == FILET) {
				/*
				 * Got "write(f, ...", make
				 * f the active file, and save
				 * it and its type for use in
				 * processing the rest of the
				 * arguments to write.
				 */
				putRV((char *) 0 , cbn , CURFILEOFFSET , NLOCAL ,
					PCCTM_PTR|PCCT_STRTY );
				putleaf( PCC_ICON , 0 , 0
				    , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
				    , "_UNIT" );
				file = argv->list_node.list;
				filetype = ap->type;
				(void) stklval(argv->list_node.list, NOFLAGS);
				putop( PCC_CALL , PCCT_INT );
				putop( PCC_ASSIGN , PCCTM_PTR|PCCT_STRTY );
				putdot( filename , line );
				/*
				 * Skip over the first argument
				 */
				argv = argv->list_node.next;
				argc--;
			} else {
				/*
				 * Set up for writing on 
				 * standard output.
				 */
				putRV((char *) 0, cbn , CURFILEOFFSET ,
					NLOCAL , PCCTM_PTR|PCCT_STRTY );
				putLV( "_output" , 0 , 0 , NGLOBAL ,
					PCCTM_PTR|PCCT_STRTY );
				putop( PCC_ASSIGN , PCCTM_PTR|PCCT_STRTY );
				putdot( filename , line );
				output->nl_flags |= NUSED;
			}
		} else {
			putRV((char *) 0, cbn , CURFILEOFFSET , NLOCAL ,
				PCCTM_PTR|PCCT_STRTY );
			putLV( "_output" , 0 , 0 , NGLOBAL , PCCTM_PTR|PCCT_STRTY );
			putop( PCC_ASSIGN , PCCTM_PTR|PCCT_STRTY );
			putdot( filename , line );
			output->nl_flags |= NUSED;
		}
		/*
		 * Loop and process each
		 * of the arguments.
		 */
		for (; argv != TR_NIL; argv = argv->list_node.next) {
		        soffset_flag = FALSE;
			/*
			 * fmtspec indicates the type (CONstant or VARiable)
			 *	and number (none, WIDTH, and/or PRECision)
			 *	of the fields in the printf format for this
			 *	output variable.
			 * fmt is the format output indicator (D, E, F, O, X, S)
			 * fmtstart = 0 for leading blank; = 1 for no blank
			 */
			fmtspec = NIL;
			fmt = 'D';
			fmtstart = 1;
			al = argv->list_node.list;
			if (al == NIL)
				continue;
			if (al->tag == T_WEXP)
				alv = al->wexpr_node.expr1;
			else
				alv = al;
			if (alv == TR_NIL)
				continue;
			codeoff();
			ap = stkrval(alv, NLNIL , (long) RREQ );
			codeon();
			if (ap == NLNIL)
				continue;
			typ = classify(ap);
			if (al->tag == T_WEXP) {
				/*
				 * Handle width expressions.
				 * The basic game here is that width
				 * expressions get evaluated. If they
				 * are constant, the value is placed
				 * directly in the format string.
				 * Otherwise the value is pushed onto
				 * the stack and an indirection is
				 * put into the format string.
				 */
				if (al->wexpr_node.expr3 == 
						(struct tnode *) OCT)
					fmt = 'O';
				else if (al->wexpr_node.expr3 == 
						(struct tnode *) HEX)
					fmt = 'X';
				else if (al->wexpr_node.expr3 != TR_NIL) {
					/*
					 * Evaluate second format spec
					 */
					if ( constval(al->wexpr_node.expr3)
					    && isa( con.ctype , "i" ) ) {
						fmtspec += CONPREC;
						prec = con.crval;
					} else {
						fmtspec += VARPREC;
					}
					fmt = 'f';
					switch ( typ ) {
					case TINT:
						if ( opt( 's' ) ) {
						    standard();
						    error("Writing %ss with two write widths is non-standard", clnames[typ]);
						}
						/* and fall through */
					case TDOUBLE:
						break;
					default:
						error("Cannot write %ss with two write widths", clnames[typ]);
						continue;
					}
				}
				/*
				 * Evaluate first format spec
				 */
				if (al->wexpr_node.expr2 != TR_NIL) {
					if ( constval(al->wexpr_node.expr2)
					    && isa( con.ctype , "i" ) ) {
						fmtspec += CONWIDTH;
						field = con.crval;
					} else {
						fmtspec += VARWIDTH;
					}
				}
				if ((fmtspec & CONPREC) && prec < 0 ||
				    (fmtspec & CONWIDTH) && field < 0) {
					error("Negative widths are not allowed");
					continue;
				}
				if ( opt('s') &&
				    ((fmtspec & CONPREC) && prec == 0 ||
				    (fmtspec & CONWIDTH) && field == 0)) {
					standard();
					error("Zero widths are non-standard");
				}
			}
			if (filetype != nl+T1CHAR) {
				if (fmt == 'O' || fmt == 'X') {
					error("Oct/hex allowed only on text files");
					continue;
				}
				if (fmtspec) {
					error("Write widths allowed only on text files");
					continue;
				}
				/*
				 * Generalized write, i.e.
				 * to a non-textfile.
				 */
				putleaf( PCC_ICON , 0 , 0
				    , (int) (PCCM_ADDTYPE(
					PCCM_ADDTYPE(
					    PCCM_ADDTYPE( p2type( filetype )
						    , PCCTM_PTR )
					    , PCCTM_FTN )
					, PCCTM_PTR ))
				    , "_FNIL" );
				(void) stklval(file, NOFLAGS);
				putop( PCC_CALL
				    , PCCM_ADDTYPE( p2type( filetype ) , PCCTM_PTR ) );
				putop( PCCOM_UNARY PCC_MUL , p2type( filetype ) );
				/*
				 * file^ := ...
				 */
				switch ( classify( filetype ) ) {
				    case TBOOL:
				    case TCHAR:
				    case TINT:
				    case TSCAL:
					precheck( filetype , "_RANG4"  , "_RSNG4" );
					    /* and fall through */
				    case TDOUBLE:
				    case TPTR:
					ap = rvalue( argv->list_node.list , filetype , RREQ );
					break;
				    default:
					ap = rvalue( argv->list_node.list , filetype , LREQ );
					break;
				}
				if (ap == NIL)
					continue;
				if (incompat(ap, filetype, argv->list_node.list)) {
					cerror("Type mismatch in write to non-text file");
					continue;
				}
				switch ( classify( filetype ) ) {
				    case TBOOL:
				    case TCHAR:
				    case TINT:
				    case TSCAL:
					    postcheck(filetype, ap);
					    sconv(p2type(ap), p2type(filetype));
						/* and fall through */
				    case TDOUBLE:
				    case TPTR:
					    putop( PCC_ASSIGN , p2type( filetype ) );
					    putdot( filename , line );
					    break;
				    default:
					    putstrop(PCC_STASG,
						    PCCM_ADDTYPE(p2type(filetype),
							    PCCTM_PTR),
						    (int) lwidth(filetype),
						    align(filetype));
					    putdot( filename , line );
					    break;
				}
				/*
				 * put(file)
				 */
				putleaf( PCC_ICON , 0 , 0
				    , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
				    , "_PUT" );
				putRV((char *) 0 , cbn , CURFILEOFFSET , NLOCAL ,
					PCCTM_PTR|PCCT_STRTY );
				putop( PCC_CALL , PCCT_INT );
				putdot( filename , line );
				continue;
			}
			/*
			 * Write to a textfile
			 *
			 * Evaluate the expression
			 * to be written.
			 */
			if (fmt == 'O' || fmt == 'X') {
				if (opt('s')) {
					standard();
					error("Oct and hex are non-standard");
				}
				if (typ == TSTR || typ == TDOUBLE) {
					error("Can't write %ss with oct/hex", clnames[typ]);
					continue;
				}
				if (typ == TCHAR || typ == TBOOL)
					typ = TINT;
			}
			/*
			 * If there is no format specified by the programmer,
			 * implement the default.
			 */
			switch (typ) {
			case TPTR:
				warning();
				if (opt('s')) {
					standard();
				}
				error("Writing %ss to text files is non-standard",
				    clnames[typ]);
				/* and fall through */
			case TINT:
				if (fmt == 'f') {
					typ = TDOUBLE;
					goto tdouble;
				}
				if (fmtspec == NIL) {
					if (fmt == 'D')
						field = 10;
					else if (fmt == 'X')
						field = 8;
					else if (fmt == 'O')
						field = 11;
					else
						panic("fmt1");
					fmtspec = CONWIDTH;
				}
				break;
			case TCHAR:
			     tchar:
				fmt = 'c';
				break;
			case TSCAL:
				warning();
				if (opt('s')) {
					standard();
				}
				error("Writing %ss to text files is non-standard",
				    clnames[typ]);
			case TBOOL:
				fmt = 's';
				break;
			case TDOUBLE:
			     tdouble:
				switch (fmtspec) {
				case NIL:
					field = 14 + (5 + EXPOSIZE);
				        prec = field - (5 + EXPOSIZE);
					fmt = 'e';
					fmtspec = CONWIDTH + CONPREC;
					break;
				case CONWIDTH:
					field -= REALSPC;
					if (field < 1)
						field = 1;
				        prec = field - (5 + EXPOSIZE);
					if (prec < 1)
						prec = 1;
					fmtspec += CONPREC;
					fmt = 'e';
					break;
				case VARWIDTH:
					fmtspec += VARPREC;
					fmt = 'e';
					break;
				case CONWIDTH + CONPREC:
				case CONWIDTH + VARPREC:
					field -= REALSPC;
					if (field < 1)
						field = 1;
				}
				format[0] = ' ';
				fmtstart = 1 - REALSPC;
				break;
			case TSTR:
				(void) constval( alv );
				switch ( classify( con.ctype ) ) {
				    case TCHAR:
					typ = TCHAR;
					goto tchar;
				    case TSTR:
					strptr = con.cpval;
					for (strnglen = 0;  *strptr++;  strnglen++) /* void */;
					strptr = con.cpval;
					break;
				    default:
					strnglen = width(ap);
					break;
				}
				fmt = 's';
				strfmt = fmtspec;
				if (fmtspec == NIL) {
					fmtspec = SKIP;
					break;
				}
				if (fmtspec & CONWIDTH) {
					if (field <= strnglen)
						fmtspec = SKIP;
					else
						field -= strnglen;
				}
				break;
			default:
				error("Can't write %ss to a text file", clnames[typ]);
				continue;
			}
			/*
			 * Generate the format string
			 */
			switch (fmtspec) {
			default:
				panic("fmt2");
			case NIL:
				if (fmt == 'c') {
					if ( opt( 't' ) ) {
					    putleaf( PCC_ICON , 0 , 0
						, PCCM_ADDTYPE( PCCTM_FTN|PCCT_INT , PCCTM_PTR )
						, "_WRITEC" );
					    putRV((char *) 0 , cbn , CURFILEOFFSET ,
						    NLOCAL , PCCTM_PTR|PCCT_STRTY );
					    (void) stkrval( alv , NLNIL , (long) RREQ );
					    putop( PCC_CM , PCCT_INT );
					} else {
					    putleaf( PCC_ICON , 0 , 0
						, PCCM_ADDTYPE( PCCTM_FTN|PCCT_INT , PCCTM_PTR )
						, "_fputc" );
					    (void) stkrval( alv , NLNIL ,
							(long) RREQ );
					}
					putleaf( PCC_ICON , 0 , 0
					    , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
					    , "_ACTFILE" );
					putRV((char *) 0, cbn , CURFILEOFFSET ,
						NLOCAL , PCCTM_PTR|PCCT_STRTY );
					putop( PCC_CALL , PCCT_INT );
					putop( PCC_CM , PCCT_INT );
					putop( PCC_CALL , PCCT_INT );
					putdot( filename , line );
				} else  {
					sprintf(&format[1], "%%%c", fmt);
					goto fmtgen;
				}
			case SKIP:
				break;
			case CONWIDTH:
				sprintf(&format[1], "%%%1D%c", field, fmt);
				goto fmtgen;
			case VARWIDTH:
				sprintf(&format[1], "%%*%c", fmt);
				goto fmtgen;
			case CONWIDTH + CONPREC:
				sprintf(&format[1], "%%%1D.%1D%c", field, prec, fmt);
				goto fmtgen;
			case CONWIDTH + VARPREC:
				sprintf(&format[1], "%%%1D.*%c", field, fmt);
				goto fmtgen;
			case VARWIDTH + CONPREC:
				sprintf(&format[1], "%%*.%1D%c", prec, fmt);
				goto fmtgen;
			case VARWIDTH + VARPREC:
				sprintf(&format[1], "%%*.*%c", fmt);
			fmtgen:
				if ( opt( 't' ) ) {
				    putleaf( PCC_ICON , 0 , 0
					, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
					, "_WRITEF" );
				    putRV((char *) 0 , cbn , CURFILEOFFSET ,
					    NLOCAL , PCCTM_PTR|PCCT_STRTY );
				    putleaf( PCC_ICON , 0 , 0
					, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
					, "_ACTFILE" );
				    putRV((char *) 0 , cbn , CURFILEOFFSET ,
					    NLOCAL , PCCTM_PTR|PCCT_STRTY );
				    putop( PCC_CALL , PCCT_INT );
				    putop( PCC_CM , PCCT_INT );
				} else {
				    putleaf( PCC_ICON , 0 , 0
					, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
					, "_fprintf" );
				    putleaf( PCC_ICON , 0 , 0
					, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
					, "_ACTFILE" );
				    putRV((char *) 0 , cbn , CURFILEOFFSET ,
					    NLOCAL , PCCTM_PTR|PCCT_STRTY );
				    putop( PCC_CALL , PCCT_INT );
				}
				putCONG( &format[ fmtstart ]
					, strlen( &format[ fmtstart ] )
					, LREQ );
				putop( PCC_CM , PCCT_INT );
				if ( fmtspec & VARWIDTH ) {
					/*
					 * either
					 *	,(temp=width,MAX(temp,...)),
					 * or
					 *	, MAX( width , ... ) ,
					 */
				    if ( ( typ == TDOUBLE &&
						al->wexpr_node.expr3 == TR_NIL )
					|| typ == TSTR ) {
					soffset_flag = TRUE;
					soffset = sizes[cbn].curtmps;
					tempnlp = tmpalloc((long) (sizeof(long)),
						nl+T4INT, REGOK);
					putRV((char *) 0 , cbn ,
					    tempnlp -> value[ NL_OFFS ] ,
					    tempnlp -> extra_flags , PCCT_INT );
					ap = stkrval( al->wexpr_node.expr2 ,
						NLNIL , (long) RREQ );
					putop( PCC_ASSIGN , PCCT_INT );
					putleaf( PCC_ICON , 0 , 0
					    , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
					    , "_MAX" );
					putRV((char *) 0 , cbn ,
					    tempnlp -> value[ NL_OFFS ] ,
					    tempnlp -> extra_flags , PCCT_INT );
				    } else {
					if (opt('t')
					    || typ == TSTR || typ == TDOUBLE) {
					    putleaf( PCC_ICON , 0 , 0
						,PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT, PCCTM_PTR )
						,"_MAX" );
					}
					ap = stkrval( al->wexpr_node.expr2,
						NLNIL , (long) RREQ );
				    }
				    if (ap == NLNIL)
					    continue;
				    if (isnta(ap,"i")) {
					    error("First write width must be integer, not %s", nameof(ap));
					    continue;
				    }
				    switch ( typ ) {
				    case TDOUBLE:
					putleaf( PCC_ICON , REALSPC , 0 , PCCT_INT , (char *) 0 );
					putop( PCC_CM , PCCT_INT );
					putleaf( PCC_ICON , 1 , 0 , PCCT_INT , (char *) 0 );
					putop( PCC_CM , PCCT_INT );
					putop( PCC_CALL , PCCT_INT );
					if ( al->wexpr_node.expr3 == TR_NIL ) {
						/*
						 * finish up the comma op
						 */
					    putop( PCC_COMOP , PCCT_INT );
					    fmtspec &= ~VARPREC;
					    putop( PCC_CM , PCCT_INT );
					    putleaf( PCC_ICON , 0 , 0
						, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
						, "_MAX" );
					    putRV((char *) 0 , cbn ,
						tempnlp -> value[ NL_OFFS ] ,
						tempnlp -> extra_flags ,
						PCCT_INT );
					    putleaf( PCC_ICON ,
						5 + EXPOSIZE + REALSPC ,
						0 , PCCT_INT , (char *) 0 );
					    putop( PCC_CM , PCCT_INT );
					    putleaf( PCC_ICON , 1 , 0 , PCCT_INT , (char *) 0 );
					    putop( PCC_CM , PCCT_INT );
					    putop( PCC_CALL , PCCT_INT );
					}
					putop( PCC_CM , PCCT_INT );
					break;
				    case TSTR:
					putleaf( PCC_ICON , strnglen , 0 , PCCT_INT , (char *) 0 );
					putop( PCC_CM , PCCT_INT );
					putleaf( PCC_ICON , 0 , 0 , PCCT_INT , (char *) 0 );
					putop( PCC_CM , PCCT_INT );
					putop( PCC_CALL , PCCT_INT );
					putop( PCC_COMOP , PCCT_INT );
					putop( PCC_CM , PCCT_INT );
					break;
				    default:
					if (opt('t')) {
					    putleaf( PCC_ICON , 0 , 0 , PCCT_INT , (char *) 0 );
					    putop( PCC_CM , PCCT_INT );
					    putleaf( PCC_ICON , 0 , 0 , PCCT_INT , (char *) 0 );
					    putop( PCC_CM , PCCT_INT );
					    putop( PCC_CALL , PCCT_INT );
					}
					putop( PCC_CM , PCCT_INT );
					break;
				    }
				}
				/*
				 * If there is a variable precision,
				 * evaluate it 
				 */
				if (fmtspec & VARPREC) {
					if (opt('t')) {
					putleaf( PCC_ICON , 0 , 0
					    , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
					    , "_MAX" );
					}
					ap = stkrval( al->wexpr_node.expr3 ,
						NLNIL , (long) RREQ );
					if (ap == NIL)
						continue;
					if (isnta(ap,"i")) {
						error("Second write width must be integer, not %s", nameof(ap));
						continue;
					}
					if (opt('t')) {
					    putleaf( PCC_ICON , 0 , 0 , PCCT_INT , (char *) 0 );
					    putop( PCC_CM , PCCT_INT );
					    putleaf( PCC_ICON , 0 , 0 , PCCT_INT , (char *) 0 );
					    putop( PCC_CM , PCCT_INT );
					    putop( PCC_CALL , PCCT_INT );
					}
				 	putop( PCC_CM , PCCT_INT );
				}
				/*
				 * evaluate the thing we want printed.
				 */
				switch ( typ ) {
				case TPTR:
				case TCHAR:
				case TINT:
				    (void) stkrval( alv , NLNIL , (long) RREQ );
				    putop( PCC_CM , PCCT_INT );
				    break;
				case TDOUBLE:
				    ap = stkrval( alv , NLNIL , (long) RREQ );
				    if (isnta(ap, "d")) {
					sconv(p2type(ap), PCCT_DOUBLE);
				    }
				    putop( PCC_CM , PCCT_INT );
				    break;
				case TSCAL:
				case TBOOL:
				    putleaf( PCC_ICON , 0 , 0
					, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
					, "_NAM" );
				    ap = stkrval( alv , NLNIL , (long) RREQ );
				    sprintf( format , PREFIXFORMAT , LABELPREFIX
					    , listnames( ap ) );
				    putleaf( PCC_ICON , 0 , 0 ,
					(int) (PCCTM_PTR | PCCT_CHAR), format );
				    putop( PCC_CM , PCCT_INT );
				    putop( PCC_CALL , PCCT_INT );
				    putop( PCC_CM , PCCT_INT );
				    break;
				case TSTR:
				    putCONG( "" , 0 , LREQ );
				    putop( PCC_CM , PCCT_INT );
				    break;
				default:
				    panic("fmt3");
				    break;
				}
				putop( PCC_CALL , PCCT_INT );
				putdot( filename , line );
			}
			/*
			 * Write the string after its blank padding
			 */
			if (typ == TSTR ) {
				if ( opt( 't' ) ) {
				    putleaf( PCC_ICON , 0 , 0
					, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
					, "_WRITES" );
				    putRV((char *) 0 , cbn , CURFILEOFFSET ,
					    NLOCAL , PCCTM_PTR|PCCT_STRTY );
				    ap = stkrval(alv, NLNIL , (long) RREQ );
				    putop( PCC_CM , PCCT_INT );
				} else {
				    putleaf( PCC_ICON , 0 , 0
					, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
					, "_fwrite" );
				    ap = stkrval(alv, NLNIL , (long) RREQ );
				}
				if (strfmt & VARWIDTH) {
					    /*
					     *	min, inline expanded as
					     *	temp < len ? temp : len
					     */
					putRV((char *) 0 , cbn ,
					    tempnlp -> value[ NL_OFFS ] ,
					    tempnlp -> extra_flags , PCCT_INT );
					putleaf( PCC_ICON , strnglen , 0 , PCCT_INT , (char *) 0 );
					putop( PCC_LT , PCCT_INT );
					putRV((char *) 0 , cbn ,
					    tempnlp -> value[ NL_OFFS ] ,
					    tempnlp -> extra_flags , PCCT_INT );
					putleaf( PCC_ICON , strnglen , 0 , PCCT_INT , (char *) 0 );
					putop( PCC_COLON , PCCT_INT );
					putop( PCC_QUEST , PCCT_INT );
				} else {
					if (   ( fmtspec & SKIP )
					    && ( strfmt & CONWIDTH ) ) {
						strnglen = field;
					}
					putleaf( PCC_ICON , strnglen , 0 , PCCT_INT , (char *) 0 );
				}
				putop( PCC_CM , PCCT_INT );
				putleaf( PCC_ICON , 1 , 0 , PCCT_INT , (char *) 0 );
				putop( PCC_CM , PCCT_INT );
				putleaf( PCC_ICON , 0 , 0
				    , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
				    , "_ACTFILE" );
				putRV((char *) 0, cbn , CURFILEOFFSET , NLOCAL ,
					PCCTM_PTR|PCCT_STRTY );
				putop( PCC_CALL , PCCT_INT );
				putop( PCC_CM , PCCT_INT );
				putop( PCC_CALL , PCCT_INT );
				putdot( filename , line );
			}
			if (soffset_flag) {
				tmpfree(&soffset);
				soffset_flag = FALSE;
			}
		}
		/*
		 * Done with arguments.
		 * Handle writeln and
		 * insufficent number of args.
		 */
		switch (p->value[0] &~ NSTAND) {
			case O_WRITEF:
				if (argc == 0)
					error("Write requires an argument");
				break;
			case O_MESSAGE:
				if (argc == 0)
					error("Message requires an argument");
			case O_WRITLN:
				if (filetype != nl+T1CHAR)
					error("Can't 'writeln' a non text file");
				if ( opt( 't' ) ) {
				    putleaf( PCC_ICON , 0 , 0
					, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
					, "_WRITLN" );
				    putRV((char *) 0 , cbn , CURFILEOFFSET ,
					    NLOCAL , PCCTM_PTR|PCCT_STRTY );
				} else {
				    putleaf( PCC_ICON , 0 , 0
					, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
					, "_fputc" );
				    putleaf( PCC_ICON , '\n' , 0 , (int) PCCT_CHAR , (char *) 0 );
				    putleaf( PCC_ICON , 0 , 0
					, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
					, "_ACTFILE" );
				    putRV((char *) 0 , cbn , CURFILEOFFSET ,
					    NLOCAL , PCCTM_PTR|PCCT_STRTY );
				    putop( PCC_CALL , PCCT_INT );
				    putop( PCC_CM , PCCT_INT );
				}
				putop( PCC_CALL , PCCT_INT );
				putdot( filename , line );
				break;
		}
		return;

	case O_READ4:
	case O_READLN:
		/*
		 * Set up default
		 * file "input".
		 */
		file = NIL;
		filetype = nl+T1CHAR;
		/*
		 * Determine the file implied
		 * for the read and generate
		 * code to make it the active file.
		 */
		if (argv != TR_NIL) {
			codeoff();
			ap = stkrval(argv->list_node.list, NLNIL, (long) RREQ );
			codeon();
			if (ap == NLNIL)
				argv = argv->list_node.next;
			if (ap != NLNIL && ap->class == FILET) {
				/*
				 * Got "read(f, ...", make
				 * f the active file, and save
				 * it and its type for use in
				 * processing the rest of the
				 * arguments to read.
				 */
				file = argv->list_node.list;
				filetype = ap->type;
				putRV((char *) 0, cbn , CURFILEOFFSET , NLOCAL ,
					PCCTM_PTR|PCCT_STRTY );
				putleaf( PCC_ICON , 0 , 0 
					, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
					, "_UNIT" );
				(void) stklval(argv->list_node.list, NOFLAGS);
				putop( PCC_CALL , PCCT_INT );
				putop( PCC_ASSIGN , PCCTM_PTR|PCCT_STRTY );
				putdot( filename , line );
				argv = argv->list_node.next;
				argc--;
			} else {
				/*
				 * Default is read from
				 * standard input.
				 */
				putRV((char *) 0, cbn , CURFILEOFFSET , NLOCAL ,
					PCCTM_PTR|PCCT_STRTY );
				putLV( "_input" , 0 , 0 , NGLOBAL ,
					PCCTM_PTR|PCCT_STRTY );
				putop( PCC_ASSIGN , PCCTM_PTR|PCCT_STRTY );
				putdot( filename , line );
				input->nl_flags |= NUSED;
			}
		} else {
			putRV((char *) 0, cbn , CURFILEOFFSET , NLOCAL ,
				PCCTM_PTR|PCCT_STRTY );
			putLV( "_input" , 0 , 0 , NGLOBAL , PCCTM_PTR|PCCT_STRTY );
			putop( PCC_ASSIGN , PCCTM_PTR|PCCT_STRTY );
			putdot( filename , line );
			input->nl_flags |= NUSED;
		}
		/*
		 * Loop and process each
		 * of the arguments.
		 */
		for (; argv != TR_NIL; argv = argv->list_node.next) {
			/*
			 * Get the address of the target
			 * on the stack.
			 */
			al = argv->list_node.list;
			if (al == TR_NIL)
				continue;
			if (al->tag != T_VAR) {
				error("Arguments to %s must be variables, not expressions", p->symbol);
				continue;
			}
			codeoff();
			ap = stklval(al, MOD|ASGN|NOUSE);
			codeon();
			if (ap == NLNIL)
				continue;
			if (filetype != nl+T1CHAR) {
				/*
				 * Generalized read, i.e.
				 * from a non-textfile.
				 */
				if (incompat(filetype, ap, argv->list_node.list )) {
					error("Type mismatch in read from non-text file");
					continue;
				}
				/*
				 * var := file ^;
				 */
				ap = lvalue( al , MOD | ASGN | NOUSE , RREQ );
				if ( isa( ap , "bsci" ) ) {
					precheck( ap , "_RANG4" , "_RSNG4" );
				}
				putleaf( PCC_ICON , 0 , 0
				    , (int) (PCCM_ADDTYPE(
					PCCM_ADDTYPE(
					    PCCM_ADDTYPE(
						p2type( filetype ) , PCCTM_PTR )
					    , PCCTM_FTN )
					, PCCTM_PTR ))
				    , "_FNIL" );
				if (file != NIL)
					(void) stklval(file, NOFLAGS);
				else /* Magic */
					putRV( "_input" , 0 , 0 , NGLOBAL ,
						PCCTM_PTR | PCCT_STRTY );
				putop(PCC_CALL, PCCM_ADDTYPE(p2type(filetype), PCCTM_PTR));
				switch ( classify( filetype ) ) {
				    case TBOOL:
				    case TCHAR:
				    case TINT:
				    case TSCAL:
				    case TDOUBLE:
				    case TPTR:
					putop( PCCOM_UNARY PCC_MUL
						, p2type( filetype ) );
				}
				switch ( classify( filetype ) ) {
				    case TBOOL:
				    case TCHAR:
				    case TINT:
				    case TSCAL:
					    postcheck(ap, filetype);
					    sconv(p2type(filetype), p2type(ap));
						/* and fall through */
				    case TDOUBLE:
				    case TPTR:
					    putop( PCC_ASSIGN , p2type( ap ) );
					    putdot( filename , line );
					    break;
				    default:
					    putstrop(PCC_STASG,
						    PCCM_ADDTYPE(p2type(ap), PCCTM_PTR),
						    (int) lwidth(ap),
						    align(ap));
					    putdot( filename , line );
					    break;
				}
				/*
				 * get(file);
				 */
				putleaf( PCC_ICON , 0 , 0 
					, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
					, "_GET" );
				putRV((char *) 0 , cbn , CURFILEOFFSET , NLOCAL ,
					PCCTM_PTR|PCCT_STRTY );
				putop( PCC_CALL , PCCT_INT );
				putdot( filename , line );
				continue;
			}
			    /*
			     *	if you get to here, you are reading from
			     *	a text file.  only possiblities are:
			     *	character, integer, real, or scalar.
			     *	read( f , foo , ... ) is done as
			     *	foo := read( f ) with rangechecking
			     *	if appropriate.
			     */
			typ = classify(ap);
			op = rdops(typ);
			if (op == NIL) {
				error("Can't read %ss from a text file", clnames[typ]);
				continue;
			}
			    /*
			     *	left hand side of foo := read( f )
			     */
			ap = lvalue( al , MOD|ASGN|NOUSE , RREQ );
			if ( isa( ap , "bsci" ) ) {
			    precheck( ap , "_RANG4" , "_RSNG4" );
			}
			switch ( op ) {
			    case O_READC:
				readname = "_READC";
				readtype = PCCT_INT;
				break;
			    case O_READ4:
				readname = "_READ4";
				readtype = PCCT_INT;
				break;
			    case O_READ8:
				readname = "_READ8";
				readtype = PCCT_DOUBLE;
				break;
			    case O_READE:
				readname = "_READE";
				readtype = PCCT_INT;
				break;
			}
			putleaf( PCC_ICON , 0 , 0
				, (int) PCCM_ADDTYPE( PCCTM_FTN | readtype , PCCTM_PTR )
				, readname );
			putRV((char *) 0 , cbn , CURFILEOFFSET , NLOCAL ,
				PCCTM_PTR|PCCT_STRTY );
			if ( op == O_READE ) {
				sprintf( format , PREFIXFORMAT , LABELPREFIX
					, listnames( ap ) );
				putleaf( PCC_ICON , 0, 0, (int) (PCCTM_PTR | PCCT_CHAR),
					format );
				putop( PCC_CM , PCCT_INT );
				warning();
				if (opt('s')) {
					standard();
				}
				error("Reading scalars from text files is non-standard");
			}
			putop( PCC_CALL , (int) readtype );
			if ( isa( ap , "bcsi" ) ) {
			    postcheck(ap, readtype==PCCT_INT?nl+T4INT:nl+TDOUBLE);
			}
			sconv((int) readtype, p2type(ap));
			putop( PCC_ASSIGN , p2type( ap ) );
			putdot( filename , line );
		}
		/*
		 * Done with arguments.
		 * Handle readln and
		 * insufficient number of args.
		 */
		if (p->value[0] == O_READLN) {
			if (filetype != nl+T1CHAR)
				error("Can't 'readln' a non text file");
			putleaf( PCC_ICON , 0 , 0 
				, (int) PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
				, "_READLN" );
			putRV((char *) 0 , cbn , CURFILEOFFSET , NLOCAL ,
				PCCTM_PTR|PCCT_STRTY );
			putop( PCC_CALL , PCCT_INT );
			putdot( filename , line );
		} else if (argc == 0)
			error("read requires an argument");
		return;

	case O_GET:
	case O_PUT:
		if (argc != 1) {
			error("%s expects one argument", p->symbol);
			return;
		}
		putRV((char *) 0 , cbn , CURFILEOFFSET , NLOCAL , PCCTM_PTR|PCCT_STRTY );
		putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			, "_UNIT" );
		ap = stklval(argv->list_node.list, NOFLAGS);
		if (ap == NLNIL)
			return;
		if (ap->class != FILET) {
			error("Argument to %s must be a file, not %s", p->symbol, nameof(ap));
			return;
		}
		putop( PCC_CALL , PCCT_INT );
		putop( PCC_ASSIGN , PCCTM_PTR|PCCT_STRTY );
		putdot( filename , line );
		putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			, op == O_GET ? "_GET" : "_PUT" );
		putRV((char *) 0 , cbn , CURFILEOFFSET , NLOCAL , PCCTM_PTR|PCCT_STRTY );
		putop( PCC_CALL , PCCT_INT );
		putdot( filename , line );
		return;

	case O_RESET:
	case O_REWRITE:
		if (argc == 0 || argc > 2) {
			error("%s expects one or two arguments", p->symbol);
			return;
		}
		if (opt('s') && argc == 2) {
			standard();
			error("Two argument forms of reset and rewrite are non-standard");
		}
		putleaf( PCC_ICON , 0 , 0 , PCCT_INT
			, op == O_RESET ? "_RESET" : "_REWRITE" );
		ap = stklval(argv->list_node.list, MOD|NOUSE);
		if (ap == NLNIL)
			return;
		if (ap->class != FILET) {
			error("First argument to %s must be a file, not %s", p->symbol, nameof(ap));
			return;
		}
		if (argc == 2) {
			/*
			 * Optional second argument
			 * is a string name of a
			 * UNIX (R) file to be associated.
			 */
			al = argv->list_node.next;
			al = (struct tnode *) stkrval(al->list_node.list,
					NLNIL , (long) RREQ );
			if (al == TR_NIL)
				return;
			if (classify((struct nl *) al) != TSTR) {
				error("Second argument to %s must be a string, not %s", p->symbol, nameof((struct nl *) al));
				return;
			}
			strnglen = width((struct nl *) al);
		} else {
			putleaf( PCC_ICON , 0 , 0 , PCCT_INT , (char *) 0 );
			strnglen = 0;
		}
		putop( PCC_CM , PCCT_INT );
		putleaf( PCC_ICON , strnglen , 0 , PCCT_INT , (char *) 0 );
		putop( PCC_CM , PCCT_INT );
		putleaf( PCC_ICON , text(ap) ? 0: width(ap->type) , 0 , PCCT_INT , (char *) 0 );
		putop( PCC_CM , PCCT_INT );
		putop( PCC_CALL , PCCT_INT );
		putdot( filename , line );
		return;

	case O_NEW:
	case O_DISPOSE:
		if (argc == 0) {
			error("%s expects at least one argument", p->symbol);
			return;
		}
		alv = argv->list_node.list;
		codeoff();
		ap = stklval(alv, op == O_NEW ? ( MOD | NOUSE ) : MOD );
		codeon();
		if (ap == NLNIL)
			return;
		if (ap->class != PTR) {
			error("(First) argument to %s must be a pointer, not %s", p->symbol, nameof(ap));
			return;
		}
		ap = ap->type;
		if (ap == NLNIL)
			return;
		if (op == O_NEW)
			cmd = "_NEW";
		else /* op == O_DISPOSE */
			if ((ap->nl_flags & NFILES) != 0)
				cmd = "_DFDISPOSE";
			else
				cmd = "_DISPOSE";
		putleaf( PCC_ICON, 0, 0, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR ), cmd);
		(void) stklval(alv, op == O_NEW ? ( MOD | NOUSE ) : MOD );
		argv = argv->list_node.next;
		if (argv != TR_NIL) {
			if (ap->class != RECORD) {
				error("Record required when specifying variant tags");
				return;
			}
			for (; argv != TR_NIL; argv = argv->list_node.next) {
				if (ap->ptr[NL_VARNT] == NIL) {
					error("Too many tag fields");
					return;
				}
				if (!isconst(argv->list_node.list)) {
					error("Second and successive arguments to %s must be constants", p->symbol);
					return;
				}
				gconst(argv->list_node.list);
				if (con.ctype == NIL)
					return;
				if (incompat(con.ctype, (ap->ptr[NL_TAG])->type , TR_NIL )) {
					cerror("Specified tag constant type clashed with variant case selector type");
					return;
				}
				for (ap = ap->ptr[NL_VARNT]; ap != NIL; ap = ap->chain)
					if (ap->range[0] == con.crval)
						break;
				if (ap == NIL) {
					error("No variant case label value equals specified constant value");
					return;
				}
				ap = ap->ptr[NL_VTOREC];
			}
		}
		putleaf( PCC_ICON , width( ap ) , 0 , PCCT_INT , (char *) 0 );
		putop( PCC_CM , PCCT_INT );
		putop( PCC_CALL , PCCT_INT );
		putdot( filename , line );
		if (opt('t') && op == O_NEW) {
		    putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			    , "_blkclr" );
		    (void) stkrval(alv, NLNIL , (long) RREQ );
		    putleaf( PCC_ICON , width( ap ) , 0 , PCCT_INT , (char *) 0 );
		    putop( PCC_CM , PCCT_INT );
		    putop( PCC_CALL , PCCT_INT );
		    putdot( filename , line );
		}
		return;

	case O_DATE:
	case O_TIME:
		if (argc != 1) {
			error("%s expects one argument", p->symbol);
			return;
		}
		putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			, op == O_DATE ? "_DATE" : "_TIME" );
		ap = stklval(argv->list_node.list, MOD|NOUSE);
		if (ap == NIL)
			return;
		if (classify(ap) != TSTR || width(ap) != 10) {
			error("Argument to %s must be a alfa, not %s", p->symbol, nameof(ap));
			return;
		}
		putop( PCC_CALL , PCCT_INT );
		putdot( filename , line );
		return;

	case O_HALT:
		if (argc != 0) {
			error("halt takes no arguments");
			return;
		}
		putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			, "_HALT" );

		putop( PCCOM_UNARY PCC_CALL , PCCT_INT );
		putdot( filename , line );
		noreach = TRUE;
		return;

	case O_ARGV:
		if (argc != 2) {
			error("argv takes two arguments");
			return;
		}
		putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			, "_ARGV" );
		ap = stkrval(argv->list_node.list, NLNIL , (long) RREQ );
		if (ap == NLNIL)
			return;
		if (isnta(ap, "i")) {
			error("argv's first argument must be an integer, not %s", nameof(ap));
			return;
		}
		al = argv->list_node.next;
		ap = stklval(al->list_node.list, MOD|NOUSE);
		if (ap == NLNIL)
			return;
		if (classify(ap) != TSTR) {
			error("argv's second argument must be a string, not %s", nameof(ap));
			return;
		}
		putop( PCC_CM , PCCT_INT );
		putleaf( PCC_ICON , width( ap ) , 0 , PCCT_INT , (char *) 0 );
		putop( PCC_CM , PCCT_INT );
		putop( PCC_CALL , PCCT_INT );
		putdot( filename , line );
		return;

	case O_STLIM:
		if (argc != 1) {
			error("stlimit requires one argument");
			return;
		}
		putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			, "_STLIM" );
		ap = stkrval(argv->list_node.list, NLNIL , (long) RREQ );
		if (ap == NLNIL)
			return;
		if (isnta(ap, "i")) {
			error("stlimit's argument must be an integer, not %s", nameof(ap));
			return;
		}
		putop( PCC_CALL , PCCT_INT );
		putdot( filename , line );
		return;

	case O_REMOVE:
		if (argc != 1) {
			error("remove expects one argument");
			return;
		}
		putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			, "_REMOVE" );
		ap = stkrval(argv->list_node.list, NLNIL, (long) RREQ );
		if (ap == NLNIL)
			return;
		if (classify(ap) != TSTR) {
			error("remove's argument must be a string, not %s", nameof(ap));
			return;
		}
		putleaf( PCC_ICON , width( ap ) , 0 , PCCT_INT , (char *) 0 );
		putop( PCC_CM , PCCT_INT );
		putop( PCC_CALL , PCCT_INT );
		putdot( filename , line );
		return;

	case O_LLIMIT:
		if (argc != 2) {
			error("linelimit expects two arguments");
			return;
		}
		putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			, "_LLIMIT" );
		ap = stklval(argv->list_node.list, NOFLAGS|NOUSE);
		if (ap == NLNIL)
			return;
		if (!text(ap)) {
			error("linelimit's first argument must be a text file, not %s", nameof(ap));
			return;
		}
		al = argv->list_node.next;
		ap = stkrval(al->list_node.list, NLNIL , (long) RREQ );
		if (ap == NLNIL)
			return;
		if (isnta(ap, "i")) {
			error("linelimit's second argument must be an integer, not %s", nameof(ap));
			return;
		}
		putop( PCC_CM , PCCT_INT );
		putop( PCC_CALL , PCCT_INT );
		putdot( filename , line );
		return;
	case O_PAGE:
		if (argc != 1) {
			error("page expects one argument");
			return;
		}
		putRV((char *) 0 , cbn , CURFILEOFFSET , NLOCAL , PCCTM_PTR|PCCT_STRTY );
		putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			, "_UNIT" );
		ap = stklval(argv->list_node.list, NOFLAGS);
		if (ap == NLNIL)
			return;
		if (!text(ap)) {
			error("Argument to page must be a text file, not %s", nameof(ap));
			return;
		}
		putop( PCC_CALL , PCCT_INT );
		putop( PCC_ASSIGN , PCCTM_PTR|PCCT_STRTY );
		putdot( filename , line );
		if ( opt( 't' ) ) {
		    putleaf( PCC_ICON , 0 , 0
			, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			, "_PAGE" );
		    putRV((char *) 0 , cbn , CURFILEOFFSET , NLOCAL , PCCTM_PTR|PCCT_STRTY );
		} else {
		    putleaf( PCC_ICON , 0 , 0
			, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			, "_fputc" );
		    putleaf( PCC_ICON , '\f' , 0 , (int) PCCT_CHAR , (char *) 0 );
		    putleaf( PCC_ICON , 0 , 0
			, PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			, "_ACTFILE" );
		    putRV((char *) 0 , cbn , CURFILEOFFSET , NLOCAL , PCCTM_PTR|PCCT_STRTY );
		    putop( PCC_CALL , PCCT_INT );
		    putop( PCC_CM , PCCT_INT );
		}
		putop( PCC_CALL , PCCT_INT );
		putdot( filename , line );
		return;

	case O_ASRT:
		if (!opt('t'))
			return;
		if (argc == 0 || argc > 2) {
			error("Assert expects one or two arguments");
			return;
		}
		if (argc == 2)
			cmd = "_ASRTS";
		else
			cmd = "_ASRT";
		putleaf( PCC_ICON , 0 , 0
		    , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR ) , cmd );
		ap = stkrval(argv->list_node.list, NLNIL , (long) RREQ );
		if (ap == NLNIL)
			return;
		if (isnta(ap, "b"))
			error("Assert expression must be Boolean, not %ss", nameof(ap));
		if (argc == 2) {
			/*
			 * Optional second argument is a string specifying
			 * why the assertion failed.
			 */
			al = argv->list_node.next;
			al = (struct tnode *) stkrval(al->list_node.list, NLNIL , (long) RREQ );
			if (al == TR_NIL)
				return;
			if (classify((struct nl *) al) != TSTR) {
				error("Second argument to assert must be a string, not %s", nameof((struct nl *) al));
				return;
			}
			putop( PCC_CM , PCCT_INT );
		}
		putop( PCC_CALL , PCCT_INT );
		putdot( filename , line );
		return;

	case O_PACK:
		if (argc != 3) {
			error("pack expects three arguments");
			return;
		}
		putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			, "_PACK" );
		pu = "pack(a,i,z)";
		pua = (al = argv)->list_node.list;
		pui = (al = al->list_node.next)->list_node.list;
		puz = (al = al->list_node.next)->list_node.list;
		goto packunp;
	case O_UNPACK:
		if (argc != 3) {
			error("unpack expects three arguments");
			return;
		}
		putleaf( PCC_ICON , 0 , 0 , PCCM_ADDTYPE( PCCTM_FTN | PCCT_INT , PCCTM_PTR )
			, "_UNPACK" );
		pu = "unpack(z,a,i)";
		puz = (al = argv)->list_node.list;
		pua = (al = al->list_node.next)->list_node.list;
		pui = (al = al->list_node.next)->list_node.list;
packunp:
		ap = stkrval(pui, NLNIL , (long) RREQ );
		if (ap == NIL)
			return;
		ap = stklval(pua, op == O_PACK ? NOFLAGS : MOD|NOUSE);
		if (ap == NIL)
			return;
		if (ap->class != ARRAY) {
			error("%s requires a to be an unpacked array, not %s", pu, nameof(ap));
			return;
		}
		putop( PCC_CM , PCCT_INT );
		al = (struct tnode *) stklval(puz, op == O_UNPACK ? NOFLAGS : MOD|NOUSE);
		if (((struct nl *) al)->class != ARRAY) {
			error("%s requires z to be a packed array, not %s", pu, nameof(ap));
			return;
		}
		if (((struct nl *) al)->type == NIL || 
			((struct nl *) ap)->type == NIL)
			return;
		if (((struct nl *) al)->type != ((struct nl *) ap)->type) {
			error("%s requires a and z to be arrays of the same type", pu, nameof(ap));
			return;
		}
		putop( PCC_CM , PCCT_INT );
		k = width((struct nl *) al);
		itemwidth = width(ap->type);
		ap = ap->chain;
		al = ((struct tnode *) ((struct nl *) al)->chain);
		if (ap->chain != NIL || ((struct nl *) al)->chain != NIL) {
			error("%s requires a and z to be single dimension arrays", pu);
			return;
		}
		if (ap == NIL || al == NIL)
			return;
		/*
		 * al is the range for z i.e. u..v
		 * ap is the range for a i.e. m..n
		 * i will be n-m+1
		 * j will be v-u+1
		 */
		i = ap->range[1] - ap->range[0] + 1;
		j = ((struct nl *) al)->range[1] - 
			((struct nl *) al)->range[0] + 1;
		if (i < j) {
			error("%s cannot have more elements in a (%d) than in z (%d)", pu, (char *) j, (char *) i);
			return;
		}
		/*
		 * get n-m-(v-u) and m for the interpreter
		 */
		i -= j;
		j = ap->range[0];
		putleaf( PCC_ICON , itemwidth , 0 , PCCT_INT , (char *) 0 );
		putop( PCC_CM , PCCT_INT );
		putleaf( PCC_ICON , j , 0 , PCCT_INT , (char *) 0 );
		putop( PCC_CM , PCCT_INT );
		putleaf( PCC_ICON , i , 0 , PCCT_INT , (char *) 0 );
		putop( PCC_CM , PCCT_INT );
		putleaf( PCC_ICON , k , 0 , PCCT_INT , (char *) 0 );
		putop( PCC_CM , PCCT_INT );
		putop( PCC_CALL , PCCT_INT );
		putdot( filename , line );
		return;
	case 0:
		error("%s is an unimplemented extension", p->symbol);
		return;

	default:
		panic("proc case");
	}
}
#endif PC
