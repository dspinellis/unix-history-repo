/* Copyright (c) 1979 Regents of the University of California */

static	char sccsid[] = "@(#)pcproc.c 1.7 %G%";

#include "whoami.h"
#ifdef PC
    /*
     * and to the end of the file
     */
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include	"pc.h"
#include	"pcops.h"

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
	int *r;
{
	register struct nl *p;
	register int *alv, *al, op;
	struct nl *filetype, *ap;
	int argc, *argv, typ, fmtspec, strfmt, stkcnt, *file;
	char fmt, format[20], *strptr;
	int prec, field, strnglen, fmtlen, fmtstart, pu;
	int *pua, *pui, *puz;
	int i, j, k;
	int itemwidth;
	char		*readname;
	struct nl	*tempnlp;
	long		readtype;
	struct tmps	soffset;

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
	p = lookup(r[2]);
	if (p == NIL) {
		rvlist(r[3]);
		return;
	}
	if (p->class != PROC && p->class != FPROC) {
		error("Can't call %s, its %s not a procedure", p->symbol, classes[p->class]);
		rvlist(r[3]);
		return;
	}
	argv = r[3];

	/*
	 * Call handles user defined
	 * procedures and functions.
	 */
	if (bn != 0) {
		call(p, argv, PROC, bn);
		return;
	}

	/*
	 * Call to built-in procedure.
	 * Count the arguments.
	 */
	argc = 0;
	for (al = argv; al != NIL; al = al[2])
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
			putleaf( P2ICON , 0 , 0 , P2INT , "_PFLUSH" );
			putop( P2UNARY P2CALL , P2INT );
			putdot( filename , line );
			return;
		}
		if (argc != 1) {
			error("flush takes at most one argument");
			return;
		}
		putleaf( P2ICON , 0 , 0
			, ADDTYPE( P2FTN | P2INT , P2PTR )
			, "_FLUSH" );
		ap = stklval(argv[1], NOFLAGS);
		if (ap == NIL)
			return;
		if (ap->class != FILET) {
			error("flush's argument must be a file, not %s", nameof(ap));
			return;
		}
		putop( P2CALL , P2INT );
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
			putleaf( P2ICON , 0 , 0 , P2INT , "_PFLUSH" );
			putop( P2UNARY P2CALL , P2INT );
			putdot( filename , line );
			putRV( 0 , cbn , CURFILEOFFSET , NLOCAL ,
				P2PTR|P2STRTY );
			putLV( "__err" , 0 , 0 , NGLOBAL , P2PTR|P2STRTY );
			putop( P2ASSIGN , P2PTR|P2STRTY );
			putdot( filename , line );
		} else if (argv != NIL && (al = argv[1])[0] != T_WEXP) {
			/*
			 * If there is a first argument which has
			 * no write widths, then it is potentially
			 * a file name.
			 */
			codeoff();
			ap = stkrval(argv[1], NIL , RREQ );
			codeon();
			if (ap == NIL)
				argv = argv[2];
			if (ap != NIL && ap->class == FILET) {
				/*
				 * Got "write(f, ...", make
				 * f the active file, and save
				 * it and its type for use in
				 * processing the rest of the
				 * arguments to write.
				 */
				putRV( 0 , cbn , CURFILEOFFSET , NLOCAL ,
					P2PTR|P2STRTY );
				putleaf( P2ICON , 0 , 0
				    , ADDTYPE( P2FTN | P2INT , P2PTR )
				    , "_UNIT" );
				file = argv[1];
				filetype = ap->type;
				stklval(argv[1], NOFLAGS);
				putop( P2CALL , P2INT );
				putop( P2ASSIGN , P2PTR|P2STRTY );
				putdot( filename , line );
				/*
				 * Skip over the first argument
				 */
				argv = argv[2];
				argc--;
			} else {
				/*
				 * Set up for writing on 
				 * standard output.
				 */
				putRV( 0, cbn , CURFILEOFFSET ,
					NLOCAL , P2PTR|P2STRTY );
				putLV( "_output" , 0 , 0 , NGLOBAL ,
					P2PTR|P2STRTY );
				putop( P2ASSIGN , P2PTR|P2STRTY );
				putdot( filename , line );
			}
		} else {
			putRV( 0, cbn , CURFILEOFFSET , NLOCAL ,
				P2PTR|P2STRTY );
			putLV( "_output" , 0 , 0 , NGLOBAL , P2PTR|P2STRTY );
			putop( P2ASSIGN , P2PTR|P2STRTY );
			putdot( filename , line );
		}
		/*
		 * Loop and process each
		 * of the arguments.
		 */
		for (; argv != NIL; argv = argv[2]) {
			/*
			 * fmtspec indicates the type (CONstant or VARiable)
			 *	and number (none, WIDTH, and/or PRECision)
			 *	of the fields in the printf format for this
			 *	output variable.
			 * stkcnt is the number of longs pushed on the stack
			 * fmt is the format output indicator (D, E, F, O, X, S)
			 * fmtstart = 0 for leading blank; = 1 for no blank
			 */
			fmtspec = NIL;
			stkcnt = 0;
			fmt = 'D';
			fmtstart = 1;
			al = argv[1];
			if (al == NIL)
				continue;
			if (al[0] == T_WEXP)
				alv = al[1];
			else
				alv = al;
			if (alv == NIL)
				continue;
			codeoff();
			ap = stkrval(alv, NIL , RREQ );
			codeon();
			if (ap == NIL)
				continue;
			typ = classify(ap);
			if (al[0] == T_WEXP) {
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
				if (al[3] == OCT)
					fmt = 'O';
				else if (al[3] == HEX)
					fmt = 'X';
				else if (al[3] != NIL) {
					/*
					 * Evaluate second format spec
					 */
					if ( constval(al[3])
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
				if (al[2] != NIL) {
					if ( constval(al[2])
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
				putleaf( P2ICON , 0 , 0
				    , ADDTYPE(
					ADDTYPE(
					    ADDTYPE( p2type( filetype )
						    , P2PTR )
					    , P2FTN )
					, P2PTR )
				    , "_FNIL" );
				stklval(file, NOFLAGS);
				putop( P2CALL
				    , ADDTYPE( p2type( filetype ) , P2PTR ) );
				putop( P2UNARY P2MUL , p2type( filetype ) );
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
					ap = rvalue( argv[1] , filetype , RREQ );
					break;
				    default:
					ap = rvalue( argv[1] , filetype , LREQ );
					break;
				}
				if (ap == NIL)
					continue;
				if (incompat(ap, filetype, argv[1])) {
					cerror("Type mismatch in write to non-text file");
					continue;
				}
				switch ( classify( filetype ) ) {
				    case TBOOL:
				    case TCHAR:
				    case TINT:
				    case TSCAL:
					    postcheck( filetype );
						/* and fall through */
				    case TDOUBLE:
				    case TPTR:
					    putop( P2ASSIGN , p2type( filetype ) );
					    putdot( filename , line );
					    break;
				    default:
					    putstrop( P2STASG
							, p2type( filetype )
							, lwidth( filetype )
							, align( filetype ) );
					    putdot( filename , line );
					    break;
				}
				/*
				 * put(file)
				 */
				putleaf( P2ICON , 0 , 0
				    , ADDTYPE( P2FTN | P2INT , P2PTR )
				    , "_PUT" );
				putRV( 0 , cbn , CURFILEOFFSET , NLOCAL ,
					P2PTR|P2STRTY );
				putop( P2CALL , P2INT );
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
				error("Writing scalars to text files is non-standard");
			case TBOOL:
				fmt = 's';
				break;
			case TDOUBLE:
			     tdouble:
				switch (fmtspec) {
				case NIL:
					field = 21;
					prec = 14;
					fmt = 'e';
					fmtspec = CONWIDTH + CONPREC;
					break;
				case CONWIDTH:
					if (--field < 1)
						field = 1;
					prec = field - 7;
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
					if (--field < 1)
						field = 1;
				}
				format[0] = ' ';
				fmtstart = 0;
				break;
			case TSTR:
				constval( alv );
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
					    putleaf( P2ICON , 0 , 0
						, ADDTYPE( P2FTN|P2INT , P2PTR )
						, "_WRITEC" );
					    putRV( 0 , cbn , CURFILEOFFSET ,
						    NLOCAL , P2PTR|P2STRTY );
					    stkrval( alv , NIL , RREQ );
					    putop( P2LISTOP , P2INT );
					} else {
					    putleaf( P2ICON , 0 , 0
						, ADDTYPE( P2FTN|P2INT , P2PTR )
						, "_fputc" );
					    stkrval( alv , NIL , RREQ );
					}
					putleaf( P2ICON , 0 , 0
					    , ADDTYPE( P2FTN | P2INT , P2PTR )
					    , "_ACTFILE" );
					putRV( 0, cbn , CURFILEOFFSET ,
						NLOCAL , P2PTR|P2STRTY );
					putop( P2CALL , P2INT );
					putop( P2LISTOP , P2INT );
					putop( P2CALL , P2INT );
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
				    putleaf( P2ICON , 0 , 0
					, ADDTYPE( P2FTN | P2INT , P2PTR )
					, "_WRITEF" );
				    putRV( 0 , cbn , CURFILEOFFSET ,
					    NLOCAL , P2PTR|P2STRTY );
				    putleaf( P2ICON , 0 , 0
					, ADDTYPE( P2FTN | P2INT , P2PTR )
					, "_ACTFILE" );
				    putRV( 0 , cbn , CURFILEOFFSET ,
					    NLOCAL , P2PTR|P2STRTY );
				    putop( P2CALL , P2INT );
				    putop( P2LISTOP , P2INT );
				} else {
				    putleaf( P2ICON , 0 , 0
					, ADDTYPE( P2FTN | P2INT , P2PTR )
					, "_fprintf" );
				    putleaf( P2ICON , 0 , 0
					, ADDTYPE( P2FTN | P2INT , P2PTR )
					, "_ACTFILE" );
				    putRV( 0 , cbn , CURFILEOFFSET ,
					    NLOCAL , P2PTR|P2STRTY );
				    putop( P2CALL , P2INT );
				}
				putCONG( &format[ fmtstart ]
					, strlen( &format[ fmtstart ] )
					, LREQ );
				putop( P2LISTOP , P2INT );
				if ( fmtspec & VARWIDTH ) {
					/*
					 * either
					 *	,(temp=width,MAX(temp,...)),
					 * or
					 *	, MAX( width , ... ) ,
					 */
				    if ( ( typ == TDOUBLE && al[3] == NIL )
					|| typ == TSTR ) {
					soffset = sizes[cbn].curtmps;
					tempnlp = tmpalloc(sizeof(long),
						nl+T4INT, REGOK);
					putRV( 0 , cbn ,
					    tempnlp -> value[ NL_OFFS ] ,
					    tempnlp -> extra_flags , P2INT );
					ap = stkrval( al[2] , NIL , RREQ );
					putop( P2ASSIGN , P2INT );
					putleaf( P2ICON , 0 , 0
					    , ADDTYPE( P2FTN | P2INT , P2PTR )
					    , "_MAX" );
					putRV( 0 , cbn ,
					    tempnlp -> value[ NL_OFFS ] ,
					    tempnlp -> extra_flags , P2INT );
				    } else {
					if (opt('t')
					    || typ == TSTR || typ == TDOUBLE) {
					    putleaf( P2ICON , 0 , 0
						,ADDTYPE( P2FTN | P2INT, P2PTR )
						,"_MAX" );
					}
					ap = stkrval( al[2] , NIL , RREQ );
				    }
				    if (ap == NIL)
					    continue;
				    if (isnta(ap,"i")) {
					    error("First write width must be integer, not %s", nameof(ap));
					    continue;
				    }
				    switch ( typ ) {
				    case TDOUBLE:
					putleaf( P2ICON , 1 , 0 , P2INT , 0 );
					putop( P2LISTOP , P2INT );
					putleaf( P2ICON , 1 , 0 , P2INT , 0 );
					putop( P2LISTOP , P2INT );
					putop( P2CALL , P2INT );
					if ( al[3] == NIL ) {
						/*
						 * finish up the comma op
						 */
					    putop( P2COMOP , P2INT );
					    fmtspec &= ~VARPREC;
					    putop( P2LISTOP , P2INT );
					    putleaf( P2ICON , 0 , 0
						, ADDTYPE( P2FTN | P2INT , P2PTR )
						, "_MAX" );
					    putRV( 0 , cbn ,
						tempnlp -> value[ NL_OFFS ] ,
						tempnlp -> extra_flags ,
						P2INT );
					    tmpfree(&soffset);
					    putleaf( P2ICON , 8 , 0 , P2INT , 0 );
					    putop( P2LISTOP , P2INT );
					    putleaf( P2ICON , 1 , 0 , P2INT , 0 );
					    putop( P2LISTOP , P2INT );
					    putop( P2CALL , P2INT );
					}
					putop( P2LISTOP , P2INT );
					break;
				    case TSTR:
					putleaf( P2ICON , strnglen , 0 , P2INT , 0 );
					putop( P2LISTOP , P2INT );
					putleaf( P2ICON , 0 , 0 , P2INT , 0 );
					putop( P2LISTOP , P2INT );
					putop( P2CALL , P2INT );
					putop( P2COMOP , P2INT );
					putop( P2LISTOP , P2INT );
					break;
				    default:
					if (opt('t')) {
					    putleaf( P2ICON , 0 , 0 , P2INT , 0 );
					    putop( P2LISTOP , P2INT );
					    putleaf( P2ICON , 0 , 0 , P2INT , 0 );
					    putop( P2LISTOP , P2INT );
					    putop( P2CALL , P2INT );
					}
					putop( P2LISTOP , P2INT );
					break;
				    }
				}
				/*
				 * If there is a variable precision,
				 * evaluate it 
				 */
				if (fmtspec & VARPREC) {
					if (opt('t')) {
					putleaf( P2ICON , 0 , 0
					    , ADDTYPE( P2FTN | P2INT , P2PTR )
					    , "_MAX" );
					}
					ap = stkrval( al[3] , NIL , RREQ );
					if (ap == NIL)
						continue;
					if (isnta(ap,"i")) {
						error("Second write width must be integer, not %s", nameof(ap));
						continue;
					}
					if (opt('t')) {
					    putleaf( P2ICON , 0 , 0 , P2INT , 0 );
					    putop( P2LISTOP , P2INT );
					    putleaf( P2ICON , 0 , 0 , P2INT , 0 );
					    putop( P2LISTOP , P2INT );
					    putop( P2CALL , P2INT );
					}
				 	putop( P2LISTOP , P2INT );
				}
				/*
				 * evaluate the thing we want printed.
				 */
				switch ( typ ) {
				case TCHAR:
				case TINT:
				    stkrval( alv , NIL , RREQ );
				    putop( P2LISTOP , P2INT );
				    break;
				case TDOUBLE:
				    ap = stkrval( alv , NIL , RREQ );
				    if ( isnta( ap , "d" ) ) {
					putop( P2SCONV , P2DOUBLE );
				    }
				    putop( P2LISTOP , P2INT );
				    break;
				case TSCAL:
				case TBOOL:
				    putleaf( P2ICON , 0 , 0
					, ADDTYPE( P2FTN | P2INT , P2PTR )
					, "_NAM" );
				    ap = stkrval( alv , NIL , RREQ );
				    sprintf( format , PREFIXFORMAT , LABELPREFIX
					    , listnames( ap ) );
				    putleaf( P2ICON , 0 , 0 , P2PTR | P2CHAR
					    , format );
				    putop( P2LISTOP , P2INT );
				    putop( P2CALL , P2INT );
				    putop( P2LISTOP , P2INT );
				    break;
				case TSTR:
				    putCONG( "" , 0 , LREQ );
				    putop( P2LISTOP , P2INT );
				    break;
				}
				putop( P2CALL , P2INT );
				putdot( filename , line );
			}
			/*
			 * Write the string after its blank padding
			 */
			if (typ == TSTR ) {
				if ( opt( 't' ) ) {
				    putleaf( P2ICON , 0 , 0
					, ADDTYPE( P2FTN | P2INT , P2PTR )
					, "_WRITES" );
				    putRV( 0 , cbn , CURFILEOFFSET ,
					    NLOCAL , P2PTR|P2STRTY );
				    ap = stkrval(alv, NIL , RREQ );
				    putop( P2LISTOP , P2INT );
				} else {
				    putleaf( P2ICON , 0 , 0
					, ADDTYPE( P2FTN | P2INT , P2PTR )
					, "_fwrite" );
				    ap = stkrval(alv, NIL , RREQ );
				}
				if (strfmt & VARWIDTH) {
					    /*
					     *	min, inline expanded as
					     *	temp < len ? temp : len
					     */
					putRV( 0 , cbn ,
					    tempnlp -> value[ NL_OFFS ] ,
					    tempnlp -> extra_flags , P2INT );
					putleaf( P2ICON , strnglen , 0 , P2INT , 0 );
					putop( P2LT , P2INT );
					putRV( 0 , cbn ,
					    tempnlp -> value[ NL_OFFS ] ,
					    tempnlp -> extra_flags , P2INT );
					putleaf( P2ICON , strnglen , 0 , P2INT , 0 );
					putop( P2COLON , P2INT );
					putop( P2QUEST , P2INT );
					tmpfree(&soffset);
				} else {
					if (   ( fmtspec & SKIP )
					    && ( strfmt & CONWIDTH ) ) {
						strnglen = field;
					}
					putleaf( P2ICON , strnglen , 0 , P2INT , 0 );
				}
				putop( P2LISTOP , P2INT );
				putleaf( P2ICON , 1 , 0 , P2INT , 0 );
				putop( P2LISTOP , P2INT );
				putleaf( P2ICON , 0 , 0
				    , ADDTYPE( P2FTN | P2INT , P2PTR )
				    , "_ACTFILE" );
				putRV( 0, cbn , CURFILEOFFSET , NLOCAL ,
					P2PTR|P2STRTY );
				putop( P2CALL , P2INT );
				putop( P2LISTOP , P2INT );
				putop( P2CALL , P2INT );
				putdot( filename , line );
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
				    putleaf( P2ICON , 0 , 0
					, ADDTYPE( P2FTN | P2INT , P2PTR )
					, "_WRITLN" );
				    putRV( 0 , cbn , CURFILEOFFSET ,
					    NLOCAL , P2PTR|P2STRTY );
				} else {
				    putleaf( P2ICON , 0 , 0
					, ADDTYPE( P2FTN | P2INT , P2PTR )
					, "_fputc" );
				    putleaf( P2ICON , '\n' , 0 , P2CHAR , 0 );
				    putleaf( P2ICON , 0 , 0
					, ADDTYPE( P2FTN | P2INT , P2PTR )
					, "_ACTFILE" );
				    putRV( 0 , cbn , CURFILEOFFSET ,
					    NLOCAL , P2PTR|P2STRTY );
				    putop( P2CALL , P2INT );
				    putop( P2LISTOP , P2INT );
				}
				putop( P2CALL , P2INT );
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
		if (argv != NIL) {
			codeoff();
			ap = stkrval(argv[1], NIL , RREQ );
			codeon();
			if (ap == NIL)
				argv = argv[2];
			if (ap != NIL && ap->class == FILET) {
				/*
				 * Got "read(f, ...", make
				 * f the active file, and save
				 * it and its type for use in
				 * processing the rest of the
				 * arguments to read.
				 */
				file = argv[1];
				filetype = ap->type;
				putRV( 0, cbn , CURFILEOFFSET , NLOCAL ,
					P2PTR|P2STRTY );
				putleaf( P2ICON , 0 , 0 
					, ADDTYPE( P2FTN | P2INT , P2PTR )
					, "_UNIT" );
				stklval(argv[1], NOFLAGS);
				putop( P2CALL , P2INT );
				putop( P2ASSIGN , P2PTR|P2STRTY );
				putdot( filename , line );
				argv = argv[2];
				argc--;
			} else {
				/*
				 * Default is read from
				 * standard input.
				 */
				putRV( 0, cbn , CURFILEOFFSET , NLOCAL ,
					P2PTR|P2STRTY );
				putLV( "_input" , 0 , 0 , NGLOBAL ,
					P2PTR|P2STRTY );
				putop( P2ASSIGN , P2PTR|P2STRTY );
				putdot( filename , line );
				input->nl_flags |= NUSED;
			}
		} else {
			putRV( 0, cbn , CURFILEOFFSET , NLOCAL ,
				P2PTR|P2STRTY );
			putLV( "_input" , 0 , 0 , NGLOBAL , P2PTR|P2STRTY );
			putop( P2ASSIGN , P2PTR|P2STRTY );
			putdot( filename , line );
			input->nl_flags |= NUSED;
		}
		/*
		 * Loop and process each
		 * of the arguments.
		 */
		for (; argv != NIL; argv = argv[2]) {
			/*
			 * Get the address of the target
			 * on the stack.
			 */
			al = argv[1];
			if (al == NIL)
				continue;
			if (al[0] != T_VAR) {
				error("Arguments to %s must be variables, not expressions", p->symbol);
				continue;
			}
			codeoff();
			ap = stklval(al, MOD|ASGN|NOUSE);
			codeon();
			if (ap == NIL)
				continue;
			if (filetype != nl+T1CHAR) {
				/*
				 * Generalized read, i.e.
				 * from a non-textfile.
				 */
				if (incompat(filetype, ap, argv[1] )) {
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
				putleaf( P2ICON , 0 , 0
				    , ADDTYPE(
					ADDTYPE(
					    ADDTYPE(
						p2type( filetype ) , P2PTR )
					    , P2FTN )
					, P2PTR )
				    , "_FNIL" );
				if (file != NIL)
					stklval(file, NOFLAGS);
				else /* Magic */
					putRV( "_input" , 0 , 0 , NGLOBAL ,
						P2PTR | P2STRTY );
				putop( P2CALL , P2INT );
				switch ( classify( filetype ) ) {
				    case TBOOL:
				    case TCHAR:
				    case TINT:
				    case TSCAL:
				    case TDOUBLE:
				    case TPTR:
					putop( P2UNARY P2MUL
						, p2type( filetype ) );
				}
				switch ( classify( filetype ) ) {
				    case TBOOL:
				    case TCHAR:
				    case TINT:
				    case TSCAL:
					    postcheck( ap );
						/* and fall through */
				    case TDOUBLE:
				    case TPTR:
					    putop( P2ASSIGN , p2type( ap ) );
					    putdot( filename , line );
					    break;
				    default:
					    putstrop( P2STASG
							, p2type( ap )
							, lwidth( ap )
							, align( ap ) );
					    putdot( filename , line );
					    break;
				}
				/*
				 * get(file);
				 */
				putleaf( P2ICON , 0 , 0 
					, ADDTYPE( P2FTN | P2INT , P2PTR )
					, "_GET" );
				putRV( 0 , cbn , CURFILEOFFSET , NLOCAL ,
					P2PTR|P2STRTY );
				putop( P2CALL , P2INT );
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
				readtype = P2INT;
				break;
			    case O_READ4:
				readname = "_READ4";
				readtype = P2INT;
				break;
			    case O_READ8:
				readname = "_READ8";
				readtype = P2DOUBLE;
				break;
			    case O_READE:
				readname = "_READE";
				readtype = P2INT;
				break;
			}
			putleaf( P2ICON , 0 , 0
				, ADDTYPE( P2FTN | readtype , P2PTR )
				, readname );
			putRV( 0 , cbn , CURFILEOFFSET , NLOCAL ,
				P2PTR|P2STRTY );
			if ( op == O_READE ) {
				sprintf( format , PREFIXFORMAT , LABELPREFIX
					, listnames( ap ) );
				putleaf( P2ICON , 0 , 0 , P2PTR | P2CHAR
					, format );
				putop( P2LISTOP , P2INT );
				warning();
				if (opt('s')) {
					standard();
				}
				error("Reading scalars from text files is non-standard");
			}
			putop( P2CALL , readtype );
			if ( isa( ap , "bcsi" ) ) {
			    postcheck( ap );
			}
			putop( P2ASSIGN , p2type( ap ) );
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
			putleaf( P2ICON , 0 , 0 
				, ADDTYPE( P2FTN | P2INT , P2PTR )
				, "_READLN" );
			putRV( 0 , cbn , CURFILEOFFSET , NLOCAL ,
				P2PTR|P2STRTY );
			putop( P2CALL , P2INT );
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
		putRV( 0 , cbn , CURFILEOFFSET , NLOCAL , P2PTR|P2STRTY );
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
			, "_UNIT" );
		ap = stklval(argv[1], NOFLAGS);
		if (ap == NIL)
			return;
		if (ap->class != FILET) {
			error("Argument to %s must be a file, not %s", p->symbol, nameof(ap));
			return;
		}
		putop( P2CALL , P2INT );
		putop( P2ASSIGN , P2PTR|P2STRTY );
		putdot( filename , line );
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
			, op == O_GET ? "_GET" : "_PUT" );
		putRV( 0 , cbn , CURFILEOFFSET , NLOCAL , P2PTR|P2STRTY );
		putop( P2CALL , P2INT );
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
		putleaf( P2ICON , 0 , 0 , P2INT
			, op == O_RESET ? "_RESET" : "_REWRITE" );
		ap = stklval(argv[1], MOD|NOUSE);
		if (ap == NIL)
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
			al = argv[2];
			al = stkrval(al[1], NOFLAGS , RREQ );
			if (al == NIL)
				return;
			if (classify(al) != TSTR) {
				error("Second argument to %s must be a string, not %s", p->symbol, nameof(al));
				return;
			}
			strnglen = width(al);
		} else {
			putleaf( P2ICON , 0 , 0 , P2INT , 0 );
			strnglen = 0;
		}
		putop( P2LISTOP , P2INT );
		putleaf( P2ICON , strnglen , 0 , P2INT , 0 );
		putop( P2LISTOP , P2INT );
		putleaf( P2ICON , text(ap) ? 0: width(ap->type) , 0 , P2INT , 0 );
		putop( P2LISTOP , P2INT );
		putop( P2CALL , P2INT );
		putdot( filename , line );
		return;

	case O_NEW:
	case O_DISPOSE:
		if (argc == 0) {
			error("%s expects at least one argument", p->symbol);
			return;
		}
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
			, op == O_DISPOSE ? "_DISPOSE" :
				opt('t') ? "_NEWZ" : "_NEW" );
		ap = stklval(argv[1], op == O_NEW ? ( MOD | NOUSE ) : MOD );
		if (ap == NIL)
			return;
		if (ap->class != PTR) {
			error("(First) argument to %s must be a pointer, not %s", p->symbol, nameof(ap));
			return;
		}
		ap = ap->type;
		if (ap == NIL)
			return;
		argv = argv[2];
		if (argv != NIL) {
			if (ap->class != RECORD) {
				error("Record required when specifying variant tags");
				return;
			}
			for (; argv != NIL; argv = argv[2]) {
				if (ap->ptr[NL_VARNT] == NIL) {
					error("Too many tag fields");
					return;
				}
				if (!isconst(argv[1])) {
					error("Second and successive arguments to %s must be constants", p->symbol);
					return;
				}
				gconst(argv[1]);
				if (con.ctype == NIL)
					return;
				if (incompat(con.ctype, (ap->ptr[NL_TAG])->type , NIL )) {
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
		putleaf( P2ICON , width( ap ) , 0 , P2INT , 0 );
		putop( P2LISTOP , P2INT );
		putop( P2CALL , P2INT );
		putdot( filename , line );
		return;

	case O_DATE:
	case O_TIME:
		if (argc != 1) {
			error("%s expects one argument", p->symbol);
			return;
		}
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
			, op == O_DATE ? "_DATE" : "_TIME" );
		ap = stklval(argv[1], MOD|NOUSE);
		if (ap == NIL)
			return;
		if (classify(ap) != TSTR || width(ap) != 10) {
			error("Argument to %s must be a alfa, not %s", p->symbol, nameof(ap));
			return;
		}
		putop( P2CALL , P2INT );
		putdot( filename , line );
		return;

	case O_HALT:
		if (argc != 0) {
			error("halt takes no arguments");
			return;
		}
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
			, "_HALT" );

		putop( P2UNARY P2CALL , P2INT );
		putdot( filename , line );
		noreach = 1;
		return;

	case O_ARGV:
		if (argc != 2) {
			error("argv takes two arguments");
			return;
		}
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
			, "_ARGV" );
		ap = stkrval(argv[1], NIL , RREQ );
		if (ap == NIL)
			return;
		if (isnta(ap, "i")) {
			error("argv's first argument must be an integer, not %s", nameof(ap));
			return;
		}
		al = argv[2];
		ap = stklval(al[1], MOD|NOUSE);
		if (ap == NIL)
			return;
		if (classify(ap) != TSTR) {
			error("argv's second argument must be a string, not %s", nameof(ap));
			return;
		}
		putop( P2LISTOP , P2INT );
		putleaf( P2ICON , width( ap ) , 0 , P2INT , 0 );
		putop( P2LISTOP , P2INT );
		putop( P2CALL , P2INT );
		putdot( filename , line );
		return;

	case O_STLIM:
		if (argc != 1) {
			error("stlimit requires one argument");
			return;
		}
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
			, "_STLIM" );
		ap = stkrval(argv[1], NIL , RREQ );
		if (ap == NIL)
			return;
		if (isnta(ap, "i")) {
			error("stlimit's argument must be an integer, not %s", nameof(ap));
			return;
		}
		putop( P2CALL , P2INT );
		putdot( filename , line );
		return;

	case O_REMOVE:
		if (argc != 1) {
			error("remove expects one argument");
			return;
		}
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
			, "_REMOVE" );
		ap = stkrval(argv[1], NOFLAGS , RREQ );
		if (ap == NIL)
			return;
		if (classify(ap) != TSTR) {
			error("remove's argument must be a string, not %s", nameof(ap));
			return;
		}
		putleaf( P2ICON , width( ap ) , 0 , P2INT , 0 );
		putop( P2LISTOP , P2INT );
		putop( P2CALL , P2INT );
		putdot( filename , line );
		return;

	case O_LLIMIT:
		if (argc != 2) {
			error("linelimit expects two arguments");
			return;
		}
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
			, "_LLIMIT" );
		ap = stklval(argv[1], NOFLAGS|NOUSE);
		if (ap == NIL)
			return;
		if (!text(ap)) {
			error("linelimit's first argument must be a text file, not %s", nameof(ap));
			return;
		}
		al = argv[2];
		ap = stkrval(al[1], NIL , RREQ );
		if (ap == NIL)
			return;
		if (isnta(ap, "i")) {
			error("linelimit's second argument must be an integer, not %s", nameof(ap));
			return;
		}
		putop( P2LISTOP , P2INT );
		putop( P2CALL , P2INT );
		putdot( filename , line );
		return;
	case O_PAGE:
		if (argc != 1) {
			error("page expects one argument");
			return;
		}
		putRV( 0 , cbn , CURFILEOFFSET , NLOCAL , P2PTR|P2STRTY );
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
			, "_UNIT" );
		ap = stklval(argv[1], NOFLAGS);
		if (ap == NIL)
			return;
		if (!text(ap)) {
			error("Argument to page must be a text file, not %s", nameof(ap));
			return;
		}
		putop( P2CALL , P2INT );
		putop( P2ASSIGN , P2PTR|P2STRTY );
		putdot( filename , line );
		if ( opt( 't' ) ) {
		    putleaf( P2ICON , 0 , 0
			, ADDTYPE( P2FTN | P2INT , P2PTR )
			, "_PAGE" );
		    putRV( 0 , cbn , CURFILEOFFSET , NLOCAL , P2PTR|P2STRTY );
		} else {
		    putleaf( P2ICON , 0 , 0
			, ADDTYPE( P2FTN | P2INT , P2PTR )
			, "_fputc" );
		    putleaf( P2ICON , '\f' , 0 , P2CHAR , 0 );
		    putleaf( P2ICON , 0 , 0
			, ADDTYPE( P2FTN | P2INT , P2PTR )
			, "_ACTFILE" );
		    putRV( 0 , cbn , CURFILEOFFSET , NLOCAL , P2PTR|P2STRTY );
		    putop( P2CALL , P2INT );
		    putop( P2LISTOP , P2INT );
		}
		putop( P2CALL , P2INT );
		putdot( filename , line );
		return;

	case O_PACK:
		if (argc != 3) {
			error("pack expects three arguments");
			return;
		}
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
			, "_PACK" );
		pu = "pack(a,i,z)";
		pua = (al = argv)[1];
		pui = (al = al[2])[1];
		puz = (al = al[2])[1];
		goto packunp;
	case O_UNPACK:
		if (argc != 3) {
			error("unpack expects three arguments");
			return;
		}
		putleaf( P2ICON , 0 , 0 , ADDTYPE( P2FTN | P2INT , P2PTR )
			, "_UNPACK" );
		pu = "unpack(z,a,i)";
		puz = (al = argv)[1];
		pua = (al = al[2])[1];
		pui = (al = al[2])[1];
packunp:
		ap = stkrval((int *) pui, NLNIL , RREQ );
		if (ap == NIL)
			return;
		ap = stklval(pua, op == O_PACK ? NOFLAGS : MOD|NOUSE);
		if (ap == NIL)
			return;
		if (ap->class != ARRAY) {
			error("%s requires a to be an unpacked array, not %s", pu, nameof(ap));
			return;
		}
		putop( P2LISTOP , P2INT );
		al = (struct nl *) stklval(puz, op == O_UNPACK ? NOFLAGS : MOD|NOUSE);
		if (al->class != ARRAY) {
			error("%s requires z to be a packed array, not %s", pu, nameof(ap));
			return;
		}
		if (al->type == NIL || ap->type == NIL)
			return;
		if (al->type != ap->type) {
			error("%s requires a and z to be arrays of the same type", pu, nameof(ap));
			return;
		}
		putop( P2LISTOP , P2INT );
		k = width(al);
		itemwidth = width(ap->type);
		ap = ap->chain;
		al = al->chain;
		if (ap->chain != NIL || al->chain != NIL) {
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
		j = al->range[1] - al->range[0] + 1;
		if (i < j) {
			error("%s cannot have more elements in a (%d) than in z (%d)", pu, j, i);
			return;
		}
		/*
		 * get n-m-(v-u) and m for the interpreter
		 */
		i -= j;
		j = ap->range[0];
		putleaf( P2ICON , itemwidth , 0 , P2INT , 0 );
		putop( P2LISTOP , P2INT );
		putleaf( P2ICON , j , 0 , P2INT , 0 );
		putop( P2LISTOP , P2INT );
		putleaf( P2ICON , i , 0 , P2INT , 0 );
		putop( P2LISTOP , P2INT );
		putleaf( P2ICON , k , 0 , P2INT , 0 );
		putop( P2LISTOP , P2INT );
		putop( P2CALL , P2INT );
		putdot( filename , line );
		return;
	case 0:
		error("%s is an unimplemented 6400 extension", p->symbol);
		return;

	default:
		panic("proc case");
	}
}
#endif PC
