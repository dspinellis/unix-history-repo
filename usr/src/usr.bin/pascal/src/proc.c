/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)proc.c 1.10 %G%";

#include "whoami.h"
#ifdef OBJ
    /*
     *	and the rest of the file
     */
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"

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
proc(r)
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
	struct tmps soffset;
	struct nl	*tempnlp;

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
			put(1, O_MESSAGE);
			return;
		}
		if (argc != 1) {
			error("flush takes at most one argument");
			return;
		}
		ap = stklval(argv[1], NIL , LREQ );
		if (ap == NIL)
			return;
		if (ap->class != FILET) {
			error("flush's argument must be a file, not %s", nameof(ap));
			return;
		}
		put(1, op);
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
			put(1, O_MESSAGE);
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
				file = argv[1];
				filetype = ap->type;
				stklval(argv[1], NIL , LREQ );
				put(1, O_UNIT);
				/*
				 * Skip over the first argument
				 */
				argv = argv[2];
				argc--;
			} else
				/*
				 * Set up for writing on 
				 * standard output.
				 */
				put(1, O_UNITOUT);
		} else
			put(1, O_UNITOUT);
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
			 * stkcnt is the number of bytes pushed on the stack
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
				stklval(file, NIL , LREQ );
				put(1, O_FNIL);
				/*
				 * file^ := ...
				 */
				ap = rvalue(argv[1], NIL);
				if (ap == NIL)
					continue;
				if (incompat(ap, filetype, argv[1])) {
					cerror("Type mismatch in write to non-text file");
					continue;
				}
				convert(ap, filetype);
				put(2, O_AS, width(filetype));
				/*
				 * put(file)
				 */
				put(1, O_PUT);
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
			 * Place the arguement on the stack. If there is
			 * no format specified by the programmer, implement
			 * the default.
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
				if (fmt != 'f') {
					ap = stkrval(alv, NIL , RREQ );
					stkcnt += sizeof(long);
				} else {
					ap = stkrval(alv, NIL , RREQ );
					put(1, O_ITOD);
					stkcnt += sizeof(double);
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
				if (fmtspec == NIL) {
					put(1, O_FILE);
					ap = stkrval(alv, NIL , RREQ );
					convert(nl + T4INT, INT_TYP);
					put(2, O_WRITEC,
						sizeof(char *) + sizeof(int));
					fmtspec = SKIP;
					break;
				}
				ap = stkrval(alv, NIL , RREQ );
				convert(nl + T4INT, INT_TYP);
				stkcnt += sizeof(int);
				fmt = 'c';
				break;
			case TSCAL:
				warning();
				if (opt('s')) {
					standard();
				}
				error("Writing %ss to text files is non-standard",
				    clnames[typ]);
				/* and fall through */
			case TBOOL:
				stkrval(alv, NIL , RREQ );
				put(2, O_NAM, (long)listnames(ap));
				stkcnt += sizeof(char *);
				fmt = 's';
				break;
			case TDOUBLE:
				ap = stkrval(alv, TDOUBLE , RREQ );
				stkcnt += sizeof(double);
			     tdouble:
				switch (fmtspec) {
				case NIL:
#					ifdef DEC11
					    field = 21;
#					else
					    field = 22;
#					endif DEC11
					prec = 14;
					fmt = 'e';
					fmtspec = CONWIDTH + CONPREC;
					break;
				case CONWIDTH:
					if (--field < 1)
						field = 1;
#					ifdef DEC11
					    prec = field - 7;
#					else
					    prec = field - 8;
#					endif DEC11
					if (prec < 1)
						prec = 1;
					fmtspec += CONPREC;
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
					if (field <= strnglen) {
						fmtspec = SKIP;
						break;
					} else
						field -= strnglen;
				}
				/*
				 * push string to implement leading blank padding
				 */
				put(2, O_LVCON, 2);
				putstr("", 0);
				stkcnt += sizeof(char *);
				break;
			default:
				error("Can't write %ss to a text file", clnames[typ]);
				continue;
			}
			/*
			 * If there is a variable precision, evaluate it onto
			 * the stack
			 */
			if (fmtspec & VARPREC) {
				ap = stkrval(al[3], NIL , RREQ );
				if (ap == NIL)
					continue;
				if (isnta(ap,"i")) {
					error("Second write width must be integer, not %s", nameof(ap));
					continue;
				}
				if ( opt( 't' ) ) {
				    put(3, O_MAX, 0, 0);
				}
				convert(nl+T4INT, INT_TYP);
				stkcnt += sizeof(int);
			}
			/*
			 * If there is a variable width, evaluate it onto
			 * the stack
			 */
			if (fmtspec & VARWIDTH) {
				if ( ( typ == TDOUBLE && fmtspec == VARWIDTH )
				    || typ == TSTR ) {
					soffset = sizes[cbn].curtmps;
					tempnlp = tmpalloc(sizeof(long),
						nl+T4INT, REGOK);
					put(2, O_LV | cbn << 8 + INDX, 
					    tempnlp -> value[ NL_OFFS ] );
				}
				ap = stkrval(al[2], NIL , RREQ );
				if (ap == NIL)
					continue;
				if (isnta(ap,"i")) {
					error("First write width must be integer, not %s", nameof(ap));
					continue;
				}
				/*
				 * Perform special processing on widths based
				 * on data type 
				 */
				switch (typ) {
				case TDOUBLE:
					if (fmtspec == VARWIDTH) {
						fmt = 'e';
						put(1, O_AS4);
						put(2, O_RV4 | cbn << 8 + INDX,
						    tempnlp -> value[NL_OFFS] );
#						ifdef DEC11
						    put(3, O_MAX, 8, 1);
#						else
						    put(3, O_MAX, 9, 1);
#						endif DEC11
						convert(nl+T4INT, INT_TYP);
						stkcnt += sizeof(int);
						put(2, O_RV4 | cbn << 8 + INDX, 
						    tempnlp->value[NL_OFFS] );
						fmtspec += VARPREC;
						tmpfree(&soffset);
					}
					put(3, O_MAX, 1, 1);
					break;
				case TSTR:
					put(1, O_AS4);
					put(2, O_RV4 | cbn << 8 + INDX, 
					    tempnlp -> value[ NL_OFFS ] );
					put(3, O_MAX, strnglen, 0);
					break;
				default:
					if ( opt( 't' ) ) {
					    put(3, O_MAX, 0, 0);
					}
					break;
				}
				convert(nl+T4INT, INT_TYP);
				stkcnt += sizeof(int);
			}
			/*
			 * Generate the format string
			 */
			switch (fmtspec) {
			default:
				panic("fmt2");
			case SKIP:
				break;
			case NIL:
				sprintf(&format[1], "%%%c", fmt);
				goto fmtgen;
			case CONWIDTH:
				sprintf(&format[1], "%%%d%c", field, fmt);
				goto fmtgen;
			case VARWIDTH:
				sprintf(&format[1], "%%*%c", fmt);
				goto fmtgen;
			case CONWIDTH + CONPREC:
				sprintf(&format[1], "%%%d.%d%c", field, prec, fmt);
				goto fmtgen;
			case CONWIDTH + VARPREC:
				sprintf(&format[1], "%%%d.*%c", field, fmt);
				goto fmtgen;
			case VARWIDTH + CONPREC:
				sprintf(&format[1], "%%*.%d%c", prec, fmt);
				goto fmtgen;
			case VARWIDTH + VARPREC:
				sprintf(&format[1], "%%*.*%c", fmt);
			fmtgen:
				fmtlen = lenstr(&format[fmtstart], 0);
				put(2, O_LVCON, fmtlen);
				putstr(&format[fmtstart], 0);
				put(1, O_FILE);
				stkcnt += 2 * sizeof(char *);
				put(2, O_WRITEF, stkcnt);
			}
			/*
			 * Write the string after its blank padding
			 */
			if (typ == TSTR) {
				put(1, O_FILE);
				put(2, CON_INT, 1);
				if (strfmt & VARWIDTH) {
					put(2, O_RV4 | cbn << 8 + INDX , 
					    tempnlp -> value[ NL_OFFS ] );
					put(2, O_MIN, strnglen);
					convert(nl+T4INT, INT_TYP);
					tmpfree(&soffset);
				} else {
					if ((fmtspec & SKIP) &&
					   (strfmt & CONWIDTH)) {
						strnglen = field;
					}
					put(2, CON_INT, strnglen);
				}
				ap = stkrval(alv, NIL , RREQ );
				put(2, O_WRITES,
					2 * sizeof(char *) + 2 * sizeof(int));
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
				put(1, O_WRITLN);
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
				stklval(argv[1], NIL , LREQ );
				put(1, O_UNIT);
				argv = argv[2];
				argc--;
			} else {
				/*
				 * Default is read from
				 * standard input.
				 */
				put(1, O_UNITINP);
				input->nl_flags |= NUSED;
			}
		} else {
			put(1, O_UNITINP);
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
			ap = stklval(al, MOD|ASGN|NOUSE);
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
				if (file != NIL)
					stklval(file, NIL , LREQ );
				else /* Magic */
					put(2, PTR_RV, (int)input->value[0]);
				put(1, O_FNIL);
				put(2, O_IND, width(filetype));
				convert(filetype, ap);
				if (isa(ap, "bsci"))
					rangechk(ap, ap);
				put(2, O_AS, width(ap));
				/*
				 * get(file);
				 */
				put(1, O_GET);
				continue;
			}
			typ = classify(ap);
			op = rdops(typ);
			if (op == NIL) {
				error("Can't read %ss from a text file", clnames[typ]);
				continue;
			}
			if (op != O_READE)
				put(1, op);
			else {
				put(2, op, (long)listnames(ap));
				warning();
				if (opt('s')) {
					standard();
				}
				error("Reading scalars from text files is non-standard");
			}
			/*
			 * Data read is on the stack.
			 * Assign it.
			 */
			if (op != O_READ8 && op != O_READE)
				rangechk(ap, op == O_READC ? ap : nl+T4INT);
			gen(O_AS2, O_AS2, width(ap),
				op == O_READ8 ? 8 : op == O_READ4 ? 4 : 2);
		}
		/*
		 * Done with arguments.
		 * Handle readln and
		 * insufficient number of args.
		 */
		if (p->value[0] == O_READLN) {
			if (filetype != nl+T1CHAR)
				error("Can't 'readln' a non text file");
			put(1, O_READLN);
		}
		else if (argc == 0)
			error("read requires an argument");
		return;

	case O_GET:
	case O_PUT:
		if (argc != 1) {
			error("%s expects one argument", p->symbol);
			return;
		}
		ap = stklval(argv[1], NIL , LREQ );
		if (ap == NIL)
			return;
		if (ap->class != FILET) {
			error("Argument to %s must be a file, not %s", p->symbol, nameof(ap));
			return;
		}
		put(1, O_UNIT);
		put(1, op);
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
		codeoff();
		ap = stklval(argv[1], MOD|NOUSE);
		codeon();
		if (ap == NIL)
			return;
		if (ap->class != FILET) {
			error("First argument to %s must be a file, not %s", p->symbol, nameof(ap));
			return;
		}
		put(2, O_CON24, text(ap) ? 0: width(ap->type));
		if (argc == 2) {
			/*
			 * Optional second argument
			 * is a string name of a
			 * UNIX (R) file to be associated.
			 */
			al = argv[2];
			codeoff();
			al = stkrval(al[1], NOFLAGS , RREQ );
			codeon();
			if (al == NIL)
				return;
			if (classify(al) != TSTR) {
				error("Second argument to %s must be a string, not %s", p->symbol, nameof(al));
				return;
			}
			put(2, O_CON24, width(al));
			al = argv[2];
			al = stkrval(al[1], NOFLAGS , RREQ );
		} else {
			put(2, O_CON24, 0);
			put(2, PTR_CON, NIL);
		}
		ap = stklval(argv[1], MOD|NOUSE);
		put(1, op);
		return;

	case O_NEW:
	case O_DISPOSE:
		if (argc == 0) {
			error("%s expects at least one argument", p->symbol);
			return;
		}
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
		put(2, op, width(ap));
		return;

	case O_DATE:
	case O_TIME:
		if (argc != 1) {
			error("%s expects one argument", p->symbol);
			return;
		}
		ap = stklval(argv[1], MOD|NOUSE);
		if (ap == NIL)
			return;
		if (classify(ap) != TSTR || width(ap) != 10) {
			error("Argument to %s must be a alfa, not %s", p->symbol, nameof(ap));
			return;
		}
		put(1, op);
		return;

	case O_HALT:
		if (argc != 0) {
			error("halt takes no arguments");
			return;
		}
		put(1, op);
		noreach = 1;
		return;

	case O_ARGV:
		if (argc != 2) {
			error("argv takes two arguments");
			return;
		}
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
		put(2, op, width(ap));
		return;

	case O_STLIM:
		if (argc != 1) {
			error("stlimit requires one argument");
			return;
		}
		ap = stkrval(argv[1], NIL , RREQ );
		if (ap == NIL)
			return;
		if (isnta(ap, "i")) {
			error("stlimit's argument must be an integer, not %s", nameof(ap));
			return;
		}
		if (width(ap) != 4)
			put(1, O_STOI);
		put(1, op);
		return;

	case O_REMOVE:
		if (argc != 1) {
			error("remove expects one argument");
			return;
		}
		codeoff();
		ap = stkrval(argv[1], NOFLAGS , RREQ );
		codeon();
		if (ap == NIL)
			return;
		if (classify(ap) != TSTR) {
			error("remove's argument must be a string, not %s", nameof(ap));
			return;
		}
		put(2, O_CON24, width(ap));
		ap = stkrval(argv[1], NOFLAGS , RREQ );
		put(1, op);
		return;

	case O_LLIMIT:
		if (argc != 2) {
			error("linelimit expects two arguments");
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
		ap = stklval(argv[1], NOFLAGS|NOUSE);
		if (ap == NIL)
			return;
		if (!text(ap)) {
			error("linelimit's first argument must be a text file, not %s", nameof(ap));
			return;
		}
		put(1, op);
		return;
	case O_PAGE:
		if (argc != 1) {
			error("page expects one argument");
			return;
		}
		ap = stklval(argv[1], NIL , LREQ );
		if (ap == NIL)
			return;
		if (!text(ap)) {
			error("Argument to page must be a text file, not %s", nameof(ap));
			return;
		}
		put(1, O_UNIT);
		put(1, op);
		return;

	case O_PACK:
		if (argc != 3) {
			error("pack expects three arguments");
			return;
		}
		pu = "pack(a,i,z)";
		pua = argv[1];
		al = argv[2];
		pui = al[1];
		alv = al[2];
		puz = alv[1];
		goto packunp;
	case O_UNPACK:
		if (argc != 3) {
			error("unpack expects three arguments");
			return;
		}
		pu = "unpack(z,a,i)";
		puz = argv[1];
		al = argv[2];
		pua = al[1];
		alv = al[2];
		pui = alv[1];
packunp:
		codeoff();
		ap = stklval(pua, op == O_PACK ? NOFLAGS : MOD|NOUSE);
		al = (struct nl *) stklval(puz, op == O_UNPACK ? NOFLAGS : MOD|NOUSE);
		codeon();
		if (ap == NIL)
			return;
		if (ap->class != ARRAY) {
			error("%s requires a to be an unpacked array, not %s", pu, nameof(ap));
			return;
		}
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
		put(2, O_CON24, k);
		put(2, O_CON24, i);
		put(2, O_CON24, j);
		put(2, O_CON24, itemwidth);
		al = (struct nl *) stklval(puz, op == O_UNPACK ? NOFLAGS : MOD|NOUSE);
		ap = stklval(pua, op == O_PACK ? NOFLAGS : MOD|NOUSE);
		ap = stkrval((int *) pui, NLNIL , RREQ );
		if (ap == NIL)
			return;
		put(1, op);
		return;
	case 0:
		error("%s is an unimplemented 6400 extension", p->symbol);
		return;

	default:
		panic("proc case");
	}
}
#endif OBJ
