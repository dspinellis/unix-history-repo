#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.0 August 1977
 */

#include "whoami"
#include "0.h"
#include "tree.h"
#include "opcode.h"

/*
 * The following arrays are used to determine which classes may be
 * read and written to/from text files.
 * They are indexed by the return types from classify.
 */
#define rdops(x) rdxxxx[(x)-(TFIRST)]
#define wrops(x) wrxxxx[(x)-(TFIRST)]

int rdxxxx[] {
	0,		/* -7  file types */
	0,		/* -6  record types */
	0,		/* -5  array types */
	0,		/* -4  scalar types */
	0,		/* -3  pointer types */
	0,		/* -2  set types */
	0,		/* -1  string types */
	0,		/*  0  nil - i.e. no type */
	0,		/*  1  booleans */
	O_READC,	/*  2  character */
	O_READ4,	/*  3  integer */
	O_READ8		/*  4  real */
};

int wrxxxx[] {
	0,		/* -7  file types */
	0,		/* -6  record types */
	0,		/* -5  array types */
	0,		/* -4  scalar types */
	0,		/* -3  pointer types */
	0,		/* -2  set types */
	O_WRITG,	/* -1  string types */
	0,		/*  0  nil - i.e. no type */
	O_WRITB,	/*  1  booleans */
	O_WRITC,	/*  2  character */
	O_WRIT4,	/*  3  integer */
	O_WRIT8,	/*  4  real */
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
	register int *al, op;
	struct nl *filetype, *ap;
	int argc, *argv, c, two, oct, hex, *file;
	int pu;
	int *pua, *pui, *puz;
	int i, j, k;

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
	if (p->class != PROC) {
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

	case O_NULL:
		if (argc != 0)
			error("null takes no arguments");
		return;

	case O_FLUSH:
		if (argc == 0) {
			put1(O_MESSAGE);
			return;
		}
		if (argc != 1) {
			error("flush takes at most one argument");
			return;
		}
		ap = rvalue(argv[1], NIL);
		if (ap == NIL)
			return;
		if (ap->class != FILE) {
			error("flush's argument must be a file, not %s", nameof(ap));
			return;
		}
		put1(op);
		return;

	case O_MESSAGE:
	case O_WRIT2:
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
			put1(O_MESSAGE);
		} else if (argv != NIL && (al = argv[1])[0] != T_WEXP) {
			/*
			 * If there is a first argument which has
			 * no write widths, then it is potentially
			 * a file name.
			 */
			codeoff();
			ap = rvalue(argv[1], NIL);
			codeon();
			if (ap == NIL)
				argv = argv[2];
			if (ap != NIL && ap->class == FILE) {
				/*
				 * Got "write(f, ...", make
				 * f the active file, and save
				 * it and its type for use in
				 * processing the rest of the
				 * arguments to write.
				 */
				file = argv[1];
				filetype = ap->type;
				rvalue(argv[1], NIL);
				put1(O_UNIT);
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
				put1(O_UNITOUT);
		} else
			put1(O_UNITOUT);
		/*
		 * Loop and process each
		 * of the arguments.
		 */
		for (; argv != NIL; argv = argv[2]) {
			al = argv[1];
			if (al == NIL)
				continue;
			/*
			 * Op will be used to
			 * accumulate width information,
			 * and two records the fact
			 * that we saw two write widths
			 */
			op = 0;
			two = 0;
			oct = 0;
			hex = 0;
			if (al[0] == T_WEXP) {
				if (filetype != nl+T1CHAR) {
					error("Write widths allowed only with text files");
					continue;
				}
				/*
				 * Handle width expressions.
				 * The basic game here is that width
				 * expressions get evaluated and left
				 * on the stack and their width's get
				 * packed into the high byte of the
				 * affected opcode (subop).
				 */
				if (al[3] == OCT) 
					oct++;
				else if (al[3] == HEX)
					hex++;
				else if (al[3] != NIL) {
					two++;
					/*
					 * Arrange for the write
					 * opcode that takes two widths
					 */
					op =| O_WRIT82-O_WRIT8;
					ap = rvalue(al[3], NIL);
					if (ap == NIL)
						continue;
					if (isnta(ap, "i")) {
						error("Second write width must be integer, not %s", nameof(ap));
						continue;
					}
					op =| even(width(ap)) << 11;
				}
				if (al[2] != NIL) {
					ap = rvalue(al[2], NIL);
					if (ap == NIL)
						continue;
					if (isnta(ap, "i")) {
						error("First write width must be integer, not %s", nameof(ap));
						continue;
					}
					op =| even(width(ap)) << 8;
				}
				al = al[1];
				if (al == NIL)
					continue;
			}
			if (filetype != nl+T1CHAR) {
				if (oct || hex) {
					error("Oct/hex allowed only on text files");
					continue;
				}
				if (op) {
					error("Write widths allowed only on text files");
					continue;
				}
				/*
				 * Generalized write, i.e.
				 * to a non-textfile.
				 */
				rvalue(file, NIL);
				put1(O_FNIL);
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
				put2(O_AS, width(filetype));
				/*
				 * put(file)
				 */
				put1(O_PUT);
				continue;
			}
			/*
			 * Write to a textfile
			 *
			 * Evaluate the expression
			 * to be written.
			 */
			ap = rvalue(al, NIL);
			if (ap == NIL)
				continue;
			c = classify(ap);
			if (two && c != TDOUBLE) {
				if (isnta(ap, "i")) {
					error("Only reals can have two write widths");
					continue;
				}
				convert(ap, nl+TDOUBLE);
				c = TDOUBLE;
			}
			if (oct || hex) {
				if (opt('s')) {
					standard();
					error("Oct and hex are non-standard");
				}
				switch (c) {
					case TREC:
					case TARY:
					case TFILE:
					case TSTR:
					case TSET:
					case TDOUBLE:
						error("Can't write %ss with oct/hex", clnames[c]);
						continue;
				}
				put1(op | (oct ? O_WROCT2 : O_WRHEX2) | (width(ap) >> 2));
				continue;
			}
			if (wrops(c) == NIL) {
				error("Can't write %ss to a text file", clnames[c]);
				continue;
			}
			if (c == TINT && width(ap) != 4)
				op =| O_WRIT2;
			else
				op =| wrops(c);
			if (c == TSTR)
				put2(op, width(ap));
			else
				put1(op);
		}
		/*
		 * Done with arguments.
		 * Handle writeln and
		 * insufficent number of args.
		 */
		switch (p->value[0] &~ NSTAND) {
			case O_WRIT2:
				if (argc == 0)
					error("Write requires an argument");
				break;
			case O_MESSAGE:
				if (argc == 0)
					error("Message requires an argument");
			case O_WRITLN:
				if (filetype != nl+T1CHAR)
					error("Can't 'writeln' a non text file");
				put1(O_WRITLN);
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
			ap = rvalue(argv[1], NIL);
			codeon();
			if (ap == NIL)
				argv = argv[2];
			if (ap != NIL && ap->class == FILE) {
				/*
				 * Got "read(f, ...", make
				 * f the active file, and save
				 * it and its type for use in
				 * processing the rest of the
				 * arguments to read.
				 */
				file = argv[1];
				filetype = ap->type;
				rvalue(argv[1], NIL);
				put1(O_UNIT);
				argv = argv[2];
				argc--;
			} else {
				/*
				 * Default is read from
				 * standard input.
				 */
				put1(O_UNITINP);
				input->nl_flags =| NUSED;
			}
		} else {
			put1(O_UNITINP);
			input->nl_flags =| NUSED;
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
			ap = lvalue(al, MOD|ASGN|NOUSE);
			if (ap == NIL)
				continue;
			if (filetype != nl+T1CHAR) {
				/*
				 * Generalized read, i.e.
				 * from a non-textfile.
				 */
				if (incompat(filetype, ap, NIL)) {
					error("Type mismatch in read from non-text file");
					continue;
				}
				/*
				 * var := file ^;
				 */
				if (file != NIL)
					rvalue(file, NIL);
				else /* Magic */
					put2(O_RV2, input->value[0]);
				put1(O_FNIL);
				put2(O_IND, width(filetype));
				convert(filetype, ap);
				if (isa(ap, "bsci"))
					rangechk(ap, ap);
				put2(O_AS, width(ap));
				/*
				 * get(file);
				 */
				put1(O_GET);
				continue;
			}
			c = classify(ap);
			op = rdops(c);
			if (op == NIL) {
				error("Can't read %ss from a text file", clnames[c]);
				continue;
			}
			put1(op);
			/*
			 * Data read is on the stack.
			 * Assign it.
			 */
			if (op != O_READ8)
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
			put1(O_READLN);
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
		ap = rvalue(argv[1], NIL);
		if (ap == NIL)
			return;
		if (ap->class != FILE) {
			error("Argument to %s must be a file, not %s", p->symbol, nameof(ap));
			return;
		}
		put1(O_UNIT);
		put1(op);
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
		ap = lvalue(argv[1], MOD|NOUSE);
		if (ap == NIL)
			return;
		if (ap->class != FILE) {
			error("First argument to %s must be a file, not %s", p->symbol, nameof(ap));
			return;
		}
		if (argc == 2) {
			/*
			 * Optional second argument
			 * is a string name of a
			 * UNIX file to be associated.
			 */
			al = argv[2];
			al = rvalue(al[1], NIL);
			if (al == NIL)
				return;
			if (classify(al) != TSTR) {
				error("Second argument to %s must be a string, not %s", p->symbol, nameof(al));
				return;
			}
			c = width(al);
		} else
			c = 0;
		if (c > 127) {
			error("File name too long");
			return;
		}
		put2(op | c << 8, text(ap) ? 0: width(ap->type));
		return;

	case O_NEW:
	case O_DISPOSE:
		if (argc == 0) {
			error("%s expects at least one argument", p->symbol);
			return;
		}
		ap = lvalue(argv[1], MOD|NOUSE);
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
				if (ap->value[NL_VARNT] == NIL) {
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
				if (incompat(con.ctype, ap->value[NL_TAG]->type)) {
					cerror("Specified tag constant type clashed with variant case selector type");
					return;
				}
				for (ap = ap->value[NL_VARNT]; ap != NIL; ap = ap->chain)
					if (ap->range[0] == con.crval)
						break;
				if (ap == NIL) {
					error("No variant case label value equals specified constant value");
					return;
				}
				ap = ap->value[NL_VTOREC];
			}
		}
		put2(op, width(ap));
		return;

	case O_DATE:
	case O_TIME:
		if (argc != 1) {
			error("%s expects one argument", p->symbol);
			return;
		}
		ap = lvalue(argv[1], MOD|NOUSE);
		if (ap == NIL)
			return;
		if (classify(ap) != TSTR || width(ap) != 10) {
			error("Argument to %s must be a alfa, not %s", p->symbol, nameof(ap));
			return;
		}
		put1(op);
		return;

	case O_HALT:
		if (argc != 0) {
			error("halt takes no arguments");
			return;
		}
		put1(op);
		noreach = 1;
		return;

	case O_ARGV:
		if (argc != 2) {
			error("argv takes two arguments");
			return;
		}
		ap = rvalue(argv[1], NIL);
		if (ap == NIL)
			return;
		if (isnta(ap, "i")) {
			error("argv's first argument must be an integer, not %s", nameof(ap));
			return;
		}
		convert(ap, nl+T2INT);
		al = argv[2];
		ap = lvalue(al[1], MOD|NOUSE);
		if (ap == NIL)
			return;
		if (classify(ap) != TSTR) {
			error("argv's second argument must be a string, not %s", nameof(ap));
			return;
		}
		put2(op, width(ap));
		return;

	case O_STLIM:
		if (argc != 1) {
			error("stlimit requires one argument");
			return;
		}
		ap = rvalue(argv[1], NIL);
		if (ap == NIL)
			return;
		if (isnta(ap, "i")) {
			error("stlimit's argument must be an integer, not %s", nameof(ap));
			return;
		}
		if (width(ap) != 4)
			put1(O_STOI);
		put1(op);
		return;

	case O_REMOVE:
		if (argc != 1) {
			error("remove expects one argument");
			return;
		}
		ap = rvalue(argv[1], NIL);
		if (ap == NIL)
			return;
		if (classify(ap) != TSTR) {
			error("remove's argument must be a string, not %s", nameof(ap));
			return;
		}
		put2(op, width(ap));
		return;

	case O_LLIMIT:
		if (argc != 2) {
			error("linelimit expects two arguments");
			return;
		}
		ap = lvalue(argv[1], NOMOD|NOUSE);
		if (ap == NIL)
			return;
		if (!text(ap)) {
			error("linelimit's first argument must be a text file, not %s", nameof(ap));
			return;
		}
		al = argv[2];
		ap = rvalue(al[1], NIL);
		if (ap == NIL)
			return;
		if (isnta(ap, "i")) {
			error("linelimit's second argument must be an integer, not %s", nameof(ap));
			return;
		}
		convert(ap, nl+T2INT);
		put1(op);
		return;
	case O_PAGE:
		if (argc != 1) {
			error("page expects one argument");
			return;
		}
		ap = rvalue(argv[1], NIL);
		if (ap == NIL)
			return;
		if (!text(ap)) {
			error("Argument to page must be a text file, not %s", nameof(ap));
			return;
		}
		put1(O_UNIT);
		put1(op);
		return;

	case O_PACK:
		if (argc != 3) {
			error("pack expects three arguments");
			return;
		}
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
		pu = "unpack(z,a,i)";
		puz = (al = argv)[1];
		pua = (al = al[2])[1];
		pui = (al = al[2])[1];
packunp:
		ap = rvalue(pui, NIL);
		if (ap == NIL)
			return;
		if (width(ap) == 4)
			put1(O_ITOS);
		ap = lvalue(pua, op == O_PACK ? NOMOD : MOD|NOUSE);
		if (ap == NIL)
			return;
		if (ap->class != ARRAY) {
			error("%s requires a to be an unpacked array, not %s", pu, nameof(ap));
			return;
		}
		al = lvalue(puz, op == O_UNPACK ? NOMOD : MOD|NOUSE);
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
		i =- j;
		j = ap->range[0];
		put(5, op, width(ap), j, i, k);
		return;
	case 0:
		error("%s is an unimplemented 6400 extension", p->symbol);
		return;

	default:
		panic("proc case");
	}
}
