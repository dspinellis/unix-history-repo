/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)printdecl.c 1.1 %G%";

/*
 * print out the type of a symbol
 */

#include "defs.h"
#include "sym.h"
#include "symtab.h"
#include "tree.h"
#include "btypes.h"
#include "classes.h"
#include "sym.rep"

printdecl(s)
SYM *s;
{
	register SYM *t;
	BOOLEAN semicolon;

	semicolon = TRUE;
	switch(s->class) {
		case CONST:
			if (s->type->class == SCAL) {
				printf("(enumeration constant, ord %ld)",
					s->symvalue.iconval);
			} else {
				printf("const %s = ", s->symbol);
				if (s->type == t_real->type) {
					printf("%g", s->symvalue.fconval);
				} else {
					printf("%ld", s->symvalue.iconval);
				}
			}
			break;

		case TYPE:
			printf("type %s = ", s->symbol);
			printtype(s, s->type);
			break;

		case VAR:
			if (isparam(s)) {
				printf("(parameter) %s : ", s->symbol);
			} else {
				printf("var %s : ", s->symbol);
			}
			printtype(s, s->type);
			break;

		case REF:
			printf("(var parameter) %s : ", s->symbol);
			printtype(s, s->type);
			break;

		case RANGE:
		case ARRAY:
		case RECORD:
		case VARNT:
		case PTR:
			printtype(s, s);
			semicolon = FALSE;
			break;

		case FVAR:
			printf("(function variable) %s : ", s->symbol);
			printtype(s, s->type);
			break;

		case FIELD:
			printf("(field) %s : ", s->symbol);
			printtype(s, s->type);
			break;

		case PROC:
			printf("procedure %s", s->symbol);
			listparams(s);
			break;

		case PROG:
			printf("program %s", s->symbol);
			t = s->chain;
			if (t != NIL) {
				printf("(%s", t->symbol);
				for (t = t->chain; t != NIL; t = t->chain) {
					printf(", %s", t->symbol);
				}
				printf(")");
			}
			break;

		case FUNC:
			printf("function %s", s->symbol);
			listparams(s);
			printf(" : ");
			printtype(s, s->type);
			break;

		default:
			error("class %s in printdecl", classname(s));
	}
	if (semicolon) {
		putchar(';');
	}
	putchar('\n');
}

/*
 * Recursive whiz-bang procedure to print the type portion
 * of a declaration.  Doesn't work quite right for variant records.
 *
 * The symbol associated with the type is passed to allow
 * searching for type names without getting "type blah = blah".
 */

LOCAL printtype(s, t)
SYM *s;
SYM *t;
{
	register SYM *tmp;

	tmp = findtype(t);
	if (tmp != NIL && tmp != s) {
		printf("%s", tmp->symbol);
		return;
	}
	switch(t->class) {
		case VAR:
		case CONST:
		case FUNC:
		case PROC:
			panic("printtype: class %s", classname(t));
			break;

		case ARRAY:
			printf("array[");
			tmp = t->chain;
			for (;;) {
				printtype(tmp, tmp);
				tmp = tmp->chain;
				if (tmp == NIL) {
					break;
				}
				printf(", ");
			}
			printf("] of ");
			printtype(t, t->type);
			break;

		case RECORD:
			printf("record\n");
			if (t->chain != NIL) {
				printtype(t->chain, t->chain);
			}
			printf("end");
			break;

		case FIELD:
			if (t->chain != NIL) {
				printtype(t->chain, t->chain);
			}
			printf("\t%s : ", t->symbol);
			printtype(t, t->type);
			printf(";\n");
			break;

		case RANGE: {
			long r0, r1;

			r0 = t->symvalue.rangev.lower;
			r1 = t->symvalue.rangev.upper;
			if (t == t_char) {
				printf("'%c'..'%c'", (char) r0, (char) r1);
			} else {
				printf("%ld..%ld", r0, r1);
			}
			break;
		}

		case PTR:
			putchar('^');
			printtype(t, t->type);
			break;

		case TYPE:
			if (t->symbol != NIL) {
				printf("%s", t->symbol);
			} else {
				printtype(t, t->type);
			}
			break;

		case SCAL:
			printf("(");
			t = t->type->chain;
			if (t != NIL) {
				printf("%s", t->symbol);
				t = t->chain;
				while (t != NIL) {
					printf(", %s", t->symbol);
					t = t->chain;
				}
			} else {
				panic("empty enumeration");
			}
			printf(")");
			break;

		default:
			printf("(class %d)", t->class);
			break;
	}
}

/*
 * List the parameters of a procedure or function.
 * No attempt is made to combine like types.
 */

listparams(s)
SYM *s;
{
	SYM *t;

	if (s->chain != NIL) {
		putchar('(');
		for (t = s->chain; t != NIL; t = t->chain) {
			switch (t->class) {
				case REF:
					printf("var ");
					break;

				case FPROC:
					printf("procedure ");
					break;

				case FFUNC:
					printf("function ");
					break;

				case VAR:
					break;

				default:
					panic("unexpected class %d for parameter", t->class);
			}
			printf("%s : ", t->symbol);
			printtype(t, t->type);
			if (t->chain != NIL) {
				printf("; ");
			}
		}
		putchar(')');
	}
}
