/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Global and local symbol routines.
 */

#include "calc.h"
#include "token.h"
#include "symbol.h"
#include "string.h"
#include "opcodes.h"
#include "func.h"

#define HASHSIZE	37	/* size of hash table */


static STRINGHEAD localnames;	/* list of local variable names */
static STRINGHEAD globalnames;	/* list of global variable names */
static STRINGHEAD paramnames;	/* list of parameter variable names */
static GLOBAL *globalhash[HASHSIZE];	/* hash table for globals */

static void fitprint();


/*
 * Hash a symbol name so we can find it in the hash table.
 * Args are the symbol name and the symbol name size.
 */
#define HASH(n, s) ((unsigned)((n)[0]*123 + (n)[s-1]*135 + (s)*157) % HASHSIZE)


/*
 * Initialize the global symbol table.
 */
void
initglobals()
{
	int i;		/* index counter */

	for (i = 0; i < HASHSIZE; i++)
		globalhash[i] = NULL;
	initstr(&globalnames);
}


/*
 * Define a possibly new global variable.
 * If it did not already exist, it is created with an undefined value.
 * The address of the global symbol structure is returned.
 */
GLOBAL *
addglobal(name)
	char *name;		/* name of global variable */
{
	GLOBAL *sp;		/* current symbol pointer */
	GLOBAL **hp;		/* hash table head address */
	long len;		/* length of string */

	len = strlen(name);
	if (len <= 0)
		return NULL;
	hp = &globalhash[HASH(name, len)];
	for (sp = *hp; sp; sp = sp->g_next) {
		if ((sp->g_len == len) && (strcmp(sp->g_name, name) == 0))
			return sp;
	}
	sp = (GLOBAL *) malloc(sizeof(GLOBAL));
	if (sp == NULL)
		return sp;
	sp->g_name = addstr(&globalnames, name);
	sp->g_len = len;
	sp->g_value.v_type = V_NULL;
	sp->g_next = *hp;
	*hp = sp;
	return sp;
}


/*
 * Look up the name of a global variable and return its address.
 * Returns NULL if the symbol was not found.
 */
GLOBAL *
findglobal(name)
	char *name;		/* name of global variable */
{
	GLOBAL *sp;		/* current symbol pointer */
	long len;		/* length of string */

	len = strlen(name);
	sp = globalhash[HASH(name, len)];
	while (sp) {
		if ((sp->g_len == len) && (strcmp(sp->g_name, name) == 0))
			return sp;
		sp = sp->g_next;
	}
	return sp;
}


/*
 * Return the name of a global variable given its address.
 */
char *
globalname(sp)
	GLOBAL *sp;		/* address of global pointer */
{
	if (sp)
		return sp->g_name;
	return "";
}


/*
 * Show the value of all global variables, typing only the head and
 * tail of very large numbers.
 */
void
showglobals()
{
	GLOBAL **hp;			/* hash table head address */
	register GLOBAL *sp;		/* current global symbol pointer */
	long count;			/* number of global variables shown */
	NUMBER *num, *den;
	long digits;

	count = 0;
	for (hp = &globalhash[HASHSIZE-1]; hp >= globalhash; hp--) {
		for (sp = *hp; sp; sp = sp->g_next) {
			if (sp->g_value.v_type != V_NUM)
				continue;
			if (count++ == 0) {
				printf("\nName    Digits  Value\n");
				printf(  "----    ------  -----\n");
			}
			printf("%-8s ", sp->g_name);
			num = qnum(sp->g_value.v_num);
			digits = qdigits(num);
			printf("%-7ld ", digits);
			fitprint(num, digits, 60L);
			qfree(num);
			if (!qisint(sp->g_value.v_num)) {
				den = qden(sp->g_value.v_num);
				digits = qdigits(den);
				printf("\n	%-6ld /", digits);
				fitprint(den, digits, 60L);
				qfree(den);
			}
			printf("\n");
		}
	}
	printf(count ? "\n" : "No global variables defined.\n");
}


/*
 * Print an integer which is guaranteed to fit in the specified number
 * of columns, using imbedded '...' characters if it is too large.
 */
static void
fitprint(num, digits, width)
	NUMBER *num;		/* number to print */
	long digits, width;
{
	long show, used;
	NUMBER *p, *t, *div, *val;

	if (digits <= width) {
		qprintf("%r", num);
		return;
	}
	show = (width / 2) - 2;
	t = itoq(10L);
	p = itoq((long) (digits - show));
	div = qpowi(t, p);
	val = qquo(num, div);
	qprintf("%r...", val);
	qfree(p);
	qfree(div);
	qfree(val);
	p = itoq(show);
	div = qpowi(t, p);
	val = qmod(num, div);
	used = qdigits(val);
	while (used++ < show) printf("0");
	qprintf("%r", val);
	qfree(p);
	qfree(div);
	qfree(val);
	qfree(t);
}


/*
 * Write all normal global variables to an output file.
 * Note: Currently only simple types are saved.
 * Returns nonzero on error.
 */
writeglobals(name)
	char *name;
{
	FILE *fp;
	GLOBAL **hp;			/* hash table head address */
	register GLOBAL *sp;		/* current global symbol pointer */
	int savemode;			/* saved output mode */

	fp = f_open(name, "w");
	if (fp == NULL)
		return 1;
	setfp(fp);
	for (hp = &globalhash[HASHSIZE-1]; hp >= globalhash; hp--) {
		for (sp = *hp; sp; sp = sp->g_next) {
			switch (sp->g_value.v_type) {
				case V_NUM:
				case V_COM:
				case V_STR:
					break;
				default:
					continue;
			}
			math_fmt("%s = ", sp->g_name);
			savemode = _outmode_;
			_outmode_ = MODE_HEX;
			printvalue(&sp->g_value, PRINT_UNAMBIG);
			_outmode_ = savemode;
			math_str(";\n");
		}
	}
	setfp(stdout);
	if (fclose(fp))
		return 1;
	return 0;
}


/*
 * Initialize the local and parameter symbol table information.
 */
void
initlocals()
{
	initstr(&localnames);
	initstr(&paramnames);
	curfunc->f_localcount = 0;
	curfunc->f_paramcount = 0;
}


/*
 * Add a possibly new local variable definition.
 * Returns the index of the variable into the local symbol table.
 * Minus one indicates the symbol could not be added.
 */
long
addlocal(name)
	char *name;		/* name of local variable */
{
	long index;		/* current symbol index */

	index = findstr(&localnames, name);
	if (index >= 0)
		return index;
	index = localnames.h_count;
	(void) addstr(&localnames, name);
	curfunc->f_localcount++;
	return index;
}


/*
 * Find a local variable name and return its index.
 * Returns minus one if the variable name is not defined.
 */
long
findlocal(name)
	char *name;		/* name of local variable */
{
	return findstr(&localnames, name);
}


/*
 * Return the name of a local variable.
 */
char *
localname(n)
	long n;
{
	return namestr(&localnames, n);
}


/*
 * Add a possibly new parameter variable definition.
 * Returns the index of the variable into the parameter symbol table.
 * Minus one indicates the symbol could not be added.
 */
long
addparam(name)
	char *name;		/* name of parameter variable */
{
	long index;		/* current symbol index */

	index = findstr(&paramnames, name);
	if (index >= 0)
		return index;
	index = paramnames.h_count;
	(void) addstr(&paramnames, name);
	curfunc->f_paramcount++;
	return index;
}


/*
 * Find a parameter variable name and return its index.
 * Returns minus one if the variable name is not defined.
 */
long
findparam(name)
	char *name;		/* name of parameter variable */
{
	return findstr(&paramnames, name);
}


/*
 * Return the name of a parameter variable.
 */
char *
paramname(n)
	long n;
{
	return namestr(&paramnames, n);
}


/*
 * Return the type of a variable name.
 * This is either local, parameter, global, or undefined.
 */
symboltype(name)
	char *name;		/* variable name to find */
{
	if (findlocal(name) >= 0)
		return SYM_LOCAL;
	if (findparam(name) >= 0)
		return SYM_PARAM;
	if (findglobal(name))
		return SYM_GLOBAL;
	return SYM_UNDEFINED;
}

/* END CODE */
