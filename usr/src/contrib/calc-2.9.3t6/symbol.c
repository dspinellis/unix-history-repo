/*
 * Copyright (c) 1994 David I. Bell
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


static int filescope;		/* file scope level for static variables */
static int funcscope;		/* function scope level for static variables */
static STRINGHEAD localnames;	/* list of local variable names */
static STRINGHEAD globalnames;	/* list of global variable names */
static STRINGHEAD paramnames;	/* list of parameter variable names */
static GLOBAL *globalhash[HASHSIZE];	/* hash table for globals */

static void fitprint MATH_PROTO((NUMBER *num, long digits, long width));
static void unscope MATH_PROTO((void));


/*
 * Hash a symbol name so we can find it in the hash table.
 * Args are the symbol name and the symbol name size.
 */
#define HASHSYM(n, s) ((unsigned)((n)[0]*123 + (n)[s-1]*135 + (s)*157) % HASHSIZE)


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
	filescope = SCOPE_STATIC;
	funcscope = 0;
}


/*
 * Define a possibly new global variable which may or may not be static.
 * If it did not already exist, it is created with a value of zero.
 * The address of the global symbol structure is returned.
 */
GLOBAL *
addglobal(name, isstatic)
	char *name;		/* name of global variable */
	BOOL isstatic;		/* TRUE if symbol is static */
{
	GLOBAL *sp;		/* current symbol pointer */
	GLOBAL **hp;		/* hash table head address */
	long len;		/* length of string */
	int newfilescope;	/* file scope being looked for */
	int newfuncscope;	/* function scope being looked for */

	newfilescope = SCOPE_GLOBAL;
	newfuncscope = 0;
	if (isstatic) {
		newfilescope = filescope;
		newfuncscope = funcscope;
	}
	len = strlen(name);
	if (len <= 0)
		return NULL;
	hp = &globalhash[HASHSYM(name, len)];
	for (sp = *hp; sp; sp = sp->g_next) {
		if ((sp->g_len == len) && (strcmp(sp->g_name, name) == 0)
			&& (sp->g_filescope == newfilescope)
			&& (sp->g_funcscope == newfuncscope))
				return sp;
	}
	sp = (GLOBAL *) malloc(sizeof(GLOBAL));
	if (sp == NULL)
		return sp;
	sp->g_name = addstr(&globalnames, name);
	sp->g_len = len;
	sp->g_filescope = newfilescope;
	sp->g_funcscope = newfuncscope;
	sp->g_value.v_num = qlink(&_qzero_);
	sp->g_value.v_type = V_NUM;
	sp->g_next = *hp;
	*hp = sp;
	return sp;
}


/*
 * Look up the name of a global variable and return its address.
 * Since the same variable may appear in different scopes, we search
 * for the one with the highest function scope value within the current
 * file scope level (or which is global).  Returns NULL if the symbol
 * was not found.
 */
GLOBAL *
findglobal(name)
	char *name;		/* name of global variable */
{
	GLOBAL *sp;		/* current symbol pointer */
	GLOBAL *bestsp;		/* found symbol with highest scope */
	long len;		/* length of string */

	bestsp = NULL;
	len = strlen(name);
	for (sp = globalhash[HASHSYM(name, len)]; sp; sp = sp->g_next) {
		if ((sp->g_len != len) || strcmp(sp->g_name, name))
			continue;
		if (sp->g_filescope == SCOPE_GLOBAL) {
			if (bestsp == NULL)
				bestsp = sp;
			continue;
		}
		if (sp->g_filescope != filescope)
			continue;
		if ((bestsp == NULL) || (sp->g_funcscope > bestsp->g_funcscope))
			bestsp = sp;
	}
	return bestsp;
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
 * tail of very large numbers.  Only truly global symbols are shown.
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
			if (sp->g_filescope != SCOPE_GLOBAL)
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
int
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
	math_setfp(fp);
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
			savemode = math_setmode(MODE_HEX);
			printvalue(&sp->g_value, PRINT_UNAMBIG);
			math_setmode(savemode);
			math_str(";\n");
		}
	}
	math_setfp(stdout);
	if (fclose(fp))
		return 1;
	return 0;
}


/*
 * Reset the file and function scope levels back to the original values.
 * This is called on errors to forget any static variables which were being
 * defined.
 */
void
resetscopes()
{
	filescope = SCOPE_STATIC;
	funcscope = 0;
	unscope();
}


/*
 * Enter a new file scope level so that newly defined static variables
 * will have the appropriate scope, and so that previously defined static
 * variables will temporarily be unaccessible.  This should only be called
 * when the function scope level is zero.
 */
void
enterfilescope()
{
	filescope++;
	funcscope = 0;
}


/*
 * Exit from a file scope level.  This deletes from the global symbol table
 * all of the static variables that were defined within this file scope level.
 * The function scope level is also reset to zero.
 */
void
exitfilescope()
{
	if (filescope > SCOPE_STATIC)
		filescope--;
	funcscope = 0;
	unscope();
}


/*
 * Enter a new function scope level within the current file scope level.
 * This allows newly defined static variables to override previously defined
 * static variables in the same file scope level.
 */
void
enterfuncscope()
{
	funcscope++;
}


/*
 * Exit from a function scope level.  This deletes static symbols which were
 * defined within the current function scope level, and makes previously
 * defined symbols with the same name within the same file scope level
 * accessible again.
 */
void
exitfuncscope()
{
	if (funcscope > 0)
		funcscope--;
	unscope();
}


/*
 * Remove all the symbols from the global symbol table which have file or
 * function scopes larger than the current scope levels.  Their memory
 * remains allocated since their values still actually exist.
 */
static void
unscope()
{
	GLOBAL **hp;			/* hash table head address */
	register GLOBAL *sp;		/* current global symbol pointer */
	GLOBAL *prevsp;			/* previous kept symbol pointer */

	for (hp = &globalhash[HASHSIZE-1]; hp >= globalhash; hp--) {
		prevsp = NULL;
		for (sp = *hp; sp; sp = sp->g_next) {
			if ((sp->g_filescope == SCOPE_GLOBAL) ||
				(sp->g_filescope < filescope) ||
				((sp->g_filescope == filescope) &&
					(sp->g_funcscope <= funcscope)))
			{
				prevsp = sp;
				continue;
			}

			/*
			 * This symbol needs removing.
			 */
			if (prevsp)
				prevsp->g_next = sp->g_next;
			else
				*hp = sp->g_next;
		}
	}
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
 * This is either local, parameter, global, static, or undefined.
 */
int
symboltype(name)
	char *name;		/* variable name to find */
{
	GLOBAL *sp;

	if (findlocal(name) >= 0)
		return SYM_LOCAL;
	if (findparam(name) >= 0)
		return SYM_PARAM;
	sp = findglobal(name);
	if (sp) {
		if (sp->g_filescope == SCOPE_GLOBAL)
			return SYM_GLOBAL;
		return SYM_STATIC;
	}
	return SYM_UNDEFINED;
}

/* END CODE */
