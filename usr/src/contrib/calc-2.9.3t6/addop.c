/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Add opcodes to a function being compiled.
 */

#include "calc.h"
#include "opcodes.h"
#include "string.h"
#include "func.h"
#include "token.h"
#include "label.h"
#include "symbol.h"


#define	FUNCALLOCSIZE	20	/* reallocate size for functions */
#define	OPCODEALLOCSIZE	100	/* reallocate size for opcodes in functions */


static long maxopcodes;		/* number of opcodes available */
static long newindex;		/* index of new function */
static long oldop;		/* previous opcode */
static long debugline;		/* line number of latest debug opcode */
static long funccount;		/* number of functions */
static long funcavail;		/* available number of functions */
static FUNC *functemplate;	/* function definition template */
static FUNC **functions;	/* table of functions */
static STRINGHEAD funcnames;	/* function names */
static int codeflag;


/*
 * Initialize the table of user defined functions.
 */
void
initfunctions()
{
	initstr(&funcnames);
	maxopcodes = OPCODEALLOCSIZE;
	functemplate = (FUNC *) malloc(funcsize(maxopcodes));
	if (functemplate == NULL)
		math_error("Cannot allocate function template");
	functions = (FUNC **) malloc(sizeof(FUNC *) * FUNCALLOCSIZE);
	if (functions == NULL)
		math_error("Cannot allocate function table");
	funccount = 0;
	funcavail = FUNCALLOCSIZE;
}


/*
 * Show the list of user defined functions.
 */
void
showfunctions()
{
	FUNC **fpp;		/* pointer into function table */
	FUNC *fp;		/* current function */

	if (funccount == 0) {
		printf("No user functions defined.\n");
		return;
	}
	printf("Name Arguments\n");
	printf("---- ---------\n");
	for (fpp = &functions[funccount - 1]; fpp >= functions; fpp--) {
		fp = *fpp;
		if (fp == NULL)
			continue;
		printf("%-12s %-2d\n", fp->f_name, fp->f_paramcount);
	}
	printf("\n");
}


/*
 * Initialize a function for definition.
 * Newflag is TRUE if we should allocate a new function structure,
 * instead of the usual overwriting of the template function structure.
 * The new structure is returned in the global curfunc variable.
 */
void
beginfunc(name, newflag)
	char *name;			/* name of function */
	BOOL newflag;			/* TRUE if need new structure */
{
	register FUNC *fp;		/* current function */

	newindex = adduserfunc(name);
	maxopcodes = OPCODEALLOCSIZE;
	fp = functemplate;
	if (newflag) {
		fp = (FUNC *) malloc(funcsize(maxopcodes));
		if (fp == NULL)
			math_error("Cannot allocate temporary function");
	}
	fp->f_next = NULL;
	fp->f_localcount = 0;
	fp->f_opcodecount = 0;
	fp->f_savedvalue.v_type = V_NULL;
	fp->f_name = namestr(&funcnames, newindex);
	curfunc = fp;
	initlocals();
	initlabels();
	oldop = OP_NOP;
	debugline = 0;
	errorcount = 0;
}


/*
 * Commit the just defined function for use.
 * This replaces any existing definition for the function.
 * This should only be called for normal user-defined functions.
 */
void
endfunc()
{
	register FUNC *fp;		/* function just finished */
	long size;			/* size of just created function */

	checklabels();
	if (errorcount) {
		printf("\"%s\": %ld error%s\n", curfunc->f_name, errorcount,
			((errorcount == 1) ? "" : "s"));
		return;
	}
	size = funcsize(curfunc->f_opcodecount);
	fp = (FUNC *) malloc(size);
	if (fp == NULL)
		math_error("Cannot commit function");
	memcpy((char *) fp, (char *) curfunc, size);
	if (curfunc != functemplate)
		free(curfunc);
	if (codeflag) {
		for (size = 0; size < fp->f_opcodecount; ) {
			printf("%ld: ", (long)size);
			size += dumpop(&fp->f_opcodes[size]);
		}
	}
	if (functions[newindex])
		free(functions[newindex]);
	functions[newindex] = fp;
	objuncache();
	if (inputisterminal())
		printf("\"%s\" defined\n", fp->f_name);
}


/*
 * Find the user function with the specified name, and return its index.
 * If the function does not exist, its name is added to the function table
 * and an error will be generated when it is called if it is still undefined.
 */
long
adduserfunc(name)
	char *name;		/* name of function */
{
	long index;		/* index of function */

	index = findstr(&funcnames, name);
	if (index >= 0)
		return index;
	if (funccount >= funcavail) {
		functions = (FUNC **) realloc(functions,
			sizeof(FUNC *) * (funcavail + FUNCALLOCSIZE));
		if (functions == NULL)
			math_error("Failed to reallocate function table");
		funcavail += FUNCALLOCSIZE;
	}
	if (addstr(&funcnames, name) == NULL)
		math_error("Cannot save function name");
	index = funccount++;
	functions[index] = NULL;
	return index;
}


/*
 * Clear any optimization that may be done for the next opcode.
 * This is used when defining a label.
 */
void
clearopt()
{
	oldop = OP_NOP;
	debugline = 0;
}


/*
 * Find a function structure given its index.
 */
FUNC *
findfunc(index)
	long index;
{
	if ((unsigned long) index >= funccount)
		math_error("Undefined function");
	return functions[index];
}


/*
 * Return the name of a function given its index.
 */
char *
namefunc(index)
	long index;
{
	return namestr(&funcnames, index);
}


/*
 * Let a matrix indexing operation know that it will be treated as a write
 * reference instead of just as a read reference.
 */
void
writeindexop()
{
	if (oldop == OP_INDEXADDR)
		curfunc->f_opcodes[curfunc->f_opcodecount - 1] = TRUE;
}


/*
 * Add an opcode to the current function being compiled.
 * Note: This can change the curfunc global variable when the
 * function needs expanding.
 */
void
addop(op)
	long op;
{
	register FUNC *fp;		/* current function */
	NUMBER *q;

	fp = curfunc;
	if ((fp->f_opcodecount + 5) >= maxopcodes) {
		maxopcodes += OPCODEALLOCSIZE;
		fp = (FUNC *) malloc(funcsize(maxopcodes));
		if (fp == NULL)
			math_error("cannot malloc function");
		memcpy((char *) fp, (char *) curfunc,
			funcsize(curfunc->f_opcodecount));
		if (curfunc != functemplate)
			free(curfunc);
		curfunc = fp;
	}
	/*
	 * Check the current opcode against the previous opcode and try to
	 * slightly optimize the code depending on the various combinations.
	 */
	if (op == OP_GETVALUE) {
		switch (oldop) {

		case OP_NUMBER: case OP_ZERO: case OP_ONE: case OP_IMAGINARY:
		case OP_GETEPSILON: case OP_SETEPSILON: case OP_STRING:
		case OP_UNDEF: case OP_GETCONFIG: case OP_SETCONFIG:
			return;
		case OP_DUPLICATE:
			fp->f_opcodes[fp->f_opcodecount - 1] = OP_DUPVALUE;
			oldop = OP_DUPVALUE;
			return;
		case OP_FIADDR:
			fp->f_opcodes[fp->f_opcodecount - 1] = OP_FIVALUE;
			oldop = OP_FIVALUE;
			return;
		case OP_GLOBALADDR:
			fp->f_opcodes[fp->f_opcodecount - 2] = OP_GLOBALVALUE;
			oldop = OP_GLOBALVALUE;
			return;
		case OP_LOCALADDR:
			fp->f_opcodes[fp->f_opcodecount - 2] = OP_LOCALVALUE;
			oldop = OP_LOCALVALUE;
			return;
		case OP_PARAMADDR:
			fp->f_opcodes[fp->f_opcodecount - 2] = OP_PARAMVALUE;
			oldop = OP_PARAMVALUE;
			return;
		case OP_ELEMADDR:
			fp->f_opcodes[fp->f_opcodecount - 2] = OP_ELEMVALUE;
			oldop = OP_ELEMVALUE;
			return;
		}
	}
	if ((op == OP_NEGATE) && (oldop == OP_NUMBER)) {
		q = constvalue(fp->f_opcodes[fp->f_opcodecount - 1]);
		fp->f_opcodes[fp->f_opcodecount - 1] = addqconstant(qneg(q));
		oldop = OP_NUMBER;
		return;
	}
	if ((op == OP_POWER) && (oldop == OP_NUMBER)) {
		if (qcmpi(constvalue(fp->f_opcodes[fp->f_opcodecount - 1]), 2L) == 0) {
			fp->f_opcodecount--;
			fp->f_opcodes[fp->f_opcodecount - 1] = OP_SQUARE;
			oldop = OP_SQUARE;
			return;
		}
		if (qcmpi(constvalue(fp->f_opcodes[fp->f_opcodecount - 1]), 4L) == 0) {
			fp->f_opcodes[fp->f_opcodecount - 2] = OP_SQUARE;
			fp->f_opcodes[fp->f_opcodecount - 1] = OP_SQUARE;
			oldop = OP_SQUARE;
			return;
		}
	}
	if ((op == OP_POP) && (oldop == OP_ASSIGN)) {	/* optimize */
		fp->f_opcodes[fp->f_opcodecount - 1] = OP_ASSIGNPOP;
		oldop = OP_ASSIGNPOP;
		return;
	}
	/*
	 * No optimization possible, so store the opcode.
	 */
	fp->f_opcodes[fp->f_opcodecount] = op;
	fp->f_opcodecount++;
	oldop = op;
}


/*
 * Add an opcode and and one integer argument to the current function
 * being compiled.
 */
void
addopone(op, arg)
	long op;
	long arg;
{
	NUMBER *q;

	switch (op) {
	case OP_NUMBER:
		q = constvalue(arg);
		if (q == NULL)
			break;
		if (qiszero(q)) {
			addop(OP_ZERO);
			return;
		}
		if (qisone(q)) {
			addop(OP_ONE);
			return;
		}
		break;

	case OP_DEBUG:
		if ((traceflags & TRACE_NODEBUG) || (arg == debugline))
			return;
		debugline = arg;
		if (oldop == OP_DEBUG) {
			curfunc->f_opcodes[curfunc->f_opcodecount - 1] = arg;
			return;
		}
		break;
	}
	addop(op);
	curfunc->f_opcodes[curfunc->f_opcodecount] = arg;
	curfunc->f_opcodecount++;
}


/*
 * Add an opcode and and two integer arguments to the current function
 * being compiled.
 */
void
addoptwo(op, arg1, arg2)
	long op;
	long arg1;
	long arg2;
{
	addop(op);
	curfunc->f_opcodes[curfunc->f_opcodecount++] = arg1;
	curfunc->f_opcodes[curfunc->f_opcodecount++] = arg2;
}


/*
 * Add an opcode and a character pointer to the function being compiled.
 */
void
addopptr(op, ptr)
	long op;
	char *ptr;
{
	char **ptraddr;

	addop(op);
	ptraddr = (char **) &curfunc->f_opcodes[curfunc->f_opcodecount];
	*ptraddr = ptr;
	curfunc->f_opcodecount += PTR_SIZE;
}


/*
 * Add an opcode and an index and an argument count for a function call.
 */
void
addopfunction(op, index, count)
	int count;
	long op;
	long index;
{
	long newop;

	if ((op == OP_CALL) && ((newop = builtinopcode(index)) != OP_NOP)) {
		if ((newop == OP_SETCONFIG) && (count == 1))
			newop = OP_GETCONFIG;
		if ((newop == OP_SETEPSILON) && (count == 0))
			newop = OP_GETEPSILON;
		if ((newop == OP_ABS) && (count == 1))
			addop(OP_GETEPSILON);
		addop(newop);
		return;
	}
	addop(op);
	curfunc->f_opcodes[curfunc->f_opcodecount++] = index;
	curfunc->f_opcodes[curfunc->f_opcodecount++] = count;
}


/*
 * Add a jump-type opcode and a label to the function being compiled.
 */
void
addoplabel(op, label)
	long op;
	LABEL *label;		/* label to be added */
{
	addop(op);
	uselabel(label);
}

/* END CODE */
