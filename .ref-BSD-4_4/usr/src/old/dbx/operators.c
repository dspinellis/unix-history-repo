/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)operators.c	5.3 (Berkeley) 6/1/90";
#endif /* not lint */

/*
 * Tree node classes.
 */

#include "defs.h"
#include "operators.h"

#ifndef public
typedef struct {
    char numargs;
    char opflags;
    String opstring;
} Opinfo;

typedef enum {
    O_NOP,
    O_NAME, O_SYM, O_LCON, O_CCON, O_FCON, O_SCON,
    O_RVAL, O_INDEX, O_INDIR, O_DOT,
    O_COMMA,

    O_ITOF, O_ADD, O_ADDF, O_SUB, O_SUBF, O_NEG, O_NEGF,
    O_MUL, O_MULF, O_DIVF, O_DIV, O_MOD,

    O_AND, O_OR,

    O_LT, O_LTF, O_LE, O_LEF, O_GT, O_GTF, O_GE, O_GEF,
    O_EQ, O_EQF, O_NE, O_NEF,

    O_ALIAS,		/* rename a command */
    O_ASSIGN,		/* assign a value to a program variable */
    O_CALL,		/* call a procedure in the program */
    O_CATCH,		/* catch a signal before program does */
    O_CHFILE,		/* change (or print) the current source file */
    O_CONT,		/* continue execution */
    O_DEBUG,		/* invoke a dbx internal debugging routine */
    O_DELETE,		/* remove a trace/stop */
    O_DUMP,		/* dump out variables */
    O_EDIT,		/* edit a file (or function) */
    O_FUNC,		/* set the current function */
    O_GRIPE,		/* send mail to debugger support person */
    O_HELP,		/* print a synopsis of debugger commands */
    O_IGNORE,		/* let program catch signal */
    O_LIST,		/* list source lines */
    O_PRINT,		/* print the values of a list of expressions */
    O_PSYM,		/* print symbol information */
    O_RUN,		/* start up program */
    O_SKIP,		/* skip the current line */
    O_SOURCE,		/* read commands from a file */
    O_STATUS,		/* display currently active trace/stop's */
    O_STEP,		/* execute a single line */
    O_STOP,		/* stop on an event */
    O_STOPI,		/* stop on an event at an instruction boundary */
    O_TRACE,		/* trace something on an event */
    O_TRACEI,		/* trace at the instruction level */
    O_WHATIS,		/* print the declaration of a variable */
    O_WHERE,		/* print a stack trace */
    O_WHEREIS,		/* print all the symbols with the given name */
    O_WHICH,		/* print out full qualification of a symbol */
    O_EXAMINE,		/* examine program instructions/data */

    O_ADDEVENT,		/* add an event */
    O_ENDX,		/* end of program reached */
    O_IF,		/* if first arg is true, do commands in second arg */
    O_ONCE,		/* add a "one-time" event, delete when first reached */
    O_PRINTCALL,	/* print out the current procedure and its arguments */
    O_PRINTIFCHANGED,	/* print the value of the argument if it has changed */
    O_PRINTRTN,		/* print out the routine and value that just returned */
    O_PRINTSRCPOS,	/* print out the current source position */
    O_PROCRTN,		/* call completed */
    O_QLINE,		/* filename, line number */
    O_STOPIFCHANGED,	/* stop if the value of the argument has changed */
    O_STOPX,		/* stop execution */
    O_TRACEON,		/* begin tracing source line, variable, or all lines */
    O_TRACEOFF,		/* end tracing source line, variable, or all lines */

    O_TYPERENAME,	/* state the type of an expression */
    O_RERUN,		/* re-run program with the same arguments as before */
    O_RETURN,		/* continue execution until procedure returns */
    O_UP,		/* move current function up the call stack */
    O_DOWN,		/* move current function down the call stack */
    O_CALLPROC,		/* call command */
    O_SEARCH,		/* regular expression pattern search through source */
    O_SET,		/* set a debugger variable */
    O_UNSET,		/* unset a debugger variable */
    O_UNALIAS,		/* remove an alias */

    O_LASTOP
} Operator;

/*
 * Operator flags and predicates.
 */

#define null 0
#define LEAF 01
#define UNARY 02
#define BINARY 04
#define BOOL 010
#define REALOP 020
#define INTOP 040

#define isbitset(a, m)	((a&m) == m)
#define isleaf(o)	isbitset(opinfo[ord(o)].opflags, LEAF)
#define isunary(o)	isbitset(opinfo[ord(o)].opflags, UNARY)
#define isbinary(o)	isbitset(opinfo[ord(o)].opflags, BINARY)
#define isreal(o)	isbitset(opinfo[ord(o)].opflags, REALOP)
#define isint(o)	isbitset(opinfo[ord(o)].opflags, INTOP)
#define isboolean(o)	isbitset(opinfo[ord(o)].opflags, BOOL)

#define degree(o)	(opinfo[ord(o)].opflags&(LEAF|UNARY|BINARY))
#define nargs(o)	(opinfo[ord(o)].numargs)

#endif

/*
 * Operator information structure.
 */

public Opinfo opinfo[] ={
/* O_NOP */		0,	null,		0,
/* O_NAME */		-1,	LEAF,		0,
/* O_SYM */		-1,	LEAF,		0,
/* O_LCON */		-1,	LEAF,		0,
/* O_CCON */		-1,	LEAF,		0,
/* O_FCON */		-1,	LEAF,		0,
/* O_SCON */		-1,	LEAF,		0,
/* O_RVAL */		1,	UNARY,		0,
/* O_INDEX */		2,	null,		0,
/* O_INDIR */		1,	UNARY,		"^",
/* O_DOT */		2,	null,		".",
/* O_COMMA */		2,	null,		",",
/* O_ITOF */		1,	UNARY|INTOP,	0,
/* O_ADD */		2,	BINARY|INTOP,	"+",
/* O_ADDF */		2,	BINARY|REALOP,	"+",
/* O_SUB */		2,	BINARY|INTOP,	"-",
/* O_SUBF */		2,	BINARY|REALOP,	"-",
/* O_NEG */		1,	UNARY|INTOP,	"-",
/* O_NEGF */		1,	UNARY|REALOP,	"-",
/* O_MUL */		2,	BINARY|INTOP,	"*",
/* O_MULF */		2,	BINARY|REALOP,	"*",
/* O_DIVF */		2,	BINARY|REALOP,	"/",
/* O_DIV */		2,	BINARY|INTOP,	" div ",
/* O_MOD */		2,	BINARY|INTOP,	" mod ",
/* O_AND */		2,	BINARY|INTOP,	" and ",
/* O_OR */		2,	BINARY|INTOP,	" or ",
/* O_LT */		2,	BINARY|INTOP,	" < ",
/* O_LTF */		2,	BINARY|REALOP,	" < ",
/* O_LE */		2,	BINARY|INTOP,	" <= ",
/* O_LEF */		2,	BINARY|REALOP,	" <= ",
/* O_GT */		2,	BINARY|INTOP,	" > ",
/* O_GTF */		2,	BINARY|REALOP,	" > ",
/* O_GE */		2,	BINARY|INTOP,	" >= ",
/* O_GEF */		2,	BINARY|REALOP,	" >= ",
/* O_EQ */		2,	BINARY|INTOP,	" = ",
/* O_EQF */		2,	BINARY|REALOP,	" = ",
/* O_NE */		2,	BINARY|INTOP,	" <> ",
/* O_NEF */		2,	BINARY|REALOP,	" <> ",

/* O_ALIAS */		2,	null,		"alias",
/* O_ASSIGN */		2,	null,		" := ",
/* O_CALL */		2,	null,		"call",
/* O_CATCH */		0,	null,		"catch",
/* O_CHFILE */		0,	null,		"file",
/* O_CONT */		0,	null,		"cont",
/* O_DEBUG */		0,	null,		"debug",
/* O_DELETE */		1,	null,		"delete",
/* O_DUMP */		1,	null,		"dump",
/* O_EDIT */		0,	null,		"edit",
/* O_FUNC */		1,	null,		"func",
/* O_GRIPE */		0,	null,		"gripe",
/* O_HELP */		0,	null,		"help",
/* O_IGNORE */		0,	null,		"ignore",
/* O_LIST */		2,	null,		"list",
/* O_PRINT */		1,	null,		"print",
/* O_PSYM */		1,	null,		"psym",
/* O_RUN */		0,	null,		"run",
/* O_SKIP */		0,	null,		"skip",
/* O_SOURCE */		0,	null,		"source",
/* O_STATUS */		0,	null,		"status",
/* O_STEP */		0,	null,		"step",
/* O_STOP */		3,	null,		"stop",
/* O_STOPI */		3,	null,		"stopi",
/* O_TRACE */		3,	null,		"trace",
/* O_TRACEI */		3,	null,		"tracei",
/* O_WHATIS */		1,	null,		"whatis",
/* O_WHERE */		0,	null,		"where",
/* O_WHEREIS */		1,	null,		"whereis",
/* O_WHICH */		1,	null,		"which",
/* O_EXAMINE */		0,	null,		"examine",

/* O_ADDEVENT */	0,	null,		"when",
/* O_ENDX */		0,	null,		nil,
/* O_IF */		0,	null,		"if",
/* O_ONCE */		0,	null,		"once",
/* O_PRINTCALL */	1,	null,		"printcall",
/* O_PRINTIFCHANGED */	1,	null,		"printifchanged",
/* O_PRINTRTN */	1,	null,		"printrtn",
/* O_PRINTSRCPOS */	1,	null,		"printsrcpos",
/* O_PROCRTN */		1,	null,		"procrtn",
/* O_QLINE */		2,	null,		nil,
/* O_STOPIFCHANGED */	1,	null,		"stopifchanged",
/* O_STOPX */		0,	null,		"stop",
/* O_TRACEON */		1,	null,		"traceon",
/* O_TRACEOFF */	1,	null,		"traceoff",
/* O_TYPERENAME */	2,	UNARY,		"type rename",
/* O_RERUN */		0,	null,		"rerun",
/* O_RETURN */		1,	null,		"return",
/* O_UP */		1,	UNARY,		"up",
/* O_DOWN */		1,	UNARY,		"down",
/* O_CALLPROC */	2,	null,		"call",
/* O_SEARCH */		2,	null,		"search",
/* O_SET */		2,	null,		"set",
/* O_UNSET */		1,	null,		"unset",
/* O_UNALIAS */		1,	null,		"unalias",
};
