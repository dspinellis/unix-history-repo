/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
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
static char sccsid[] = "@(#)print.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * Routines to print out symbols.
 */

#include "defs.h"
#include "sym.h"
#include "process.h"
#include "tree.h"
#include "runtime.h"
#include "classes.h"
#include "sym.rep"
#include "process/process.rep"

/*
 * Note the entry of the given block, unless it's the main program.
 */

printentry(s)
SYM *s;
{
	if (s != program) {
		printf("\nentering %s %s\n", classname(s), s->symbol);
	}
}

/*
 * Note the exit of the given block
 */

printexit(s)
SYM *s;
{
	if (s != program) {
		printf("leaving %s %s\n\n", classname(s), s->symbol);
	}
}

/*
 * Note the call of s from t.
 */

printcall(s, t)
SYM *s, *t;
{
	printf("calling %s", s->symbol);
	printparams(s, NIL);
	printf(" from %s %s\n", classname(t), t->symbol);
}

/*
 * Note the return from s.  If s is a function, print the value
 * it is returning.  This is somewhat painful, since the function
 * has actually just returned.
 */

printrtn(s)
SYM *s;
{
	register int len;

	printf("returning ");
	if (s->class == FUNC) {
		len = size(s->type);
		dread(sp, process->sp, len);
		sp += len;
#ifdef tahoe
		alignstack();
#endif
		printval(s->type);
		putchar(' ');
	}
	printf("from %s\n", s->symbol);
}

/*
 * Print the values of the parameters of the given procedure or function.
 * The frame distinguishes recursive instances of a procedure.
 */

printparams(f, frame)
SYM *f;
FRAME *frame;
{
	SYM *param;

	for (param = f->chain; param != NIL; param = param->chain) {
		if (param == f->chain) {
			printf("(");
		}
		printv(param, frame);
		if (param->chain != NIL) {
			printf(", ");
		} else {
			printf(")");
		}
	}
}

/*
 * Print the name and value of a variable.
 */

printv(s, frame)
SYM *s;
FRAME *frame;
{
	ADDRESS addr;
	int len;

	if (s->class == REF) {
		dread(&addr, address(s, frame), sizeof(ADDRESS));
		len = size(s->type);
	} else {
		addr = address(s, frame);
		len = size(s);
	}
	printf("%s = ", s->symbol);
	if (!rpush(addr, len)) {
		printf("*** expression too large ***");
	} else {
		if (s->class == REF || s->class == VAR) {
			printval(s->type);
		} else {
			printval(s);
		}
	}
}

/*
 * Print the fully specified variable that is described by the given identifer.
 */

printwhich(s)
SYM *s;
{
	printouter(s->func);
	printf("%s", s->symbol);
}

LOCAL printouter(s)
SYM *s;
{
	if (s->func != NIL) {
		printouter(s->func);
	}
	printf("%s.", s->symbol);
}
