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
static char sccsid[] = "@(#)opinfo.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * Operator information structure.
 */

#include "defs.h"
#include "opinfo.h"

OPINFO opinfo[] ={
/* O_NOP */		0,		0,
/* O_NAME */		LEAF,		0,
/* O_QNAME */		LEAF,		"$",
/* O_LCON */		LEAF,		0,
/* O_FCON */		LEAF,		0,
/* O_SCON */		LEAF,		0,
/* O_INDEX */		UNARY,		0,
/* O_INDIR */		UNARY,		"^",
/* O_RVAL */		UNARY,		0,
/* O_COMMA */		BINARY,		",",
/* O_ITOF */		UNARY|INTOP,	0,
/* O_ADD */		BINARY|INTOP,	"+",
/* O_ADDF */		BINARY|REALOP,	"+",
/* O_SUB */		BINARY|INTOP,	"-",
/* O_SUBF */		BINARY|REALOP,	"-",
/* O_NEG */		UNARY|INTOP,	"-",
/* O_NEGF */		UNARY|REALOP,	"-",
/* O_MUL */		BINARY|INTOP,	"*",
/* O_MULF */		BINARY|REALOP,	"*",
/* O_DIVF */		BINARY|REALOP,	"/",
/* O_DIV */		BINARY|INTOP,	" div ",
/* O_MOD */		BINARY|INTOP,	" mod ",
/* O_AND */		BINARY|INTOP,	" and ",
/* O_OR */		BINARY|INTOP,	" or ",
/* O_LT */		BINARY|INTOP,	" < ",
/* O_LTF */		BINARY|REALOP,	" < ",
/* O_LE */		BINARY|INTOP,	" <= ",
/* O_LEF */		BINARY|REALOP,	" <= ",
/* O_GT */		BINARY|INTOP,	" > ",
/* O_GTF */		BINARY|REALOP,	" > ",
/* O_GE */		BINARY|INTOP,	" >= ",
/* O_GEF */		BINARY|REALOP,	" >= ",
/* O_EQ */		BINARY|INTOP,	" = ",
/* O_EQF */		BINARY|REALOP,	" = ",
/* O_NE */		BINARY|INTOP,	" <> ",
/* O_NEF */		BINARY|REALOP,	" <> ",
/* O_ASSIGN */		BINARY,		" := ",
/* O_CHFILE */		0,		NIL,
/* O_CONT */		0,		NIL,
/* O_LIST */		0,		NIL,
/* O_NEXT */		0,		NIL,
/* O_PRINT */		0,		NIL,
/* O_STEP */		0,		NIL,
/* O_WHATIS */		0,		NIL,
/* O_WHERE */		0,		NIL,
/* O_XI */			0,		NIL,
/* O_XD */			0,		NIL,
/* O_CALL */		0,		NIL,
/* O_EDIT */		0,		NIL,
/* O_DUMP */		0,		NIL,
/* O_HELP */		0,		NIL,
/* O_REMAKE */		0,		NIL,
/* O_RUN */		0,		NIL,
/* O_SOURCE */		0,		NIL,
/* O_STATUS */		0,		NIL,
/* O_TRACE */		0,		NIL,
/* O_TRACEI */		0,		NIL,
/* O_STOP */		0,		NIL,
/* O_STOPI */		0,		NIL,
/* O_DELETE */		0,		NIL,
/* O_WHICH */		0,		NIL,
/* O_QLINE */		LEAF,		NIL,
/* O_ALIAS */		LEAF,		NIL,
/* O_GRIPE */		0,		NIL,
};
