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
static char sccsid[] = "@(#)maketypes.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * make symbols
 */

#include "defs.h"
#include "sym.h"
#include "symtab.h"
#include "sym/btypes.h"
#include "sym/classes.h"
#include "sym/sym.rep"

/*
 * point the basic types in the right direction
 */

maketypes()
{
	t_int = st_lookup(symtab, "integer")->type;
	t_real = st_lookup(symtab, "real")->type;
	t_char = st_lookup(symtab, "char")->type;
	t_boolean = st_lookup(symtab, "boolean")->type;
	if (t_int==NIL || t_real==NIL || t_char==NIL || t_boolean==NIL) {
		panic("basic types are missing from namelist");
	}
}

/*
 * enter a keyword in the given table
 */

make_keyword(table, name, tnum)
SYMTAB *table;
char *name;
int tnum;
{
	register SYM *s;

	s = st_insert(table, name);
	s->class = BADUSE;
	s->blkno = 0;
	s->symvalue.token.toknum = tnum;
}
