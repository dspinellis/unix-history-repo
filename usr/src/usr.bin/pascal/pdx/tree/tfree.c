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
static char sccsid[] = "@(#)tfree.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * Free a tree; this is expensive but useful.
 */

#include "defs.h"
#include "tree.h"
#include "sym.h"
#include "tree.rep"

tfree(p)
register NODE *p;
{
	if (p == NIL) {
		return;
	}
	switch(degree(p->op)) {
		case LEAF:
			switch(p->op) {
				case O_CALL:
					tfree(p->left);
					tfree(p->right);
					break;

				case O_QLINE:
					dispose(p->left->sconval);
					dispose(p->left);
					tfree(p->right);
					break;

				case O_ALIAS:
					dispose(p->left->sconval);
					dispose(p->left);
					dispose(p->right->sconval);
					dispose(p->right);
					break;

				case O_SCON:
					unmkstring(p->nodetype);
					free(p->nodetype);
					free(p->sconval);
					p->sconval = NIL;
					break;
			}
			break;

		case BINARY:
			tfree(p->right);
			/* fall through */
		case UNARY:
			tfree(p->left);
			break;

		default:
			panic("bad op %d in tfree", p->op);
	}
	dispose(p);
}
