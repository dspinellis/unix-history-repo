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
static char sccsid[] = "@(#)prtree.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * Print a tree back out in Pascal form.
 */

#include "defs.h"
#include "tree.h"
#include "sym.h"
#include "sym/btypes.h"
#include "tree.rep"

prtree(p)
NODE *p;
{
	OP op;

	if (p == NIL) {
		return;
	}
	op = p->op;
	if (op < O_NOP || op > O_LASTOP) {
		panic("bad op %d in prtree", p->op);
	}
	switch (op) {
		case O_NAME: {
			SYM *s;

			s = p->nameval;
			if (isredirected() || isambiguous(s)) {
				printwhich(s);
			} else {
				printf("%s", name(s));
			}
			break;
		}

		case O_QNAME:
			prtree(p->left);
			printf(".%s", name(p->right->nameval));
			break;

		case O_QLINE:
			prtree(p->left);
			printf(":");
			prtree(p->right);
			break;

		case O_LCON:
			push(long, p->lconval);
			printval(p->nodetype);
			break;

		case O_FCON:
			printf("%g", p->fconval);
			break;

		case O_SCON:
			printf("'%s'", p->sconval);
			break;

		case O_INDEX:
			prtree(p->left);
			printf("[");
			prtree(p->right);
			printf("]");
			break;

		case O_COMMA:
			prtree(p->left);
			if (p->right != NIL) {
				printf(", ");
				prtree(p->right);
			}
			break;

		case O_RVAL:
		case O_ITOF:
			prtree(p->left);
			break;

		case O_CALL:
			prtree(p->left);
			if (p->right != NIL) {
				printf("(");
				prtree(p->right);
				printf(")");
			}
			break;

		case O_INDIR:
			prtree(p->left);
			if (!isvarparam(p->left->nodetype)) {
				printf("^");
			}
			break;

		default:
			switch(degree(op)) {
				case BINARY:
					prtree(p->left);
					printf("%s", opinfo[op].opstring);
					prtree(p->right);
					break;

				case UNARY:
					printf("%s", opinfo[op].opstring);
					prtree(p->left);
					break;

				default:
					panic("bad op %d in prtree", op);
			}
			break;
	}
}

/*
 * Print an error associated with a particular tree.
 * The string is searched for a "%t" which is replaced by
 * the printed representation of the tree.
 */

/* VARARGS2 */
trerror(s, tree, a, b, c, d, e, f, g, h, i, j)
char *s;
NODE *tree;
{
	register char *p;

	fflush(stdout);
	for (p = s; *p != '\0'; p++) {
		if (p[0] == '%' && p[1] == 't') {
			fputc('"', stderr);
			prtree(tree);
			fflush(stdout);
			fputc('"', stderr);
			error(&p[2], a, b, c, d, e, f, g, h, i, j);
			/* NOTREACHED */
		}
		fputc(*p, stderr);
	}
	panic("bad call to trerror");
}
