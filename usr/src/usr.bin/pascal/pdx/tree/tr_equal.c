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
static char sccsid[] = "@(#)tr_equal.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * A recursive tree search routine to test if two trees
 * are structurally equivalent.
 */

#include "defs.h"
#include "tree.h"
#include "tree.rep"

BOOLEAN tr_equal(t1, t2)
register NODE *t1;
register NODE *t2;
{
	if (t1 == NIL && t2 == NIL) {
		return(TRUE);
	}
	if (t1 == NIL || t2 == NIL) {
		return(FALSE);
	}
	if (t1->op != t2->op || degree(t1->op) != degree(t2->op)) {
		return(FALSE);
	}
	switch(degree(t1->op)) {
		case LEAF:
			switch(t1->op) {
				case O_NAME:
					return(t1->nameval == t2->nameval);

				case O_QNAME:
					if (!tr_equal(t1->right, t2->right)) {
						return(FALSE);
					}
					return(tr_equal(t1->left, t2->left));

				case O_LCON:
					return(t1->lconval == t2->lconval);

				case O_FCON:
					return(t1->fconval == t2->fconval);

				case O_SCON:
					return(t1->sconval == t2->sconval);

				default:
					panic("tr_equal: leaf %d\n", t1->op);
			}
			/*NOTREACHED*/

		case BINARY:
			if (!tr_equal(t1->right, t2->right)) {
				return(FALSE);
			}
			/* else fall through */
		case UNARY:
			return(tr_equal(t1->left, t2->left));

		default:
			panic("tr_equal: bad degree for op %d\n", t1->op);
	}
	/*NOTREACHED*/
}
