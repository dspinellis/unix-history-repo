/*-
 * Copyright (c) 1993
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
 *
 *	@(#)hdr.h	8.1 (Berkeley) 6/9/93
 */

/*
 * The HDR structure is a special structure for heading doubly linked lists.
 * It depends on all structures using these lists beginning with the exact
 * same four pointer fields.  There are macros below which insert, append
 * and delete into doubly linked lists.
 */
typedef struct _hdr {
	void *next;			/* First set of pointers. */
	void *prev;
	void *snext;			/* Second set of pointers. */
	void *sprev;

#define	HDR_INUSE	0x01		/* Is active flag. */
	u_char	flags;
} HDR;

/* Initialize the doubly linked list header. */
#define	HDR_INIT(hdr, n, p) {						\
	(hdr).n = (hdr).p = &(hdr);					\
}

/* Insert after node in a doubly linked list. */
#define HDR_APPEND(ins, node, n, p, cast) {				\
        (ins)->n = ((cast *)(node))->n;					\
        (ins)->p = (cast *)(node);					\
        ((cast *)(node))->n->p = (ins);					\
        ((cast *)(node))->n = (ins);					\
}

/* Insert before node in a doubly linked list. */
#define HDR_INSERT(ins, node, n, p, cast) {				\
	((cast *)(node))->p->n = (ins);					\
	(ins)->p = ((cast *)(node))->p;					\
	((cast *)(node))->p = (ins);					\
	(ins)->n = (cast *)(node);					\
}

/* Delete node from doubly linked list. */
#define	HDR_DELETE(node, n, p, cast) {					\
	((cast *)(node))->p->n = ((cast *)node)->n;			\
	((cast *)(node))->n->p = ((cast *)node)->p;			\
}
