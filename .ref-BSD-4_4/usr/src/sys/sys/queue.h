/* 
 * Copyright (c) 1991, 1993
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
 *	@(#)queue.h	8.1 (Berkeley) 6/2/93
 */

#ifndef	_QUEUE_H_
#define	_QUEUE_H_

/*
 * This file defines two types of data structures, lists and queues.
 *
 * A list is headed by a single forward pointer (or an array of forward
 * pointers for a hash table header). The elements are doubly linked
 * so that an arbitrary element can be removed without a need to
 * traverse the list. New elements can be added to the list after
 * an existing element or at the head of the list.
 *
 * A queue is headed by a pair of pointers, one to the head of the list
 * and the other to the tail of the list. The elements are doubly linked
 * so that an arbitrary element can be removed without a need to
 * traverse the list. New elements can be added to the list after
 * an existing element, at the head of the list, or at the end of
 * the list.
 *
 * Note that the implementation used here avoids the need to special
 * case inserting into an empty list, deleting the last element from
 * a list, or inserting at the beginning or end of a list. The drawback
 * to this method is that it is not possible to traverse a list or
 * queue backwards.
 */

struct queue_entry {
	void	*qe_next;	/* next element */
	void	**qe_prev;	/* address of previous next element */
};

struct list_entry {
	void	*le_next;	/* next element */
};

/*
 * Value for pointers on removed entries.
 */
#define	NOLIST	(void *)0xdead

/*
 * Queue functions.
 */
#define	queue_init(head) { \
	(head)->qe_next = 0; \
	(head)->qe_prev = &(head)->qe_next; \
}

#define queue_enter_tail(head, elm, type, field) { \
	(elm)->field.qe_next = 0; \
	(elm)->field.qe_prev = (head)->qe_prev; \
	*(head)->qe_prev = (elm); \
	(head)->qe_prev = &(elm)->field.qe_next; \
}

#define queue_enter_head(head, elm, type, field) { \
	type queue_ptr; \
	if (queue_ptr = (head)->qe_next) \
		queue_ptr->field.qe_prev = &(elm)->field.qe_next; \
	else \
		(head)->qe_prev = &(elm)->field.qe_next; \
	(head)->qe_next = (elm); \
	(elm)->field.qe_next = queue_ptr; \
	(elm)->field.qe_prev = &(head)->qe_next; \
}

#define queue_insert_after(head, listelm, elm, type, field) { \
	type queue_ptr; \
	if (queue_ptr = (listelm)->qe_next) \
		queue_ptr->field.qe_prev = &(elm)->field.qe_next; \
	else \
		(head)->qe_prev = &(elm)->field.qe_next; \
	(listelm)->qe_next = (elm); \
	(elm)->field.qe_next = queue_ptr; \
	(elm)->field.qe_prev = &(listelm)->qe_next; \
}

#define queue_remove(head, elm, type, field) { \
	type queue_ptr; \
	if (queue_ptr = (elm)->field.qe_next) \
		queue_ptr->field.qe_prev = (elm)->field.qe_prev; \
	else \
		(head)->qe_prev = (elm)->field.qe_prev; \
	*(elm)->field.qe_prev = queue_ptr; \
	(elm)->field.qe_next = NOLIST; \
	(elm)->field.qe_prev = NOLIST; \
}

/*
 * List functions.
 */
#define list_enter_head(head, elm, type, field) { \
	type queue_ptr; \
	if (queue_ptr = (head)->le_next) \
		queue_ptr->field.qe_prev = &(elm)->field.qe_next; \
	(head)->le_next = (elm); \
	(elm)->field.qe_next = queue_ptr; \
	(elm)->field.qe_prev = &(head)->le_next; \
}

#define list_insert_after(listelm, elm, type, field) { \
	type queue_ptr; \
	if (queue_ptr = (listelm)->qe_next) \
		queue_ptr->field.qe_prev = &(elm)->field.qe_next; \
	(listelm)->qe_next = (elm); \
	(elm)->field.qe_next = queue_ptr; \
	(elm)->field.qe_prev = &(listelm)->qe_next; \
}

#define list_remove(elm, type, field) { \
	type queue_ptr; \
	if (queue_ptr = (elm)->field.qe_next) \
		queue_ptr->field.qe_prev = (elm)->field.qe_prev; \
	*(elm)->field.qe_prev = queue_ptr; \
	(elm)->field.qe_next = NOLIST; \
	(elm)->field.qe_prev = NOLIST; \
}

#endif	/* !_QUEUE_H_ */
