/* 
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)queue.h	7.6 (Berkeley) %G%
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
