/* 
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)queue.h	7.5 (Berkeley) %G%
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
	void	*next;		/* next element */
	void	**prev;		/* address of previous next element */
};

struct list_entry {
	void	*next;		/* next element */
};

/*
 * Value for pointers on removed entries.
 */
#define	NOLIST	(void *)0xdead

/*
 * Queue functions.
 */
#define	queue_init(head)	((head)->next = 0, (head)->prev = &(head)->next)

#define queue_enter_tail(head, elm, type, field) { \
	(elm)->field.next = 0; \
	(elm)->field.prev = (head)->prev; \
	*(head)->prev = (elm); \
	(head)->prev = &(elm)->field.next; \
}

#define queue_enter_head(head, elm, type, field) { \
	type queue_ptr; \
	if (queue_ptr = (head)->next) \
		queue_ptr->field.prev = &(elm)->field.next; \
	else \
		(head)->prev = &(elm)->field.next; \
	(head)->next = (elm); \
	(elm)->field.next = queue_ptr; \
	(elm)->field.prev = &(head)->next; \
}

#define queue_insert_after(head, listelm, elm, type, field) { \
	type queue_ptr; \
	if (queue_ptr = (listelm)->next) \
		queue_ptr->field.prev = &(elm)->field.next; \
	else \
		(head)->prev = &(elm)->field.next; \
	(listelm)->next = (elm); \
	(elm)->field.next = queue_ptr; \
	(elm)->field.prev = &(listelm)->next; \
}

#define queue_remove(head, elm, type, field) { \
	type queue_ptr; \
	if (queue_ptr = (elm)->field.next) \
		queue_ptr->field.prev = (elm)->field.prev; \
	else \
		(head)->prev = (elm)->field.prev; \
	*(elm)->field.prev = queue_ptr; \
	(elm)->field.next = NOLIST; \
	(elm)->field.prev = NOLIST; \
}

/*
 * List functions.
 */
#define list_enter_head(head, elm, type, field) { \
	type queue_ptr; \
	if (queue_ptr = (head)->next) \
		queue_ptr->field.prev = &(elm)->field.next; \
	(head)->next = (elm); \
	(elm)->field.next = queue_ptr; \
	(elm)->field.prev = &(head)->next; \
}

#define list_insert_after(listelm, elm, type, field) { \
	type queue_ptr; \
	if (queue_ptr = (listelm)->next) \
		queue_ptr->field.prev = &(elm)->field.next; \
	(listelm)->next = (elm); \
	(elm)->field.next = queue_ptr; \
	(elm)->field.prev = &(listelm)->next; \
}

#define list_remove(elm, type, field) { \
	type queue_ptr; \
	if (queue_ptr = (elm)->field.next) \
		queue_ptr->field.prev = (elm)->field.prev; \
	*(elm)->field.prev = queue_ptr; \
	(elm)->field.next = NOLIST; \
	(elm)->field.prev = NOLIST; \
}

#endif	/* !_QUEUE_H_ */
