/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: queu.h,v 2.1 85/08/22 16:07:02 timo Exp $ */

/*
 * B editor -- Definitions for queues of nodes.
 */

typedef struct queue *queue;

struct queue {
	char	type;
	char	_unused;
	intlet	refcnt;
	intlet	len;
	node	q_data;
	queue	q_link;
};

#define Qnil ((queue) NULL)
#define qcopy(q) ((queue)copy((value)(q)))
#define qrelease(q) release((value)(q))
#define emptyqueue(q) (!(q))

node queuebehead();
