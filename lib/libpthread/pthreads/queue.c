/* ==== queue.c ============================================================
 * Copyright (c) 1993 by Chris Provenzano, proven@athena.mit.edu
 *
 * Description : Queue functions.
 *
 *  1.00 93/07/15 proven
 *      -Started coding this file.
 */

#include <pthread/copyright.h>
#include <pthread.h>

/*
 * All routines in this file assume that the queue has been appropriatly
 * locked.
 */

/* ==========================================================================
 * pthread_queue_init()
 */
void pthread_queue_init(struct pthread_queue *queue)
{
	queue->q_next = NULL;
	queue->q_last = NULL;
	queue->q_data = NULL;
}

/* ==========================================================================
 * pthread_queue_enq()
 */
void pthread_queue_enq(struct pthread_queue *queue, struct pthread *thread)
{
	if (queue->q_last) {
		queue->q_last->next = thread;
	} else {
		queue->q_next = thread;
	}
	queue->q_last = thread;
	thread->queue = queue;
	thread->next = NULL;
	
}

/* ==========================================================================
 * pthread_queue_get()
 */
struct pthread *pthread_queue_get(struct pthread_queue *queue)
{
	return(queue->q_next);
}

/* ==========================================================================
 * pthread_queue_deq()
 */
struct pthread *pthread_queue_deq(struct pthread_queue *queue)
{
	struct pthread *thread = NULL;

	if (queue->q_next) {
		thread = queue->q_next;
		if (!(queue->q_next = queue->q_next->next)) {
			queue->q_last = NULL;
		}
		thread->queue = NULL;
		thread->next = NULL;
	}
	return(thread);
}

/* ==========================================================================
 * pthread_queue_remove()
 */
void pthread_queue_remove(struct pthread_queue *queue, struct pthread *thread)
{
	struct pthread **current = &(queue->q_next);
	struct pthread *prev = NULL;

	while (*current) {
		if (*current == thread) {
			if ((*current)->next) {
				*current = (*current)->next;
			} else {
				queue->q_last = prev;
				*current = NULL;
			}
		}
		prev = *current;
		current = &((*current)->next);
	}
	thread->queue = NULL;
	thread->next = NULL;
}
