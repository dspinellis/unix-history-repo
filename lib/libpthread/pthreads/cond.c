/* ==== cond.c ============================================================
 * Copyright (c) 1993 by Chris Provenzano, proven@athena.mit.edu
 *
 * Description : Condition cariable functions.
 *
 *  1.00 93/10/28 proven
 *      -Started coding this file.
 */

#include <pthread/copyright.h>
#include "pthread.h"
#include <errno.h>

/* ==========================================================================
 * pthread_cond_init()
 *
 * In this implementation I don't need to allocate memory.
 * ENOMEM, EAGAIN should never be returned. Arch that have
 * weird constraints may need special coding.
 */
int pthread_cond_init(pthread_cond_t *cond, pthread_condattr_t *cond_attr)
{
	/* Only check if attr specifies some mutex type other than fast */
	if ((cond_attr) && (cond_attr->c_type != COND_TYPE_FAST)) {
		if (cond_attr->c_type >= COND_TYPE_MAX) {
			return(EINVAL);
		}
		if (cond->c_flags & COND_FLAGS_INITED) {	
			return(EBUSY);
		}
		cond->c_type = cond_attr->c_type;
	} else {
		cond->c_type = COND_TYPE_FAST;
	}
	/* Set all other paramaters */
	pthread_queue_init(&cond->c_queue);
	cond->c_flags 	|= COND_FLAGS_INITED;
	cond->c_lock 	= SEMAPHORE_CLEAR;
	return(OK);
}

/* ==========================================================================
 * pthread_cond_destroy()
 */
int pthread_cond_destroy(pthread_cond_t *cond)
{
	/* Only check if cond is of type other than fast */
	switch(cond->c_type) {
	case COND_TYPE_FAST:
		break;
	case COND_TYPE_STATIC_FAST:
	default:
		return(EINVAL);
		break;
	}

	/* Cleanup cond, others might want to use it. */
	pthread_queue_init(&cond->c_queue);
	cond->c_flags 	|= COND_FLAGS_INITED;
	cond->c_lock 	= SEMAPHORE_CLEAR;
	cond->c_flags	= 0;
	return(OK);
}

/* ==========================================================================
 * pthread_cond_wait()
 */
int pthread_cond_wait(pthread_cond_t *cond, pthread_mutex_t *mutex)
{
	semaphore *lock, *plock;
	int rval;

	lock = &(cond->c_lock);
	while (SEMAPHORE_TEST_AND_SET(lock)) {
		pthread_yield();
	}
	
	switch (cond->c_type) {
	/*
	 * Fast condition variables do not check for any error conditions.
     */
	case COND_TYPE_FAST: 
	case COND_TYPE_STATIC_FAST:
		plock = &(pthread_run->lock);
		while (SEMAPHORE_TEST_AND_SET(plock)) {
			 pthread_yield();
		}
		pthread_queue_enq(&cond->c_queue, pthread_run);
		pthread_mutex_unlock(mutex);
		SEMAPHORE_RESET(lock);

		/* Reschedule will unlock pthread_run */
		reschedule(PS_COND_WAIT);

		return(pthread_mutex_lock(mutex));
		break;
	default:
		rval = EINVAL;
		break;
	}
	SEMAPHORE_RESET(lock);
	return(rval);
}

/* ==========================================================================
 * pthread_cond_signal()
 */
int pthread_cond_signal(pthread_cond_t *cond)
{
	struct pthread *pthread;
	semaphore *lock, *plock;
	int rval;

	lock = &(cond->c_lock);
	while (SEMAPHORE_TEST_AND_SET(lock)) {
		pthread_yield();
	}
	
	switch (cond->c_type) {
	case COND_TYPE_FAST: 
	case COND_TYPE_STATIC_FAST:
		if (pthread = pthread_queue_get(&cond->c_queue)) {
			plock = &(pthread->lock);
			while (SEMAPHORE_TEST_AND_SET(plock)) {
				 pthread_yield();
			}
			pthread_queue_deq(&cond->c_queue);
			pthread->state = PS_RUNNING;
			SEMAPHORE_RESET(plock);
		}
		rval = OK;
		break;
	default:
		rval = EINVAL;
		break;
	}
	SEMAPHORE_RESET(lock);
	return(rval);
}

/* ==========================================================================
 * pthread_cond_broadcast() 
 *
 * Not much different then the above routine.
 */
int pthread_cond_broadcast(pthread_cond_t *cond)
{
	struct pthread *pthread;
	semaphore *lock, *plock;
	int rval;

	lock = &(cond->c_lock);
	while (SEMAPHORE_TEST_AND_SET(lock)) {
		pthread_yield();
	}
	
	switch (cond->c_type) {
	case COND_TYPE_FAST: 
	case COND_TYPE_STATIC_FAST:
		while (pthread = pthread_queue_get(&cond->c_queue)) {
			plock = &(pthread->lock);
			while (SEMAPHORE_TEST_AND_SET(plock)) {
				 pthread_yield();
			}
			pthread_queue_deq(&cond->c_queue);
			pthread->state = PS_RUNNING;
			SEMAPHORE_RESET(plock);
		}
		rval = OK;
		break;
	default:
		rval = EINVAL;
		break;
	}
	SEMAPHORE_RESET(lock);
	return(rval);
}
