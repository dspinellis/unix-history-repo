/* ==== pthread.c ============================================================
 * Copyright (c) 1993 by Chris Provenzano, proven@athena.mit.edu
 *
 * Description : Pthread functions.
 *
 *  1.00 93/07/26 proven
 *      -Started coding this file.
 */

/*
 * Copyright (c) 1993 by Chris Provenzano and contributors, proven@mit.edu
 * All rights reserved.
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
 *  This product includes software developed by Chris Provenzano,
 *	the University of California, Berkeley, and contributors.
 * 4. Neither the name of Chris Provenzano, the University, nor the names of
 *	  contributors may be used to endorse or promote products derived
 *	  from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY CHRIS PROVENZANO AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL CHRIS PROVENZANO, THE REGENTS OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include "pthread.h"
#include <signal.h>
#include <errno.h>

/*
 * These first functions really should not be called by the user.
 */

/* ==========================================================================
 * pthread_init()
 *
 * This function should be called in crt0.o before main() is called.
 * But on some systems It may not be possible to change crt0.o so currently
 * I'm requiring this function to be called first thing after main.
 * Actually I'm assuming it is, because I do no locking here.
 */
void pthread_init(void)
{
	struct machdep_pthread machdep_data = MACHDEP_PTHREAD_INIT;

	/* Initialize the signal handler. */
	sig_init();

	/* Initialize the fd table. */
	fd_init();

	/* Initialize the first thread */
	if (pthread_initial = (pthread_t)malloc(sizeof(struct pthread))) {
		bcopy(machdep_data, &(pthread_initial->machdep_data), sizeof(machdep_data));
		pthread_initial->state = PS_RUNNING;
		pthread_initial->queue = NULL;
		pthread_initial->next = NULL;
		pthread_initial->pll = NULL;

		pthread_initial->lock = SEMAPHORE_CLEAR;
		pthread_initial->error = 0;

		pthread_link_list = pthread_initial;
		pthread_run = pthread_initial;
		return;
	}
	PANIC();
}

/* ==========================================================================
 * pthread_cleanup()
 */
void pthread_cleanup(pthread_t *thread)
{
	void *stack;

	/* Check attr to see what needs cleanup. */
	if (stack = (void *)machdep_pthread_cleanup(&((*thread)->machdep_data))) {
		free(stack);
	}
	free(*thread);
}

/* ==========================================================================
 * pthread_yield()
 */
void pthread_yield()
{
	sig_handler_fake(SIGVTALRM);
}

/* ======================================================================= */
/* ==========================================================================
 * pthread_self()
 */
pthread_t pthread_self()
{
	return(pthread_run);
}

/* ==========================================================================
 * pthread_equal()
 */
int pthread_equal(pthread_t t1, pthread_t t2)
{
	return(t1 == t2);
}

/* ==========================================================================
 * pthread_exit()
 * 
 * Once this routine gets the lock it never gives it up.
 * Joining with a thread that has exited is not valid anymore, so
 * there now is no valid opperation that can be done to a thread once it
 * has done the pthread_exit().
 * It doesn't matter if a context switch occurs before yield is called
 * but after the state is set. 
 */
void pthread_exit(void *status)
{
	semaphore *lock;

	lock = &pthread_run->lock;
	if (SEMAPHORE_TEST_AND_SET(lock)) {
		pthread_yield();
	}
	pthread_run->state = PS_DEAD;
	pthread_yield();
}

/* ==========================================================================
 * pthread_create()
 *
 * After the new thread structure is allocated and set up, it is added to
 * pthread_run_next_queue, which requires a sig_prevent(),
 * sig_check_and_resume()
 */
int pthread_create(pthread_t *thread, const pthread_attr_t *attr,
  void * (*start_routine)(void *), void *arg)
{
	long nsec = 100000000;
	void *stack;

	if ((*thread) = (pthread_t)malloc(sizeof(struct pthread))) {

		if (! attr) { attr = &pthread_default_attr; }

		/* Get a stack, if necessary */
		if ((stack = attr->stackaddr_attr) ||
		  (stack = (void *)malloc(attr->stacksize_attr))) {

			machdep_pthread_create(&((*thread)->machdep_data),
			  start_routine, arg, 65536, stack, nsec);

			memcpy(&(*thread)->attr, attr, sizeof(pthread_attr_t));

			(*thread)->queue = NULL;
			(*thread)->next = NULL;

			(*thread)->lock = SEMAPHORE_CLEAR;
			(*thread)->error = 0;

			sig_prevent();

			/* Add to the link list of all threads. */
			(*thread)->pll = pthread_link_list;
			pthread_link_list = (*thread);

			(*thread)->state = PS_RUNNING;
			sig_check_and_resume();

			return(OK);
		}
		free((*thread));
	}
	return(ENOMEM);
}

/* ==========================================================================
 * pthread_cancel()
 *
 * This routine will also require a sig_prevent/sig_check_and_resume()
 */
