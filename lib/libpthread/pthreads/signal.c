/* ==== signal.c ============================================================
 * Copyright (c) 1993 by Chris Provenzano, proven@athena.mit.edu
 *
 * Description : Queue functions.
 *
 *  1.00 93/07/21 proven
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

/*
 * Global for user-kernel lock, and blocked signals
 */
static volatile	sigset_t sig_to_process;
static volatile	int kernel_lock = 0;
static volatile	int	sig_count = 0;

static void set_thread_timer();
void sig_prevent(void);
void sig_resume(void);

/* ==========================================================================
 * context_switch()
 *
 * This routine saves the current state of the running thread gets
 * the next thread to run and restores it's state. To allow different
 * processors to work with this routine, I allow the machdep_restore_state()
 * to either return or have it return from machdep_save_state with a value
 * other than 0, this is for implementations which use setjmp/longjmp. 
 */
void fd_kern_wait() {
	fd_kern_poll();
}

static void context_switch()
{
	struct pthread **current, *next;

	/* save state of current thread */
	if (machdep_save_state()) {
		return;
	}

	if (pthread_run = pthread_queue_deq(&pthread_current_queue)) {
		/* restore state of new current thread */
		machdep_restore_state();
		return;
	}
	/* Poll all the kernel fds */
	fd_kern_poll();

context_switch_reschedule:;
	/*
	 * Go through the reschedule list once, this is the only place
	 * that goes through the queue without using the queue routines.
	 *
	 * But first delete the current queue.
	 */
	pthread_queue_init(&pthread_current_queue);
	current = &(pthread_link_list);
	while (*current) {
		switch((*current)->state) {
		case PS_RUNNING:
			pthread_queue_enq(&pthread_current_queue, *current);
			current = &((*current)->pll);
			break;
		case PS_DEAD:
			/* Cleanup thread */
			next = (*current)->pll;
			pthread_cleanup(current);
			*current = next;
			break;
		default:
			/* Should be on a different queue. Ignore. */
			current = &((*current)->pll);
			break;
		}
	}

	/* Are there any threads at all */
	if (!pthread_link_list) {
		exit(0);
	}

	if (pthread_run = pthread_queue_deq(&pthread_current_queue)) {
        /* restore state of new current thread */
		machdep_restore_state();
        return;
    }

	/*
	 * Okay, make sure the context switch timer is off, so we don't get any
	 * SIG_VTALRM signals while waiting for a fd to unblock.
	 */
	/* machdep_unset_thread_timer();
	sigdelset(&sig_to_process, SIGVTALRM); */

	/* Well have to unlock the kernel/then relock it but that should be ok */
	fd_kern_wait();
	goto context_switch_reschedule;
}

/* ==========================================================================
 * context_switch_done()
 *
 * This routine does all the things that are necessary after a context_switch()
 * calls the machdep_restore_state(). DO NOT put this in the context_switch()
 * routine because sometimes the machdep_restore_state() doesn't return
 * to context_switch() but instead ends up in machdep_thread_start() or
 * some such routine, which will need to call this routine and
 * sig_check_and_resume().
 */
void context_switch_done()
{
	sigdelset(&sig_to_process, SIGVTALRM);
	set_thread_timer();
}

/* ==========================================================================
 * set_thread_timer()
 *
 * Assums kernel is locked.
 */
static void set_thread_timer()
{
	static int last_sched_attr = SCHED_RR;

	switch (pthread_run->attr.sched_attr) {
	case SCHED_RR:
		machdep_set_thread_timer(&(pthread_run->machdep_data));
		break;
	case SCHED_FIFO:
		if (last_sched_attr != SCHED_FIFO) {
			machdep_unset_thread_timer();
		}
		break;
	case SCHED_IO:
		if (last_sched_attr != SCHED_IO) {
			machdep_set_thread_timer(&(pthread_run->machdep_data));
		}
		break;
	default:
		machdep_set_thread_timer(&(pthread_run->machdep_data));
		break;
	} 
}

/* ==========================================================================
 * sig_handler()
 *
 * Assumes the kernel is locked. 
 */
static void sig_handler(int sig)
{
	sig_handler_top:;

	switch(sig) {
	case 0:
		break;
	case SIGVTALRM:
		if (sig_count) {
			sigset_t sigall;

			sig_count = 0;

			/* Unblock all signals */
			sigemptyset(&sigall);
			sigprocmask(SIG_SETMASK, &sigall, NULL); 
		}
		context_switch();
		context_switch_done();
		break;
	case SIGALRM:
	/*	if (sleep_wakeup()) {
			break;
		} */
		/* Do the defaul action no threads were sleeping */
	default:
		PANIC();
	}

	/* Determine if there are any other signals */
	if (sig_to_process) {
		for (sig = 1; sig <= SIGMAX; sig++) {
			if (sigismember(&sig_to_process, sig)) {
		
				/* goto sig_handler_top */
				goto sig_handler_top;
			}
		}
	}
}

/* ==========================================================================
 * sig_handler_real()
 * 
 * On a multi-processor this would need to use the test and set instruction
 * otherwise the following will work.
 */
void sig_handler_real(int sig)
{
	if (kernel_lock) {
		sigaddset(&sig_to_process, sig);
		return;
	}
	sig_prevent();
	sig_count++;
	sig_handler(sig);
	sig_resume();
}

/* ==========================================================================
 * sig_handler_fake()
 */
void sig_handler_fake(int sig)
{
	if (kernel_lock) {
		/* Currently this should be impossible */
		PANIC();
	}
	sig_prevent();
	sig_handler(sig);
	sig_resume();
}

/* ==========================================================================
 * reschedule()
 *
 * This routine assumes that the caller is the current pthread, pthread_run
 * and that it has a lock on itself and that it wants to reschedule itself.
 */
void reschedule(enum pthread_state state)
{
	semaphore *plock;

	if (kernel_lock) {
		/* Currently this should be impossible */
		PANIC();
	}
	sig_prevent();
	pthread_run->state = state;
	SEMAPHORE_RESET((plock = &(pthread_run->lock)));
	sig_handler(SIGVTALRM);
	sig_resume();
}

/* ==========================================================================
 * sig_prevent()
 */
void sig_prevent(void)
{
	kernel_lock++;
}

/* ==========================================================================
 * sig_resume()
 */
void sig_resume()
{
	kernel_lock--;
}

/* ==========================================================================
 * sig_check_and_resume()
 */
void sig_check_and_resume()
{
	/* Some routine name that is yet to be determined. */
	
	/* Only bother if we are truely unlocking the kernel */
	while (!(--kernel_lock)) {

		/* Assume sigset_t is not a struct or union */
		if (sig_to_process) {
			kernel_lock++;
			sig_handler(0);
		} else {
			break;
		}
	}
}

/* ==========================================================================
 * sig_init()
 *
 * SIGVTALRM	(NOT POSIX) needed for thread timeslice timeouts.
 *				Since it's not POSIX I will replace it with a 
 *				virtual timer for threads.
 * SIGALRM		(IS POSIX) so some special handling will be
 * 				necessary to fake SIGALRM signals
 */
void sig_init(void)
{
	int sig_to_init[] = { SIGVTALRM, SIGALRM, 0 };
	int i;

	/* Initialize only the necessary signals */

	for (i = 0; sig_to_init[i]; i++) {
		if (signal(sig_to_init[i], sig_handler_real)) {
			PANIC();
		}
	}
}

