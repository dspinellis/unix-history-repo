/* ==== file.c ============================================================
 * Copyright (c) 1993 by Chris Provenzano, proven@athena.mit.edu
 *
 * Description : All the new stdio functions.
 *
 *  1.00 93/09/04 proven
 *      -Started coding this file.
 */

#include <pthread/copyright.h>
#include "pthread.h"
#include <stdio.h> /* Remove this when the stdio library is done. */

/* ==========================================================================
 * flockfile()
 */
void flockfile(FILE *fp)
{
	semaphore *lock;
	int fd;

	fd = fileno(fp);
	lock = &(fd_table[fd]->lock);
	while (SEMAPHORE_TEST_AND_SET(lock)) {
		pthread_yield();
	}

	if (fd_table[fd]->r_owner != pthread_run) {
		fd_basic_lock(fd, FD_RDWR, lock);
	}
	fd_table[fd]->lockcount++;
	SEMAPHORE_RESET(lock);
}

/* ==========================================================================
 * funlockfile()
 */
void funlockfile(FILE *fp)
{
	semaphore *lock;
	int fd;

	fd = fileno(fp);
	lock = &(fd_table[fd]->lock);
	while (SEMAPHORE_TEST_AND_SET(lock)) {
		pthread_yield();
	}

	if (fd_table[fd]->r_owner == pthread_run) {
		if (--fd_table[fd]->lockcount == 0) {
			fd_basic_unlock(fd, FD_RDWR);
		}
	} 
	SEMAPHORE_RESET(lock);
}

