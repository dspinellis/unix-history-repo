/* ==== globals.c ============================================================
 * Copyright (c) 1993 by Chris Provenzano, proven@athena.mit.edu
 *
 * Description : Global variables.
 *
 *  1.00 93/07/26 proven
 *      -Started coding this file.
 */

#include <pthread/copyright.h>
#include "pthread.h"

/*
 * Initial thread, running thread, and top of link list
 * of all threads.
 */
struct pthread *pthread_run;
struct pthread *pthread_initial;
struct pthread *pthread_link_list;

/*
 * default thread attributes
 */
pthread_attr_t pthread_default_attr = { SCHED_RR, NULL, PTHREAD_STACK_DEFAULT };

/*
 * Queue for all threads elidgeable to run this scheduling round.
 */
struct pthread_queue pthread_current_queue = PTHREAD_QUEUE_INITIALIZER;

/*
 * File table information
 */
struct fd_table_entry *fd_table[64];


