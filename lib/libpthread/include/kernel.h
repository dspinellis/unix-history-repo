/* ==== kernel.h ============================================================
 * Copyright (c) 1993 by Chris Provenzano, proven@athena.mit.edu
 *
 * Description : mutex header.
 *
 *  1.00 93/07/22 proven
 *      -Started coding this file.
 */

/*
 * Defines only for the pthread user kernel.
 */
#if defined(PTHREAD_KERNEL)

#define PANIC()		abort()

/* Time each rr thread gets */
#define PTHREAD_RR_TIMEOUT	100000000

#endif
