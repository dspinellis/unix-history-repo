/* ==== engine.h ============================================================
 * Copyright (c) 1993 Chris Provenzano, proven@athena.mit.edu
 *
 */

#include <pthread/copyright.h>
#include <unistd.h>
#include <setjmp.h>
#include <sys/time.h>

#if defined(PTHREAD_KERNEL)
#include "machdep.h"
#endif

/*
 * New types
 */
typedef long    semaphore;

#define SIGMAX	31

/*
 * New Strutures
 */
struct machdep_pthread {
    void        		*(*start_routine)(void *);
    void        		*start_argument;
    void        		*machdep_stack;
	struct itimerval	machdep_timer;
    jmp_buf     		machdep_state;
};

/*
 * Static machdep_pthread initialization values.
 * For initial thread only.
 */
#define MACHDEP_PTHREAD_INIT    \
{ NULL, NULL, NULL, { { 0, 0 }, { 0, 0 } }, 0 }

/*
 * Some fd flag defines that are necessary to distinguish between posix
 * behavior and bsd4.3 behavior.
 */
#define __FD_NONBLOCK 		O_NONBLOCK

/*
 * New functions
 */

__BEGIN_DECLS

#if defined(PTHREAD_KERNEL)

int semaphore_text_and_set  __P((semaphore *));
int machdep_save_state      __P((void));

#endif

__END_DECLS
