/* ==== machdep.h ============================================================
 * Copyright (c) 1993 Chris Provenzano, proven@athena.mit.edu
 *
 */

#include <pthread/copyright.h>

/*
 * The first machine dependent functions are the SEMAPHORES
 * needing the test and set instruction.
 */
#define SEMAPHORE_CLEAR 0
#define SEMAPHORE_SET   1

#define SEMAPHORE_TEST_AND_SET(lock)    \
({										\
volatile long temp = SEMAPHORE_SET;     \
										\
__asm__("xchgl %0,(%2)"                 \
        :"=r" (temp)                    \
        :"0" (temp),"r" (lock));        \
temp;                                   \
})

#define SEMAPHORE_RESET(lock)           *lock = SEMAPHORE_CLEAR

/*
 * Minimum stack size
 */
#define PTHREAD_STACK_MIN	1024

