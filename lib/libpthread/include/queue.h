/* ==== queue.h ============================================================
 * Copyright (c) 1993 by Chris Provenzano, proven@athena.mit.edu
 *
 * Description : mutex header.
 *
 *  1.00 93/07/20 proven
 *      -Started coding this file.
 */

#include <pthread/copyright.h>

/*
 * New queue structures
 */
struct pthread_queue {
    struct pthread  *q_next;
    struct pthread  *q_last;
    void            *q_data;
};

/*
 * Static queue initialization values.
 */
#define PTHREAD_QUEUE_INITIALIZER	{ NULL, NULL, NULL }

/*
 * New functions
 * Should make pthread_queue_get a macro
 */

__BEGIN_DECLS

void    pthread_queue_init  	__P((struct pthread_queue *));
void    pthread_queue_enq		__P((struct pthread_queue *, struct pthread *));
void    pthread_queue_remove 	__P((struct pthread_queue *, struct pthread *));
struct pthread *pthread_queue_get    __P((struct pthread_queue *));
struct pthread *pthread_queue_deq    __P((struct pthread_queue *));

__END_DECLS
