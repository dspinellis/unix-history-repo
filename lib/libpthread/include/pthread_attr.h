/* ==== pthread_attr.h ========================================================
 * Copyright (c) 1993 by Chris Provenzano, proven@athena.mit.edu
 *
 * Description : Basic pthread attributes header.
 *
 *  1.00 93/11/03 proven
 *      -Started coding this file.
 */

#include <pthread/copyright.h>

#define _POSIX_THREAD_ATTR_STACKSIZE

#define	PTHREAD_STACK_DEFAULT	65536

/*
 * New pthread attribute types.
 */
enum pthread_sched_attr {
	SCHED_RR,
	SCHED_IO,
	SCHED_FIFO,
	SCHED_OTHER,
};

typedef struct pthread_attr {
	enum pthread_sched_attr	sched_attr;
	void *					stackaddr_attr;
	size_t					stacksize_attr;
} pthread_attr_t;

/*
 * New functions
 */

__BEGIN_DECLS

int	pthread_attr_init			__P((pthread_attr_t *));
int	pthread_attr_destroy		__P((pthread_attr_t *));
int	pthread_attr_setstacksize	__P((pthread_attr_t *, size_t));
int	pthread_attr_getstacksize	__P((pthread_attr_t *, size_t *));
int	pthread_attr_setstackaddr	__P((pthread_attr_t *, void *));
int	pthread_attr_getstackaddr	__P((pthread_attr_t *, void **));
		
__END_DECLS
