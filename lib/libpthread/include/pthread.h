/* ==== pthread.h ============================================================
 * Copyright (c) 1993 by Chris Provenzano, proven@athena.mit.edu
 *
 * Description : Basic pthread header.
 *
 *  1.00 93/07/20 proven
 *      -Started coding this file.
 */

#include <pthread/copyright.h>
#include <pthread/engine.h>
#include <pthread/kernel.h>
#include <pthread/queue.h>
#include <pthread/mutex.h>
#include <pthread/cond.h>
#include <pthread/fd.h>

#include <pthread/util.h>
/* #include <pthread/stdio.h> Because I'm a moron -- proven */
#include <errno.h>

/* More includes, that need size_t or NULL */
#include <pthread/pthread_attr.h>

enum pthread_state {
	PS_RUNNING,
	PS_MUTEX_WAIT,
	PS_COND_WAIT,
	PS_FDLR_WAIT,
	PS_FDLW_WAIT,
	PS_FDR_WAIT,
	PS_FDW_WAIT,
	PS_DEAD
};

struct pthread {
	struct machdep_pthread	machdep_data;
	struct pthread_queue	*queue;
	enum pthread_state		state;
	pthread_attr_t			attr;

	/*
	 * Thread implementations are just multiple queue type implemenations,
	 * Below are the various link lists currently necessary
	 * It is possible for a thread to be on multiple, or even all the
	 * queues at once, much care must be taken during queue manipulation.
	 */ 
	struct pthread			*pll;		/* ALL threads, in any state */
	/* struct pthread		*rll;		/* Current run queue, before resced */
	struct pthread			*next;		/* Standard for mutexes, etc ... */
	struct pthread			*s_next;	/* For sleeping threads */
	/* struct pthread			*fd_next;	/* For kernel fd operations */

	int						fd;			/* Used when thread waiting on fd */

	semaphore				lock;
	int						error;
};

typedef struct pthread*			pthread_t;

/*
 * Globals
 */
extern	struct pthread 			*pthread_run;
extern	struct pthread 			*pthread_initial;
extern	struct pthread 			*pthread_link_list;
extern	pthread_attr_t			pthread_default_attr;
extern	struct pthread_queue 	pthread_current_queue;
extern	struct fd_table_entry 	*fd_table[];

/*
 * New functions
 */

__BEGIN_DECLS

int			pthread_create		__P((pthread_t *, const pthread_attr_t *,
							   	  void * (*start_routine)(void *), void *));
void		pthread_exit		__P((void *));
pthread_t	pthread_self		__P((void));
int			pthread_equal		__P((pthread_t, pthread_t));
		
#if defined(PTHREAD_KERNEL)

void		pthread_yield		__P((void));

/* Not valid, but I can't spell so this will be caught at compile time */
#define		pthread_yeild(notvalid)

#endif

__END_DECLS
