/* ==== mutex.h ============================================================
 * Copyright (c) 1993 by Chris Provenzano, proven@athena.mit.edu
 *
 * Description : mutex header.
 *
 *  1.00 93/07/20 proven
 *      -Started coding this file.
 */

#include <pthread/copyright.h>
/*
 * New mutex structures
 */
enum pthread_mutex_type {
	MUTEX_TYPE_FAST,
	MUTEX_TYPE_STATIC_FAST,
	MUTEX_TYPE_RECURSIVE,
	MUTEX_TYPE_METERED,
	MUTEX_TYPE_DEBUG, /* Debug mutexes will have lots of options */
	MUTEX_TYPE_MAX
};

typedef struct pthread_mutex {
	enum pthread_mutex_type	m_type;
	struct pthread_queue	m_queue;
	struct pthread			*m_owner;
	semaphore				m_lock;	
	void					*m_data;
	long					m_flags;
} pthread_mutex_t;

typedef struct pthread_mutex_attr {
	enum pthread_mutex_type	m_type;
	long					m_flags;
} pthread_mutexattr_t;

/*
 * Flags for mutexes.
 */
#define MUTEX_FLAGS_PRIVATE	0x01
#define MUTEX_FLAGS_INITED	0x02
#define MUTEX_FLAGS_BUSY	0x04

/*
 * Static mutex initialization values.
 */
#define PTHREAD_MUTEX_INITIALIZER	\
{ MUTEX_TYPE_STATIC_FAST, PTHREAD_QUEUE_INITIALIZER, \
	 NULL, SEMAPHORE_CLEAR, NULL, MUTEX_FLAGS_INITED }

/*
 * New functions
 */

__BEGIN_DECLS

int     pthread_mutex_init  	__P((pthread_mutex_t *, pthread_mutexattr_t *));
int     pthread_mutex_lock      __P((pthread_mutex_t *));
int     pthread_mutex_unlock    __P((pthread_mutex_t *));
int     pthread_mutex_trylock   __P((pthread_mutex_t *));
int     pthread_mutex_destroy   __P((pthread_mutex_t *));

__END_DECLS
	
