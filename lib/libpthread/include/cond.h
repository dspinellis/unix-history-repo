/* ==== cond.h ============================================================
 * Copyright (c) 1993 by Chris Provenzano, proven@athena.mit.edu
 *
 * Description : mutex header.
 *
 *  1.00 93/10/30 proven
 *      -Started coding this file.
 */

/*
 * New cond structures
 */
enum pthread_cond_type {
	COND_TYPE_FAST,
	COND_TYPE_STATIC_FAST,
	COND_TYPE_METERED,
	COND_TYPE_DEBUG, /* Debug conds will have lots of options */
	COND_TYPE_MAX
};

typedef struct pthread_cond {
	enum pthread_cond_type	c_type;
	struct pthread_queue	c_queue;
	semaphore				c_lock;	
	void				  * c_data;
	long					c_flags;
} pthread_cond_t;

typedef struct pthread_cond_attr {
	enum pthread_cond_type	c_type;
	long					c_flags;
} pthread_condattr_t;

/*
 * Flags for conds.
 */
#define COND_FLAGS_PRIVATE	0x01
#define COND_FLAGS_INITED	0x02
#define COND_FLAGS_BUSY		0x04

/*
 * Static cond initialization values.
 */
#define PTHREAD_COND_INITIALIZER	\
{ COND_TYPE_STATIC_FAST, PTHREAD_QUEUE_INITIALIZER, \
	 NULL, SEMAPHORE_CLEAR, COND_FLAGS_INITED }

/*
 * New functions
 */

__BEGIN_DECLS

int     pthread_cond_init  		__P((pthread_cond_t *, pthread_condattr_t *));
int     pthread_cond_wait      	__P((pthread_cond_t *, pthread_mutex_t *));
int     pthread_cond_signal    	__P((pthread_cond_t *));
int     pthread_cond_broadcast	__P((pthread_cond_t *));
int     pthread_cond_destroy   	__P((pthread_cond_t *));

__END_DECLS
	
