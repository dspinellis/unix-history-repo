/* ==== pthread_attr.c =======================================================
 * Copyright (c) 1993 by Chris Provenzano, proven@athena.mit.edu
 *
 * Description : Pthread attribute functions.
 *
 *  1.00 93/11/04 proven
 *      -Started coding this file.
 */

#include <pthread/copyright.h>
#include "pthread.h"
#include <errno.h>

/* Currently we do no locking, should we just to be safe? CAP */
/* ==========================================================================
 * pthread_attr_init()
 */
int pthread_attr_init(pthread_attr_t *attr)
{
	memcpy(attr, &pthread_default_attr, sizeof(pthread_attr_t));
	return(OK);
}

/* ==========================================================================
 * pthread_attr_destroy()
 */
int pthread_attr_destroy(pthread_attr_t *attr)
{
	return(OK);
}

/* ==========================================================================
 * pthread_attr_getstacksize()
 */
int pthread_attr_getstacksize(pthread_attr_t *attr, size_t * stacksize)
{
	*stacksize = attr->stacksize_attr;
	return(OK);
}

/* ==========================================================================
 * pthread_attr_setstacksize()
 */
int pthread_attr_setstacksize(pthread_attr_t *attr, size_t stacksize)
{
	if (stacksize >= PTHREAD_STACK_MIN) {
		attr->stacksize_attr = stacksize;
		return(OK);
	}
	return(EINVAL);
}

/* ==========================================================================
 * pthread_attr_getstackaddr()
 */
int pthread_attr_getstackaddr(pthread_attr_t *attr, void ** stackaddr)
{
	*stackaddr = attr->stackaddr_attr;
	return(OK);
}

/* ==========================================================================
 * pthread_attr_setstackaddr()
 */
int pthread_attr_setstackaddr(pthread_attr_t *attr, void * stackaddr)
{
	attr->stackaddr_attr = stackaddr;
	return(OK);
}
