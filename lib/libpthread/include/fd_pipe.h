/* ==== fd_pipe.h ============================================================
 * Copyright (c) 1993 by Chris Provenzano, proven@athena.mit.edu
 *
 * Description : The new fast ITC pipe header.
 *
 *  1.00 93/08/14 proven
 *      -Started coding this file.
 */

struct __pipe {
	semaphore			lock;
	char 			  * buf;
	int					size;
	int					flags;
	int					count;
	int					offset;
	struct pthread	  *	wait;
	char			  * wait_buf;
	size_t				wait_size;
};

#define	RD_CLOSED		0x01
#define WR_CLOSED		0x02

