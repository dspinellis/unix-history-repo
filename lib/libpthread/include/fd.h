/* ==== fd.h ============================================================
 * Copyright (c) 1993 by Chris Provenzano, proven@athena.mit.edu
 *
 * Description : Basic fd header.
 *
 *  1.00 93/08/14 proven
 *      -Started coding this file.
 *
 *	1.01 93/11/13 proven
 *		-The functions readv() and writev() added
 */

/*
 * New pthread types.
 */
enum fd_type {
	FD_NT,					/* Not tested */
	FD_NIU,					/* Known to be not in use */
	FD_HALF_DUPLEX,			/* Files, and seeking devices */
	FD_FULL_DUPLEX			/* pipes, sockets, drivers, ... */
};


#define FD_READ				0x1
#define FD_WRITE			0x2
#define FD_RDWR				(FD_READ | FD_WRITE)

struct fd_ops {
	int 					(*write)();
	int 					(*read)();
	int						(*close)();
	int						(*fcntl)();

	int						(*writev)();
	int						(*readv)();
};

union fd_data {
	void 					*ptr;
	int						i;
};

struct fd_table_entry {
	struct pthread_queue	r_queue;
	struct pthread_queue	w_queue;
	struct pthread			*r_owner;
	struct pthread			*w_owner;
	semaphore				lock;
	struct fd_table_entry	*next;
	struct fd_ops			*ops;
	enum fd_type			type;
	int						lockcount;		/* Count for FILE locks */
	int						count;

	/* data that needs to be passed to the type dependent fd */
	int						flags;
	union fd_data			fd;
};

/*
 * Important data structure
 */
extern struct fd_table_entry *fd_table[];
extern int dtablesize;

/*
 * New functions
 */

__BEGIN_DECLS

#if defined(PTHREAD_KERNEL)

#endif

__END_DECLS
