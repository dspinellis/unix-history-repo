/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)file.h	7.8 (Berkeley) %G%
 */

#include <sys/fcntl.h>
#include <sys/unistd.h>

#ifdef KERNEL
/*
 * Kernel descriptor table entry;
 * one for each open kernel vnode and socket.
 */
struct file {
	int	f_flag;		/* see below */
#define	DTYPE_VNODE	1	/* file */
#define	DTYPE_SOCKET	2	/* communications endpoint */
	short	f_type;		/* descriptor type */
	short	f_count;	/* reference count */
	short	f_msgcount;	/* references from message queue */
	struct	ucred *f_cred;	/* credentials associated with descriptor */
	struct	fileops {
		int	(*fo_read)__P((
				struct file *fp,
				struct uio *uio,
				struct ucred *cred));
		int	(*fo_write)__P((
				struct file *fp,
				struct uio *uio,
				struct ucred *cred));
		int	(*fo_ioctl)__P((
				struct file *fp,
				int com,
				caddr_t data,
				struct proc *p));
		int	(*fo_select)__P((
				struct file *fp,
				int which,
				struct proc *p));
		int	(*fo_close)__P((
				struct file *fp,
				struct proc *p));
	} *f_ops;
	caddr_t	f_data;		/* inode */
	off_t	f_offset;
};

struct file *file, *fileNFILE;
int nfile;

#endif /* KERNEL */
