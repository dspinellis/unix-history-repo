/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)file.h	7.5 (Berkeley) %G%
 */

#ifdef KERNEL
#include "fcntl.h"
#include "unistd.h"

/*
 * Descriptor table entry.
 * One for each kernel object.
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
		int	(*fo_read)();
		int	(*fo_write)();
		int	(*fo_ioctl)();
		int	(*fo_select)();
		int	(*fo_close)();
	} *f_ops;
	caddr_t	f_data;		/* inode */
	off_t	f_offset;
};

struct file *file, *fileNFILE;
int nfile;

/* convert O_RDONLY/O_WRONLY/O_RDWR to FREAD/FWRITE */
#define	FOPEN		(-1)
#define	FREAD		1
#define	FWRITE		2

/* kernel only versions -- deprecated, should be removed */
#define	FCREAT		O_CREAT
#define	FDEFER		O_DEFER
#define	FEXCL		O_EXCL
#define	FEXLOCK		O_EXLOCK
#define	FMARK		O_MARK
#define	FSHLOCK		O_SHLOCK
#define	FTRUNC		O_TRUNC

/* bits to save after open */
#define	FMASK		(FREAD|FWRITE|O_APPEND|O_ASYNC|O_NONBLOCK)
/* bits not settable by fcntl(F_SETFL, ...) */
#define	FCNTLCANT	(FREAD|FWRITE|O_DEFER|O_EXLOCK|O_MARK|O_SHLOCK)

#else

#include <sys/fcntl.h>
#include <sys/unistd.h>

#endif

/* operation for lseek(2); renamed by POSIX 1003.1 to unistd.h */
#define	L_SET		0	/* set file offset to offset */
#define	L_INCR		1	/* set file offset to current plus offset */
#define	L_XTND		2	/* set file offset to EOF plus offset */
