/*	file.h	4.16	83/06/03	*/

#ifdef KERNEL
/*
 * Descriptor table entry.
 * One for each kernel object.
 */
struct	file {
	short	f_flag;		/* see below */
	short	f_type;		/* descriptor type */
	short	f_count;	/* reference count */
	short	f_msgcount;	/* references from message queue */
	struct	fileops {
		int	(*fo_rw)();
		int	(*fo_ioctl)();
		int	(*fo_select)();
		int	(*fo_stat)();
		int	(*fo_close)();
	} *f_ops;
	caddr_t	f_data;		/* inode */
	off_t	f_offset;
};

struct	file *file, *fileNFILE;
int	nfile;
struct	file *getf();
struct	file *falloc();

/* flags */
#define	FOPEN		(-1)
#define	FREAD		00001		/* descriptor read/receive'able */
#define	FWRITE		00002		/* descriptor write/send'able */
#define	FNDELAY		00004		/* no delay */
#define	FAPPEND		00010		/* append on each write */
#define	FMARK		00020		/* mark during gc() */
#define	FDEFER		00040		/* defer for next gc pass */
#define	FASYNC		00100		/* signal pgrp when data ready */

/* bits to save after open */
#define	FMASK		00117	
#define	FCNTLCANT	(FREAD|FWRITE|FMARK|FDEFER)
#endif

/* open only modes */
#define	FCREAT		01000		/* create if nonexistant */
#define	FTRUNC		02000		/* truncate to zero length */
#define	FEXCL		04000		/* error if already created */

/*
 * User definitions.
 */

/*
 * Open call.
 */
#define	O_RDONLY	000		/* open for reading */
#define	O_WRONLY	001		/* open for writing */
#define	O_RDWR		002		/* open for read & write */
#define	O_NDELAY	004 		/* non-blocking open */
#define	O_APPEND	010		/* append on each write */
#define	O_CREAT		FCREAT		/* open with file create */
#define	O_TRUNC		FTRUNC		/* open with truncation */
#define	O_EXCL		FEXCL		/* error on create if file exists */

/*
 * Flock call.
 */
#define	LOCK_SH		1	/* shared lock */
#define	LOCK_EX		2	/* exclusive lock */
#define	LOCK_UN		4	/* unlock */

/*
 * Access call.
 */
#define	F_OK		0	/* does file exist */
#define	X_OK		1	/* is it executable by caller */
#define	W_OK		2	/* writable by caller */
#define	R_OK		4	/* readable by caller */

/*
 * Lseek call.
 */
#define	L_SET		0	/* absolute offset */
#define	L_INCR		1	/* relative to current offset */
#define	L_XTND		2	/* relative to end of file */

#ifdef KERNEL
#define	GETF(fp, fd) { \
	if ((unsigned)(fd) >= NOFILE || ((fp) = u.u_ofile[fd]) == NULL) { \
		u.u_error = EBADF; \
		return; \
	} \
}
#define	DTYPE_INODE	1	/* file */
#define	DTYPE_SOCKET	2	/* communications endpoint */
#endif
