/*	file.h	4.12	82/08/10	*/

#ifdef KERNEL
/*
 * Descriptor table entry.
 * One for each kernel object.
 */
struct	file {
	short	f_flag;			/* see below */
	short	f_type;		/* descriptor type */
	char	f_nbhow;	/* state from dnblock */
	char	f_sighow;	/* state from dsignal */
	short	f_count;		/* reference count */
/* begin XXX */
	struct	inode *f_inode;		/* inode */
	union {
		struct f_in { off_t fi_offset; } f_in;
		struct f_so { struct socket *fs_socket; } f_so;
	} f_un;
#define f_offset	f_un.f_in.fi_offset
#define	f_socket	f_un.f_so.fs_socket
/* end XXX */
};

struct	file *file, *fileNFILE;
int	nfile;
struct	file *getf();
struct	file *falloc();

/* flags */
#define	FREAD		0x001		/* descriptor read/receive'able */
#define	FWRITE		0x002		/* descriptor write/send'able */
#define	FAPPEND		0x004		/* append on each write */
/* the following defines the bits that users can set in f_flag */
#define	FMODES	(FREAD|FWRITE|FAPPEND)
#endif

/*
 * User visible desriptor attributes.
 * These are supplied at open or flock time.
 * FRDONLY, FWRONLY, and FRDWR are
 * converted to FREAD and FWRITE on open.
 */
#define	FRDONLY		0x000		/* open for reading only */
#define	FWRONLY		0x001		/* open for writing only */
#define	FRDWR		0x002		/* open for reading and writing */
#define	FAPPEND		0x004		/* append on each write */
#define	FRDLOCK		0x008		/* apply read lock */
#define	FWRLOCK		0x010		/* apply write lock */
#define	FUNLOCK		0x100		/* release all locks */
#define	FCREATE		0x200		/* create file if nonexistant */
#define	FTRUNCATE	0x400		/* truncate file to size 0 on open */
#define	FNBLOCK		0x800		/* don't block on open */

/* these are for 3.0 "compatibility" */
#define	O_RDONLY	FRDONLY		/* open for read */
#define	O_WRONLY	FWRONLY		/* open for writing */
#define	O_RDWR		FRDWR		/* open for read & write */
#define	O_NDELAY	FNBLOCK 	/* non-blocking I/O */
#define	O_APPEND	FAPPEND		/* append */
#define	O_CREAT		FCREATE		/* open with file create */
#define	O_TRUNC		FTRUNCATE	/* open with truncation */
#define	O_EXCL		FWRLOCK		/* exclusive open */

/* flags supplied to access call */
#define	FACCESS_EXISTS	0x0	/* does file exist */
#define	FACCESS_EXECUTE	0x1	/* is it executable by caller */
#define	FACCESS_WRITE	0x2	/* writable by caller */
#define	FACCESS_READ	0x4	/* readable by caller */

/* flags supplies to lseek call */
#define	FSEEK_ABSOLUTE	0x0	/* absolute offset */
#define	FSEEK_RELATIVE	0x1	/* relative to current offset */
#define	FSEEK_EOF	0x2	/* relative to end of file */

/* file types which may be specified to mknod */
#define	FTYPE_CDEV	0x2000	/* character special device */
#define	FTYPE_DIR	0x4000	/* directory */
#define	FTYPE_BDEV	0x8000	/* block special device */
