/*	saio.h	4.7	%G%	*/

/*
 * header file for standalone package
 */

/*
 * io block: includes an
 * inode, cells for the use of seek, etc,
 * and a buffer.
 */
struct	iob {
	int	i_flgs;
	struct	inode i_ino;
	int	i_unit;
	daddr_t	i_boff;
	daddr_t	i_cyloff;
	off_t	i_offset;
	daddr_t	i_bn;
	char	*i_ma;
	int	i_cc;
	int	i_error;
	int	i_errcnt;
	int	i_active;
	char	i_buf[MAXBSIZE];
	union {
		struct fs ui_fs;
		char dummy[SBSIZE];
	} i_un;
};
#define i_fs i_un.ui_fs

#define F_READ		0x1	/* file opened for reading */
#define F_WRITE		0x2	/* file opened for writing */
#define F_ALLOC		0x4	/* buffer allocated */
#define F_FILE		0x8	/* file instead of device */
/* io types */
#define	F_RDDATA	0x0100	/* read data */
#define	F_WRDATA	0x0200	/* write data */
#define F_HDR		0x0400	/* include header on next i/o */
#define F_CHECK		0x0800	/* perform check of data read/write */
#define F_HCHECK	0x1000	/* perform check of header and data */

#define	F_TYPEMASK	0xff00

/*
 * dev switch
 */
struct devsw {
	char	*dv_name;
	int	(*dv_strategy)();
	int	(*dv_open)();
	int	(*dv_close)();
	int	(*dv_ioctl)();
};

struct devsw devsw[];

/*
 * request codes. Must be the same a F_XXX above
 */
#define	READ	1
#define	WRITE	2

#define	NBUFS	4

char	b[NBUFS][MAXBSIZE];
daddr_t	blknos[NBUFS];

#define NFILES	4
struct	iob iob[NFILES];

#define	PHYSUBA0	0x20006000
#define	PHYSMBA0	0x20010000
#define	PHYSMBA1	0x20012000
#define	PHYSUMEM	0x2013e000

extern	int errno;	/* just like unix */

/* error codes */
#define	EBADF	1	/* bad file descriptor */
#define	EOFFSET	2	/* relative seek not supported */
#define	EDEV	3	/* improper device specification on open */
#define	ENXIO	4	/* unknown device specified */
#define	EUNIT	5	/* improper unit specification */
#define	ESRCH	6	/* directory search for file failed */
#define	EIO	7	/* generic error */
#define ECMD	10	/* undefined driver command */
#define EBSE	11	/* bad sector error */
#define EWCK	12	/* write check error */
#define EHER	13	/* hard error */
#define EECC	14	/* severe ecc error, sector recorded as bad*/

/* ioctl's -- for disks just now */
#define	SAIOHDR	(('d'<<8)|1)	/* next i/o includes header */
#define	SAIOCHECK	(('d'<<8)|2)	/* next i/o checks data */
#define	SAIOHCHECK	(('d'<<8)|3)	/* next i/o checks header & data */
