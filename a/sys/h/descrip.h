/*	descrip.h	5.3	82/07/21	*/

#define	DNSTD	5		/* number of standard descriptors */

/* offsets of standard descriptors from dstd() */
#define	DOFF_KERNEL	0	/* descriptor of kernel */
#define	DOFF_IPC	1	/* UNIX ipc domain */
#define	DOFF_ROOT	2	/* root directory */
#define	DOFF_DOT	3	/* current directory */
#define	DOFF_TERMINAL	4	/* terminal, used in /dev/tty */

/* types of descriptors */
#define	DTYPE_KERNEL	1	/* handle to UNIX kernel */
#define	DTYPE_FILESYS	2	/* handle to file system */
#define	DTYPE_FILE	3	/* file */
#define	DTYPE_DIR	4	/* directory */
#define	DTYPE_BDEV	5	/* structured device */
#define	DTYPE_CDEV	6	/* unstructured device */
#define	DTYPE_PROCESS	7	/* process control handle */
#define	DTYPE_SOCKET	8	/* communications endpoint */
#define	DTYPE_DOMAIN	9	/* communications domain */
#define	DTYPE_TERMINAL	10	/* terminal */

/* descriptor type structure for dtype and dwrap */
struct dtype {
	int	dt_type;	/* object type */
	int	dt_protocol;	/* protocol implementing type */
};

/* flags for how in dnblock(d, how) */
#define	DNBLOCK_IN		0x1	/* input */
#define	DNBLOCK_OUT		0x2	/* output or in-progress operation */
#define	DNBLOCK_INOUT		0x3	/* input and output */
#define	DNBLOCK_EXCEPT		0x4	/* exceptional condition */
#define	DNBLOCK_SAME		0x8	/* leave as is */
