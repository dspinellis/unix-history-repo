/*	file.h	4.3	81/02/19	*/

/*
 * One file structure is allocated
 * for each open/creat/pipe call.
 * Main use is to hold the read/write
 * pointer associated with each open
 * file.
 */
struct	file
{
	short	f_flag;
	short	f_count;		/* reference count */
	struct inode *f_inode;		/* pointer to inode structure */
	union {
		off_t	f_offset;	/* read/write character pointer */
		struct chan *f_chan;	/* mpx channel pointer */
#ifdef CHAOS
		struct connection *f_conn;
#endif
	} f_un;
};

#ifdef	KERNEL
extern	struct file file[];		/* the file table itself */

struct	file *getf();
struct	file *falloc();
#endif

/* flags */
#define	FREAD	01
#define	FWRITE	02
#define	FPIPE	04
#define	FMPX	010
#define	FMPY	020
#define	FMP	030
