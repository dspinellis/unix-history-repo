/*	file.h	4.9	81/11/08	*/

/*
 * One file structure is allocated
 * for each open/creat/pipe call.
 * Main use is to hold the read/write
 * pointer associated with each open
 * file.
 */
struct	file {
	short	f_flag;		/* read/write and type (socket or inode) */
	short	f_count;	/* reference count */
	union {
		struct f_in {
			struct inode *fi_inode;
			off_t fi_offset;
		} f_in;
		struct f_so {
			struct socket *fs_socket;
		} f_so;
	} f_un;
};
#define f_offset	f_un.f_in.fi_offset
#define	f_inode		f_un.f_in.fi_inode
#define	f_socket	f_un.f_so.fs_socket

#ifdef	KERNEL
struct	file *file, *fileNFILE;
int	nfile;

struct	file *getf();
struct	file *falloc();
#endif

/* flags */
#define	FINODE		0x0		/* descriptor of an inode (pseudo) */
#define	FREAD		0x1		/* descriptor read/receive'able */
#define	FWRITE		0x2		/* descriptor write/send'able */
#define	FSOCKET		0x4		/* descriptor of a socket */
