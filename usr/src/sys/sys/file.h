/*	file.h	4.11	82/07/24	*/

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

#ifdef	KERNEL
struct	file *file, *fileNFILE;
int	nfile;
struct	file *getf();
struct	file *falloc();
#endif

/* flags */
#define	FREAD		0x1		/* descriptor read/receive'able */
#define	FWRITE		0x2		/* descriptor write/send'able */
/* note: other flags for f_flag defined in fcntl.h */
