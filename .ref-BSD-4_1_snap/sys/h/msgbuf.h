/*	msgbuf.h	4.2	81/02/19	*/

#define	MSG_MAGIC	0x063060
#define	MSG_BSIZE	(NBPG*CLSIZE - 2 * sizeof (long))
struct	msgbuf {
	long	msg_magic;
	long	msg_bufx;
	char	msg_bufc[MSG_BSIZE];
};
#ifdef KERNEL
struct	msgbuf msgbuf;
#endif
