/*	msgbuf.h	6.1	83/07/29	*/

#define	MSG_MAGIC	0x063060
#define	MSG_BSIZE	(4096 - 2 * sizeof (long))
struct	msgbuf {
	long	msg_magic;
	long	msg_bufx;
	char	msg_bufc[MSG_BSIZE];
};
#ifdef KERNEL
struct	msgbuf msgbuf;
#endif
