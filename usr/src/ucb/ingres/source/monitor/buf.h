#

/*
**  BUF.H -- buffer definitions
**
**	Version:
**		@(#)buf.h	7.1	2/5/81
*/

# define	BUFSIZE		256

struct buf
{
	struct buf	*nextb;
	char		buffer[BUFSIZE];
	char		*ptr;
};

# ifndef NULL
# define	NULL	0
# endif
