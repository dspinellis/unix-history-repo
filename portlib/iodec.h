# define	MAXFILES	15

struct fileps
{
	char	*buff;		/* beginning of buffer */
	char	*bptr;		/* current position */
	int	nchars;		/* number of characters internal */
	int	bsize;		/* size of buffer */
	char	eoferr;		/* end of file flag */
	char	wrflag;		/* mode flag */
	char	*pbuff;		/* bottom of peek buffer */
};

struct fileps	__filehdr[MAXFILES];

struct param
{
	int	bufsize;	/* initial buffer size */
	int	peeksize;	/* initial peek size */
};

extern struct param	__param;

int			__statbuf[MAXFILES];
