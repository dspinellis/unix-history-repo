#define	BUFSIZ	512
#define RMSSIZ	256
#define	_NFILE	20
# ifndef FILE
extern	struct	_iobuf {
	char	*_ptr;
	char	*_rms;
	char	*_base;
long	int	_sectr;
short	int	_flag;
short	int	_cnt;
	char	_links;
	char	_file;
short	int	_maxoffset;
long	int	_maxsectr;
} _iob[_NFILE];
# endif

#define	_IOREAD	01
#define	_IOWRT	02
#define	_IOBIN	04
#define _IOPRT	010
#define _IOMODE 014
#define _IOTXT	00
#define	_IOMYBUF	002000
#define	_IOEOF	020
#define	_IOERR	040
#define	_IOSTRG	0100
#define	_IONBF	0200
#define	_IOTTY	0400
#define _IODIRT	01000
#define	NULL	0
#define	FILE	struct _iobuf
#define	EOF	(-1)

#define	stdin	(&_iob[0])
#define	stdout	(&_iob[1])
#define	stderr	(&_iob[2])
#define getc(p)		(--(p)->_cnt>=0? *(p)->_ptr++&0377:_filbuf(p))
#define	getchar()	getc(stdin)
#define	putchar(x)	putc(x,stdout)
#define	feof(p)		(((p)->_flag&_IOEOF)!=0)
#define	ferror(p)	(((p)->_flag&_IOERR)!=0)
#define	fileno(p)	p->_file

FILE	*fopen();
FILE	*freopen();
extern struct io$head {
	int	initzd;
	int	lobr;
	FILE	*ufiles[_NFILE];
	int	hibr;
	int	mxbr;
	int	lopg;
	int	hipg;
	int	mxpg;
	char	stinrms[RMSSIZ];
	char	storms[RMSSIZ];
	char	sterrms[RMSSIZ];
	char	stinbuf[BUFSIZ];
	char	stobuf[BUFSIZ];
	char	sterbuf[BUFSIZ];
} io_com ;

#define check(c,s);	if (c) {errno = s; return(-1);}
#define EBADF	9
#define EINVAL	22
#define EMFILE	24

extern int errno;
