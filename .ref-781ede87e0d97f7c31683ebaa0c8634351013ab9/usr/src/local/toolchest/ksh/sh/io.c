/* @(#)io.c	1.1 */
/*
 * UNIX shell
 *
 * S. R. Bourne
 * Rewritten by David Korn
 * AT&T Bell Laboratories
 *
 */

#include	<errno.h>
#if BSD
#include	<sys/types.h>
#include	<sys/stat.h>
#include	<sys/ioctl.h>
# ifdef BSD_4_2
# include	<fcntl.h>
# endif	/* BSD_4_2 */
# define CAST	(char*)
#else
# ifdef VENIX
# include	<sys/types.h>
# include	<sys/stat.h>
# define CAST	(char*)
# else
# include	<fcntl.h>
# define CAST	(unsigned char*)
# endif	/* VENIX */
#endif	/* BSD */
#include	"defs.h"
#include	"flags.h"
#include	"sym.h"
#include	"io.h"
#include	"shtype.h"
#ifndef F_DUPFD
#define	F_DUPFD	0
#define NO_FCNTL 1
#endif	/* F_DUPFD */

/* This module defines the following routines */
FILE *fdopen();
FILE *create();
FILE *chkrdwr();
int	ispipe();
void	sync_io();
void	settemp();
void	swap_iodoc_nm();
void	initf();
void	restore();
int	estabf();
void	chkpipe();

/* This module references the following externals */
extern STKPTR locstak(),cpystak();
extern void chkpr();
extern void failed();
extern void free();
extern char *heap();
extern char *itos();
extern long lseek();
extern char *movstr();
extern void p_flush();
extern char *strrchr();


static int qtrim();
static int serial;
static char *temp_suffix;
static struct filesave fdmap[MAXFILES];


/* ======== input output and file copying ======== */

/*
 * initialize temp file names
 */
void settemp(pid)
char *pid;
{
	register char *sp = movstr(pid,tmpout+7);
	*sp++ = '.';
	temp_suffix = sp;
	serial = 0;
	states &= ~NO_TMP;
}

/*
 * set up a fileblk associated with the stream fd
 */

void	initf(fd)
FILE	*fd;
{
	register SHFILE f=standin;
	f->fdes=fd;
	f->feval=0;
	f->flin=1;
}

/*
 * set up an I/O stream that will cause reading from a string
 */

int	estabf(s,fd)
register FILE *fd;
register char *s;
{
	register SHFILE f;
	(f=standin)->fdes = fd;
	fd->_flag = _IOREAD;
	fd->_base = fd->_ptr = CAST s;
	fd->_file = F_STRING;
	fd->_cnt = F_INFINITE;
	f->flin = 1;
	fd->_flag|=(s==0?_IOEOF:0);
	return(feof(fd));
}

push(af)
SHFILE af;
{
	register SHFILE f;
	(f=af)->fstak=standin;
	f->feval=0;
	standin=f;
}

pop(flag)
register int flag;
{
	register SHFILE f;
	register FILE *fd;
	register int fno;
	if((f=standin)->fstak)
	{
		fd = f->fdes;
		fno = fileno(fd);
		if(flag==0 && fno>0 && fno!=F_STRING && fno!=INIO && fd!=cpipe[INPIPE])
			closefd(fd);
		standin=f->fstak;
		return(1);
	}
	return(0);
}

/*
 * sync_io - flushes output buffer and positions stdin if necessary
 */

void sync_io()
{
	register FILE *fp = stdin;
	p_flush();
	/* position back the read-ahead characters */
	if(fp->_cnt)
	{
		lseek(fileno(fp),-((long)(fp->_cnt)),1);
		setbuf(fp,(char*)fp->_base);
	}
}


/*
 * This non-standard version of fdopen makes stream numbers
 * correspond to file unit numbers
 */

FILE *fdopen(fd, mode)
register int	fd;
register char *mode;
{
	register FILE *iop;
	if(fd < 0)
		return(NULL);
	iop = file_fd(fd);
	iop->_cnt = 0;
	iop->_file = fd;
	iop->_base = NULL;
	switch(*mode)
	{

		 case 'r':
			iop->_flag |= _IOREAD;
			break;
		 case 'a':
			lseek(fd, 0L, 2);
			/* No break */
		 case 'w':
			iop->_flag |= _IOWRT;
			break;
		 default:
			return(NULL);
		}

	if(mode[1] == '+')
	{
		 iop->_flag &= ~(_IOREAD | _IOWRT);
		 iop->_flag |= _IORW;
		}
	return(iop);
}

void	chkpipe(pv)
FILE	*pv[];
{
	int ipv[2];
	if(pipe(ipv)<0 || ipv[INPIPE]<0 || ipv[OTPIPE]<0)
		error(piperr);
	pv[INPIPE] = fdopen(ipv[INPIPE],"r");
	pv[OTPIPE] = fdopen(ipv[OTPIPE],"w");
}

/*
 * close a pipe
 */

void pipe_close(pv)
register FILE *pv[];
{
	if(pv[INPIPE])
		fclose(pv[INPIPE]);
	if(pv[OTPIPE])
		fclose(pv[OTPIPE]);
}

/*
 * Open a stream for reading
 * On failure, print message.
 */

FILE *chkopen(name)
register char *name;
{
	register FILE	*fd;
	if((fd=fdopen(open(name,0),"r"))==NULL)
		failed(name,badopen);
	return(fd);
}


/*
 * given a file stream f1, move it to a new file stream with file number
 * f2.  If f2 is open then it is closed first.
 * If the MARK bit not set on f2, then close on exec will be set for f2>2
 * The original stream is closed.
 * File numbers greater than 2 are marked close on exec if frenumber is 
 *  invoked by a parent shell.
 *  The new file descriptor is returned;
 */

FILE *frenumber(f1,f2)
FILE *f1;
register int f2;
{
	register FILE *fd;
	register int flag = (f2&MARK);
	register int fs=0;
	register char *type;
	f2 &= ~MARK;
	if(f2>2 && flag==0)
		fs = 1;
	fd = file_fd(f2);
	if(fileno(f1)!=f2)
	{
		int fno;
		if(fs==0)
			fs = fcntl(f2,1,0);
		if(fisopen(fd))
		{
			closefd(fd);
		}
		else
			close(f2);
		fno = fcntl(fileno(f1),0,f2);
		if(fno < 0)
			error(badfile);
		flag = f1->_flag;
		if(flag&_IORW)
			type="w+";
		else
			type = (f1->_flag&_IOREAD?"r":"w");
		fclose(f1);
		fd = fdopen(f2,type);
#ifdef apollo
		fd->_file = fno;
#endif	/* apollo */
		fd->_flag = flag;
		if(fd==output)
			setbuf(fd,(char*)_sobuf);
		else if(fd==input && (fd->_flag&_IONBF)==0)
			setbuf(fd,(char*)_sibuf);
		else
		{
			fd->_cnt = f1->_cnt;
			fd->_ptr = f1->_ptr;
			fd->_base = f1->_base;
		}
		setbuf(f1,NIL);
		if(f2==0)
			ioset |= 1;
	}
	if(fs==1)
#ifdef BSD
		ioctl(f2, FIOCLEX, NULL);
#else
		fcntl(f2,2,1);
#endif	/* BSD */
	return(fd);
}

FILE *tmp_open(fname)
register char *fname;
{
	register int maxtry = 10;
	register char *tmp_name = tmpout;
	register FILE *fd;
	if(states&NO_TMP)
		settemp(itos(getpid()));
	do
	{
		movstr(itos(++serial),temp_suffix);
	}
	while((fd=create(tmp_name))== NULL && maxtry--);
	if(fname)
	{
		movstr(tmp_name,fname);
		if((fd = chkrdwr(tmp_name,fd))==NULL)
			failed(tmp_name,badcreate);
	}
	return(fd);
}
	 
/*
 * create the file named s and return an open stream to it
 */
 
FILE *create(s)
char *s;
{
	register FILE *fd;
	fd = fdopen(creat(s,0666),"w+");
	return(fd);
}

/*
 * close file stream and reopen for reading and writing
 */

FILE *chkrdwr(name,fd)
register char *name;
register FILE *fd;
{
	if(fd!=NULL)
	{
		fclose(fd);
		fd = fdopen(open(name,2),"w+");
	}
	return(fd);
}

closefd(fd)
register FILE *fd;
{

	/* reposition seek pointer if necessary */
	if((fd->_flag&_IOREAD) && fd->_cnt)
		lseek(fileno(fd),-((long)(fd->_cnt)),1);
	free(fd->_base);
	fclose(fd);
	setbuf(fd,NIL);
}

copy(ioparg)
IOPTR	ioparg;
{
	register char c = '\n';
	register char *clinep;
	register IOPTR iop;
	register FILE	*fd;
	BOOL 	nosubst;
	char *ends,*cline,obuff[BUFSIZ];
	if(iop=ioparg)
	{
		int stripflg = iop->iofile&IOSTRIP;
		register nlflg = stripflg;
		copy(iop->iolst);
		ends=iop->ioname;
		/* check for and strip quoted characters in ends */
		nosubst = qtrim(ends);
		if(nosubst)
			iop->iofile &= ~IODOC;
		fd = tmp_open(NIL);
		iop->ioname = (char*)cpystak(tmpout);
		setbuf(fd,obuff);
		iop->iolst=iotemp; iotemp=iop;
		cline=(char*)locstak();
		if(stripflg)
			while(*ends=='\t')
				ends++;
		clinep = cline++;
		*cline = 0;
		do
		{
			if(c=='\n')
			{
				*clinep = 0;
				if(eq(ends,cline))
					break;
				chkpr(0);
				*clinep++ = '\n';
				*clinep = 0;
				fputs(cline,fd);
				clinep = cline;
				nlflg = stripflg;
			}
			else if(c=='\t' && nlflg)
				;
			else
			{
				*clinep++ = c;
				nlflg = 0;
			}
		}
		while(c=(nosubst?readc():nextc()));
		closefd(fd);
	}
}

/*
 * trim quotes and the escapes
 * returns non-zero if string is quoted 0 otherwise
 */

static int qtrim(string)
char *string;
{
	register char *sp = string;
	register char *dp = sp;
	register int c;
	register int quote = 0;
	while(c= *sp++)
	{
		if(c == ESCAPE)
		{
			quote = 1;
			c = *sp++;
		}
		else if(c == '"')
		{
			quote = 1;
			continue;
		}
		*dp++ = c;
	}
	*dp = 0;
	return(quote);
}

/*
 * short version of fputs
 */

int fputs(s,fd)
register char *s;
register FILE *fd;
{
	register char c;
	if(s==NULL || fd==NULL)
		return(EOF);
	while(c = *s++)
		putc(c,fd);
	return(0);
}


/*
 * create a link to iodoc for child process to use
 */

link_iodocs(i)
register struct ionod	*i;
{
	while(i)
	{
		/* generate a tempory file name */
		fclose(tmp_open(NIL));
		unlink(tmpout);
		free(i->iolink);
		i->iolink = heap(tmpout);
		link(i->ioname, i->iolink);
		i = i->iolst;
	}
}


/*
 * rename the file with the link name of the parent
 */

void	swap_iodoc_nm(i)
register struct ionod	*i;
{
	while(i)
	{
		free(i->ioname);
		i->ioname = i->iolink;
		i->iolink = 0;
		i = i->iolst;
	}
}


/*
 * copy file fd into a save place
 */

savefd(fd,oldtop)
register int fd;
{
	register int	f = topfd;
	register FILE *f1 = file_fd(fd);
	/* see if already saved, only save once */
	while(f > oldtop)
	{
		if(fdmap[--f].org_fd == fd)
			return;
	}
	if(fiswrite(f1))
		fflush(f1);
	else if(f1==stdin)
		sync_io();
	f = fcntl(fd, F_DUPFD, USERIO);
	if(topfd >= MAXFILES)
		error(nomorefiles);
	if(f >= 0)
	{
		*(file_fd(f)) = *f1;
		setbuf(f1,NIL);
	}
	fdmap[topfd].org_fd = fd;
	fdmap[topfd++].dup_fd = f;
	return;
}


/*
 *  restore saved file descriptors from <last> on
 */

void	restore(last)
register int	last;
{
	register int 	i;
	register int	dupfd;

	for (i = topfd - 1; i >= last; i--)
	{
		if ((dupfd = fdmap[i].dup_fd) > 0)
		{
			(file_fd(dupfd))->_file = dupfd;
			frenumber(file_fd(dupfd), fdmap[i].org_fd);
		}
		else
			fclose(file_fd(fdmap[i].org_fd));
	}
	topfd = last;
}


/*
 * This routine returns 1 if fd corresponds to a pipe, 0 otherwise.
 */

int ispipe(fd)
FILE *fd;
{
	register int fno = fileno(fd);
	if(lseek(fno,0L,1)>=0)
		return(0);
	if(errno==ESPIPE)
		return(!isatty(fno));
#ifdef BSD
	/* This may be a bug in lseek */
	else if(errno==EINVAL)
		return(1);
#endif /* BSD */
	else
		return(0);
}


#if ESH || VSH
/*
 * Stripped down version of _filbuf from standard I/O library
 */

_filbuf(iop)
register FILE *iop;
{
	register unsigned state = states;
	unsigned char cc;
	register int syncread;

	if (iop->_flag & _IORW)
		iop->_flag |= _IOREAD;

	if ((iop->_flag&_IOREAD) == 0)
		return(EOF);
	if(fnobuff(iop))
	{
		/* unbuffered reads needed for pipes */
		p_flush();
		iop->_cnt = read(fileno(iop),(char*)(&cc),1);
		if(iop->_cnt>0)
			{
				iop->_cnt--;
				return(cc);
			}
		goto skip;
	}
	syncread = ((state&PROMPT) && iop==input && (standin->fstak==0||(state&RWAIT)));
#ifdef ESH
	if(is_option(EMACS|GMACS) && syncread)
		iop->_cnt = hread(fileno(iop), (char*)iop->_base, BUFSIZ);
	else
#endif	/* ESH */
#ifdef VSH
	if(is_option(EDITVI) && syncread)
		iop->_cnt = vread(fileno(iop), (unsigned char*)iop->_base, BUFSIZ);
	else
#endif	/* VSH */
		{
			/* flush before a read */
			if(syncread)
				p_flush();
			iop->_cnt = read(fileno(iop), (char*)iop->_base, BUFSIZ);
		}
	iop->_ptr = iop->_base;
	skip:
	if (--iop->_cnt < 0)
	{
		if (iop->_cnt == -1)
		{
			iop->_flag |= _IOEOF;
			if (iop->_flag & _IORW)
				iop->_flag &= ~_IOREAD;
		}
		else
			iop->_flag |= _IOERR;
		iop->_cnt = 0;
		return(-1);
	}
	return(*iop->_ptr++&STRIP);
}
#endif


#ifdef NO_FCNTL
static int fcntl(f1,type,arg)
register int arg;
{
	struct stat statbuf;
	if(type==F_DUPFD)
	{
		register int fd;
		/* find first non-open file */
		while(arg < _NFILE &&  (fstat(arg,&statbuf)>=0))
			arg++;
		if(arg >= _NFILE)
			return(-1);
		fd = dup(f1|DUPFLG,arg);
		return(fd);
	   }
	else 
		return(0);
}
#endif	/* NO_FCNTL */

#if u370 || uts

extern int isatty();
extern unsigned char _smbuf[][_SBFSIZ];

void setbuf(iop, buf)
register FILE *iop;
char	*buf;
{
	register int fno = fileno(iop);  /* file number */

	if(iop->_base != NULL && iop->_flag & _IOMYBUF)
		free((char*)iop->_base);
	iop->_flag &= ~(_IOMYBUF | _IONBF | _IOLBF);
	if((iop->_base = (unsigned char*)buf) == NULL)
	{
		iop->_flag |= _IONBF; /* file unbuffered except in fastio */

			_bufend(iop) = (iop->_base = _smbuf[fno]) + _SBFSIZ;
	}
	else
	{  /* regular buffered I/O, standard buffer size */
		_bufend(iop) = iop->_base + BUFSIZ;
		if (isatty(fno))
			iop->_flag |= _IOLBF;
	}
	iop->_ptr = iop->_base;
	iop->_cnt = 0;
}
#endif	/* u370 */

#ifdef INT16
/*
 * special version of fread for to save space
 * only works if count is 1
 */

fread(ptr,size,count,iop)
register char *ptr;
unsigned size,count;
register FILE *iop;
{
	register int c;
	do
	{
		if((c=getc(iop))>=0)
			*ptr++ = c;
		else
			return(0);
	}
	while(--size);
	return(1);
}
#endif	/* INT16 */

#ifdef VENIX
int getppid()
{
	return(1);
}
#endif	/* VENIX */

#ifdef _N_STATIC_IOBS
/*  ULTRIX doesn't have complete _iob */
FILE	_myiob[FCIO+1- _N_STATIC_IOBS];

FILE	*file_fd(n)
{
	if(n < _N_STATIC_IOBS)
		return(&_iob[n]);
	else
		return(&_myiob[n- _N_STATIC_IOBS]);
}
#endif /* _N_STATIC_IOBS */
