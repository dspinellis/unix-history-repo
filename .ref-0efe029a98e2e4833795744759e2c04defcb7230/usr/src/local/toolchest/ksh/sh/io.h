/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)io.h	1.1 */

/*
 *	UNIX shell
 *	S. R. Bourne
 *	rewritten by David Korn
 *
 */

#ifndef _NFILE		/* This is true for BSD 4.3 */
#define _NFILE	20
#define _N_STATIC_IOBS	3
#endif	/* _NFILE */

/* used for input and output of shell */
#define FCIO	19	/* history file */
#define INIO	18	/* saved standard ioput */
#define TMPIO	17	/* used for command substitution */
#define CINPIPE 16	/* inpipe for cooperating process */
#define COTPIPE 15	/* outpipe for cooperating process */
#define MAXFILES 10	/* maximum number of saved open files */

/*io nodes*/
#define USERIO	10
#define IOUFD	15
#define IODOC	16
#define IOPUT	32
#define IOAPP	64
#define IOMOV	128
#define IORDW	256
#define IOSTRIP 512
#define INPIPE	0
#define OTPIPE	1
#define DUPFLG	0100

#define TMPSIZ		20
#define F_STRING	((unsigned char)_NFILE)	/* file number for incore files */
#define F_INFINITE	0x7fff			/* effectively infinite */

/* temp files and io */
/* SHELL file structure */
struct fileblk
{
	FILE		*fdes;
	unsigned	flin;
	char		**feval;
	SHFILE		fstak;
};

#ifdef _N_STATIC_IOBS
extern FILE	*file_fd();
extern FILE	_myiob[];
#else
#define	file_fd(n)	(&_iob[n])
#endif /* _N_STATIC_IOBS */
#define input	(standin->fdes)
#define fisopen(fd)	((fd)->_flag)
#define fiswrite(fd)	((fd)->_flag&(_IOWRT|_IORW))
#define fisread(fd)	((fd)->_flag&(_IOREAD|_IORW))
#define fnobuff(fd)	(((fd)->_flag&_IONBF)||(fd)->_base==NULL)
#define nextchar(fd)	(*((fd)->_ptr))
#ifndef clearerr
#define clearerr(fd)	((fd)->_flag &= ~(_IOERR|_IOEOF))
#endif

struct ionod
{
	int	iofile;
	char	*ioname;
	char	*iolink;
	IOPTR	ionxt;
	IOPTR	iolst;
};

#define	IOTYPE	(sizeof(struct ionod))

struct filesave
{
	short	org_fd;
	short	dup_fd;
};


extern void	rmtemp();
extern FILE	*frenumber();
extern unsigned char _sibuf[];
extern unsigned char _sobuf[];
extern FILEBLK	stdfile;
extern char	tmpout[];

/* the following are readonly */
extern MSG	badcreate;
extern MSG	badfile;
extern MSG	badopen;
extern MSG	devnull;
extern MSG	endoffile;
extern MSG	nomorefiles;
extern MSG	piperr;
extern MSG	profile;
#ifdef BSD_4_2
extern MSG	prohibited;
#endif /* BSD_4_2 */
extern MSG	sysprofile;
extern MSG	unknown;
#ifdef SUID_EXEC
extern MSG	devfdNN;
extern MSG	suid_exec;
#endif /* SUID_EXEC */

#ifdef apollo
/* only a fool would have changed these names */
#define	_IOREAD		_SIRD
#define _IOWRT		_SIWR
#define	_IOLBF		_SINLB
#define	_IONBF		_SIUNB
#define	_IOMYBUF	_SIBIG
#define _IORW		_SIUP
#define _IOEOF		_SIEOF
#define	_IOERR		_SIERR
#define	_flsbuf		_flshbuf
#define	_file		_fd
#endif	/* apollo */
