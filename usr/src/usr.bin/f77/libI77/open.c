/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)open.c	5.3	%G%
 */

/*
 * open.c  -  f77 file open and I/O library initialization routines
 */

#include	<sys/types.h>
#include	<sys/stat.h>
#include	<errno.h>
#include	"fio.h"

#define SCRATCH	(st=='s')
#define NEW	(st=='n')
#define OLD	(st=='o')
#define OPEN	(b->ufd)
#define FROM_OPEN	"\2"	/* for use in f_clos() */
#define BUF_LEN 256

LOCAL char *tmplate = "tmp.FXXXXXX";	/* scratch file template */
LOCAL char *fortfile = "fort.%d";	/* default file template */

char *getenv();

f_open(a) olist *a;
{	unit *b;
	struct stat sbuf;
	int n,exists;
	char buf[BUF_LEN], env_name[BUF_LEN];
	char *env_val, *p1, *p2, ch, st;
	cllist x;

	lfname = NULL;
	elist = NO;
	external = YES;			/* for err */
	errflag = a->oerr;
	lunit = a->ounit;
	if(not_legal(lunit)) err(errflag,F_ERUNIT,"open")
	b= &units[lunit];
	if(a->osta) st = lcase(*a->osta);
	else st = 'u';
	if(SCRATCH)
	{	strcpy(buf,tmplate);
		/* make a new temp file name, err if mktemp fails */
		if( strcmp( mktemp(buf), "/" ) == 0 )
			err(errflag, F_ERSYS, "open")
	}
	else 
	{
		if(a->ofnm) g_char(a->ofnm,a->ofnmlen,buf);
		else sprintf(buf,fortfile,lunit);
		/*   check if overriding file name via environment variable
		 *   first copy tail of name - delete periods as Bourne Shell
		 *      croaks if any periods in name
		 */
		 p1 = buf;
		 p2 = env_name;
		 while ((ch = *p1++) != '\0') {
			if(ch == '/') p2 = env_name;
			else if(ch != '.') *p2++ = ch;
		 }
		 if(p2 != env_name) {
		    *p2 = '\0';
		    if( (env_val = getenv( env_name  )) != NULL ) {
			if(strlen(env_val) >= BUF_LEN-1 )
			    err(errflag,F_ERSTAT,"open: file name too long");
			strcpy(buf, env_val);
		    }
		 }
	}
	lfname = &buf[0];
	if(OPEN)
	{
		if(!a->ofnm || inode(buf)==b->uinode)
		{
			if(a->oblnk) b->ublnk= (lcase(*a->oblnk)== 'z');
#ifndef KOSHER
			if(a->ofm && b->ufmt) b->uprnt = (lcase(*a->ofm)== 'p');
#endif
			return(OK);
		}
		x.cunit=lunit;
		x.csta=FROM_OPEN;
		x.cerr=errflag;
		if(n=f_clos(&x)) return(n);
	}
	exists = (stat(buf,&sbuf)==NULL);
	if(!exists && OLD) err(errflag,F_EROLDF,"open");
	if( exists && NEW) err(errflag,F_ERNEWF,"open");
	errno = F_ERSYS;
	if(isdev(buf))
	{	if((b->ufd = fopen(buf,"r")) != NULL) b->uwrt = NO;
		else	err(errflag,errno,buf)
	}
	else
	{
		errno = F_ERSYS;
		if((b->ufd = fopen(buf, "a")) != NULL)
		{	if(!opneof)
			{	if(freopen(buf, "r", b->ufd) != NULL)
					b->uwrt = NO;
				else
					err(errflag, errno, buf)
			}
			else
				b->uwrt = YES;
		}
		else if((b->ufd = fopen(buf, "r")) != NULL)
		{	if (opneof)
				fseek(b->ufd, 0L, 2);
			b->uwrt = NO;
		}
		else	err(errflag, errno, buf)
	}
	if((b->uinode=finode(b->ufd))==-1) err(errflag,F_ERSTAT,"open")
	b->ufnm = (char *) calloc(strlen(buf)+1,sizeof(char));
	if(b->ufnm==NULL) err(errflag,F_ERSPACE,"open")
	strcpy(b->ufnm,buf);
	b->uscrtch = SCRATCH;
	b->uend = NO;
	b->useek = canseek(b->ufd);
	if (a->oacc == NULL)
		a->oacc = "seq";
	if (lcase(*a->oacc)=='s' && a->orl > 0)
	{
		fputs("Warning: open: record length ignored on sequential access\n", units[0].ufd);
		b->url = 0;
	}
	else if (a->orl < 0 || (lcase(*a->oacc)=='d' && a->orl == 0))
		err(errflag,F_ERARG,"recl on open")
	else
		b->url = a->orl;
	if (a->oblnk)
		b->ublnk = (lcase(*a->oblnk)=='z');
	else if (lunit == STDERR)
		b->ublnk = NO;
	else
		b->ublnk = blzero;
	if (a->ofm)
	{
		switch(lcase(*a->ofm))
		{
		case 'f':
			b->ufmt = YES;
			b->uprnt = NO;
			break;
#ifndef KOSHER
		case 'p':	/* print file *** NOT STANDARD FORTRAN ***/
			b->ufmt = YES;
			b->uprnt = YES;
			break;
#endif
		case 'u':
			b->ufmt = NO;
			b->uprnt = NO;
			break;
		default:
			err(errflag,F_ERARG,"open form=")
		}
	}
	else	/* not specified */
	{	b->ufmt = (b->url==0);
		if (lunit == STDERR)
			b->uprnt = NO;
		else
			b->uprnt = ccntrl;
	}
	if(b->url && b->useek) rewind(b->ufd);
	return(OK);
}

fk_open(rd,seq,fmt,n) ftnint n;
{	char nbuf[10];
	olist a;
	sprintf(nbuf, fortfile, (int)n);
	a.oerr=errflag;
	a.ounit=n;
	a.ofnm=nbuf;
	a.ofnmlen=strlen(nbuf);
	a.osta=NULL;
	a.oacc= seq==SEQ?"s":"d";
	a.ofm = fmt==FMT?"f":"u";
	a.orl = seq==DIR?1:0;
	a.oblnk=NULL;
	return(f_open(&a));
}

LOCAL
isdev(s) char *s;
{	struct stat x;
	int j;
	if(stat(s, &x) == -1) return(NO);
	if((j = (x.st_mode&S_IFMT)) == S_IFREG || j == S_IFDIR) return(NO);
	else	return(YES);
}

/*initialization routine*/
f_init()
{
	ini_std(STDERR, stderr, WRITE);
	ini_std(STDIN, stdin, READ);
	ini_std(STDOUT, stdout, WRITE);
	setlinebuf(stderr);
}

LOCAL
ini_std(u,F,w) FILE *F;
{	unit *p;
	p = &units[u];
	p->ufd = F;
	p->ufnm = NULL;
	p->useek = canseek(F);
	p->ufmt = YES;
	p->uwrt = (w==WRITE)? YES : NO;
	p->uscrtch = p->uend = NO;
	p->ublnk = blzero;
	p->uprnt = ccntrl;
	p->url = 0;
	p->uinode = finode(F);
}

LOCAL
canseek(f) FILE *f; /*SYSDEP*/
{	struct stat x;
	return( (fstat(fileno(f),&x)==0) &&
	(x.st_nlink > 0 /*!pipe*/) && !isatty(fileno(f)) );
}

LOCAL
finode(f) FILE *f;
{	struct stat x;
	if(fstat(fileno(f),&x)==0) return(x.st_ino);
	else return(-1);
}

inode(a) char *a;
{	struct stat x;
	if(stat(a,&x)==0) return(x.st_ino);
	else return(-1);
}
