/*
 * open.c  -  f77 file open routines
 */

#include	<sys/types.h>
#include	<sys/stat.h>
#include	<errno.h>
#include	"fio.h"

#define SCRATCH	(st=='s')
#define NEW	(st=='n')
#define OLD	(st=='o')
#define OPEN	(b->ufd)
#define FROM_OPEN	"\1"	/* for use in f_clos() */

extern char *tmplate;
extern char *fortfile;

f_open(a) olist *a;
{	unit *b;
	int n,exists;
	char buf[256],st;
	cllist x;

	lfname = NULL;
	elist = NO;
	external = YES;			/* for err */
	errflag = a->oerr;
	lunit = a->ounit;
	if(not_legal(lunit)) err(errflag,101,"open")
	b= &units[lunit];
	if(a->osta) st = lcase(*a->osta);
	else st = 'u';
	if(SCRATCH)
	{	strcpy(buf,tmplate);
		mktemp(buf);
	}
	else if(a->ofnm) g_char(a->ofnm,a->ofnmlen,buf);
	else sprintf(buf,fortfile,lunit);
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
	exists = (access(buf,0)==NULL);
	if(!exists && OLD) err(errflag,118,"open");
	if( exists && NEW) err(errflag,117,"open");
	if(isdev(buf))
	{	if((b->ufd = fopen(buf,"r")) != NULL) b->uwrt = NO;
		else	err(errflag,errno,buf)
	}
	else
	{	if((b->ufd = fopen(buf, "a")) != NULL) b->uwrt = YES;
		else if((b->ufd = fopen(buf, "r")) != NULL)
		{	fseek(b->ufd, 0L, 2);
			b->uwrt = NO;
		}
		else	err(errflag, errno, buf)
	}
	if((b->uinode=finode(b->ufd))==-1) err(errflag,108,"open")
	b->ufnm = (char *) calloc(strlen(buf)+1,sizeof(char));
	if(b->ufnm==NULL) err(errflag,113,"open")
	strcpy(b->ufnm,buf);
	b->uscrtch = SCRATCH;
	b->uend = NO;
	b->useek = canseek(b->ufd);
	b->url = a->orl;
	b->ublnk = (a->oblnk && (lcase(*a->oblnk)=='z'));
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
			err(errflag,121,"open form=")
		}
	}
	else	/* not specified */
	{	b->ufmt = (b->url==0);
		b->uprnt = NO;
	}
	if(b->url && b->useek) rewind(b->ufd);
	return(OK);
}

fk_open(rd,seq,fmt,n) ftnint n;
{	char nbuf[10];
	olist a;
	sprintf(nbuf,fortfile,n);
	a.oerr=1;
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

isdev(s) char *s;
{	struct stat x;
	int j;
	if(stat(s, &x) == -1) return(NO);
	if((j = (x.st_mode&S_IFMT)) == S_IFREG || j == S_IFDIR) return(NO);
	else	return(YES);
}

