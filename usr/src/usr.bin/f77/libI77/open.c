/*
char id_open[] = "@(#)open.c	1.3";
 *
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
	if(not_legal(lunit)) err(errflag,F_ERUNIT,"open")
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
	if(!exists && OLD) err(errflag,F_EROLDF,"open");
	if( exists && NEW) err(errflag,F_ERNEWF,"open");
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
	if((b->uinode=finode(b->ufd))==-1) err(errflag,F_ERSTAT,"open")
	b->ufnm = (char *) calloc(strlen(buf)+1,sizeof(char));
	if(b->ufnm==NULL) err(errflag,F_ERSPACE,"open")
	strcpy(b->ufnm,buf);
	b->uscrtch = SCRATCH;
	b->uend = NO;
	b->useek = canseek(b->ufd);
	if (a->oacc && lcase(*a->oacc)=='s' && a->orl > 0)
		fputs("Warning: open: record length ignored on sequential access\n", units[0].ufd);
	else if (a->orl < 0 || (a->oacc && lcase(*a->oacc)=='d' && a->orl == 0))
		err(errflag,F_ERARG,"recl on open")
	else
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
			err(errflag,F_ERARG,"open form=")
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

isdev(s) char *s;
{	struct stat x;
	int j;
	if(stat(s, &x) == -1) return(NO);
	if((j = (x.st_mode&S_IFMT)) == S_IFREG || j == S_IFDIR) return(NO);
	else	return(YES);
}

