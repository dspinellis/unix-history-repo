/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)err.c	5.4 (Berkeley) %G%";
#endif /* not lint */

/*
 * fatal(): i/o error routine
 * flush_(): flush file buffer
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include "fio.h"

/*
 * global definitions
 */

unit units[MXUNIT];	/*unit table*/
flag reading;		/*1 if reading,		0 if writing*/
flag external;		/*1 if external io,	0 if internal */
flag sequential;	/*1 if sequential io,	0 if direct*/
flag formatted;		/*1 if formatted io,	0 if unformatted,
				-1 if list directed, -2 if namelist */
char *fmtbuf, *icptr, *icend, *fmtptr;
int (*doed)(),(*doned)();
int (*doend)(),(*donewrec)(),(*dorevert)(),(*dotab)();
int (*lioproc)();
int (*getn)(),(*putn)(),(*ungetn)();	/*for formatted io*/
FILE *cf;		/*current file structure*/
unit *curunit;		/*current unit structure*/
int lunit;		/*current logical unit*/
char *lfname;		/*current filename*/
int recpos;		/*place in current record*/
ftnint recnum;		/* current record number */
int reclen;		/* current record length */
int cursor,scale;
int radix;
ioflag signit,tab,cplus,cblank,elist,errflag,endflag,lquit,l_first;
flag leof;
int lcount,line_len;
struct ioiflg ioiflg_;	/* initialization flags */

/*error messages*/

extern int sys_nerr;

extern char *f_errlist[];
extern int f_nerr;


fatal(n,s) char *s;
{
	ftnint lu;
	char *strerror();

	for (lu=1; lu < MXUNIT; lu++)
		flush_(&lu);
	if(n<0)
		fprintf(stderr,"%s: [%d] end of file\n",s,n);
	else if(n>=0 && n<sys_nerr)
		fprintf(stderr,"%s: [%d] %s\n",s,n, strerror(n));
	else if(n>=F_ER && n<F_MAXERR)
		fprintf(stderr,"%s: [%d] %s\n",s,n,f_errlist[n-F_ER]);
	else
		fprintf(stderr,"%s: [%d] unknown error number\n",s,n);
	if(external)
	{
		if(!lfname) switch (lunit)
		{	case STDERR: lfname = "stderr";
					break;
			case STDIN:  lfname = "stdin";
					break;
			case STDOUT: lfname = "stdout";
					break;
			default:     lfname = "";
		}
		fprintf(stderr,"logical unit %d, named '%s'\n",lunit,lfname);
	}
	if (elist)
	{	fprintf(stderr,"lately: %s %s %s %s I/O\n",
			reading?"reading":"writing",
			sequential?"sequential":"direct",
			formatted>0?"formatted":(formatted==0?"unformatted":
				(formatted==LISTDIRECTED?"list":"namelist")),
			external?"external":"internal");
		if (formatted)
		{	if(fmtbuf) prnt_fmt(n);
			if (external)
			{	if(reading && curunit->useek)
					prnt_ext();  /* print external data */
			}
			else prnt_int();	/* print internal array */
		}
	}
	f77_abort(n);
}

LOCAL
prnt_ext()
{	int ch;
	int i=1;
	long loc;
	fprintf (stderr, "part of last data: ");
	loc = ftell(curunit->ufd);
	if(loc)
	{	if(loc==1L) rewind(curunit->ufd);
		else for(;i<12 && last_char(curunit->ufd)!='\n';i++);
		while(i--) ffputc(fgetc(curunit->ufd),stderr);
	}
	fputc('|',stderr);
	for(i=0;i<5 && (ch=fgetc(curunit->ufd))!=EOF;i++) ffputc(ch,stderr);
	fputc('\n',stderr);
}

LOCAL
prnt_int()
{	char *ep;
	fprintf (stderr,"part of last string: ");
	ep = icptr - (recpos<12?recpos:12);
	while (ep<icptr) ffputc(*ep++,stderr);
	fputc('|',stderr);
	while (ep<(icptr+5) && ep<icend) ffputc(*ep++,stderr);
	fputc('\n',stderr);
}

LOCAL
prnt_fmt(n) int n;
{	int i; char *ep;
	fprintf(stderr, "format: ");
	if(n==F_ERFMT)
	{	i = fmtptr - fmtbuf;
		ep = fmtptr - (i<25?i:25);
		if(ep != fmtbuf) fprintf(stderr, "... ");
		i = i + 5;
	}
	else
	{	ep = fmtbuf;
		i = 25;
		fmtptr = fmtbuf - 1;
	}
	while(i && *ep)
	{	ffputc((*ep==GLITCH)?'"':*ep,stderr);
		if(ep==fmtptr) fputc('|',stderr);
		ep++; i--;
	}
	if(*ep) fprintf(stderr, " ...");
	fputc('\n',stderr);
}

LOCAL
ffputc(c, f)
int	c;
FILE	*f;
{
	c &= 0177;
	if (c < ' ' || c == 0177)
	{
		fputc('^', f);
		c ^= 0100;
	}
	fputc(c, f);
}

ftnint
flush_(u) ftnint *u;
{
	FILE *F;

	if(not_legal(*u))
		return(F_ERUNIT);
	F = units[*u].ufd;
	if(F)
		return(fflush(F));
	else
		return(F_ERNOPEN);
}
