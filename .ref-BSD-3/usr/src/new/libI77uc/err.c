/*
 * file i/o error and initialization routines
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include "fiodefs.h"

/*
 * global definitions
 */

char *tmplate = "tmp.FXXXXXX";	/* scratch file template */
char *fortfile = "fort.%D";	/* default file template */

unit units[MXUNIT];	/*unit table*/
flag reading;		/*1 if reading,		0 if writing*/
flag external;		/*1 if external io,	0 if internal */
flag sequential;	/*1 if sequential io,	0 if direct*/
flag formatted;		/*1 if formatted io,	0 if unformatted, -1 if list*/
char *fmtbuf, *icptr, *icend, *fmtptr;
int (*doed)(),(*doned)();
int (*doend)(),(*donewrec)(),(*dorevert)(),(*dotab)();
int (*lioproc)();
int (*getn)(),(*putn)(),(*ungetn)();	/*for formatted io*/
icilist *svic;		/* active internal io list */
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

/*error messages*/

extern char *sys_errlist[];

char *F_err[] =
{
/* 100 */	"error in format",
/* 101 */	"illegal unit number",
/* 102 */	"formatted io not allowed",
/* 103 */	"unformatted io not allowed",
/* 104 */	"direct io not allowed",
/* 105 */	"sequential io not allowed",
/* 106 */	"can't backspace file",
/* 107 */	"off beginning of record",
/* 108 */	"can't stat file",
/* 109 */	"no * after repeat count",
/* 110 */	"off end of record",
/* 111 */	"truncation failed",
/* 112 */	"incomprehensible list input",
/* 113 */	"out of free space",
/* 114 */	"unit not connected",
/* 115 */	"read unexpected character",
/* 116 */	"blank logical input field",
/* 117 */	"'new' file exists",
/* 118 */	"can't find 'old' file",
/* 119 */	"unknown system error",
/* 120 */	"requires seek ability",
/* 121 */	"illegal argument",
};

#define MAXERR (sizeof(F_err)/sizeof(char *)+100)


fatal(n,s) char *s;
{
	if(n<100 && n>=0)
		fprintf(stderr,"%s: [%d] %s\n",s,n,sys_errlist[n]);
	else if(n>=(int)MAXERR)
		fprintf(stderr,"%s: [%d] illegal error number\n",s,n);
	else if(n<0)
		fprintf(stderr,"%s: [%d] end of file\n",s,n);
	else
		fprintf(stderr,"%s: [%d] %s\n",s,n,F_err[n-100]);
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
	{	fprintf(stderr,"lately: %s %s %s %s IO\n",
			reading?"reading":"writing",
			sequential?"sequential":"direct",
			formatted>0?"formatted":(formatted<0?"list":"unformatted"),
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
	_cleanup();
	abort();
}

prnt_ext()
{	int ch;
	int i=1;
	long loc;
	fprintf (stderr, "part of last data: ");
	loc = ftell(curunit->ufd);
	if(loc)
	{	if(loc==1L) rewind(curunit->ufd);
		else for(;i<12 && last_char(curunit->ufd)!='\n';i++);
		while(i--) fputc(fgetc(curunit->ufd),stderr);
	}
	fputc('|',stderr);
	for(i=0;i<5 && (ch=fgetc(curunit->ufd)!=EOF);i++) fputc(ch,stderr);
	fputc('\n',stderr);
}

prnt_int()
{	char *ep;
	fprintf (stderr,"part of last string: ");
	ep = icptr - (recpos<12?recpos:12);
	while (ep<icptr) fputc(*ep++,stderr);
	fputc('|',stderr);
	while (ep<(icptr+5) && ep<icend) fputc(*ep++,stderr);
	fputc('\n',stderr);
}

prnt_fmt(n) int n;
{	int i; char *ep;
	fprintf(stderr, "part of last format: ");
	if(n==100)
	{	i = fmtptr - fmtbuf;
		ep = fmtptr - (i<20?i:20);
		i = i + 5;
	}
	else
	{	ep = fmtbuf;
		i = 25;
		fmtptr = fmtbuf - 1;
	}
	while(i && *ep)
	{	fputc((*ep==GLITCH)?'"':*ep,stderr);
		if(ep==fmtptr) fputc('|',stderr);
		ep++; i--;
	}
	fputc('\n',stderr);
}

/*initialization routine*/
f_init()
{	ini_std(STDERR, stderr, WRITE);
	ini_std(STDIN, stdin, READ);
	ini_std(STDOUT, stdout, WRITE);
}

