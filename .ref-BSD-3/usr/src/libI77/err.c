#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include "fio.h"
#define STR(x) (x==NULL?"":x)

/*global definitions*/
unit units[MXUNIT];	/*unit table*/
flag init;	/*0 on entry, 1 after initializations*/
cilist *elist;	/*active external io list*/
flag reading;	/*1 if reading, 0 if writing*/
flag cplus,cblank;
char *fmtbuf;
flag external;	/*1 if external io, 0 if internal */
int (*doed)(),(*doned)();
int (*doend)(),(*donewrec)(),(*dorevert)();
flag sequential;	/*1 if sequential io, 0 if direct*/
flag formatted;	/*1 if formatted io, 0 if unformatted*/
int (*getn)(),(*putn)();	/*for formatted io*/
FILE *cf;	/*current file*/
unit *curunit;	/*current unit*/
int recpos;	/*place in current record*/
int cursor,scale;

/*error messages*/
char *F_err[] =
{
	"error in format",
	"illegal unit number",
	"formatted io not allowed",
	"unformatted io not allowed",
	"direct io not allowed",
	"sequential io not allowed",
	"can't backspace file",
	"null file name",
	"can't stat file",
	"unit not connected",
	"off end of record",
	"truncation failed in endfile",
	"incomprehensible list input",
	"out of free space",
	"unit not connected",
	"read unexpected character",
	"blank logical input field",
};
#define MAXERR (sizeof(F_err)/sizeof(char *)+100)
fatal(n,s) char *s;
{
	if(n<100 && n>=0) perror(s); /*SYSDEP*/
	else if(n>=(int)MAXERR)
	{	fprintf(stderr,"%s: illegal error number %d\n",s,n);
	}
	else if(n<0) fprintf(stderr,"%s: end of file %d\n",s,n);
	else
		fprintf(stderr,"%s: %s\n",s,F_err[n-100]);
	fprintf(stderr,"apparent state: unit %d named %s\n",curunit-units,
		STR(curunit->ufnm));
	fprintf(stderr,"last format: %s\n",STR(fmtbuf));
	fprintf(stderr,"lately %s %s %s %s IO\n",reading?"reading":"writing",
		sequential?"sequential":"direct",formatted?"formatted":"unformatted",
		external?"external":"internal");
	_cleanup();
	abort();
}
/*initialization routine*/
f_init()
{	unit *p;
	init=1;
	p= &units[0];
	p->ufd=stderr;
	p->useek=canseek(stderr);
	p->ufmt=1;
	p->uwrt=1;
	p = &units[5];
	p->ufd=stdin;
	p->useek=canseek(stdin);
	p->ufmt=1;
	p->uwrt=0;
	p= &units[6];
	p->ufd=stdout;
	p->useek=canseek(stdout);
	p->ufmt=1;
	p->uwrt=1;
}
canseek(f) FILE *f; /*SYSDEP*/
{	struct stat x;
	fstat(fileno(f),&x);
	if(x.st_nlink > 0 /*pipe*/ && !isatty(fileno(f)))
	{
		return(1);
	}
	return(0);
}
nowreading(x) unit *x;
{
	long loc;
	x->uwrt=0;
	loc=ftell(x->ufd);
	freopen(x->ufnm,"r",x->ufd);
	fseek(x->ufd,loc,0);
}
nowwriting(x) unit *x;
{
	long loc;
	loc=ftell(x->ufd);
	x->uwrt=1;
	freopen(x->ufnm,"a",x->ufd);
	fseek(x->ufd,loc,0);
}
