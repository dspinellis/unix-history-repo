/*
char id_util[] = "@(#)util.c	1.2";
 *
 * utility routines
 */

#include <sys/types.h>
#include <sys/stat.h>
#include "fio.h"


ini_std(u,F,w) FILE *F;
{	unit *p;
	p = &units[u];
	p->ufd = F;
	p->ufnm = NULL;
	p->useek = canseek(F);
	p->ufmt = YES;
	p->uwrt = (w==WRITE)? YES : NO;
	p->ublnk = p->uscrtch = p->uprnt = p->uend = NO;
	p->url = 0;
	p->uinode = finode(F);
}

canseek(f) FILE *f; /*SYSDEP*/
{	struct stat x;
	return( (fstat(fileno(f),&x)==0) &&
	(x.st_nlink > 0 /*!pipe*/) && !isatty(fileno(f)) );
}

nowreading(x) unit *x;
{
	long loc;
	x->uwrt = NO;
	loc=ftell(x->ufd);
	freopen(x->ufnm,"r",x->ufd);
	fseek(x->ufd,loc,0);
}

nowwriting(x) unit *x;
{
	long loc;
	x->uwrt = YES;
	loc=ftell(x->ufd);
	freopen(x->ufnm,"a",x->ufd);
	fseek(x->ufd,loc,0);
}

g_char(a,alen,b) char *a,*b; ftnlen alen;
{	char *x=a+alen-1, *y=b+alen-1;
	while (x >= a  &&  *x == ' ') {x--; y--;}
	*(y+1) = '\0';
	while (x >= a) *y-- = *x--;
}

b_char(from, to, tolen) char *from, *to; ftnlen tolen;
{	int i=0;
	while (*from && i < tolen) {
		*to++ = *from++;
		i++;
	}
	while (i++ < tolen)
		*to++ = ' ';
}

inode(a) char *a;
{	struct stat x;
	if(stat(a,&x)==0) return(x.st_ino);
	else return(-1);
}

finode(f) FILE *f;
{	struct stat x;
	if(fstat(fileno(f),&x)==0) return(x.st_ino);
	else return(-1);
}

char
last_char(f) FILE *f;
{
	fseek(f,-2L,1);
	if(ftell(f)) return(getc(f));
	else return('\n');
}
