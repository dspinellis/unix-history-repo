/*
 * endfile
 */

#include "fio.h"

char *endf = "endfile";
extern char *tmplate;

f_end(a) alist *a;
{
	unit *b;
	lfname = NULL;
	elist = NO;
	errflag = a->aerr;
	lunit = a->aunit;
	if (not_legal(lunit)) err(errflag,101,endf)
	b = &units[lunit];
	if(!b->ufd) err(errflag,114,endf)
	if(b->uend) return(0);
	lfname = b->ufnm;
	b->uend = YES;
	return(t_runc(b,errflag));
}

t_runc(b,flag) unit *b; ioflag flag;
{
	char buf[128],nm[16];
	FILE *tmp;
	int n,m;
	long loc,len;
	fflush(b->ufd);
	if(b->uwrt) nowreading(b);
	if(b->url || !b->useek || !b->ufnm) return(OK); /*don't trunc dir files*/
	loc=ftell(b->ufd);
	fseek(b->ufd,0L,2);
	len=ftell(b->ufd);
	if (loc==len) return(OK);
	strcpy(nm,tmplate);
	mktemp(nm);
	if(!(tmp=fopen(nm,"w"))) err(flag,errno,endf);
	fseek(b->ufd,0L,0);
	while (loc)
	{
		n=fread(buf,1,loc>sizeof(buf)?sizeof(buf):(int)loc,b->ufd);
		loc -= n;
		fwrite(buf,1,n,tmp);
	}
	fflush(tmp);
	for(n=0;n<10;n++)
	{
		if((m=fork())==-1) continue;
		else if(m==0)
		{
			execl("/bin/cp","cp",nm,b->ufnm,0);
			execl("/usr/bin/cp","cp",nm,b->ufnm,0);
			fatal(119,"no cp for trunc");
		}
		wait(&m);
		if(m) err(flag,111,endf);
		fclose(tmp);
		unlink(nm);
		return(OK);
	}
	err(flag,111,endf);
}
