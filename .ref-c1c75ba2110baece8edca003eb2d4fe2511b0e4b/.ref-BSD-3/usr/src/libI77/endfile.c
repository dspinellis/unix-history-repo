#include "fio.h"
static alist *ax;
f_end(a) alist *a;
{
	unit *b;
	if(a->aunit>=MXUNIT || a->aunit<0) err(a->aerr,101,"endfile");
	b = &units[a->aunit];
	if(b->ufd==NULL) return(0);
	b->uend=1;
	if( b->useek==0) return(0);
	ax=a;
	if(b->uwrt) nowreading(b);
	return(t_runc(b));
}
t_runc(b) unit *b;
{
	char buf[128],nm[16];
	FILE *tmp;
	int n,m;
	long loc,len;
	if(b->url) return(0);	/*don't truncate direct files*/
	loc=ftell(b->ufd);
	fseek(b->ufd,0L,2);
	len=ftell(b->ufd);
	if(loc==len || b->useek==0 || b->ufnm==NULL) return(0);
	strcpy(nm,"tmp.FXXXXXX");
	if(b->uwrt) nowreading(b);
	mktemp(nm);
	tmp=fopen(nm,"w");
	fseek(b->ufd,0L,0);
	for(;loc>0;)
	{
		n=fread(buf,1,loc>128?128:(int)loc,b->ufd);
		if(n>loc) n=loc;
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
			fprintf(stdout,"no cp\n");
			exit(1);
		}
		wait(&m);
		if(m!=0) err(ax->aerr,111,"endfile");
		fclose(tmp);
		unlink(nm);
		return(0);
	}
	err(ax->aerr,111,"endfile");
}
