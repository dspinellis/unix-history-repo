#include <stdio.h>
#include <a.out.h>
#include <param.h>
#include <proc.h>
#include <sys/stat.h>
struct stat xstat;
struct proc mproc[NPROC];
struct nlist nl[]
{	{ "_proc"},
	{ ""},
};
int err;
main(argc,argv) char **argv;
{	int i,j;
	if(argc<=1) exit(0);
	for(i=1;i<argc;i++)
	{	if((j=isalock(argv[i]))>0)
			if(isapid(j))
			{	fprintf(stderr,"recovery: %s in use\n",argv[i]);
				err=1;
			}
			else	unlink(argv[i]);
		else	fprintf(stderr,"reocvery: %s not a lock\n",argv[i]);
	}
	exit(err);
}
isalock(s) char *s;
{	int pid,fd;
	if(stat(s,&xstat)<0) return(0);
	if(xstat.st_size!=2) return(0);
	fd=open(s,0);
	if(fd<0) return(-1);
	read(fd,&pid,2);
	close(fd);
	return(pid);
}
int gotpids,pids[NPROC];
isapid(n)
{	int i;
	if(gotpids==0) getpids();
	for(i=0;i<NPROC;i++)
		if(n==pids[i]) return(1);
	return(0);
}
getpids()
{	int i,mem;
	nlist("/unix",nl);
	if(nl[0].n_type==0)
	{	fprintf(stderr,"no namelist, no lock recovery\n");
		exit(1);
	}
	mem=open("/dev/mem",0);
	lseek(mem,(long)nl[0].n_value,0);
	read(mem,mproc,sizeof(mproc));
	if(mproc[0].p_pid != 0)
	{	fprintf(stderr,"/unix not loaded, no lock recovery\n");
		exit(1);
	}
	for(i=0;i<NPROC;i++)
	{	if(mproc[i].p_stat==0 ||
			mproc[i].p_stat==SZOMB)
			pids[i]=0;
		else	pids[i]=mproc[i].p_pid;
	}
	gotpids=1;
}
unlock(s) char *s;
{
	unlink(s);
}
