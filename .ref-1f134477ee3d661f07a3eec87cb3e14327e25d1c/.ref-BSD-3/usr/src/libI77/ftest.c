#include "fio.h"
#define FLOAT double
cilist x,y;
main(argc,argv) char **argv;
{	FLOAT dd[20];
	int ret[22];
	long one=1;
	int i,n,j;
	if(argc<2)
	{	fprintf(stderr,"%s infmt [outfmt] [n]\n",argv[0]);
		exit(1);
	}
	setcilist(&x,5,argv[1],0,1,1);
	argc--;
	argv++;
	if(argc<2 || argv[1][0]!='(')
		setcilist(&y,6,argv[0],0,1,1);
	else
	{	setcilist(&y,6,argv[1],0,1,1);
		argc--;
		argv++;
	}
	if(argc>=2) n=atoi(argv[1]);
	else n=1;
	j=0;
	ret[j++]=s_rsfe(&x);
	for(i=0;i<n;i++)
	{	ret[j++]=do_fio(&one,&dd[i],(long)sizeof(FLOAT));
	}
	ret[j++]=e_rsfe();
	for(i=0;i<j;i++) fprintf(stderr,"%d ",ret[i]);
	putc('\n',stderr);
	j=0;
	ret[j++]=s_wsfe(&y);
	for(i=0;i<n;i++)
		ret[j++]=do_fio(&one,&dd[i],(long)sizeof(FLOAT));
	ret[j++]=e_wsfe();
	for(i=0;i<j;i++) fprintf(stderr,"%d ",ret[i]);
	putc('\n',stderr);
	f_exit();
	exit(0);
}
