/* Copyright (c) 1979 Regents of the University of California */
# include "defs.h"
struct packet *xptr, *gptr;
main(argc,argv)
  char **argv;{
	int n,t,i;
	char buf[2000];
	printf("len: ");
	fflush(stdout);
	gets(buf,stdin);
	n = atoi(buf);
	printf("iter: ");
	fflush(stdout);
	gets(buf,stdin);
	t = atoi(buf);
	printf("len %d iter %d\n",n,t);
	debugflg = DBV;
	setupdaemon(argc,argv);
	sendreset();
	xwrite(&n,1,2);
	for(i=0; i<n; i++)buf[i] = 'A';
	buf[n-1] = '\n';
	datasize = n;
	xptr = gptr = NULL;
	for(i=0;i<t;i++){
		if(i == t-1)buf[0] = 'Z';
		xwrite(buf,1,n);
		putchar('.');
		}
	}
