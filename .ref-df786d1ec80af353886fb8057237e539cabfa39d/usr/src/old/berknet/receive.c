static char sccsid[] = "@(#)receive.c	4.1	(Berkeley)	%G%";

/*
	receive.c

	take the file sent by "store.c" and write it locally
*/
# include "defs.h"

/* global variables */
struct daemonparms netd;

main(argc,argv)
  char **argv; {
	FILE *fp;
	char *p, save[40];
	int i, n;
	char buf[BUFSIZ];
	long length;
	debugflg = DBV;
	setupdaemon(argc,argv);
	putchar('!');
	for(;;){
		initseqno();
		while((i = nread(buf,20)) == BROKENREAD);
		if(i != 20){
			printf("Didn't read file name\n");
			exit(EX_USAGE);
			}
		for(p=buf; *p && *p != '\n' && *p != ' '; p++);
		*p = 0;
		printf("Creating file %s ",buf);
		fp = fopen(buf,"w");
		if(fp == NULL){
			fp = fopen(buf,"w");
			exit(EX_OSFILE);
			}
		strcpy(save,buf);
		while((i = nread(buf,10)) == BROKENREAD);
		if(i != 10){
			printf("Didn't read length right\n");
			exit(EX_SOFTWARE);
			}
		length = atol(buf+1);
		printf("length %ld\n",length);
		while(length > 0){
			i = min(length,512);
			while((n = nread(buf,i)) == BROKENREAD);
			length -= n;
			fwrite(buf,1,n,fp);
			}
		fclose(fp);
		printf("Finished file %s\n",save);
		}
	}
