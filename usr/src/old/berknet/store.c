static char sccsid[] = "@(#)store.c	4.1	(Berkeley)	10/2/82";

/*
	store.c

	send a file to the program "receive.c" 
*/
# include "defs.h"
main(argc,argv)
  char **argv; {
	FILE *fp;
	int buf[BUFSIZ], n;
	long work;
	char str[50];
	char ifile[20],ofile[20];
	struct stat statbuf;

	printf("from file: ");
	fflush(stdout);
	gets(ifile,stdout);
	printf("to file: ");
	fflush(stdout);
	gets(ofile,stdout);
	fp = fopen(ifile,"r");
	if(fp == NULL){
		perror(ifile);
		exit(1);
		}
	debugflg = 1;
	setupdaemon(argc,argv);
	strcpy(str,ofile);
	initseqno();
	strcat(str,"                             ");
	xwrite(str,20);
	if(stat(ifile,&statbuf) < 0){
		perror(ifile);
		exit(1);
		}
	work = getsize(&statbuf);
	sprintf(buf, "|%08ld|", work);
	xwrite(buf,10);
	while((n = fread(buf,1,BUFSIZ,fp)) > 0)
		xwrite(buf,n);
	fclose(fp);
	}
