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
	debugflg = DBV;
	setupdaemon(argc,argv);
	strcpy(str,ofile);
	strcat(str,"                             ");
	sendreset();
	xwrite(str,20);
	if(stat(ifile,&statbuf) < 0){
		perror(ifile);
		exit(1);
		}
	work = getsize(&statbuf);
	work = fixuplong(work);
	xwrite(&work,4);
	while((n = fread(buf,1,BUFSIZ,fp)) > 0)
		xwrite(buf,n);
	fclose(fp);
	}
