/* Copyright (c) 1979 Regents of the University of California */
# include "defs.h"
main(argc,argv)
  char **argv; {
	FILE *fp;
	int buf[BFS], n;
	long work;
	char str[50];
	char ifile[20],ofile[20];
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
	xwrite(str,1,20);
	if(stat(ifile,&statbuf) < 0){
		perror(ifile);
		exit(1);
		}
	work = getsize(&statbuf);
	work = fixuplong(work);
	xwrite(&work,1,4);
	while((n = fread(buf,1,512,fp)) > 0)
		xwrite(buf,1,n);
	fclose(fp);
	}
