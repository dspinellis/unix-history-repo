/* Copyright (c) 1979 Regents of the University of California */
# include "defs.h"
main(argc,argv)
  char **argv; {
	FILE *fp;
	char *p, save[40];
	int i, n;
	char buf[BFS*2];
	long length;
	debugflg = DBV;
	setupdaemon(argc,argv);
	putchar('!');
	for(;;){
		lastseqno = -1;
		while(getreset() == BROKENREAD);
		while((i = nread(buf,1,20)) == BROKENREAD);
		if(i != 20){
			printf("Didn't read file name\n");
			exit(1);
			}
		for(p=buf; *p && *p != '\n' && *p != ' '; p++);
		*p = 0;
		printf("Creating file %s ",buf);
		fp = fopen(buf,"w");
		if(fp == NULL){
			perror(buf);
			exit(1);
			}
		strcpy(save,buf);
		while((i = nread(&length,1,4)) == BROKENREAD);
		if(i != 4){
			printf("Didn't read length right\n");
			exit(1);
			}
		length = fixuplong(length);
		printf("length %ld\n",length);
		while(length > 0){
			i = min(length,512);
			while((n = nread(buf,1,i)) == BROKENREAD);
			length -= n;
			fwrite(buf,1,n,fp);
			}
		fclose(fp);
		printf("Finished file %s\n",save);
		}
	}
