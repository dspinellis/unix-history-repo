/* Copyright (c) 1979 Regents of the University of California */
#include	"stdio.h"
#define tst(a,b) (*mode == 'r' ? (b) : (a))

FILE *
popen(cmd,mode)
char *cmd;
char *mode;
{
	register i;
	FILE *fptr;
	struct pstruct {
		int reader;
		int writer;
	} str;

	if (pipe(&str)<0) return NULL;
	if ((i=fork())==0) {
		close(tst(str.writer,str.reader));
		close(tst(0,1));
		dup(tst(str.reader,str.writer));
		close(tst(str.reader,str.writer));
		execl("/bin/sh","sh","-c",cmd,0);
		exit(1);
	}
	if (i== -1) return NULL;
	close(tst(str.reader,str.writer));
	fptr=fopen("/dev/null",tst("w","r"));
	setbuf(fptr,NULL);
	fptr->_file=tst(str.writer,str.reader);
	return fptr;
}

pclose(ptr)
FILE *ptr;
{
	int st;

	fclose(ptr);
	wait(&st);
	return st;
}
