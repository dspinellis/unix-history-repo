/*	hexdmp.c	1.5	83/05/13	*/

#include <ctype.h>
#include <stdio.h>
#include "cpmio.h"
#include "cpmfio.h"

dump(cmdline)
	char cmdline[];
{
	char name[9], ext[4];
	C_FILE *cid;

	if (!(namesep(cmdline, name, ext))) 
		return;
	if ((cid = c_open(name, ext, READ)) == NULL)
		return;
	hexdump(cid);
	c_close(cid);  
}

hexdump(fp)
	C_FILE *fp;
{
	int c, nc=0, cbuf[16], blcnt=0;

	printf("\n");
	while ((c = c_getc(fp)) != EOF) {
		cbuf[nc%16] = c;
		if (nc%128 == 0)
			printf("\n      Block %04d\n", blcnt++);
		if (nc%16  == 0)
			printf("%04x  ", nc);
		++nc;
		printf("%02x ", c);
		if (nc%16  == 0) 
			printline(cbuf, 16);
	}
	if (nc%16 != 0)
		printline(cbuf, nc%16);
	printf("\n");
}

printline(cbuf, nc)
	int cbuf[];
{
	int i1;

	for (i1=0; i1< nc; ++i1) { 
		if (cbuf[i1] > 31 && cbuf[i1] < 127)
			printf("%c", cbuf[i1]);
		else
			printf(".");
	}
	printf("\n");
}
