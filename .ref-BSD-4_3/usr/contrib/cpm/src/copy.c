/*	copy.c	1.8	83/05/13	*/

#include <stdio.h>
#include "cpmio.h"
#include "cpmfio.h"

#define	CTRL_Z	0x1a		/* CP/M end-of-file */

/* 
 * copy cpmfile to unix file 
 */

copyc(cmdline, bin)
	char cmdline[];
{

	char	*index(), *i;

	if ((i = index(cmdline,' ')) == NULL) {
		printf("too few arguments: %s\n", cmdline);
		return;
	}
	*i = '\0';
	copy(cmdline, i+1, bin);
}

copy(cpmfile, unixfile, bin)
	char cpmfile[], unixfile[];
{

	FILE *ufid;
	char name[9], ext[4];
	C_FILE *cid;

	if (!(namesep(cpmfile, name, ext))) 
		return;
	if ((cid = c_open(name, ext, READ)) == NULL)
		return;

	if ( unixfile == (char *)stdout )
		ufid = stdout;
	else {
		if (access(unixfile,0) == 0) {
			printf("%s already exists\n", unixfile);
			return;
		}
		if ((ufid = fopen(unixfile, "w")) == NULL) {
			printf("can't open %s\n", unixfile);
			return;
		}
	}
	if (bin)
		copybin(cid, ufid);
	else
		copytext(cid, ufid);
	c_close(cid);  
}

copytext(cid, ufid)
	FILE *ufid;
	C_FILE *cid;
{
	int c = 0;

	while (((c = c_getc(cid)) != EOF) && (c != CTRL_Z)) {
		if ( c != '\r') 
			putc(c, ufid);
	}
	if (isatty(fileno(ufid))) 
		printf("\n");
	else
		fclose(ufid);
}

copybin(cid, ufid)
	FILE *ufid;
	C_FILE *cid;
{
	int c = 0;

	while ((c = c_getc(cid)) != EOF) 
		putc(c, ufid);
	fclose(ufid);
}
