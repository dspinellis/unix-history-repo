/*	cmdhdl.c	1.12	85/03/24	*/

#include <ctype.h>
#include <stdio.h>
#include "cpmio.h"

/*
 * Write prompt to stdout.
 * Read command from stdin.
 * return the number of characters read.
 */

cmdinp(cmd)
	char cmd[];
{

	int cnt = 0;

	while (cnt == 0) {
		printf("cpm> ");
		while((cmd[cnt] = getchar()) != '\n') {
			if (cmd[cnt] == EOF)
				return(-1);
			cnt++;
		}
		cmd[cnt] = '\0';
	}
	return (cnt);
}

/*
 * Compare the command pointed to by cmd to the table of defined
 * commands in defcmd, return the command index if found, null
 * otherwise.
 */

struct command {
	char *cmd;
	int  lth, abbr;
} defcmd[] = {
	"directory",	9,	1,		/* 1 */
	"rename",	6,	1,		/* 2 */
	"copyin",	6,	0,		/* 3 */
	"delete",	6,	1,		/* 4 */
	"erase",	5,	1,		/* 5 */
	"exit",		4,	1,		/* 6 */	
	"type",		4,	1,		/* 7 */
	"help",		4,	1,		/* 8 */
	"ls ",		2,	0,		/* 9 */	
	"logout",	6,	1,		/* 10 */	
	"ccopyin",	7,	0,		/* 11 */
	"ccopyout",	8,	0, 		/* 12 */
	"copyout",	7,	0,		/* 13 */
	"dump",		4,	1, 		/* 14 */
	/*"!! ",		2,	0,		/* 15 */
	'\0' , 0 };

chkcmd(cmd)
	char *cmd;

{
	int index, len;

	len = strlen(cmd);
	for (index=0; *defcmd[index].cmd != '\0' ; index++) {
		if ((len == 3) && defcmd[index].abbr) {
			if (strncmp(defcmd[index].cmd, cmd, 3) == 0)
				goto ok;
		} else {
			if (strncmp(defcmd[index].cmd, cmd, defcmd[index].lth) 
									== 0)
				goto ok;
		}
	}
	return (0);

ok:
	if (len > defcmd[index].lth)
		return (0);
	else
		return (++index);
}

help()
{

	FILE *fd, *fopen();
	int c;

	if ((fd = fopen(HELPFILE, "r")) == NULL)
		printf("Can't find help file (cpm.hlp) \n");
	else
		while ( (c = getc(fd)) != EOF) 
			putchar(c);
}


/*
 * Separate fname into name and extension, return NULL if
 * bad file name, otherwise 1.
 */

namesep(fname, name, ext)
	char fname[], name[], ext[];
{

	int i = 0;

	strncpy(name, "         ", 9);
	strncpy(ext, "    ", 4);
	while(i<8 && !(iscntrl(fname[i])) && fname[i] != '.') {
		name[i] = fname[i];
		i++;
	}
#ifdef DEBUG
	printf("namesep: name=%s, len=%d ", name, i);
#endif
	clean(name, 8);
	if (fname[i] == '.') {
		strncpy(ext, fname+i+1, 3);
		clean(ext, 3);
	} else {
		if (fname[i] != ' ' && fname[i] != '\0') {
			fprintf(stderr, "Illegal filename\n");
			return (NULL);
		}
	}
#ifdef DEBUG
	printf("name: %s, ext: %s, combined: %s\n", name, ext,
		fixname(name, ext));
#endif
	if (!(isalnum(name[0]))) {
		fprintf(stderr, "Illegal filename\n");
		return(NULL);
	}
	return(1);
}

clean(str, len)
char str[];
int len;
{
	str[len] = '\0';
	while (len-- > 0) {
		if (!(isspace(str[len])) && !(iscntrl(str[len]))) break;
		str[len] = ' ';
	}
	while (len >= 0) {
		str[len] = islower(str[len]) ? toupper(str[len]):str[len] ;
		len--;
	}
}
