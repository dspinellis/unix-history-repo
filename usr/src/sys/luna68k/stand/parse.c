/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)parse.c	8.1 (Berkeley) 6/10/93
 */

/*
 * parse.c -- command parser
 * by A.Fujita, JAN-30-1992
 */

#include <luna68k/stand/status.h>

/* for scsi.c */

int scsi();

/* for disklabel.c */

int disklabel();

/* for boot.c */

int boot();

int load();

int how_to_boot();

/* for tape.c */

int tape();

/* for fsdump.c */

int fsdump();
int fsrestore();

/* for screen.c */

int screen();

int
check_args(argc, argv)
	int   argc;
	char *argv[];
{
	register int i;

	for ( i = 0; i < argc; i++)
		printf("argv[%d] = \"%s\"\n", i, argv[i]);

	return(ST_NORMAL);
}

int
exit_program(argc, argv)
	int   argc;
	char *argv[];
{
	return(ST_EXIT);
}

struct command_entry {
	char *name;
	int (*func)();
};

struct command_entry entries[] = {
{ "b",		boot         },
{ "boot",	boot         },
{ "chkargs",	check_args   },
{ "disklabel",	disklabel    },
{ "exit",	exit_program },
{ "fsdump",	fsdump       },
{ "fsrestore",	fsrestore    },
{ "howto",	how_to_boot  },
{ "load",       load         },
{ "screen",	screen	     },
{ "tape",	tape	     },
{ "tp",		tape	     },
{ "scsi",	scsi         },
{ "quit",	exit_program },
{ 0, 0 }
};


int 
parse(argc, argv)
	int   argc;
	char *argv[];
{
	register int i, status = ST_NOTFOUND;

	for (i = 0; entries[i].name != (char *) 0; i++) {
		if (!strcmp(argv[0], entries[i].name)) {
			status = (*entries[i].func)(argc, argv);
			break;
		}
	}

	return(status);
}



/*
 * getargs -- make argument arrays
 */

getargs(buffer, argv, maxargs)
	char buffer[], *argv[];
	int  maxargs;
{
	register int   n = 0;
	register char *p = buffer;

	argv[n++] = p;
	while (*p != '\0') {
		if ( *p == ' ' ) {
			*p = '\0';
		} else if (p != buffer && *(p-1) == '\0') {
			if ( n < maxargs )
				argv[n++] = p;
		}
		p++;
	}

	return(n);
}
