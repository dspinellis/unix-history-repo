/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1980, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)px_header.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * pxheader - program to sit in front of interpreter code to make shell mods
 *	      unnecessary to make Pascal obj's look like real programs.
 *
 * Bill Joy UCB February 6, 1978
 */

#include <stdio.h>
#include <sys/types.h>
#include <a.out.h>
#include "config.h"
#include "whoami.h"
#include "objfmt.h"

#define	ETXTBSY	26
#define	ADDR_LC \
	(START + HEADER_BYTES - sizeof (struct exec) - sizeof (struct pxhdr))
#define MAXARGS 512

extern	errno;

main(argc, argv)
	register int argc;
	register char *argv[];
{
	register int i;
	int codesiz, symtabsiz;
	register char *cp;
	char *largv[MAXARGS];
	int fd, pv[2], pid;

	cp = (char *)(ADDR_LC);
	codesiz = ((struct pxhdr *)(cp))->objsize + sizeof(struct pxhdr);
	symtabsiz = ((struct pxhdr *)(cp))->symtabsize;
	if (argc > MAXARGS - 3)
		error(2, "Too many arguments.\n");
	if (symtabsiz != 0) {
		largv[0] = "pxhdr";
		largv[1] = "/tmp/px00000";
		cp = &largv[1][11];
		for (i = getpid(); i > 0; i /= 10)
			*cp-- = '0' + i % 10;
		fd = creat(largv[1], 0444);
		if (fd < 0)
			error(3, "Cannot create /tmp file\n");
		for (i = 0; i < argc; i++)
			largv[i + 2] = argv[i];
		largv[argc + 2] = 0;
		writeobj(fd, codesiz, symtabsiz);
		run(px_debug, largv);
		/* no return */
	}
	largv[0] = "pipe";
	for (i = 0; i < argc; i++)
		largv[i + 1] = argv[i];
	largv[argc + 1] = 0;
	pipe(pv);
	pid = fork();
	if (pid != 0) {
		if (pv[0] != 3) {
			close(3);
			dup(pv[0]);
			close(pv[0]);
		}
		close(pv[1]);
		run(px_intrp, largv);
		/* no return */
	}
	writeobj(pv[1], codesiz, symtabsiz);
	exit(0);
}

writeobj(fd, codesiz, symtabsiz)
	int fd;
	int codesiz, symtabsiz;
{
	int i;
	register char *cp;

	cp = (char *)(ADDR_LC);
	while (codesiz != 0) {
		i = (codesiz < BUFSIZ) ? codesiz : BUFSIZ;
		write(fd, cp, i);
		cp += i;
		codesiz -= i;
	}
	while (symtabsiz != 0) {
		i = (symtabsiz < BUFSIZ) ? symtabsiz : BUFSIZ;
		write(fd, cp, i);
		cp += i;
		symtabsiz -= i;
	}
	close(fd);
}

run(prog, args)
	char *prog;
	char **args;
{
	for (;;) {
		execv(prog, args);
		if (errno != ETXTBSY)
			break;
		sleep(2);
	}
	error(0, prog);
	error(1, " not found.\n");
}

error(errcode, cp)
	int errcode;
	register char *cp;
{
	register int i;
	register char *dp;

	dp = cp;
	i = 0;
	while (*dp++)
		i++;
	write(2, cp, i);
	if (errcode)
		exit(errcode);
}

exit(i)
{
	_exit(i);
}
