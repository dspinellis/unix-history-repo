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
static char sccsid[] = "@(#)pix.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * pix - pi then px
 *
 * Bill Joy UCB August 26, 1977
 */

#include "whoami.h"
#include "objfmt.h"
#include "config.h"
#define	ERRS	1

char	*name;

int	onintr();

#define	ETXTBSY	26

main(argc, argv)
	int argc;
	char *argv[];
{
	register char **av;
	register int ac;
	int i, io, pid, status;
	extern errno;

	do
		io = open("/dev/null", 0);
	while (io >= 0 && io < 3);
	for (io = 3; io < 15; io++)
		close(io);
	if ((signal(2, 1) & 01) == 0)
		signal(2, onintr);
	for (ac = 1; ac < argc; ac++)
		if (dotted(argv[ac], 'p')) {
			ac++;
			break;
		}
	name = "-o/tmp/pixaXXXXX" + 2;
	mktemp(name);
	for (;;) {
		io = creat(name, 0400);
		if (io > 0)
			break;
		if (name[8] == 'z') {
			perror(name);
			exit(1);
		}
		name[8]++;
	}
	pid = fork();
	if (pid == -1) {
		write(2, "No more processes\n", 18);
		onintr();
	}
	if (pid == 0) {
		if (io != 3) {
			write(2, "Impossible error in pix\n", 24);
			onintr();
		}
		argv[ac] = 0;
		argv[0] = name - 2;
		do
			execv(pi_comp, argv);
		while (errno == ETXTBSY);
		write(2, "Can't find pi\n", 14);
		onintr();
	}
	close(io);
	do
		i = wait(&status);
	while (i != pid && i != -1);
	if (i == -1 || (status & 0377))
		onintr();
	if (status != 0) {
		if ((status >> 8) == ERRS)
			write(2, "Execution suppressed due to compilation errors\n", 47);
		onintr();
	}
	ac--;
	argv[ac] = name;
	ac--;
	argv[ac] = "pix";
	argv[argc] = 0;
	do
		execv(px_debug, &argv[ac]);
	while (errno == ETXTBSY);
	write(2, "Can't find px\n", 14);
	onintr();
}

dotted(cp, ch)
	char *cp, ch;
{
	register int i;

	i = strlen(cp);
	return (i > 1 && cp[i - 2] == '.' && cp[i - 1] == ch);
}

onintr()
{

	signal(2, 1);
	unlink(name);
	exit(1);
}
