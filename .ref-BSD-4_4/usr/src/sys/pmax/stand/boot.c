/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
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
 *	@(#)boot.c	8.1 (Berkeley) 6/10/93
 */

#include <sys/param.h>
#include <sys/exec.h>
#include <pmax/stand/dec_prom.h>

char	line[1024];

/*
 * This gets arguments from the PROM, calls other routines to open
 * and load the program to boot, and then transfers execution to that
 * new program.
 * Argv[0] should be something like "rz(0,0,0)vmunix" on a DECstation 3100.
 * Argv[0,1] should be something like "boot 5/rz0/vmunix" on a DECstation 5000.
 * The argument "-a" means vmunix should do an automatic reboot.
 */
void
main(argc, argv)
	int argc;
	char **argv;
{
	register char *cp;
	int ask, entry;

#ifdef JUSTASK
	ask = 1;
#else
	/* check for DS5000 boot */
	if (strcmp(argv[0], "boot") == 0) {
		argc--;
		argv++;
	}
	cp = *argv;
	ask = 0;
#endif /* JUSTASK */
	for (;;) {
		if (ask) {
			printf("Boot: ");
			gets(line);
			if (line[0] == '\0')
				continue;
			cp = line;
			argv[0] = cp;
			argc = 1;
		} else
			printf("Boot: %s\n", cp);
		entry = loadfile(cp);
		if (entry != -1)
			break;
		ask = 1;
	}
	printf("Starting at 0x%x\n\n", entry);
	if (callv == &callvec)
		((void (*)())entry)(argc, argv, 0, 0);
	else
		((void (*)())entry)(argc, argv, DEC_PROM_MAGIC, callv);
}

/*
 * Open 'filename', read in program and return the entry point or -1 if error.
 */
loadfile(fname)
	register char *fname;
{
	register struct devices *dp;
	register int fd, i, n;
	struct exec aout;

	if ((fd = open(fname, 0)) < 0) {
		goto err;
	}

	/* read the exec header */
	i = read(fd, (char *)&aout, sizeof(aout));
	if (i != sizeof(aout)) {
		goto cerr;
	} else if (aout.a_magic != OMAGIC) {
		goto cerr;
	}

	/* read the code and initialized data */
	printf("Size: %d+%d", aout.a_text, aout.a_data);
	if (lseek(fd, (off_t)N_TXTOFF(aout), 0) < 0) {
		goto cerr;
	}
	i = aout.a_text + aout.a_data;
	n = read(fd, (char *)aout.a_entry, i);
#ifndef SMALL
	(void) close(fd);
#endif
	if (n < 0) {
		goto err;
	} else if (n != i) {
		goto err;
	}

	/* kernel will zero out its own bss */
	n = aout.a_bss;
	printf("+%d\n", n);

	return ((int)aout.a_entry);

cerr:
#ifndef SMALL
	(void) close(fd);
#endif
err:
	printf("Can't boot '%s'\n", fname);
	return (-1);
}
