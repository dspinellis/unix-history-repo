/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)boot.c	7.1 (Berkeley) %G%
 */

#include "reboot.h"
#include "exec.h"

#ifndef TEST
#define DEF_MONFUNCS
#include "../include/machMon.h"
#endif

char	line[1024];

/*
 * This gets arguments from the PROM, calls other routines to open
 * and load the program to boot, and then transfers execution to that
 * new program.
 * Argv[0] should be something like "rz(0,0,0)vmunix"
 * The argument "-a" means we were invoked by the 'auto' command from the prom.
 */
void
main(argc, argv, argenv)
	int argc;
	char **argv;
	char **argenv;
{
	register char *cp;
	int howto, entry;

	for (entry = 0; entry < argc; entry++)
		printf("%d: '%s'\n", entry, argv[entry]);
#ifdef JUSTASK
	howto = RB_ASKNAME | RB_SINGLE;
#else
	howto = (argc > 1 && strcmp(argv[1], "-a") == 0) ?
		0 : RB_SINGLE;
	for (cp = argv[0]; *cp; cp++) {
		if (*cp == ')' && cp[1]) {
			cp = argv[0];
			goto fnd;
		}
	}
	howto |= RB_ASKNAME;
fnd:
	;
#endif
	for (;;) {
		if (howto & RB_ASKNAME) {
			printf("Boot: ");
			gets(line);
			if (line[0] == '\0')
				continue;
			cp = line;
		} else
			printf("Boot: %s\n", cp);
		entry = loadfile(cp);
		if (entry != -1)
			break;
		howto = RB_ASKNAME | RB_SINGLE;
	}
#ifndef TEST
	Boot_Transfer(argc, argv, argenv, entry);
#endif
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

	if ((fd = Open(fname, 0)) < 0)
		goto err;

	/* read the COFF header */
	i = Read(fd, (char *)&aout, sizeof(aout));
	if (i != sizeof(aout)) {
		printf("No a.out header\n");
		goto cerr;
	} else if (aout.a_magic != OMAGIC) {
		printf("A.out? magic 0%o size %d+%d+%d\n", aout.a_magic,
			aout.a_text, aout.a_data, aout.a_bss);
		goto cerr;
	}

	/* read the code and initialized data */
	printf("Size: %d+%d", aout.a_text, aout.a_data);
	if (Lseek(fd, N_TXTOFF(aout), 0) < 0) {
		printf("\nSeek error\n");
		goto cerr;
	}
	i = aout.a_text + aout.a_data;
#ifndef TEST
	n = Read(fd, (char *)aout.ex_aout.codeStart, i);
#else
	n = i;
#endif
	(void) Close(fd);
	if (n < 0) {
		printf("\nRead error\n");
		goto err;
	} else if (n != i) {
		printf("\nShort read (%d)\n", n);
		goto err;
	}

	/* kernel will zero out its own bss */
	n = aout.a_bss;
	printf("+%d\n", n);

	return ((int)aout.a_entry);

cerr:
	(void) Close(fd);
err:
	return (-1);
}
