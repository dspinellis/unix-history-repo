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
 *	@(#)init_main.c	8.2 (Berkeley) 8/15/93
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <machine/cpu.h>
#include <machine/stinger.h>
#include <luna68k/stand/romvec.h>
#include <luna68k/stand/status.h>

extern int cpuspeed;
extern int dipsw1, dipsw2;

extern char default_file[];

#define	VERS_LOCAL	"Phase-31"

extern int howto;
extern int devtype;
       int nplane;

/* KIFF */

struct KernInter  KIFF;
struct KernInter *kiff = &KIFF;

/* for command parser */

#define BUFFSIZE 100
#define MAXARGS  30

char buffer[BUFFSIZE];

int   argc;
char *argv[MAXARGS];

char  prompt[16];

main()
{
	int i, status;
	int *p;

	/*
	 * Initialize the console before we print anything out.
	 */
	cpuspeed = MHZ_25;				/* for DELAY() macro */

	nplane   = get_plane_numbers();

	cninit();

	printf("\n\nStinger ver 0.0 [%s]\n\n", VERS_LOCAL);

	kiff->maxaddr = (caddr_t) (ROM_memsize -1);
	kiff->dipsw   = ~((dipsw2 << 8) | dipsw1) & 0xFFFF;
	kiff->plane   = nplane;

	i = (int) kiff->maxaddr + 1;
	printf("Physical Memory = 0x%x  ", i);
	i >>= 20;
	printf("(%d MB)\n", i);
	printf("\n");

	bcopy(VERS_LOCAL, prompt, sizeof(VERS_LOCAL));
	prompt[sizeof(VERS_LOCAL) - 1]	= '>';
	prompt[sizeof(VERS_LOCAL)]	= ' ';
	prompt[sizeof(VERS_LOCAL) + 1]	= 0;

	/*
	 * IO configuration
	 */

	find_devs();
	configure();
	printf("\n");

	howto = reorder_dipsw(dipsw2);

	if ((howto & 0xFE) == 0) {
		printf("auto-boot %s\n", default_file);
		
		i = open(default_file, 0);
		if (i >= 0) {
			bootunix(howto, devtype, i);
			close(i);
		}
	}

	/*
	 * Main Loop
	 */

	do {
		bzero(buffer, BUFFSIZE);
		if (getline(prompt, buffer) > 0) {
			argc = getargs(buffer, argv, sizeof(argv)/sizeof(char *));

			status = parse(argc, argv);
			if (status == ST_NOTFOUND)
				printf("Command \"%s\" is not found !!\n", argv[0]);
		}
	} while(status != ST_EXIT);

	exit();
}

int
get_plane_numbers()
{
	register int r = ROM_plane;
	register int n = 0;

	for (; r ; r >>= 1)
		if (r & 0x1)
			n++;

	return(n);
}

int
reorder_dipsw(dipsw)
	int dipsw;
{
	int i, sw = 0;

	for (i = 0; i < 8; i++) {
		if ((dipsw & 0x01) == 0)
			sw += 1;

		if (i == 7)
			break;

		sw <<= 1;
		dipsw >>= 1;
	}

	return(sw);
}
