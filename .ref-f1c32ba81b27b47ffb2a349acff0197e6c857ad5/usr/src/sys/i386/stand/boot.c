/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)boot.c	7.1 (Berkeley) %G%";
#endif /* not lint */

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/dir.h"
#include "../h/reboot.h"
#include "../h/disk.h"
#include <a.out.h>
#include "saio.h"

/*
 * Boot program... arguments passed in r6 and r7 determine
 * whether boot stops to ask for system name and which device
 * boot comes from.
 */

#define	UNIX	"/vmunix"

char line[100] = UNIX;
char line2[100] = "/stand/";
extern	int howto, bootdev, unit, cyloffset, boottype;
extern	int opendev, openfirst;
int	retry = 0;

main()
{
	int io;

howto = RB_SINGLE|RB_ASKNAME;
	for (;;) {
		if (howto & RB_ASKNAME) {
			printf("Boot: ");
			gets(line);
		} else
			printf("Boot: %s\n", line);
		if (line[0] == 0) {
			strcpy(line, UNIX);
			printf("Boot: %s\n", line);
		}

		io = open(line, 0);
		/*if (io < 0) {
			strcat(line2,line) ;
			io = open(line2, 0);
		}*/
		if (io >= 0) {
			bootdev = opendev;
			copyunix(io);
		}
		openfirst = 1;
		if (++retry > 2)
			howto = RB_SINGLE|RB_ASKNAME;
	}
}

/*ARGSUSED*/
copyunix(io)
	register io;
{
	struct exec x;
	int i;
	char *addr;

	i = read(io, (char *)&x, sizeof x);
	if (i != sizeof x ||
	    (x.a_magic != 0407 && x.a_magic != 0413 && x.a_magic != 0410))
		_stop("Bad format\n");
	printf("%d", x.a_text);
	if (x.a_magic == 0413 && lseek(io, 0x400, 0) == -1)
		goto shread;
	if (read(io, (char *)0, x.a_text) != x.a_text)
		goto shread;
	addr = (char *)x.a_text;
	if (x.a_magic == 0413 || x.a_magic == 0410)
		while ((int)addr & CLOFSET)
			*addr++ = 0;
	printf("+%d", x.a_data);
	if (read(io, addr, x.a_data) != x.a_data)
		goto shread;
	addr += x.a_data;
	printf("+%d", x.a_bss);
	x.a_bss += 128*512;	/* slop */
	for (i = 0; i < x.a_bss; i++)
		*addr++ = 0;
	printf(" start 0x%x\n", x.a_entry);
	setregs() ;
	i = (*((int (*)()) x.a_entry))();
	if (i) printf("exit %d\n", i) ; 
	return;
shread:
	_stop("Short read\n");
}
