/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)boot.c	7.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/reboot.h>

#include <a.out.h>
#include <setjmp.h>
#include <stand/saio.h>

/*
 * Boot program... arguments from lower-level bootstrap determine
 * whether boot stops to ask for system name and which device
 * boot comes from.
 */

char line[100] = UNIX;
extern	int opendev, bootdev, cyloffset;
int	retry = 0;
extern jmp_buf  exception;

main(howto, dev, off)
{
	int io;

	if((dev&B_MAGICMASK) == B_DEVMAGIC) {
		bootdev = dev;
		cyloffset = off;
	} else	goto again;

	if(_setjmp(exception)) {
		close(io);
		printf("- load aborted\n");
again:
		howto = RB_SINGLE|RB_ASKNAME;
		cyloffset = 0; 
	}
		
	for (;;) {
		if (howto & RB_ASKNAME) {
			char *cp;

			printf("Boot: ");
			gets(line);

			/* process additional flags if any */
			if(cp = (char *)index(line, ' ')) {
				howto = strtol (cp, 0, 0);
				*cp = '\0';
			}
			cyloffset = 0;
		} else
			printf("Boot: %s\n", line);

		if (line[0] == 0) {
			strcpy(line, UNIX);
			printf("Boot: %s\n", line);
		}

		io = open(line, 0);
		if (io >= 0) {
			copyunix(io, howto);
			goto again;
		} else if (++retry > 2)
			goto again;
	}
}

/*ARGSUSED*/
copyunix(io, howto)
	register io;
{
	struct exec x;
	int i;
	char *addr,c;

	i = read(io, (char *)&x, sizeof x);
	if (i != sizeof x ||
	    (x.a_magic != 0407 && x.a_magic != 0413 && x.a_magic != 0410)) {
		printf("Bad format\n");
		return;
	}

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

	/* mask high order bits corresponding to relocated system base */
	x.a_entry &= 0x000fffff;
	printf(" start 0x%x\n", x.a_entry);

	if(c=scankbd())
		_longjmp(&exception,1);

	i = (*((int (*)()) x.a_entry))(howto, opendev, 0, cyloffset);

	if (i) printf("exit %d\n", i) ; 
	return;
shread:
	printf("Short read\n");
	return;
}
