/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)bootxx.c	7.5 (Berkeley) 4/4/90
 */

#include "param.h"
#include "vm.h"
#include "reboot.h"
#include <a.out.h>
#include "saio.h"

char bootprog[] = "boot";
extern	unsigned opendev;

/*
 * Boot program... arguments passed in r10 and r11
 * are passed through to the full boot program.
 */

main()
{
	register unsigned howto, devtype;	/* howto=r11, devtype=r10 */
	int io, unit, partition;
	register char *cp;

#ifdef lint
	howto = 0; devtype = 0; devtype = devtype;
#endif
	printf("loading %s\n", bootprog);
	io = open(bootprog, 0);
	if (io >= 0)
		copyunix(howto, opendev, io);
	_stop("boot failed\n");
}

/*ARGSUSED*/
copyunix(howto, devtype, io)
	register howto, devtype, io;	/* howto=r11, devtype=r10 */
{
	struct exec x;
	register int i;
	char *addr;

	i = read(io, (char *)&x, sizeof x);
	if (i != sizeof x || N_BADMAG(x))
		_stop("Bad format\n");
	if ((x.a_magic == ZMAGIC || x.a_magic == NMAGIC) &&
	    lseek(io, 0x400, L_SET) == -1)
		goto shread;
	if (read(io, (char *)0, x.a_text) != x.a_text)
		goto shread;
	addr = (char *)x.a_text;
	if (x.a_magic == ZMAGIC || x.a_magic == NMAGIC)
		while ((int)addr & CLOFSET)
			*addr++ = 0;
	if (read(io, addr, x.a_data) != x.a_data)
		goto shread;
	addr += x.a_data;
	x.a_bss += 128*512;	/* slop */
	for (i = 0; i < x.a_bss; i++)
		*addr++ = 0;
	x.a_entry &= 0x7fffffff;
	(*((int (*)()) x.a_entry))();
	return;
shread:
	_stop("Short read\n");
}
