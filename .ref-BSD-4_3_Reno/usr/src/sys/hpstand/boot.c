/*
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)boot.c	7.1 (Berkeley) 5/8/90
 */

#include <a.out.h>
#include "saio.h"
#include "../sys/reboot.h"

#ifndef INSECURE
#include "../sys/stat.h"
struct stat sb;
#endif

#define B_MAKEDEV(a,u,p,t) \
	(((a) << B_ADAPTORSHIFT) | ((u) << B_UNITSHIFT) | \
	 ((p) << B_PARTITIONSHIFT) | ((t) << B_TYPESHIFT))

/*
 * Boot program... arguments in `devtype' and `howto' determine
 * whether boot stops to ask for system name and which device
 * boot comes from.
 */

/* Types in `devtype' specifying major device */
char	devname[][2] = {
	0,0,		/* 0 = ct */
	0,0,		/* 1 = fd */
	'r','d',	/* 2 = rd */
	0,0,		/* 3 = sw */
	's','d',	/* 4 = sd */
};
#define	MAXTYPE	(sizeof(devname) / sizeof(devname[0]))

#define	UNIX	"vmunix"
char line[100];

int	retry = 0;
extern	char *lowram;
extern	int noconsole;
extern	int howto, devtype;

#define	MSUS (0xfffffedc)

char rom2mdev[] = {
	0,	/*  0 - none */
	0,	/*  1 - none */
	0,	/*  2 - none */
	0,	/*  3 - none */
	0,	/*  4 - none */
	0,	/*  5 - none */
	0,	/*  6 - none */
	0,	/*  7 - none */
	0,	/*  8 - none */
	0,	/*  9 - none */
	0,	/* 10 - none */
	0,	/* 11 - none */
	0,	/* 12 - none */
	0,	/* 13 - none */
	4,	/* 14 - SCSI disk */
	0,	/* 15 - none */
	2,	/* 16 - CS/80 device on HPIB */
	2,	/* 17 - CS/80 device on HPIB */
	0,	/* 18 - none */
	0,	/* 19 - none */
	0,	/* 20 - none */
	0,	/* 21 - none */
	0,	/* 22 - none */
	0,	/* 23 - none */
	0,	/* 24 - none */
	0,	/* 25 - none */
	0,	/* 26 - none */
	0,	/* 27 - none */
	0,	/* 28 - none */
	0,	/* 29 - none */
	0,	/* 30 - none */
	0,	/* 31 - none */
};

main()
{
	register type, part, unit, io;
	register char *cp;

	printf("\nBoot\n");
#ifdef JUSTASK
	howto = RB_ASKNAME|RB_SINGLE;
#else
	type = (devtype >> B_TYPESHIFT) & B_TYPEMASK;
	unit = (devtype >> B_UNITSHIFT) & B_UNITMASK;
	unit += 8 * ((devtype >> B_ADAPTORSHIFT) & B_ADAPTORMASK);
	part = (devtype >> B_PARTITIONSHIFT) & B_PARTITIONMASK;
	if ((howto & RB_ASKNAME) == 0) {
		if ((devtype & B_MAGICMASK) != B_DEVMAGIC) {
			/*
			 * we have to map the ROM device type codes
			 * to Unix major device numbers.
			 */
			type = rom2mdev[*(char *)MSUS & 0x1f];
			devtype = (devtype &~ (B_TYPEMASK << B_TYPESHIFT))
				  | (type << B_TYPESHIFT);
		}
		if (type >= 0 && type <= MAXTYPE && devname[type][0]) {
			cp = line;
			*cp++ = devname[type][0];
			*cp++ = devname[type][1];
			*cp++ = '(';
			if (unit >= 10)
				*cp++ = unit / 10 + '0';
			*cp++ = unit % 10 + '0';
			*cp++ = ',';
			*cp++ = part + '0';
			*cp++ = ')';
			strcpy(cp, UNIX);
		} else
			howto = RB_SINGLE|RB_ASKNAME;
	}
#endif
	for (;;) {
		if (!noconsole && (howto & RB_ASKNAME)) {
			printf(": ");
			gets(line);
		} else
			printf(": %s\n", line);
		io = open(line, 0);
		if (io >= 0) {
#ifndef INSECURE
			(void) fstat(io, &sb);
			if (sb.st_uid || (sb.st_mode & 2)) {
				printf("non-secure file, will not load\n");
				howto = RB_SINGLE|RB_ASKNAME;
				continue;
			}
#endif
			if (howto & RB_ASKNAME) {
				/*
				 * Build up devtype register to pass on to
				 * booted program.
				 */ 
				cp = line;
				for (type = 0; type <= MAXTYPE; type++)
					if ((devname[type][0] == cp[0]) && 
					    (devname[type][1] == cp[1]))
					    	break;
				if (type <= MAXTYPE) {
					cp += 3;
					unit = *cp++ - '0';
					if (*cp >= '0' && *cp <= '9')
						unit = unit * 10 + *cp++ - '0';
					cp++;
					part = atol(cp);
					devtype = B_MAKEDEV(unit >> 3, unit & 7, part, type);
				}
			}
			devtype |= B_DEVMAGIC;
			copyunix(howto, devtype, io);
			close(io);
			howto = RB_SINGLE|RB_ASKNAME;
		}
	bad:
		if (++retry > 2)
			howto = RB_SINGLE|RB_ASKNAME;
	}
}

/*ARGSUSED*/
copyunix(howto, devtype, io)
	register howto;		/* d7 contains boot flags */
	register devtype;	/* d6 contains boot device */
	register io;
{
	struct exec x;
	register int i;
	register char *load;	/* a5 contains load addr for unix */
	register char *addr;

	i = read(io, (char *)&x, sizeof x);
	if (i != sizeof x ||
	    (x.a_magic != 0407 && x.a_magic != 0413 && x.a_magic != 0410))
		_stop("Bad format\n");
	printf("%d", x.a_text);
	if (x.a_magic == 0413 && lseek(io, 0x400, 0) == -1)
		goto shread;
	load = addr = lowram;
	if (read(io, (char *)addr, x.a_text) != x.a_text)
		goto shread;
	addr += x.a_text;
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
	x.a_entry += (int)lowram;
	printf(" start 0x%x\n", x.a_entry);
#ifdef __GNUC__
	asm("	movl %0,d7" : : "m" (howto));
	asm("	movl %0,d6" : : "m" (devtype));
	asm("	movl %0,a5" : : "a" (load));
#endif
	(*((int (*)()) x.a_entry))();
	exit();
shread:
	_stop("Short read\n");
}
