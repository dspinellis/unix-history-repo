/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)boot.c	7.11 (Berkeley) %G%
 */

#include "param.h"
#include "inode.h"
#include "fs.h"
#include "vm.h"
#include "reboot.h"

#include <a.out.h>
#include "saio.h"

/*
 * Boot program... arguments passed in r10 and r11 determine
 * whether boot stops to ask for system name and which device
 * boot comes from.
 */

char line[100];

extern	unsigned opendev;

main()
{
	register unsigned howto, devtype;	/* howto=r11, devtype=r10 */
	int io, retry, type;

#ifdef lint
	howto = 0; devtype = 0;
#endif
	printf("\nBoot\n");
#ifdef JUSTASK
	howto = RB_ASKNAME|RB_SINGLE;
#else
	if ((howto & RB_ASKNAME) == 0) {
		type = (devtype >> B_TYPESHIFT) & B_TYPEMASK;
		if ((unsigned)type < ndevs && devsw[type].dv_name)
			strcpy(line, UNIX);
		else
			howto |= RB_SINGLE|RB_ASKNAME;
	}
#endif
	for (retry = 0;;) {
		if (howto & RB_ASKNAME) {
			printf(": ");
			gets(line);
			if (line[0] == 0) {
				strcpy(line, UNIX);
				printf(": %s\n", line);
			}
		} else
			printf(": %s\n", line);
		io = open(line, 0);
		if (io >= 0) {
#ifdef VAX750
			loadpcs();
#endif
			copyunix(howto, opendev, io);
			close(io);
			howto |= RB_SINGLE|RB_ASKNAME;
		}
		if (++retry > 2)
			howto |= RB_SINGLE|RB_ASKNAME;
	}
}

/*ARGSUSED*/
copyunix(howto, devtype, aio)
	register howto, devtype;	/* howto=r11, devtype=r10 */
	int aio;
{
	register int esym;		/* must be r9 */
	struct exec x;
	register int io = aio, i;
	char *addr;

	if (read(io, (char *)&x, sizeof(x)) != sizeof(x) || N_BADMAG(x)) {
		printf("bad magic #\n");
		return;
	}
	printf("%d", x.a_text);
	if (x.a_magic == ZMAGIC && lseek(io, 0x400, L_SET) == -1)
		goto shread;
	if (read(io, (char *)0, x.a_text) != x.a_text)
		goto shread;
	addr = (char *)x.a_text;
	if (x.a_magic == ZMAGIC || x.a_magic == NMAGIC)
		while ((int)addr & CLOFSET)
			*addr++ = 0;
	printf("+%d", x.a_data);
	if (read(io, addr, x.a_data) != x.a_data)
		goto shread;
	addr += x.a_data;
	printf("+%d", x.a_bss);
	for (i = 0; i < x.a_bss; i++)
		*addr++ = 0;
	if (howto & RB_KDB && x.a_syms) {
		*(int *)addr = x.a_syms;		/* symbol table size */
		addr += sizeof (int);
		printf("[+%d", x.a_syms);
		if (read(io, addr, x.a_syms) != x.a_syms)
			goto shread;
		addr += x.a_syms;
		if (read(io, addr, sizeof (int)) != sizeof (int))
			goto shread;
		i = *(int *)addr - sizeof (int);	/* string table size */
		addr += sizeof (int);
		printf("+%d]", i);
		if (read(io, addr, i) != i)
			goto shread;
		addr += i;
		esym = roundup((int)addr, sizeof (int));
		x.a_bss = 0;
	} else
		howto &= ~RB_KDB;
	for (i = 0; i < 128*512; i++)	/* slop */
		*addr++ = 0;
	x.a_entry &= 0x7fffffff;
	printf(" start 0x%x\n", x.a_entry);
	(*((int (*)()) x.a_entry))();
	return;
shread:
	printf("Short read\n");
	return;
}

#ifdef VAX750
/* 750 Patchable Control Store magic */

#include "../vax/mtpr.h"
#include "../vax/cpu.h"
#define	PCS_BITCNT	0x2000		/* number of patchbits */
#define	PCS_MICRONUM	0x400		/* number of ucode locs */
#define	PCS_PATCHADDR	0xf00000	/* start addr of patchbits */
#define	PCS_PCSADDR	(PCS_PATCHADDR+0x8000)	/* start addr of pcs */
#define	PCS_PATCHBIT	(PCS_PATCHADDR+0xc000)	/* patchbits enable reg */
#define	PCS_ENABLE	0xfff00000	/* enable bits for pcs */

loadpcs()
{
	register int *ip;	/* known to be r11 below */
	register int i;		/* known to be r10 below */
	register int *jp;	/* known to be r9 below */
	register int j;
	static int pcsdone = 0;
	union cpusid sid;
	char pcs[100];
	char *cp;

	sid.cpusid = mfpr(SID);
	if (sid.cpuany.cp_type!=VAX_750 || sid.cpu750.cp_urev<95 || pcsdone)
		return;
	printf("Updating 11/750 microcode: ");
	for (cp = line; *cp; cp++)
		if (*cp == ')' || *cp == ':')
			break;
	if (*cp) {
		strncpy(pcs, line, 99);
		pcs[99] = 0;
		i = cp - line + 1;
	} else
		i = 0;
	strcpy(pcs + i, "pcs750.bin");
	i = open(pcs, 0);
	if (i < 0)
		return;
	/*
	 * We ask for more than we need to be sure we get only what we expect.
	 * After read:
	 *	locs 0 - 1023	packed patchbits
	 *	 1024 - 11264	packed microcode
	 */
	if (read(i, (char *)0, 23*512) != 22*512) {
		printf("Error reading %s\n", pcs);
		close(i);
		return;
	}
	close(i);

	/*
	 * Enable patchbit loading and load the bits one at a time.
	 */
	*((int *)PCS_PATCHBIT) = 1;
	ip = (int *)PCS_PATCHADDR;
	jp = (int *)0;
	for (i=0; i < PCS_BITCNT; i++) {
		asm("	extzv	r10,$1,(r9),(r11)+");
	}
	*((int *)PCS_PATCHBIT) = 0;

	/*
	 * Load PCS microcode 20 bits at a time.
	 */
	ip = (int *)PCS_PCSADDR;
	jp = (int *)1024;
	for (i=j=0; j < PCS_MICRONUM * 4; i+=20, j++) {
		asm("	extzv	r10,$20,(r9),(r11)+");
	}

	/*
	 * Enable PCS.
	 */
	i = *jp;		/* get 1st 20 bits of microcode again */
	i &= 0xfffff;
	i |= PCS_ENABLE;	/* reload these bits with PCS enable set */
	*((int *)PCS_PCSADDR) = i;

	sid.cpusid = mfpr(SID);
	printf("new rev level=%d\n", sid.cpu750.cp_urev);
	pcsdone = 1;
}
#endif
