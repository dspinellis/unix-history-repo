/*	boot.c	7.2	89/05/24	*/

#include "machine/mtpr.h"

#include "param.h"
#include "inode.h"
#include "fs.h"
#include "vm.h"
#include "saio.h"
#include "reboot.h"

#include <a.out.h>

/*
 * Boot program... arguments passed in r10 and r11 determine
 * whether boot stops to ask for system name and which device
 * boot comes from.
 */

#define	DEV_DFLT	1		/* vd/dk */
/*#define	DEV_DFLT	2		/* hd */

char line[100];

extern	unsigned opendev;
extern	unsigned bootdev;

main()
{
	register char *cp;		/* skip r12 */
	register u_int howto, devtype;	/* howto=r11, devtype=r10 */
	int io = 0, retry, type;

#ifdef lint
	howto = 0; devtype = 0;
#endif
	if ((devtype & B_MAGICMASK) != B_DEVMAGIC)
		devtype = DEV_DFLT << B_TYPESHIFT;	/* unit, partition 0 */
	bootdev = devtype;
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
		if (io >= 0)
			printf("\nBoot");
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
			copyunix(howto, opendev, io);
			close(io);
			howto |= RB_SINGLE|RB_ASKNAME;
		}
		if (++retry > 2)
			howto |= RB_SINGLE|RB_ASKNAME;
	}
}

/*ARGSUSED*/
copyunix(howto, devtype, io)
	register io, howto, devtype;	/* NOTE ORDER */
{
	register int esym;		/* must be r9 */
	register int i;
	register char *addr;
	struct exec x;

	if (read(io, (char *)&x, sizeof(x)) != sizeof(x) || N_BADMAG(x)) {
		printf("bad magic #\n");
		return;
	}
	printf("%ld", x.a_text);
	if (x.a_magic == ZMAGIC && lseek(io, 0x400, 0) == -1)
		goto shread;
	if (read(io, (char *)RELOC, x.a_text) != x.a_text)
		goto shread;
	addr = (char *)(x.a_text + RELOC);
	if (x.a_magic == ZMAGIC || x.a_magic == NMAGIC)
		while ((int)addr & CLOFSET)
			*addr++ = 0;
	printf("+%ld", x.a_data);
	if (read(io, addr, x.a_data) != x.a_data)
		goto shread;
	addr += x.a_data;
	printf("+%ld", x.a_bss);
	if (howto & RB_KDB && x.a_syms) {
		for (i = 0; i < x.a_bss; i++)
			*addr++ = 0;
		*(int *)addr = x.a_syms;		/* symbol table size */
		addr += sizeof (int);
		printf("[+%ld", x.a_syms);
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
	x.a_bss += 32*1024;	/* slop */
	for (i = 0; i < x.a_bss; i++)
		*addr++ = 0;
	x.a_entry &= 0x1fffffff;
	printf(" start 0x%lx\n", x.a_entry);
	mtpr(PADC, 0);		/* Purge data cache */
	mtpr(PACC, 0);		/* Purge code cache */
	mtpr(DCR, 1);		/* Enable data cache */
	(*((int (*)()) x.a_entry))();
	return;
shread:
	printf("short read\n");
	return;
}
