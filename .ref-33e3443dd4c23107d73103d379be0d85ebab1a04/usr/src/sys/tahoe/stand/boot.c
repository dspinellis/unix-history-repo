/*	boot.c	1.5	86/12/18	*/

#include "../machine/mtpr.h"

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

/* Types in r10 specifying major device */
char	devname[][2] = {
	0, 0,		/* 0 = ud */
	'd','k',	/* 1 = vd/dk */
	0, 0,		/* 2 = xp */
	'c','y',	/* 3 = cy */
};
#define	MAXTYPE	(sizeof(devname) / sizeof(devname[0]))
#define	DEV_DFLT	1		/* vd/dk */

#define	UNIX	"vmunix"
char line[100];

int	retry = 0;

main()
{
	register dummy;		/* skip r12 */
	register howto, devtype;	/* howto=r11, devtype=r10 */
	int io, i;
	register type, part, unit;
	register char *cp;
	long atol();


#ifdef lint
	howto = 0; devtype = 0;
#endif
#ifdef JUSTASK
	howto = RB_ASKNAME|RB_SINGLE;
#else
	if ((devtype & B_MAGICMASK) != B_DEVMAGIC)
		devtype = DEV_DFLT << B_TYPESHIFT;	/* unit, partition 0 */
	type = (devtype >> B_TYPESHIFT) & B_TYPEMASK;
	unit = (devtype >> B_UNITSHIFT) & B_UNITMASK;
	unit += 8 * ((devtype >> B_ADAPTORSHIFT) & B_ADAPTORMASK);
	part = (devtype >> B_PARTITIONSHIFT) & B_PARTITIONMASK;
	if ((howto & RB_ASKNAME) == 0) {
		if (type >= 0 && type <= MAXTYPE && devname[type][0]) {
			cp = line;
			*cp++ = devname[type][0];
			*cp++ = devname[type][1];
			*cp++ = '(';
			if (unit >= 10)
				*cp++ = unit / 10 + '0';
			*cp++ = unit % 10 + '0';
			*cp++ = ',';
			if (part >= 10)
				*cp++ = part / 10 + '0';
			*cp++ = part % 10 + '0';
			*cp++ = ')';
			strcpy(cp, UNIX);
		} else
			howto = RB_SINGLE|RB_ASKNAME;
	}
#endif
	for (;;) {
		printf("\nBoot\n");
		if (howto & RB_ASKNAME) {
			printf(": ");
			gets(line);
		} else
			printf(": %s\n", line);
		io = open(line, 0);
		if (io >= 0) {
			if (howto & RB_ASKNAME) {
				/*
				 * Build up devtype register to pass on to
				 * booted program.
				 */ 
				cp = line;
				for (i = 0; i <= MAXTYPE; i++)
					if ((devname[i][0] == cp[0]) && 
					    (devname[i][1] == cp[1]))
					    	break;
				if (i <= MAXTYPE) {
					devtype = i << B_TYPESHIFT;
					cp += 3;
					i = *cp++ - '0';
					if (*cp >= '0' && *cp <= '9')
						i = i * 10 + *cp++ - '0';
					cp++;
					devtype |= ((i % 8) << B_UNITSHIFT);
					devtype |= ((i / 8) << B_ADAPTORSHIFT);
					devtype |= atol(cp) << B_PARTITIONSHIFT;
				}
			}
			devtype |= B_DEVMAGIC;
			copyunix(howto, devtype, io);
			close(io);
			howto = RB_SINGLE|RB_ASKNAME;
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

	i = read(io, (char *)&x, sizeof x);
	if (i != sizeof x ||
	    (x.a_magic != 0407 && x.a_magic != 0413 && x.a_magic != 0410))
		_stop("Bad format\n");
	printf("%d", x.a_text);
	if (x.a_magic == 0413 && lseek(io, 0x400, 0) == -1)
		goto shread;
	if (read(io, (char *)RELOC, x.a_text) != x.a_text)
		goto shread;
	addr = (char *)(x.a_text + RELOC);
	if (x.a_magic == 0413 || x.a_magic == 0410)
		while ((int)addr & CLOFSET)
			*addr++ = 0;
	printf("+%d", x.a_data);
	if (read(io, addr, x.a_data) != x.a_data)
		goto shread;
	addr += x.a_data;
	printf("+%d", x.a_bss);
	if (howto & RB_KDB && x.a_syms) {
		for (i = 0; i < x.a_bss; i++)
			*addr++ = 0;
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
	x.a_bss += 32*1024;	/* slop */
	for (i = 0; i < x.a_bss; i++)
		*addr++ = 0;
	x.a_entry &= 0x1fffffff;
	printf(" start 0x%x\n", x.a_entry);
	mtpr(PADC, 0);		/* Purge data cache */
	mtpr(PACC, 0);		/* Purge code cache */
	mtpr(DCR, 1);		/* Enable data cache */
	(*((int (*)()) x.a_entry))();
	return;
shread:
	_stop("Short read\n");
}
