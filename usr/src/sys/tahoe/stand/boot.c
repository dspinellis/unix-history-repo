/*	boot.c	1.1	86/01/12	*/
/*	boot.c	6.1	83/07/29	*/

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

/*	r11 = 0 -> automatic boot, load file '/vmunix' */
/*	r11 = 1 -> ask user for file to load */

/* Types in r10 specifying major device */
char	devname[][3] = {
	'f','s','d',	/* 0 = fsd */
	's','m','d',	/* 1 = smd or cmd */
	'x','f','d',	/* 2 = xfd */
	'x','s','d',	/* 2 = xsd */
	'c','y','p',	/* 3 = cypher tape */
};

#ifdef FSD
char line[100] = "fsd(0,0)vmunix";
#endif
#ifdef SMD
char line[100] = "smd(0,0)vmunix";
#endif
#ifdef XFD
char line[100] = "xfd(0,0)vmunix";
#endif
#ifdef XSD
char line[100] = "xsd(0,0)vmunix";
#endif
#ifdef JUSTASK
char line[100];
#endif

int	retry = 0;

main()
{
	register dummy;		/* skip r12 */
	register howto, devtype;	/* howto=r11, devtype=r10 */
	int io;

#ifdef lint
	howto = 0; devtype = 0;
#endif
	printf("\nBoot\n");
#ifdef JUSTASK
	howto = RB_ASKNAME|RB_SINGLE;
#endif
	for (;;) {
		if (howto & RB_ASKNAME) {
			printf(": ");
			gets(line);
		} else
			printf(": %s\n", line);
		io = open(line, 0);
		if (io >= 0)
			copyunix(howto, io);
		if (++retry > 2)
			howto |= RB_SINGLE|RB_ASKNAME;
	}
}

/*ARGSUSED*/
copyunix(howto, io)
	register io, howto;
{
	struct exec x;
	register int i;
	char *addr;

	i = read(io, (char *)&x, sizeof x);
	if (i != sizeof x ||
	    (x.a_magic != 0407 && x.a_magic != 0413 && x.a_magic != 0410))
		_stop("Bad format\n");
	printf("%d", x.a_text);
	if (x.a_magic == 0413 && lseek(io, 0x400, 0) == -1)
		goto shread;
	if (read(io, (char *)0x800, x.a_text) != x.a_text)
		goto shread;
	addr = (char *)(x.a_text + 0x800);
	if (x.a_magic == 0413 || x.a_magic == 0410)
		while ((int)addr & CLOFSET)
			*addr++ = 0;
	printf("+%d", x.a_data);
	if (read(io, addr, x.a_data) != x.a_data)
		goto shread;
	addr += x.a_data;
	printf("+%d", x.a_bss);
	x.a_bss += 32*1024;	/* slop */
	for (i = 0; i < x.a_bss; i++)
		*addr++ = 0;
	x.a_entry &= 0x1fffffff;
	printf(" start 0x%x\n", x.a_entry);
	mtpr(PADC, 0);		/* Purge data cache */
	mtpr(PACC, 0);		/* Purge code cache */
	if ((howto & RB_DCOFF) == 0) 
		mtpr(DCR, 1);	/* Enable data cache */
	(*((int (*)()) x.a_entry))();
shread:
	_stop("Short read\n");
}
