/*	boot.c	4.4	81/03/22	*/

#include "../h/param.h"
#include "../h/ino.h"
#include "../h/inode.h"
#include "../h/filsys.h"
#include "../h/dir.h"
#include "../h/vm.h"
#include <a.out.h>
#include "saio.h"
#include <sys/reboot.h>

/*
 * Boot program... arguments passed in r10 and r11 determine
 * whether boot stops to ask for system name and which device
 * boot comes from.
 */

/* Types in r10 specifying major device */
char	devname[][2] = {
	'h','p',	/* 0 = hp */
	0,0,		/* 1 = ht */
	'u','p',	/* 2 = up */
	'h','k',	/* 3 = hk */
};

char line[100] = "xx(0,0)vmunix";

int	retry = 0;

main()
{
	register howto, devtype;	/* howto=r11, devtype=r10 */
	int io;

#ifdef lint
	howto = 0; devtype = 0;
#endif
	printf("\nBoot\n");
#ifdef JUSTASK
	howto = RB_ASKNAME|RB_SINGLE;
#else
	if ((howto&RB_ASKNAME)==0) {
		if (devtype>=0 && devtype<sizeof(devname)/2
		    && devname[devtype][0]) {
			line[0] = devname[devtype][0];
			line[1] = devname[devtype][1];
		} else
			howto = RB_SINGLE|RB_ASKNAME;
	}
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
			howto = RB_SINGLE|RB_ASKNAME;
	}
}

/*ARGSUSED*/
copyunix(howto, io)
	register howto, io;
{
	struct exec x;
	register int i;
	char *addr;

	i = read(io, (char *)&x, sizeof x);
	if (i != sizeof x || x.a_magic != 0410)
		_stop("Bad format\n");
	printf("%d", x.a_text);
	if (read(io, (char *)0, x.a_text) != x.a_text)
		goto shread;
	addr = (char *)x.a_text;
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
	x.a_entry &= 0x7fffffff;
	printf(" start 0x%x\n", x.a_entry);
	(*((int (*)()) x.a_entry))();
	_exit();
shread:
	_stop("Short read\n");
}
