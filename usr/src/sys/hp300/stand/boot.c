/*-
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)boot.c	7.7 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/reboot.h>
#include <a.out.h>
#include <stand/saio.h>

#ifndef INSECURE
#include <sys/stat.h>
struct stat sb;
#endif

#define	PRTCPU		/* print out cpu type */

/*
 * Boot program... bits in `howto' determine whether boot stops to
 * ask for system name.  Boot device is derived from ROM provided
 * information.
 */

char line[100];

extern	unsigned opendev;
extern	char *lowram;
extern	int noconsole;
extern	int howto, bootdev;

#ifdef PRTCPU
#include <hp300/stand/samachdep.h>
#endif

main()
{
	register char *cp;
	int io, retry, type;
#ifdef PRTCPU
	extern int machineid;

	printf("\nHP");
	switch (machineid) {
	case HP_320:
		cp = "320"; break;
	case HP_330:
		cp = "318/319/330"; break;
	case HP_340:
		cp = "340"; break;
	case HP_350:
		cp = "350"; break;
	case HP_360:
		cp = "360"; break;
	case HP_370:
		cp = "370"; break;
	case HP_375:
		cp = "345/375/400"; break;
	case HP_380:
		cp = "380/425"; break;
	case HP_433:
		cp = "433"; break;
	default:
		cp = "???"; break;
	}
	printf("%s CPU\nBoot\n", cp);
#else
	printf("\nBoot\n");
#endif
#ifdef JUSTASK
	howto = RB_ASKNAME|RB_SINGLE;
#else
	if ((howto & RB_ASKNAME) == 0) {
		type = (bootdev >> B_TYPESHIFT) & B_TYPEMASK;
		if ((unsigned)type < ndevs && devsw[type].dv_name)
			strcpy(line, UNIX);
		else
			howto |= RB_SINGLE|RB_ASKNAME;
	}
#endif
	for (retry = 0;;) {
		if (!noconsole && (howto & RB_ASKNAME)) {
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
#ifndef INSECURE
			(void) fstat(io, &sb);
			if (sb.st_uid || (sb.st_mode & 2)) {
				printf("non-secure file, will not load\n");
				close(io);
				howto = RB_SINGLE|RB_ASKNAME;
				continue;
			}
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
copyunix(howto, devtype, io)
	register int howto;	/* d7 contains boot flags */
	register u_int devtype;	/* d6 contains boot device */
	register int io;
{
	struct exec x;
	register int i;
	register char *load;	/* a5 contains load addr for unix */
	register char *addr;

	i = read(io, (char *)&x, sizeof(x));
	if (i != sizeof(x) ||
	    (x.a_magic != OMAGIC && x.a_magic != ZMAGIC && x.a_magic != NMAGIC)) {
		printf("Bad format\n");
		return;
	}
	printf("%d", x.a_text);
	if (x.a_magic == ZMAGIC && lseek(io, 0x400, L_SET) == -1)
		goto shread;
	load = addr = lowram;
	if (read(io, (char *)addr, x.a_text) != x.a_text)
		goto shread;
	addr += x.a_text;
	if (x.a_magic == ZMAGIC || x.a_magic == NMAGIC)
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
	return;
shread:
	printf("Short read\n");
	return;
}
