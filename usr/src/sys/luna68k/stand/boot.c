/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)boot.c	7.3 (Berkeley) %G%
 */

/*
 * boot.c -- boot program
 * by A.Fujita, MAR-01-1992
 */

#include <sys/param.h>
#include <sys/reboot.h>
#include <sys/exec.h>
#include <machine/stinger.h>
#include <luna68k/stand/saio.h>
#include <luna68k/stand/status.h>

extern struct KernInter	*kiff;

int howto;
int devtype = MAKEBOOTDEV(4, 0, 6, 0, 0);

char *copyunix();

struct exec header;
char default_file[] = "sd(0,0)vmunix";

char *how_to_info[] = {
"RB_ASKNAME	ask for file name to reboot from",
"RB_SINGLE	reboot to single user only",
"RB_NOSYNC	dont sync before reboot",
"RB_HALT	don't reboot, just halt",
"RB_INITNAME	name given for /etc/init (unused)",
"RB_DFLTROOT	use compiled-in rootdev",
"RB_KDB		give control to kernel debugger",
"RB_RDONLY	mount root fs read-only"
};

#define TAPE
#ifdef TAPE /* A.Kojima */
extern dev_t  rst0;
extern dev_t nrst0;
char *stcopyunix();
#endif

int
how_to_boot(argc, argv)
	int   argc;
	char *argv[];
{
	int i, h = howto;

	if (argc < 2) {
		printf("howto: 0x%s\n\n", hexstr(howto, 2));

		if (h == 0) {
			printf("\t%s\n", "RB_AUTOBOOT	flags for system auto-booting itself");
		} else {
			for (i = 0; i < 8; i++, h >>= 1) {
				if (h & 0x01) {
					printf("\t%s\n", how_to_info[i]);
				}
			}
		}

		printf("\n");
	}
}

int
get_boot_device(s)
	char *s;
{
	register int unit = 0;
	register int part = 0;
	register char *p = s;

	while (*p != '(') {
		if (*p == '\0')
			goto error;
		p++;
	}

	while (*++p != ',') {
		if (*p == '\0')
			goto error;
		if (*p >= '0' && *p <= '9')
			unit = (unit * 10) + (*p - '0');
	}

	while (*++p != ')') {
		if (*p == '\0')
			goto error;
		if (*p >= '0' && *p <= '9')
			part = (part * 10) + (*p - '0');
	}

	return(MAKEBOOTDEV(4, 0, (6 - unit), unit, part));

error:
	return(MAKEBOOTDEV(4, 0, 6, 0, 0));
}

int
boot(argc, argv)
	int   argc;
	char *argv[];
{
	register int io;
	char *line;

	if (argc < 2)
		line = default_file;
	else
		line = argv[1];

	devtype = get_boot_device(line);

	printf("Booting %s\n", line);

#ifdef TAPE /* A.Kojima */
	if (!strcmp("st", argv[1])) {
		io = argc < 3 ? 0 : *argv[2] - '0';
		printf("boot tape file number:%d\n", io);
		stbootunix(howto, devtype, io);
		return;
	}
#endif
	io = open(line, 0);
	if (io >= 0) {
		bootunix(howto, devtype, io);
		close(io);
	}
}

int
load(argc, argv)
	int   argc;
	char *argv[];
{
	register int io;
	char *line;

	if (argc < 2)
		line = default_file;
	else
		line = argv[1];

	printf("loading %s\n", line);

	io = open(line, 0);
	if (io >= 0) {
		copyunix(io);
		printf("\n");
		close(io);
	}
}

int
bootunix(howto, devtype, io)
	register howto;		/* d7 contains boot flags */
	register devtype;	/* d6 contains boot device */
	register io;
{
	register char *load;	/* a5 contains load addr for unix */

	load = copyunix(io);

	printf(" start 0x%x\n", load);
	asm("	movl %0,d7" : : "d" (howto));
	asm("	movl %0,d6" : : "d" (devtype));
	asm("	movl %0,a5" : : "a" (kiff));
	(*((int (*)()) load))();
}

char *
copyunix(io)
	register io;
{

	register int i;
	register char *load;	/* a5 contains load addr for unix */
	register char *addr;

	/*
	 * Read a.out file header
	 */

	i = read(io, (char *)&header, sizeof(struct exec));
	if (i != sizeof(struct exec) ||
	    (header.a_magic != 0407 && header.a_magic != 0413 && header.a_magic != 0410)) {
		printf("illegal magic number ... 0x%x\n");
		printf("Bad format\n");
		return(0);
	}

	load = addr = (char *) (header.a_entry & 0x00FFFFFF);

	printf("%d", header.a_text);
	if (header.a_magic == 0413 && lseek(io, 0x400, 0) == -1)
		goto shread;

	/*
	 * Load TEXT Segment
	 */

	if (read(io, (char *)addr, header.a_text) != header.a_text)
		goto shread;
	addr += header.a_text;
	if (header.a_magic == 0413 || header.a_magic == 0410)
		while ((int)addr & CLOFSET)
			*addr++ = 0;

	/*
	 * Load DATA Segment
	 */

	printf("+%d", header.a_data);
	if (read(io, addr, header.a_data) != header.a_data)
		goto shread;

	/*
	 * Clear BSS Segment
	 */

	addr += header.a_data;
	printf("+%d", header.a_bss);
	header.a_bss += 128*512;	/* slop */
	for (i = 0; i < header.a_bss; i++)
		*addr++ = 0;

	return(load);

shread:
	printf("   Short read\n");
	return(0);
}

#ifdef TAPE /* A.Kojima */
int
stbootunix(howto, devtype, skip)
	register howto;		/* d7 contains boot flags */
	register devtype;	/* d6 contains boot device */
	register skip;		/* tape skip */
{
	register int i;
	register char *load;	/* a5 contains load addr for unix */

	/*
	 * Tape rewind and skip
	 */
	st_rewind(rst0);
	for (i = 0; i < skip; i++) {
		st_skip(rst0);
	}

	load = stcopyunix();

	st_rewind(rst0);

	printf(" start 0x%x\n", load);
	asm("	movl %0,d7" : : "d" (howto));
	asm("	movl %0,d6" : : "d" (devtype));
	asm("	movl %0,a5" : : "a" (kiff));
	(*((int (*)()) load))();
}

char *
stcopyunix()
{

	register int i;
	register char *load;	/* a5 contains load addr for unix */
	register char *addr;
	u_char buf[0x400];

	/*
	 * Read a.out file header
	 */

	i = tread(/*io,*/ (char *)&header, sizeof(struct exec));
	if (i != sizeof(struct exec) ||
	    (header.a_magic != 0407 && header.a_magic != 0413 && header.a_magic != 0410)) {
		printf("illegal magic number ... 0x%x\n");
		printf("Bad format\n");
		return(0);
	}

	load = addr = (char *) (header.a_entry & 0x00FFFFFF);

	printf("%d", header.a_text);

	i = 0x400 - i;
	if (header.a_magic == 0413 && tread(buf, i) != i) { /* easy seek */
		goto shread;
	}

	/*
	 * Load TEXT Segment
	 */

	if (tread(/*io,*/ (char *)addr, header.a_text) != header.a_text)
		goto shread;
	addr += header.a_text;
	if (header.a_magic == 0413 || header.a_magic == 0410)
		while ((int)addr & CLOFSET)
			*addr++ = 0;

	/*
	 * Load DATA Segment
	 */

	printf("+%d", header.a_data);
	if (tread(/*io,*/ addr, header.a_data) != header.a_data)
		goto shread;

	/*
	 * Clear BSS Segment
	 */

	addr += header.a_data;
	printf("+%d", header.a_bss);
	header.a_bss += 128*512;	/* slop */
	for (i = 0; i < header.a_bss; i++)
		*addr++ = 0;

	return(load);

shread:
	printf("   Short read\n");
	return(0);
}

int
tread(addr, size)
	int	addr;
	int	size;
{
	static u_char	buf[512];
	static int	head = 512;
	static int	tail = 512;
	int             req_size = size;
	int		rest = tail - head;
	
	if (rest > 0) {
		if (size <= rest) {
			bcopy(&buf[head], addr, size);
			head += size;
			return size;
		} else { /* size > rest */
			bcopy(&buf[head], addr, rest);
			addr += rest;
			size -= rest;
			if (tail != 512) {
				head = 512;
				tail = 512;
				printf("tread() EOF 0\n");
				return rest;
			}
		}
	}

	/* head = 0; */

	while (size > 512) {
		if ((tail = stread(rst0, addr, 512)) == 512) {
			addr += 512;
			size -= 512;
		} else { /* eof ( tail < 512 ) */
			size -= tail;
			head = tail;
			printf("tread() EOF 1\n");
			return req_size - size;
		}
	}
	tail = stread(rst0, buf, 512);
	if (tail >= size) {
		bcopy(buf, addr, size);
		head = size;
		return req_size;
	} else {
		bcopy(buf, addr, tail);
		head = tail;
		printf("tread() EOF 2\n");
		return req_size - size;
	}
}

#endif

