/* (c) 1979 Regents of the University of California */
#
/*
 * px - interpreter for Berkeley Pascal
 * Version 1.0 August 1977
 *
 * Bill Joy, Charles Haley, Ken Thompson
 */

#include "0x.h"
#include "opcode.h"
#include "E.h"

int	display[20]	{ display };

int	onintr();

main(ac, av)
	int ac;
	char *av[];
{
	register char *cp;
	register int bytes, rmdr;
	int size, *bp, i, of;

	i = signal(2, 1);
	argc = ac - 1, argv = av + 1;
	randim = 1./randm;
	setmem();
	if (av[0][0] == '-' && av[0][1] == 'o') {
		av[0] =+ 2;
		file = av[0];
		argv--, argc++;
		discard++;
	} else if (argc == 0)
		file = *--argv = "obj", argc++;
	else if (argv[0][0] == '-' && argv[0][1] == 0) {
		argv[0][0] = 0;
		file = 0;
		argv[0] = argv[-1];
	} else
		file = *argv;
	if (file) {
		cp = file;
		of = open(cp, 0);
		if (discard)
			unlink(cp);
	} else
		of = 3;
	if ((i & 01) == 0)
		signal(2, onintr);
	if (of < 0) {
oops:
		perror(cp);
		exit(1);
	}
	if (file) {
		fstat(of, display);
		size = display[5];
	} else
		if (read(of, &size, 2) != 2) {
			ferror("Improper argument");
			exit(1);
		}
	if (size == 0) {
		ferror("File is empty");
		exit(1);
	}
	if (file) {
		read(of, &i, 2);
		if (i == 0407) {
			size =- 1024;
			seek(of, 1024, 0);
		} else
			seek(of, 0, 0);
	}
	bp = cp = alloc(size);
	if (cp == -1) {
		ferror("Too large");
		exit(1);
	}
	rmdr = size;
	while (rmdr != 0) {
		i = (rmdr > 0 && rmdr < 512) ? rmdr : 512;
		bytes = read(of, cp, i);
		if (bytes <= 0) {
			ferror("Unexpected end-of-file");
			exit(1);
		}
		rmdr =- bytes;
		cp =+ bytes;
	}
	if (read(of, cp, 1) == 1) {
		ferror("Expected end-of-file");
		exit(1);
	}
	close(of);
	if (file == 0)
		wait(&i);
	if (*bp++ != 0404) {
		ferror("Not a Pascal object file");
		exit(1);
	}
	if (discard && bp[(bp[0] == O_PXPBUF ? bp[5] + 8 : bp[1]) / 2 + 1] != O_NODUMP)
		write(2, "Execution begins...\n", 20);
	interpret(bp, size);
}

Perror(file, mesg)
	char *file, *mesg;
{
	extern int errno;
	extern char *sys_errlist[];

	errno = 0;
	sys_errlist[0] = mesg;
	perror(file);
}

/*
 * Initialization of random number "constants"
 */
long	seed	7774755.;
double	randa	62605.;
double	randc	113218009.;
double	randm	536870912.;

/*
 * Routine to put a string on the current
 * pascal output given a pointer to the string
 */
puts(str)
	char *str;
{
	register char *cp;

	cp = str;
	while (*cp)
		pputch(*cp++);
}

ferror(cp)
	char *cp;
{

	Perror(file, cp);
}

onintr()
{
	extern int draino[];

	if (dp == 0)
		exit(1);
	draino[0] = 512;
	draino[1] = &draino[2];
	error(EINTR);
}
