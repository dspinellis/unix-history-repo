#
/*
 * px - interpreter for UNIX Pascal
 * Version 1.0 August 1977
 *
 * Bill Joy, Charles Haley, Ken Thompson
 */

#include "0x.h"
#include "whoami"
#include "opcode.h"
#include "E.h"

int	display[20]	{ display };

int	onintr();

main(ac, av)
	int ac;
	char *av[];
{
	register char *cp;
	register int of;
	int size, *bp, i;

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
	else
		file = *argv;
	cp = file;
	of = open(cp, 0);
	if (discard)
		unlink(cp);
	if ((i & 01) == 0)
		signal(2, onintr);
	if (of < 0) {
oops:
		perror(cp);
		exit(1);
	}
	fstat(of, display);
	size = display[5];
	if (size == 0) {
		ferror("File is empty");
		exit(1);
	}
	cp = alloc(size);
	if (cp == -1) {
		ferror("Too large");
		exit(1);
	}
	if (read(of, cp, size) != size)
		goto oops;
	bp = cp;
	if (*bp++ != 0404) {
		ferror("Not a Pascal object file");
		exit(1);
	}
	close(of);
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
