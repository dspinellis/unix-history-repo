#include <stdio.h>
#include <sgtty.h>

main(argc, argv)
	int argc;
	char **argv;
{
	register char *cp;
	struct sgttyb stb, stb2;
	int pendin = LPENDIN;

	ioctl(2, TIOCGETP, &stb);
	stb2 = stb;
	stb.sg_flags &= ~ECHO;
	ioctl(2, TIOCSETN, &stb);
	for (argc--, argv++; argc > 0; argc--, argv++) {
		for (cp = *argv; cp && *cp; cp++)
			ioctl(2, TIOCSTI, cp);
		if (argc > 1)
			ioctl(2, TIOCSTI, " ");
	}
	ioctl(2, TIOCSETN, &stb2);
	ioctl(2, TIOCLBIS, &pendin);
	exit(0);
}
