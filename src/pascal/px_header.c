/*
 * pxheader - program to sit in front of interpreter code to make shell mods
 *	      unnecessary to make Pascal obj's look like real programs.
 *
 * This program lives in /usr/lib/px_header
 * Bill Joy UCB February 6, 1978
 */

struct {
	int pint;
};
char	BINPX[]		"/usr/ucb/px";		/* A little strange ... */
char	USRBINPX[]	"/usr/bin/px";

extern	errno;

#define	ETXTBSY	26

main(argc, argv)
	register int argc;
	register char *argv[];
{
	register int i, j, *ip;
	int largv[512], pv[2];

	if (argc > 510) {
		error("Too many arguments.\n");
		exit(1);
	}
	largv[0] = argv[0];
	largv[1] = "-";
	for (i = 1; i < argc; i++)
		largv[i + 1] = argv[i];
	largv[argc + 1] = 0;
	pipe(pv);
	i = fork();
	if (i == -1)
		error("Try again.\n");
	if (i == 0) {
		close(pv[0]);
		write(pv[1], 1006, 2);
		ip = 1006;
		i = ip->pint;
		ip++;
		while (i != 0) {
			j = (i > 0 && i < 512) ? i : 512;
			write(pv[1], ip, j);
			ip =+ 512 / sizeof (int);
			i =- j;
		}
		exit(1);
	}
	close(pv[1]);
	if (pv[0] != 3) {
		close(3);
		dup(pv[0]);
		close(pv[0]);
	}
	for (;;) {
		execv(BINPX, largv);
		if (errno != ETXTBSY)
			break;
		sleep(2);
	}
	for (;;) {
		execv(USRBINPX, largv);
		if (errno != ETXTBSY)
			break;
		sleep(2);
	}
	error("Px not found.\n");
}

error(cp)
	register char *cp;
{
	register int i;
	register char *dp;

	dp = cp;
	i = 0;
	while (*dp++)
		i++;
	write(2, cp, i);
	exit (1);
}
