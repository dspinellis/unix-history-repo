#include <pagsiz.h>
#define	BUFSIZ	BSIZE
/*
 * pxheader - program to sit in front of interpreter code to make shell mods
 *	      unnecessary to make Pascal obj's look like real programs.
 *
 * This program lives in /usr/lib/px_header
 * Bill Joy UCB February 6, 1978
 */

char	USRUCBPX[] = "/usr/ucb/px";

extern	errno;

#define	ETXTBSY	26


struct header 
    {
	int		magic;
	unsigned	txt_size;
	unsigned	data_size;
	unsigned	bss_size;
	unsigned	syms_size;
	unsigned	entry_point;
	unsigned	tr_size;
	unsigned	dr_size;
    };

#define	HEADER_BYTES	1024
#define	ADDR_LC		HEADER_BYTES - sizeof (struct header) - sizeof (int)

main(argc, argv)
	register int argc;
	register char *argv[];
{
	register int i, j;
	register unsigned short *ip;
	char *largv[512];
	int pv[2];

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
		write(pv[1], ADDR_LC, sizeof ( int ));
		ip = (unsigned short *) (ADDR_LC);
		i = *(int *)ip; ip += 2;
		while (i != 0) {
			j = (i > 0 && i < BUFSIZ) ? i : BUFSIZ;
			write(pv[1], ip, j);
			ip += BUFSIZ / sizeof ( unsigned short );
			i -= j;
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
		execv(USRUCBPX, largv);
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
	exit(1);
}

exit(i)
{
	_exit(i);
}
