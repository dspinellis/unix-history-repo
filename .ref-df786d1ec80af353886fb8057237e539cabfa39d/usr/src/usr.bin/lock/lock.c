#ifndef lint
static char *sccsid = "@(#)lock.c	4.3 (Berkeley) %G%";
#endif

/*
 * Lock a terminal up until the knowledgeable Joe returns.
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <sgtty.h>

char	masterp[] =	"hasta la vista\n";
struct	sgttyb tty, ntty;
char	s[BUFSIZ], s1[BUFSIZ];

main(argc, argv)
	char **argv;
{
	register int t;
	struct stat statb;

	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGTSTP, SIG_IGN);
	if (argc > 0)
		argv[0] = 0;
	if (ioctl(0, TIOCGETP, &tty))
		exit(1);
	ntty = tty; ntty.sg_flags &= ~ECHO;
	ioctl(0, TIOCSETN, &ntty);
	printf("Key: ");
	fgets(s, sizeof s, stdin);
	printf("\nAgain: ");
	fgets(s1, sizeof s1, stdin);
	putchar('\n');
	if (strcmp(s1, s)) {
		putchar(07);
		stty(0, &tty);
		exit(1);
	}
	s[0] = 0;
	for (;;) {
		fgets(s, sizeof s, stdin);
		if (strcmp(s1, s) == 0)
			break;
		if (strcmp(s, masterp) == 0)
			break;
		putchar(07);
		if (ioctl(0, TIOCGETP, &ntty))
			exit(1);
	}
	ioctl(0, TIOCSETN, &tty);
}
