#
/*
 * pix - pi then px
 *
 * Bill Joy UCB August 26, 1977
 */

#define	ERRS	1

char	*name;

int	onintr();

#define	ETXTBSY	26

main(argc, argv)
	int argc;
	char *argv[];
{
	register char **av;
	register int ac;
	int i, io, pid, status;
	extern errno;

	do
		io = open("/dev/null", 0);
	while (io >= 0 && io < 3);
	for (io = 3; io < 15; io++)
		close(io);
	if ((signal(2, 1) & 01) == 0)
		signal(2, onintr);
	for (ac = 1; ac < argc; ac++)
		if (dotted(argv[ac], 'p')) {
			ac++;
			break;
		}
	name = "-o/tmp/pixaXXXXX" + 2;
	mktemp(name);
	for (;;) {
		io = creat(name, 0400);
		if (io > 0)
			break;
		if (name[8] == 'z') {
			perror(name);
			exit(1);
		}
		name[8]++;
	}
	pid = fork();
	if (pid == -1) {
		write(2, "No more processes\n", 18);
		onintr();
	}
	if (pid == 0) {
		if (io != 3) {
			write(2, "Impossible error in pix\n", 24);
			onintr();
		}
		argv[ac] = 0;
		argv[0] = name - 2;
#ifdef CORY
		/* Temporary to allow accounting to distinguish pix's and pi's */
		do
			execv("/usr/bin/pix-pi", argv);
		while (errno == ETXTBSY);
#endif
		do
			execv("/usr/ucb/pi", argv);
		while (errno == ETXTBSY);
		do
			execv("/usr/bin/pi", argv);
		while (errno == ETXTBSY);
		write(2, "Can't find pi\n", 14);
		onintr();
	}
	close(io);
	do
		i = wait(&status);
	while (i != pid && i != -1);
	if (i == -1 || (status & 0377))
		onintr();
	if (status != 0) {
		if ((status >> 8) == ERRS)
			write(2, "Execution suppressed due to compilation errors\n", 47);
		onintr();
	}
	ac--;
	argv[ac] = name - 2;
	argv[argc] = 0;
	/* Temporary to allow accounting to distinguish pix's and px's */
	do
		execv("/usr/bin/pix-px", &argv[ac]);
	while (errno == ETXTBSY);
	do
		execv("/usr/ucb/px", &argv[ac]);
	while (errno == ETXTBSY);
	do
		execv("/usr/bin/px", &argv[ac]);
	while (errno == ETXTBSY);
	write(2, "Can't find px\n", 14);
	onintr();
}

dotted(cp, ch)
	char *cp, ch;
{
	register int i;

	i = strlen(cp);
	return (i > 1 && cp[i - 2] == '.' && cp[i - 1] == ch);
}

onintr()
{

	signal(2, 1);
	unlink(name);
	exit(1);
}
