/*
 * mesg -- set current tty to accept or
 *	forbid write permission.
 *
 *	mesg [y] [n]
 *		y allow messages
 *		n forbid messages
 */

int	sbuf[40];

main(argc, argv)
char *argv[];
{
	register char *tty;

	tty = "/dev/ttyx";
	tty[8] = ttyn(1);
	if(stat(tty, sbuf) < 0) {
		write(2, "cannot stat\n", 12);
		exit(1);
	}
	if(argc < 2) {
		if(sbuf[2] & 02)
			goto no;
		goto yes;
	}
	if(*argv[1] == 'y')
		goto yes;

no:
	if(chmod(tty, 0600) < 0)
		goto bad;
	goto was;

yes:
	if(chmod(tty, 0622) < 0)
		goto bad;

was:
	if(sbuf[2] & 02)
		write(2, "was y\n", 6); else
		write(2, "was n\n", 6);
	exit(0);

bad:
	write(2, "cannot change mode\n", 19);
	exit(1);
}
