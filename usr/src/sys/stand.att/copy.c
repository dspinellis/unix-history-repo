/*	copy.c	4.1	83/02/13	*/

/*
 * Copy from to in 10K units.
 * Intended for use in system
 * installation.
 */
main()
{
	int from, to;
	char fbuf[50], tbuf[50];
	char buffer[10240];
	register int i;

	from = getdev("From", fbuf, 0);
	to = getdev("To", tbuf, 1);
	for (;;) {
		i = read(from, buffer, sizeof (buffer));
		if (i != sizeof (buffer))
			break;
		(void) write(to, buffer, i);
	}
	printf("Copy completed\n");
	/* can't call exit here */
}

getdev(prompt, buf, mode)
	char *prompt, *buf;
	int mode;
{
	register int i;

	do {
		printf("%s: ", prompt);
		gets(buf);
		i = open(buf, mode);
	} while (i <= 0);
	return (i);
}
