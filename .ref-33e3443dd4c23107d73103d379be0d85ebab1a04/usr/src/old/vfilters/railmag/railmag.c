/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)railmag.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * tell vcat which fonts are loaded on the "typesetter"
 */

#define MAGIC_NUMBER 0436
#define RAILMAG_FILE "/usr/lib/vfont/railmag"

char	*concat();
int	rmfd;
char	*rm[4];
char	tbuf[256];

main(argc, argv)
	int argc;
	char *argv[];
{
	register int fnum;
	char cbuf[4][50];

	readrm();
	if (argc <= 1) {
		printrm();
		exit(0);
	}
	while (--argc) {
		argv++;
		fnum = argv[0][0] - '0';
		if (fnum < 1 || fnum > 4)
			error("Invalid font number");
		checkfont(argv[1]);
		if (argv[1][0] == '/')
			rm[fnum-1] = argv[1];
		else
			rm[fnum-1] = concat(cbuf[fnum-1], "/usr/lib/vfont/", argv[1]);
		argv++;	argc--;
	}
	writerm();
}

error(str)
	char *str;
{
	write(2, "Railmag: ", 9);
	write(2, str, strlen(str));
	write(2, "\n", 1);
	exit();
}

checkfont(file)
	char *file;
{
	register int fd;
	char cbuf[80];
	char cbuf2[80];
	short word;

	if ((fd = open(concat(cbuf, file, ".10"), 0)) < 0)
		if ((fd = open(concat(cbuf2, "/usr/lib/vfont/", cbuf), 0)) < 0)
			error("cant open font");
	if (read(fd, &word, 2) != 2)
		error("cant read font");
	if (word != MAGIC_NUMBER)
		error("font has no magic number");
	close(fd);
}

readrm()
{
	register int i;
	register char *cp;
	char c;

	if ((rmfd = open(RAILMAG_FILE, 0)) < 0)
		error("No railmag file");
	cp = tbuf;
	for (i = 0; i < 4; i++) {
		rm[i] = cp;
		while (read(rmfd, &c, 1) == 1 && c != '\n')
			*cp++ = c;
		*cp++ = '\0';
	}
}

printrm()
{
	register int i;

	for (i = 0; i < 4; i++)
		printf("%s on %d\n", rm[i], i+1);
}

writerm()
{
	register int i;
	register char *cp;

	unlink(RAILMAG_FILE);
	if ((rmfd = creat(RAILMAG_FILE, 0644)) < 0)
		error("cant recreate railmag file");
	for (i = 0; i < 4; i++) {
		cp = rm[i];
		while (*cp != '\0')
			write(rmfd, cp++, 1);
		write(rmfd, "\n", 1);
	}
}

char *
concat(outbuf, in1, in2)
	register char *outbuf, *in1, *in2;
{
	char *save;

	save = outbuf;
	while (*in1)
		*outbuf++ = *in1++;
	while (*in2)
		*outbuf++ = *in2++;
	return(save);
}
