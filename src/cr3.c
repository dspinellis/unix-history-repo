/* Copyright (c) 1979 Regents of the University of California */
#include <retrofit.h>
#include <stdio.h>
#include <sgtty.h>
/*
 * cr3 - simulate chuck haley's cr3
 * Bill Joy UCB July 1, 1977
 *
 * This filter simulates chuck haleys cr3,
 * stopping output after each page (22 lines) to wait for
 * a carriage return, sending 22 more lines, or a EOF, sending 10 more lines.
 *
 * Typing in a positive number will cause that many lines to be sent,
 * a negative number that many lines to be skipped.
 */

struct	sgttyb otty, ntty;
int	left, nxtleft, onintr();
char	ch;
char	obuf[BUFSIZ];

main(argc, argv)
	int argc;
	char *argv[];
{
	register c;
	FILE *f;

	setbuf(stdout, obuf);
	gtty(1, &otty);
	gtty(1, &ntty);
	ntty.sg_flags &= ~ECHO;
	signal(2, onintr);
	stty(1, &ntty);
	left = 23;
	argc--;
	argv++;
	f = stdin;
	do {
		if (argc > 0) {
			close(0);
			if ((f=fopen(argv[0], "r")) == NULL) {
				fflush(stdout);
				perror(argv[0]);
				fflush(stdout);
				onintr();
			}
			argc--;
			argv++;
		}
		for (;;) {
			c = getc(f);
			if (c == -1)
				break;
			if (left < 0) {
				left++;
				if (left == 0)
					left = nxtleft;
			} else {
				left--;
				if (left == 0) {
					fflush(stdout);
					getleft();
				}
			}
			if (left > 0)
				putchar(c);
			while (c != '\n') {
				c = getc(f);
				if (c == -1)
					goto endfile;
				if (left > 0)
					putchar(c);
			}
			fflush(stdout);
		}
endfile:
		fflush(stdout);
	} while (argc > 0);
	stty(1, &otty);
	exit(0);
}

onintr()
{

	signal(2, 1);
	stty(1, &otty);
	exit(1);
}

getleft()
{
	int i;

	i = number();
	if (i == 0) {
		left = 22;
		return;
	}
	left = i;
	if (i < 0) {
		if (ch == '\n')
			nxtleft = 11;
		else {
			i = number();
			nxtleft = i == 0 ? 11 : i;
		}
	}
}

char ch;

number()
{
	int i, sign;

	i = 0;
	sign = 0;
	while (read(2, &ch, 1) == 1) {
		switch (ch) {
			case ' ':
				if (sign == 0)
					continue;
			case '\n':
				return (i * sign);
			case '-':
				sign = -1;
				continue;
			default:
				if (ch < '0' || ch > '9')
					continue;
				i *= 10;
				i += ch - '0';
				if (sign == 0)
					sign = 1;
				continue;
		}
	}
	return (11);
}
