/* Copyright (c) 1979 Regents of the University of California */
#include <retrofit.h>
#include <stdio.h>
#include <ctype.h>
#include <sgtty.h>
#include <signal.h>

#define	CTRL(x)	('x' & 037)
#define	DELETE	0177

int	btime, wtime;
int	state;
int	paused;
int	move;
int	movebrk;
int	cmoves;
int	ctime;

#define	WHITE	0
#define	BLACK	1

struct	sgttyb norm;
struct	sgttyb raw;

char	*pt();

main(argc, argv)
	int argc;
	char *argv[];
{

	argc--, argv++;
	gtty(1, &norm);
	gtty(1, &raw);		/* No structure assignment */
	btime = wtime = 300;
	movebrk = 80, ctime = 0, cmoves = 0;
	while (argc > 0) {
		if (argv[0][0] != '-') {
usage:
			fprintf(stderr, "Usage: chessclock [ -m moves ] [ -t tlim ] [ -M xmoves ] [ -T xtime ]\n");
			exit (1);
		}
		if (argc < 2)
			goto usage;
		switch (argv[0][1]) {

		case 'm':
			if (!number(argv[1]))
				goto usage;
			movebrk = 2 * getn(argv[1]) + 1;
			break;

		case 't':
			if (!istime(argv[1]))
				goto usage;
			btime = wtime = gettime(argv[1]);
			break;

		case 'M':
			if (!number(argv[1]))
				goto usage;
			cmoves = 2 * getn(argv[1]);
			break;

		case 'T':
			if (!istime(argv[1]))
				goto usage;
			ctime = gettime(argv[1]);
			break;

		default:
			goto usage;
		}
		argv += 2, argc -= 2;
	}
	if (movebrk) {
		if (cmoves == 0)
			cmoves = movebrk - 1;
		if (ctime == 0)
			ctime = btime;
	}
	raw.sg_flags |= RAW;
	raw.sg_flags &= ~ECHO;
	stty(1, &raw);
	printf("Hit space to begin, ? for information\r\n");
	state = BLACK;
	paused = 1;
	doit();
}

onalrm()
{
	char tbuf[10];

	signal(SIGALRM, onalrm);
	if (paused)
		return;
	charge(1);
	printf("\b\b\b\b%s\b", pt(tbuf, state == BLACK ? btime : wtime));
	alarm(1);
}

doit()
{
	int t, c;

	move = 0;
	signal(SIGALRM, onalrm);
	for (;;) {
		alarm(1);
		c = readone();
		t = 1 - alarm(0);
		switch (c) {

		default:
			printf("\r\n\r\nSpace   move\r\n");
			printf("p       pause\r\n");
			printf("c       continue\r\n");
			printf("q       quit\r\n");
			printf("!       shell escape\r\n");
			printf("\r\n");
			fixit();
			continue;

		case ' ':
			if (!paused) {
				charge(t);
				if (state == BLACK)
					printf("\r\n");
			}
			paused = 0;
			move++;
			if (move == movebrk && cmoves != 0) {
				movebrk += cmoves;
				btime = wtime = ctime;
			}
			state = state == WHITE ? BLACK : WHITE;
			fixit();
			continue;

		case 'c':
			if (!paused)
				charge(t);
			paused = 0;
			continue;

		case '!':
			if (!paused)
				charge(t);
			stty(1, &norm);
			printf("\r\n!");
			system("sh -t");
			printf("!\r\n");
			printf("Hit c to continue\r\n");
			stty(1, &raw);
			t = 0;
			fixit();
			paused = 1;
			continue;

		case 'p':
			if (!paused)
				charge(t);
			paused = 1;
			continue;

		case 'q':
		case CTRL(\\\\):
		case DELETE:
			printf("\r\n");
			exit(1);
		}
	}
}

fixit()
{
	register int i;
	char wbuf[10], bbuf[10];

	printf("\r%3d     %s   %s", (move + 1) / 2,
		pt(wbuf, wtime), pt(bbuf, btime));
	if (state == WHITE)
		for (i = 0; i < 8; i++)
			printf("\b");
	printf("\b");
}

charge(c)
	int c;
{

	if (state == WHITE) {
		wtime -= c;
		if (wtime <= 0) {
			printf("\r\n\07\07\07\07\07White loses on time\r\n");
			exit(0);
		}
	} else {
		btime -= c;
		if (btime <= 0) {
			printf("\r\n\07\07\07\07\07Black loses on time\r\n");
			exit(0);
		}
	}
}

readone()
{
	char c;

	while (read(0, &c, 1) != 1)
		if (!isatty(0))
			exit(1);
	return (c);
}

exit(c)
	int c;
{

	stty(1, &norm);
	_exit(c);
}

char *
pt(buf, tim)
	char *buf;
	int tim;
{

	sprintf(buf, "%2d:%1d%1d", tim / 60, (tim % 60) / 10, tim % 10);
	return (buf);
}

number(cp)
	char *cp;
{

	if (!isdigit(*cp))
		return (0);
	while (isdigit(*cp))
		cp++;
	return (*cp == 0);
}

istime(cp)
	char *cp;
{

	if (*cp != ':' && !isdigit(*cp))
		return (0);
	while (isdigit(*cp))
		cp++;
	if (*cp == ':')
		cp++;
	while (isdigit(*cp))
		cp++;
	return (*cp == 0);
}

gettime(cp)
	char *cp;
{
	int t = 0, s = 0;

	while (isdigit(*cp))
		t = t * 10 + *cp++ - '0';
	if (*cp != ':')
		return (t * 60);
	cp++;
	s = 0;
	while (isdigit(*cp))
		s = s * 10 + *cp++ - '0';
	return (t * 60 + s);
}

getn(cp)
	char *cp;
{

	int i = 0;

	while (*cp)
		i = i * 10 + *cp++ - '0';
	return (i);
}
