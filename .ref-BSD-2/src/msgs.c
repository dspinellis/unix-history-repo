/* Copyright (c) 1979 Regents of the University of California */
/* #define CORY */
#include <stdio.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <ctype.h>

typedef	char	bool;

FILE	*f;
bool	hdrs;
bool	qopt;
char	*sep;
bool	ruptible;
int	onintr();

main(argc, argv)
	int argc;
	char *argv[];
{
	char obuf[BUFSIZ];
	bool newrc, already, argfirst = 0;
	int rcfirst, lastmsg, firstmsg;
	FILE *bounds, *msgsrc;
	int i, nextty;
	bool hush = 0;
	bool send = 0;

	ruptible = signal(SIGINT, SIG_IGN) == SIG_DFL;
	if (ruptible)
		signal(SIGINT, SIG_DFL);
	setbuf(stdout, obuf);
	argc--, argv++;
	while (argc >= 1) {
		if (isdigit(argv[0][0])) {
			argfirst = 1;
			rcfirst = atoi(argv[0]);
		} else switch (argv[0][1]) {
		
		case 'f':
			hush++;
			break;

		case 'q':
			qopt++;
			break;

		case 'h':
			hdrs++;
			break;

		case 's':
			send++;
			break;

		default:
			fprintf(stderr, "usage: msgs [ -f ] [ number ]\n");
			exit(1);
		}
		argc--, argv++;
	}
	if (!send) {
		setuid(getuid());
		if (chdir(getenv("HOME")) < 0)
			perror(getenv("HOME")), exit(1);
		newrc = 0;
		msgsrc = fopen(".msgsrc", "r");
		nextty = 1;
		if (msgsrc != NULL) {
			fscanf(msgsrc, "%d\n", &i);
			fclose(msgsrc);
			nextty = i;
			if (!argfirst)
				rcfirst = i;
		} else
			newrc = 1;
		msgsrc = fopen(".msgsrc", "w");
		if (msgsrc == NULL)
			perror(".msgsrc"), exit(1);
	}
	if (chdir("/usr/msgs") < 0)
		perror("/usr/msgs"), exit(1);
	bounds = fopen("bounds", "r");
	if (bounds == NULL) {
		FILE *d = fopen(".", "r");
		struct direct dirent;

		if (d == NULL)
			perror("/usr/msgs"), exit(1);
		firstmsg = 25000;
		lastmsg = 0;
		while (fread(&dirent, sizeof dirent, 1, d) == 1) {
			register char *cp = dirent.d_name;
			register int i = 0;

			if (dirent.d_ino == 0)
				continue;
			while (isdigit(*cp))
				i = i * 10 + *cp++ - '0';
			if (*cp)
				continue;
			if (i > lastmsg)
				lastmsg = i;
			if (i < firstmsg)
				firstmsg = i;
		}
		if (firstmsg == 25000) {
			firstmsg = 1;
			lastmsg = 0;
		}
		fclose(d);
		if (send) {
			unlink("bounds");
			bounds = fopen("bounds", "w");
#ifdef CORY
			chmod("bounds", 0644);
#else
			chmod("bounds", 0666);
#endif
			if (bounds == NULL)
				perror("bounds"), exit(1);
			fprintf(bounds, "%d %d\n", firstmsg, lastmsg);
			fclose(bounds);
		}
	} else {
		fscanf(bounds, "%d %d\n", &firstmsg, &lastmsg);
		fclose(bounds);
	}
	if (send) {
		char newname[16];
		FILE *newm;

		sprintf(newname, "%d", lastmsg+1);
		bounds = fopen("bounds", "w");
		if (bounds == NULL)
			perror("bounds"), exit(1);
		fprintf(bounds, "%d %d\n", firstmsg, lastmsg+1);
		fclose(bounds);
		newm = fopen(newname, "w");
		if (newm == NULL)
			fprintf(stderr, "/usr/msgs/"), perror(newname), exit(1);
		chmod(newname, 0644);
		for (;;) {
			register c;
			c = getchar();
			if (feof(stdin))
				exit(0);
			if (ferror(stdin))
				exit(1);
			putc(c, newm);
		}
	}
	if (!newrc)
		firstmsg = rcfirst;
	already = 0;
	for (i = firstmsg; i <= lastmsg; i++) {
		register int c;
		char inline[BUFSIZ];
		struct stat stbuf;
		char fname[16];

		sprintf(fname, "%d", i);
		f = fopen(fname, "r");
		if (f == NULL)
			continue;
		if (qopt) {
			fseek(msgsrc, (long) 0, 0);
			fprintf(msgsrc, "%d\n", nextty);
			fflush(msgsrc);
			printf("There are new messages.\n");
			exit(0);
		}
		if (already)
			printf("\n");
		already = 1;
		fseek(msgsrc, (long) 0, 0);
		fprintf(msgsrc, "%d\n", nextty);
		fflush(msgsrc);
		printf("Message %d:\n", i);
		fgets(inline, sizeof inline, f);
		printf("%s", inline);
		if (fgets(inline, sizeof inline, f)) {
			if (strcmp(inline, "To: msgs\n") != 0 && inline[0] != '\n')
				printf("%s", inline);
			if (fgets(inline, sizeof inline, f) && inline[0] != '\n')
				printf("%s", inline);
		}
		c = getc(f);
		sep = "-";
		if (c == '\n')
			c = getc(f);
		if (!feof(f))
			printf("(%d more lines)", linecnt(f));
		if (hdrs) {
			printf("\n-----\n");
			fclose(f);
			continue;
		}
		if (feof(f))
			printf("(continue) [yq] ");
		else
			printf(" type [ynq] ? ");
		fflush(stdout);
		inline[0] = 0;
		fgets(inline, sizeof inline, stdin);
		if (inline[0] == 0)
			printf("\n");
		if (inline[0] == 'q')
			exit(1);
		if (isdigit(inline[0])) {
			sscanf(inline, "%d", &i);
			printf("--Goto %d--\n", i);
			i--;
			fflush(stdout);
				fclose(f);
				continue;
		}
		if (inline[0] == 'n' || inline[0] == 'N') {
			printf("--Flushed--\n");
			fflush(stdout);
			fclose(f);
			if (i >= nextty)
				nextty = i + 1;
			continue;
		}
		if (ruptible)
			signal(SIGINT, onintr);
		if (!feof(f))
			for (;;) {
				putchar(c);
				c = getc(f);
				if (feof(f))
					break;
			}
		printf("--%s--\n", sep);
		if (i >= nextty)
			nextty = i + 1;
		fflush(stdout);
		if (ruptible)
			signal(SIGINT, SIG_DFL);
		fclose(f);
	}
	fseek(msgsrc, (long) 0, 0);
	fprintf(msgsrc, "%d\n", nextty);
	fclose(msgsrc);
	if (qopt)
		exit(0);
	if (!already && !hush)
		printf("No messages.\n");
	exit(0);
}

onintr()
{

	sep = "Skip";
	printf("\n");
	fseek(f, (long) 0, 2);
}

linecnt(f)
	FILE *f;
{
	off_t ftell();
	off_t oldpos = ftell(f);
	int l = 0;
	char lbuf[BUFSIZ];

	while (fgets(lbuf, sizeof lbuf, f))
		l++;
	clearerr(f);
	fseek(f, oldpos, 0);
	return (l);
}
