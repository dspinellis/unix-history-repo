/*
 * sendnews - send news article by mail.
 */

#ifdef SCCSID
static char	*SccsId = "@(#)sendnews.c	2.13	9/24/87";
#endif /* SCCSID */

#include <stdio.h>
#include <ctype.h>
#include "defs.h"

char buffer[BUFSIZ];

int linecount, oflag = 0, aflag = 0, bflag = 0, toflag = 0;

extern FILE *popen();

/* ARGSUSED */
main(argc, argv)
char **argv;
{
	register FILE *out;
	char newsgroup[BUFSIZ];

	while (**(++argv) == '-') {
		if (*++*argv == 'o')
			oflag++;
		else if (**argv == 'a')
			aflag++;
		else if (**argv == 'b')
			bflag++;
		else if (**argv == 'n')
			strcpy(newsgroup, *(++argv));
	}
	if (aflag && bflag) {
		fprintf(stderr, "'-a' and '-b' options mutually exclusive.\n");
		exit(1);
	}

#ifdef DEBUG
	printf("/bin/mail %s\n", *argv);
	sprintf(buffer, "cat");
#else
#ifdef SENDMAIL
	(void) sprintf(buffer, "%s -i -odq %s", SENDMAIL, *argv);
#else /* !SENDMAIL */
#ifdef M_XENIX
	(void) sprintf(buffer, "/usr/bin/mail %s", *argv);
#else /* XENIX is not quite Unix.... */
	(void) sprintf(buffer, "/bin/mail %s", *argv);
#endif /* !M_XENIX */
#endif /* !SENDMAIL */
#endif
	if ((out = popen(buffer, "w")) == NULL) {
		perror(buffer);
		exit(1);
	}

	/* Standard mail prelude to make the formatters happy */
	fprintf(out, "Subject: network news article\n");
	fprintf(out, "To: %s\n\n", *argv);

	while (fgets(buffer, sizeof buffer, stdin)) {
		if (*newsgroup && ngline()) {
			if (oflag)
				sprintf(buffer, "%s\n", newsgroup);
			else
				sprintf(buffer, "Newsgroups: %s\n", newsgroup);
		}
		putc('N', out);
		fputs(buffer, out);
		if (ferror(out))
			exit(1);
	}
	pclose(out);
	exit(0);
}

ngline()
{
	if (oflag)
		return linecount == 2;
	if (!toflag && (!strncmp("Newsgroups: ", buffer, 12) ||
		!strncmp("To: ",buffer, 4)))
		return ++toflag;
	return 0;
}
