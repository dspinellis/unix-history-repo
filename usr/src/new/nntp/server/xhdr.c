#ifndef lint
static char	*sccsid = "@(#)xhdr.c	1.3	(Berkeley) 7/17/87";
#endif

#include "common.h"

#ifdef XHDR

/*
 * XHDR header [<messageid>|articlerange]
 *
 * header is a case-insensitive header field, minus any colons.
 *
 * articlerange is one of:
 *	an article number
 *	an article number followed by a dash to indicate all following
 *	an article number followed by a dash followed by another
 *		article number.
 * e.g.,
 * XHDR subject			retrieve subject of current article
 * XHDR subject 5589-6325	retrieve subject of arts 5589 to 6325
 * XHDR subject 5589-		retrieve subject of arts 5589 and up
 * XHDR subject 5589		retrieve subject of art 5589 only
 * XHDR subject <123@ucbvax>	retrieve subject of art <123@ucbvax>
 *
 * This command is an extention, and not included in RFC 977.
 */

xhdr(argc, argv)
	int		argc;
	char		*argv[];
{
	char		buf[MAX_STRLEN];
	register int	artptr;
	register int	artnum;
	register int	low, high;
	register FILE	*fp;
	register char	*cp;

	if (argc < 2 || argc > 3) {
		printf("%d Usage: XHDR headerfield [artrange|<message-id>]\r\n",
			ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	}

	if (!canread) {
		printf("%d You only have permission to transfer, sorry.\r\n",
			ERR_ACCESS);
		(void) fflush(stdout);
		return;
	}

	/* Handle message-id requests */

	if (argc == 3 && *argv[2] == '<') {	/* Message ID */
		fp = openartbyid(argv[2]);
		if (fp == NULL) {
			printf("%d No article by message-id %s, sorry.\r\n",
				ERR_NOART, argv[2]);
			(void) fflush(stdout);
			return;
		}
		printf("%d 0 %s header of article %s.\r\n",
			OK_HEAD, argv[1], argv[2]);
		print_header(fp, argv[1], argv[2]);
		(void) fclose(fp);

		putchar('.');
		putchar('\r');
		putchar('\n');
		(void) fflush(stdout);
		return;
	}

	/*
	 * It must be a range of articles, which means that we need
	 * to be in a newsgroup already.
	 */

	if (!ingroup) {
		printf("%d You are not currently in a newsgroup.\r\n",
			ERR_NCING);
		(void) fflush(stdout);
		return;
	}

	artptr = 0;

	if (argc == 2) {
		if (art_ptr < 0 || art_ptr >= num_arts) {
			printf("%d No article is currently selected.\r\n",
				ERR_NOCRNT);
			(void) fflush(stdout);
			return;
		}
		high = low = art_array[art_ptr];
		artptr = art_ptr;
	} else {
		cp = index(argv[2], '-');
		if (cp == NULL)
			low = high = atoi(argv[2]);
		else {
			*cp = '\0';
			low = atoi(argv[2]);
			cp++;
			high = atoi(cp);
			if (high < low)
				high = art_array[num_arts-1];
		}
	}

	printf("%d %s fields follow\r\n", OK_HEAD, argv[1]);

	for (;; artptr++) {
		if ((artnum = art_array[artptr]) < low)
			continue;
		if (artnum > high)
			break;

		(void) sprintf(buf, "%d", artnum);
		fp = fopen(buf, "r");
		if (fp == NULL)
			continue;
		print_header(fp, argv[1], buf);
		(void) fclose(fp);
	}

	putchar('.');
	putchar('\r');
	putchar('\n');
	(void) fflush(stdout);
}


print_header(fp, header, artname)
	register FILE	*fp;
	register char	*header;
	register char	*artname;
{
	char		line[MAX_STRLEN];
	register char	*cp, *cp1;

	while (fgets(line, sizeof (line), fp) != NULL) {
		if (*line == '\n' || *line == '\0') {
			printf("%s (none)\r\n", artname);
			return;
		}
		if (cp = index(line, ':')) {
			*cp = '\0';
			if (streql(header, line)) {
				if (cp1 = index(cp + 2, '\n'))
					*cp1 = '\0';
				printf("%s %s\r\n", artname, cp + 2);
				return;
			}
		}
	}
}

#endif XHDR
