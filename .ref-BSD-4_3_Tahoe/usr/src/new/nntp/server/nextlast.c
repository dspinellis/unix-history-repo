#ifndef lint
static char	*sccsid = "@(#)nextlast.c	1.3	(Berkeley) 6/26/87";
#endif

#include "common.h"

/*
 * NEXT
 * LAST
 *
 * Retrieve the message-id of the next or last article in the
 * newsgroup.  Position the current article pointer to this
 * article.
 */

nextlast(argc, argv)
	int	argc;
	char	*argv[];
{
	char	artbuf[MAX_STRLEN], art_id[MAX_STRLEN];
	int	oldptr;
	int	next;

	if (!canread) {
		printf("%d You only have permission to transfer, sorry.\r\n",
			ERR_ACCESS);
		(void) fflush(stdout);
		return;
	}

	if (!ingroup) {
		printf("%d You are not currently in a newsgroup.\r\n",
			ERR_NCING);
		(void) fflush(stdout);
		return;
	}

	if (argc != 1) {
		printf("%d NEXT/LAST need no arguments.\r\n", ERR_CMDSYN);
		(void) fflush(stdout);
		return;
	}

	next = (argv[0][0] == 'n' || argv[0][0] == 'N');

	if (art_ptr < 0 || art_ptr >= num_arts) {
		printf("%d No current article selected.\r\n",
			ERR_NOCRNT);
		(void) fflush(stdout);
		return;
	}

	if (next ? (art_ptr + 1 >= num_arts) : (art_ptr - 1 < 0)) {
		printf("%d No %s article to retrieve.\r\n",
			ERR_NONEXT,  next ? "next" : "previous");
		(void) fflush(stdout);
		return;
	}

	oldptr = art_ptr;
	(void) sprintf(artbuf, "%d", art_array[next ? ++art_ptr : --art_ptr]);

	if (!valid_art(artbuf)) {
		printf("%d Invalid article number: %s.\r\n", ERR_NOARTIG,
			artbuf);
		(void) fflush(stdout);
		return;
	}

	while (open_valid_art(artbuf, art_id) == NULL) {
		if (((next) ? (++art_ptr >= num_arts) : (--art_ptr < 0))) {
			printf("%d No %s article to retrieve.\r\n",
				ERR_NONEXT, next ? "next" : "previous");
			art_ptr = oldptr;
			(void) fflush(stdout);
			return;
		}
		(void) sprintf(artbuf, "%d", art_array[art_ptr]);
	}

	printf("%d %s %s Article retrieved; request text separately.\r\n",
		OK_NOTEXT, artbuf, art_id);

	if (argc > 1)
		art_ptr = findart(artbuf);
	(void) fflush(stdout);
}
