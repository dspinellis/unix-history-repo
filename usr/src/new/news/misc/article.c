/* down!honey 4/84 */
/* article msg-id [ ... msg-id ] */
 * where message-id is usually of the form number@machine.domain,
 * and the domain in the message-id is optional.
 *
 * BUGS:
 *	Cannot handle all domains, for instance, BERKELEY.EDU.
 */

#define HISTORY "/usr/lib/news/history"
#define NEWSDIR "/usr/spool/news"

char	*formats[] = {		/* add as appropriate */
	"<%s.uucp>",
	"<%s>",
	"<%s.arpa>",
	"<%s.oz>",
	0
};

#include <stdio.h>
#include <sysexits.h>
#include <ctype.h>

typedef struct {
	char	*dptr;
	int	dsize;
} datum;

#if defined(USG_INDEX)
/* S3 or S5 both call strchr() what 4.X BSD calls index */
#define index strchr
#endif

long	lseek();
char	*index();
datum	fetch(), dofetch();

main(argc, argv)
char **argv;
{
	int	fd;
	char	buf[BUFSIZ], *ptr1, *ptr2;
	datum	content;

	if ((fd = open(HISTORY, 0)) < 0) {
		perror(HISTORY);
		exit(EX_UNAVAILABLE);
	}
	if (dbminit(HISTORY) < 0) {
		fprintf(stderr, "dbm error\n");
		exit(EX_UNAVAILABLE);
	}
	for (--argc, argv++; argc; --argc, argv++) {
		long	foff;		/* file offset */
		content = dofetch(*argv);
		if (content.dptr == 0) {
			printf("%s: No such key\n", *argv);
			continue;
		}

		/* Correct a machine dependent bug here, caused		*/
		/* because the lseek offset pointed to by dptr might	*/
		/* not be long-aligned.					*/
		/* Guy Harris suggested bug fix to prevent core drop.	*/
		/* This bug was written up in net.bugs.4bsd and		*/
		/* cross-posted to net.news.b				*/
		bcopy(content.dptr, (char *)&foff, sizeof foff);

		if (lseek(fd, foff, 0) < 0)
			continue;
		if (read(fd, buf, sizeof buf) <= 0)
			continue;

		/*
		 * To understand this piece of code, you must understand
		 * that the format of lines in the history file are either:
		 * <msg-id>TAB<date>SPACE<time>TABthenNL
		 * <msg-id>TAB<date>SPACE<time>TAB<spoolpathname>NL
		 * <msg-id>TAB<date>SPACE<time>TAB<spoolpathname>SPACE...
		 * The first format occurs when expired,
		 * the second form occurs when exactly one pathname,
		 * and the third occurs when cross-postings.
		 */
		 *

		/* remove end of line */
		if ((ptr2 = index(buf, '\n')) == 0)
			continue;
		*ptr2 = '\0';

		/* The 4th field contains the article file name */

		if ((ptr1 = index(buf, '\t')) == 0)
			continue;
		ptr1++;

		/* ptr1 now at begin of field 2 - the date field */

		if ((ptr1 = index(ptr1, '\t')) == 0)
			continue;
		ptr1++;

		/* ptr1 now at begin of field 4 - the article spool pathname */
		/* or the newline that has been converted to a NULL */

		/* change net.unix/231 to net/unix/231 */
		for (ptr2 = ptr1; ptr2 = index(ptr2, '.'); *ptr2 = '/')
			;

		/* terminate after the first pathname, if any */
		if ((ptr2 = index(ptr1, ' ')) != NULL)
			*ptr2 = '\0';

		if (*ptr1 == '\0')
			printf("expired\n");
		else
			printf("%s/%s\n", NEWSDIR, ptr1);
	}
	exit(EX_OK);
}

datum
dofetch(str)
char	*str;
{
	datum	key, content;
	char	buf[BUFSIZ], **fmt;
	register char *rcp;

	for (fmt = formats; *fmt; fmt++) {
		sprintf(buf, *fmt, str);
		rcp = buf - 1;
		while (*++rcp)
			if (isupper(*rcp))
				*rcp = tolower(*rcp);
		key.dptr = buf;
		key.dsize = strlen(buf) + 1;
		content = fetch(key);
		if (content.dptr)
			break;
	}
	return(content);
}
