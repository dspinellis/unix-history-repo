/* down!honey 4/84 */

#define HISTORY "/usr/new/lib/news/history"
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

long	lseek();
char	*index();
datum	fetch(), dofetch();

main(argc, argv)
char **argv;
{
	int	fd;
	char	buf[BUFSIZ], *ptr1, *ptr2;
	datum	content;
	long	offset;

	if ((fd = open(HISTORY, 0)) < 0) {
		perror(HISTORY);
		exit(EX_UNAVAILABLE);
	}
	if (dbminit(HISTORY) < 0) {
		fprintf(stderr, "dbm error\n");
		exit(EX_UNAVAILABLE);
	}
	for (--argc, argv++; argc; --argc, argv++) {
		content = dofetch(*argv);
		if (content.dptr == 0) {
			printf("%s: No such key\n", *argv);
			continue;
		}
		/* ensure alignment */
		bcopy(content.dptr, (char *)&offset, sizeof offset);
		if (lseek(fd, offset, 0) < 0)
			continue;
		if (read(fd, buf, sizeof buf) <= 0)
			continue;
		if ((ptr2 = index(buf, '\n')) == 0)
			continue;
		*ptr2 = '\0';
		if ((ptr1 = index(buf, '\t')) == 0)
			continue;
		ptr1++;
		if ((ptr1 = index(ptr1, '\t')) == 0)
			continue;
		ptr1++;
		for (ptr2 = ptr1; ptr2 = index(ptr2, '.'); *ptr2 = '/')
			;
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
