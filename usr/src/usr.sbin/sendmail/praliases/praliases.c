# include "sendmail.h"

static	char sccsid[] = "@(#)praliases.c	5.2	%G%";

typedef struct { char *dptr; int dsize; } datum;
datum	firstkey(), nextkey(), fetch();
char	*filename = ALIASFILE;

main(argc, argv)
	char **argv;
{
	datum content, key;

	if (argc > 2 && strcmp(argv[1], "-f") == 0)
	{
		argv++;
		filename = *++argv;
		argc -= 2;
	}

	if (dbminit(filename) < 0)
		exit(EX_OSFILE);
	argc--, argv++;
	if (argc == 0) {
		for (key = firstkey(); key.dptr; key = nextkey(key)) {
			content = fetch(key);
			printf("\n%s:%s\n", key.dptr, content.dptr);
		}
		exit(EX_OK);
	}
	while (argc) {
		key.dptr = *argv;
		key.dsize = strlen(*argv)+1;
		content = fetch(key);
		if (content.dptr == 0)
			printf("%s: No such key\n", key.dptr);
		else
			printf("\n%s:%s\n", key.dptr, content.dptr);
		argc--, argv++;
	}
	exit(EX_OK);
}
