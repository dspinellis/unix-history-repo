#include <stdio.h>
#include <gdbm.h>
#include <sys/file.h>
#include <ctype.h>

main (argc, argv)
int	argc;
char	**argv;
{
	int		filearg, verbose;
	FILE		*fp;
	GDBM_FILE	db;
	datum		key, content;
	char		buf[200000], kbuf[256];
	char		*bp, *rc;
	int		buflen, len;
	char		type[80], version[256];
	char		gfname[1024];
	char		*TidyString();

	if ( argc < 2 || argc > 3 ) {
		fprintf(stderr, "usage: %s [-v] edbfile\n", argv[0]);
		exit(1);
	}

	filearg = 1;
	verbose = 0;
	if ( argc == 3 )
		if ( strcmp(argv[1], "-v") == 0 ) {
			verbose++;
			filearg++;
		} else if ( strcmp(argv[1], "-vv") == 0 ) {
			verbose += 2;
			filearg++;
		} else
			fprintf(stderr, "invalid flag ignored: %s\n", argv[1]);

	if ( (fp = fopen(argv[filearg], "r")) == NULL ) {
		perror(argv[filearg]);
		exit(1);
	}

	strcpy(gfname, argv[filearg]);
	strcat(gfname, ".gdbm");
	if ( (db = gdbm_open(gfname, 0, GDBM_NEWDB, 00664, 0)) == NULL ) {
		fprintf(stderr, "could not open %s\n", gfname);
		exit(1);
	}

	if ( fgets(type, sizeof(type), fp) == NULL ) {
		fprintf(stderr, "File is empty!\n");
		exit(1);
	}
	if ( fgets(version, sizeof(version), fp) == NULL ) {
		fprintf(stderr, "No version specified!\n");
		exit(1);
	}
	sprintf(buf, "%s%s", type, version);
	key.dptr = "HEADER";
	key.dsize = sizeof("HEADER");
	content.dptr = buf;
	content.dsize = strlen(buf) + 1;
	if ( verbose > 0 ) printf("HEADER: (%s)\n", content.dptr);
	if ( gdbm_store(db, key, content, GDBM_INSERT) != 0 ) {
		fprintf(stderr, "could not dbm_store header");
		exit(1);
	}

	while ( feof(fp) == 0 ) {
		buflen = sizeof(buf);
		bp = buf;
		while ( rc = fgets(bp, buflen, fp) )
			if ( *buf !=  '#' && *buf != '\n' ) break;
		if ( rc == NULL ) break;

		strcpy(kbuf, buf);
		kbuf[strlen(kbuf)-1] = '\0';
		key.dptr = TidyString(kbuf);
		key.dsize = strlen(kbuf) + 1;

		if ( verbose > 0 ) printf("key (%s)\n", key.dptr);

		sprintf(buf, "%s\n", kbuf);
		len = strlen(bp);
		bp += len;
		buflen -= len;

		while ( rc = fgets(bp, buflen, fp) ) {
			if ( *bp == '\n' ) break;

			len = strlen(bp);
			bp += len;
			buflen -= len;
		}
		*bp = '\n';

		content.dptr = buf;
		content.dsize = strlen(buf) + 1;
		if ( verbose > 1 ) printf("content (%s)\n", content.dptr);
		if ( gdbm_store(db, key, content, GDBM_INSERT) != 0 ) {
			fprintf(stderr, "error: dbm_store\n");
			exit(1);
		}
		free(content.dptr);
		free(key.dptr);
	}

	(void) gdbm_close(db);
}


char * TidyString (a)
register char * a;
{
register char * b;
char * c;
register int i = 0;

	if (!*a)
		return (a);

	/* remove white space from front of string */
	while (isspace (*a))
		a++;

	/* now continue removing multiple and trailing spaces */
	c = a, b = a;
	while (*a) {
		if (isspace (*a)) {
			*b = ' ';       /* make sure not a tab etc */
			while (isspace (*++a))
				i = 1;

			if (*a)
				b++;
			else
				break;
		}
		if (i == 1)
			*b = *a;

		a++, b++;
	}

	*b = 0;

	if (*--b == '\n')
		*b-- = 0;

	if (*b == ' ')
		*b = 0;

	return (c);
}

