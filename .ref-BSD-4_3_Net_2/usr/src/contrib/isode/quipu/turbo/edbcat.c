#include <stdio.h>
#include <gdbm.h>
#include <sys/file.h>
#include <ctype.h>

extern int gdbm_errno;

main (argc, argv)
int	argc;
char	**argv;
{
	GDBM_FILE	db;
	datum		key, content;
	char		buf[4096], kbuf[256];
	char		*bp, *rc, *p;
	int		buflen, len;
	char		type[80], version[256];
	char		gfname[1024];
	char		*TidyString(), *rindex();

	if ( argc != 2 ) {
		printf("usage: %s edbdbmfile\n", argv[0]);
		exit(1);
	}

	strcpy(gfname, argv[1]);
	if ( (p = rindex(argv[1], '.')) == NULL 
	    || strcmp(p, ".gdbm") != 0 )
		strcat(gfname, ".gdbm");

	if ( (db = gdbm_open(gfname, 0, GDBM_READER, 0, 0)) == NULL ) {
		printf("Can't open (%s)\ndbm_error is (%d)\n",gfname,gdbm_errno);
		exit(1);
	}

	key.dptr = "HEADER";
	key.dsize = sizeof("HEADER");
	content = gdbm_fetch(db, key);
	if ( content.dptr == NULL ) 
		printf("No header!  Continuing...\n");
	else
		printf("%s\n", content.dptr);

	for ( key = gdbm_firstkey(db); key.dptr; key = gdbm_nextkey(db, key) ) {
		if ( strcmp(key.dptr, "HEADER") == 0 )
			continue;

		content = gdbm_fetch(db, key);
		printf("%s", content.dptr);
		free(content.dptr);
		content.dptr = NULL;
	}

	(void) gdbm_close(db);
}
