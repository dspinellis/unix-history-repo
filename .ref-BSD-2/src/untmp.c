/* Copyright (c) 1979 Regents of the University of California */
/*
 * Remove stuff in /tmp owned by the invoker.
 */
#include <retrofit.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <sys/dir.h>

struct	direct	dirent[2];

main()
{
	struct stat stbuf;
	register int uid;

	uid = getuid();
	if (chdir("/tmp") < 0) {
		perror("/tmp");
		exit(1);
	}
	if (freopen(".", "r", stdin) == NULL)
		exit(1);
	while (fread((char *) &dirent[0], sizeof (struct direct), 1, stdin) == 1) {
#define	ent	dirent[0]
		if (ent.d_ino == 0)
			continue;
		if (stat(ent.d_name, &stbuf))
			continue;
		if (!strcmp(ent.d_name, ".") || !strcmp(ent.d_name, ".."))
			continue;
		if (stbuf.st_uid != uid)
			continue;
		if ((stbuf.st_mode & S_IFMT) != S_IFREG)
			continue;
		if (unlink(ent.d_name))
			continue;
		printf("%s\n", ent.d_name);
	}
	exit(0);
}
