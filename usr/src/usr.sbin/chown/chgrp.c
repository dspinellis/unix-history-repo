static	char *sccsid = "@(#)chgrp.c	4.2 82/03/05";
/*
 * chgrp gid file ...
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <grp.h>
#include <pwd.h>

struct	group	*gr, *getgrnam(), *getgrgid();
struct passwd *getpwuid(), *pwd;
struct	stat	stbuf;
int	gid, uid;
int	status;
char	ingroup;

main(argc, argv)
char *argv[];
{
	register c, i;
	char	*getlogin(), *name;

	if(argc < 3) {
		printf("usage: chgrp gid file ...\n");
		exit(4);
	}
	uid = getuid();
	if(isnumber(argv[1])) {
		gid = atoi(argv[1]);
		if (uid && (gr=getgrgid(gid)) == NULL) {
			printf("unknown group: %s\n",argv[1]);
			exit(4);
		}
	} else {
		if((gr=getgrnam(argv[1])) == NULL) {
			printf("unknown group: %s\n",argv[1]);
			exit(4);
		}
		gid = gr->gr_gid;
	}
	if (!(name = getlogin())) {
		pwd = getpwuid(uid);
		name = pwd->pw_name;
	}
	for (i=0; uid && gr->gr_mem[i]; i++)
		if (!(strcmp(name, gr->gr_mem[i])))
			ingroup = 1;
	if (!ingroup && uid) {
		printf("illegal group: %s\n",argv[1]);
		exit(4);
	}
	for(c=2; c<argc; c++) {
		stat(argv[c], &stbuf);
		if (uid && uid != stbuf.st_uid) {
			printf("%s: not owner\n", argv[c]);
			status = 1;
		} else
			chown(argv[c], stbuf.st_uid, gid);
	}
	exit(status);
}

isnumber(s)
char *s;
{
	register c;

	while(c = *s++)
		if(!isdigit(c))
			return(0);
	return(1);
}


