/*
 * ln [ -f ] target [ new name ]
 */

#include <sys/types.h>
#include <sys/stat.h>
#include "stdio.h"
char	*rindex();

main(argc, argv)
char **argv;
{
	struct stat statb;
	register char *np;
	int fflag = 0;
	char nb[100], *name=nb, *arg2;
	int statres;

	if (argc >1 && strcmp(argv[1], "-f")==0) {
		argc--;
		argv++;
		fflag++;
	}
	if (argc<2 || argc>3) {
		printf("Usage: ln target [ newname ]\n");
		exit(1);
	}
	np = rindex(argv[1], '/');
	if (np==0)
		np = argv[1];
	else
		np++;
	if (argc==2)
		arg2 = np;
	else
		arg2 = argv[2];
	statres = stat(argv[1], &statb);
	if (statres<0) {
		printf ("ln: %s does not exist\n", argv[1]);
		exit(1);
	}
	if (fflag==0 && (statb.st_mode&S_IFMT) == S_IFDIR) {
		printf("ln: %s is a directory\n", argv[1]);
		exit(1);
	}
	statres = stat(arg2, &statb);
	if (statres>=0 && (statb.st_mode&S_IFMT) == S_IFDIR)
		sprintf(name, "%s/%s", arg2, np);
	else
		name = arg2;
	if (link(argv[1], name)<0) {
		perror("ln");
		exit(1);
	}
	exit(0);
}
