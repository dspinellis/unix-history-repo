#include <sys/types.h>
#include <sys/stat.h>
#include "../h/config.h"
#define ICONX "/usr/new/iconx"
char *pptr;
main(argc,argv)
int argc;
char **argv;
{

	char *path, *getenv();
	char file[256];
	char *name;
	
	name = argv[0];
	/*
	 * If the name contains any /'s we skip the path search and
	 *  just try to run the file.
	 */
	if (index(name,'/'))
		doiconx(argv,name);
	
	pptr = path = getenv("PATH");
	while (trypath(name,file)) {
		if (canrun(file))
			doiconx(argv,file);
		}
	/*
	 * If we can't find it, we assume that it must exist somewhere
	 *  and infer that it's in the current directory.
	 */
	if (canrun(name))
		doiconx(argv,name);
	exit(100);
}
canrun(file)
char *file;
{
	struct stat statb;
	if (access(file,5) == 0) {
		stat(file,&statb);
		if (statb.st_mode & S_IFREG)
			return 1;
		}
	return 0;
}
doiconx(av,file)
char **av; char *file;
{
	av[0] = file;
	av[-1] = "-iconx";
	execv(ICONX,&av[-1]);
	exit(200);
}
trypath(name,file)
char *name, *file;
{
	char *n, c;
	
	while (*pptr == ':')
		pptr++;
	if (!*pptr)
		return 0;
	do {
		c = (*file++ = *pptr++);
		} while (c != ':' && c);
	pptr--;
	file--;
	
	*file++ = '/';
	while (*file++ = *name++);
	*file = 0;
}
exit(c)
{
	_exit(c);
}
