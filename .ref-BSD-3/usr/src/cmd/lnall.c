#include <sys/types.h>
#include <sys/stat.h>

/* lnall-
 *	son of mvall, son of cpall, son of ...
 */

char	buf[100];

main(argc, argv)
char **argv;
{
	register char *c1, *c2, *cp;
	int i;

	if(argc < 3) {
		write(2, "arg count\n", 10);
		exit();
	}
	argc--;
	if (!dir(argv[argc]))
	{
		printf("%s: not a directory\n", argv[argc]);
		exit(1);
	}
	c1 = buf;
	c2 = argv[argc];
	while(*c1++ = *c2++);
	c1[-1] = '/';
	for(i=1; i<argc; i++)
	{
		c2 = c1;
		cp = (char *) actual(argv[i]);
		while (*c2++ = *cp++);
		if (link(argv[i],buf)) perror(argv[i]);
	}
}

dir(n)
	char *n;
{
     struct stat  statbuf;
	if (stat(n,&statbuf)) return(0);
	return((statbuf.st_mode & 060000) == 040000);
}

actual(str)
	char *str;
{
	register char *f;
	register slash;
	f = str;
	slash = 0;
	while (*f) if (*f++ == '/') slash++;
	if (!slash) return(str);
	while (*f != '/') --f;
	++f;
	return(f);
}


