#include <sys/types.h>
#include <sys/stat.h>
main(argc, argv)
	int argc;
	char **argv;
{

	argc--, argv++;
	while (argc > 0) {
		struct stat stb; int c, f;
		if (stat(*argv, &stb) < 0)
			goto bad;
		if (chmod(*argv, stb.st_mode | 0200) < 0)
			goto bad;
		f = open(*argv, 2);
		if (f < 0)
			goto bad;
		lseek(f, 0, 0);
		read(f, &c, 1);
		lseek(f, 0, 0);
		write(f, &c, 1);
		close(f);
		chmod(*argv, stb.st_mode);
		argc--, argv++;
		continue;
bad:
		perror(*argv);
		argc--, argv++;
		continue;
	}
}
