#include <retrofit.h>

main(argc, argv)
	int argc;
	char *argv[];
{

	argc--, argv++;
	if (argc > 0) {
		do
			printenv(*argv++);
		while (--argc > 0);
	} else {
		printenv("HOME");
		printenv("TERM");
	}
	exit(0);
}

printenv(cp)
	char *cp;
{
	if (!strcmp(cp, "TERM")) {
		char buf[512];
		cp = getenv("TERM");
		if (tgetent(buf, cp) > 0)
			cp = longname(buf);
		printf("TERM=%s\n", cp);
	} else if (!strcmp(cp, "HOME"))
		printf("HOME=%s\n", getenv("HOME"));
}

longname(cp)
	char *cp;
{
	char *dp = cp;

	while (*dp && *dp != ':') {
		if (*dp == '|')
			cp = dp + 1;
		dp++;
	}
	*dp = 0;
	return (cp);
}
