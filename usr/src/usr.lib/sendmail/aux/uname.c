# include <stdio.h>
# include <sysexits.h>

/*
**  UNAME -- print UNIX system name (fake version)
**
**	For UNIX 3.0 compatiblity.
*/

main(argc, argv)
	int argc;
	char **argv;
{
	char buf[40];

	gethostname(buf, sizeof buf);
	printf("%s\n", buf);
	exit(EX_OK);
}
