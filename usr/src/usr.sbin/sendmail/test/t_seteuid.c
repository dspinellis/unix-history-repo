/*
**  This program checks to see if your version of seteuid works.
**  Compile it, make it setuid root, and run it as yourself (NOT as
**  root).  If it won't compile or outputs any MAYDAY messages, don't
**  define USESETEUID in conf.h.
**
**	NOTE:  It is not sufficient to have seteuid in your library.
**	You must also have saved uids that function properly.
**
**  Compilation is trivial -- just "cc t_seteuid.c".
*/

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>

#ifdef __hpux
#define seteuid(e)	setresuid(-1, e, -1)
#endif

main()
{
	uid_t realuid = getuid();

	printuids("initial uids", realuid, 0);

	if (geteuid() != 0)
	{
		printf("re-run setuid root\n");
		exit(1);
	}

	if (seteuid(1) < 0)
		printf("seteuid(1) failure\n");
	printuids("after seteuid(1)", realuid, 1);

	if (geteuid() != 1)
		printf("MAYDAY!  Wrong effective uid\n");

	/* do activity here */

	if (seteuid(0) < 0)
		printf("seteuid(0) failure\n");
	printuids("after seteuid(0)", realuid, 0);

	if (geteuid() != 0)
		printf("MAYDAY!  Wrong effective uid\n");
	if (getuid() != realuid)
		printf("MAYDAY!  Wrong real uid\n");
	printf("\n");

	if (seteuid(2) < 0)
		printf("seteuid(2) failure\n");
	printuids("after seteuid(2)", realuid, 2);

	if (geteuid() != 2)
		printf("MAYDAY!  Wrong effective uid\n");

	/* do activity here */

	if (seteuid(0) < 0)
		printf("seteuid(0) failure\n");
	printuids("after seteuid(0)", realuid, 0);

	if (geteuid() != 0)
		printf("MAYDAY!  Wrong effective uid\n");
	if (getuid() != realuid)
		printf("MAYDAY!  Wrong real uid\n");

	exit(0);
}

printuids(str, r, e)
	char *str;
	int r, e;
{
	printf("%s (should be %d/%d): r/euid=%d/%d\n", str, r, e,
		getuid(), geteuid());
}
