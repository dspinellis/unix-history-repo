#include <retrofit.h>
#include <stdio.h>

main()
{
	char lbuf[BUFSIZ];

	if (chdir(getenv("HOME")) < 0)
		exit(1);
	if (freopen(".mail", "r", stdin) == NULL)
		exit(0);
	while(fgets(lbuf, sizeof lbuf, stdin) != NULL)
		if (lbuf[0] == 'F' && lbuf[1] == 'r' && lbuf[2] == 'o' && lbuf[3] == 'm')
			printf("%s", lbuf);
	exit(0);
}
