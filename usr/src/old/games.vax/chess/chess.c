
static char sccsid[] = "	chess.c	4.2	88/10/19	";

#include <stdio.h>
main()
{
	execl("/usr/games/lib/compat", "chess", "/usr/games/lib/chess", 0);
	fprintf(stderr, "chess: can't find /usr/games/lib/compat.\n");
	exit(1);
}
