
static char sccsid[] = "	zork.c	4.1	82/10/24	";

#include <stdio.h>
/*
 * Dungeon - open UP dungeon
 */

#ifdef CHECKUID
int users[] = {
	522,    /* sa */
	164,    /* Leiby */
	229,    /* richards */
	264,    /* marshall */
	1099,   /* wizard */
	425,    /* clm */
	15,     /* mowle */
	32,     /* ghg */
	27,	/* qtip (zager) */
	530,	/* mike */
	16,	/* bc */
	333,	/* pdh */
	230,	/* wa1yyn */
	19,	/* joe
	43,	/* bruner */
	308,	/* gedeon (watch him closely!) */
	429,	/* mayhew */
	743,	/* alicia */
	367,	/* feather */
	85,	/* clark bar */
	382,	/* malcolm */
	99,	/* jones */
	636,    /* gfg */
	0 };
#endif

main()
{

	register int *up;
	register uid;
	int fd3, fd4, fd5;

#ifdef CHECKUID

	uid = getuid();
	for (up=users; *up; up++)
		if (*up == uid)
			goto ok;
	printf("You are not a Wizard!\n");
	exit();
#endif
	/*
	 * open up files needed by program
	 * look in current directory first, then try default names
	 * The following files must be as follows:
	 * "dtext.dat" open read-only on fd 3
	 * "dindex.dat open read-only on fd 4 (maybe this file isn't used)
	 * "doverlay" open read-only on fd 5 (put this file on fast disk)
	 */
	close(3);
	close(4);
	close(5);
	if ((fd3 = open("dtext.dat", 0)) < 0)
		if ((fd3 = open("/usr/games/lib/dtext.dat", 0)) < 0)
			error("Can't open dtext.dat\n");

	if ((fd4 = open("dindex.dat", 0)) < 0)
		if ((fd4 = open("/usr/games/lib/dindex.dat", 0)) < 0)
			error("Can' open dindex.dat\n");

	if ((fd5 = open("doverlay", 0)) < 0)
		if ((fd5 = open("/tmp/nedtmp/doverlay", 0)) < 0)
			if ((fd5 = open("/usr/games/lib/doverlay", 0)) < 0)
				error("Can't open doverlay\n");

	if (fd3 != 3 || fd4 != 4 || fd5 != 5)
		error("Files opened on wrong descriptors\n");

	signal(2,1);

	printf("You are in an open field west of a big white house with a boarded\n");
	printf("front door.\n");
	printf("There is a small mailbox here.\n>");
	fflush(stdout);
#ifdef pdp11
	execl("dungeon","zork", 0);
	execl("/usr/games/lib/dungeon","zork", 0);
#else
	if( (uid=open("dungeon", 0)) > 0 ) {
		close(uid);
		execlp("compat", "zork", "dungeon", 0);
		execlp("/usr/games/lib/compat", "zork", "dungeon", 0);
	}
	execlp("compat", "zork", "/usr/games/lib/dungeon", 0);
	execlp("/usr/games/lib/compat", "zork", "/usr/games/lib/dungeon", 0);
#endif
	printf("Can't start dungeons.\n");
	exit(0);
}
error(s)
char *s;
{
	printf("%s", s);
	exit(1);
}
