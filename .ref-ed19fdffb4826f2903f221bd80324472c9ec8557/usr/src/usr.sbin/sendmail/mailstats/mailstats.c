# include "sendmail.h"

SCCSID(@(#)mailstats.c	3.4		%G%);

/*
**  MAILSTATS -- print mail statistics.
**
**	Flags:
**		-Ffile		Name of statistics file.
**
**	Exit Status:
**		zero.
*/

main(argc, argv)
	char  **argv;
{
	register int fd;
	struct statistics stat;
	char *sfile = "/usr/lib/sendmail.st";
	register int i;
	extern char *ctime();

	fd = open(sfile, 0);
	if (fd < 0)
	{
		perror(sfile);
		exit(EX_NOINPUT);
	}
	if (read(fd, &stat, sizeof stat) != sizeof stat ||
	    stat.stat_size != sizeof stat)
	{
		(void) sprintf(stderr, "File size change\n");
		exit(EX_OSERR);
	}

	printf("Statistics from %s", ctime(&stat.stat_itime));
	printf(" M msgsfr bytes_from  msgsto   bytes_to\n");
	for (i = 0; i < MAXMAILERS; i++)
	{
		if (stat.stat_nf[i] == 0 && stat.stat_nt[i] == 0)
			continue;
		printf("%2d ", i);
		printf("%6d %10dK ", stat.stat_nf[i], stat.stat_bf[i]);
		printf("%6d %10dK\n", stat.stat_nt[i], stat.stat_bt[i]);
	}
}
