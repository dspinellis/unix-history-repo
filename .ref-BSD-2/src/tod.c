/* Copyright (c) 1979 Regents of the University of California */
/*
 * Print out a meaningful phrase depending on the time of
 * day.
 *
 * Author: Robert Toxen (UCB) 7/10/78
 */

/*
 * The following structure parameterizes the
 * whole she-bang.
 */

struct daytime {
	int	d_start;		/* starting hour */
	char	*d_mesg;		/* applicable message */
} daytime[] {
	0,	"It's late",
	4,	"You really should be home in bed",
	7,	"Good morning",
	12,	"Good afternoon",
	18,	"Good evening",
	22,	"Good night",
	50,	"panic: time of day bug",
	-1,	0
};

/*
 * The special case structure:  if the hour is exactly
 * one of these, print the corresponding message.
 */

struct special {
	int	s_time;			/* Applicable hour */
	char	*s_mesg;		/* Corresponding mesg */
} special[] {
	12,	"Had lunch yet?",
	17,	"You should be eating dinner",
	0,	"It's past midnight",
	-1,	0
};

main(argc, argv)
	char **argv;
{
	register struct daytime *dp;
	register struct special *sp;
	register int hour;
	int tv[2], *t;

	if (argc > 1)
		hour = atoi(argv[1]);
	else {
		time(tv);
		t = localtime(tv);
		hour = t[2];
	}
	for (sp = &special[0]; sp->s_time != -1; sp++)
		if (sp->s_time == hour) {
			printf("%s\n", sp->s_mesg);
			exit(1);
		}
	for (dp = &daytime[0]; dp->d_start != -1; dp++)
		if (hour < (dp+1)->d_start) {
			printf("%s\n", dp->d_mesg);
			exit(0);
		}

	/*
	 * Why didn't this thing print anything !?!
	 */

	printf("No message for time!?!\n");
	abort();
}
