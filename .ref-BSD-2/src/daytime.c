/* Copyright (c) 1979 Regents of the University of California */
/*
 * Print out the time to a human desirable
 * accuracy.
 *
 * Author: Kurt Shoens (UCB) July 11, 1978
 */

char *hourNames[] {
	"midnight",
	"one",
	"two",
	"three",
	"four",
	"five",
	"six",
	"seven",
	"eight",
	"nine",
	"ten",
	"eleven",
	"noon",
	0
};

char *quadrants[] {
	"o' clock",
	"quarter past",
	"half past",
	"quarter 'til",
	0
};

main(argc, argv)
	char **argv;
{
	int hour, *t, min, q, tv[2];
	char *hs, *qs;

	time(tv);
	t = localtime(tv);
	hour = t[2];
	min = t[1];
	if (argc >= 3) {
		hour = atoi(argv[1]);
		min = atoi(argv[2]);
	}
	q = (min+8) / 15;
	if (q >= 4) {
		q = 0;
		hour++;
	}
	if (q > 2)
		hour++;
	if (hour >= 24)
		hour = 0;
	if (hour > 12)
		hour =- 12;
	hs = hourNames[hour];
	qs = quadrants[q];
	if ((hour == 12 || hour == 0) && q == 0) {
		*hs = raise(*hs);
		printf("%s\n", hs);
		exit(0);
	}
	if (q == 0) {
		*hs = raise(*hs);
		printf("%s %s\n", hs, qs);
		exit(0);
	}
	*qs = raise(*qs);
	printf("%s %s\n", qs, hs);
	exit(0);
}

/*
 * Return the upper case version of the possibly
 * lower case letter.
 */

raise(c)
{
	if (c >= 'a' && c <= 'z')
		c =+ 'A' - 'a';
	return(c);
}
