/*
 * Remote date and time.
 *
 * Usage: datime machine
 */
#include <stdio.h>
#include "Time.h"

main(argc, argv)
	int argc;
	char **argv;
{
	Time time;

	if (argc != 2) {
		fprintf(stderr, "Usage: %s machine\n", argv[0]);
		exit(1);
	}
	BindTimeToMachine(argv[1]);
	time = LocalTime();
	display(&time, "Local time");
	time = GMTime();
	display(&time, "GMT");
}

display(tp, msg)
	Time *tp;
	char *msg;
{
	printf("%s: %d:%02d:%02d %d/%d/%d\n",
		msg, tp->tm_hour, tp->tm_min, tp->tm_sec,
		tp->tm_mon + 1, tp->tm_mday, tp->tm_year);
}
