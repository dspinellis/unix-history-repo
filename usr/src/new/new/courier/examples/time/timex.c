/*
 * Remote date and time.
 *
 * Usage: timex machine1 ... machineN
 */
#include <stdio.h>
#include "Time.h"

main(argc, argv)
	int argc;
	char **argv;
{
	char *machine;
	Time time;

	while (--argc > 0) {
		machine = *++argv;
		printf("%s:\n", machine);
		time = LocalTime(machine);
		display(&time, "Local time");
		time = GMTime(machine);
		display(&time, "GMT");
	}
}

display(tp, msg)
	Time *tp;
	char *msg;
{
	printf("%s: %d:%02d:%02d %d/%d/%d\n",
		msg, tp->tm_hour, tp->tm_min, tp->tm_sec,
		tp->tm_mon + 1, tp->tm_mday, tp->tm_year);
}
