/* Copyright (c) 1979 Regents of the University of California */
#include <utmp.h>

struct utmp reboot;

main()
{

	if (getuid())
		exit(1);
	close(1);
	open("/usr/adm/wtmp", 1);
	strcpy(reboot.ut_line, "tty~");
	strcpy(reboot.ut_name, "reboot");
	time(&reboot.ut_time);
	lseek(1, (long) 0, 2);
	write(1, (char *) &reboot, sizeof reboot);
	exit(0);
}
