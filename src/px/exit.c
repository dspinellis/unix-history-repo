/* (c) 1979 Regents of the University of California */
exit(c)
	int c;
{

	if (fork() == 0) {
		char *cp = "-00";
		if (c > 10) {
			cp[1] |= (c / 10) % 10;
			cp[2] |= c % 10;
		} else {
			cp[1] |= c;
			cp[2] = 0;
		}
		execl("/usr/lib/gather", "gather", cp, "px", 0);
	}
	_exit(c);
}
