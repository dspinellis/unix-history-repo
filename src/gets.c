/* Copyright (c) 1979 Regents of the University of California */
#include <retrofit.h>
#include <stdio.h>

/*
 * gets [ default ]
 *
 *	read a line from standard input, echoing to std output
 *	if an error occurs just return "default"
 *	if no default and error exit abnormally
 */
main(argc, argv)
	int argc;
	char *argv[];
{
	char buf[BUFSIZ];
	
	if (gets(buf) == NULL) {
		if (argc == 1)
			exit(1);
		buf[0] = 0;
	}
	printf("%s\n", buf);
	exit(0);
}
