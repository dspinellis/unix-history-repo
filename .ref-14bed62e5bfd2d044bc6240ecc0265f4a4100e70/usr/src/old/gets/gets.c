static char *sccsid = "@(#)gets.c	4.2 (Berkeley) %G%";
#include <stdio.h>

/*
 * gets [ default ]
 *
 *      read a line from standard input, echoing to std output
 *      if an error occurs just return "default"
 *      if no default and error exit abnormally
 */
main(argc, argv)
	int argc;
	char *argv[];
{
	char buf[BUFSIZ];
	
	setbuf(stdin, NULL);
	if (gets(buf) == NULL || buf[0] < ' ') {
		if (argc == 1)
			exit(1);
		strcpy(buf,argv[1]);
	}
	printf("%s\n", buf);
	exit(0);
}
