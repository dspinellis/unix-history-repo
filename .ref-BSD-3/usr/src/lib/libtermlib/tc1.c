/*
 * tc1 [term]
 * dummy program to test termlib.
 * gets entry, counts it, and prints it.
 */
#include <stdio.h>
char buf[1024];
char *getenv();

main(argc, argv) char **argv; {
	char *p;
	int rc;

	if (argc < 2)
		p = getenv("TERM");
	else
		p = argv[1];
	rc = tgetent(buf,p);
	printf("tgetent returns %d, len=%d, text=\n'%s'\n",rc,strlen(buf),buf);
}
