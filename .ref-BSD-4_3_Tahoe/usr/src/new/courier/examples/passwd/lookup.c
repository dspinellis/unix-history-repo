/*
 * Sample program to access remote password lookup.
 *
 * Usage: lookup machine username
 */
#include <stdio.h>
#include "PasswordLookup.h"

main(argc, argv)
	int argc;
	char **argv;
{
	Passwd passwd;

	if (argc != 3) {
		fprintf(stderr, "Usage: %s machine username\n", argv[0]);
		exit(1);
	}
	BindPasswordLookupToMachine(argv[1]);
	passwd = LookupUser(argv[2]);
	if (strcmp(passwd.pw_name, argv[2]) != 0)
		printf("User %s unknown on %s.\n", argv[2], argv[1]);
	else
		display(&passwd);
}

display(p)
	Passwd *p;
{
	printf("%s:%s:%d:%d:%s:%s:%s\n",
		p->pw_name,
		p->pw_passwd,
		p->pw_uid,
		p->pw_gid,
		p->pw_gecos,
		p->pw_dir,
		p->pw_shell);
}
