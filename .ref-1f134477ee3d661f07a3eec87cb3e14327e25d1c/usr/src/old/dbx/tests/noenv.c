/*
 * Exec a program with no environment.
 */

#include <stdio.h>

extern int errno;
extern char *getenv(), *index();

main (argc, argv)
int argc;
char *argv[];
{
    execvep(argv[1], &argv[1], 0);
    printf("exec failed, errno %d\n", errno);
}

execvep (name, argv, envp)
char *name, *argv[], *envp[];
{
    char *path;
    register char *cp;
    char fullname[1000];

    path = getenv("PATH");
    if (path == NULL) {
	path = "";
	cp = NULL;
    } else {
	cp = index(path, ':');
    }
    for (;;) {
	if (cp != NULL) {
	    *cp = '\0';
	}
	sprintf(fullname, "%s/%s", path, name);
	execve(fullname, argv, envp);
	if (cp != NULL) {
	    path = cp + 1;
	    cp = index(path, ':');
	} else {
	    break;
	}
    }
    return -1;
}
