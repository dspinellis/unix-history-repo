#include <stdio.h>
#include <grp.h>
#include <pwd.h>

struct	group	*getgrnam(), *grp;
struct	passwd	*getpwuid(), *pwd;
char	*getpass(), *crypt();

main(argc,argv)
int	argc;
char	**argv;
{
	register i;
	if(argc != 2) {
		printf("usage: newgrp groupname\n");
		exit(13);
	}
	if((grp=getgrnam(argv[1])) == NULL) {
		printf("%s: no such group\n", argv[1]);
		exit(13);
	}
	if((pwd=getpwuid(getuid())) == NULL) {
		printf("You do not exist!\n");
		exit(13);
	}
	for(i=0;grp->gr_mem[i];i++) 
		if(strcmp(grp->gr_mem[i], pwd->pw_name) == 0)
			break;
	if(grp->gr_mem[i] == 0 && strcmp(grp->gr_name,"other")) {
		printf("Sorry\n");
		exit(13);
	}

	if(grp->gr_passwd[0] != '\0' && pwd->pw_passwd[0] == '\0') {
		if(strcmp(grp->gr_passwd, crypt(getpass("Password:"),grp->gr_passwd)) != 0) {
			printf("Sorry\n");
			exit(13);
		}
	}
	if(setgid(grp->gr_gid) < 0) {
		perror("setgid");
		exit(13);
	}
	setuid(getuid());
	for (i=3; i<15; i++)
		close(i);
	execl((pwd->pw_shell[0]?pwd->pw_shell:"/bin/sh"), "-i", 0);
	printf("No shell!\n");
	exit(0);
}
