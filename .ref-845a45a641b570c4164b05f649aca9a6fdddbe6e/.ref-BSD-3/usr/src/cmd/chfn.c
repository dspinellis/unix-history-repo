/*
 * chfn - change full name (or other info in gecos field)
 */
#include <stdio.h>
#include <signal.h>
#include <pwd.h>

char	passwd[] = "/etc/passwd";
char	temp[]	 = "/etc/ptmp";
struct	passwd *pwd;
struct	passwd *getpwent();
int	endpwent();
char	*crypt();
char	*getpass();
char	*pw;
char	pwbuf[10];
char	buf[BUFSIZ];

main(argc, argv)
char *argv[];
{
	char *p;
	int i;
	char saltc[2];
	long salt;
	int u,fi,fo;
	int insist;
	int ok, flags;
	int c;
	int pwlen;
	FILE *tf;

	insist = 0;
	if (argc != 3) {
		printf("Usage: chfn user full-name\n");
		goto bex;
	}
	if (index(argv[2], ':') || index(argv[2], '\n')) {
		printf("Illegal character in new string\n");
		exit(1);
	}
	while((pwd=getpwent()) != NULL){
		if(strcmp(pwd->pw_name,argv[1]) == 0){
			u = getuid();
			if(u!=0 && u != pwd->pw_uid){
				printf("Permission denied.\n");
				goto bex;
				}
			break;
			}
		}
	endpwent();
	signal(SIGHUP, 1);
	signal(SIGINT, 1);
	signal(SIGQUIT, 1);

	if(access(temp, 0) >= 0) {
		printf("Temporary file busy -- try again\n");
		goto bex;
	}
	if((tf=fopen(temp,"w")) == NULL) {
		printf("Cannot create temporary file\n");
		goto bex;
	}

/*
 *	copy passwd to temp, replacing matching lines
 *	with new shell.
 */

	while((pwd=getpwent()) != NULL) {
		if(strcmp(pwd->pw_name,argv[1]) == 0) {
			u = getuid();
			if(u != 0 && u != pwd->pw_uid) {
				printf("Permission denied.\n");
				goto out;
			}
			pwd->pw_gecos = argv[2];
		}
		fprintf(tf,"%s:%s:%d:%d:%s:%s:%s\n",
			pwd->pw_name,
			pwd->pw_passwd,
			pwd->pw_uid,
			pwd->pw_gid,
			pwd->pw_gecos,
			pwd->pw_dir,
			pwd->pw_shell);
	}
	endpwent();
	fclose(tf);

/*
 *	copy temp back to passwd file
 */

	if((fi=open(temp,0)) < 0) {
		printf("Temp file disappeared!\n");
		goto out;
	}
	if((fo=creat(passwd, 0644)) < 0) {
		printf("Cannot recreat passwd file.\n");
		goto out;
	}
	while((u=read(fi,buf,sizeof(buf))) > 0) write(fo,buf,u);

out:
	unlink(temp);

bex:
	exit(1);
}
