static char *sccsid = "@(#)rm.c	4.7 (Berkeley) %G%";
int	errcode;

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ndir.h>

char	*sprintf();

main(argc, argv)
char *argv[];
{
	register char *arg;
	int fflg, iflg, rflg;

	fflg = 0;
	if (isatty(0) == 0)
		fflg++;
	iflg = 0;
	rflg = 0;
	while(argc>1 && argv[1][0]=='-') {
		arg = *++argv;
		argc--;

		/*
		 *  all files following a null option are considered file names
		 */
		if (*(arg+1) == '\0') break;

		while(*++arg != '\0')
			switch(*arg) {
			case 'f':
				fflg++;
				break;
			case 'i':
				iflg++;
				break;
			case 'r':
				rflg++;
				break;
			default:
				printf("rm: unknown option %s\n", *argv);
				exit(1);
			}
	}
	while(--argc > 0) {
		if(!strcmp(*++argv, "..")) {
			fprintf(stderr, "rm: cannot remove `..'\n");
			continue;
		}
		rm(*argv, fflg, rflg, iflg, 0);
	}

	exit(errcode);
}

rm(arg, fflg, rflg, iflg, level)
char arg[];
{
	struct stat buf;
	struct direct *dp;
	DIR *dirp;
	char name[BUFSIZ];
	int d;

	if(lstat(arg, &buf)) {
		if (fflg==0) {
			printf("rm: %s nonexistent\n", arg);
			++errcode;
		}
		return;
	}
	if ((buf.st_mode&S_IFMT) == S_IFDIR) {
		if(rflg) {
			if (access(arg, 02) < 0) {
				if (fflg==0)
					printf("%s not changed\n", arg);
				errcode++;
				return;
			}
			if(iflg && level!=0) {
				printf("remove directory %s? ", arg);
				if(!yes())
					return;
			}
			if((dirp = opendir(arg)) == NULL) {
				printf("rm: cannot read %s?\n", arg);
				exit(1);
			}
			while((dp = readdir(dirp)) != NULL) {
				if(dp->d_ino != 0 && !dotname(dp->d_name)) {
					sprintf(name, "%s/%s", arg, dp->d_name);
					rm(name, fflg, rflg, iflg, level+1);
				}
			}
			closedir(dirp);
			errcode += rmdir(arg, iflg);
			return;
		}
		printf("rm: %s directory\n", arg);
		++errcode;
		return;
	}

	if(iflg) {
		printf("rm: remove %s? ", arg);
		if(!yes())
			return;
	}
	else if(!fflg) {
		if (access(arg, 02)<0) {
			printf("rm: override protection %o for %s? ", buf.st_mode&0777, arg);
			if(!yes())
				return;
		}
	}
	if(unlink(arg) && (fflg==0 || iflg)) {
		printf("rm: %s not removed\n", arg);
		++errcode;
	}
}

dotname(s)
char *s;
{
	if(s[0] == '.')
		if(s[1] == '.')
			if(s[2] == '\0')
				return(1);
			else
				return(0);
		else if(s[1] == '\0')
			return(1);
	return(0);
}

rmdir(f, iflg)
char *f;
{
	int status, i;

	if(dotname(f))
		return(0);
	if(iflg) {
		printf("rm: remove %s? ", f);
		if(!yes())
			return(0);
	}
	while((i=fork()) == -1)
		sleep(3);
	if(i) {
		wait(&status);
		return(status);
	}
	execl("/bin/rmdir", "rmdir", f, 0);
	execl("/usr/bin/rmdir", "rmdir", f, 0);
	printf("rm: can't find rmdir\n");
	exit(1);
}

yes()
{
	int i, b;

	i = b = getchar();
	while(b != '\n' && b != EOF)
		b = getchar();
	return(i == 'y');
}
