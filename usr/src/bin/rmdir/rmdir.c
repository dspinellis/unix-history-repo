static char *sccsid = "@(#)rmdir.c	4.3 (Berkeley) %G%";
/*
 * Remove directory
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <ndir.h>
#include <stdio.h>

int	Errors = 0;
char	*rindex();
char	*strcat();
char	*strcpy();

main(argc,argv)
int argc;
char **argv;
{

	if(argc < 2) {
		fprintf(stderr, "rmdir: arg count\n");
		exit(1);
	}
	while(--argc)
		rmdir(*++argv);
	exit(Errors!=0);
}

rmdir(d)
char *d;
{
	char	*np, name[BUFSIZ];
	struct	stat	st, cst;
	struct	direct	*dp;
	DIR	*dirp;

	strcpy(name, d);

	/* eat trailing slashes */
	np = &(name[strlen(name)-1]);
	while (*np == '/' && np != name) {
		*np = '\0';
		np--;
	}

	/* point after last slash */
	if((np = rindex(name, '/')) == NULL)
		np = name;
	else
		np++;

	if(!strcmp(np, ".") || !strcmp(np, "..")) {
		fprintf(stderr, "rmdir: cannot remove . or ..\n");
		++Errors;
		return;
	}
	if(stat(name,&st) < 0) {
		fprintf(stderr, "rmdir: %s non-existent\n", name);
		++Errors;
		return;
	}
	if (stat("", &cst) < 0) {
		fprintf(stderr, "rmdir: cannot stat \"\"");
		++Errors;
		exit(1);
	}
	if((st.st_mode & S_IFMT) != S_IFDIR) {
		fprintf(stderr, "rmdir: %s not a directory\n", name);
		++Errors;
		return;
	}
	if(st.st_ino==cst.st_ino &&st.st_dev==cst.st_dev) {
		fprintf(stderr, "rmdir: cannot remove current directory\n");
		++Errors;
		return;
	}
	if((dirp = opendir(name)) == NULL) {
		fprintf(stderr, "rmdir: %s unreadable\n", name);
		++Errors;
		return;
	}
	while((dp = readdir(dirp)) != NULL) {
		if(dp->d_ino == 0) continue;
		if(!strcmp(dp->d_name, ".") || !strcmp(dp->d_name, ".."))
			continue;
		fprintf(stderr, "rmdir: %s not empty\n", name);
		++Errors;
		closedir(dirp);
		return;
	}
	closedir(dirp);
	strcat(name, "/.");
	if((access(name, 0)) < 0) {		/* name/. non-existent */
		strcat(name, ".");
		goto unl;
	}
	strcat(name, ".");
	if((access(name, 0)) < 0)		/* name/.. non-existent */
		goto unl2;
	if(access(name, 02)) {
		name[strlen(name)-3] = '\0';
		fprintf(stderr, "rmdir: %s: no permission\n", name);
		++Errors;
		return;
	}
unl:
	unlink(name);	/* unlink name/.. */
unl2:
	name[strlen(name)-1] = '\0';
	unlink(name);	/* unlink name/.  */
	name[strlen(name)-2] = '\0';
	if (unlink(name) < 0) {
		fprintf(stderr, "rmdir: %s not removed\n", name);
		++Errors;
	}
}
