#include "uucp.h"
#include <sys/types.h>
#include <sys/stat.h>

/***
 *	xcp(f1, f2)	copy f1 to f2
 *	char *f1, *f2;
 *
 *	return - 0 ok  |  FAIL failed
 */

xcp(f1, f2)
char *f1, *f2;
{
	char buf[BUFSIZ];
	int len;
	FILE *fp1, *fp2;
	char *lastpart();
	char full[100];
	struct stat s;

	if ((fp1 = fopen(f1, "r")) == NULL)
		return(FAIL);
	strcpy(full, f2);
	if (stat(f2, &s) == 0) {
		/* check for directory */
		if ((s.st_mode & S_IFMT) == S_IFDIR) {
			strcat(full, "/");
			strcat(full, lastpart(f1));
		}
	}
	DEBUG(4, "full %s\n", full);
	if ((fp2 = fopen(full, "w")) == NULL) {
		fclose(fp1);
		return(FAIL);
	}
	while((len = fread(buf, sizeof (char), BUFSIZ, fp1)) > 0)
		fwrite(buf, sizeof (char), len, fp2);
	fclose(fp1);
	fclose(fp2);
	chmod(f2, 0666);
	return(0);
}


/*
 *	xmv(f1, f2)	move f1 to f2
 *	char * f1, *f2;
 *
 *	return  0 ok  |  FAIL failed
 */

xmv(f1, f2)
char *f1, *f2;
{
	int ret;

	if (link(f1, f2) < 0) {
		/*  copy file  */
		ret = xcp(f1, f2);
		if (ret == 0)
			unlink(f1);
		return(ret);
	}
	unlink(f1);
	return(0);
}
