#include "uucp.h"

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

	if ((fp1 = fopen(f1, "r")) == NULL)
		return(FAIL);
	if ((fp2 = fopen(f2, "w")) == NULL) {
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
