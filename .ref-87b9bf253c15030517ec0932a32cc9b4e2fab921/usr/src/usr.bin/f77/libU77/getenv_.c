/*
char id_getenv[] = "@(#)getenv_.c	1.1";
 *
 * return environment variables
 *
 * calling sequence:
 *	character*20 evar
 *	call getenv (ENV_NAME, evar)
 * where:
 *	ENV_NAME is the name of an environment variable
 *	evar is a character variable which will receive
 *		the current value of ENV_NAME,
 *		or all blanks if ENV_NAME is not defined
 */

extern char **environ;

getenv_(fname, value, flen, vlen)
char *value, *fname;
long int vlen, flen;
{
	register char *ep, *fp;
	register char **env = environ;
	int i;

	while (ep = *env++) {
		for (fp=fname, i=0; i <= flen; i++) {
			if (i == flen || *fp == ' ') {
				if (*ep++ == '=') {
					b_char(ep, value, vlen);
					return(0);
				}
				else break;
			}
			else if (*ep++ != *fp++) break;
		}
	}
	b_char(" ", value, vlen);
	return(0);
}
