static char sccsid[] = "@(#)environ.c	4.1	(Berkeley)	9/12/82";

/* sccs id variable */
static char *env_sid = "@(#)environ.c	1.2";

# include "defs.h"

/* 
	environ.c

	examine the environment variables and see if they
	have network login name and password information
*/

envloginpasswd(mch,sn,spasswd)
register char *sn, *spasswd;{
	register char *senv;
	char stemp[30], fgetlogin;
	char *envlook();
# ifdef V6
	return;			/* doesn't work on V6 */
# endif
	senv = envlook(mch);
	if(senv == NULL)return;
	if(!isalpha(senv[0]))return;	/* not login name, ignore */
	fgetlogin = (sn[0] == 0);
	while(*senv && *senv != ','){
		if(fgetlogin)*sn++ = *senv;
		else if(*sn++ != *senv)return;
		senv++;
	}
	strcpy(stemp,++senv);
	mkpwclear(stemp,mch,spasswd);
}
extern char **environ;
char *envlook(mch){
	static char svalue[100];
	register char *s, *sv, **env;
	env = environ;
	while(*env != NULL){
		s = *env++;
		if(s[0] == 'M' && s[1] == 'A' && s[2] == 'C' && s[3] == 'H'){
			sv = s+4;
			while(*sv && *sv != '=')sv++;
			*sv++ = 0;
			if(lookup(s+4) == mch){
				strcpy(svalue,sv);
				return(svalue);
			}
		}
	}
	return(NULL);
}
/*
	reverse the sfrom string, copying into sto.
	sfrom and sto may not be the same string
*/
sreverse(sto,sfrom)
register char *sto, *sfrom;
{
	register int i;
	i = strlen(sfrom);
	while(i >= 0)
		*sto++ = sfrom[i--];
}
/* 
	mkenvkey
	
	make key to encrypt environment passwds.
	return NULL if error
*/
static char *mkenvkey(mch)
char mch;
{
	static char skey[40];
	register struct utmp *putmp;
	char stemp[40], stemp1[40], sttyname[30];
	register char *sk,*p;

	if(isatty(2))strcpy(sttyname,ttyname(2));
	else if(isatty(0))strcpy(sttyname,ttyname(0));
	else if(isatty(1))strcpy(sttyname,ttyname(1));
	else return(NULL);
	putmp = getutmp(sttyname);
	if(putmp == NULL) return(NULL);
	sk = skey;
	p = putmp->ut_line;
	while(*p)*sk++ = *p++;
	*sk++ = mch;
	sprintf(stemp,"%ld",putmp->ut_time);
	sreverse(stemp1,stemp);
	p = stemp1;
	while(*p)*sk++ = *p++;
	*sk = 0;
	return(skey);
}
/*
	make an encrypted passwd
*/
mkpwunclear(spasswd,mch,sencpasswd)
	char mch,*spasswd,*sencpasswd;
{
	register char *skey;
	if(spasswd[0] == 0){
		sencpasswd[0] = 0;
		return;
	}
	skey = mkenvkey(mch);
	if(skey == NULL){
		fprintf(stderr,"Can't make key\n");
		exit(EX_OSERR);
	}
	nbsencrypt(spasswd,skey,sencpasswd);
}
/* 
	make an unecrypted passwd
*/
mkpwclear(sencpasswd,mch,spasswd)
char mch,*spasswd,*sencpasswd;
{
	register char *skey;
	if(sencpasswd[0] == 0){
		spasswd[0] = 0;
		return;
	}
	skey = mkenvkey(mch);
	if(skey == NULL){
		fprintf(stderr,"Can't make key\n");
		exit(EX_OSERR);
	}
	nbsdecrypt(sencpasswd,skey,spasswd);
}
