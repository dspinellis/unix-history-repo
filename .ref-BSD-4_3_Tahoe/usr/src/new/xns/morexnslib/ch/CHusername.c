#ifndef lint
static char *rcsid = "$Header: CHusername.c,v 1.1 87/05/11 09:59:41 ed Exp $";
#endif lint

/* contains:
 * CH_NameToUser
 */

#ifndef CHUSERIDFILE
#define CHUSERIDFILE "/usr/new/lib/xnscourier/chusermap"
#endif

#include <stdio.h>
#include <sys/types.h>
#include <netns/ns.h>
#include <xnscourier/Clearinghouse2.h>
#include <ctype.h>
#include <ndbm.h>
extern char *rindex(), *CH_NameToString();

/*
 * produce a UNIX user name by table lookup given a CHS name
 */
char *
CH_NameToUser(chs_name)
	Clearinghouse2_ThreePartName chs_name;
{
	static char userid[12];
	DBM *db;
	datum cptr, uptr;
	register char *ptr;
	Clearinghouse2_ObjectName chdefault;
	FILE *dbf;
	char charbuf[100];
	int n;

	uptr.dptr = NULL;
	cptr.dptr = CH_NameToString(chs_name);
	for (ptr =cptr.dptr; *ptr != '\0'; ptr++)
		if (islower(*ptr)) *ptr = toupper(*ptr);

	/* lookup user id in hashed database */
	if ( (db= dbm_open(CHUSERIDFILE, 0, 0)) != (DBM*)0 ) {
		cptr.dsize = strlen(cptr.dptr);
		uptr = dbm_fetch(db, cptr);
		dbm_close(db);
	}
	/* lookup user id in text database using linear search */
	else if ( (dbf = fopen(CHUSERIDFILE, 0)) >= 0 ) {
		while ((n=fscanf(dbf, "%[^\t\n]\t%s\n",charbuf,userid)) >= 0) {
			if (n != 2) continue;
			for (ptr=charbuf; *ptr != '\0'; ptr++)
				if (islower(*ptr)) *ptr = toupper(*ptr);
			if (strcmp(charbuf,cptr.dptr) == 0) {
				uptr.dptr = userid;
				break;	/* found it! */
			}
		}
		fclose(dbf);
	}
	if (uptr.dptr != NULL) return(uptr.dptr);

	/* default case */
	chdefault.object = chdefault.domain = chdefault.organization = NULL;
	CH_NameDefault(&chdefault);
	if ( strcmp(chs_name.domain,chdefault.domain) != 0 ||
	     strcmp(chs_name.organization,chdefault.organization) != 0)
		return("nobody");
	else if ( (ptr= rindex(chs_name.object, ' ')) == 0 )
		return(chs_name.object);
	else
		return(ptr+1);
}
