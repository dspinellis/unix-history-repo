#include "uucp.h"
#include <pwd.h>


/*******
 *	guinfo(uid, name, path)	get passwd file info for uid
 *	int uid;
 *	char *path, *name;
 *
 *	return codes:  0  |  FAIL
 */

guinfo(uid, name, path)
int uid;
char *path, *name;
{
	struct passwd *pwd;
	struct passwd *getpwuid();

	if ((pwd = getpwuid(uid)) == NULL) {
		/* can not find uid in passwd file */
		*path = '\0';
		return(FAIL);
	}

	strcpy(path, pwd->pw_dir);
	strcpy(name, pwd->pw_name);
	return(0);
}


/***
 *	gninfo(name, uid, path)	get passwd file info for name
 *	char *path, *name;
 *	int *uid;
 *
 *	return codes:  0  |  FAIL
 */

gninfo(name, uid, path)
char *path, *name;
int *uid;
{
	struct passwd *pwd;
	struct passwd *getpwnam();

	if ((pwd = getpwnam(name)) == NULL) {
		/* can not find name in passwd file */
		*path = '\0';
		return(FAIL);
	}

	strcpy(path, pwd->pw_dir);
	*uid = pwd->pw_uid;
	return(0);
}


