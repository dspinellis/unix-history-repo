#include <stdio.h>
#include "PasswordLookup.h"

extern Passwd *getpwnam(), *getpwuid();

Passwd empty = { "", "", 0, 0, 0, "", "" };

Passwd
LookupUser(user)
	String user;
{
	Passwd *pw;

	pw = getpwnam(user);
	if (pw == 0)
		return (empty);
	else
		return (*pw);
}

Passwd
LookupUid(uid)
	Cardinal uid;
{
	Passwd *pw;

	pw = getpwuid(uid);
	if (pw == 0)
		return (empty);
	else
		return (*pw);
}
