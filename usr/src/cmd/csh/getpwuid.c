/* Copyright (c) 1979 Regents of the University of California */
#include <pwd.h>

struct passwd *
getpwuid(uid)
register uid;
{
	register struct passwd *p;
	struct passwd *getpwent();

	setpwent();
	while( (p = getpwent()) && p->pw_uid != uid );
	endpwent();
	return(p);
}
