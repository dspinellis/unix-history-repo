/*	log.c	4.2	81/11/20	*/
#include "tip.h"

#ifdef ACULOG
static FILE *flog = NULL;

/*
 * Log file maintenance routines
 */

logent(group, num, acu, message)
	char *group, *num, *acu, *message;
{
	char *user, *timestamp;
	struct passwd *pwd;
	long t;

	if (flog == NULL)
		return;
	if (!lock(value(LOCK))) {
		fprintf(stderr, "can't lock up accounting file\r\n");
		return;
	}
	if ((user = getlogin()) == NOSTR)
		if ((pwd = getpwuid(getuid())) == NOPWD)
			user = "???";
		else
			user = pwd->pw_name;
	t = time(0);
	timestamp = ctime(&t);
	timestamp[24] = '\0';
	fprintf(flog, "%s (%s) <%s, %s, %s> %s\n",
		user, timestamp, group,
#ifdef PRISTINE
		"",
#else
		num,
#endif
		acu, message);
	fflush(flog);
	unlock();
}

loginit()
{
	if ((flog = fopen(value(LOG), "a")) == NULL)
		fprintf(stderr, "can't open log file\r\n");
}
#endif
