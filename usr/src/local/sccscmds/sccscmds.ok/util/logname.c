#include <sys/types.h>
#include <pwd.h>
extern struct passwd *getpwuid();
extern char *getlogin();
extern char *getenv();

char *
logname()
{
	register struct passwd *pw;
	register char *cp;

	cp = getenv("USER");
	if (cp != 0 && *cp != '\0')
		return (cp);
	cp = getlogin();
	if (cp != 0 && *cp != '\0')
		return (cp);
	setpwent();
	pw=getpwuid(getuid());
	endpwent();
	return(pw->pw_name);
}
