#include <retrofit.h>
#include <pwd.h>

getenv(name)
	char *name;
{
	register int ttyno;
	static char hgot;

top:
	if (strcmp(name, "TERM") == 0) {
		if (!hgot)
			goto getit;
		return (hsgettype());
	}
	if (strcmp(name, "HOME") == 0) {
		if (getuid() == 0)
			return ("/");
		if (!hgot)
			goto getit;
		return (hgethome());
	}
	return (0);
getit:
	ttyno = ttyn(2);
	if (ttyno < 0 || ttyno == 'x' || hget(ttyno) < 0) {
		register struct passwd *p = getpwuid(getuid());
		extern struct htmp {
			int	uid;
			char	home[28];
			int	ttytype;
		} hentry;
		strcpy(hentry.home, p->pw_dir);
		hentry.ttytype = 'xx';
	}
	hgot = 1;
	goto top;
}
