#include <stdio.h>
#include <ctype.h>
#include "msdos.h"

extern char *mcwd;

/*
 * Get name component of filename.  Translates name to upper case.  Returns
 * pointer to static area.
 */

char *
get_name(filename)
char *filename;
{
	char *s, *temp, *strcpy(), *strrchr(), buf[MAX_PATH];
	static char ans[13];

	strcpy(buf, filename);
	temp = buf;
					/* skip drive letter */
	if (buf[0] && buf[1] == ':')
		temp = &buf[2];
					/* find the last separator */
	if (s = strrchr(temp, '/'))
		temp = s + 1;
	if (s = strrchr(temp, '\\'))
		temp = s + 1;
					/* xlate to upper case */
	for (s = temp; *s; ++s) {
		if (islower(*s))
			*s = toupper(*s);
	}

	strcpy(ans, temp);
	return(ans);
}

/*
 * Get the path component of the filename.  Translates to upper case.
 * Returns pointer to a static area.  Doesn't alter leading separator,
 * always strips trailing separator (unless it is the path itself).
 */

char *
get_path(filename)
char *filename;
{
	char *s, *end, *begin, *strcpy(), *strrchr(), buf[MAX_PATH];
	char drive, *strcat();
	static char ans[MAX_PATH];
	int has_sep;

	strcpy(buf, filename);
	begin = buf;
					/* skip drive letter */
	drive = '\0';
	if (buf[0] && buf[1] == ':') {
		drive = (islower(buf[0])) ? toupper(buf[0]) : buf[0];
		begin = &buf[2];
	}
					/* if absolute path */
	if (*begin == '/' || *begin == '\\')
		ans[0] = '\0';
	else {
		if (!drive || drive == *mcwd)
			strcpy(ans, mcwd + 2);
		else
			strcpy(ans, "/");
	}
					/* find last separator */
	has_sep = 0;
	end = begin;
	if (s = strrchr(end, '/')) {
		has_sep++;
		end = s;
	}
	if (s = strrchr(end, '\\')) {
		has_sep++;
		end = s;
	}
					/* zap the trailing separator */
	*end = '\0';
					/* translate to upper case */
	for (s = begin; *s; ++s) {
		if (islower(*s))
			*s = toupper(*s);
		if (*s == '\\')
			*s = '/';
	}
					/* if separator alone, put it back */
	if (!strlen(begin) && has_sep)
		strcat(ans, "/");

	strcat(ans, begin);
	return(ans);
}

/*
 * get the drive letter designation
 */

char
get_drive(filename)
char *filename;
{
	if (*filename && *(filename + 1) == ':') {
		if (islower(*filename))
			return(toupper(*filename));
		else
			return(*filename);
	}
	else
		return(*mcwd);
}
