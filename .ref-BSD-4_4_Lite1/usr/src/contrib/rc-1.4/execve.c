/* execve.c: an execve() for geriatric unices without #! */

/*
   NOTE: this file depends on a hack in footobar.c which places two free spots before
   av[][] so that execve does not have to call malloc.
*/

#include <errno.h>
#include "rc.h"

#define giveupif(x) { if (x) goto fail; }

extern int my_execve(const char *path, const char **av, const char **ev) {
	int fd, len, fst, snd, end;
	bool noarg;
	char pb[256]; /* arbitrary but generous limit */
	execve(path, av, ev);
	if (errno != ENOEXEC)
		return -1;
	fd = rc_open(path, rFrom);
	giveupif(fd < 0);
	len = read(fd, pb, sizeof pb);
	close(fd);
	/* reject scripts which don't begin with #! */
	giveupif(len <= 0 || pb[0] != '#' || pb[1] != '!');
	for (fst = 2; fst < len && (pb[fst] == ' ' || pb[fst] == '\t'); fst++)
		; /* skip leading whitespace */
	giveupif(fst == len);
	for (snd = fst; snd < len && pb[snd] != ' ' && pb[snd] != '\t' && pb[snd] != '\n'; snd++)
		; /* skip first arg */
	giveupif(snd == len);
	noarg = (pb[snd] == '\n');
	pb[snd++] = '\0'; /* null terminate the first arg */
	if (!noarg) {
		while (snd < len && (pb[snd] == ' ' || pb[snd] == '\t'))
			snd++; /* skip whitespace to second arg */
		giveupif(snd == len);
		noarg = (pb[snd] == '\n'); /* could have trailing whitespace after only one arg */
		if (!noarg) {
			for (end = snd; end < len && pb[end] != ' ' && pb[end] != '\t' && pb[end] != '\n'; end++)
				; /* skip to the end of the second arg */
			giveupif(end == len);
			if (pb[end] == '\n') {
				pb[end] = '\0'; /* null terminate the first arg */
			} else {		/* else check for a spurious third arg */
				pb[end++] = '\0';
				while (end < len && (pb[end] == ' ' || pb[end] == '\t'))
					end++;
				giveupif(end == len || pb[end] != '\n');
			}
		}
	}
	*av = path;
	if (!noarg)
		*--av = pb + snd;
	*--av = pb + fst;
	execve(*av, av, ev);
	return -1;
fail:	errno = ENOEXEC;
	return -1;
}
