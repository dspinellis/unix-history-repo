#include <stdio.h>
#include "msdos.h"

extern int dir_entries;
static int descend();

/*
 * Descends the directory tree.  Returns 1 on error.  Attempts to optimize by
 * remembering the last path it parsed
 */

int
subdir(drive, pathname)
char drive;
char *pathname;
{
	char *s, *tmp, tbuf[MAX_PATH], *path, *strcpy();
	static char last_drive, lastpath[MAX_PATH];
	int code;
	void reset_chain();

	strcpy(tbuf, pathname);
					/* if paths are same, do nothing */
	if (!strcmp(tbuf, lastpath) && last_drive == drive)
		return(0);
					/* start at root */
	reset_chain(OLD);
	strcpy(lastpath, tbuf);
	last_drive = drive;
					/* separate the parts */
	tmp = &tbuf[1];
	for (s = tmp; *s; ++s) {
		if (*s == '/') {
			path = tmp;
			*s = '\0';
			if (descend(path))
				return(1);
			tmp = s + 1;
		}
	}
	code = descend(tmp);
	return(code);
}

/*
 * Find the directory and load a new dir_chain[].  A null directory
 * is ok.  Returns a 1 on error.
 */

static int
descend(path)
char *path;
{
	int entry;
	unsigned int start;
	char *newname, *unix_name();
	struct directory *dir, *dir_read();
	void reset_chain();
					/* nothing required */
	if (*path == '\0')
		return(0);

	for (entry = 0; entry < dir_entries; entry++) {
		dir = dir_read(entry);
					/* if empty */
		if (dir->name[0] == 0x0)
			break;
					/* if erased */
		if (dir->name[0] == 0xe5)
			continue;
					/* skip if not a directory */
		if (!(dir->attr & 0x10))
			continue;

		newname = unix_name(dir->name, dir->ext);

		/*
		 * Be careful not to match '.' and '..' with wildcards
		 */
		if (*newname == '.' && *path != '.')
			continue;

		if (match(newname, path)) {
			start = dir->start[1] * 0x100 + dir->start[0];

					/* if '..' points to root */
			if (!start && !strcmp(path, "..")) {
				reset_chain(OLD);
				return(0);
			}
					/* fill in the directory chain */
			if (fill_chain(start))
				return(1);

			return(0);
		}
	}

	/*
	 * If path is '.' or '..', but they weren't found, then you must be
	 * at root.
	 */
	if (!strcmp(path, ".") || !strcmp(path, "..")) {
		reset_chain(OLD);
		return(0);
	}
	fprintf(stderr, "Path component \"%s\" is not a directory\n", path);
	return(1);
}
