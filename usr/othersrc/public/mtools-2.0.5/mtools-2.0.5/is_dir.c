/*
 * Test to see if a filename is a directory.  Subdir() has to be called
 * on the directory above this one first...  Returns 1 if true.
 */

#include <stdio.h>
#include "msdos.h"

extern int dir_entries;

int
is_dir(path)
char *path;
{
	register int entry;
	char *newname, *unix_name();
	struct directory *dir, *dir_read();
					/* no path */
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
		if (match(newname, path))
			return(1);
	}

	/*
	 * If the file is "." or ".." but it fails to match, then you
	 * must be at root
	 */
	if (!strcmp(path, ".") || !strcmp(path, ".."))
		return(1);

	return(0);
}
