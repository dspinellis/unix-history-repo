#include <stdio.h>
#include "msdos.h"

long dir_chain[MAX_DIR_SECS];		/* chain of sectors in directory */
unsigned char *dir_buf;			/* the directory buffer */
int dir_dirty;				/* is the buffer dirty? */

extern int dir_len, dir_start, clus_size, dir_entries, fat_error;
extern unsigned int last_fat;

/*
 * Read a directory entry, return a pointer a static structure.
 */

struct directory *
dir_read(num)
int num;
{
	char *memcpy();
	unsigned char *offset;
	static struct directory dir;

	offset = dir_buf + (num * MDIR_SIZE);
	memcpy((char *) &dir, (char *) offset, MDIR_SIZE);
	return(&dir);
}

/*
 * Fill in the global variable dir_chain[].  Argument is the starting
 * cluster number.  Returns -1 on error.
 */

int
fill_chain(num)
unsigned int num;
{
	register int i, length;
	unsigned int next, fat_decode();
	unsigned char *offset;
	char *malloc();
	void free(), perror(), exit(), disk_read(), dir_flush();

	length = 0;
	/* CONSTCOND */
	while (1) {
		dir_chain[length] = (long) (num - 2) * clus_size + dir_start + dir_len;
		length++;
					/* sectors, not clusters! */
		for (i = 1; i < clus_size; i++) {
			dir_chain[length] = dir_chain[length - 1] + 1L;
			length++;
		}

		if (length >= MAX_DIR_SECS) {
			fprintf(stderr, "fill_chain: directory too large\n");
			return(-1);
		}
					/* get next cluster number */
		next = fat_decode(num);
		if (next == 1) {
			fprintf(stderr, "fill_chain: FAT problem\n");
			fat_error++;
			return(-1);
		}
					/* end of cluster chain */
		if (next >= last_fat)
			break;
		num = next;
	}
	if (dir_dirty)
		dir_flush();
					/* fill the dir_buf */
	free((char *) dir_buf);
	dir_buf = (unsigned char *) malloc((unsigned int) length * MSECTOR_SIZE);
	if (dir_buf == NULL) {
		perror("fill_chain: malloc");
		exit(1);
	}

	for (i = 0; i < length; i++) {
		offset = dir_buf + (i * MSECTOR_SIZE);
		disk_read(dir_chain[i], offset, MSECTOR_SIZE);
	}

	dir_entries = length * 16;
	return(0);
}

/*
 * Reset the global variable dir_chain[] to the root directory.
 */

void
reset_chain(code)
int code;
{
	register int i;
	char *malloc();
	void free(), disk_read(), dir_flush(), exit(), perror();

	if (dir_dirty)
		dir_flush();

	for (i = 0; i < dir_len; i++)
		dir_chain[i] = (long) dir_start + i;

	if (code == OLD)
		free((char *) dir_buf);

	dir_buf = (unsigned char *) malloc((unsigned int) dir_len * MSECTOR_SIZE);
	if (dir_buf == NULL) {
		perror("reset_chain: malloc");
		exit(1);
	}
	disk_read((long) dir_start, dir_buf, dir_len * MSECTOR_SIZE);

	dir_entries = dir_len * 16;
	return;
}

/*
 * Get rid of spaces in an MSDOS 'raw' name (one that has come from the
 * directory structure) so that it can be used for regular expression
 * matching with a unix filename.  Also used to 'unfix' a name that has
 * been altered by dos_name().  Returns a pointer a static buffer.
 */

char *
unix_name(name, ext)
unsigned char *name, *ext;
{
	char *s, tname[9], text[4], *strcpy(), *strcat(), *strchr();
	char *strncpy();
	static char ans[13];

	strncpy(tname, (char *) name, 8);
	tname[8] = '\0';
	if (s = strchr(tname, ' '))
		*s = '\0';

	strncpy(text, (char *) ext, 3);
	text[3] = '\0';
	if (s = strchr(text, ' '))
		*s = '\0';

	if (*text) {
		strcpy(ans, tname);
		strcat(ans, ".");
		strcat(ans, text);
	}
	else
		strcpy(ans, tname);
	return(ans);
}
