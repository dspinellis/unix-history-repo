#include <stdio.h>
#include <time.h>
#include "msdos.h"

extern int dir_start, dir_len, clus_size, dir_entries, fat_error, clus_size;
extern unsigned int num_clus, end_fat, last_fat;
extern long dir_chain[MAX_DIR_SECS];
extern unsigned char *dir_buf;

/*
 * Make a subdirectory grow in length.  Only subdirectories (not root)
 * may grow.  Returns a 0 on success, 1 on failure (disk full), or -1
 * on error.
 */

int
dir_grow(fat)
unsigned int fat;
{
	int i, num, buflen, new;
	long sector;
	char *memset(), *realloc();
	unsigned char *offset, tbuf[MAX_CLUSTER];
	unsigned int next, last, next_fat(), fat_decode();
	void perror(), exit(), disk_write(), disk_read();

	last = next_fat(0);
	if (last == 1)
		return(1);

	/* CONSTCOND */
	while (1) {
		next = fat_decode(fat);
		if (next == 1) {
			fprintf(stderr, "dir_grow: FAT problem\n");
			fat_error++;
			return(-1);
		}
					/* end of cluster chain */
		if (next >= last_fat)
			break;
		fat = next;
	}
					/* mark the end of the chain */
	fat_encode(fat, last);
	fat_encode(last, end_fat);
					/* zero the buffer */
	buflen = clus_size * MSECTOR_SIZE;
	memset((char *) tbuf, '\0', buflen);

					/* write the cluster */
	sector = (long) (last - 2) * clus_size + dir_start + dir_len;
	disk_write(sector, tbuf, buflen);

					/* fix up the globals.... */
	num = dir_entries / 16;
	dir_entries += clus_size * 16;
	for (i = 0; i < clus_size; i++)
		dir_chain[num + i] = sector + i;

					/* fix up dir_buf.... */
	new = num + clus_size;
	dir_buf = (unsigned char *) realloc((char *) dir_buf, (unsigned int) new * MSECTOR_SIZE);
	if (dir_buf == NULL) {
		perror("dir_grow: malloc");
		exit(1);
	}
	offset = dir_buf + (num * MSECTOR_SIZE);
	disk_read(dir_chain[num], offset, clus_size * MSECTOR_SIZE);
	return(0);
}

/*
 * Returns next free cluster or 1 if none are available.
 */

unsigned int
next_fat(last)
unsigned int last;
{
	register unsigned int i;
	unsigned int fat_decode();

	for (i = last + 1; i < num_clus + 2; i++) {
		if (!fat_decode(i))
			return(i);
	}
	return(1);
}
