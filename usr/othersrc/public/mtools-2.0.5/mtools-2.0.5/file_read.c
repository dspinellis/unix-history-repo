#include <stdio.h>
#include "msdos.h"

extern int clus_size, dir_start, dir_len, fat_error;
extern unsigned int last_fat;

/*
 * Read the clusters given the beginning FAT entry.  Returns 0 on success.
 */

int
file_read(fp, fat, textmode, stripmode, size)
FILE *fp;
unsigned int fat;
int textmode, stripmode;
long size;
{
	register int i;
	int in_len, out_len;
	unsigned int fat_decode();
	long current, start;
	unsigned char in_buf[MAX_CLUSTER], out_buf[MAX_CLUSTER];
	void disk_read(), perror();

	current = 0L;
	in_len = clus_size * MSECTOR_SIZE;
					/* a zero length file? */
	if (fat == 0)
		return(0);

	/* CONSTCOND */
	while (1) {
		start = (long) (fat - 2) * clus_size + dir_start + dir_len;
		disk_read(start, in_buf, in_len);

					/* do the translations */
		if (textmode || stripmode) {
			out_len = 0;
			for (i = 0; i < in_len; i++) {
				current++;
				if (current > size)
					break;
				if (textmode && in_buf[i] == '\r')
					continue;
				if (textmode && current == size && in_buf[i] == 0x1a)
					continue;
				if (stripmode)
					out_buf[out_len++] = in_buf[i] & 0x7f;
				else
					out_buf[out_len++] = in_buf[i];
			}
					/* write it */
			if (fwrite((char *) out_buf, sizeof(*out_buf), out_len, fp) == 0) {
				perror("file_write: fwrite");
				return(-1);
			}
		}
					/* much easier... */
		else {
			out_len = (size - current > in_len) ? in_len : size - current;
			if (fwrite((char *) in_buf, sizeof(*in_buf), out_len, fp) == 0) {
				perror("file_write: fwrite");
				return(-1);
			}

			current += out_len;
			if (current >= size)
				break;
		}

					/* get next cluster number */
		fat = fat_decode(fat);
		if (fat == 1) {
			fprintf(stderr, "file_read: FAT problem\n");
			fat_error++;
			return(-1);
		}
					/* end of cluster chain */
		if (fat >= last_fat)
			break;
	}
	return(0);
}
