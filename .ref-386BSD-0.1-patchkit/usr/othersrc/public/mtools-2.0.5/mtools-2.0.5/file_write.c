#include <stdio.h>
#include "msdos.h"

extern int full, fat_error, clus_size, dir_start, dir_len;
extern unsigned end_fat;

static int need_nl, put_cluster();

/*
 * Write out clusters with input from the given file pointer.   Returns the
 * size of the file (which may have changed) or -1 on error.
 */

long
file_write(fp, firstfat, filesize, textmode)
FILE *fp;
unsigned int firstfat;
long filesize;
int textmode;
{
	int i;
	unsigned int fat, oldfat, next_fat();
	long size;

	oldfat = 0;
	fat = firstfat;
	need_nl = 0;
	size = 0L;

	/* CONSTCOND */
	while (1) {
		if ((i = put_cluster(fp, fat, &filesize, textmode)) < 0) {
			if (oldfat) {
				fat_encode(oldfat, end_fat);
				if (fat_free(firstfat))
					fat_error++;
				full = 1;
			}
			return(-1);
		}

		size += i;
		if (size >= filesize) {
			fat_encode(fat, end_fat);
			break;
		}
		oldfat = fat;
					/* get next free cluster */
		fat = next_fat(oldfat);
		if (fat == 1) {
			fat_encode(oldfat, end_fat);
					/* delete it, if doesn't fit */
			if (fat_free(firstfat))
				fat_error++;
			full = 1;
			return(-1);
		}
		fat_encode(oldfat, fat);
	}
	return(size);
}

/*
 * Write to the cluster from the named Unix file descriptor.  Returns the
 * size of what was written, or -1 on error.
 */

static int
put_cluster(fp, num, filesize, textmode)
FILE *fp;
unsigned int num;
long *filesize;
int textmode;
{
	int i, buflen, c;
	long start;
	unsigned char tbuf[MAX_CLUSTER];
	void disk_write(), perror();

	start = (long) (num - 2) * clus_size + dir_start + dir_len;
	buflen = clus_size * MSECTOR_SIZE;
					/* '\n' to '\r\n' translation */
	if (textmode) {
		i = 0;
		if (need_nl) {
			tbuf[i++] = '\n';
			need_nl = 0;
		}
		while (i < buflen) {
			if ((c = fgetc(fp)) == EOF) {
					/* put a file EOF marker */
				tbuf[i++] = 0x1a;
					/* make the file appear larger */
				*filesize = *filesize + 1;
				break;
			}
			if (c == '\n') {
				tbuf[i++] = '\r';
					/* make the file appear larger */
				*filesize = *filesize + 1;
					/* if at the end of the buffer */
				if (i == buflen) {
					need_nl++;
					break;
				}
			}
			tbuf[i++] = (unsigned char) c;
		}
	}
					/* much easier.... */
	else {
		if ((i = fread((char *) tbuf, sizeof(*tbuf), buflen, fp)) <= 0) {
			perror("putcluster: fread");
			return(-1);
		}
	}

	disk_write(start, tbuf, buflen);
	return(i);
}
