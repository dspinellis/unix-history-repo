#include <stdio.h>
#include "msdos.h"

int fat_len;				/* length of FAT table (in sectors) */
unsigned int end_fat;			/* the end-of-chain marker */
unsigned int last_fat;			/* the last in a chain marker */
unsigned char *fat_buf;			/* the File Allocation Table */

extern int fat_bits;

/*
 * Get and decode a FAT (file allocation table) entry.  Returns the cluster
 * number on success or 1 on failure.
 */

unsigned int
fat_decode(num)
unsigned int num;
{
	unsigned int fat, fat_hi, fat_low, byte_1, byte_2;
	int start;

	if (fat_bits == 12) {
		/*
		 *	|    byte n     |   byte n+1    |   byte n+2    |
		 *	|7|6|5|4|3|2|1|0|7|6|5|4|3|2|1|0|7|6|5|4|3|2|1|0|
		 *	| | | | | | | | | | | | | | | | | | | | | | | | |
		 *	| n+0.0 | n+0.5 | n+1.0 | n+1.5 | n+2.0 | n+2.5 |
		 *	    \_____  \____   \______/________/_____   /
		 *	      ____\______\________/   _____/  ____\_/
		 *	     /     \      \          /       /     \
		 *	| n+1.5 | n+0.0 | n+0.5 | n+2.0 | n+2.5 | n+1.0 |
		 *	|      FAT entry k      |    FAT entry k+1      |
		 */
					/* which bytes contain the entry */
		start = num * 3 / 2;
		if (start <= 2 || start + 1 > (fat_len * MSECTOR_SIZE))
			return(1);

		byte_1 = *(fat_buf + start);
		byte_2 = *(fat_buf + start + 1);
					/* (odd) not on byte boundary */
		if (num % 2) {
			fat_hi = (byte_2 & 0xff) << 4;
			fat_low = (byte_1 & 0xf0) >> 4;
		}
					/* (even) on byte boundary */
		else {
			fat_hi = (byte_2 & 0xf) << 8;
			fat_low = byte_1 & 0xff;
		}
		fat = (fat_hi + fat_low) & 0xfff;
	}
	else {
		/*
		 *	|    byte n     |   byte n+1    |
		 *	|7|6|5|4|3|2|1|0|7|6|5|4|3|2|1|0|
		 *	| | | | | | | | | | | | | | | | |
		 *	|         FAT entry k           |
		 */
					/* which bytes contain the entry */
		start = num * 2;
		if (start <= 3 || start + 1 > (fat_len * MSECTOR_SIZE))
			return(1);

		fat = (*(fat_buf + start + 1) * 0x100) + *(fat_buf + start);
	}
	return(fat);
}

/*
 * Read the entire FAT table into memory.
 */

void
fat_read(start)
int start;
{
	int buflen;
	char *malloc();
	void perror(), exit(), disk_read();
					/* only the first copy of the FAT */
	buflen = fat_len * MSECTOR_SIZE;
	fat_buf = (unsigned char *) malloc((unsigned int) buflen);
	if (fat_buf == NULL) {
		perror("fat_read: malloc");
		exit(1);
	}
					/* read the FAT sectors */
	disk_read((long) start, fat_buf, buflen);

					/* the encoding scheme */
	if (fat_bits == 12) {
		end_fat = 0xfff;
		last_fat = 0xff8;
	}
	else {
		end_fat = 0xffff;
		last_fat = 0xfff8;
	}
	return;
}
