#include <stdio.h>
#include "msdos.h"

extern int fd, dir_start, num_fat, fat_len, fat_error, fat_bits;
extern unsigned char *fat_buf;

/*
 * Puts a code into the FAT table.  Is the opposite of fat_decode().  No
 * sanity checking is done on the code.  Returns a 1 on error.
 */

int
fat_encode(num, code)
unsigned int num;
unsigned int code;
{
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
					/* (odd) not on byte boundary */
		if (num % 2) {
			*(fat_buf + start) = (*(fat_buf + start) & 0x0f) + ((code << 4) & 0xf0);
			*(fat_buf + start + 1) = (code >> 4) & 0xff;
		}
					/* (even) on byte boundary */
		else {
			*(fat_buf + start) = code & 0xff;
			*(fat_buf + start + 1) = (*(fat_buf + start + 1) & 0xf0) + ((code >> 8) & 0x0f);
		}
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

		*(fat_buf + start + 1) =  code / 0x100;
		*(fat_buf + start) =  code % 0x100;
	}
	return(0);
}

/*
 * Write the FAT table to the disk.  Up to now the FAT manipulation has
 * been done in memory.  All errors are fatal.  (Might not be too smart
 * to wait till the end of the program to write the table.  Oh well...)
 */

void
fat_write()
{
	int fat_start, buflen, dups;
	void disk_write();

	if (fd < 0)
		return;

	fat_start = dir_start - (fat_len * num_fat);
	buflen = fat_len * MSECTOR_SIZE;

	disk_write((long) fat_start, fat_buf, buflen);

	/*
	 * Only duplicate the FAT table if no errors were detected
	 */
	if (!fat_error) {
		dups = num_fat - 1;
		while (dups--) {
			fat_start += fat_len;
			disk_write((long) fat_start, fat_buf, buflen);
		}
	}
	return;
}
