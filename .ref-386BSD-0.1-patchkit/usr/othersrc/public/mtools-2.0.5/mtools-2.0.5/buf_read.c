/*
 * Do full cylinder buffered reads from slow devices.  Uses a simple
 * buffered read/delayed write algorithm.
 */

#include <stdio.h>
#include "msdos.h"

unsigned char *disk_buf;		/* disk read/write buffer */
int disk_size;				/* size of read/write buffer */
long disk_current;			/* first sector in buffer */
int disk_dirty;				/* is the buffer dirty? */

extern int fd;
extern long disk_offset;

void
disk_read(start, buf, len)
long start;
unsigned char *buf;
int len;
{
	register long i;
	int length;
	unsigned char *buf_ptr, *disk_ptr;
	char *memcpy();
	long where, tail, lseek();
	void perror(), exit(), disk_flush();

					/* don't use cache? */
	if (disk_size == 1) {
		where = (start * MSECTOR_SIZE) + disk_offset;
		if (lseek(fd, where, 0) < 0) {
			perror("disk_read: lseek");
			exit(1);
		}
					/* read it! */
		if (read(fd, (char *) buf, (unsigned int) len) != len) {
			perror("disk_read: read");
			exit(1);
		}
		return;
	}

	tail = start + (len / MSECTOR_SIZE) - 1;
	for (i = start; i <= tail; i++) {
					/* a "cache" miss */
		if (i < disk_current || i >= disk_current + disk_size) {

			if (disk_dirty)
				disk_flush();

			disk_current = (i / disk_size) * disk_size;
			where = (disk_current * MSECTOR_SIZE) + disk_offset;
			length = disk_size * MSECTOR_SIZE;

					/* move to next location */
			if (lseek(fd, where, 0) < 0) {
				perror("disk_read: lseek");
				exit(1);
			}
					/* read it! */
			if (read(fd, (char *) disk_buf, (unsigned int) length) != length) {
				perror("disk_read: read");
				exit(1);
			}
		}
					/* a cache hit... */
		buf_ptr = buf + ((i - start) * MSECTOR_SIZE);
		disk_ptr = disk_buf + ((i - disk_current) * MSECTOR_SIZE);
		memcpy((char *) buf_ptr, (char *) disk_ptr, MSECTOR_SIZE);
	}
	return;
}
