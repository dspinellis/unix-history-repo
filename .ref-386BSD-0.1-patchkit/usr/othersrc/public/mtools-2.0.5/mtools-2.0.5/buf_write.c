/*
 * Do full cylinder buffered writes to slow devices.  Uses a simple
 * buffered read/delayed write algorithm
 */

#include <stdio.h>
#include "msdos.h"

extern int fd, disk_size, disk_dirty, dir_start, dir_len;
extern int clus_size;
extern long disk_offset, disk_current;
extern unsigned char *disk_buf;
static int blank_cyl();

void
disk_write(start, buf, len)
long start;
unsigned char *buf;
int len;
{
	register long i;
	int length;
	unsigned char *buf_ptr, *disk_ptr;
	char *memcpy();
	void perror(), exit(), disk_flush();
	long where, tail, lseek();

					/* don't use cache? */
	if (disk_size == 1) {
		where = (start * MSECTOR_SIZE) + disk_offset;
		if (lseek(fd, where, 0) < 0) {
			perror("disk_write: lseek");
			exit(1);
		}
					/* write it! */
		if (write(fd, (char *) buf, (unsigned int) len) != len) {
			perror("disk_write: write");
			exit(1);
		}
		return;
	}

	tail = start + (len / MSECTOR_SIZE) - 1;
	for (i = start; i <= tail; i++) {
					/* a cache miss... */
		if (i < disk_current || i >= disk_current + disk_size) {

			if (disk_dirty)
				disk_flush();

			/*
			 * If there is something on the new cylinder that
			 * you want to keep, you'll have to read it first
			 * before writing.
			 */
			if (blank_cyl(i))
				disk_current = (i / disk_size) * disk_size;
			else {
				disk_current = (i / disk_size) * disk_size;
				where = (disk_current * MSECTOR_SIZE) + disk_offset;
				length = disk_size * MSECTOR_SIZE;

					/* move to next location */
				if (lseek(fd, where, 0) < 0) {
					perror("disk_write: lseek");
					exit(1);
				}
					/* read it! */
				if (read(fd, (char *) disk_buf, (unsigned int) length) != length) {
					perror("disk_write: read");
					exit(1);
				}
			}
		}
					/* a cache hit ... */
		buf_ptr = buf + ((i - start) * MSECTOR_SIZE);
		disk_ptr = disk_buf + ((i - disk_current) * MSECTOR_SIZE);
		memcpy((char *) disk_ptr, (char *) buf_ptr, MSECTOR_SIZE);
		disk_dirty = 1;
	}
	return;
}

/*
 * Flush a dirty buffer to disk.  Resets disk_dirty to zero.
 * All errors are fatal.
 */

void
disk_flush()
{
	int len;
	long where, lseek();
	void perror(), exit();

	if (fd < 0 || disk_current < 0L || !disk_dirty)
		return;

	where = (disk_current * MSECTOR_SIZE) + disk_offset;
	if (lseek(fd, where, 0) < 0) {
		perror("disk_flush: lseek");
		exit(1);
	}
					/* write it! */
	len = disk_size * MSECTOR_SIZE;
	if (write(fd, (char *) disk_buf, (unsigned int) len) != len) {
		perror("disk_flush: write");
		exit(1);
	}
	disk_dirty = 0;
	return;
}

/*
 * Determine if the cylinder has some useful information on it.  Returns a 1
 * if it is blank.
 */

static int
blank_cyl(num)
long num;
{
	register unsigned int i;
	unsigned int start, end, fat_decode();
	long sector;

	sector = (num / disk_size) * disk_size;
	if (!sector)
		return(0);

	start = ((sector - dir_start - dir_len) / clus_size) + 2;
	end = ((sector + disk_size - dir_start - dir_len) / clus_size) + 2;

	for (i = start; i < end; i++) {
					/* if fat_decode returns non-zero */
		if (fat_decode(i))
			return(0);
	}
	return(1);
}
