.\" Copyright (c) 1983 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.man%
.\"
.\"	@(#)tests.ms	6.2 (Berkeley) %G%
.\"
.ds RH Tests
.NH
Tests
.PP
Our battery of tests consists of four programs,
read_8192, write_8192, write_4096
and rewrite_8192 originally written by [McKusick83]
to evaluate the performance of the new file system in 4.2BSD.
These programs all follow the the same model and are typified by
read_8192 shown here.
.DS
#define	BUFSIZ 8192
main( argc, argv)
char **argv;
{
	char buf[BUFSIZ];
	int i, j;

	j = open(argv[1], 0);
	for (i = 0; i < 1024; i++)
		read(j, buf, BUFSIZ);
}
.DE
The remaining programs are included in appendix A.
.PP
These programs read, write with two different blocking factors,
and rewrite logical files in structured file system on the disk
under test.
The write programs create new files while the rewrite program
overwrites an existing file.
Each of these programs represents an important segment of the
typical UNIX file system activity with the read program
representing by far the largest class and the rewrite the smallest.
.PP
A blocking factor of 8192 is used by all programs except write_4096.
This is typical of most 4.2BSD user programs since a standard set of
I/O support routines is commonly used and these routines buffer
data in similar block sizes.
.PP
For each test run, a empty eight Kilobyte block
file system was created in the target
storage system.
Then each of the four tests was run and timed.
Each test was run three times;
the first to clear out any useful data in the cache,
and the second two to insure that the experiment
had stablized and was repeatable.
Each test operated on eight Megabytes of data to 
insure that the cache did not overly influence the results.
Another file system was then initialized using a 
basic blocking factor of four Kilobytes and the same tests
were run again and timed.
A command script for a run appears as follows:
.DS
#!/bin/csh
set time=2
echo "8K/1K file system"
newfs /dev/rhp0g eagle
mount /dev/hp0g /mnt0
mkdir /mnt0/foo
echo "write_8192 /mnt0/foo/tst2"
rm -f /mnt0/foo/tst2
write_8192 /mnt0/foo/tst2
rm -f /mnt0/foo/tst2
write_8192 /mnt0/foo/tst2
rm -f /mnt0/foo/tst2
write_8192 /mnt0/foo/tst2
echo "read_8192 /mnt0/foo/tst2"
read_8192 /mnt0/foo/tst2
read_8192 /mnt0/foo/tst2
read_8192 /mnt0/foo/tst2
umount /dev/hp0g
.DE
.ds RH Results
.bp
