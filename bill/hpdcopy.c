
char	*from = "/dev/rhp0c";
char	*to = "/dev/rup0h";
char	*buf;
int	bsize = 3*22;
int	start = 0;
int	count = 814; /*480 for h file system on ampex*/

main(argc, argv)
	int argc;
	char **argv;
{

	argc--, argv++;
	if (argc > 0)
		start = atoi(*argv++), argc--;
	if (argc > 0)
		bsize = atoi(*argv++), argc--;
	if (argc > 0)
		count = atoi(*argv++), argc--;
	printf("from %s to %s, %d groups of %d blocks offset %d\n",
	    from, to, count, bsize, start);
	close(0);
	if (open(from, 0) != 0)
		perror(from), exit(1);
	close(1);
	if (open(to, 1) != 1)
		perror(to), exit(1);
	start *= 512;
	bsize *= 512;
	buf = (char *)sbrk(bsize);
	while (count > 0) {
		if (lseek(0, start, 0) < 0)
			perror("seek 0"), exit(1);
		if (lseek(1, start+(200000*512), 0) < 0)
			perror("seek 1"), exit(1);
		if (count % 25 == 0)
			printf("%d\n", count);
		if (read(0, buf, bsize) != bsize) {
			printf("read block %d: ", start / 512);
			perror("read 0");
		} else if (write(1, buf, bsize) != bsize) {
			printf("write block %d: ", start / 512);
			perror("write 1");
		}
		start += bsize;
		count--;
	}
	printf("DONE!\n");
}
