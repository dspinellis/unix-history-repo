/*	fastcopy.c	1.1	86/01/12	*/
/*	fastcopy.c */

#define	BSIZE	1024
#define FACTOR	32	/* 32k bytes in one i/o */

char	buffer[BSIZE * FACTOR];

main()
{
	char		in_dev[50];
	char		out_dev[50];
	char		blocks[50];
	register int	input;
	register int	output;
	register int	count;
	register int	firstread = 1;
	register int	blk_siz = BSIZE * FACTOR;
	register int	blk_num = 0;
	register int	read_count = 0;
	register int	write_count = 0;
	register int	transfers = 0;

	in_dev[0] = 0;
	out_dev[0] = 0;
	blocks[0] = 0;
	for(;;) {
		/* get input and output devices */
		printf("Source device : ");
		gets(in_dev);
		if((input = open(in_dev, 0)) > 0)
			break;
		printf("Cannot open input file '%s'\n", in_dev);
	}

	for(;;) {
		printf("Copy to device : ");
		gets(out_dev);
		if((output = open(out_dev, 1)) > 0)
			break;
		printf("Cannot open output file '%s'\n", out_dev);
	}

	for(;;) {
		printf("Number of blocks : ");
		gets(blocks);
		count = number(blocks);
		if(count > 0)
			break;
	}
	printf("\nCopy %d blocks from %s to %s\n", count, in_dev, out_dev);
	do {	
		if ((transfers > 0) && !(transfers % 25))
			printf("%d blocks\n", blk_num);
		transfers++;
		read_count = read(input, buffer,
		    ((count*BSIZE) > blk_siz) ? blk_siz : count*BSIZE);
		if (firstread) {
			if (read_count != blk_siz) {
				blk_siz = read_count;
			}
			firstread = 0;
			printf("Block size from input = %d bytes\n", blk_siz);
		}
		if (read_count > 0) {
			if (read_count != blk_siz)
				printf("Short read!  Block %d: %d read, %d bytes requested\n", blk_num, read_count, blk_siz);
			write_count = write(output, buffer, read_count);
			if (write_count != read_count)
				printf("Short write!  Block %d: %d bytes written of %d bytes possible\n", blk_num, write_count, read_count);
			count -= read_count / BSIZE;
			blk_num += read_count / BSIZE;
		}
	} while((read_count > 0) && (write_count > 0) && (count > 0));
	printf ("Total of %d blocks copied\n", blk_num);
	close(input);
	close(output);
}

int number (response)
char *response;
{
	int	total;
	
	total = 0;
	while (*response == ' ' || *response == '\t') response++;
	while (*response >= '0' && *response <= '9') {
		total = total * 10 + (*response - '0');
		response++;
	}
	return (total);
}
