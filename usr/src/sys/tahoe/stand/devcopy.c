/*	devcopy.c	1.1	86/01/12	*/
/*	devcopy.c */
#define	BSIZE	1024
char dev[50];
char disk[50];
char blocks[50] ;
int	fsi;
int	fso;
char	buffer[BSIZE];

main()
{
	int j, c, i, n;
	char buf[50];
	int input, output;

	do {
		printf("Source device : ");
		gets(dev);
		printf("Copy to device : ");
		gets(disk);
		printf("Number of blocks : ");
		gets(blocks);
		j = number(blocks);
		input = open(dev, 0);
		if (input <= 0) printf("Cannot open %s\n", dev);
		output = open(disk, 1);
		if (output <= 0) printf("Cannot open output\n", disk);
	} while (input <= 0 || output <= 0);

	i = 0;	/* Block number */
	n = BSIZE;
	while (n == BSIZE && i < j) {
		if (i > 0 && (i%500 == 0) ) printf("%d blocks\n", i);
		lseek (input, i*BSIZE, 0);
		n = read (input, buffer, BSIZE);
		if (n == BSIZE) {
			lseek (output, i*BSIZE, 0);
			n = write(output, buffer, BSIZE);
			if (n != BSIZE) printf("Short write, block %d, %d bytes only\n",i,n);
			i++;
		}
		else printf("Short read, block %d, %d bytes only\n",i,n);
	}
	printf ("Total of %d blocks copied\n",i);
}

int number (response)
char *response;
{
	int	i, j;
	
	j = 0;	/* Total */
	while (*response == ' ' || *response == '\t') response++;
	while (*response >= '0' && *response <= '9') {
		j = j*10 + *response - '0';
		response++;
	}
	return (j);
}
