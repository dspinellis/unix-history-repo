
# include <stdio.h>

main()
{	
	unsigned int i;
	char buf[1];
	register int result, count;

	while ((result = scanf("%02x", &i)) != EOF){
		count++;
		buf[0]=(char) i;
		if (result) write(1, buf, 1);
		else
		{
			fprintf(stderr,"Conversion failed at byte number %d\n", count);
			exit(1);
		}
	}
	exit(0);
}
