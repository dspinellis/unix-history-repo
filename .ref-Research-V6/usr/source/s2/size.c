/*
	size -- determine object size

*/

main(argc, argv)
char **argv;
{
	int buf[010], f, ac, sum;

	ac = argc;
	if (ac==1) {
		*argv = "a.out";
		ac++;
		--argv;
	}
	while(--ac) {
		++argv;
		if((f=open(*argv, 0))<0) {
			printf("%s not found\n", *argv);
			continue;
		}
		read(f, buf, 0020);
		if(buf[0]!=0411 && buf[0]!=0410 && buf[0]!=0407) {
			printf("Bad format: %s\n", *argv);
			close(f);
			continue;
		}
		if (argc>2)
			printf("%s: ", *argv);
		printf("%l+%l+%l=", buf[1],buf[2],buf[3]);
		sum = buf[1]+buf[2]+buf[3];
		printf("%l (%o)\n", sum, sum);
		close(f);
	}
}
