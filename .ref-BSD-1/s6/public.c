#
/*
 * public - make a file available to the world
 *
 * Author: Chuck Haley
 *
 * Yet another setuserid security problem!
 */
#define PUBLICID 8
char	buf[512];
main(argc, argv)
char	*argv[];
{
	char *c, *cp, *c1;
	int out;
	int stbuf[18];
	int i, j, k, pf;
	int file;

	if (argc == 1)
	{
		printf("usage: public file1 file2 ... filen\n");
		exit(0);
	}

	pf = open("/usr/adm/publog", 1);
	seek(pf, 0, 2);
	while (--argc)
	{
		c = *++argv;
		if (access(c, 4) < 0)
		{
			printf("%s: cannot read!!!\n", c);
			continue;
		}
		if (stat(c, stbuf) < 0)
		{
			printf("%s: cannot stat\n", c);
			continue;
		}
		if ((stbuf[2] & 060000))
		{
			printf("%s: ", c);
			if ((stbuf[2] & 060000) == 040000)
				printf("directory\n");
			else
				printf("special file\n");
			continue;
		}
		if ((stbuf[2] & 5) == 0)
		{
			printf("%s: neither read nor execute access for others\n", c);
			continue;
		}
		cp = c - 1;
		while (*c)
			if (*c == '/')
				cp = c++;
			else c++;
		c = c1 = "/usr/public/xxxxxxxxxxxxxxx";
		c =+ 12;
		cp++;
		while (*c++ = *cp++);
		if ((i = open(c1, 0)) >= 0)
		{
			printf("%s: file already exists\n", c1);
			continue;
		}
		close(i);
		file = open(*argv, 0);
		out = creat(c1, stbuf[2] & 0777);
		chown(c1, PUBLICID);
		while ((i = read(file, buf, 512)) > 0)
			write(out, buf, i);
		close(1);
		dup(pf);
		printf("%d,%d\t%s\n", getuid() & 0377, (getuid() >> 8) & 0377, c1);
		close(1);
		dup(2);
	}
}
