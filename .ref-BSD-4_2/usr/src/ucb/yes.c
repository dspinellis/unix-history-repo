static char *sccsid ="@(#)yes.c	4.1 (Berkeley) 10/8/80";
main(argc, argv)
char **argv;
{
	for (;;)
		printf("%s\n", argc>1? argv[1]: "y");
}
