/*
 * Benchmark "null big job" program.
 */
/* 250 here is intended to approximate vi's text+data size */
char	space[1024 * 250] = "force into data segment";

main(argc, argv)
	char *argv[];
{

	exit(0);
}
