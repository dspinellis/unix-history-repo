.\" Copyright (c) 1983 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.man%
.\"
.\"	@(#)appendix.ms	6.2 (Berkeley) %G%
.\"
.nr H2 1
.ds RH Appendix A
.SH
\s+2Appendix A\s0
.SH
read_8192
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
.SH
write_4096
.DS
#define	BUFSIZ 4096
main( argc, argv)
char **argv;
{
	char buf[BUFSIZ];
	int i, j;

	j = creat(argv[1], 0666);
	for (i = 0; i < 2048; i++)
		write(j, buf, BUFSIZ);
}
.DE
.SH
write_8192
.DS
#define	BUFSIZ 8192
main( argc, argv)
char **argv;
{
	char buf[BUFSIZ];
	int i, j;

	j = creat(argv[1], 0666);
	for (i = 0; i < 1024; i++)
		write(j, buf, BUFSIZ);
}
.DE
.bp
.SH
rewrite_8192
.DS
#define	BUFSIZ 8192
main( argc, argv)
char **argv;
{
	char buf[BUFSIZ];
	int i, j;

	j = open(argv[1], 2);
	for (i = 0; i < 1024; i++)
		write(j, buf, BUFSIZ);
}
.DE
