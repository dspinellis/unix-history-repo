/*

	filecat

	does nothing more than copy standard input to standard
	output, like the cat command, but reports write errors.
	Takes no arguments.
	Uses getc and putc rather than fwrite and fread because
	the latter call getc and putc.

	Exit codes:
		0	ok
		1	error on read
		2	error on write

*/
# include <stdio.h>
main(){
	char c,sOutbuf[BUFSIZ];

	setbuf(stdout,sOutbuf);

	while((c = getc(stdin)) != EOF){
		putc(c,stdout);
		if(ferror(stdout)){
			perror("filecat: stdout");
			exit(2);
		}
	}
	if(ferror(stdin)){
		perror("filecat: stdin");
		exit(1);
	}
	fclose(stdin);
	fclose(stdout);
	exit(0);
}
