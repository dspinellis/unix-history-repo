main(argc,argv)
char *argv[];
{
	char *shell, *getenv();
	int code;
	if(argc!=2) error("Usage: chroot <dir>\n");
	code = chdir(argv[1]);
	if(code<0)
		perror(argv[1]),_exit(2);
	code = chroot(argv[1]);
	if(code<0)
		perror(argv[1]),_exit(2);
	setuid(getuid());
	shell = getenv("SHELL");
	if(shell==0)shell = "sh";
	execlp(shell, shell, "-i", 0);
	error("Chroot: No shell\n");
}
error(mesg)
char *mesg;
{
	write(2,mesg,strlen(mesg));
	_exit(2);
}
