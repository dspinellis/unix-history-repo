/**
 **	make a system call
 **/

system(str)
char	*str;
{
	int		status;
	register int	i;
	register int	pid;

	pid = fork();
	if (pid == -1)
		__error("system: fork error");
	if (pid == 0)
	{
		execl("/bin/sh", "-sh", "-c", str, 0);
		__error("system: exec error");
	}
	while ((i = wait(&status)) != -1)
		if (i == pid)
			break;
	return(status);
}
