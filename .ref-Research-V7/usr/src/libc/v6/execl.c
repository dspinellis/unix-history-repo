execl(name, arg1, a2, a3, a4, a5)
char *name, arg1, a2, a3, a4, a5;
{
	char *args[6];

	args[0] = arg1;
	args[1] = a2;
	args[2] = a3;
	args[3] = a4;
	args[4] = a5;
	args[5] = 0;

	syscall(11, 0, 0, name, args, 0);
}
