main()
{
	int uid;

	printf("euid=%d ruid=%d\n", geteuid(), uid = getuid());
	if (setuid(uid) < 0)
		perror("setuid");
	printf("euid=%d ruid=%d\n", geteuid(), uid = getuid());
}
