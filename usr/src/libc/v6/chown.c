chown(name, owner, group)
char *name;
int owner, group;
{
	return(syscall(16, 0, 0, name, (group<<8)|(owner&0377), 0));
}
