system(str)
 char *str;
{
int status;
  if(fork() == 0)
    execl("/bin/sh", "sh", "-c", str, 0);
  wait(&status);
  }
