char msg[] = "No vfork available - aborting\n";
vfork()
{
  write(1, msg, sizeof(msg));
}

sigsetmask()
{
  /* no signals support in go32 (yet) */
}

getuid()
{
  
  return 42;
}
getgid()
{
  return 42;
}

