main(argc, argv)
char **argv;
{
char buf[1024];
close(0);
open("/dev/rp2h", 0);
lseek(0, 512*atoi(argv[1]), 0);
read(0, buf, 1024);
write(1, buf,1024);
}
