main(argc, argv)
char **argv;
{
char buf[1024];
close(1);
open("/dev/rrp2h", 1);
lseek(1, 512*atoi(argv[1]), 0);
write(1, buf, 1024);
}
