main(argc, argv)
char **argv;
{
close(1);
open(argv[1], 1);
lseek(1, 0, 0);
execl("/bin/cat", "cat", 0);
}
