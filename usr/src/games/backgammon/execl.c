main (argc,argv)
int	argc;
char	**argv;

{
execl ("/usr/games/backgammon","backgammon","s",*argv,0);
}
