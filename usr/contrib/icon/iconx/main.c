/*
 * C startup program for Icon.  Just pass argv to mstart and let it
 *  do the work.
 */
main(argc,argv)
int argc;
char **argv;
{
	mstart(argc, argv);
}
