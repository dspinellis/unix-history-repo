/* mhl.c - the MH message listing program */


main (argc, argv)
int     argc;
char  **argv;
{
    done (mhl (argc, argv));
}

/*  */

/* Cheat:  we are loaded with adrparse, which wants a routine called
   OfficialName().  We call adrparse:getm() with the correct arguments
   to prevent OfficialName() from being called.  Hence, the following
   is to keep the loader happy.
 */

char   *OfficialName (name)
register char  *name;
{
    return name;
}
