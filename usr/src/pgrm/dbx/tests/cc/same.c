/*
 * The purpose of this test case is to make sure that dbx interprets
 * the command "stop in same" at the top level to mean stop in the
 * routine same.  Originally, dbx complained since "same" in the initial
 * context refers to the module named "same" (since this file is "same.c").
 */

same ()
{
    printf("same function and module names\n");
}

main ()
{
    same();
    exit(0);
}
