/*
Unhandled exceptions cause the program to abort.
Argument FILENAME is the name of the file that caught the exception.
Argument LINENO is the line number at which the exception was
caught.
*/

extern volatile void abort();

void
__unhandled_exception (char *filename, int lineno)
{
  abort ();
}

void
__raise_exception (void **addr, void *id)
{
  *addr = id;
}
