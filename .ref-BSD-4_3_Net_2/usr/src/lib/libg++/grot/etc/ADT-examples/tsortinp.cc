//  Here's a short program that generates acyclic input to test this program
//      against the UNIX tsort.

#include <stream.h>

main(int argc,char *argv[]) 
{
  int Dimension = atoi(argv[1]);

  for (int y = 1; y < Dimension; y++) 
    for (int x = 0; x < y; x++) 
      cout << dec(y,4) << dec(x,4) << "\n";

}
