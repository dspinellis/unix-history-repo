#include <iostream.h>

main(int argc, char** argv)
{
    istream *istr = &cin;
    char buf[40];
    int i = 0;
    for (;;) {
	i ++;
	istr->getline(buf, 40);
	if (istr->gcount() == 0)
	    break;
	cout << "Line " << i << " (" << istr->gcount()
	    << "char): [" << buf << "]\n";
    }
}
