
#include "Clearinghouse2_defs.h"

main(argc,argv)
	int argc;
	char *argv[];
{
	Name pattern;
	Name defaults;
	Property property;
	static printit();
	extern Name CH_StringToName();


	defaults.object = "*";
	defaults.domain = "Computer Science";
	defaults.organization = "Cornell-Univ";

	if (argc > 1) property = atoi(argv[1]);
	if (argc > 2) pattern = CH_StringToName(argv[2],&defaults);

	printf("Property = %d, pattern = %s:%s:%s\n",
			property,
			pattern.object,pattern.domain,pattern.organization);
	CH_Enumerate(pattern,property,printit);
}

printit(name)
	Name name;
{
	printf("\t%s:%s:%s\n", name.object, name.domain, name.organization);
}
