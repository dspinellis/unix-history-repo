s/y\.tab\./parse_tab\./
s/@DEFS@/-DMS_DOS/
s/@LIBS@//
s/@srcdir@/./
s/@YACC@/bison/
s/@CC@/gcc/
s/@RANLIB@/ranlib/
s/@ALLOCA@//
/^flex/ s/\.bootstrap//
/sed.*extern.*malloc/ c\
	@mv parse_tab.c parse.c
/rm.*parse_tab.c/ d
