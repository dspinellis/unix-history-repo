#
# gulam script to produce configuration file for Atari ST;
# performs the same job as configure, but only for this specific configuration;
# it is assumed that it is located in a subdirectory .\atari
#
if { -e ..\config\atari }
    sed -n -f mkscrpt.sed ..\config\atari > sedscr
    sed -f sedscr ..\config.h-d > config.h
    sed -n '/^#echo./s///p' ..\config\atari
    rm sedscr
    mv config.h ..
ef
    echo "'..\config\atari' was lost somewhere"
    echo "Either construct one based on the examples in the config directory,"
    echo "or, in source directory, copy config.h-dist to config.h and edit it."
    exit 1
endif
exit 0
