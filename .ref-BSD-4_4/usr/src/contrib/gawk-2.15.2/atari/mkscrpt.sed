# there is no automatic editing of Makefile for Atari right now
# but lines starting with "MAKE_" string are processed for consistency
# with other configuration files and in a case they would be needed
# in a future
:start
    /^MAKE_/d
    /^[^#]/s/.*/s~__SYSTEM__~&~/p
    t cont
    n
    b start
:cont
    n
    /^MAKE_/d
    /^[^#]/s:^\([^ 	]*\)[ 	].*:s~^/\\* #define[ 	]*\1.*~#define  &~:p
b cont
