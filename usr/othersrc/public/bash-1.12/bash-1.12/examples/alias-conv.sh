#! /bin/sh
#
# Convert Csh aliases to Bash aliases.  Adapted from a similar program
# supplied with zsh.
#
# This is a quick script to convert csh aliases to bash aliases/functions.
# Pipe the output of csh's alias command through this; it will generate
# a series of alias/function definitions on stdout, suitable for
# processing by bash.
#
# This is not perfect, but it gets most common aliases; it should manage to
# cut down a lot of the busy work.
#
sed -e 's/	(\(.*\))/	\1/' >/tmp/cz$$.1
grep ! /tmp/cz$$.1 >/tmp/cz$$.2
grep -v ! /tmp/cz$$.1 >/tmp/cz$$.3
sed -e "s/'/'"\\\\"''"/g -e 's/^\([^	]*\)	\(.*\)$/alias \1='"'\2'/" \
	/tmp/cz$$.3
sed -e 's/![:#]*/$/g' -e 's/^\([^	]*\)	\(.*\)$/\1 () { \2 }/' /tmp/cz$$.2
rm /tmp/cz$$.?

exit 0
