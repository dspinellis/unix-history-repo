: run this script through /bin/sh

db=rcvtrip draftf=drafts field=advised form=tripcomps pgm=bin/rcvtrip

echo "Remove the line

	addr $USER pipe R \"$pgm \\\"\$(reply-to)\\\"\"

from the .maildelivery file in your HOME directory.  It should
be the last line there."

exit 0
