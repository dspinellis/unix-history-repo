: run this script through /bin/sh

db=rcvtrip draftf=drafts field=advised form=tripcomps pgm=bin/rcvtrip

if [ -x $HOME/$pgm ];
then
    echo "This is ontrip, for MH.6"
else
    echo "Script $pgm is not present in your HOME directory" 1>&2
    exit 1
fi

file=`mhpath +`/$form

if [ -f $file ];
then
    if [ "x$EDITOR" = x ];
    then
	echo "Please edit the reply template:"
	echo "You are now in the editor 'ex'"
	ex $file	
    else
	$EDITOR $file
    fi
else
    echo "Reply template $form is not present in your MH directory" 1>&2
    exit 1
fi

folder=`mhpath +$db`
if [ -d $folder ];
then
    echo "Resetting database +$db"
    rmm +$db all > /dev/null 1>&2
else
    folder +$db
fi
touch $folder/1

echo "Now add the line

	addr $USER pipe R \"$pgm \\\"\$(reply-to)\\\"\"

to the end of the .maildelivery file in your HOME directory"

exit 0
