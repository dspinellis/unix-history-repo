#!/bin/sh
# nohup -- run a command immume to hangups, with output to a non-tty
# Copyright (C) 1991 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

# David MacKenzie <djm@ai.mit.edu>

if [ $# -eq 0 ]; then
  echo "Usage: nohup command [arg...]" 2>&1
  exit 1
fi

trap "" 1
oldmask=`umask`; umask 077
if [ -t 1 ]; then
  if cat /dev/null >> nohup.out; then
    echo "nohup: appending output to \`nohup.out'" 2>&1
    umask $oldmask
    exec nice -5 "$@" >> nohup.out 2>&1
  else
    cat /dev/null >> $HOME/nohup.out
    echo "nohup: appending output to \`$HOME/nohup.out'" 2>&1
    umask $oldmask
    exec nice -5 "$@" >> $HOME/nohup.out 2>&1
  fi
else
  umask $oldmask
  exec nice -5 "$@"
fi
