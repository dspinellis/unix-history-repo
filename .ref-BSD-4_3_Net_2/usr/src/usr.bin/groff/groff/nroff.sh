#! /bin/sh -
exec groff -Tascii -C -mtty ${1+"$@"}
