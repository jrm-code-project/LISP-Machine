-*- Mode:Text; Package:ZWEI -*-

This directory contains a menu-driven DIRED that has all
the capabilities of both DIRED and FS:DEBUG. It allows you
to find a file in the DIRED way and do anything to it you can
do in regular DIRED.  In addition, you can enter a DEBUG mode.
Then you can do anything to the file that you could with
FS:DEBUG, by making choices in a fixed CHOOSE-VARIABLE-VALUES
pane. The filemap is displayed in a scrollable window next to
the choice pane.

To run DIRED-FSDEBUG, load the sysdef, then do META-X DIRED.
The rest should be self-explanatory.

DIRED-FSDEBUG was tested under 3.0. I haven't tried it under 119.


BUGS:

A zero-length file or empty directory gets an error in
DEBUG mode because they have no file map at all, while the
program expects everything to have one. Easy fix.

The highlight-bar that marks the current file can move
above the first file or below the last one. This is harmless
but annoying. Maybe not so easy: ZMACS is a jealous god.


INCOMPLETENESSES:

There is some code to lock out remote file servers during
debugging, but it is turned off to avoid locking everyone out
during prolonged testing sessions. It needs to be turned on.

When a file is edited from DIRED, the DIRED window remains on
display even though it is not relevant to editing. The DIRED
constraint frames have configurations to reduce everything back
to a single ZMACS pane for editing, then return to DIRED
configuration when the next DIRED is requested. These are not
turned on yet. To use them, put appropriate :SET-CONFIGURATION
messages into the (:AFTER :SET-INTERVAL-INTERNAL) methods in the
ODM-DIRED-ENVIRONMENT file.

Additional menus and configurations could produce a menu-driven
ZMACS that configures itself for various levels of user, types of
editing, or whatever. The essential machinery is all there.
