
This directory contains BoTeX sources for the K processor technical
manual.  The main file of interest is Manual.botex, which contains
"@include" directives to load in BoTeX files for the various chapters.

To recompile the manual, execute the command "botex Manual" within this
directory.  To print it out, execute the command "iptex Manual.dvi"

Note that the manual makes use of a custom library, byf1.lib.  A second
copy of this library file is kept in this directory; the "real" copy is
kept in /usr/lib/tex/macros, where BoTeX expects to find it.

                James Rauen
                Consultant
                GigaMos Systems, Inc.
                10-May-88




