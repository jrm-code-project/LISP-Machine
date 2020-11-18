
  "Ftp is the user interface to the ARPANET standard File
Transfer Protocol.  The program allows a user to transfer
files to and from a remote network site.

The client host with which ftp is to communicate may be
specified on the command line.  If this is done, ftp will
immediately attempt to establish a connection to an FTP
server on that host; otherwise, ftp will enter its command
interpreter and await instructions from the user.  When ftp
is awaiting commands from the user the prompt /"ftp>/" is pro-
vided the user.  The following commands are recognized by
ftp:

append local-file [ remote-file ]
     Append a local file to a file on the remote machine.
     If remote-file is left unspecified, the local file name
     is used in naming the remote file.  File transfer uses
     the current settings for type, format, mode, and struc-
     ture.

ascii
     Set the file transfer type to network ASCII.  This is
     the default type.

bell Arrange that a bell be sounded after each file transfer
     command is completed.

binary
     Set the file transfer type to support binary image
     transfer.

bye  Terminate the FTP session with the remote server and
     exit ftp.

cd remote-directory
     Change the working directory on the remote machine to
     remote-directory.

close
     Terminate the FTP session with the remote server, and
     return to the command interpreter.

delete remote-file
     Delete the file remote-file on the remote machine.

debug [ debug-value ]
     Toggle debugging mode.  If an optional debug-value is
     specified it is used to set the debugging level.  When
     debugging is on, ftp prints each command sent to the
     remote machine, preceded by the string /"-->/".

dir [ remote-directory ] [ local-file ]
     Print a listing of the directory contents in the direc-
     tory, remote-directory, and, optionally, placing the
     output in local-file.  If no directory is specified,
     the current working directory on the remote machine is
     used.  If no local file is specified, output comes to
     the terminal.

form format
     Set the file transfer form to format.  The default for-
     mat is /"file/".

get remote-file [ local-file ]
     Retrieve the remote-file and store it on the local
     machine.  If the local file name is not specified, it
     is given the same name it has on the remote machine.
     The current settings for type, form, mode, and struc-
     ture are used while transferring the file.

hash Toggle hash-sign (``#'') printing for each data block
     transferred.  The size of a data block is 1024 bytes.

glob Toggle file name globbing.  With file name globbing
     enabled, each local file or pathname is processed for
     csh(1) metacharacters.  These characters include
     ``*?[]~{}''.  Remote files specified in mutliple item
     commands, e.g.  mput, are globbed by the remote server.
     With globbing disabled all files and pathnames are
     treated literally.

help [ command ]
     Print an informative message about the meaning of com-
     mand.  If no argument is given, ftp prints a list of
     the known commands.

lcd [ directory ]
     Change the working directory on the local machine.  If
     no directory is specified, the user's home directory is
     used.

ls [ remote-directory ] [ local-file ]
     Print an abbreviated listing of the contents of a
     directory on the remote machine.  If remote-directory
     is left unspecified, the current working directory is
     used.  If no local file is specified, the output is
     sent to the terminal.

mdelete remote-files
     Delete the specified files on the remote machine.  If
     globbing is enabled, the specification of remote files
     will first be expanded using ls.

mdir remote-files local-file
     Obtain a directory listing of multiple files on the
     remote machine and place the result in local-file.

mget remote-files
     Retrieve the specified files from the remote machine
     and place them in the current local directory.  If
     globbing is enabled, the specification of remote files
     will first be expanding using ls.

mkdir directory-name
     Make a directory on the remote machine.

mls remote-files local-file
     Obtain an abbreviated listing of multiple files on the
     remote machine and place the result in local-file.

mode [ mode-name ]
     Set the file transfer mode to mode-name.  The default
     mode is /"stream/" mode.

mput local-files
     Transfer multiple local files from the current local
     directory to the current working directory on the
     remote machine.

open host [ port ]
     Establish a connection to the specified host FTP
     server.  An optional port number may be supplied, in
     which case, ftp will attempt to contact an FTP server
     at that port.  If the auto-login option is on
     (default), ftp will also attempt to automatically log
     the user in to the FTP server (see below).

prompt
     Toggle interactive prompting.  Interactive prompting
     occurs during multiple file transfers to allow the user
     to selectively retrieve or store files.  If prompting
     is turned off (default), any mget or mput will transfer
     all files.

put local-file [ remote-file ]
     Store a local file on the remote machine.  If remote-
     file is left unspecified, the local file name is used
     in naming the remote file.  File transfer uses the
     current settings for type, format, mode, and structure.

pwd  Print the name of the current working directory on the
     remote machine.

quit A synonym for bye.

quote arg1 arg2 ...
     The arguments specified are sent, verbatim, to the
     remote FTP server.  A single FTP reply code is expected
     in return.

recv remote-file [ local-file ]
     A synonym for get.

remotehelp [ command-name ]
     Request help from the remote FTP server.  If a
     command-name is specified it is supplied to the server
     as well.

rename [ from ] [ to ]
     Rename the file from on the remote machine, to the file
     to.

rmdir directory-name
     Delete a directory on the remote machine.

send local-file [ remote-file ]
     A synonym for put.

sendport
     Toggle the use of PORT commands.  By default, ftp will
     attempt to use a PORT command when establishing a con-
     nection for each data transfer.  If the PORT command
     fails, ftp will use the default data port.  When the
     use of PORT commands is disabled, no attempt will be
     made to use PORT commands for each data transfer.  This
     is useful for certain FTP implementations which do
     ignore PORT commands but, incorrectly, indicate they've
     been accepted.

status
     Show the current status of ftp.

struct [ struct-name ]
     Set the file transfer structure to struct-name.  By
     default /"stream/" structure is used.

tenex
     Set the file transfer type to that needed to talk to
     TENEX machines.

trace
     Toggle packet tracing.

type [ type-name ]
     Set the file transfer type to type-name.  If no type is
     specified, the current type is printed.  The default
     type is network ASCII.

user user-name [ password ] [ account ]
     Identify yourself to the remote FTP server.  If the
     password is not specified and the server requires it,
     ftp will prompt the user for it (after disabling local
     echo).  If an account field is not specified, and the
     FTP server requires it, the user will be prompted for
     it.  Unless ftp is invoked with /"auto-login/" disabled,
     this process is done automatically on initial connec-
     tion to the FTP server.

verbose
     Toggle verbose mode.  In verbose mode, all responses
     from the FTP server are displayed to the user.  In
     addition, if verbose is on, when a file transfer com-
     pletes, statistics regarding the efficiency of the
     transfer are reported.  By default, verbose is on.

? [ command ]
     A synonym for help.

Command arguments which have embedded spaces may be quoted
with quote (/") marks.

FILE TRANSFER PARAMETERS

The FTP specification specifies many parameters which may
affect a file transfer.  The type may be one of /"ascii/",
/"image/" (binary), /"ebcdic/", and /"local byte size/" (for PDP-
10's and PDP-20's mostly).  Ftp supports the ascii and image
types of file transfer.

Ftp supports only the default values for the remaining file
transfer parameters: mode, form, and struct."
