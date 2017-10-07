# album-log
Log albums listened to, the date listened to, the total unique albums listened to, etc

## Installation

#### Install prerequisited
You need stack installed.

##### Debian based:
`sudo apt-get install haskell-stack`

##### Arch based:
`sudo pacman -S stack`

#### Clone the repository

`git clone https://github.com/ZedPea/album-log.git`

`cd album-log`

#### Compile and install

`stack install`

#### Running

Either add ~/.local/bin to your path and run 

`album-log`

Or, run

`stack exec album-log`

You will then be prompted for a file name, an album, and an artist to add.

#### Arguments

You can pass arguments on the command line.

`album-log --file="/media/albums.txt" --artist="Foo" --album="Bar"`

Quotes are optional but should be used if you have weird characters, spaces, etc

You can specify as many or as few of these as you like on the command line, you will be prompted for missing ones.

The file will be updated in place if you specify a file on the command line.

#### Commands

##### add
`album-log --command=add`

This is the default command, it doesn't need to be specified.

##### create
`album-log --command=create --file=newfile.txt`

This creates a new parsable file, with empty content. Specify the file you wish it to be outputted to, or it will output to output.txt.

##### remove
`album-log --command=remove --album=mistyped-album --artist=mistyped-artist`

This removes the specified album from the file. It will alert you if it couldn't find the album you specified.

Note that this can make your album/date mapping inaccurate. 
