# album-log
A set of utilities for logging albums listened to, the date listened to, the total unique albums listened to, etc

Mainly started to try out using parsec on my previously manually curated file layout, which grew unwieldly to edit manually.

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

You can specify just the file, or just the album and artist, but don't specify one of the album or artist, specify both.

There is also a --command switch which will be used later for other modes than adding.
