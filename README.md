# album-log
A set of utilities for logging albums listened to, the date listened to, the total unique albums listened to, etc

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

You can also specify the command to run. The default command is to add, but you can also specify `album-log --command=create`

This will create a sample file which can then be manually added to or parsed and then added to.

You can specify a filepath to write the sample file to, or you can leave it blank and it will be written to output.txt

`album-log --command=create --file="albums.txt"`


Currently the program writes the new file to output.txt, however once the program is mature it will overwrite the old file. I'm doing it this way so I don't accidentaly nuke my file ;)
