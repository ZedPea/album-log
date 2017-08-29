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

Then either add ~/.local/bin to your path and run 

`album-log filename`

Or, run

`stack exec album-log filename`

Where filename is the file you want parsed.

Currently the program opens the file specified, parses it, and outputs the decoded file to output.txt

It will sort artists and albums case insensitively.

More options and details on the file layout will come later.
