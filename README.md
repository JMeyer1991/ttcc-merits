This software offers a quick and easy way to calculate how many merits you
require for your next promotion in Toontown: Corporate Clash, as well as how
to best acquire them through your preferred method: buildings (departmental
or Boardbot) or facilities. You are free to copy, use, and modify this
software as you see fit. This software is not in any way affiliated with
Toontown: Corporate Clash or its development team.

Special thanks to the editors of the official Toontown: Corporate Clash wiki
for recording the estimated number of merits each building and facility yields.

# Building the Software

This software is written in "modern" (i.e., post-1990) Fortran and should be
supported by most Fortran compilers. If you are unsure which to use, the
GNU Fortran compiler (part of the GNU Compiler Collection) is recommended.

To build ttcc-merits using the GNU Fortran compiler, use the following command:

`gfortran merits.f90 main.f90 -o ttcc_merits`

# Using the Software

You can use the software interactively by simply typing `.\ttcc_merits` at
the terminal prompt. This will interactively walk you through the calculations
for how to best obtain merits in your desired department. Alternatively, you
may wish to use the command flags below.

## Command Flags

The following command flags are available. If any flag is omitted, you will
be prompted to provide that information interactively.

* `-b <num>`: current number of boosters
* `-d <dpt>`: department (s, c, l, or b)
* `-h <num>`: current number of merits
* `-p`: persistent mode; keeps looping through interactive prompts until exit
* `-r <num>`: target number of merits for next promotion
* `--help`: display help and exit

For example, a toon with one booster and 1400 merits who needs 3140 total for
their next Cashbot promotion would use the following command:

`.\ttcc_merits -b 1 -d c -h 1400 -r 3140`
