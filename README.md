# ANALEMMA_Fortran77

## Evaluating the Equation of Time
ANALEMMA is a FORTRAN77 program which evaluates the equation of time, based on a C program by Brian Tung.

The program can compute and plot the equation of time, the declination, and an analemma curve for various orbital parameters. The analemma is the curve traced by the position of the sun, measured at clock noon, over a year.

The program creates data and command files which must be processed by the GNUPLOT program:

        gnuplot < analemma_commands.txt
      
which will create PNG images of the analemma, the declination, and the equation of time.

## Usage:
analemma ecc lon obliq where the optional input parameters are:
- ecc, the eccentricity;
- lon, the perihelion longitude in degrees;
- obliq, the axial obliquity in degrees;

The program with no options uses the following default values:

- eccentricity = 0.01671;
- longitude of perihelion = 1.347 radians = 77.18 degrees;
- axial obliquity = 0.4091 radians = 23.44 degrees;
Licensing:
The computer code and data files described and made available on this web page are distributed under the MIT license.


## Author:
Original C version by Brian Tung. 
FORTRAN77 version by Sourangshu Ghosh.

## Reference:
Brian Tung,
Figure Eight in the Sky, a new perspective on an old fascination,
Astronomical Games, August 2002,
http://www.astronomycorner.net/games/analemma.html.

## Source Code:
- analemma.f, the source code.
- analemma.sh, BASH commands to compile the source code.

## Examples and Tests:
- analemma_data.txt the data file.
- analemma_commands.txt, the command file.
- analemma.png, the PNG file of the analemma, created by GNUPLOT.
- declination.png, the PNG file of the declination, created by GNUPLOT.
- eot.png, the PNG file of the equation of time, created by GNUPLOT.

## List of Routines:
- **MAIN** is the main program for ANALEMMA.
- **GET_UNIT** returns a free FORTRAN unit number.
- **S_TO_R8** reads an R8 value from a string.
- **TIMESTAMP** prints the current YMDHMS date as a time stamp.

