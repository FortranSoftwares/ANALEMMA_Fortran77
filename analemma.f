      program main

c*********************************************************************72
c
cc MAIN is the main program for ANALEMMA.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c   Sourangshu Ghosh
c
c  Author:
c
c    Original C version by Brian Tung.
c    FORTRAN77 version by John Burkardt.
c
c  Local parameters:
c
c    Local, double precision ECC, the orbital eccentricity.
c
c    Local, double precision LON, the longitude of the perihelion in radians.
c
c    Local, double precision OBLIQ, the obliquity in radians.
c
      implicit none

      character * ( 255 ) arg
      character * ( 80 ) command_filename
      integer command_unit
      character * ( 80 ) data_filename
      integer data_unit
      double precision days
      parameter ( days = 365.242D+00 )
      double precision dec
      double precision degrees
      parameter ( degrees = 3.141592653589793D+00 / 180.0D+00 )
      double precision ecc
      double precision eot
      double precision f
      integer i
      double precision lon
      integer n
      integer numarg
      double precision obliq
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision t
      double precision tau
      double precision theta
      double precision x1
      double precision x2
      double precision x3
      double precision y1
      double precision y2
      double precision y3
      double precision z1
      double precision z2
      double precision z3

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'ANALEMMA'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) 
     &  '  Compute and plot the analemma, equation of time,'
      write ( *, '(a)' ) '  and declination.'
      write ( *, '(a)' ) 
     &  '  This program is based on a C program by Brian Tung.'
c
c  Parse the arguments 
c
      numarg = iargc ( )

      if ( numarg .lt. 1 ) then
        ecc = 0.01671D+00
      else
        i = 1
        call getarg ( i, arg )
        call s_to_r8 ( arg, ecc )
      end if

      if ( numarg .lt. 2 ) then
        lon = 1.347D+00
      else
        i = 2
        call getarg ( i, arg )
        call s_to_r8 ( arg, lon )
        lon = lon * pi / 180.0D+00
      end if

      if ( numarg .lt. 3 ) then
        obliq = 0.4091D+00
      else
        i = 3
        call getarg ( i, arg )
        call s_to_r8 ( arg, obliq )
        obliq = obliq * pi / 180.0D+00
      end if
c
c  Compute the data.
c
      data_filename = 'analemma_data.txt'
      call get_unit ( data_unit )
      open ( unit = data_unit, file = data_filename )

      n = 10000

      do i = 0, n - 1

        f = dble ( i ) / dble ( n )
        tau = 2.0D+00 * pi * f
c
c  Set theta to the current longitude. 
c
        theta = atan2 ( sqrt ( 1.0D+00 - ecc * ecc ) * sin ( tau ), 
     &    cos ( tau ) - ecc )
c 
c  Rotate clockwise in XY plane by theta, corrected by lon.
c
        x1 = cos ( theta - ( lon - pi / 2.0D+00 ) )
        y1 = sin ( theta - ( lon - pi / 2.0D+00 ) )
        z1 = 0.0D+00
c 
c  Rotate counter-clockwise in XZ plane by obliq.
c
        x2 =   cos ( obliq ) * x1 + sin ( obliq ) * z1
        y2 = y1
        z2 = - sin ( obliq ) * x1 + cos ( obliq ) * z1
c 
c  Set t equal to real time from tau and
c  rotate counter-clockwise by t, corrected by lon 
c
        t = tau - ecc * sin ( tau );
        x3 =   cos ( t - ( lon - pi / 2.0D+00 ) ) * x2 
     &       + sin ( t - ( lon - pi / 2.0D+00 ) ) * y2;
        y3 = - sin ( t - ( lon - pi / 2.0D+00 ) ) * x2 
     &       + cos ( t - ( lon - pi / 2.0D+00 ) ) * y2;
        z3 = z2

        eot = - atan2 ( y3, x3 ) * 4.0D+00 / degrees 
     &    * days / ( days + 1.0D+00 )
        dec = asin ( z3 ) / degrees
c 
c  Print results in minutes early/late and degrees north/south 
c
        write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    t / ( 2.0D+00 * pi ), eot, dec

      end do

      close ( unit = data_unit )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Created data file "' // trim ( data_filename ) // '".'
c
c  Create the command file.
c
      command_filename = 'analemma_commands.txt'
      call get_unit ( command_unit )
      open ( unit = command_unit, file = command_filename )
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) 'set output "eot.png"'
      write ( command_unit, '(a)' ) 
     &  'set xlabel "<---Normalized Date--->"'
      write ( command_unit, '(a)' ) 
     &  'set ylabel "<---Minutes Early/Late--->"'
      write ( command_unit, '(a)' ) 'set title "The equation of time"'
      write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) 
     &  // '" using 1:2 with lines'
      write ( command_unit, '(a)' ) 'set output "declination.png"'
      write ( command_unit, '(a)' ) 
     &  'set xlabel "<---Normalized Date--->"'
      write ( command_unit, '(a)' ) 
     &  'set ylabel "<---Degrees North/South--->"'
      write ( command_unit, '(a)' ) 'set title "Declination"'
      write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) 
     &  // '" using 1:3 with lines'
      write ( command_unit, '(a)' ) 'set output "analemma.png"'
      write ( command_unit, '(a)' ) 
     &  'set xlabel "<---Minutes Early/Late--->"'
      write ( command_unit, '(a)' ) 
     &  'set ylabel "<---Degrees North/South--->"'
      write ( command_unit, '(a)' ) 'set title "The analemma"'
      write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) 
     &  // '" using 2:3 with lines'
      write ( command_unit, '(a)' ) 'quit'

      close ( unit = command_unit )

      write ( *, '(a)' ) '  Created command file "' 
     &  // trim ( command_filename ) // '".'
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'ANALEMMA'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine get_unit ( iunit )

c*********************************************************************72
c
cc GET_UNIT returns a free FORTRAN unit number.
c
c  Discussion:
c
c    A "free" FORTRAN unit number is a value between 1 and 99 which
c    is not currently associated with an I/O device.  A free FORTRAN unit
c    number is needed in order to open a file with the OPEN command.
c
c    If IUNIT = 0, then no free FORTRAN unit could be found, although
c    all 99 units were checked (except for units 5, 6 and 9, which
c    are commonly reserved for console I/O).
c
c    Otherwise, IUNIT is a value between 1 and 99, representing a
c    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
c    are special, and will never return those values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer IUNIT, the free unit number.
c
      implicit none

      integer i
      integer iunit
      logical value

      iunit = 0

      do i = 1, 99

        if ( i .ne. 5 .and. i .ne. 6 .and. i .ne. 9 ) then

          inquire ( unit = i, opened = value, err = 10 )

          if ( .not. value ) then
            iunit = i
            return
          end if

        end if

10      continue

      end do

      return
      end
      subroutine s_to_r8 ( s, r8 )

c*********************************************************************72
c
cc S_TO_R8 reads an R8 value from a string.
c
c  Discussion:
c
c    An "R8" value is simply a real number to be stored as a
c    variable of type "double precision".
c
c    The routine will read as many characters as possible until it reaches
c    the end of the string, or encounters a character which cannot be
c    part of the number.
c
c    Legal input is:
c
c       1 blanks,
c       2 '+' or '-' sign,
c       2.5 blanks
c       3 integer part,
c       4 decimal point,
c       5 fraction part,
c       6 'E' or 'e' or 'D' or 'd', exponent marker,
c       7 exponent sign,
c       8 exponent integer part,
c       9 exponent decimal point,
c      10 exponent fraction part,
c      11 blanks,
c      12 final comma or semicolon,
c
c    with most quantities optional.
c
c  Example:
c
c    S                 R8
c
c    '1'               1.0
c    '     1   '       1.0
c    '1A'              1.0
c    '12,34,56'        12.0
c    '  34 7'          34.0
c    '-1E2ABCD'        -100.0
c    '-1X2ABCD'        -1.0
c    ' 2E-1'           0.2
c    '23.45'           23.45
c    '-4.2E+2'         -420.0
c    '17d2'            1700.0
c    '-14e-2'         -0.14
c    'e2'              100.0
c    '-12.73e-9.23'   -12.73 * 10.0^(-9.23)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, the string containing the
c    data to be read.  Reading will begin at position 1 and
c    terminate at the end of the string, or when no more
c    characters can be read to form a legal real.  Blanks,
c    commas, or other nonnumeric data will, in particular,
c    cause the conversion to halt.
c
c    Output, double precision R8, the value read from the string.
c
      implicit none

      character c
      integer ierror
      integer ihave
      integer isgn
      integer iterm
      integer jbot
      integer jsgn
      integer jtop
      integer length
      integer ndig
      double precision r8
      double precision rbot
      double precision rexp
      double precision rtop
      character * ( * ) s
      integer s_length
      character TAB
      parameter ( TAB = char ( 9 ) )

      s_length = len_trim ( s )

      ierror = 0
      r8 = 0.0D+00
      length = -1
      isgn = 1
      rtop = 0
      rbot = 1
      jsgn = 1
      jtop = 0
      jbot = 1
      ihave = 1
      iterm = 0

10    continue

        length = length + 1

        if ( s_length .lt. length + 1 ) then
          go to 20
        end if

        c = s(length+1:length+1)
c
c  Blank character.
c
        if ( c .eq. ' ' .or. c .eq. TAB ) then

          if ( ihave .eq. 2 ) then

          else if ( ihave .eq. 6 .or. ihave .eq. 7 ) then
            iterm = 1
          else if ( 1 < ihave ) then
            ihave = 11
          end if
c
c  Comma.
c
        else if ( c .eq. ',' .or. c .eq. ';' ) then

          if ( ihave .ne. 1 ) then
            iterm = 1
            ihave = 12
            length = length + 1
          end if
c
c  Minus sign.
c
        else if ( c .eq. '-' ) then

          if ( ihave .eq. 1 ) then
            ihave = 2
            isgn = -1
          else if ( ihave .eq. 6 ) then
            ihave = 7
            jsgn = -1
          else
            iterm = 1
          end if
c
c  Plus sign.
c
        else if ( c .eq. '+' ) then

          if ( ihave .eq. 1 ) then
            ihave = 2
          else if ( ihave .eq. 6 ) then
            ihave = 7
          else
            iterm = 1
          end if
c
c  Decimal point.
c
        else if ( c .eq. '.' ) then

          if ( ihave .lt. 4 ) then
            ihave = 4
          else if ( 6 .le. ihave .and. ihave .le. 8 ) then
            ihave = 9
          else
            iterm = 1
          end if
c
c  Scientific notation exponent marker.
c
        else if ( c .eq. 'E' .or. c .eq. 'e' .or. 
     &            c .eq. 'D' .or. c .eq. 'd' ) then

          if ( ihave .lt. 6 ) then
            ihave = 6
          else
            iterm = 1
          end if
c
c  Digit.
c
        else if (  ihave .lt. 11 .and. lle ( '0', c ) 
     &    .and. lle ( c, '9' ) ) then

          if ( ihave .le. 2 ) then
            ihave = 3
          else if ( ihave .eq. 4 ) then
            ihave = 5
          else if ( ihave .eq. 6 .or. ihave .eq. 7 ) then
            ihave = 8
          else if ( ihave .eq. 9 ) then
            ihave = 10
          end if

          ndig = ichar ( c ) - 48

          if ( ihave .eq. 3 ) then
            rtop = 10.0D+00 * rtop + dble ( ndig )
          else if ( ihave .eq. 5 ) then
            rtop = 10.0D+00 * rtop + dble ( ndig )
            rbot = 10.0D+00 * rbot
          else if ( ihave .eq. 8 ) then
            jtop = 10 * jtop + ndig
          else if ( ihave .eq. 10 ) then
            jtop = 10 * jtop + ndig
            jbot = 10 * jbot
          end if
c
c  Anything else is regarded as a terminator.
c
        else
          iterm = 1
        end if
c
c  If we haven't seen a terminator, and we haven't examined the
c  entire string, go get the next character.
c
        if ( iterm .eq. 1 ) then
          go to 20
        end if

      go to 10

20    continue
c
c  If we haven't seen a terminator, and we have examined the
c  entire string, then we're done, and LENGTH is equal to S_LENGTH.
c
      if ( iterm .ne. 1 .and. length + 1 .eq. s_length ) then
        length = s_length
      end if
c
c  Number seems to have terminated.  Have we got a legal number?
c  Not if we terminated in states 1, 2, 6 or 7!
c
      if ( ihave .eq. 1 .or. ihave .eq. 2 .or. ihave .eq. 6 .or. 
     &  ihave .eq. 7 ) then
        ierror = ihave
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'S_TO_R8 - Serious errorc'
        write ( *, '(a)' ) '  Illegal or nonnumeric input:'
        write ( *, '(a)' ) '    ' // trim ( s )
        stop
      end if
c
c  Number seems OK.  Form it.
c
      if ( jtop .eq. 0 ) then
        rexp = 1.0D+00
      else
        if ( jbot .eq. 1 ) then
          rexp = 10.0D+00 ** ( jsgn * jtop )
        else
          rexp = 10.0D+00 ** ( dble ( jsgn * jtop ) / dble ( jbot ) )
        end if
      end if

      r8 = dble ( isgn ) * rexp * rtop / rbot

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    None
c
      implicit none

      character * ( 8 ) ampm
      integer d
      character * ( 8 ) date
      integer h
      integer m
      integer mm
      character * ( 9 ) month(12)
      integer n
      integer s
      character * ( 10 ) time
      integer y

      save month

      data month /
     &  'January  ', 'February ', 'March    ', 'April    ',
     &  'May      ', 'June     ', 'July     ', 'August   ',
     &  'September', 'October  ', 'November ', 'December ' /

      call date_and_time ( date, time )

      read ( date, '(i4,i2,i2)' ) y, m, d
      read ( time, '(i2,i2,i2,1x,i3)' ) h, n, s, mm

      if ( h .lt. 12 ) then
        ampm = 'AM'
      else if ( h .eq. 12 ) then
        if ( n .eq. 0 .and. s .eq. 0 ) then
          ampm = 'Noon'
        else
          ampm = 'PM'
        end if
      else
        h = h - 12
        if ( h .lt. 12 ) then
          ampm = 'PM'
        else if ( h .eq. 12 ) then
          if ( n .eq. 0 .and. s .eq. 0 ) then
            ampm = 'Midnight'
          else
            ampm = 'AM'
          end if
        end if
      end if

      write ( *,
     &  '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' )
     &  d, month(m), y, h, ':', n, ':', s, '.', mm, ampm

      return
      end
