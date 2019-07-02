      integer function system(command)
      include 'fsublib.fi'
      character*(*) command
      integer retcode

      system = fsystem(command)
      return
      end

      subroutine fdate(dstr)
      include 'fsublib.fi'
      include 'fsignal.fi'
      character*(*) dstr
      integer*2 year,month,day,hours,mins,secs,hsecs
      character*3 months(12)
      external fpehandler
      integer      signal_count, signal_number, signal_type
      common /fpe/ signal_count, signal_number, signal_type


      months(1) = 'Jan'
      months(2) = 'Feb'
      months(3) = 'Mar'
      months(4) = 'Apr'
      months(5) = 'May'
      months(6) = 'Jun'
      months(7) = 'Jul'
      months(8) = 'Aug'
      months(9) = 'Sep'
      months(10) = 'Oct'
      months(11) = 'Nov'
      months(12) = 'Dec'
      call getdat(year,month,day)
      call gettim(hours,mins,secs,hsecs)

      write(dstr,100) months(month),day,hours,mins,secs,year
100   format(A3,' ',I2,' ',I2,':',I2,':',I2,' 20',I2)
C     now, set up the error handler
      call fsignal( SIGFPE, fpehandler )
      return
      end

      subroutine getarg(argnum,arg)
      include 'fsublib.fi'
      integer argnum,argc,arglen
      character*256 targ
      character*(*) arg

      argc = iargc()
      if (argnum .gt. argc) then
         arg(1:1) = '\0'
      else
         arglen = igetarg(argnum,arg)
      endif
      return
      end

      subroutine unlink(fname)
      include 'fsublib.fi'
      integer retval
      character*(*) fname
      character*1024 command

      open(97,FILE=fname)
      close(97,STATUS='DELETE')
C     write(command,100) fname
C     retval = fsystem(command)
      return
100   format('remove ',A255)
      end

*$ifdef __386__
*$ifdef __stack_conventions__
*$pragma aux _clear87 "!"
*$else
*$pragma aux _clear87 "!_"
*$endif
*$else
*$pragma aux _clear87 "!_"
*$endif
*$pragma aux fpehandler parm( value, value )

      subroutine fpehandler( sig_num, fpe_type )

      implicit none

*     sig_num and fpe_type are passed by value, not by reference

      integer sig_num, fpe_type

      include 'fsignal.fi'

      integer      signal_count, signal_number, signal_type
      common /fpe/ signal_count, signal_number, signal_type
*     we could add this to our common block
*     integer      signal_split( FPE_INVALID:FPE_IOVERFLOW )

      signal_count = signal_count + 1
      signal_number = sig_num
      signal_type = fpe_type

*     floating-point exception types

*     FPE_INVALID         = 129 (0)
*     FPE_DENORMAL        = 130 (1)
*     FPE_ZERODIVIDE      = 131 (2)
*     FPE_OVERFLOW        = 132 (3)
*     FPE_UNDERFLOW       = 133 (4)
*     FPE_INEXACT         = 134 (5)
*     FPE_UNEMULATED      = 135 (6)
*     FPE_SQRTNEG         = 136 (7)
*     undefined           = 138 (8)

*     FPE_STACKOVERFLOW   = 137 (9)
*     FPE_STACKUNDERFLOW  = 138 (10)
*     FPE_EXPLICITGEN     = 139 (11)
*     FPE_IOVERFLOW       = 140 (12)

*     log the type of error for interest only */
*     signal_split( fpe_type ) =
*    1signal_split( fpe_type ) + 1

*     get rid of any errors
      call _clear87

*     resignal for more exceptions

      call fsignal( SIGFPE, fpehandler )

*     if we don't then a subsequent exception will
*     cause an abnormal program termination

      end

