      subroutine runsmac
c
c   This program tests the ability to call a FORTRAN program
c   from within another FORTRAN program.
c
c   Mark Riordan   22 Apr 94
c
      character *(*) prognm
C     parameter (prognm='/home5/edstat/frank/bin/smacof1b')
      parameter (prognm='./smacof1b')
      integer retcod
      call unlink( 'smacdist')
      call unlink( 'coeff')
C      retcod = system(prognm)
       call system(prognm)

C      write(*,*) prognm,' returned code ',retcod
       RETURN
      end
