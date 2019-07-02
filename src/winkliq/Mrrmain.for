      program smatst
c
c   This program tests the ability to call a FORTRAN program
c   from within another FORTRAN program.
c
c   Mark Riordan   22 Apr 94
c
      CALL TESTSUB
      END
      SUBROUTINE TESTSUB
      character *(*) prognm
      parameter (prognm='/home/edstat/frank/cluster/develop/smacof1b')
      integer retcod

      retcod = sh(prognm)

      write(*,*) prognm,' returned code ',retcod
       RETURN
      end
      
