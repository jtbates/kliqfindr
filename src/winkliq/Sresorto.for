
C
C     SUBROUTINE FOR RESTORTING ELEMENTS OF GROUP LIST
      SUBROUTINE SRESORTO (ELEMENTS,NUMELEM,RSEED)
      INCLUDE 'PARAM.H'
      INTEGER ELEMENTS(MAXKLIQ),NUMELEM,FAKETCH
      INTEGER OELEMENT(MAXKLIQ),Z,J,K,I,CONVERT(MAXKLIQ),RITER
      DOUBLE PRECISION RSEED
      INTEGER RANDARRY(MAXKLIQ),OLDPOS(MAXKLIQ)
C
C   NOW THE RANDARRY IS AN ARRAY OF RANDOM NUMBERS OF LENGTH NUMELEM
C
      DO 3 K=1,NUMELEM
      OELEMENT(K)=ELEMENTS(K)
    3 CONTINUE
      FAKETCH=NUMELEM
      CALL GGPER(RSEED,FAKETCH,RANDARRY)
      DO 4 Z=1,NUMELEM
      ELEMENTS(Z)=OELEMENT(RANDARRY(Z))
C      WRITE(18,219) Z,RANDARRY(Z),OELEMENT(Z),ELEMENTS(Z)
    4 CONTINUE
  219 FORMAT(I3,1X,I5,1X,I5,1X,I3)
      RETURN
      END
