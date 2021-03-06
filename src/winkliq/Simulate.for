

C                        I   I       I        i        I
C        CALL SIMULATE(MAXG,KPGROUP,NUMTEACH,MAXCHOIC,MAXFREQ,
C         I     I     ?     I
C     C INMAT,DEPART,SSEED,USEMARG)


      SUBROUTINE SIMULATE(NUMGROUP,PERGROUP,NUMPEOPL,ACHOICE,MAXFREQ,
     C OUTMAT,DEPART,QSEED,USEMARG)
      INCLUDE 'PARAM.H'
      INTEGER D,I,J,DEPART(MAXKLIQ),D2,D3,CHOICES(MAXKLIQ),
     C        OUTMAT(MAXKLIQ,MAXKLIQ),
     C        D4,D5,COL,NUMGROUP,PERGROUP,NUMPEOPL,ONELESS,ACHOICE,
     C        MAXFREQ,MV,FREQS(MAXKLIQ),USEMARG,TID
       DOUBLE PRECISION QSEED
C      QSEED=5163.D0
      ONELESS=NUMPEOPL-1
      D=0
      IF (USEMARG .EQ. 0) THEN
      DO 84 MV=1,MAXFREQ
       FREQS(MV)=MV
   84 CONTINUE
      DO 4 D2=1,ACHOICE
       CHOICES(D2)=1
    4 CONTINUE
      DO 5 D3=(ACHOICE+1),ONELESS
       CHOICES(D3)=0
    5 CONTINUE
C      
      END IF

      DO 2 I=1,NUMGROUP
       DO 3 J=1,PERGROUP
       D=D+1
       DEPART(D)=I
    3 CONTINUE
    2 CONTINUE
C   
      CALL SRESORTO(DEPART,NUMPEOPL,QSEED)
C
      DO 6 D4=1,NUMPEOPL
      IF (USEMARG .EQ. 1) THEN
      MAXFREQ=1.
      DO 10 D3=1,NUMPEOPL
       IF (D3 .NE. D4) THEN

       TID=D3
       IF (D3 .GT. D4) THEN
        TID=TID-1.
       END IF
       CHOICES(TID)=OUTMAT(D4,D3)
C       OUTMAT NE 0
       END IF
C       D3 NE D4
00010   CONTINUE
       END IF
C       USEMARG
      
      CALL SRESORTO(CHOICES,ONELESS,QSEED)
      OUTMAT(D4,D4)=0
      DO 7 D5=1,ONELESS
      COL=D5
      IF (COL .GE. D4) THEN
       COL=COL+1
      END IF
      IF (USEMARG .EQ. 0) THEN
      CALL SRESORTO(FREQS,MAXFREQ,QSEED)
      ELSE
      FREQS(1)=1.
      END IF
      OUTMAT(D4,COL)=CHOICES(D5)*FREQS(1)  
C      COL=COL+1
    7 CONTINUE
    6 CONTINUE

  101 FORMAT(I2,251(I1))
  112 FORMAT(I3,1X,I3,1X,I3,1X,I3,1X,I3)
      END
