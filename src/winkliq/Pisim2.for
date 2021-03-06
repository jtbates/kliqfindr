
      SUBROUTINE PISIM2(NUMPEOPL,RANGEC,MINCHOI,RANGEG,BASEG,
     C BASEP,BETARAT,BETARAT2,OUTMAT,QSEED,DEPART,MAXFREQ,RANGEP,
     C BASEWP,WPIW,WPIB,WRANGEP,NSIM,XBASEP,XRANGEP,
     C PREPWI,PREQWI,PREPBT,PREQBT,PKPG,QKPG,PSIZE,QSIZE)
      INCLUDE 'PARAM.H'


      INTEGER D,I,J,DEPART(MAXKLIQ),D2,D3,OUTMAT(MAXKLIQ,MAXKLIQ),
     C        SIMRAND,THISD,
     C        D4,D5,COL,NUMGROUP,PERGROUP,NUMPEOPL,ONELESS,ACHOICE,
     C        MAXFREQ,MV,FREQS(MAXKLIQ),USEMARG,TID,NR,AC,DIDG,DIDNG,
     C       OPENS,HAVEMEM,FILLG,FILLNG,COUNTING,COUNTNNG,FG,NFG,
     C  COUNTG(MAXKLIQ),ING,GOTTEM,TG,IR(1),BYDENSE,TWT,NSIM,
     C  ELEM(MAXKLIQ),
     C KCHOICE(MAXKLIQ),MINCHOI,ONE,BASEG,RANGEG,DMEM,TARGETS,TN,
     C KOCHOICE(MAXKLIQ),DIDEM,RANGEC,NUMG,IDEV(1),NSUBID(MAXKLIQ),ZERO,
     C RANGEP,WRANGEP,BADONE
       INTEGER*4
     C  MEMBERS(100,MAXGR),NONMEM(100,MAXKLIQ),BI,BG
       DOUBLE PRECISION QSEED
       REAL DEVIATE(1),INDEPT(MAXKLIQ),OUTDEPT(MAXKLIQ),EXTRA,PORTION,
     C KPOSS,
     C R(1),INP(MAXKLIQ),INQ(MAXKLIQ),OUTP(MAXKLIQ),XBASEP,XRANGEP,
     C OUTQ(MAXKLIQ),WOUTQ(MAXKLIQ),REM,TCOUNT,PSIZE,QSIZE,LOW,HIGH,
     C WOUTP(MAXKLIQ),BASE,RANGE,PREPWI,PREQWI,PREPBT,PREQBT,
     C SUMB,PP,PQ,PREP,PREQ,WPREP,WPREQ,PKPG,QKPG,
     C BETARAT,TEMPP(MAXKLIQ),BETARAT2,BASEP,IEXPECT(MAXKLIQ),
     C ISTD(MAXKLIQ),ICON(MAXKLIQ),NOWCLOSE(MAXKLIQ),BASEWP,WPIW,WPIB

C      QSEED=5163.D0
C       NUMPEOPL=30
       OPEN(51,file='sim.betaps')
       NR=1
       DO 22 I=1,NUMPEOPL
       DO 33 J=1,NUMPEOPL
       OUTMAT(I,J)=0
00033   CONTINUE
00022    CONTINUE


       ONE=1
       DO 433 I=1,NUMPEOPL
       R(1)= GENBET(PKPG,QKPG)
       KCHOICE(I)=INT(R(1)*RANGEC+MINCHOI+.9999)
       R(1)= GENBET(PKPG,QKPG)
       KOCHOICE(I)=INT(R(1)*RANGEC+MINCHOI+.9999)

00433 CONTINUE

       D=0
       DIDEM=0

       I=0
       DO WHILE (DIDEM .EQ. 0)
       I=I+1
       COUNTG(I)=0
       R(1)=GENBET(PSIZE,QSIZE)

C       CALL GGUD(QSEED,RANGEG,ONE,IDEV)

       COUNTG(I)=INT(R(1)*RANGEG+ BASEG+.9999)
       IF (COUNTG(I) .GT. (NUMPEOPL-(D+BASEG))) THEN
       DIDEM=1
       COUNTG(I)=NUMPEOPL-D
       END IF
       DO 3 J=1,COUNTG(I)
       D=D+1
       DEPART(D)=I
      MEMBERS(I,J)=D
    3 CONTINUE
       END DO
       NUMG=I

        I=0
      BADONE=0
      WPREP=.5
      WPREQ=WPREP
      IF ((PREPWI .GT. 999) .OR. (PREPWI .LE. 0)) THEN
      PREPWI=1.4
      END IF
      IF ((PREQWI .GT. 999) .OR. (PREQWI .LE. 0)) THEN
      PREQWI=4
      END IF

      IF ((PREPBT .GT. 999) .OR. (PREPBT .LE. 0)) THEN
      PREPBT=.12
      END IF
      IF ((PREQBT .GT. 999) .OR. (PREQBT .LE. 0)) THEN
      PREQBT=1.4
      END IF

      DO 77 BI=1,NUMG
      DO 88 BG=1,NUMG
      IF (BI .EQ. BG) THEN
      PREP=PREPWI
      PREQ=PREQWI
      ELSE
      PREP=PREPBT
      PREQ=PREQBT
      END IF

      R(1)= GENBET(PREQ,PREP)
C      CALL GGUD(QSEED,RANGEP,ONE,IDEV)     
      IF (BI .EQ. BG) THEN
      BASE=BASEP
      RANGE=RANGEP
      TRATIO=BETARAT
      TRATIOW=WPIW
      ELSE
      BASE=XBASEP
      RANGE=XRANGEP
      TRATIO=BETARAT2
      TRATIOW=WPIB
      END IF
    
      OUTP(BG)=BASE+R(1)*RANGE
      IDEV(1)=IGNUIN(0,WRANGEP)
C      CALL GGUD(QSEED,WRANGEP,ONE,IDEV)     
C      CALL GENBET(WPREP,WPREQ)
      WOUTP(BG)=BASEWP+IDEV(1)
      OUTQ(BG)=OUTP(BG)/TRATIO
      WOUTQ(BG)=WOUTP(BG)/TRATIOW
00088  CONTINUE
      DO 7 TI=1,COUNTG(BI)
       I=I+1.0
       NSUBID(I)=I
      SUMB=0
      DO 8 D5=1,NUMG
C      IF (D5 .EQ. DEPART(I)) THEN
C      PP=OUTP(DEPART(I),DEPART(I))
C      PQ=
C      ELSE
C      PP=OUTP(DEPART(I),D5)
C      PQ=OUTQ(DEPART(I),D5)
C       END IF
      R(1)= GENBET(OUTP(D5),OUTQ(D5))
      TCOUNT=COUNTG(D5)
      IF (BI .EQ. D5) THEN
      TCOUNT=TCOUNT-1.0
      END IF
      IF ((R(1) .GE. 0) .AND. (R(1) .LE. 1) .AND.
     C (TCOUNT .GT. 0) .AND.
     C (NUMPEOPL .GT. 1)) THEN
      TEMPP(D5)=R(1)*TCOUNT/(NUMPEOPL-1.00)
      SUMB=SUMB+TEMPP(D5)
      ELSE
      BADONE=BADONE+1
      END IF
00008  CONTINUE
C      WRITE(51,199) I,DEPART(I)
      DO 9 D5=1,NUMG
      TEMPP(D5)=TEMPP(D5)/SUMB
      WRITE(51,198) I,DEPART(I),D5,TEMPP(D5)
      TARGETS=0
      REM=0
      IF ((TEMPP(D5) .GT. 0) .AND. (TEMPP(D5) .LE. 1)) THEN

      TARGETS=INT(TEMPP(D5)*KCHOICE(I)+.9999)
      REM=TEMPP(D5)*KCHOICE(I)-TARGETS

      END IF
      LOW=0
      HIGH=1
      R(1)=GENUNF(LOW,HIGH)
C CALL GGUBS(QSEED,NR,R)
      IF (REM .GT. R(1)) THEN
       TARGETS=TARGETS+1
      END IF 
       TN=COUNTG(D5)
         IF (D5 .EQ. DEPART(I)) THEN
         TN=TN-1
          END IF
      DO 4722 D2=1,TN
      ELEM(D2)=D2
04722  CONTINUE
       CALL SRESORTO(ELEM,TN,QSEED)
       IF (TARGETS .GT. TN) THEN
       TARGETS=TN
       END IF 
       DO 722 D2=1,TARGETS
       DMEM=ELEM(D2)
       IF ((D5 .EQ. DEPART(I)) .AND. (MEMBERS(D5,DMEM) .GE. I)) THEN
       DMEM=DMEM+1
       END IF
       IF (MAXFREQ .GT. 1) THEN
       R(1)=GENBET(WOUTP(D5),WOUTQ(D5))
       TWT=INT(MAXFREQ*R(1)+.9999)
       ELSE
       TWT=1
       END IF 
       OUTMAT(I,MEMBERS(D5,DMEM))=TWT
00722  CONTINUE
00009  CONTINUE
C       WRITE(510,200) I
00007   CONTINUE
00077    CONTINUE
      ONE=1
      ZERO=0
       CLOSE(51)
C      CALL MATPRINT(DEPART,ONE,NUMG+1,NUMPEOPL,ONE,DEPART,
C     C OUTMAT,
C     C "CHOICES BY GROUPS, SIMULATED DATA           ",
C     C NSUBID,IEXPECT,ISTD,ICON,
C     C NSUBID,ZERO,NOWCLOSE,NUMPEOPL,ONE,DEPART,COUNTG,
C     C ZERO)

00199    FORMAT(20(I10$))
00198    FORMAT(3I10,20(F10.5))
00200    FORMAT(I10) 
C      RETURN
       END
