
C IN GETRS NOTE THS SIDEWT

      SUBROUTINE MATPRINT(EXTRAID,DIVIDO,MAXDEPT,
     CNUMTEACH,DOIT,AFFIL,MATRIX,TITLE,NSUBID,IEXPECT,ISTD,ICON,
     CNEWORD,STRUCTEQ,NOWCLOSE,NUMCOL,HOWWIDE,CAFFIL,DEPARTN,WEXTRA)
      INCLUDE 'PARAM.H'
C                                   PRINTO,NSUBID

      INTEGER NUMTEACH, DOIT,AFFIL(MAXKLIQ),MATRIX(MAXKLIQ,MAXKLIQ),P,
     C K,I,DIVIDO,MAXDEPT,OLDAFFIL,OAFFIL2,EXTRAID(MAXKLIQ),ROWMARG,
     C COLMARG(MAXKLIQ),THISVAL,PI,TOTMARG,TVAL,NSUBID(MAXKLIQ),
     C NEWORD(MAXKLIQ),STRUCTEQ,NUMCOL,HOWWIDE,HW,CAFFIL(MAXKLIQ),
     C DEPARTN(MAXKLIQ),DIGIT(3), DECI,ZZ,EXTRA,WEXTRA,GTP,TP
   
       REAL IEXPECT(MAXKLIQ),ISTD(MAXKLIQ),ICON(MAXKLIQ),
     C NOWCLOSE(MAXKLIQ)
       CHARACTER TITLE*40
       CHARACTER LABELS(300)*2
      DO 300 I=1,300
      LABELS(I)='**'
00300 CONTINUE

      LABELS(1)='A'
      LABELS(2)='B'
      LABELS(3)='C'
      LABELS(4)='D'
      LABELS(5)='E'
      LABELS(6)='F'
      LABELS(7)='G'
      LABELS(8)='H'
      LABELS(9)='I'
      LABELS(10)='J'
      LABELS(11)='K'
      LABELS(12)='L'
      LABELS(13)='M'
      LABELS(14)='N'
      LABELS(15)='O'
      LABELS(16)='P'
      LABELS(17)='Q'
      LABELS(18)='R'
      LABELS(19)='S'
      LABELS(20)='T'
      LABELS(21)='U'
      LABELS(22)='V'
      LABELS(23)='W'
      LABELS(24)='X'
      LABELS(25)='Y'
      LABELS(26)='Z'
      LABELS(27)='a'
      LABELS(28)='b'
      LABELS(29)='c'
      LABELS(30)='d'
      LABELS(31)='e'
      LABELS(32)='f'
      LABELS(33)='g'
      LABELS(34)='h'
      LABELS(35)='i'
      LABELS(36)='j'
      LABELS(37)='k'
      LABELS(38)='l'
      LABELS(39)='m'
      LABELS(40)='n'
      LABELS(41)='o'
      LABELS(42)='p'
      LABELS(43)='q'
      LABELS(44)='r'
      LABELS(45)='s'
      LABELS(46)='t'
      LABELS(47)='u'
      LABELS(48)='v'
      LABELS(49)='w'
      LABELS(50)='x'
      LABELS(51)='y'
      LABELS(52)='z'
      LABELS(53)='AA'
      LABELS(54)='BB'
      LABELS(55)='CC'
      LABELS(56)='DD'
      LABELS(57)='EE'
      LABELS(58)='FF'
      LABELS(59)='GG'
      LABELS(60)='HH'


C      IF (HOWWIDE .EQ. 1) THEN
C       PARAMETER (IWIDTH=1)
C      END IF

C      IF (HOWWIDE .EQ. 2) THEN
C       PARAMETER (IWIDTH=2)
C      END IF

C      IF (HOWWIDE .EQ. 3) THEN
C       PARAMETER (IWIDTH=3)
C      END IF

C      IF (HOWWIDE .EQ. 4) THEN
C       PARAMETER (IWIDTH=4)
C      END IF


      IF (DOIT .EQ. 1) THEN
      IF (DIVIDO .EQ. 0) THEN
      WRITE(33,101) 
      WRITE(33,101) 
      WRITE(33,1111) TITLE
      WRITE(33,101) 
      WRITE(33,1111) 'Each Cell Represents The Number of'
      WRITE(33,1111) ' Transactions (Also Known as Connections or'
      WRITE(33,101)
      WRITE(33,1111) ' Exchanges or Ties) '
      WRITE(33,1111) 'Between Actor i (Row) and Actor j (Column)'
      WRITE(33,1111)
      WRITE(33,101) 99999,99999 , (MOD(AFFIL(P),10) ,
     C  P=1,NUMCOL)
      DO 188 I=1,NUMTEACH
      WRITE(33,101) AFFIL(I),99999,(MATRIX(I,K) , K=1,NUMCOL),
     C 99999,MOD(AFFIL(I),10)
  188 CONTINUE
      WRITE(33,101) 99999,99999 , (MOD(AFFIL(P),10) ,
     C  P=1,NUMCOL)
      ELSE
      WRITE(33,101) 
      WRITE(33,1111) TITLE
      WRITE(33,101) 
      WRITE(33,1111) 'Each Cell Represents The Number of'
      WRITE(33,1111) ' Transactions (Also Known as Connections or'
      WRITE(33,101)
      WRITE(33,1111) ' Exchanges or Ties) '
      WRITE(33,1111) 'Between Actor i (Row) and Actor j (Column)'
      WRITE(33,1111)
      WRITE(33,101)
      WRITE(33,7513) 'N',' Group And Actor Id'
        DO 4420 TP=1,(HOWWIDE*NUMCOL+MAXDEPT-20)
       WRITE(33,108) ' '
04420   CONTINUE
        IF (WEXTRA .EQ. 1) THEN  
        WRITE(33,3512) 'Actor Marginals','           ',
     C 'Relation To Group'
         ELSE
         WRITE(33,101)
         END IF
       DO 987 DECI=1,4
      IF (DECI .EQ. 2) THEN
      WRITE(33,1067) '   ','   '
      END IF
      
      IF (DECI .EQ. 1) THEN
      WRITE(33,1065) NUMTEACH,'       '
      END IF
      IF (DECI .EQ. 3) THEN
      WRITE(33,1067) '   ','   '
      END IF
      IF (DECI .EQ. 4) THEN
      WRITE(33,1067) 'Group','ID'
      END IF
      WRITE(33,108) '|'

      OLDAFFIL=CAFFIL(1)
      DO 17 P = 1,NUMCOL
       IF (CAFFIL(P) .NE. OLDAFFIL) THEN
       WRITE(33,108) '|'
       END IF
C      NSUBID(EXTRAID(I))
c       Here
       IF (DECI .GT. 1) THEN
       DIGIT(1)=INT(NSUBID(EXTRAID(P))/100.)
       DIGIT(2)=INT((NSUBID(EXTRAID(P))-DIGIT(1)*100.)/10.)
       DIGIT(3)=INT(NSUBID(EXTRAID(P))-(DIGIT(1)*100. + DIGIT(2)*10.))

       TVAL=DIGIT(DECI-1)

       
*       IF (CAFFIL(P) .LE. MAXKLIQ) THEN
*       TVAL=MOD(CAFFIL(P),10)
*       ELSE
*       TVAL=9999
*       END IF
       IF (NSUBID(EXTRAID(P)) .GE. (10.**(4.-DECI))) THEN
       WRITE(33,104) TVAL 
       ELSE
       WRITE(33,1087) ' '
       END IF
        ELSE
C        DECI GT 1
       DO 77 ZZ=1,(HOWWIDE-1)
       WRITE(33,108) ' '
00077   CONTINUE
        IF (CAFFIL(P) .LE. 100.00) THEN
        WRITE(33,108) LABELS(CAFFIL(P))
        END IF
        END IF
        IF (CAFFIL(P) .LE. 100.00) THEN
        OLDAFFIL=CAFFIL(P)
        END IF
00017   CONTINUE
      WRITE(33,108) '|'
         EXTRA=12
        IF (WEXTRA .EQ. 1) THEN
        EXTRA=69
        IF (DECI .EQ. 3) THEN
        WRITE(33,108) ' '
        WRITE(33,111) 'GRP','ROW','COL','TOT',
     C  'EXP','STD','CON','Z','Z/N','DELTA' 
        ELSE
        WRITE(33,513)
        END IF
           ELSE
        WRITE(33,107)
        END IF
00987    CONTINUE
    
C        HOWWIDE*NUMCOL+MAXDEPT+EXTRA)
        DO 420 TP=1,12
       WRITE(33,5108) '-'
00420   CONTINUE
       WRITE(33,5108) '+'
       DO 6420 TP=1,MAXDEPT
       IF (DEPARTN(TP) .GT. 0) THEN
       DO 7420 GTP=1,(DEPARTN(TP)*HOWWIDE)
       WRITE(33,5108) '-'
07420   CONTINUE
       WRITE(33,5108) '+'
         END IF
06420   CONTINUE
       DO 8420 TP=1,(EXTRA-12)
       WRITE(33,5108) '-'
08420   CONTINUE
     
        WRITE(33,513)

       OAFFIL2=AFFIL(1)

      DO 19 PI=1,NUMTEACH
       IF (AFFIL(PI) .NE. OAFFIL2) THEN
        DO 10420 TP=1,12
       WRITE(33,5108) '-'
10420   CONTINUE
       WRITE(33,5108) '+'
       DO 16420 TP=1,MAXDEPT
       IF (DEPARTN(TP) .GT. 0) THEN
       DO 17420 GTP=1,(DEPARTN(TP)*HOWWIDE)
       WRITE(33,5108) '-'
17420   CONTINUE
       WRITE(33,5108) '+'
        END IF
16420   CONTINUE
       DO 18420 TP=1,(EXTRA-12)
       WRITE(33,5108) '-'
18420   CONTINUE
     
        WRITE(33,513)

C        DO 20 TP=1,(HOWWIDE*NUMCOL+MAXDEPT+EXTRA)
C       WRITE(33,108) '_'
C00020   CONTINUE
C       WRITE(33,101)

       END IF
       IF (AFFIL(PI) .LE. 300.00) THEN
       WRITE(33,106) AFFIL(PI),LABELS(AFFIL(PI)),NSUBID(EXTRAID(PI))
       END IF
       WRITE(33,108) '|'
       OAFFIL2=AFFIL(PI)      
        OLDAFFIL=CAFFIL(1)
       ROWMARG=0
       COLMARG(PI)=0
      DO 18 P = 1,NUMCOL
       IF (CAFFIL(P) .NE. OLDAFFIL) THEN
       WRITE(33,108) '|'
       END IF
       IF ((PI .EQ. P) .AND. (STRUCTEQ .NE. 1) .AND. 
     C (AFFIL(PI) .LE. 300.00))  THEN 
       WRITE(33,1087) LABELS(AFFIL(PI))
       THISVAL=-9999
       ELSE
       THISVAL= INT(MATRIX(PI,P))
       ROWMARG=ROWMARG+MATRIX(PI,P)
       COLMARG(PI)=COLMARG(PI)+MATRIX(P,PI)
       END IF
       IF (THISVAL .GT. 0) THEN
       WRITE(33,104) THISVAL
       END IF
       IF (THISVAL .EQ. 0) THEN
       WRITE(33,1087) '.'
       END IF
        OLDAFFIL=CAFFIL(P)
00018   CONTINUE

       TOTMARG=ROWMARG+COLMARG(PI)
       WRITE(33,108) '|'
        IF (WEXTRA .EQ. 1) THEN
       WRITE(33,107) AFFIL(PI),ROWMARG,COLMARG(PI),TOTMARG,99999,
     C IEXPECT(NEWORD(PI)),ISTD(NEWORD(PI)),ICON(NEWORD(PI)),
     C ((ICON(NEWORD(PI)) - IEXPECT(NEWORD(PI)))/ISTD(NEWORD(PI))),
     C ((ICON(NEWORD(PI)) - IEXPECT(NEWORD(PI)))/ISTD(NEWORD(PI)))/
     C DEPARTN(AFFIL(NEWORD(PI))),
     C NOWCLOSE(NEWORD(PI))
           ELSE
        WRITE(33,107)
        END IF
00019   CONTINUE
      END IF      
      END IF

      RETURN
07513  FORMAT(1X,A1,10X,A20,$)
C 07513  FORMAT(1X,A1,10X,A20)
  513  FORMAT(A3)
  512 FORMAT(3(A20,1X))
 3512 FORMAT(3(A))
C  101 FORMAT(I3,251(I1))
  101 FORMAT(I3,251(I1))
C   109 FORMAT(I4,1X)
  109 FORMAT(I4,1X,$)
  102 FORMAT(2I3,$)
C   102 FORMAT(2I3)
01067 FORMAT(A6,1X,A5,$)
C 01067 FORMAT(A6,1X,A5)
C  106 FORMAT(I3,1X,A2,1X,I5)
  106 FORMAT(I3,1X,A2,1X,I5,$)
01066  FORMAT(A7,$)
C  01066  FORMAT(A7)
01065 FORMAT(I3,A9,$)
C 01065 FORMAT(I3,A9)
C   104 FORMAT(I1)
  104 FORMAT(I1,$)
C  108 FORMAT(A1)
  108 FORMAT(A1,$)
05108 FORMAT(A1,$)
C 05108 FORMAT(A1)
C 01087 FORMAT(A1)
01087 FORMAT(A1,$)
  110 FORMAT(A6,$)
C  110 FORMAT(A6)
  103 FORMAT(2I3)
01111 FORMAT(A,$)
C 01111 FORMAT(A)
  107 FORMAT(1X,5(I3,1X),3X,6(F5.1,1X))
  111 FORMAT(4(A3,1X),7X,7(A5,1X))
      END
