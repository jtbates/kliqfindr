        NDFILE=INFILE(1:6)  // '.vna'
       OPEN(58,file=NDFILE,BLANK='NULL')
C      WRITE(58,'(A)') 'TR LONGDIM MEANRAD GMAXVAL IMEANVAL 
C     C NRATIO IGRATIO QMAXY QMAXX QMINY QMINX'
C      WRITE(58,'(10F10.5)') TR,LONGDIM,MEANRAD,GMAXVAL,IMEANVAL,
C     C NRATIO,IGRATIO,QMAXY,QMAXX,QMINY,QMINX

C       CALL MOVEEND(58)
      COLOR=255
      SHAPE=1
      SIZE=10
C      NUMDIM=2


C XX 

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
          
      COLORS(1)=255
      COLORS(2)=16711680
      COLORS(3)=O
      COLORS(4)=8421504
      COLORS(5)=16711935
      COLORS(6)=32768
      COLORS(7)=65280
      COLORS(8)=16776960
      COLORS(9)=65535
      COLORS(10)=8421504
      COLORS(11)=128
      COLORS(12)=8388608
      COLORS(13)=32896
      COLORS(14)=8388736
      COLORS(15)=12632256
      COLORS(16)=8421376
      COLORS(17)=16777215
      COLORS(18)=12632256
      COLORS(19)=16711680
      COLORS(20)=8384
      COLORS(21)=9291
      COLORS(22)=71225
      COLORS(23)=865789
        WANTGR=1

       WRITE(58,777) '*node data'
       WRITE(58,777) 'id type group'
        IF (WANTGR .EQ. 1) THEN
       TYPE=2
       DO 2823 I=1,MAXDEPT
       WRITE(58,'(3X,A2,A2,A1,1x,I1,1X,I3)') '"0',
     C  LABELS(I),'"',TYPE,I
        MAXRAD(I)=0
02823  CONTINUE
        END IF
       TYPE=1
       DO 4545 I=1,NUMTEACH
       WRITE(58,'(I8,1x,I1,1x,I3)') NSUBID(I),
     C TYPE,NEWDEPT(I)
04545   CONTINUE

        WRITE(58,'(A)') '"IGRATIO" 3 0'
        WRITE(58,'(A)') '"EVIDENCE" 3 0'
        WRITE(58,'(A)') '"DATA="  3 0'

       WRITE(58,220) '*Node properties'
       WRITE(58,*) 'ID x y color shape size shortlabel active'
       IF (WANTGR .EQ. 1) THEN
        DO 8182 I=1,NUMTEACH
        IF (IRADIUS(I) .GT. MAXRAD(NEWDEPT(I))) THEN
        MAXRAD(NEWDEPT(I))=IRADIUS(I)
        END IF
08182   CONTINUE

        COLOR=0
       DO 28231 I=1,MAXDEPT
       SIZE=INT(30.0*MAXRAD(I))
       WRITE(58,
     C '(4X,A2,A2,A1,1X,F10.5,1X,F10.5,1X,I3,1X,I1,1X,I3,1X,A2,1X,A4)')
     C '"0', LABELS(I),'"',GCOORD(I,2),
     C GCOORD(I,1),COLOR,SHAPE,SIZE,LABELS(I),'TRUE'
28231  CONTINUE
      END IF
       SIZE=10
       ONE=1
        DO 4646 I=1,NUMTEACH
        TCOLOR=NEWDEPT(I)
        IF (TCOLOR .LE. 0) THEN TCOLOR=0
       WRITE(58,
     C '(I8,1X,F10.5,1X,F10.5,1X,I8,1X,I1,1X,I3,1X,I8,1X,A4)')
     C NSUBID(I),ICOORD(I,2),ICOORD(I,1),
     C COLORS(TCOLOR),SHAPE,SIZE,NSUBID(I),'TRUE'
04646   CONTINUE
        WRITE(58,'(A,1X,F10.5,1X,F10.5,1X,I3,1X,I1,1X,I2,A,F7.2,A)')
     C  '"IGRATIO"',1.5*QMAXX,1.3*QMAXY,COLOR,8,SIZE,
     C  ' "Distances within vs between groups:',NRATIO,'" TRUE'
        
C  ZZZZ
        WRITE(58,'(A,1X,F10.5,1X,F10.5,1X,I3,1X,I1,1X,I2,A,F7.2,A)')
     C  '"EVIDENCE"',1.5*QMAXX,1.2*QMAXY,COLOR,8,SIZE
     C  ,' "Clusters p-val ',PERMPVAL,'" TRUE'
             WRITE(58,'(A,1X,F10.5,1X,F10.5,1X,I3,1X,I1,1X,I2,A,A6,A)')
     C  '"DATA="',0*QMAXX,1.1*QminY,COLOR,8,SIZE
     C  ,' "DATA=',infile,'" TRUE'


       ONE=1
       ZERO=0
       FRIENDS=1
       WRITE(58,*) '*Tie data'
       WRITE(58,803) 'from to friends strength vanish'
        IF (WANTGR .EQ. 1) THEN
       DO 5155 I=1,MAXDEPT
       DO 5755 J=1,MAXDEPT
       IF (I .NE. J) THEN
       TLSIZE=1
       TLSIZE=INT(20.0*COVMAT3(I,J)/HIWT)

       WRITE(58,'(4X,A2,A2,A4,A2,A1,1X,I1,1X,I3,1X,I1)') 
     C '"0',LABELS(I),'" "0',LABELS(J),'"',ONE,TLSIZE,ZERO
        END IF
05755 CONTINUE
05155  CONTINUE
        END IF

       DO 25228 I=1,NUMTEACH
       DO 25329 J=1,NUMTEACH
       IF ((J .NE. I) .AND. (NEWMAT(I,J) .GT. 0)) THEN
       WRITE(58,'(I8,1X,I8,1X,I1,1X,I1,1X,I1)') 
     C NSUBID(I),NSUBID(J),FRIENDS,NEWMAT(I,J),ONE
       END IF
25329 CONTINUE
25228 CONTINUE
        WRITE(58,'(A)')'"IGRATIO" "EVIDENCE" 1 1 1'
 
         WRITE(58,'(A)')'"IGRATIO" "DATA=" 1 1 1'
       WRITE(58,*) '*Tie properties'
      WRITE(58,*) 'FROM TO color size headcolor headsize active'
      IF (WANTGR .EQ. 1) THEN
      COLOR=16777215
      DO 7155 I=1,MAXDEPT
        DO 7235 J=1,MAXDEPT
         IF (I .NE. J) THEN
         TLSIZE=1
         TLSIZE=INT(20.0*COVMAT3(I,J)/HIWT)
             WRITE(58,'(A2,A2,A4,A2,A1,1X,I8,1X,I3,1X,I8,1X,A7)') 
     C '"0',LABELS(I),'" "0',LABELS(J),'"',COLOR,TLSIZE,
     C  COLOR,' 0 TRUE'
        END IF
      
07235 CONTINUE
07155 CONTINUE
       END IF
       
      DO 27228 I=1,NUMTEACH
       DO 27329 J=1,NUMTEACH
       IF ((J .NE. I) .AND. (NEWMAT(I,J) .GT. 0)) THEN
       TLSIZE=1
       IF (HIWT .GT. 1) THEN 
       TLSIZE=INT(4*NEWMAT(I,J)/HIWT)
       END IF
       COLOR=0
       LTYPE=0
       IF (NEWDEPT(I) .NE. NEWDEPT(J)) THEN
       COLOR=12632256
       LTYPE=1
       END IF
       WRITE(58,'(I3,1X,I3,1X,I8,1X,I2,1X,A8)') 
     C NSUBID(I),NSUBID(J),COLOR,TLSIZE,'0 8 TRUE'
       END IF
27329 CONTINUE
27228 CONTINUE
 
       WRITE(58,'(A)')'"IGRATIO" "EVIDENCE" 16777215 1 0 8 TRUE'
       WRITE(58,'(A)')'"IGRATIO" "DATA=" 16777215 1 0 8 TRUE'


       CLOSE(58)

