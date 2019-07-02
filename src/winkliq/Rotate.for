
      SUBROUTINE ROTATE(OUTCOORD,GCOORD,NUMTEACH,MAXG,ANGLE,
     C IRADIUS,
     C NINCREM,
     C MEASURE,DEPARTN,NRATIO,TEXTREME,HIWT,ORADIUS)
      INCLUDE 'PARAM.H'

       INTEGER NUMTEACH,MAXG,THISPER,I,J,K,L,NINCREM,MEASURE,
     C DEPARTN(MAXKLIQ),EXTREME,BT,NOTDONE,TEXTREME

       REAL IRADIUS(MAXKLIQ),GCOORD(MAXKLIQ,2),GMAT(MAXGR,MAXGR),
     C DIFFX,DIFFY,ADIFF,BESTDIFF,BESTANGL,ANGLE(MAXKLIQ),
     C ORADIUS(MAXKLIQ),TDIFF,TANGLE,KANGLE,KINCREM,NRATIO,
     C HIWT,OUTCOORD(MAXKLIQ,2),TCOORD1,TCOORD2

       INTEGER HUBERT(MAXKLIQ,MAXKLIQ),RESULTM(MAXKLIQ,MAXKLIQ),
     C ALLGROUP(MAXKLIQ,MAXKLIQ),NEWMAT(MAXKLIQ,MAXKLIQ)

       REAL BLAUC(MAXKLIQ,MAXKLIQ),DIFF(MAXKLIQ,MAXKLIQ),
     C ZCOMPMAT(MAXKLIQ,MAXKLIQ)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF

       REFLECT=2
       EXTREME=TEXTREME
       IF (TEXTREME .LT. 0) THEN
       REFLECT=1
       EXTREME=-TEXTREME
       END IF  


      DO 2 I=1,MAXG
      DO 3 J=1,MAXG
        IF (J .NE. I) THEN
      DIFFY=GCOORD(I,2)-GCOORD(J,2)
      DIFFX=GCOORD(I,1)-GCOORD(J,1)
      GMAT(J,I)=ATAN2(DIFFY,DIFFX)
      IF (GMAT(J,I) .LT. 0) THEN
      GMAT(J,I)=GMAT(J,I)+6.28318
      END IF
      IF (GMAT(J,I) .GT. 6.28318) THEN
      GMAT(J,I)=GMAT(J,I)-6.28318
      END IF
         END IF
00003  CONTINUE
00002   CONTINUE

      PI=3.1415926 
      DO 6 J=1,MAXG
      IF (DEPARTN(J) .GT. 1) THEN
      BESTANGL=0
      BESTDIFF=9999
      KINCREM=2*PI/NINCREM
      NOTDONE=0
      
      DO WHILE (NOTDONE .LT. REFLECT)
      NOTDONE=NOTDONE+1

      DO 7 I=1,NINCREM
      ADIFF=0
      KANGLE=(I-1)*KINCREM

      DO 8 K=1,DEPARTN(J)
      THISPER=ALLGROUP(J,K)
      IF (IRADIUS(THISPER) .GT. 0) THEN

      TANGLE=ANGLE(THISPER)

      IF (NOTDONE .EQ. 2) THEN
      TANGLE=PI-ANGLE(THISPER)
      END IF 

       TANGLE=TANGLE+KANGLE

      IF (TANGLE .GT. 6.28318) THEN
      TANGLE=TANGLE-6.28318
      END IF

      IF (TANGLE .LT. 0) THEN
      TANGLE=2*PI+TANGLE
       END IF


      DO 9 L=1,MAXG
      IF (L .NE. J) THEN
      IF (MEASURE .EQ. 1 ) THEN
      TDIFF=DIFF(THISPER,L)
      END IF
       IF (MEASURE .EQ. 2) THEN
      TDIFF=BLAUC(THISPER,L)
       END IF
      IF (EXTREME .EQ. 1) THEN
      TDIFF=TDIFF*IRADIUS(THISPER)/HIWT
      END IF
      ADIFF=ADIFF+ABS(TANGLE-GMAT(J,L))*TDIFF
       END IF
00009  CONTINUE
       END IF

00008   CONTINUE
       IF (ADIFF .LT. BESTDIFF) THEN
       BESTANGL=KANGLE
       YESDONE=NOTDONE
       BESTDIFF=ADIFF
       END IF

00007   CONTINUE
         END DO

        END IF

       IF (DEPARTN(J) .GE. 1) THEN
       DO 11 K=1,DEPARTN(J)
       THISPER=ALLGROUP(J,K)
       TANGLE=ANGLE(THISPER)

       IF (YESDONE .EQ. 2) THEN
       TANGLE=PI-ANGLE(THISPER)
       END IF 

       ANGLE(THISPER)=TANGLE+BESTANGL
      
      IF (ANGLE(THISPER) .LT. 0) THEN
      ANGLE(THISPER)=ANGLE(THISPER)+6.28318
      END IF

      IF (ANGLE(THISPER) .GT. 6.28318) THEN
      ANGLE(THISPER)=ANGLE(THISPER)-6.28318
      END IF

       ORADIUS(THISPER)=IRADIUS(THISPER)*NRATIO
       OUTCOORD(THISPER,1)=GCOORD(J,1)+ORADIUS(THISPER)*
     C COS(ANGLE(THISPER))
       OUTCOORD(THISPER,2)=GCOORD(J,2)+ORADIUS(THISPER)*
     C SIN(ANGLE(THISPER))
00011   CONTINUE
       END IF

00006    CONTINUE




       RETURN
       END 