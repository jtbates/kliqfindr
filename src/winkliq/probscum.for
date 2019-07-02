
       SUBROUTINE PROBS (MAXGROUP,NUMTEACH,DEPARTN,PAGB,
     C NEWDEPT,RCHOICE,WHOLEGRP,INPER,INGROUP2,MBI)
      INCLUDE 'PARAM.H'

       INTEGER I,J,PERS,GROUP,IER,MBI,IER2,MAXDEPT

       INTEGER  MAXGROUP,NUMTEACH,DEPARTN(MAXKLIQ),
     C  RCHOICE(MAXKLIQ),NEWDEPT(MAXKLIQ),THISN,
     C  WHOLEGRP,BEGPER,ENDPER,BEGGRP,ENDGRP,INGROUP2,INPER,
     C NEWNUM,TARGET,OUTCON(MAXKLIQ),G,NORMALZ,
     C SUCCESS,TRIALS,MN,MX,A,Q
       REAL ZCOMPMAT(MAXKLIQ,MAXKLIQ)
       INTEGER HUBERT(MAXKLIQ,MAXKLIQ),RESULTM(MAXKLIQ,MAXKLIQ),
     C ALLGROUP(MAXKLIQ,MAXKLIQ),NEWMAT(MAXKLIQ,MAXKLIQ)

C       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT
       REAL BLAUC(MAXKLIQ,MAXKLIQ),DIFF(MAXKLIQ,MAXKLIQ)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF


       REAL*4 PAGB(MAXKLIQ,200),TOTNUM(MAXKLIQ)

       REAL PK,PS,KTHETA,NUMCHOIC,INGROUP,OSLOTS,RTHISN,
     C GCONN(MAXKLIQ),TCONN,TSLOTS,TCONN2,GCONN2(MAXKLIQ),
     C KTHETA2,TTHETA,PK2,PS2,INCM,
     C TOTP(MAXKLIQ),KTHETA3,THETA1(MAXKLIQ),THETA2(MAXKLIQ),
     C G1,G2,ACHOICE(MAXKLIQ,MAXKLIQ),TCHOICE(MAXKLIQ)

       DOUBLE PRECISION PEQK,PLEK,PEQK2,PLEK2
      MAXDEPT=MAXGROUP
      MBI=MBI+1
      INGROUP=0.000
      NUMCHOIC=0.000
      DO 4 I=1,NUMTEACH
      TCHOICE(I)=0
      TOTP(I)=0
      DO 5 J=1,MAXGROUP
       ACHOICE(I,J)=0
00005  CONTINUE
00004   CONTINUE

       OSLOTS=0.000
      DO 11 J=1,MAXGROUP
      GCONN(J)=0.000
      GCONN2(J)=0.000
00011 CONTINUE

 
      DO 2 I =1, NUMTEACH
       DO 3 J=1, NUMTEACH
        IF (NEWMAT(I,J) .GE. 1) THEN 
          NUMCHOIC=NUMCHOIC+NEWMAT(I,J)
          GCONN2(NEWDEPT(I))=GCONN2(NEWDEPT(I))+NEWMAT(I,J)
          GCONN2(NEWDEPT(J))=GCONN2(NEWDEPT(J))+NEWMAT(I,J)

          TCHOICE(I)=TCHOICE(I)+NEWMAT(I,J)
          TCHOICE(J)=TCHOICE(J)+NEWMAT(I,J)
          ACHOICE(I,NEWDEPT(J))=ACHOICE(I,NEWDEPT(J))+NEWMAT(I,J)
          ACHOICE(J,NEWDEPT(I))=ACHOICE(J,NEWDEPT(I))+NEWMAT(I,J)
          IF  (NEWDEPT(I) .EQ. NEWDEPT(J)) THEN
           INGROUP=INGROUP+1
          GCONN(NEWDEPT(I))=GCONN(NEWDEPT(I))+NEWMAT(I,J)
          END IF
        END IF
00003   CONTINUE
00002  CONTINUE
C          INCM=INGROUP*(NUMTEACH-1)*NUMTEACH/(NUMCHOIC*OSLOTS)


        DO 08 J=1,MAXDEPT
        THETA1(J)=2*GCONN(J)/GCONN2(J)
        THETA2(J)=(GCONN2(J)-2*GCONN(J))/(NUMCHOIC-GCONN(J))
00008    CONTINUE
        DO 09 I=1,NUMTEACH
        DO 10 G=1,MAXDEPT
        IF ((DEPARTN(G) .LT. 1) .OR. 
     C  ((DEPARTN(G) .EQ. 1) .AND. (NEWDEPT(I) .EQ. G))) THEN
        PAGB(I,G)=0.000
        ELSE
C        G1=(THETA1(G)**ACHOICE(I,G)) *  
C     C  (1-THETA1(G))**(TCHOICE(I)-ACHOICE(I,G))
C
C        G2=(THETA2(G)**ACHOICE(I,G)) *  
C     C  (1-THETA2(G))**(TCHOICE(I)-ACHOICE(I,G))
         X=ACHOICE(I,G)
         A=TCHOICE(I)
         B=THETA1(G)
         
                 if ( x .lt. 0 ) then

          cdf = 0.0E+00

        else if ( x .ge. a ) then

          cdf = 1.0E+00      

        else if ( b .eq. 0.0E+00 ) then

          cdf = 1.0E+00

        else if ( b .eq. 1.0E+00 ) then

          cdf = 0.0E+00

        else

          cdf = 0.0E+00

          do j = 0, x
          
          mn = min ( j, a-j )

        if ( mn .lt. 0 ) then

          cnk = 0

        else if ( mn .eq. 0 ) then

          cnk = 1

        else

          mx = max ( j, a-j )
          cnk = mx + 1

          do Q = 2, mn
            cnk = ( cnk * ( mx + Q ) ) / Q
          end do

        end if

            pr = real ( cnk ) * b**j * ( 1.0E+00 - b )**( a - j )

            cdf = cdf + pr

          end do

        end if

         G1=CDF

         B=THETA2(G)
        if ( x .lt. 0 ) then

          cdf = 0.0E+00

        else if ( x .ge. a ) then

          cdf = 1.0E+00      

        else if ( b .eq. 0.0E+00 ) then

          cdf = 1.0E+00

        else if ( b .eq. 1.0E+00 ) then

          cdf = 0.0E+00

        else

          cdf = 0.0E+00

          do j = 0, x
          
          mn = min ( j, a-j )

        if ( mn .lt. 0 ) then

          cnk = 0

        else if ( mn .eq. 0 ) then

          cnk = 1

        else

          mx = max ( j, a-j )
          cnk = mx + 1

          do Q = 2, mn
            cnk = ( cnk * ( mx + Q ) ) / Q
          end do

        end if

            pr = real ( cnk ) * b**j * ( 1.0E+00 - b )**( a - j )

            cdf = cdf + pr

          end do

        end if

         G2=CDF
C         CALL BINOMIAL(SUCCESS,TRIALS,THETA1(G),G1)
C         CALL BINOMIAL(SUCCESS,TRIALS,THETA2(G),G2)
         PAGB(I,G)=(DEPARTN(G)*G1)/
     C  (DEPARTN(G)*G1+(NUMTEACH-DEPARTN(G))*G2)
        IF ((PAGB(I,G) .GE. 0 ) .AND. (PAGB(I,G) .LT. 999999)) THEN
        HAPPY=1
        ELSE
        PAGB(I,G)=0.000
        END IF
C        NORMALZ=1
C        TOTP(I)=1
C        IF (NORMALZ .EQ. 1) THEN 
        TOTP(I)=TOTP(I)+PAGB(I,G)
C        END IF
        END IF
00010    CONTINUE
00009     CONTINUE

         DO 13 I=1,NUMTEACH
         DO 12 J=1,MAXDEPT
          
          PAGB(I,J)=PAGB(I,J)/TOTP(I)
00012      CONTINUE
00013       CONTINUE
         RETURN
         END
