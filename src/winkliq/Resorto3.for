C     END SUBROUTINE RESORTO

C      CALL RESORTO3(OLDDEPT,NUMTEACH,QSEED,USEED,UCUT,NEWDEPT)

      SUBROUTINE RESORTO3 (ELEMENTS,NUMELEM,RSEED,USEED,UCUT,
     C  NELEMENT,TMAXDEPT,NEWGRPS)
      INCLUDE 'PARAM.H'

      INTEGER ELEMENTS(MAXKLIQ),NUMELEM,FAKETCH,NELEMENT(MAXKLIQ)
      INTEGER OELEMENT(MAXKLIQ),Z,J,K,I,CONVERT(MAXKLIQ),RITER,LMAXDEPT,
     C LGROUP(MAXKLIQ),DR(1),DR2(2),NEWGRPS,TMAXDEPT
      DOUBLE PRECISION RSEED,USEED
      REAL UCUT,COMPVAL(MAXKLIQ),LOW,HIGH
      INTEGER RANDARRY(MAXKLIQ),OLDPOS(MAXKLIQ),TRUENUM,
     C CHANGEIT(MAXKLIQ),SUB,NGROUP


C       IF (NEWGRPS .EQ. 0) THEN
C       LMAXDEPT=TMAXDEPT-1
C       ELSE
        LMAXDEPT=TMAXDEPT
C        END IF

C
C   NOW THE RANDARRY IS AN ARRAY OF RANDOM NUMBERS OF LENGTH NUMELEM
C


C        CALL REMOVEO2(LMAXDEPT,NELEMENT,LGROUP,1)

C      TRUENUM=1
C     DO 3 K=1,NUMELEM
C
C      NELEMENT(TRUENUM)=ELEMENTS(TRUENUM)
C      TRUENUM=TRUENUM+1
C       END IF       
C    3 CONTINUE
C       TRUENUM=TRUENUM-1
C      FAKETCH=NUMELEM
       LOW=0
       HIGH=1

      DO 00001 SUB=1,NUMELEM
      NELEMENT(SUB)=ELEMENTS(SUB) 
      IF (ELEMENTS(SUB) .GE. 1) THEN
        CHANGEIT(SUB)=0
        COMPVAL(SUB) = GENUNF(LOW,HIGH)
        IF (COMPVAL(SUB) .GT. UCUT) THEN

C          CALL GGUD(USEED,LMAXDEPT,1,DR)
          DR(1)=IGNUIN(1,LMAXDEPT)
          NGROUP=DR(1)

          IF ((NGROUP .EQ. LMAXDEPT) .AND. (NEWGRPS .EQ. 1)) THEN
          LMAXDEPT=LMAXDEPT+1
          NELEMENT(SUB)=LMAXDEPT
          CHANGEIT(SUB)=1

          DR2(1)=IGNUIN(1,NUMELEM)
          DR2(2)=IGNUIN(1,NUMELEM)

C          CALL GGUD(USEED,NUMELEM,2,DR2)           
          NELEMENT(DR2(1))=LMAXDEPT
          NELEMENT(DR2(2))=LMAXDEPT

          ELSE

           IF ((NGROUP .GE. NELEMENT(SUB)) .AND. (NEWGRPS .EQ. 1)) 
     C       THEN
              NGROUP=NGROUP + 1
            END IF 
          NELEMENT(SUB)=NGROUP
          CHANGEIT(SUB)=1
          END IF
      END IF
      END IF
00001  CONTINUE


C      CALL GGPER(RSEED,TRUENUM,RANDARRY)
C
C      DO 4 Z=1,TRUENUM
C      IF (CHANGEIT(Z) .EQ. 1) THEN
C      NELEMENT(Z)=OELEMENT(RANDARRY(Z))
C      END IF
C    4 CONTINUE



  219 FORMAT(I3,1X,I5,1X,I5,1X,I3)
      RETURN
      END
