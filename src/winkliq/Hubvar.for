C      END SUBROUTINE REMOVEO4
C



C      CALL HUBVAR(HUBERT,NEWMAT,NUMTEACH,ACTVAL,STDVAL)
        SUBROUTINE HUBVAR(PMAT,QMAT,TMATSIZE,ACTVAL,STDVAL,ONECOUNT,
     C HUBERTM,HUBERTSD,TOTCOUNT)
      INCLUDE 'PARAM.H'
       INTEGER PMAT(MAXKLIQ,MAXKLIQ),QMAT(MAXKLIQ,MAXKLIQ),TMATSIZE,I,
     C J,T,TOTCOUNT
       REAL SUMPJI(MAXKLIQ), SUMQJI(MAXKLIQ), SUMPIJ(MAXKLIQ),
     C  SUMQIJ(MAXKLIQ),SUMPIJ2,SUMQIJ2,SUMPJ(MAXKLIQ),SUMQJ(MAXKLIQ),
     C  SUMPIJJI(MAXKLIQ),SUMQIJJI(MAXKLIQ),B62(MAXKLIQ),B61(MAXKLIQ),
     C  B6PART1,B6PART2,B5PART1,B5PART2,B3PART1,B3PART2,D3PART1,
     C  D3PART2,HUBERTM,HUBERTV,HUBERTSD,TOTQIJ,TOTPIJ,MATSIZE,
     C  ACTVAL,STDVAL,ONECOUNT
       MATSIZE=TMATSIZE
        B6PART1=0
        B6PART2=0
        B5PART1=0
        B5PART2=0
        B3PART1=0
        B3PART2=0
       TOTPIJ=0
       TOTQIJ=0
       D3PART1=0
       D3PART2=0
       SUMQIJ2=0
       SUMPIJ2=0
       ACTVAL=0.000
       DO 2 T=1,MATSIZE
       SUMPIJJI(T)=0
       SUMQIJJI(T)=0
        SUMPJI(T)=0
        SUMQJI(T)=0
        SUMPIJ(T)=0
        SUMQIJ(T)=0
        SUMPJ(T)=0
        SUMQJ(T)=0
       B62(T)=0
       B61(T)=0
    2 CONTINUE
       ONECOUNT=0
       TOTCOUNT=0
       DO 5 I=1,MATSIZE
        DO 10 J=1,MATSIZE
         ACTVAL=ACTVAL+PMAT(I,J)*QMAT(I,J)
         SUMPJI(I)=SUMPJI(I) + PMAT(J,I)
         SUMQJI(I)=SUMQJI(I) + QMAT(J,I)
          IF (PMAT(I,J) .GT. 0) THEN
          ONECOUNT=ONECOUNT+1.
          TOTCOUNT=TOTCOUNT+PMAT(I,J)
          END IF
         SUMPIJ(I)=SUMPIJ(I) + PMAT(I,J)
         SUMQIJ(I)=SUMQIJ(I) + QMAT(I,J)
         SUMPIJ2=SUMPIJ2 + PMAT(I,J)**2
         SUMQIJ2=SUMQIJ2 + QMAT(I,J)**2
         SUMPJ(I)=SUMPJ(I) + PMAT(I,J)
         SUMQJ(I)=SUMQJ(I) + QMAT(I,J)
         SUMPIJJI(I)=SUMPIJJI(I) + PMAT(I,J)*PMAT(J,I) 
         SUMQIJJI(I)=SUMQIJJI(I) + QMAT(I,J)*QMAT(J,I) 
         B62(I)=B62(I)+QMAT(J,I)
         B61(I)=B61(I)+PMAT(J,I)
  10   CONTINUE
        TOTPIJ=TOTPIJ+SUMPIJ(I)
        TOTQIJ=TOTQIJ+SUMQIJ(I)
        B6PART1=B6PART1+B61(I)**2
        B6PART2=B6PART2+B62(I)**2
        B5PART1=B5PART1+SUMPJI(I)*SUMPIJ(I)
        B5PART2=B5PART2+SUMQJI(I)*SUMQIJ(I)
        B3PART1=B3PART1+SUMPIJJI(I)
        B3PART2=B3PART2+SUMQIJJI(I)
        D3PART1=D3PART1+SUMPJ(I)**2
        D3PART2=D3PART2+SUMQJ(I)**2
    5  CONTINUE

       B1=(TOTPIJ**2)*(TOTQIJ**2)
       B2=SUMPIJ2*SUMQIJ2
       B3=B3PART1*B3PART2
       B4=(D3PART1-SUMPIJ2)*(D3PART2-SUMQIJ2)
       B5=(B5PART1-B3PART1)*(B5PART2-B3PART2)
       B6=(B6PART1-SUMPIJ2)*(B6PART2-SUMQIJ2)
       B7=(TOTPIJ**2 - 2*D3PART1 - 2*B5PART1 + B3PART1 + SUMPIJ2)*
     C    (TOTQIJ**2 - 2*D3PART2 - 2*B5PART2 + B3PART2 + SUMQIJ2)
       CB1=(1/(MATSIZE*(MATSIZE-1)))**2
       CB23=(1/(MATSIZE*(MATSIZE-1)))
       CB456=CB23/(MATSIZE-2)
       CB7=CB456/(MATSIZE-3)
C       WRITE(14,150) CB1,CB23,CB456,CB7
       HUBERTM=TOTPIJ*TOTQIJ*CB23
       HUBERTV=-CB1*B1+CB23*(B2+B3) + CB456*(B4+2*B5+B6) +CB7*B7
C            -((1/((MATSIZE-1)*MATSIZE))**2)*D1 +
C     C          (2/((MATSIZE-2)*MATSIZE))*D2    +
C     C          (4/((MATSIZE-2)*(MATSIZE-1)*MATSIZE))*D3 +
C     C           (1/((MATSIZE-3)*(MATSIZE-2)*(MATSIZE-1)*MATSIZE))*D4
        HUBERTSD=0
         IF (HUBERTV .GT. 0) THEN
       HUBERTSD=HUBERTV**.5
          END IF
       STDVAL=(ACTVAL-HUBERTM)/HUBERTSD
C       WRITE(14,150) HUBERTM,TOTPIJ,TOTQIJ,MATSIZE,HUBERTSD,B1,B2,B3,
C     C      B4,B5,B6,B7,CB1,CB23,CB456,CB7,B3PART1,B3PART2
  150 FORMAT('HUBERTS= ',(F40.15))
      RETURN
      END