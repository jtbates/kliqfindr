
      SUBROUTINE ONEPIECE(I,COUNTN,TEMPMAT,OLDORD,TSIZE)
      INCLUDE 'PARAM.H'
      INTEGER I,J,K,COUNTN,OLDORD(MAXKLIQ),TSIZE
      REAL TEMPMAT(MAXGR,MAXGR)

      REAL ZCOMPMAT(MAXKLIQ,MAXKLIQ)

       INTEGER HUBERT(MAXKLIQ,MAXKLIQ),RESULTM(MAXKLIQ,MAXKLIQ),
     C ALLGROUP(MAXKLIQ,MAXKLIQ),
     C NEWMAT(MAXKLIQ,MAXKLIQ)
       REAL BLAUC(MAXKLIQ,MAXKLIQ),DIFF(MAXKLIQ,MAXKLIQ)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF

C       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT

      DO 6565 J=1,COUNTN
      OLDORD(J)=ALLGROUP(I,J)
      DO 6566 K=1,COUNTN
      TEMPMAT(J,K)=NEWMAT(ALLGROUP(I,J),ALLGROUP(I,K))
06566  CONTINUE
06565   CONTINUE
      RETURN 
      END