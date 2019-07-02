CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                                                                      C
C  S M A C O F - IB  (VERSION 1)                                       C
C                                                                      C
C  N O N M E T R I C  M U L T I D I M E N S I O N A L  S C A L I N G   C
C                                                                      C
C                                                                      C
C  F I N A L  C H A N G E   07 / 1 / 1982                              C
C                                                                      C
C                                                                      C
C  INPUT DESCRIPTION AND LIMITATIONS                                   C
C                                                                      C
C                                                                      C
C    -CARD 1: TITLE(A80)                                               C
C                                                                      C
C    -CARD 2: NP,NR,ISYM,IWEI,IMIS,ILAB,VMIS  (6I5,F10.4)              C
C                                                                      C
C             NP = NUMBER OF POINTS                                    C
C             NR = NUMBER OF REPLICATIONS; UNLIMITED (DEFAULT=1)       C
C             ISYM = 0: DATA SYMMETRIC (LOWER TRIANGULAR MATRIX)       C
C                  =-1: DATA SYMMETRIC (VECTOR)                        C
C                  = 1: DATA ASYMMETRIC                                C
C             IWEI = 0: NO WEIGHTS (DEFAULT)                           C
C                  = 1: WEIGHTS                                        C
C             IMIS = 0: NO MISSING DATA (DEFAULT)                      C
C                  = 1: MISSING DATA                                   C
C                  =-1: NEGATIVE DATA VALUES POSSIBLE (ONLY IF ILEV=2) C
C             ILAB = 0: NO LABELS READ (DEFAULT)                       C
C                  = 1: UNIT NUMBER LABELS OF POINTS                   C
C             VMIS = ALL DATAVALUES LESS THAN OR EQUAL TO VMIS ARE     C
C                    INTERPRETED AS MISSING (ONLY IF IMIS=1)           C
C                                                                      C
C    -CARD 3: IPRI,IPRJ,IPLO,IEIG,IHIS,IIND,ISHE,IPUN,INDATA,ISTORE    C
C                  (10I5)                                              C
C                                                                      C
C             IPRI < 0: NO PRINT OF DATA                               C
C                  = 0: PRINT AGGREGATED DATA ONLY (DEFAULT)           C
C                  = 1: PRINT AGG. DATA AND OF FIRST   REPLICATION     C
C                  = K: PRINT AGG. DATA AND OF FIRST K REPLICATIONS    C
C             IPRJ < 0: NO PRINT OF DISPARITIES                        C
C                  = 0: PRINT AGGREGATED DISPARITIES ONLY (DEFAULT)    C
C                  = 1: PRINT AGG. DISP. AND OF FIRST   REPLICATION    C
C                  = K: PRINT AGG. DISP. AND OF FIRST K REPLICATIONS   C
C                     : ADD 1000 IF DISP. SHOULD BE STORED             C
C                     : ADD 2000 IF DISP. SHOULD BE PRINTED AND STORED C
C             IPLO = 0: NO PLOT OF FINAL CONFIGURATION                 C
C                  = 1: PLOT FIRST DIMENSION                           C
C                  = 2: PLOT FIRST TWO DIMENSIONS                      C
C                  = K: PLOT (PAIRWISE) K DIMENSIONS                   C
C             IEIG = 0: NO PRINT OR PLOT OF INITIAL CONFIGURATION      C
C                  = 1: PLOT FIRST DIMENSION (AND PRINT INITIAL CONF)  C
C                  = 2: PLOT FIRST TWO DIMENSIONS (AND PRINT)          C
C                  = K: PLOT (PAIRWISE) K DIMENSIONS (AND PRINT)       C
C             IHIS = 0: NO HISTORY                                     C
C                    1: HISTORY OF COMPUTATION                         C
C             IIND = 0: NO PRINT OF STRESS PER REPLICATION/ROW/COLUMN  C
C                    1: PRINT OF STRESS PER REPLICATION/ROW/COLUMN     C
C                    2: PRINT OF STRESS PER REPLICATION/ROW/COLUMN AND C
C                       OF STRESS PER ROW/COLUMN OF EACH REPLICATION   C
C             ISHE < 0: NO SHEPARD PLOTS                               C
C                  = 0: PLOT OF AGG. DISP VERSUS DIST ONLY (DEFAULT)   C
C                  = K: FOR THE FIRST K REPLICATIONS, K PLOTS OF DATA  C
C                       VERSUS DISPARITIES AND DISTANCES ARE GIVEN,    C
C                       PRECEDED BY A PLOT OF AGG. DISP VERSUS DIST.   C
C             IPUN = 0: NO FINAL RESULTS ARE PUNCHED OR STORED         C
C                  = 1: THE FINAL CONFIGURATION IS STORED/PUNCHED      C
C                  = 2: THE FINAL DISTANCES ARE STORED/PUNCHED         C
C                  = 3: BOTH FINAL MATRICES ARE STORED/PUNCHED         C
C                  = K: IF THE INITIAL CONFIGURATION SHOULD BE PUNCHED/C
C                       STORED, ADD 10 TO THE OPTIONS ABOVE            C
C             INDATA = REFERENCE NUMBER OF INPUT MEDIUM FOR THE DATA   C
C             ISTORE = REFERENCE NUMBER OF PUNCH/STORAGE MEDIUM        C
C                                                                      C
C                                                                      C
C    -CARD 4: NDMA,NDMI,MAX1,MAX2,IINI,IREL,CRI1,CRI2,ANUL             C
C                  (6I5,2F10.8,F10.4)                                  C
C                                                                      C
C             NDMA = MAXIMUM NUMBER OF DIMENSIONS (DEFAULT=2)          C
C             NDMI = MINIMUM NUMBER OF DIMENSIONS (DEFAULT=NDMA)       C
C             MAXI = MAXIMUM NUMBER OF ITERATIONS (DEFAULT=50)         C
C             MAXP = MAXIMUM NUMBER OF PRELIMINARY ITERATIONS (DEF=50) C
C             IINI = 0: NO INITIAL CONFIGURATION                       C
C                  < 0: INITIAL CONFIGURATION PROVIDED BY USER         C
C                  > 0: RANDOM INITIAL CONFIGURATION (DEPENDING ON     C
C                                                    VALUE OF IINI)    C
C             IREL = 0: USE RELAXED UPDATES (DEFAULT)                  C
C                  = 1: USE GUTTMAN TRANSFORM ONLY                     C
C                  = 2: USE STEPSIZE PROCEDURE                         C
C             CRII = CONVERGENCE CRITERION (DEFAULT=.00001)            C
C             CRIP = CONVERGENCE CRIT. OF PRELIMINARY IT. (DEF=.0001)  C
C             ANUL = STEPSIZE PARAMETER                                C
C                                                                      C
C    -CARD 5: ICON,IPRO,IDIS,INOR,ILEV (5I5)                           C
C                                                                      C
C             ICON = 0: MATRIX-CONDITIONAL (DEFAULT)                   C
C                  =-1: ROW-CONDITIONAL                                C
C                  = 1: COLUMN-CONDITIONAL                             C
C             IPRO < 0: METRIC ANALYSIS                                C
C                  = 0: NONMETRIC , NO TIES (DEFAULT)                  C
C                  = 1: NONMETRIC , PRIMARY   APPROACH TO TIES         C
C                  = 2: NONMETRIC , SECONDARY APPROACH TO TIES         C
C                  = 3: NONMETRIC , TERTIARY  APPROACH TO TIES         C
C             IDIS = 0: ASCENDING  REGRESSION (DISSIMILARITIES) (DEF)  C
C                  = 1: DESCENDING REGR. (SIMILARITIES, ONLY IF ILEV=2)C
C             INOR =-2: STRESS FORMULA 2 (EXPLICIT)                    C
C                  =-1: STRESS FORMULA 1 (EXPLICIT) (DEFAULT)          C
C                  = 1: STRESS FORMULA 1 (IMPLICIT)                    C
C                  = 2: STRESS FORMULA 2 (IMPLICIT)                    C
C             ILEV = 0: LINEAR REGRESSION , RATIO LEVEL (DEFAULT)      C
C                  = 1: LINEAR REGRESSION , INTERVAL LEVEL             C
C                  = 2: MONOTONE REGRESSION                            C
C                  = 3: MONOTONE REGRESSION PRECEDED BY (1)            C
C                                                                      C
C    -CARD 6: FORMAT OF DATA (A80)                                     C
C                                                                      C
C    -CARD 7: FORMAT OF WEIGHTS IN F-TYPE (A80)                        C
C             THIS CARD ONLY IF IWEI=1                                 C
C                                                                      C
C    -CARD 8: STIMULUS LABELS (10A8)                                   C
C             THIS CARD ONLY IF ILAB GT 0                              C
C             LABELS OF STIMULI ; IF NUMBER LARGER THAN 10,            C
C             PROCEED ON FOLLOWING CARD                                C
C                                                                      C
C    -FOLLOWING CARDS: DATA; IF ISYM=0, ONLY LOWER TRIANGULAR PART     C
C                                                                      C
C    -FOLLOWING CARDS: WEIGHTS; IF ISYM=0, ONLY LOWER TRIANGULAR PART  C
C                      THESE ONLY IF IWEI=1; IN CASE OF REPLICATIONS,  C
C                      EACH DATAMATRIX SHOULD BE ALTERNATED WITH THE   C
C                      WEIGHT MATRIX                                   C
C                                                                      C
C    -FOLLOWING CARDS: INITIAL CONFIGURATION NP*NDMA (IF IINI=1 ONLY)  C
C                      WITH FIXED FORMAT: (8X,6F12.7)                  C
C                                                                      C
C                                                                      C
C  TO ESTIMATE THE TOTAL NUMBER OF WORDS NEEDED FOR THE ARRAY AREA,    C
C  USE THE FORMULA                                                     C
C     IF (ISYM.EQ.0.AND.ICON.EQ.0) NWORDS=13*NP*NP+ 7*NP+3*NP*ND       C
C     IF (ISYM.EQ.1.AND.ICON.EQ.0) NWORDS=16*NP*NP+ 8*NP+3*NP*ND       C
C     IF (ISYM.EQ.0.AND.ICON.NE.0) NWORDS=14*NP*NP+10*NP+3*NP*ND       C
C     IF (ISYM.EQ.1.AND.ICON.NE.0) NWORDS=14*NP*NP+10*NP+3*NP*ND       C
C                                                                      C
C  THE NUMBER OF REPLICATIONS DOES NOT INFLUENCE THE SIZE OF THE ARRAY C
C  AREA. IN THE STATIC ALLOCATION VERSION, NWORDS IS FIXED AT 16500 ;  C
C  THIS IMPLIES THAT ANALYSES OF 60 POINTS IN 2 OR 3 DIMENSIONS OR     C
C  50 POINTS IN 50 DIMENSIONS CAN BE DONE WITHIN THE STANDARD AREA.    C
C                                                                      C
C                                                                      C
C                                                                      C
C                                                                      C
C                                                                      C
C  AUTHORS: INEKE STOOP AND WILLEM HEISER                              C
C           DEPARTMENT OF DATATHEORY - UNIVERSITY OF LEIDEN            C
C           BREESTRAAT 70 - LEIDEN - THE NETHERLANDS                   C
C                                                                      C
C  REFERENCES:                                                         C
C          JAN DE LEEUW - APPLICATIONS OF CONVEX ANALYSIS TO MULTIDI-  C
C             MENSIONAL SCALING (1977).                                C
C          JAN DE LEEUW / WILLEM HEISER - CONVERGENCE OF CORRECTION    C
C             MATRIX ALGORITHMS FOR MULTIDIMENSIONAL SCALING (1977).   C
C          JAN DE LEEUW / WILLEM HEISER - MULTIDIMENSIONAL SCALING     C
C             WITH RESTRICTIONS ON THE CONFIGURATION (1980)            C
C          WILLEM HEISER / JAN DE LEEUW - HOW TO USE SMACOF-I.         C
C          INEKE STOOP/WILLEM HEISER/JAN DE LEEUW-HOW TO USE SMACOF-IA C
C          INEKE STOOP/JAN DE LEEUW/WILLEM HEISER-HOW TO USE SMACOF-IB C
C                                                                      C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE COQ(SS,X,Y,W,N,IWEI)
C     ******************************************************************
C     *                                                                *
C     *  C O Q                                                         *
C     *                                                                *
C     *  PURPOSE: COMPUTE (WEIGHTED) SUM X'Y                           *
C     *                                                                *
C     *  SUBROUTINES CALLED: NONE                                      *
C     *                                                                *
C     *  AUTHORS : INEKE STOOP                RELEASED FEBRUARY 1982   *
C     *                                                                *
C     ******************************************************************
      DIMENSION X(N),W(N),Y(N)
C
      SS=0.
      IF (IWEI.EQ.0) GOTO 20
      DO 10 L=1,N
         SS=SS+X(L)*Y(L)*W(L)
 10   CONTINUE
      RETURN
 20   DO 30 L=1,N
         SS=SS+X(L)*Y(L)
 30   CONTINUE
      RETURN
      END
      SUBROUTINE COVAR(CS,X,Y,W,N,IWEI,NDAAT)
C     ******************************************************************
C     *                                                                *
C     *  C O V A R                                                     *
C     *                                                                *
C     *  PURPOSE: COMPUTE (X-XM)'(Y-YM)                                *
C     *                                                                *
C     *  SUBROUTINES CALLED: NONE                                      *
C     *                                                                *
C     *  AUTHORS : INEKE STOOP                     RELEASED MAY 1982   *
C     *                                                                *
C     ******************************************************************
      DIMENSION X(N),W(N),Y(N)
C
      XM=0.
      YM=0.
      CS=0.
      IF (IWEI.EQ.0) GOTO 20
      XW=0.
      DO 10 L=1,N
         XW=XW+W(L)
         XM=XM+X(L)*W(L)
         YM=YM+Y(L)*W(L)
 10   CONTINUE
      IF (XW.GT.1.0E-6) GOTO 40
      RETURN
 20   DO 30 L=1,N
         XM=XM+X(L)
         YM=YM+Y(L)
 30   CONTINUE
      XW=FLOAT(NDAAT)
 40   XM=XM/XW
      YM=YM/XW
      DO 50 L=1,N
         DIFX=X(L)-XM
         DIFY=Y(L)-YM
         IF (IWEI.EQ.0) CS=CS+DIFX*DIFY
         IF (IWEI.EQ.1) CS=CS+DIFX*DIFY*W(L)
 50   CONTINUE
      RETURN
      END
      SUBROUTINE DEVFM(X,NC,NR,XM)
C     ******************************************************************
C     *                                                                *
C     *  D E V F M                                                     *
C     *                                                                *
C     *  PURPOSE: COMPUTE X IN DEVIATION FROM COLUMN MEANS             *
C     *                                                                *
C     *  SUBROUTINES CALLED: NONE                                      *
C     *                                                                *
C     *  AUTHORS : INEKE STOOP                RELEASED SEPTEMBER 1982  *
C     *                                                                *
C     ******************************************************************
      DIMENSION X(NR,NC),XM(NC)
C----------------------------------------------------------------------C
      FINR=1./FLOAT(NR)
      DO 20 J=1,NC
         XM(J)=0.
         DO 10 I=1,NR
            XM(J)=XM(J)+X(I,J)
 10      CONTINUE
         XM(J)=XM(J)*FINR
 20   CONTINUE
C----------------------------------------------------------------------C
      DO 40 I=1,NR
         DO 30 J=1,NC
            X(I,J)=X(I,J)-XM(J)
 30      CONTINUE
 40   CONTINUE
      RETURN
      END
      SUBROUTINE DISCOM(X,DIST,NDAT,NP,ND)
C     ******************************************************************
C     *                                                                *
C     *  D I S C O M                                                   *
C     *                                                                *
C     *  PURPOSE: COMPUTE EUCLIDEAN DISTANCES                          *
C     ******************************************************************
      DIMENSION  X(NP,ND),DIST(NDAT)
      L=0
      DO 30 I=2,NP
         IMIN=I-1
         DO 20 J=1,IMIN
            L=L+1
            DIST(L)=0.0
            DO 10 K=1,ND
               DIFF=X(I,K)-X(J,K)
               DIST(L)=DIST(L)+DIFF*DIFF
 10         CONTINUE
            DIST(L)=SQRT(DIST(L))
 20      CONTINUE
 30   CONTINUE
      RETURN
      END
      SUBROUTINE DISSIM(DATA,N)
C     ******************************************************************
C     *                                                                *
C     *  D I S S I M                                                   *
C     *                                                                *
C     *  PURPOSE: - MAKE DISSIMILARITIES FROM SIMIMILARITIES           *
C     *                                                                *
C     ******************************************************************
      DIMENSION DATA(N)
      DATA DMAX / 0.0 /
      DO 10 L=1,N
         IF (DATA(L).GT.DMAX) DMAX=DATA(L)
 10   CONTINUE
         DMAX=1.1*DMAX
      DO 20 L=1,N
         DATA(L)=DMAX-DATA(L)
 20   CONTINUE
      RETURN
      END
      SUBROUTINE DMFSD(A,NA,N,EPS,IER)                                  MFS00010
C     ******************************************************************MFS00020
C     *                                                                *MFS00030
C     *  D M F S D                                                      MFS00040
C     *                                                                *MFS00050
C     *  PURPOSE: FACTORS A GIVEN SYMMETRIC POSITIVE DEFINITE MATRIX   *MFS00060
C     *                                                                *MFS00070
C     *  SUBROUTINES CALLED: NONE                                      *MFS00080
C     *                                                                *MFS00090
C     *  ADAPTED FROM SSP                                              *MFS00100
C     *                                                                *MFS00110
C     ******************************************************************MFS00120
      DOUBLE PRECISION DPIV,DSUM,A,DSQRT                                MFS00130
      DIMENSION A(NA)                                                   MFS00140
      IF(N-1) 12,1,1                                                    MFS00150
    1 IER=0                                                             MFS00160
      KPIV=0                                                            MFS00170
      DO 11 K=1,N                                                       MFS00180
      KPIV=KPIV+K                                                       MFS00190
      IND=KPIV                                                          MFS00200
      LEND=K-1                                                          MFS00210
      TOL=ABS(EPS*SNGL(A(KPIV)))                                        MFS00220
      DO 11 I=K,N                                                       MFS00230
      DSUM=0.D0                                                         MFS00240
      IF(LEND) 2,4,2                                                    MFS00250
    2 DO 3 L=1,LEND                                                     MFS00260
      LANF=KPIV-L                                                       MFS00270
      LIND=IND-L                                                        MFS00280
    3 DSUM=DSUM+(A(LANF)*A(LIND))                                       MFS00290
    4 DSUM=A(IND)-DSUM                                                  MFS00300
      IF(I-K) 10,5,10                                                   MFS00310
    5 IF(SNGL(DSUM)-TOL) 6,6,9                                          MFS00320
    6 IF(DSUM) 12,12,7                                                  MFS00330
    7 IF(IER) 8,8,9                                                     MFS00340
    8 IER=K-1                                                           MFS00350
    9 DPIV=DSQRT(DSUM)                                                  MFS00360
      A(KPIV)=DPIV                                                      MFS00370
      DPIV=1.D0/DPIV                                                    MFS00380
      GO TO 11                                                          MFS00390
   10 A(IND)=DSUM*DPIV                                                  MFS00400
   11 IND=IND+I                                                         MFS00410
      RETURN                                                            MFS00420
   12 IER=-1                                                            MFS00430
      RETURN                                                            MFS00440
      END                                                               MFS00450
      SUBROUTINE PRIMIR(B,NI,NJ,VMIS,IWRITE)                            MIS00010
C     ******************************************************************MIS00020
C     *                                                                *MIS00030
C     *  P R I M I R                                                   *MIS00040
C     *                                                                *MIS00050
C     *  PURPOSE: PRINTS THE SQUARE ASYMMETRIC NI*NJ MATRIX WITH           00060
C     *           MISSING ELEMENTS, STORED IN THE TWO-DIMENSIONAL       MIS00070
C     *           ARRAY B (SIZE NI*NJ). DIAGONAL ENTRIES               *MIS00080
C     *           ARE PRINTED AS '*'; VALUES LESS THAN OR EQUAL  *      MIS00090
C     *           TO VMIS ARE INTERPRETED AS MISSING AND PRINTED AS '-'*MIS00100
C     *                                                                *MIS00110
C     *  SUBROUTINES CALLED: NONE                                      *MIS00120
C     *                                                                *MIS00130
C     *  AUTHOR: WILLEM HEISER              RELEASED: APRIL 1977       *MIS00140
C     *                                                                *MIS00150
C     ******************************************************************MIS00160
      CHARACTER*4 FFORM,AFORM,STAR,AMIN,FORM(28),SPRP(12)
      DIMENSION B(NI,NJ),SPR(12)
C     DATA FORM/4H(1H ,4H, I5,4H, 2X,4H ,  ,4H A9 ,4H ,  ,4H A9 ,       MIS00180
C    1          4H ,  ,4H A9 ,4H ,  ,4H A9 ,4H ,  ,4H A9 ,4H ,  ,       MIS00190
C    2          4H A9 ,4H ,  ,4H A9 ,4H ,  ,4H A9 ,4H ,  ,4H A9 ,       MIS00200
C    3          4H ,  ,4H A9 ,4H ,  ,4H A9 ,4H ,  ,4H A9 ,4H)   /       MIS00210
C     DATA FFORM,AFORM/4HF9.3,4H A9 /                                   MIS00220
C     DATA STAR,AMIN/4H   *,4H   -/                                     MIS00
      DATA FORM/'(1H ',', I5',', 2X',' ,  ',' A9 ',' ,  ',' A9 ',
     1          ' ,  ',' A9 ',' ,  ',' A9 ',' ,  ',' A9  ',' ,  ',
     2          ' A9 ',' ,  ',' A9 ',' ,  ',' A9 ',' ,  ',' A9 ',
     3          ' ,  ',' A9 ',' ,  ',' A9 ',' ,  ',' A9 ',')   '/
      DATA FFORM/'F9.3'/,AFORM/' A9 '/
      DATA STAR/'   *'/,AMIN/'   -'/
      EQUIVALENCE (SPR,SPRP)
C                                                                      CMIS00240
C  CHECK HOW MANY TIMES THE NUMBER OF COLUMNS EXCEEDS 12               CMIS00250
C                                                                      CMIS00260
      NTRU=NI/12                                                        MIS00270
      NREM=NI-NTRU*12                                                   MIS00280
      IF (NREM.GT.0) GO TO 1                                            MIS00290
         NREM=12                                                        MIS00300
         NTRU=NTRU-1                                                    MIS00310
 1    NREP=NTRU+1                                                       MIS00320
C                                                                      CMIS00330
C  PRINT NREP TIMES A BLOCK OF ELEMENTS                                CMIS00340
C                                                                      CMIS00350
      KB=1                                                              MIS00360
      NK=12                                                             MIS00370
      DO 2 I=1,NREP                                                     MIS00380
         IF (I.EQ.NREP) NK=NREM                                         MIS00390
         KE=KB+NK-1                                                     MIS00410
         WRITE(IWRITE,60) (K,K=KB,KE)
         WRITE(IWRITE,61)
         DO 3 L=1,NI
              ISPR=0
              IFORM=3
              DO 8 J=KB,KE
                   ISPR=ISPR+1
                   IFORM=IFORM+2
                   IF (L.NE.J) GOTO 10
C                  SPR(ISPR)=STAR
                   SPRP(ISPR)=STAR
                   FORM(IFORM)=AFORM
                       GOTO 8
 10                IF (B(L,J).GT.VMIS) GOTO 9
C                  SPR(ISPR)=AMIN
                   SPRP(ISPR)=AMIN
                   FORM(IFORM)=AFORM
                       GOTO 8
  9                SPR(ISPR)=B(L,J)
                   FORM(IFORM)=FFORM
  8           CONTINUE
              WRITE(IWRITE,FORM) L,(SPR(K),K=1,NK)
  3      CONTINUE
              KB=KB+NK
  2      CONTINUE
      RETURN                                                            MIS00970
 60   FORMAT(1H0,4X,12I9)                                               MIS00980
 61   FORMAT(1H )                                                       MIS00990
      END                                                               MIS01010
      SUBROUTINE PRIMIS(S,N,NS,VMIS,IWRITE)                             MIS00010
C     ******************************************************************MIS00020
C     *                                                                *MIS00030
C     *  P R I M I S                                                   *MIS00040
C     *                                                                *MIS00050
C     *  PURPOSE: PRINTS THE LOWER TRIANGULAR PART OF A SQUARE SYMME-  *MIS00060
C     *           TRIC N*N MATRIX WITH MISSING ELEMENTS, STORED IN THE *MIS00070
C     *           ONE-DIMENSIONAL ARRAY S OF LENGTH NS. DIAGONAL EN-   *MIS00080
C     *           TRIES ARE PRINTED AS '*'; VALUES LESS THAN OR EQUAL  *MIS00090
C     *           TO VMIS ARE INTERPRETED AS MISSING AND PRINTED AS '-'*MIS00100
C     *                                                                *MIS00110
C     *  SUBROUTINES CALLED: NONE                                      *MIS00120
C     *                                                                *MIS00130
C     *  AUTHOR: WILLEM HEISER              RELEASED: APRIL 1977       *MIS00140
C     *                                                                *MIS00150
C     ******************************************************************MIS00160
      CHARACTER*4 FORM(28),FFORM,AFORM,STAR,AMIN
      CHARACTER*4 SPRP(12)
      DIMENSION SPR(12),S(NS)
C     DATA FORM/4H(1H ,4H, I5,4H, 2X,4H ,  ,4H A9 ,4H ,  ,4H A9 ,       MIS00180
C    1          4H ,  ,4H A9 ,4H ,  ,4H A9 ,4H ,  ,4H A9 ,4H ,  ,       MIS00190
C    2          4H A9 ,4H ,  ,4H A9 ,4H ,  ,4H A9 ,4H ,  ,4H A9 ,       MIS00200
C    3          4H ,  ,4H A9 ,4H ,  ,4H A9 ,4H ,  ,4H A9 ,4H)   /       MIS00210
C     DATA FFORM,AFORM/4HF9.3,4H A9 /                                   MIS00220
C     DATA STAR,AMIN/4H   *,4H   -/                                     MI
      DATA FORM/'(1H ',', I5',', 2X',' ,  ',' A9 ',' ,  ',' A9 ',
     1          ' ,  ',' A9 ',' ,  ',' A9 ',' ,  ',' A9  ',' ,  ',
     2          ' A9 ',' ,  ',' A9 ',' ,  ',' A9 ',' ,  ',' A9 ',
     3          ' ,  ',' A9 ',' ,  ',' A9 ',' ,  ',' A9 ',')   '/
      DATA FFORM/'F9.3'/,AFORM/' A9 '/
      DATA STAR/'   *'/,AMIN/'   -'/
      EQUIVALENCE (SPR,SPRP)
C                                                                      CMIS00240
C  CHECK HOW MANY TIMES THE NUMBER OF COLUMNS EXCEEDS 12               CMIS00250
C                                                                      CMIS00260
      NTRU=N/12                                                         MIS00270
      NREM=N-NTRU*12                                                    MIS00280
      IF (NREM.GT.0) GO TO 1                                            MIS00290
         NREM=12                                                        MIS00300
         NTRU=NTRU-1                                                    MIS00310
 1    NREP=NTRU+1                                                       MIS00320
C                                                                      CMIS00330
C  PRINT NREP TIMES A BLOCK OF ELEMENTS                                CMIS00340
C                                                                      CMIS00350
      LB=1                                                              MIS00360
      NK=12                                                             MIS00370
      DO 2 I=1,NREP                                                     MIS00380
         IF (I.EQ.NREP) NK=NREM                                         MIS00390
         KB=LB                                                          MIS00400
         KE=KB+NK-1                                                     MIS00410
         WRITE(IWRITE,60) (K,K=KB,KE)                                   MIS00420
         WRITE(IWRITE,61)                                               MIS00430
         WRITE(IWRITE,62) KB,STAR                                       MIS00440
         LB=KB+1                                                        MIS00450
         LE=KE                                                          MIS00460
         IF (LB.GT.LE) RETURN                                           MIS00470
C                                                                      CMIS00480
C  PRINT TRIANGULAR PART                                               CMIS00490
C                                                                      CMIS00500
            ME=(KB*KB-KB)/2                                             MIS00510
            INCR=0                                                      MIS00520
            DO 4 L=LB,LE                                                MIS00530
               MB=ME+KB                                                 MIS00540
               ME=MB+INCR                                               MIS00550
               INCR=INCR+1                                              MIS00560
               ISPR=0                                                   MIS00570
               IFORM=3                                                  MIS00580
               DO 5 M=MB,ME                                             MIS00590
                  ISPR=ISPR+1                                           MIS00600
                  IFORM=IFORM+2                                         MIS00610
                  IF (S(M).GT.VMIS) GO TO 6                             MIS00620
C                    SPR(ISPR)=AMIN                                     MIS00630
                     SPRP(ISPR)=AMIN                                    MIS00630
                     FORM(IFORM)=AFORM                                  MIS00640
                     GO TO 5                                            MIS00650
 6                SPR(ISPR)=S(M)                                        MIS00660
                  FORM(IFORM)=FFORM                                     MIS00670
 5                CONTINUE                                              MIS00680
               ISPR=ISPR+1                                              MIS00690
C              SPR(ISPR)=STAR                                           MIS00700
               SPRP(ISPR)=STAR                                          MIS00700
               FORM(IFORM+2)=AFORM                                      MIS00710
 4             WRITE(IWRITE,FORM) L,(SPR(J),J=1,ISPR)                   MIS00720
            LB=LE+1                                                     MIS00730
            IF (LB.GT.N) RETURN                                         MIS00740
C                                                                      CMIS00750
C  PRINT RECTANGULAR PART                                              CMIS00760
C                                                                      CMIS00770
               INCR=KE-1                                                MIS00780
               DO 7 L=LB,N                                              MIS00790
                  MB=MB+INCR                                            MIS00800
                  ME=MB+11                                              MIS00810
                  INCR=INCR+1                                           MIS00820
                  ISPR=0                                                MIS00830
                  IFORM=3                                               MIS00840
                  DO 8 M=MB,ME                                          MIS00850
                     ISPR=ISPR+1                                        MIS00860
                     IFORM=IFORM+2                                      MIS00870
                     IF (S(M).GT.VMIS) GO TO 9                          MIS00880
C                       SPR(ISPR)=AMIN                                  MIS00890
                        SPRP(ISPR)=AMIN                                 MIS00890
                        FORM(IFORM)=AFORM                               MIS00900
                        GO TO 8                                         MIS00910
 9                   SPR(ISPR)=S(M)                                     MIS00920
                     FORM(IFORM)=FFORM                                  MIS00930
 8                   CONTINUE                                           MIS00940
 7                WRITE(IWRITE,FORM) L,(SPR(J),J=1,12)                  MIS00950
 2       CONTINUE                                                       MIS00960
      RETURN                                                            MIS00970
 60   FORMAT(1H0,4X,12I9)                                               MIS00980
 61   FORMAT(1H )                                                       MIS00990
 62   FORMAT(1H ,I5,2X,A9)                                              MIS01000
      END                                                               MIS01010
      SUBROUTINE PRITRI(T,N,NT,IWRITE)                                  TRI00010
C     ******************************************************************TRI00020
C     *                                                                *TRI00030
C     *  P R I T R I                                                   *TRI00040
C     *                                                                *TRI00050
C     *  PURPOSE: PRINTS THE LOWER TRIANGULAR PART OF A SQUARE SYMME-  *TRI00060
C     *           TRIC N*N MATRIX, STORED IN THE ONE-DIMENSIONAL ARRAY *TRI00070
C     *           T OF LENGTH NT. DIAGONAL ENTRIES ARE PRINTED AS STARS*TRI00080
C     *                                                                *TRI00090
C     *  SUBROUTINES CALLED: NONE                                      *TRI00100
C     *                                                                *TRI00110
C     *  AUTHOR: WILLEM HEISER          RELEASED: APRIL 1977           *TRI00120
C     *                                                                *TRI00130
C     ******************************************************************TRI00140
      CHARACTER*4 FORM(6),FREP(12),STAR
      DIMENSION T(NT)
C     DATA FORM/4H(1H ,4H, I5,4H,2X,,4H    ,4HF9.3,4H,A9)/              TRI00160
C     DATA FREP/4H   1,4H   2,4H   3,4H   4,4H   5,4H   6,              TRI00170
C    X          4H   7,4H   8,4H   9,4H  10,4H  11,4H  12/              TRI00180
C     DATA STAR/4H   */                                                 TRI00
      DATA FORM/'(1H ','  I5',',2X,','    ','F9.3',',A9)'/
      DATA FREP/'   1','   2','   3','   4','   5','   6',
     X          '   7','   8','   9','  10','  11','  12'/
      DATA STAR/'   *'/                                                 TRI00
C                                                                      CTRI00200
C  CHECK HOW MANY TIMES THE NUMBER OF COLUMNS EXCEEDS 12               CTRI00210
C                                                                      CTRI00220
      NTRU=N/12                                                         TRI00230
      NREM=N-NTRU*12                                                    TRI00240
      IF (NREM.GT.0) GO TO 1                                            TRI00250
         NREM=12                                                        TRI00260
         NTRU=NTRU-1                                                    TRI00270
 1    NREP=NTRU+1                                                       TRI00280
C                                                                      CTRI00290
C  PRINT NREP TIMES A BLOCK OF ELEMENTS                                CTRI00300
C                                                                      CTRI00310
      LB=1                                                              TRI00320
      NK=12                                                             TRI00330
      DO 2 I=1,NREP                                                     TRI00340
         IF (I.EQ.NREP) NK=NREM                                         TRI00350
         KB=LB                                                          TRI00360
         KE=KB+NK-1                                                     TRI00370
         WRITE(IWRITE,60) (K,K=KB,KE)                                   TRI00380
         WRITE(IWRITE,61)                                               TRI00390
         WRITE(IWRITE,62) KB,STAR                                       TRI00400
         LB=KB+1                                                        TRI00410
         LE=KE                                                          TRI00420
         IF (LB.GT.LE) RETURN                                           TRI00430
C                                                                      CTRI00440
C  PRINT TRIANGULAR PART                                               CTRI00450
C                                                                      CTRI00460
            ME=(KB*KB-KB)/2                                             TRI00470
            INCR=0                                                      TRI00480
         DO 4 L=LB,LE                                                   TRI00490
            MB=ME+KB                                                    TRI00500
            ME=MB+INCR                                                  TRI00510
            INCR=INCR+1                                                 TRI00520
            FORM(4)=FREP(INCR)                                          TRI00530
 4          WRITE(IWRITE,FORM) L,(T(M),M=MB,ME),STAR                    TRI00540
         LB=LE+1                                                        TRI00550
         IF (LB.GT.N) RETURN                                            TRI00560
C                                                                      CTRI00570
C  PRINT RECTANGULAR PART                                              CTRI00580
C                                                                      CTRI00590
            FORM(4)=FREP(12)                                            TRI00600
            INCR=KE-1                                                   TRI00610
            DO 7 L=LB,N                                                 TRI00620
               MB=MB+INCR                                               TRI00630
               ME=MB+11                                                 TRI00640
               INCR=INCR+1                                              TRI00650
 7             WRITE(IWRITE,FORM) L,(T(M),M=MB,ME)                      TRI00660
 2       CONTINUE                                                       TRI00670
      RETURN                                                            TRI00680
 60   FORMAT(1H0,4X,12I9)                                               TRI00690
 61   FORMAT(1H )                                                       TRI00700
 62   FORMAT(1H ,I5,2X,A9)                                              TRI00710
      END                                                               TRI00720
      SUBROUTINE DSINV(A,NA,N,EPS,IER)                                  SIN00010
C     ******************************************************************SIN00020
C     *                                                                *SIN00030
C     *  D S I N V                                                      SIN00040
C     *                                                                *SIN00050
C     *  PURPOSE: INVERTS A GIVEN SYMMETRIC POSITIVE DEFINITE MATRIX   *SIN00060
C     *                                                                *SIN00070
C     *  SUBROUTINES CALLED: DMFSD                                      SIN00080
C     *                                                                *SIN00090
C     *  ADAPTED FROM SSP                                              *SIN00100
C     *                                                                *SIN00110
C     ******************************************************************SIN00120
      DOUBLE PRECISION DIN,WORK,A                                       SIN00130
      DIMENSION A(NA)                                                   SIN00140
      CALL DMFSD(A,NA,N,EPS,IER)                                        SIN00150
      IF(IER) 9,1,1                                                     SIN00160
    1 IPIV=N*(N+1)/2                                                    SIN00170
      IND=IPIV                                                          SIN00180
      DO 6 I=1,N                                                        SIN00190
      DIN=1.D0/(A(IPIV))                                                SIN00200
      A(IPIV)=DIN                                                       SIN00210
      MIN=N                                                             SIN00220
      KEND=I-1                                                          SIN00230
      LANF=N-KEND                                                       SIN00240
      IF(KEND) 5,5,2                                                    SIN00250
    2 J=IND                                                             SIN00260
      DO 4 K=1,KEND                                                     SIN00270
      WORK=0.D0                                                         SIN00280
      MIN=MIN-1                                                         SIN00290
      LHOR=IPIV                                                         SIN00300
      LVER=J                                                            SIN00310
      DO 3 L=LANF,MIN                                                   SIN00320
      LVER=LVER+1                                                       SIN00330
      LHOR=LHOR+L                                                       SIN00340
    3 WORK=WORK+(A(LVER)*A(LHOR))                                       SIN00350
      A(J)=-WORK*DIN                                                    SIN00360
    4 J=J-MIN                                                           SIN00370
    5 IPIV=IPIV-MIN                                                     SIN00380
    6 IND=IND-1                                                         SIN00390
      DO 8 I=1,N                                                        SIN00400
      IPIV=IPIV+I                                                       SIN00410
      J=IPIV                                                            SIN00420
      DO 8 K=I,N                                                        SIN00430
      WORK=0.D0                                                         SIN00440
      LHOR=J                                                            SIN00450
      DO 7 L=K,N                                                        SIN00460
      LVER=LHOR+K-I                                                     SIN00470
      WORK=WORK+(A(LHOR)*A(LVER))                                       SIN00480
    7 LHOR=LHOR+L                                                       SIN00490
      A(J)=WORK                                                         SIN00500
    8 J=J+K                                                             SIN00510
    9 RETURN                                                            SIN00520
      END                                                               SIN00530
      SUBROUTINE IMTQL1(N,D,E,IERR)                                     IM100010
C     ******************************************************************IM100020
C     *                                                                *IM100030
C     *  I M T Q L 1                                                   *IM100040
C     *                                                                *IM100050
C     *  PURPOSE: DETERMINES THE EIGENVALUES OF A SYMMETRIC TRIDIAGONAL*IM100060
C     *           MATRIX USING THE IMPLICIT QL METHOD                  *IM100070
C     *                                                                *IM100080
C     *  SUBROUTINES CALLED: NONE                                      *IM100090
C     *                                                                *IM100100
C     *  ADAPTED FROM EISPACK                                          *IM100110
C     *                                                                *IM100120
C     ******************************************************************IM100130
      DIMENSION D(N),E(N)                                               IM100140
      REAL MACHEP                                                       IM100150
C                                                                       IM100160
C     ********** MACHEP IS A MACHINE DEPENDENT PARAMETER SPECIFYING     IM100170
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.   IM100180
C                                                                       IM100190
C                **********                                             IM100200
C                                                                       IM100210
      MACHEP = 2.0**(-20)                                               IM100220
C                                                                       IM100230
      IERR = 0                                                          IM100240
      IF (N .EQ. 1) GO TO 1001                                          IM100250
C                                                                       IM100260
      DO 100 I = 2, N                                                   IM100270
  100 E(I-1) = E(I)                                                     IM100280
C                                                                       IM100290
      E(N) = 0.0                                                        IM100300
C                                                                       IM100310
      DO 290 L = 1, N                                                   IM100320
         J = 0                                                          IM100330
C     ********** LOOK FOR SMALL SUB-DIAGONAL ELEMENT **********         IM100340
  105    DO 110 M = L, N                                                IM100350
            IF (M .EQ. N) GO TO 120                                     IM100360
            IF (ABS(E(M)) .LE. MACHEP* (ABS(D(M)) + ABS(D(M+1))))       IM100370
     X         GO TO 120                                                IM100380
  110 CONTINUE                                                          IM100390
C                                                                       IM100400
  120 P = D(L)                                                          IM100410
      IF (M .EQ. L) GO TO 215                                           IM100420
      IF (J .EQ.30) GO TO 1000                                          IM100430
      J = J + 1                                                         IM100440
C     ********** FORM SHIFT **********                                  IM100450
      G = (D(L+1) - P) / (2.0 * E(L))                                   IM100460
      R = SQRT(G*G+1.0)                                                 IM100470
      G = D(M) - P + E(L) / (G + SIGN(R,G))                             IM100480
      S = 1.0                                                           IM100490
      C = 1.0                                                           IM100500
      P = 0.0                                                           IM100510
      NML = M - L                                                       IM100520
C     ********** FOR I=M-1 STEP -1 UNTIL L DO -- **********             IM100530
      DO 200 II = 1, NML                                                IM100540
      I = M - II                                                        IM100550
      F = S * E(I)                                                      IM100560
      B = C * E(I)                                                      IM100570
      IF (ABS(F) .LT. ABS(G)) GO TO 150                                 IM100580
      C = G / F                                                         IM100590
      R = SQRT(C*C+1.0)                                                 IM100600
      E(I+1) = F * R                                                    IM100610
      S = 1.0 / R                                                       IM100620
      C = C * S                                                         IM100630
      GO TO 160                                                         IM100640
  150 S = F / G                                                         IM100650
      R = SQRT(S*S+1.0)                                                 IM100660
      E(I+1) = G* R                                                     IM100670
      C = 1.0 / R                                                       IM100680
      S = S * C                                                         IM100690
  160 G = D(I+1) - P                                                    IM100700
      R = (D(I) - G) * S + 2.0 * C * B                                  IM100710
      P = S * R                                                         IM100720
      D(I+1) = G + P                                                    IM100730
      G = C * R - B                                                     IM100740
  200 CONTINUE                                                          IM100750
C                                                                       IM100760
      D(L) = D(L) - P                                                   IM100770
      E(L) = G                                                          IM100780
      E(M) = 0.0                                                        IM100790
      GO TO 105                                                         IM100800
C     ********** ORDER EIGENVALUES **********                           IM100810
  215    IF (L.EQ.1) GO TO 250                                          IM100820
C     ********** FOR I=L STEP -1 UNTIL 2 DO -- *********                IM100830
         DO 230 II = 2, L                                               IM100840
            I = L + 2 - II                                              IM100850
            IF (P .LE. D(I-1)) GO TO 270                                IM100860
            D(I) = D(I-1)                                               IM100870
  230    CONTINUE                                                       IM100880
C                                                                       IM100890
 250     I = 1                                                          IM100900
  270    D(I) = P                                                       IM100910
  290 CONTINUE                                                          IM100920
C                                                                       IM100930
      GO TO 1001                                                        IM100940
C     ********** SET ERROR -- NO CONVERGENCE TO AN                      IM100950
C                EIGENVALUE AFTER 30 ITERATIONS **********              IM100960
 1000 IERR = L                                                          IM100970
 1001 RETURN                                                            IM100980
      END                                                               IM100990
      SUBROUTINE IMTQL2(NM,N,D,E,Z,IERR)                                IM200010
C     ******************************************************************IM200020
C     *                                                                *IM200030
C     *  I M T Q L 2                                                   *IM200040
C     *                                                                *IM200050
C     *  PURPOSE: DETERMINES THE EIGENVALUES AND EIGENVECTORS OF A     *IM200060
C     *           SYMMETRIC TRIDIAGONAL MATRIX USING THE IMPLICIT      *IM200070
C     *           QL METHOD.                                           *IM200080
C     *                                                                *IM200090
C     *  SUBROUTINES CALLED: NONE                                      *IM200100
C     *                                                                *IM200110
C     *  ADAPTED FROM EISPACK                                          *IM200120
C     *                                                                *IM200130
C     ******************************************************************IM200140
      DIMENSION D(N),E(N),Z(NM,N)                                       IM200150
      REAL MACHEP                                                       IM200160
C                                                                       IM200170
C     ********** MACHEP IS A MACHINE DEPENDENT PARAMETER SPCIFYING      IM200180
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.   IM200190
C                                                                       IM200200
C                **********                                             IM200210
C                                                                       IM200220
      MACHEP = 2.0**(-20)                                               IM200230
C                                                                       IM200240
      IERR = 0                                                          IM200250
      IF (N .EQ. 1) GO TO 1001                                          IM200260
C                                                                       IM200270
      DO 100 I = 2, N                                                   IM200280
  100 E(I-1) = E(I)                                                     IM200290
C                                                                       IM200300
      E(N) = 0.0                                                        IM200310
C                                                                       IM200320
      DO 240 L = 1, N                                                   IM200330
         J = 0                                                          IM200340
C     ********** LOOK FOR SMALL SUB-DIAGONAL ELEMENT **********         IM200350
  105    DO 110 M = L, N                                                IM200360
            IF (M .EQ. N) GO TO 120                                     IM200370
            IF (ABS(E(M)) .LE. MACHEP* (ABS(D(M)) + ABS(D(M+1))))       IM200380
     X         GO TO 120                                                IM200390
  110 CONTINUE                                                          IM200400
C                                                                       IM200410
  120 P = D(L)                                                          IM200420
      IF (M .EQ. L) GO TO 240                                           IM200430
      IF (J .EQ.30) GO TO 1000                                          IM200440
      J = J + 1                                                         IM200450
C     ********** FORM SHIFT **********                                  IM200460
      G = (D(L+1) - P) / (2.0 * E(L))                                   IM200470
      R = SQRT(G*G+1.0)                                                 IM200480
      G = D(M) - P + E(L) / (G + SIGN(R,G))                             IM200490
      S = 1.0                                                           IM200500
      C = 1.0                                                           IM200510
      P = 0.0                                                           IM200520
      NML = M - L                                                       IM200530
C     ********** FOR I=M-1 STEP -1 UNTIL L DO -- **********             IM200540
      DO 200 II = 1, NML                                                IM200550
      I = M - II                                                        IM200560
      F = S * E(I)                                                      IM200570
      B = C * E(I)                                                      IM200580
      IF (ABS(F) .LT. ABS(G)) GO TO 150                                 IM200590
      C = G / F                                                         IM200600
      R = SQRT(C*C+1.0)                                                 IM200610
      E(I+1) = F * R                                                    IM200620
      S = 1.0 / R                                                       IM200630
      C = C * S                                                         IM200640
      GO TO 160                                                         IM200650
  150 S = F / G                                                         IM200660
      R = SQRT(S*S+1.0)                                                 IM200670
      E(I+1) = G* R                                                     IM200680
      C = 1.0 / R                                                       IM200690
      S = S * C                                                         IM200700
  160 G = D(I+1) - P                                                    IM200710
      R = (D(I) - G) * S + 2.0 * C * B                                  IM200720
      P = S * R                                                         IM200730
      D(I+1) = G + P                                                    IM200740
      G = C * R - B                                                     IM200750
C     ********** FORM VECTOR **********                                 IM200760
      DO 180 K = 1, N                                                   IM200770
         F = Z(K,I+1)                                                   IM200780
         Z(K,I+1) = S * Z(K,I) + C * F                                  IM200790
         Z(K,I) = C * Z(K,I) - S * F                                    IM200800
  180 CONTINUE                                                          IM200810
C                                                                       IM200820
  200 CONTINUE                                                          IM200830
C                                                                       IM200840
      D(L) = D(L) - P                                                   IM200850
      E(L) = G                                                          IM200860
      E(M) = 0.0                                                        IM200870
      GO TO 105                                                         IM200880
  240 CONTINUE                                                          IM200890
C     ********** ORDER EIGENVALUES AND EIGENVECTORS **********          IM200900
      DO 300 II = 2, N                                                  IM200910
      I = II - 1                                                        IM200920
      K = I                                                             IM200930
      P = D(I)                                                          IM200940
C                                                                       IM200950
         DO 260 J = II, N                                               IM200960
      IF (D(J) .LE. P ) GO TO 260                                       IM200970
            K = J                                                       IM200980
            P = D(J)                                                    IM200990
  260    CONTINUE                                                       IM201000
C                                                                       IM201010
         IF (K .EQ. I) GO TO 300                                        IM201020
         D(K) = D(I)                                                    IM201030
         D(I) = P                                                       IM201040
C                                                                       IM201050
         DO 280 J = 1, N                                                IM201060
            P = Z(J,I)                                                  IM201070
            Z(J,I) = Z(J,K)                                             IM201080
            Z(J,K) = P                                                  IM201090
  280    CONTINUE                                                       IM201100
C                                                                       IM201110
  300 CONTINUE                                                          IM201120
C                                                                       IM201130
      GO TO 1001                                                        IM201140
C     ********** SET ERROR -- NO CONVERGENCE TO AN                      IM201150
C                EIGENVALUE AFTER 30 ITERATIONS **********              IM201160
 1000 IERR = L                                                          IM201170
 1001 RETURN                                                            IM201180
      END                                                               IM201190
      SUBROUTINE INITIA(DT,WT,DISP,DIST,W,V,NP,NDAT,NV)
C     ******************************************************************
C     *                                                                *
C     * I N I T I A                                                    *
C     *                                                                *
C     * INITIALIZATION OF VARIABLES                                    *
C     ******************************************************************
      DIMENSION DT(NP,NP),WT(NP,NP),DISP(NDAT),DIST(NDAT),W(NDAT)
      REAL*8 V(NV)
      L=0
      DO 10 J=1,NP
         DT(J,J)=0.
         WT(J,J)=0.
         V(NDAT+J)=0.
         IF (J.EQ.NP) GOTO 10
         IMIN=J+1
         DO 9 I=IMIN,NP
            L=L+1
            DT(I,J)=0.
            DT(J,I)=0.
            WT(I,J)=0.
            WT(J,I)=0.
            DIST(L)=0.0
            DISP(L)=0.0
               W(L)=0.0
               V(L)=0.0
  9      CONTINUE
 10   CONTINUE
      RETURN
      END
      SUBROUTINE NINPU(DATA,DISP,DIST,IORD,LBK,W,WT,DT,HULP,HELP,
     1                 DHELP,LAB,FORM1,FORM2,FORM4,
     2                 NDAT,NDAAT,NSY1,NSY2,NP,NR,NPNP,
     3                 FINR,VMIS,XSTAN,
     4                 IWEI,IMIS,IDIS,INDATA,INPARA,IPRI,IWRITE,ILEV,
     5                 IPRO,ILAB,INOR,ISYM,ICON)
C     ******************************************************************
C     *                                                                *
C     *  N I N P U                                                     *
C     *                                                                *
C     *  PURPOSE: READ, ACCUMULATE AND NORMALIZE DATA (AND WEIGHTS)    *
C     *           OPTIONALLY PRINT DATA (AND WEIGHTS)                  *
C     *                                                                *
C     *  SUBROUTINES CALLED: INPUN,WINPUN,VINPUN,VWINPU,RINPUN,RWINPU, *
C     *                      DTCARE                                    *
C     *                                                                *
C     *  AUTHOR: INEKE STOOP                                JUNE 1982  *
C     *                                                                *
C     ******************************************************************
      DIMENSION  DATA(NDAT),DISP(NDAT),DIST(NDAT),IORD(NSY1),LBK(NSY1),
     1           W(NDAT),WT(NP,NP),DT(NP,NP),HULP(NSY2),HELP(NSY2),
     2           DHELP(NP,NP),LAB(NP)
      CHARACTER*4 FORM4(2)
C     2           DHELP(NP,NP),LAB(NP)
      CHARACTER*80 FORM1,FORM2
      DOUBLE PRECISION LAB
      DATA IAPT /0/
C----------------------------------------------------------------------C
      IF (ILAB.GT.0) READ(ILAB,FORM4) (LAB(LL),LL=1,NP)
      INO=IABS(INOR)
         IF (IMIS.NE.1) IWM=IWEI
         IF (IMIS.EQ.1) IWM=1
         IF (IMIS.NE.1) IMIT=0
         IF (IMIS.EQ.1) IMIT=1
      IF (ICON.NE.0) GOTO 20
      IF (ISYM.EQ.1) GOTO 10
      IF (IWM.EQ.1) CALL WINPUN(DISP,DIST,HELP,IORD,LBK,FORM1,NR,NP,
     1                   NDAT,NDAAT,HULP,W,FORM2,IPRI,INDATA,IWRITE,
     2                   ILEV,IAPT,IDIS,IWEI,IMIT,VMIS,INO,ISYM)
      IF (IWM.EQ.0) CALL INPUN(DISP,DIST,HELP,HULP,IORD,LBK,FORM1,
     1                   NR,NP,NDAT,NDAAT,IPRI,INDATA,IWRITE,ILEV,
     2                   IAPT,IDIS,FINR,INO,ISYM)
      GOTO 30
 10   IF (IWM.EQ.1) CALL VWINPU(DISP,DHELP,DT,HELP,HULP,IORD,LBK,
     1                   HULP(NPNP+1),WT,W,FORM1,FORM2,
     2                   NR,NP,NPNP,NDAT,IPRI,INDATA,IWRITE,
     3                   ILEV,IAPT,IDIS,IWEI,IMIT,VMIS,INO)
      IF (IWM.EQ.0) CALL VINPUN(DISP,DHELP,DT,HELP,HULP,IORD,LBK,
     1                   WT,FORM1,NR,NP,NPNP,NDAT,IPRI,INDATA,
     2                   IWRITE,ILEV,IAPT,IDIS,FINR,INO)
      GOTO 30
 20   IF (IWM.EQ.1) CALL RWINPU(DISP,DHELP,DT,HELP,HULP,IORD,LBK,
     1                   HULP(NP+1),WT,W,FORM1,FORM2,NR,NP,NDAT,
     2                   IPRI,INDATA,IWRITE,ILEV,IAPT,IDIS,
     3                   ICON,ISYM,IWEI,IMIT,VMIS,INO,XSTAN)
      IF (IWM.EQ.0) CALL RINPUN(DISP,DHELP,DT,HELP,HULP,IORD,LBK,
     1                   WT,FORM1,NR,NP,NDAT,IPRI,INDATA,IWRITE,
     2                   ILEV,IAPT,IDIS,FINR,ICON,ISYM,INO,XSTAN)
 30   IWEI=IWM
      IF (ISYM.EQ.-1) ISYM=1
      IF (ILEV.GT.1.AND.((ICON.EQ.0.AND.IAPT.EQ.NR).OR.
     1   (ICON.NE.0.AND.IAPT.EQ.(NR*NP))))   IPRO=0
      CALL DTCARE(DISP,DATA,W,NDAT,NP,IPRI,IWRITE,IDIS,IWEI,IMIS,
     1                   FINR,VMIS)
      RETURN
      END
      SUBROUTINE INPUN(DATA,DISS,HELP,HULP,IORD,LBK,FORM1,
     1                 NR,NP,NDAT,NDAAT,
     2                 IPRI,INDATA,IWRITE,ILEV,IAPT,IDIS,FNR,INO,ISYM)
C     ******************************************************************
C     *                                                                *
C     *  I N P U N                                                     *
C     *                                                                *
C     *  PURPOSE: - READ THE DATA ACCORDING TO THE DATA SPECIFICATION  *
C     *             PARAMETERS                                         *
C     *           - PREPARE DATA MATRIX                                *
C     *                                                                *
C     *  SUBROUTINES CALLED: WRANK,PRITRI,SQNORW,VANORW                *
C     *                                                                *
C     ******************************************************************
      DIMENSION DATA(NDAT),DISS(NDAT),IORD(NDAT),LBK(NDAT),HELP(NDAAT),
     1          HULP(NDAT)
      CHARACTER*80 FORM1
C    1          HULP(NDAT),FORM1(20)
C----------------------------------------------------------------------C
      IWD=0
      DO 60 K=1,NR
         IF (ISYM.EQ.0) GOTO 10
            READ(INDATA,FORM1) (DISS(L),L=1,NDAT)
            GOTO 30
 10      LE=0
         DO 20 I=2,NP
            LB=LE+1
            LE=LE+I-1
            READ(INDATA,FORM1) (DISS(L),L=LB,LE)
 20      CONTINUE
 30      IF (IPRI.LT.K) GO TO 40
C----------------------------------------------------------------------C
C  PRINT THE ORIGINAL DATA                                             C
C----------------------------------------------------------------------C
            WRITE(IWRITE,1000) K
            CALL PRITRI(DISS,NP,NDAT,IWRITE)
C----------------------------------------------------------------------C
C  NORMALIZE ORIGINAL DATA                                             C
C----------------------------------------------------------------------C
 40      IF (INO.EQ.1) CALL SQNORW(DISS,HELP,NDAT,1.,0)
         IF (INO.EQ.2) CALL VANORW
     1                 (DISS,HELP,NDAT,1.,0,NDAT,ILEV,IWRITE)
C----------------------------------------------------------------------C
C IF NONMETRIC ITERATIONS HAVE TO BE PERFORMED, PREPARE IORD AND LBK   C
C----------------------------------------------------------------------C
         IF (ILEV.GT.1) CALL WRANK(DISS,HELP(1),HULP,HELP(NDAT+1),
     1                             IORD,LBK,NDAT,IAPT,IDIS,IWD)
         WRITE(2) (DISS(L),L=1,NDAT)
         DO 50 L=1,NDAT
             DATA(L)=DATA(L)+DISS(L)
 50      CONTINUE
 60   CONTINUE
C----------------------------------------------------------------------C
C  COMPUTE AGGREGATED DISPARITIES                                      C
C----------------------------------------------------------------------C
        FNR=1
      DO 70 L=1,NDAT
         DATA(L)=DATA(L)*FNR
 70   CONTINUE
      RETURN
 1000 FORMAT(///40H0ORIGINAL DATA MATRIX OF REPLICATION NO  ,I5)
      END
      SUBROUTINE KWAEX(DISP,W,N,IWEI,XX)
C     ******************************************************************
C     *                                                                *
C     *  K W A E X                                                     *
C     *                                                                *
C     *  PURPOSE:  NORMALIZATION SUM OF SQUARES EXPLICIT               *
C     *                                                                *
C     *  SUBROUTINES CALLED: SSQ                                       *
C     *                                                                *
C     *  AUTHORS: INEKE STOOP                  RELEASED: JUNE  1982    *
C     *                                                                *
C     ******************************************************************
      DIMENSION DISP(N),W(N)
C
      CALL SSQ(SK,DISP,W,N,IWEI)
         IF (SK.LE.1.0E-6) GOTO 20
      ALF=SQRT(XX/SK)
      DO 10 L=1,N
         DISP(L)=DISP(L)*ALF
 10   CONTINUE
      RETURN
 20   DO 30 L=1,N
         DISP(L)=0.
         IF (IWEI.EQ.1) W(L)=0.
 30   CONTINUE
      RETURN
      END
      SUBROUTINE KWAIM(DISP,DIST,W,N,IWEI,XX)
C     ******************************************************************
C     *                                                                *
C     *  K W A I M                                                     *
C     *                                                                *
C     *  PURPOSE:  NORMALIZATION SUM OF SQUARES IMPLICIT               *
C     *                                                                *
C     *  SUBROUTINES CALLED: SSQ                                       *
C     *                                                                *
C     *  AUTHORS: INEKE STOOP                  RELEASED: JUNE  1982    *
C     *                                                                *
C     ******************************************************************
      DIMENSION DISP(N),DIST(N),W(N)
C
      CALL SSQ(SK,DISP,W,N,IWEI)
      CALL SSQ(TSUM,DIST,W,N,IWEI)
         IF (SK.LE.1.0E-10.OR.TSUM.LE.1.0E-10) GOTO 20
      ALF=TSUM/SK
      TK=XX*SK/(TSUM*TSUM)
      DO 10 L=1,N
         DISP(L)=DISP(L)*ALF
         IF (IWEI.EQ.1) W(L)=W(L)*TK
         IF (IWEI.EQ.0) W(L)=TK
 10   CONTINUE
      RETURN
 20   DO 30 L=1,N
         DISP(L)=0.
         W(L)=0.
 30   CONTINUE
      RETURN
      END
      PROGRAM SMACOF
      EXTERNAL   SMASUN
      COMMON TITLE,FORM1,FORM2,VMIS,CRII,CRIP,
     1       NP,NR,ISYM,IWEI,IMIS,ILAB,
     2       IPRI,IPRJ,IPLO,IEIG,IHIS,IIND,ISHE,IPUN,
     3       INDATA,INPARA,IWRITE,ISTORE,
     4       NDMA,NDMI,MAXI,MAXP,IINI,IREL,ANUL,
     5       ICON,IPRO,IDIS,INOR,ILEV,
     6       NDAT,ND,NDAAT,NPNP,NV,NSY1,NSY2,NSY3,NSY4,N8
      CHARACTER*80 TITLE,FORM1,FORM2
C----------------------------------------------------------------------C
C  READER AND PRINTER SPECIFICATION; CHANGE HERE AND IN SUBROUTINE     C
C  DECLAR IF OTHER CONVENTIONS ARE USED ON YOUR MACHINE                C
C----------------------------------------------------------------------C
      INPARA=11
      IWRITE=12
      OPEN(11,FILE="smac.parm")
      OPEN(12,FILE="test.out")
C----------------------------------------------------------------------C
C  READ AND WRITE PARAMETER CARDS                                      C
C----------------------------------------------------------------------C
      READ(INPARA,503) TITLE
      READ(INPARA,500) NP,NR,ISYM,IWEI,IMIS,ILAB,VMIS
      READ(INPARA,501)
     1     IPRI,IPRJ,IPLO,IEIG,IHIS,IIND,ISHE,IPUN,INDATA,ISTORE
      READ(INPARA,502) NDMA,NDMI,MAXI,MAXP,IINI,IREL,CRII,CRIP,ANUL
      READ(INPARA,499) ICON,IPRO,IDIS,INOR,ILEV
      READ(INPARA,503) FORM1
      IF (IWEI.EQ.1) READ(INPARA,503) FORM2
C                                                                      C
      WRITE(IWRITE,600) TITLE
      WRITE(IWRITE,604)
      WRITE(IWRITE,605) NP,NR,ISYM,IWEI,IMIS,ILAB,VMIS,IPRI,
     1                  IPRJ,IPLO,IEIG,IHIS,IIND,ISHE,IPUN,INDATA,
     2                  ISTORE,NDMA,NDMI,MAXI,MAXP,IINI,IREL,CRII,CRIP,
     3                  ANUL,ICON,IPRO,IDIS,INOR,ILEV
C----------------------------------------------------------------------C
C  SET DEFAULT VALUES AND EXTRA PARAMETERS                             C
C----------------------------------------------------------------------C
      IF (NR.LE.0) NR=1
C----------------------------------------------------------------------C
      IF (ISYM.GT.1.OR.ISYM.LT.-1) ISYM=0
C----------------------------------------------------------------------C
      IF (IPRI.GT.NR) IPRI=NR
      IF (ISHE.GT.NR) ISHE=NR
C----------------------------------------------------------------------C
      IF (INDATA.LE.0) INDATA=INPARA
      IF (ISTORE.LE.0) ISTORE=IWRITE
      IF (ILAB  .LE.0) ILAB  =0
C----------------------------------------------------------------------C
      IF (NDMA.LE. 0)                 NDMA=2
      IF (NDMA.GT.NP)                 NDMA=NP
      IF (NDMI.LE. 0.OR.NDMI.GT.NDMA) NDMI=NDMA
C----------------------------------------------------------------------C
      IF (ILEV.NE.0.AND.ILEV.NE.1.AND.IPRO.LT.0) ILEV=0
      IF (ILEV.LT.0.OR.ILEV.GT.3) ILEV=0
      IF (ILEV.NE.2.OR.IDIS.NE.1) IDIS=0
      IF (ILEV.NE.2.AND.IMIS.EQ.-1) IMIS=1
      IF (IMIS.NE.1.AND.IMIS.NE.-1) IMIS=0
      IF (ILEV.LT.2) IPRO=-1
      IF (IPRO.GT.3) IPRO=2
C----------------------------------------------------------------------C
      IF (ILEV.LT.3)               MAXP= 0
      IF (ILEV.EQ.3.AND.MAXP.LE.0) MAXP=50
      IF (MAXI.LE.0)               MAXI=50
      IF (ILEV.LT.3)                     CRIP=0.0
      IF (CRIP.LT.1.0E-10.AND.ILEV.EQ.3) CRIP=1.0E-4
      IF (CRII.LT.1.0E-10)               CRII=1.0E-5
C----------------------------------------------------------------------C
      IF (ICON.NE. 1.AND.ICON.NE.-1) ICON=0
      IF (ISYM.NE. 1.AND.ICON.EQ.-1) ICON=1
C----------------------------------------------------------------------C
      IF (IIND.NE.1.AND.IIND.NE.2) IIND=0
      IF (IIND.EQ.2.AND.ICON.EQ.0) IIND=1
C----------------------------------------------------------------------C
      IF (INOR.NE. 1.AND.INOR.NE. 2.AND.INOR.NE.-2) INOR=-1
C----------------------------------------------------------------------C
         INO=IABS(INOR)
      IF (INO.EQ.2) IREL=2
      IF (ANUL.LE.1.0E-06.AND.INO.EQ.2) ANUL=3.
      IF (IREL.NE.1.AND.IREL.NE.2.OR.ANUL.LE.1.0E-6.AND.IREL.EQ.2)
     1              IREL=0
C----------------------------------------------------------------------C
      NPNP =NP*NP
      NDAAT=NPNP-NP
      NDAT =NDAAT/2
      ND   =NDMA
      NV   =NDAT+NP
      N8   =8*NP
          IF (ISYM.NE.1) NSY1=NDAT
                         NSY2=NPNP+NPNP
          IF (ISYM.NE.1.AND.ICON.EQ.0) NSY3=NDAAT
          IF (ISYM.EQ.1) NSY1=NPNP
          IF (ISYM.EQ.1.OR. ICON.NE.0) NSY3=NDAAT+NDAAT
          IF (ISYM.EQ.1.OR. ICON.NE.0) NSY4=NPNP+NPNP
          IF (ISYM.NE.1.AND.ICON.EQ.0) NSY4=NDAAT
          IF (ICON.NE.0) NSY1=NP
      IPON=IPUN-(IPUN/10)*10
C----------------------------------------------------------------------C
C  GIVE SUMMARY OF OPTIONS                                             C
C----------------------------------------------------------------------C
      WRITE(IWRITE,601) NP,NR
      IF (ISYM.NE.1) WRITE(IWRITE,602)
      IF (ISYM.EQ.1) WRITE(IWRITE,603)
      IF (IWEI.EQ.0) WRITE(IWRITE,606)
      IF (IWEI.NE.0) WRITE(IWRITE,607)
      IF (IMIS.EQ.1) WRITE(IWRITE,608) VMIS
      IF (ILAB.GT.0) WRITE(IWRITE,610) ILAB
      IF (ILEV.EQ.3) WRITE(IWRITE,609) NDMA,NDMI,MAXI,MAXP,CRII,CRIP
      IF (ILEV.NE.3) WRITE(IWRITE,709) NDMA,NDMI,MAXI,CRII
      IF (IREL.EQ.0) WRITE(IWRITE,400)
      IF (IREL.EQ.1) WRITE(IWRITE,410)
      IF (IREL.EQ.2) WRITE(IWRITE,420) ANUL
C----------------------------------------------------------------------C
      IF (ILEV.EQ.0) WRITE(IWRITE,350)
      IF (ILEV.EQ.1) WRITE(IWRITE,351)
      IF (ILEV.EQ.2) WRITE(IWRITE,352)
      IF (ILEV.EQ.3) WRITE(IWRITE,353)
C----------------------------------------------------------------------C
      IF (ICON.EQ.0) WRITE(IWRITE,310)
      IF (ICON.EQ.-1)WRITE(IWRITE,311)
      IF (ICON.EQ.1) WRITE(IWRITE,312)
C----------------------------------------------------------------------C
      IF (IPRO.EQ.0.AND.ILEV.GT.1) WRITE(IWRITE,321)
      IF (IPRO.EQ.1.AND.ILEV.GT.1) WRITE(IWRITE,322)
      IF (IPRO.EQ.2.AND.ILEV.GT.1) WRITE(IWRITE,323)
      IF (IPRO.EQ.3.AND.ILEV.GT.1) WRITE(IWRITE,324)
C----------------------------------------------------------------------C
      IF (IDIS.EQ.0) WRITE(IWRITE,330)
      IF (IDIS.EQ.1) WRITE(IWRITE,331)
C----------------------------------------------------------------------C
      IF (INOR.EQ.-2) WRITE(IWRITE,340)
      IF (INOR.EQ.-1) WRITE(IWRITE,341)
      IF (INOR.EQ. 1) WRITE(IWRITE,342)
      IF (INOR.EQ. 2) WRITE(IWRITE,343)
C----------------------------------------------------------------------C
      WRITE(IWRITE,612) INDATA,FORM1
      IF (IWEI.EQ.1) WRITE(IWRITE,613) INDATA,FORM2
      IF ((IPUN/10).EQ.1) WRITE(IWRITE,614) ISTORE
      IF (IPON.EQ.1.OR.IPON.EQ.3) WRITE(IWRITE,615) ISTORE
      IF (IPON.EQ.2.OR.IPON.EQ.3) WRITE(IWRITE,616) ISTORE
      WRITE(IWRITE,617)
C----------------------------------------------------------------------C
C  ALLOCATE STORAGE AND START COMPUTATIONS                             C
C----------------------------------------------------------------------C
      IF (ISYM.NE.1.AND.ICON.EQ.0) NWORDS=13*NP*NP+ 7*NP+3*NP*ND
      IF (ISYM.EQ.1.AND.ICON.EQ.0) NWORDS=16*NP*NP+ 8*NP+3*NP*ND
      IF (ISYM.NE.1.AND.ICON.NE.0) NWORDS=14*NP*NP+10*NP+3*NP*ND
      IF (ISYM.EQ.1.AND.ICON.NE.0) NWORDS=14*NP*NP+10*NP+3*NP*ND
      CALL DECLAR(SMASUN,A,NWORDS)
C      RETURN
C----------------------------------------------------------------------C
C  FORMAT SPECIFICATIONS                                               C
C----------------------------------------------------------------------C
 500  FORMAT(6I5,F10.4)
 501  FORMAT(10I5)
 502  FORMAT(6I5,2F10.8,F10.4)
 499  FORMAT(5I5)
 503  FORMAT(A80)
C
 600  FORMAT(1H1,21H   S M A C O F - I B ,86X,11HINEKE STOOP/
     1 1H ,46X,34HNONMETRIC MULTIDIMENSIONAL SCALING,26X,
     2     12HJAN DE LEEUW/
     3 1H ,14H   VERSION - 1,91X,13HWILLEM HEISER//
     4 1H ,11H   MAY 1982,88X,19HDEPT. OF DATATHEORY//
     5 1H ,94X,24HTHE UNIVERSITY OF LEIDEN/////
     6 12H0JOB TITLE: ,A80///)
 601  FORMAT(30H0DATA SPECIFICATIONS:            //
     1       45H0   THE NUMBER OF POINTS IS                  ,I5/
     2       45H0   THE NUMBER OF REPLICATIONS IS            ,I5)
 602  FORMAT(30H0   THE INPUT IS SYMMETRIC       )
 603  FORMAT(30H0   THE INPUT IS ASYMMETRIC      )
 604  FORMAT(25H0ECHO OF PARAMETER CARDS:)
 605  FORMAT(1H0,6I5,F10.4/
     1       1H ,10I5/
     2       1H ,6I5,2F10.8,F10.4/1H ,5I5/)
 606  FORMAT(30H0                NOT WEIGHTED    )
 607  FORMAT(30H0                WEIGHTED        )
 608  FORMAT(26H0   DATA VALUES LESS THAN ,F10.4,
     1       28H ARE INTERPRETED AS MISSING )
 609  FORMAT(/30H0ANALYSIS SPECIFICATIONS:       //
     1  51H0   THE MAXIMUM NUMBER OF DIMENSIONS IS            ,I5/
     2  51H0   THE MINIMUM NUMBER OF DIMENSIONS IS            ,I5/
     3  51H0   THE MAXIMUM NUMBER OF FINAL       ITERATIONS IS,I5/
     4  51H0   THE MAXIMUM NUMBER OF PRELIMINARY ITERATIONS IS,I5/
     5  46H0   THE FINAL       CONVERGENCE CRITERION IS  ,E10.2/
     6  46H0   THE PRELIMINARY CONVERGENCE CRITERION IS  ,E10.2)
 709  FORMAT(/30H0ANALYSIS SPECIFICATIONS:       //
     1  39H0   THE MAXIMUM NUMBER OF DIMENSIONS IS,I5/
     2  39H0   THE MINIMUM NUMBER OF DIMENSIONS IS,I5/
     3  39H0   THE MAXIMUM NUMBER OF ITERATIONS IS,I5/
     4  34H0   THE CONVERGENCE CRITERION IS  ,E10.2)
 400  FORMAT(28H0   RELAXED UPDATES ARE USED )
 410  FORMAT(31H0   GUTTMAN TRANSFORMS ARE USED)
 420  FORMAT(32H0   VALUE OF STEPSIZE PARAMETER ,F10.4)
 610  FORMAT(/30H0LABELS ARE READ FROM UNIT NR.,I3,
     1        17H WITH FORMAT 10A8)
 612  FORMAT(/36H0THE DATA WILL BE READ FROM UNIT NR.,I3,
     1        13H WITH FORMAT ,A80)
 613  FORMAT( 36H  WEIGHTS WILL BE READ FROM UNIT NR.,I3,
     1        13H WITH FORMAT ,A80)
 614  FORMAT(53H0THE INITIAL CONFIGURATION WILL BE STORED ON UNIT NR.,
     1       I3)
 615  FORMAT(51H0THE FINAL CONFIGURATION WILL BE STORED ON UNIT NR.,
     1       I3)
 616  FORMAT(53H0THE FINAL DISTANCE MATRIX WILL BE STORED ON UNIT NR.,
     1       I3)
 617  FORMAT(1H1)
 310  FORMAT(22H0   MATRIX CONDITIONAL)
 311  FORMAT(19H0   ROW CONDITIONAL)
 312  FORMAT(22H0   COLUMN CONDITIONAL)
 350  FORMAT(50H0   LINEAR REGRESSION, SCALE FACTOR               )
 351  FORMAT(50H0   LINEAR REGRESSION, SCALE FACTOR AND INTERCEPT )
 352  FORMAT(50H0   MONOTONE REGRESSION                           )
 353  FORMAT(50H0   MONOTONE REGRESSION, PRECEDED BY LIN.REGR.    )
 321  FORMAT(50H0   NONMETRIC ANALYSIS : NO TIES                  )
 322  FORMAT(50H0   NONMETRIC ANALYSIS : PRIMARY APPROACH         )
 323  FORMAT(50H0   NONMETRIC ANALYSIS : SECONDARY APPROACH       )
 324  FORMAT(50H0   NONMETRIC ANALYSIS : TERTIARY APPROACH        )
 330  FORMAT(50H0   ASCENDING REGRESSION (DISSIMILARITIES)        )
 331  FORMAT(50H0   DESCENDING REGRESSION (SIMILARITIES)          )
 340  FORMAT(32H0   STRESS FORMULA TWO, EXPLICIT)
 341  FORMAT(32H0   STRESS FORMULA ONE, EXPLICIT)
 342  FORMAT(32H0   STRESS FORMULA ONE, IMPLICIT)
 343  FORMAT(32H0   STRESS FORMULA TWO, IMPLICIT)
      END
      SUBROUTINE CONF1(X,CENT,NP,ND,FORM3,INDATA,IINI)
C     ******************************************************************
C     *                                                                *
C     *  C O N F 1                                                     *
C     *                                                                *
C     *  PURPOSE: READ AN INITIAL CONFIGURATION AND CENTER ITS AXES    *
C     *                                                                *
C     *  SUBROUTINES CALLED: RANDU                                     *
C     *                                                                *
C     ******************************************************************
      DIMENSION X(NP,ND),CENT(ND)
      CHARACTER*4 FORM3
C     DIMENSION X(NP,ND),CENT(ND),FORM3(1)
C                                                                      C
      STAND=0.0
      NPD=NP*ND
      IF (IINI.GT.0) CALL RANDU(X,NPD,IINI)
      IF (IINI.GT.0) GOTO 20
         DO 10 I=1,NP
            READ(INDATA,FORM3) (X(I,J),J=1,ND)
  10     CONTINUE
  20  DO 50 J=1,ND
         CSUM=0.0
         DO 40 I=1,NP
            CSUM=CSUM+X(I,J)
 40      CONTINUE
         CENT(J)=CSUM/NP
 50   CONTINUE
      DO 70 J=1,ND
         DO 60 I=1,NP
            X(I,J)=X(I,J)-CENT(J)
            STAND=STAND+X(I,J)*X(I,J)
 60      CONTINUE
 70   CONTINUE
      STAND=1.0/SQRT(STAND)
      DO 90 J=1,ND
         DO 80 I=1,NP
            X(I,J)=X(I,J)*STAND
 80       CONTINUE
 90   CONTINUE
      RETURN
      END
      SUBROUTINE DTCARE(DATA,DISS,W,NDAT,NP,
     1                  IPRI,IWRITE,IDIS,IWEI,IMIS,FNR,VMIS)
C     ******************************************************************
C     *                                                                *
C     *  D T C A R E                                                   *
C     *                                                                *
C     *  PURPOSE: - PRINTING AGGREGATED DATA AND WEIGHTS               *
C     *           - IF NECESSARY, MAKE DISSIMILARITIES FROM SIM.       *
C     *           - IF NECESSARY, ADD NUMBER TO MAKE ALL ELEMENTS POS. *
C     *           - ESTIMATE MISSING DISSIMILARITIES WITH THE TRIANGLE *
C     *             INEQUALITY                                         *
C     *                                                                *
C     *  SUBROUTINES CALLED: PRITRI , DISSIM , NEPLU                   *
C     *                                                                *
C     ******************************************************************
      DIMENSION DATA(NDAT),DISS(NDAT),W(NDAT)
      DATA GRDIS / 0.0 /
      REAL LUB
      INDEX(I,J)=J+(I*(I-3)+2)/2
C----------------------------------------------------------------------C
      IF (IPRI.LT.0) GOTO 20
C----------------------------------------------------------------------C
C PRINT AGGREGATED DATA                                                C
C----------------------------------------------------------------------C
         WRITE(IWRITE,100)
         CALL PRITRI(DATA,NP,NDAT,IWRITE)
         IF (IWEI.EQ.0) GOTO 20
            DO 10 L=1,NDAT
               DISS(L)=W(L)*FNR
 10         CONTINUE
            WRITE(IWRITE,200)
            CALL PRITRI(DISS,NP,NDAT,IWRITE)
C----------------------------------------------------------------------C
C  ADD NUMBER IF NEGATIVE DATA VALUES ARE ALLOWED                      C
C----------------------------------------------------------------------C
  20  IF (IMIS.EQ.-1) CALL NEPLU(DATA,NDAT)
C----------------------------------------------------------------------C
C  CHECK ACCUMULATED DATA FOR MISSING ELEMENTS                         C
C----------------------------------------------------------------------C
      DO 30 L=1,NDAT
         IF (DATA(L).GT.GRDIS) GRDIS=DATA(L)
 30   CONTINUE
      L=0
      DO 70 I=2,NP
         IMIN=I-1
         DO 60 J=1,IMIN
            L=L+1
            IF (W(L).GT.0.0.OR.IMIS.NE.1) GO TO 50
C----------------------------------------------------------------------C
C  FIND LUB AND GLB FOR MISSING ELEMENT                                C
C----------------------------------------------------------------------C
               LUB=GRDIS*2.0
               GLB=0.0
         DO 40 M=1,NP
            IF (M.EQ.I.OR.M.EQ.J) GO TO 40
               IM=INDEX(I,M)
               IF (I.LT.M) IM=INDEX(M,I)
               JM=INDEX(J,M)
               IF (J.LT.M) JM=INDEX(M,J)
               IF (DATA(IM).LE.VMIS.OR.DATA(JM).LE.VMIS) GO TO 40
               TUB=DATA(IM)+DATA(JM)
               IF (TUB.LT.LUB) LUB=TUB
               TLB=ABS(DATA(IM)-DATA(JM))
               IF (TLB.GT.GLB) GLB=TLB
 40      CONTINUE
         DATA(L)=(LUB+GLB)*0.5
 50      DISS(L)=DATA(L)
 60      CONTINUE
 70   CONTINUE
      IF (IDIS.EQ.1) CALL DISSIM(DATA,NDAT)
      RETURN
100   FORMAT(//25H0AGGREGATED DATA MATRIX    )
200   FORMAT(//25H0AGGREGATED WEIGHT MATRIX  )
      END
      SUBROUTINE NEPLU(DATA,N)
C     ******************************************************************
C     *                                                                *
C     *  N E P L U                                                     *
C     *                                                                *
C     *  PURPOSE: - ADD NUMBER TO NEGATIVE DISSIMIMILARITIES           *
C     *                                                                *
C     ******************************************************************
      DIMENSION DATA(N)
      DATA DMIN / 0.0 /
      DO 10 L=1,N
         IF (DATA(L).LT.DMIN) DMIN=DATA(L)
 10   CONTINUE
         IF (DMIN.GE.0.0) RETURN
         DMIN=1.1*DMIN
      DO 20 L=1,N
         DATA(L)=DATA(L)-DMIN
 20   CONTINUE
      RETURN
      END
      SUBROUTINE INITW(X,DATA,B,W,V,H1,H2,H3,H4,H5,H6,H7,H8,H9,
     1                 IND,FORM3,NP,ND,NDAT,NV,
     2                 IINI,IWEI,INDATA,IWRITE,INOR,DIST)
C ******************************************************************
C *                                                                *
C *  I N I T W                                                     *
C *                                                                *
C *  PURPOSE: COMPUTES AN INITIAL CONFIGURATION (YOUNG-HOUSEHOLDER-*
C *           TORGERSON METHOD) OR, IF IINI<1, READS ONE           *
C *           OR, IF IINI>1, COMPUTES A RANDOM CONFIGURATION ;     *
C *           NORMALIZES THE INITIAL CONFIGURATION AND, IN THE     *
C *           WEIGHTED CASE, COMPUTES THE NORMALIZING MATRIX V     *
C *                                                                *
C *  SUBROUTINES CALLED: CONF1,TRED1,IMTQL1,TINVIT,TRBAK1,DINVER   *
C *                      DISCOM,SSQ,SSVAR                          *
C *                                                                *
C ******************************************************************
      DIMENSION X(NP,ND),DATA(NDAT),B(NP,NP),W(NDAT),DIST(NDAT)
      DIMENSION H1(NP),H2(NP),H3(NP),H4(NP),H5(NP),H6(NP),H7(NP)
      DIMENSION H8(NP),H9(NP),IND(ND),FORM3(3)
      CHARACTER*4 FORM3
      REAL*8 V(NV)
C------------------------------------------------------------------C
      INO=IABS(INOR)
      IF (IWEI.EQ.1) CALL DINVER(W,V,NP,NDAT,NV,IWRITE)
      IF (IINI.EQ.0) GO TO 15
         CALL CONF1(X,H4,NP,ND,FORM3,INDATA,IINI)
         GOTO 30
 15   L=0
      TSUM=0.0
      DO 1 I=2,NP
         IMIN=I-1
         DO 2 J=1,IMIN
            L=L+1
            B(I,J)=DATA(L)*DATA(L)
            TSUM=TSUM+B(I,J)
 2          B(J,I)=B(I,J)
 1       CONTINUE
      TSUM=2.0*TSUM/(NP*NP)
C---------------------------------------------------------------C
C  PREPARE A DOUBLY CENTERED MATRIX OF INNER PRODUCTS           C
C---------------------------------------------------------------C
      DO 3 I=1,NP
         B(I,I)=0.0
         RSUM=0.0
         DO 4 J=1,NP
 4          RSUM=RSUM+B(I,J)
         RSUM=RSUM/NP
         H1(I)=RSUM
         DO 5 J=1,I
 5          B(I,J)=RSUM+H1(J)-B(I,J)-TSUM
 3       CONTINUE
C-------------------------------------------------------------------C
C  COMPUTE ALL EIGENVALUES AND THE FIRST ND EIGENVECTORS OF THE     C
C  INNER PRODUCT MATRIX                                             C
C-------------------------------------------------------------------C
      CALL TRED1(NP,NP,B,H1,H2,H3)
      DO 10 I=1,NP
         H4(I)=H1(I)
 10      H5(I)=H2(I)
      H3(1)=2.0
      CALL IMTQL1(NP,H4,H5,IERR)
      IF (IERR.NE.0) GO TO 990
C---------------------------------------------------------------C
C  THE EIGENVALUES ARE CONTAINED IN H4                          C
C  IND SHOULD CONTAIN INDICES FOR SUBMATRICES                   C
C---------------------------------------------------------------C
      DO 11 I=1,ND
 11      IND(I)=1
      CALL TINVIT(NP,NP,H1,H2,H3,ND,H4,IND,X,
     X            IERR,H5,H6,H7,H8,H9)
      IF (IERR.NE.0) GO TO 991
      CALL TRBAK1(NP,NP,B,H2,ND,X)
C-----------------------------------------------------------------C
C  THE EIGENVECTORS ARE NORMALIZED ACCORDING TO THEIR EIGENVALUES C
C-----------------------------------------------------------------C
      DO 20 K=1,ND
         STAND=(1.0/ND)*SQRT(AMAX1(0.0,H4(K)))
         DO 21 I=1,NP
 21         X(I,K)=X(I,K)*STAND
 20      CONTINUE
C-----------------------------------------------------------------C
C  NORMALIZE CONF. ACCORDING TO SUM OF SQUARES OR VARIANCE        C
C-----------------------------------------------------------------C
 30   CALL DISCOM(X,DIST,NDAT,NP,ND)
      IF (INO.EQ.2) CALL SSVAR(TSUM,TM,DIST,W,NDAT,IWEI,NDAT)
      IF (INO.EQ.1) CALL SSQ(TSUM,DIST,W,NDAT,IWEI)
      ALF=SQRT(1./TSUM)
      DO 50 K=1,NP
         IF (IINI.EQ.0)  H4(K)=H4(K)/TSUM
         IF (K.GT.ND) GOTO 50
         DO 40 I=1,NP
 40         X(I,K)=X(I,K)*ALF
 50      CONTINUE
         IF (IINI.EQ.0) WRITE(IWRITE,2001)
         IF (IINI.LT.0) WRITE(IWRITE,2000)
         IF (IINI.GT.0) WRITE(IWRITE,2002) IINI
 2000 FORMAT(///43H0INITIAL CONFIGURATION PROVIDED BY THE USER)
 2002 FORMAT(///46H0RANDOM INITIAL CONFIGURATION, STARTING VALUE ,I5)
 2001 FORMAT(///46H0INITIAL CONFIGURATION COMPUTED BY THE PROGRAM)
      RETURN
 990  WRITE(IWRITE,60)
 60   FORMAT(1H1,30HNO CONVERGENCE OF IMTQL1          ,
     1      /1H0,30HNO INITIAL CONFIGURATION FOUND    )
      STOP
 991  WRITE(IWRITE,61)
 61   FORMAT(1H1,30HNO CONVERGENCE OF TINVIT          ,
     1      /1H0,30HNO INITIAL CONFIGURATION FOUND    )
      STOP
      END
      SUBROUTINE LINEAD(DISP,DIST,W,NDAT,IWEI,INOR,Q)
C     ******************************************************************
C     *                                                                *
C     *  L I N E A D                                                   *
C     *                                                                *
C     *  PURPOSE: PERFORM (WEIGHTED) LINEAR REGRESSION                 *
C     *           (WITH ADDITIVE CONSTANT)                             *
C     *                                                                *
C     *  SUBROUTINES CALLED: SSVAR,COVAR,SSQ,COQ                       *
C     *                                                                *
C     *  AUTHOR: INEKE STOOP & J. VAN RIJCKEVORSEL          JUNE 1982  *
C     *                                                                *
C     ******************************************************************
      DIMENSION DISP(NDAT),DIST(NDAT),W(NDAT)
C----------------------------------------------------------------------C
      CALL SSVAR(SXX,XM,DISP,W,NDAT,IWEI,NDAT)
      CALL SSVAR(SYY,YM,DIST,W,NDAT,IWEI,NDAT)
      CALL COVAR(SXY,DIST,DISP,W,NDAT,IWEI,NDAT)
      IF (INOR.EQ.-2.OR.INOR.EQ.2) GOTO 300
C----------------------------------------------------------------------C
C  COMPUTATION NEW DISPARITIES                                         C
C----------------------------------------------------------------------C
 200  IF (SXX.GT.1.E-20) ALPHA=SXY/SXX
      BETA=YM-ALPHA*XM
C----------------------------------------------------------------------C
      DO 210 K=1,NDAT
         DISP(K)=ALPHA*DISP(K)+BETA
 210  CONTINUE
C----------------------------------------------------------------------C
      CALL SSQ(SXX,DISP,W,NDAT,IWEI)
      IF (SXX.LT.0.1E-06)  GOTO 900
 300     IF  (INOR.GT.0)  GOTO 400
      ALF=SQRT(Q/SXX)
         IF  (INOR.EQ.-2) GG=YM-ALF*XM
         IF  (INOR.EQ.-1) GG=0.
      DO 310 K=1,NDAT
         DISP(K)=ALF*DISP(K)+GG
 310  CONTINUE
      IF (IWEI.EQ.1) GOTO 650
      RETURN
C----------------------------------------------------------------------C
 400  IF (INOR.EQ.1) CALL SSQ(SYY,DIST,W,NDAT,IWEI)
      IF (INOR.EQ.1) CALL COQ(SXY,DIST,DISP,W,NDAT,IWEI)
      IF ((ABS(SXY)).LT.0.1E-10.OR.SYY.LT.0.1E-10) GOTO 900
C----------------------------------------------------------------------C
      ALF=SYY/SXY
      TK =(Q*SXY*SXY)/(SXX*SYY*SYY)
      IF (INOR.EQ.2) GG=YM-ALF*XM
      IF (INOR.EQ.1) GG=0.
C----------------------------------------------------------------------C
      DO 500 K=1,NDAT
         DISP(K)=ALF*DISP(K)+GG
         IF (IWEI.EQ.1) W(K)=TK*W(K)
         IF (IWEI.EQ.0) W(K)=TK
 500   CONTINUE
      IF (IWEI.EQ.0) RETURN
C----------------------------------------------------------------------C
 650  DO 660 K=1,NDAT
         IF (W(K).LE.1.0E-20) DISP(K)=0.
 660  CONTINUE
      RETURN
C----------------------------------------------------------------------C
 900  WRITE (6,1000)
 1000 FORMAT(29H NORMALIZATION APPROACH FAILS//
     1       24H USE OTHER NORMALIZATION)
      STOP
      END
      SUBROUTINE LINEMU(DISP,DIST,W,NDAT,IWEI,INOR,Q)
C     ******************************************************************
C     *                                                                *
C     *  L I N E M U                                                   *
C     *                                                                *
C     *  PURPOSE: PERFORM (WEIGHTED) LINEAR REGRESSION                 *
C     *           (WITHOUT ADDITIVE CONSTANT)                          *
C     *           NORMALIZE ACCORDING TO CHOSEN NORMALIZATION          *
C     *                                                                *
C     *  SUBROUTINES CALLED: SSQ,COQ,SSVAR                             *
C     *                                                                *
C     *  AUTHOR: INEKE STOOP & J. VAN RIJCKEVORSEL          JUNE 1982  *
C     *                                                                *
C     ******************************************************************
      DIMENSION DISP(NDAT),DIST(NDAT),W(NDAT)
      INTEGER YESFIX
      REAL COEFF
C----------------------------------------------------------------------C
      IF (INOR.LT.0) RETURN
      IF (INOR.EQ.1.OR.INOR.EQ.-1) CALL SSQ(SK,DISP,W,NDAT,IWEI)
      IF (INOR.EQ.2.OR.INOR.EQ.-2) CALL SSVAR
     1                             (SK,DM,DISP,W,NDAT,IWEI,NDAT)
      IF (SK.LT.0.1E-20) GOTO 900
      CALL SSQ(TSUM,DIST,W,NDAT,IWEI)
      CALL COQ(ST,DIST,DISP,W,NDAT,IWEI)
      IF ((ABS(ST)).LT.0.1E-06.OR.TSUM.LT.0.1E-06) GOTO 900
C----------------------------------------------------------------------C
      ALF=TSUM/ST
      OPEN(20,FILE="coeff.fix")
      READ(20,20201,END=6262) COEFF
      CLOSE(20)
06262 IF (ABS(COEFF) .GT. .00001) THEN
       ALF=COEFF
      END IF
      REWIND(13)
      OPEN(13,file="coeff")
      WRITE(13,20201) ALF
      CLOSE(13)
      TK =(Q*ST*ST)/(SK*TSUM*TSUM)
C----------------------------------------------------------------------C
      DO 110 K=1,NDAT
         DISP(K)=ALF*DISP(K)
         IF (IWEI.EQ.1) W(K)=TK*W(K)
         IF (IWEI.EQ.0) W(K)=TK
 110   CONTINUE
      RETURN
C----------------------------------------------------------------------C
 900  DO 910 K=1,NDAT
         DISP(K)=0.
            W(K)=0.
 910  CONTINUE
      OPEN(13,file="coeff")
      REWIND(13)
      WRITE(13,20201) ALF
      CLOSE(13)
      RETURN
20201 FORMAT(20F10.5)
20202 FORMAT(I2,20F10.5)
      END
      SUBROUTINE DFITV(X,DISP,HDISP,DIST,HELP,HULP,HDIS,IORD,LBK,SIG1,
     1                 SIG2,SIG3,SIG4,WT,W,WH,
     2                 NDAAT,NP,ND,NDAT,NR,NPNP,NSY1,NPNP2,IPRO,ILEV,
     3                 FNR,FPR,FNP,INOR,IWEI,IWHT,ICON,IWRITE,XSTAN)
C     ******************************************************************
C     *                                                                *
C     *  D F I T V                                                     *
C     *                                                                *
C     *  PURPOSE: COMPUTE EUCLIDEAN DISTANCES                          *
C     *           DETERMINE FIT MEASURES SIG1 SIG2 SIG3 SIG4           *
C     *           CALL WEIGHTED REGRESSION ROUTINE                     *
C     *           ASYMMETRIC MATRICES                                  *
C     *                                                                *
C     *  SUBROUTINES CALLED: REGRE,SET0,SETW,DISCOM,STRETC             *
C     *                                                                *
C     *  AUTHORS : INEKE STOOP                     RELEASED JULY 1982  *
C     *                                                                *
C     ******************************************************************
      DIMENSION  X(NP,ND),DISP(NDAT),DIST(NDAT),IORD(NSY1),LBK(NSY1),
     1           HELP(NP,NP),HULP(NPNP),W(NP,NP),WT(NP,NP),WH(NDAT),
     2           HDISP(NP,NP),HDIS(NP,NP)
C
      RHO = 0.0
      RHS = 0.0
      CALL SET0(HDISP,NPNP)
      IF (INOR.GT.0) CALL SET0(WT,NPNP)
      CALL DISCOM(X,DIST,NDAT,NP,ND)
      CALL STRETC(HDIS,DIST,NDAT,NP)
         IF (ILEV.EQ.2) REWIND 3
         IF (IWEI.EQ.1) REWIND 4
         IF (ILEV.LT.2) REWIND 2
      DO 100 K=1,NR
             IF (IWEI.EQ.0) CALL SETW(W,NPNP,NP)
             IF (IWEI.EQ.1) READ (4) ((W(I,J),I=1,NP),J=1,NP)
             IF (ILEV.LT.2) READ (2) ((HELP(I,J),I=1,NP),J=1,NP)
         IF (ICON.NE.0) GOTO 20
            CALL REGRE(HELP,DIST,W,IORD,LBK,HULP,HDIS,NDAT,NPNP,
     1                 NDAAT,NP,IPRO,ILEV,1,INOR,1,0,IWRITE,XSTAN)
         GOTO 40
 20      DO 30 J=1,NP
            CALL REGRE(HELP(1,J),HDIS(1,J),W(1,J),IORD,LBK,HULP(1),
     1                      HULP(NP+1),NP,NP,NP,NP,IPRO,ILEV,
     2                      1,INOR,0,ICON,IWRITE,XSTAN)
 30      CONTINUE
 40       DO 60 J=1,NP
             DO 50 I=1,NP
                IF (I.EQ.J) GOTO 50
                DD=HDIS(I,J)-HELP(I,J)
                  IF (IWHT.EQ.0) RHO=RHO+DD*DD
                  IF (IWHT.EQ.1) RHO=RHO+DD*DD*W(I,J)
                  IF (IWHT.EQ.1) HELP(I,J)=HELP(I,J)*W(I,J)
                  IF (INOR.GT.0)   WT(I,J)=  WT(I,J)+W(I,J)
                HDISP(I,J)=HDISP(I,J)+HELP(I,J)
 50         CONTINUE
 60      CONTINUE
 100  CONTINUE
      SIG1=RHO
      RHO=0.0
      IF (IWHT.EQ.0) GOTO 120
      L=0
      DO 110 J=2,NP
         IMIN=J-1
         DO 105 I=1,IMIN
            L=L+1
               IF (WT(I,J).GT.1.0E-6) FI=1./WT(I,J)
               IF (WT(I,J).LE.1.0E-6) FI=0.
               IF (WT(J,I).GT.1.0E-6) FJ=1./WT(J,I)
               IF (WT(J,I).LE.1.0E-6) FJ=0.
               WS=WT(I,J)+WT(J,I)
               WH(L)=.5*WS
               IF (WS.GT.1.0E-6) WX=1./WS
               IF (WS.LE.1.0E-6) WX=0.
            DISP(L)=(HDISP(I,J)+HDISP(J,I))*WX
            IF (FI.GT.0.AND.FJ.GT.0) DIFF=HDISP(I,J)*FI-HDISP(J,I)*FJ
            IF (FI.LE.0.OR .FJ.LE.0) DIFF=0.
            RHS=RHS+WT(I,J)*WT(J,I)*WX*DIFF*DIFF
            RHO=RHO+(DISP(L)-DIST(L))*(DISP(L)-DIST(L))*WH(L)
 105     CONTINUE
 110  CONTINUE
         SIG2= RHO*FNR
         SIG4= RHS*FNR*.5
      GOTO 140
 120  L=0
      DO 130 J=2,NP
         IMIN=J-1
         DO 125 I=1,IMIN
            L=L+1
            HDISP(I,J)=HDISP(I,J)*FNR
            HDISP(J,I)=HDISP(J,I)*FNR
            DISP(L)=.5*(HDISP(I,J)+HDISP(J,I))
               DIFF=HDISP(I,J)-HDISP(J,I)
            RHS=RHS+DIFF*DIFF
            RHO=RHO+(DISP(L)-DIST(L))*(DISP(L)-DIST(L))
 125     CONTINUE
 130  CONTINUE
         SIG2=RHO
         SIG4=RHS*.25
 140  SIG1=SIG1*FNR*.5
      SIG3=SIG1-(SIG2+SIG4)
      RETURN
      END
      SUBROUTINE DFITW(X,DISP,DIST,HELP,HULP,IORD,LBK,SIG1,SIG2,SIG3,
     1                 WT,W,NDAAT,NP,ND,NDAT,NR,IPRO,ILEV,
     2                 FNR,FPR,FNP,INOR,IWEI,IWHT,IWRITE,XSTAN)
C     ******************************************************************
C     *                                                                *
C     *  D F I T W                                                     *
C     *                                                                *
C     *  PURPOSE: COMPUTE EUCLIDEAN DISTANCES                          *
C     *           DETERMINE FIT MEASURES SIG1 SIG2 SIG3                *
C     *           CALL WEIGHTED REGRESSION ROUTINE                     *
C     *                                                                *
C     *  SUBROUTINES CALLED: REGRE,SET0,DISCOM                         *
C     *                                                                *
C     *  AUTHORS : INEKE STOOP                     RELEASED JULY 1982  *
C     *                                                                *
C     ******************************************************************
      DIMENSION  X(NP,ND),DISP(NDAT),DIST(NDAT),IORD(NDAT),LBK(NDAT),
     1           HELP(NDAT),HULP(NDAAT),W(NDAT),WT(NDAT)
C----------------------------------------------------------------------C
      RHO = 0.0
      CALL SET0(DISP,NDAT)
      IF (INOR.GT.0) CALL SET0(WT,NDAT)
      CALL DISCOM(X,DIST,NDAT,NP,ND)
         IF (ILEV.EQ.2) REWIND 3
         IF (IWEI.EQ.1) REWIND 4
         IF (ILEV.LT.2) REWIND 2
      DO 100 K=1,NR
         IF (IWEI.EQ.1) READ (4) (W(L),L=1,NDAT)
         IF (ILEV.LT.2) READ (2) (HELP(L),L=1,NDAT)
         CALL REGRE(HELP,DIST,W,IORD,LBK,HULP(1),HULP(NDAT+1),NDAT,
     1              NDAT,NDAT,NP,IPRO,ILEV,IWEI,INOR,0,0,IWRITE,XSTAN)
         DO 40 L=1,NDAT
            DD=HELP(L)-DIST(L)
            IF (IWHT.EQ.1) RHO=RHO+DD*DD*W(L)
            IF (IWHT.EQ.0) RHO=RHO+DD*DD
            IF (IWHT.EQ.1) HELP(L)=HELP(L)*W(L)
            DISP(L)=DISP(L)+HELP(L)
            IF (INOR.GT.0) WT(L)=WT(L)+W(L)
 40      CONTINUE
 100  CONTINUE
      SIG1=RHO
      RHO=0.0
      IF (IWHT.EQ.0) GOTO 120
      DO 110 L=1,NDAT
         FX=1./WT(L)
         DISP(L)=DISP(L)*FX
         RHO=RHO+(DISP(L)-DIST(L))*(DISP(L)-DIST(L))*WT(L)
 110  CONTINUE
      SIG2=RHO*FNR
      GOTO 140
 120  DO 130 L=1,NDAT
         DISP(L)=DISP(L)*FNR
         RHO=RHO+(DISP(L)-DIST(L))*(DISP(L)-DIST(L))
 130  CONTINUE
      SIG2=RHO
 140  SIG1=SIG1*FNR
      SIG3=SIG1-SIG2
      RETURN
      END
      SUBROUTINE STRIND(DIST,HELP,HULP,IORD,LBK,HDIS,W,NDAT,NDAAT,NPNP,
     1                  NSY1,NPNP2,IPRO,ILEV,IWRITE,NP,NR,FNP,
     2                  SIG,INOR,IWEI,ISYM,ICON,SS,H18,N8,XX,IIND)
C     ******************************************************************
C     *                                                                *
C     *  S T R I N D                                                   *
C     *                                                                *
C     *  PURPOSE: PRINT CONTRIBUTION TO TOTAL STRESS OF EACH           *
C     *           REPLICATION                                          *
C     *                                                                *
C     *  SUBROUTINES CALLED: REGRE, STRETC, SSQ, SET0, SETW            *
C     *                                                                *
C     ******************************************************************
      DIMENSION HELP(NPNP2),HULP(NPNP),IORD(NSY1),LBK(NSY1),
     1          DIST(NDAT),W(NPNP),HDIS(NPNP),SS(NP),H18(NP,N8)
      LOGICAL LEND
C----------------------------------------------------------------------C
      LEND=.FALSE.
      IF (INOR.LT.0.AND.ISYM.EQ.0.AND.ICON.EQ.0) IWHT=IWEI
      IF (INOR.GT.0.OR. ISYM.EQ.1.OR. ICON.NE.0) IWHT=1
      IF (ISYM.EQ.1.OR.ICON.NE.0) CALL STRETC(HDIS,DIST,NDAT,NP)
         CALL SET0(SS,NP)
      IF (ISYM.EQ.1.OR .ICON.NE.0) FF=0.5/FLOAT(NR)
      IF (ISYM.EQ.0.AND.ICON.EQ.0) FF=1.0/FLOAT(NR)
      NPL=NR/8
      NRE=NR-8*NPL
         IF (NRE.GT.0) NPL=NPL+1
         IF (NRE.EQ.0) NRE=8
      IF (NR.GT.1) WRITE(IWRITE,500)
   5  IF (ILEV.EQ.2) REWIND 3
      IF (ILEV.LT.2) REWIND 2
      IF (IWEI.EQ.1) REWIND 4
         K=0
      DO 100 KN=1,NPL
         NQ=MIN0(8,(NR-8*(KN-1)))
         NS=(8*(KN-1))+1
         NT=NS+NQ-1
         IF (NR.EQ.1.OR.IIND.EQ.1) GOTO 10
         IF (LEND.AND.ICON.EQ.-1.AND.KN.EQ.1)
     1         WRITE (IWRITE,400) (KL,KL=NS,NT)
         IF (LEND.AND.ICON.EQ.-1.AND.KN.GT.1)
     1         WRITE (IWRITE,450) (KL,KL=NS,NT)
         IF (LEND.AND.ICON.EQ. 1.AND.KN.EQ.1)
     1         WRITE (IWRITE,300) (KL,KL=NS,NT)
         IF (LEND.AND.ICON.EQ. 1.AND.KN.GT.1)
     1         WRITE (IWRITE,350) (KL,KL=NS,NT)
         IF (LEND) WRITE(IWRITE,800)
 10      IF (KN.LT.NPL) NPN=8
         IF (KN.EQ.NPL) NPN=NRE
         DO 90 KM=1,NPN
            K=K+1
            IF (IWEI.EQ.1) READ(4) (W(L),L=1,NPNP)
            IF (IWEI.EQ.0.AND.(ICON.NE.0.OR.ISYM.EQ.1))
     2                     CALL SETW(W,NPNP,NP)
            IF (ILEV.LT.2) READ(2) (HULP(L),L=1,NPNP)
            SIGI=0.
C---------------------------------------------------------------------C
C  COMPUTE FINAL DISPARITIES FOR THE I'TH REPLICATION                 C
C---------------------------------------------------------------------C
            IF (ICON.NE.0) GOTO 15
            IF (ISYM.EQ.1) CALL REGRE(HULP,DIST,W,IORD,LBK,HELP,HDIS,
     1                                NDAT,NPNP,NDAAT,NP,IPRO,ILEV,
     2                                1,INOR,ISYM,ICON,IWRITE,XX)
            IF (ISYM.EQ.0) CALL REGRE(HULP,DIST,W,IORD,LBK,HELP(1),
     1                          HELP(NDAT+1),NDAT,NDAT,NDAT,NP,IPRO,
     2                          ILEV,IWEI,INOR,ISYM,ICON,IWRITE,XX)
                        GOTO 30
C---------------------------------------------------------------------C
  15        DO 20 J=1,NP
               L=1+NP*(J-1)
               CALL REGRE(HULP(L),HDIS(L),W(L),IORD,LBK,HELP(1),
     1                    HELP(NP+1),NP,NP,NP,NP,IPRO,ILEV,
     2                    1,INOR,ISYM,ICON,IWRITE,XX)
  20        CONTINUE
C---------------------------------------------------------------------C
C  COMPUTE STRESS OF THE I'TH REPLICATION                             C
C---------------------------------------------------------------------C
  30        IF (LEND.OR.NR.EQ.1) GOTO 45
            DO 40 L=1,NPNP
               DD=HDIS(L)-HULP(L)
               IF (IWHT.EQ.0) SIGI=SIGI+DD*DD
               IF (IWHT.EQ.1) SIGI=SIGI+DD*DD*W(L)
  40        CONTINUE
            SIGI=SIGI*FF
            PSIG=100*SIGI/SIG
            WRITE(IWRITE,550) K,SIGI,PSIG
            IF (.NOT.LEND) GOTO 90
  45           IF (ICON.EQ.-1) CALL REVERS(HULP,NP,NP)
               IF (ICON.EQ.-1) CALL REVERS(   W,NP,NP)
            L=0
            DO 60 J=1,NP
               SRC=0.
               DO 50 I=1,NP
                  L=L+1
                  IF (I.EQ.J) GOTO 50
                  DD=HDIS(L)-HULP(L)
                  IF (IWHT.EQ.0) SRC=SRC+DD*DD
                  IF (IWHT.EQ.1) SRC=SRC+DD*DD*W(L)
  50           CONTINUE
               SS(J)=SS(J)+SRC
               H18(J,KM)=SRC*FF
               PSIG=100*H18(J,KM)/SIG
  60        CONTINUE
  90     CONTINUE
         IF (.NOT.LEND.OR.NR.EQ.1.OR.IIND.EQ.1) GOTO 100
         DO 95  I=1,NP
            WRITE(IWRITE,650) I,(H18(I,J),J=1,NPN)
  95     CONTINUE
 100  CONTINUE
      IF (LEND.OR.NR.EQ.1) GOTO 110
      IF (ICON.NE.0) LEND=.TRUE.
      IF (.NOT.LEND) RETURN
                     GOTO 5
 110  IF (ICON.EQ.-1) WRITE(IWRITE,600)
      IF (ICON.EQ. 1) WRITE(IWRITE,700)
      DO 200 L=1,NP
         SS(L)=SS(L)*FF
         PSIG=100*SS(L)/SIG
         WRITE(IWRITE,550) L,SS(L),PSIG
 200  CONTINUE
      RETURN
 300  FORMAT(1H1,46HCONTRIBUTION TO STRESS OF EACH COLUMN OF EACH ,
     1       11HREPLICATION,///2X,12HCOLUMN/REPL.,I6,7I12)
 350  FORMAT(///2X,12HCOLUMN/REPL.,I6,7I12)
 400  FORMAT(1H1,43HCONTRIBUTION TO STRESS OF EACH ROW OF EACH ,
     1       11HREPLICATION,///,8X,9HROW/REPL.,I6,7I12)
 450  FORMAT(///8X,9HROW/REPL.,I6,7I12)
 500  FORMAT(1H1,42HCONTRIBUTION TO STRESS OF EACH REPLICATION,///,
     1       5X,37H  REPLICATION  STRESS      PERCENTAGE,//)
 550  FORMAT(1H0,5X,I5,5X,F12.8,5X,F5.2)
 650  FORMAT(5X,I5,5X,8F12.8)
 600  FORMAT(1H1,34HCONTRIBUTION TO STRESS OF EACH ROW,///,
     1       5X,37H  ROW          STRESS      PERCENTAGE,//)
 700  FORMAT(1H1,37HCONTRIBUTION TO STRESS OF EACH COLUMN,///,
     1       5X,37H  COLUMN       STRESS      PERCENTAGE,//)
 800  FORMAT(///)
      END
      SUBROUTINE PRITEX(X,M,N,MM,LAB,IWRITE,ILAB)
C     ******************************************************************
C     *                                                                *
C     *  P R I T E X                                                   *
C     *                                                                *
C     *  PURPOSE: PRINTS A M*N ARRAY WITH ROW- AND COLUMN IDENTIFI-    *
C     *           CATION, EACH LINE CONTAINING AT MOST 11 VALUES (F9.3)*
C     *                                                                *
C     *  SUBROUTINES CALLED: NONE                                      *
C     *                                                                *
C     ******************************************************************
      DIMENSION X(MM,N),LAB(MM)
      DOUBLE PRECISION LAB
C----------------------------------------------------------------------C
C  CHECK HOW MANY TIMES THE NUMBER OF COLUMNS EXCEEDS 11               C
C----------------------------------------------------------------------C
      NTRU=N/11
      NREM=N-NTRU*11
      IF (NREM.GT.0) GO TO 1
         NREM=11
         NTRU=NTRU-1
 1    NREP=NTRU+1
C----------------------------------------------------------------------C
C  PRINT NREP TIMES A BLOCK OF ELEMENTS                                C
C----------------------------------------------------------------------C
      NK=11
      KB=1
      DO 2 I=1,NREP
         IF (I.EQ.NREP) NK=NREM
         KE=KB+NK-1
         IF (ILAB.GT.0) WRITE(IWRITE,60) (K,K=KB,KE)
         IF (ILAB.LE.0) WRITE(IWRITE,59) (K,K=KB,KE)
         WRITE(IWRITE,61)
         DO 3 L=1,M
            IF (ILAB.GT.1) WRITE(IWRITE,62) L,LAB(L),(X(L,K),K=KB,KE)
            IF (ILAB.LE.1) WRITE(IWRITE,63) L,(X(L,K),K=KB,KE)
 3       CONTINUE
         KB=KB+NK
 2    CONTINUE
      RETURN
 59   FORMAT(1H0,6X,11I9)
 60   FORMAT(1H0,14X,11I9)
 61   FORMAT(1H )
 62   FORMAT(1H ,I5,2X,A8,2X,11F9.3)
 63   FORMAT(1H ,I5,4X,11F9.3)
      END
      SUBROUTINE REGRE(DISP,DIST,W,IORD,LBK,HEL,HUL,NDAT,NPNP,NDAAT,
     1                  NP,IPRO,ILEV,IWEI,INOR,ISYM,ICON,IWRITE,XX)
C     ******************************************************************
C     *                                                                *
C     *  R E G R E                                                     *
C     *                                                                *
C     *  PURPOSE:  MONITOR FOR WEIGHTED REGRESSION                     *
C     *                                                                *
C     *  SUBROUTINES CALLED: WMRMNH,WPRAR,WSCAR,WTEAR,STRETC,          *
C     *                      KWAEX,KWAIM,VAREX,VARIM,LINEAD,LINEMU     *
C     *                                                                *
C     *  AUTHORS: INEKE STOOP                   RELEASED: JULY 1982    *
C     *                                                                *
C     ******************************************************************
      DIMENSION DISP(NPNP),DIST(NDAT),IORD(NPNP),LBK(NPNP),
     1          W(NPNP),HEL(NPNP),HUL(NPNP)
      IF (ILEV.NE.2) GOTO 100
         NW=NDAAT
C----------------------------------------------------------------------C
C  MONOTONE REGRESSION PRELIMINARIES                                   C
C----------------------------------------------------------------------C
C  PUT DISTANCES AND WEIGHTS IN THE ORDER PRESCRIBED BY IORD(K)        C
C----------------------------------------------------------------------C
         READ(3) (IORD(L),L=1,NPNP),(LBK(L),L=1,NPNP)
         DO 10 L=1,NDAAT
            IF (ISYM.EQ.1.AND.ICON.EQ.0) HEL(L)= HUL(IORD(L))
            IF (ISYM.EQ.0.OR. ICON.NE.0) HEL(L)=DIST(IORD(L))
 10      CONTINUE
         DO 25 L=1,NDAAT
            IF (IWEI.NE.1) GOTO 20
               HUL(L)=W(IORD(L))
               IF (HUL(L).GT.1.0E-06) GO TO 25
                  NW=L-1
                  GOTO 30
 20         HUL(L)=1.0
 25      CONTINUE
 30      IF (NW.EQ.1) GOTO 35
         IF (NW.EQ.0) GOTO 45
C----------------------------------------------------------------------C
C  SELECT APPROACH TO TIES (NO TIES IF IPRO=0)                         C
C----------------------------------------------------------------------C
         IF (IPRO.EQ.0) CALL WMRMNH(         HEL,HUL,NW)
         IF (IPRO.EQ.1) CALL WPRAR (IORD,LBK,HEL,HUL,NW)
         IF (IPRO.EQ.2) CALL WSCAR (     LBK,HEL,HUL,NW)
         IF (IPRO.EQ.3) CALL WTEAR (     LBK,HEL,HUL,NW)
C----------------------------------------------------------------------C
C STORE THE VALUES IN THEIR APPROPRIATE PLACES IN DISP                 C
C----------------------------------------------------------------------C
 35      DO 40 L=1,NW
            DISP(IORD(L))=HEL(L)
 40      CONTINUE
            IF (NW.EQ.NPNP) GOTO 60
 45      NT=NW+1
         DO 50 L=NT,NPNP
            DISP(IORD(L))=0.
 50      CONTINUE
 60   IF (ISYM.EQ.1.AND.ICON.EQ.0) CALL STRETC(HUL,DIST,NDAT,NP)
C----------------------------------------------------------------------C
C CALL APPROPRIATE NORMALIZATION ROUTINE                               C
C----------------------------------------------------------------------C
         IF (INOR.GT.0) GOTO 70
      IF (INOR.EQ.-1) CALL KWAEX(DISP,W,NPNP,IWEI,XX)
      IF (INOR.EQ.-2) CALL VAREX(DISP,W,NPNP,IWEI,NDAAT,XX,IWRITE)
         GOTO 85
 70      IF (ISYM.EQ.1.AND.ICON.EQ.0) GOTO 80
      IF (INOR.EQ.1) CALL KWAIM(DISP,DIST,W,NPNP,IWEI,XX)
      IF (INOR.EQ.2) CALL VARIM(DISP,DIST,W,NPNP,IWEI,NDAAT,XX,IWRITE)
         GOTO 85
 80   IF (INOR.EQ.1) CALL KWAIM(DISP,HUL,W,NPNP,IWEI,XX)
      IF (INOR.EQ.2) CALL VARIM(DISP,HUL,W,NPNP,IWEI,NDAAT,XX,IWRITE)
 85      IF (IWEI.EQ.0) RETURN
            DO 90 L=1,NPNP
               IF (W(L).LT.1.0E-16) DISP(L)=0.
 90         CONTINUE
            RETURN
C----------------------------------------------------------------------C
C  LINEAR REGRESSION                                                   C
C----------------------------------------------------------------------C
 100     DO 110 L=1,NPNP
            IF (ISYM.EQ.1.AND.ICON.EQ.0) HEL(L)= HUL(L)
            IF (ISYM.EQ.0.OR. ICON.NE.0) HEL(L)=DIST(L)
 110     CONTINUE
         IF (ILEV.EQ.0) CALL LINEMU(DISP,HEL,W,NPNP,IWEI,INOR,XX)
         IF (ILEV.EQ.1) CALL LINEAD(DISP,HEL,W,NPNP,IWEI,INOR,XX)
      RETURN
      END
      SUBROUTINE RLAXN(X0,Y,X1,DISP,DIST,W,HELP,ITER,IWEI,IWRITE,
     1                 SIG,ANUL,AHAT,CRIT,FNR,NR,NC,NDAT,NSTEP)
C     ******************************************************************
C     *                                                                *
C     *  R L A X N                                                     *
C     *                                                                *
C     * PURPUSE : COMPUTE UPDATE , DETERMINED BY STEPSIZE PARAMETER    *
C     *           AHAT                                                 *
C     *           IF NEW STRESS LARGER THAN FORMER STRESS,             *
C     *           COMPUTE NEW VALUE OF AHAT AND REPEAT PROCEDURE       *
C     *                                                                *
C     * SUBROUTINES CALLED : DEVFM                                     *
C     *                                                                *
C     * AUTHOR : INEKE STOOP                             APRIL 1982    *
C     *                                                                *
C     ******************************************************************
      DIMENSION DIST(NDAT),DISP(NDAT),W(NDAT),HELP(NC)
      DIMENSION X0(NR,NC),X1(NR,NC),Y(NR,NC)
C----------------------------------------------------------------------C
      ISOS=0
      NSTEP=0
      IF (ITER.NE.1) GOTO 30
         DO 20 K=1,NC
            DO 10 I=1,NR
               X0(I,K)=Y(I,K)
 10         CONTINUE
 20      CONTINUE
      RETURN
C----------------------------------------------------------------------C
 30   AHAT=ANUL
      CALL DEVFM(X0,NC,NR,HELP)
 40   NSTEP=NSTEP+1
      DO 60 K=1,NC
         DO 50 I=1,NR
            X1(I,K)=X0(I,K)-AHAT*(X0(I,K)-Y(I,K))
 50      CONTINUE
 60   CONTINUE
      SIG1=0.0
      L=0
      DO 90 I=2,NR
         IMIN=I-1
         DO 80 J=1,IMIN
            L=L+1
            DA=0.0
            DO 70 K=1,NC
               DIFF=X1(I,K)-X1(J,K)
               DA=DA+DIFF*DIFF
 70         CONTINUE
            DIST(L)=SQRT(DA)
            DD = DISP(L)-DIST(L)
            IF (IWEI.EQ.1) SIG1 = SIG1 + DD * DD * W(L)
            IF (IWEI.EQ.0) SIG1 = SIG1 + DD * DD
 80      CONTINUE
 90   CONTINUE
      IF (IWEI.EQ.1) SIG1=SIG1*FNR
      SDIF=SIG1-SIG
      IF (SDIF.GT.1.0E-20) GOTO 120
         DO 110 K=1,NC
            DO 100 I=1,NR
               X0(I,K)=X1(I,K)
 100        CONTINUE
 110     CONTINUE
      RETURN
C----------------------------------------------------------------------C
 120  L=0
      DO 140 I=1,NR
         DO 130 K=1,NC
            X1(I,K)=X0(I,K)-Y(I,K)
 130     CONTINUE
 140  CONTINUE
      HXY=0.
      L=0
      DO 170 I=2,NR
         IMIN=I-1
         DO 160 J=1,IMIN
            L=L+1
            DA=0.0
            DO 150 K=1,NC
               DIFF=X1(I,K)-X1(J,K)
               DA=DA+DIFF*DIFF
 150        CONTINUE
            IF (IWEI.EQ.1) HXY=HXY+DA*W(L)
            IF (IWEI.EQ.0) HXY=HXY+DA
 160        CONTINUE
 170  CONTINUE
      IF (IWEI.EQ.1) HXY=HXY*FNR
      HXY=HXY+HXY
      AHAT=.5*AHAT*AHAT*HXY/(SDIF+AHAT*HXY)
      IF (AHAT.LE.1.0E-04) GOTO 200
      GOTO 40
C----------------------------------------------------------------------C
 200  IF (ISOS.EQ.1) GOTO 210
         ISOS=1
      WRITE(IWRITE,998)
 998  FORMAT(49H STEPSIZE PROCEDURE PROBLEMS,RESTART WITH ANUL=1.)
         GOTO 40
 210  WRITE(IWRITE,999)
 999  FORMAT(31H STEPSIZE PROCEDURE BREAKS DOWN)
      STOP
      END
      SUBROUTINE SHEL10(DIS,W,IND,N,NT,IDIS,IWEI)
C     ******************************************************************
C     *                                                                *
C     *     SORT DIS IN DESCENDING OR ASCENDING ORDER                  *
C     *     PUT IND IN THE SAME ORDER                                  *
C     *     IF W(I) EQUALS 0, DIS IS PLACED LAST IN THE ARRAY          *
C     *                                                                *
C     ******************************************************************
      DIMENSION DIS(N) ,IND(N) ,W(N)
      IF (IWEI.EQ.0) GOTO 50
C----------------------------------------------------------------------C
C    IF WEIGHT EQUALS 0, DIS IS PLACED LAST IN THE ARRAY               C
C----------------------------------------------------------------------C
         INS=N
         DO 10 I=1,N
            IF (W(INS).GT.1.0E-06) GOTO 20
               INS=INS-1
  10     CONTINUE
  20     NT=INS
         IF (NT.LE.1) RETURN
  30        INS=INS-1
            IF (W(INS).GT.0) GOTO 40
               DD=DIS(INS)
               ID=IND(INS)
                  DIS(INS)=DIS(NT)
                  IND(INS)=IND(NT)
                    W(INS)=  W(NT)
               DIS(NT)=DD
               IND(NT)=ID
                 W(NT)=0.
               NT=NT-1
  40        IF (INS.GE.2) GO TO 30
         IF (NT.EQ.1) RETURN
C----------------------------------------------------------------------C
C    SORT ELEMENTS OF DISP WITH WEIGHT NOT EQUAL TO 0.                 C
C----------------------------------------------------------------------C
  50  IF (IWEI.EQ.0) NT=N
      M=NT
  60  M=M/2
         IF (M.LT.1) RETURN
         K=NT-M
         J=1
  70     I=J
  80        L=I+M
            IF (DIS(L).GE.DIS(I).AND.IDIS.EQ.0.OR.
     1          DIS(L).LE.DIS(I).AND.IDIS.EQ.1) GOTO 100
C----------------------------------------------------------------------C
C   ELEMENTS I AND L OF DIS, W AND IND CHANGE POSITION                 C
C----------------------------------------------------------------------C
               DX     = DIS(I)
               DIS(I) = DIS(L)
               DIS(L) = DX
                  IX     = IND(I)
                  IND(I) = IND(L)
                  IND(L) = IX
               IF (IWEI.EQ.0) GOTO 90
                  WX   = W(I)
                  W(I) = W(L)
                  W(L)= WX
C----------------------------------------------------------------------C
  90           I=I-M
            IF (I.GE.1) GOTO 80
 100     J=J+1
      IF (J-K) 70,70,60
      END
      SUBROUTINE SMASUN(A,NWORDS)
C     ******************************************************************
C     *                                                                *
C     *  S M A S U N                                                   *
C     *                                                                *
C     *  PURPOSE: COMPUTES THE RELATIVE ADDRESSES FOR ALL ARRAYS IN    *
C     *           THE REST OF THE PROGRAM                              *
C     *                                                                *
C     *  SUBROUTINES CALLED: SMASHN                                    *
C     *                                                                *
C     ******************************************************************
      DIMENSION  A(NWORDS)
      COMMON TITLE,FORM1,FORM2,VMIS,CRII,CRIP,
     1       NP,NR,ISYM,IWEI,IMIS,ILAB,
     2       IPRI,IPRJ,IPLO,IEIG,IHIS,IIND,ISHE,IPUN,
     3       INDATA,INPARA,IWRITE,ISTORE,
     4       NDMA,NDMI,MAXI,MAXP,IINI,IREL,ANUL,
     5       ICON,IPRO,IDIS,INOR,ILEV,
     6       NDAT,ND,NDAAT,NPNP,NV,NSY1,NSY2,NSY3,NSY4,N8
      CHARACTER*80 TITLE,FORM1,FORM2
C
      DOUBLE PRECISION LAB(NP),V(NV)
      INTEGER IORD(NSY1),LBK(NSY1),IND(NSY3)
      INAM=1
      IHE=INAM+NP+NP
      IHU=IHE+NSY2
      IDH=IHU+NSY2
      IWS=IDH+NPNP
      IH1=IWS+NSY4
      IH2=IH1+NP
      IH3=IH2+NP
      IH4=IH3+NP
      IH5=IH4+NP
      IH6=IH5+NP
      IH7=IH6+NP
      IH8=IH7+NP
      IH9=IH8+NP
      IDA=IH9+NP
      IDT=IDA+NDAT
      IDP=IDT+NDAT
      IOR=IDP+NDAT
      ILB=IOR+NSY1
      IX=ILB+NSY1
      IY=IX+NP*ND
      IXY=IY+NP*ND
      IIN=IXY+NP*ND
      IW =IIN+NSY3
      ITD=IW +NDAT
      ITW=ITD+NPNP
      IV =ITW+NPNP
      DO 10 I=1,NP
        LAB(I) = A(INAM+I)
10    CONTINUE
      DO 20 I=1,NV
        V(I) = A(IV+I)
20    CONTINUE
      DO 30 I=1,NSY1
        IORD(I) = A(IOR+I)
        LBK(I) = A(ILB+I)
30    CONTINUE
      DO 40 I=1,NSY3
        IND(I) = A(IIN+I)
40    CONTINUE
C----------------------------------------------------------------------C
C      CALL SMASHN(LAB,A(IHE),A(IHU),A(IDH),A(IWS),
C     1   A(IH1),A(IH2),A(IH3),A(IH4),A(IH5),A(IH6),A(IH7),A(IH8),A(IH9),
C     2   A(IH1),A(IDA),A(IDT),A(IDP),A(IOR),A(ILB),
C     3   A(IX) ,A(IY) ,A(IXY),A(IIN),A(IW),A(ITD),A(ITW),A(IV))
      CALL SMASHN(LAB,A(IHE),A(IHU),A(IDH),A(IWS),
     1   A(IH1),A(IH2),A(IH3),A(IH4),A(IH5),A(IH6),A(IH7),A(IH8),A(IH9),
     2   A(IH1),A(IDA),A(IDT),A(IDP),IORD,LBK,
     3   A(IX) ,A(IY) ,A(IXY),IND,A(IW),A(ITD),A(ITW),V)
C----------------------------------------------------------------------C
      RETURN
      END
      SUBROUTINE STRII(ILEV,LPRE,IWRITE,NR,ISYM,SITER,SIG1,SIG2,SIG3,
     1                 SIG4,OSIG1,OSIG2)
C     ******************************************************************
C     *                                                                *
C     *  S T R I I                                                     *
C     *                                                                *
C     *  PURPOSE: PRINTS APPROPRIATE STRESSES FOR FIRST ITERATION      *
C     *                                                                *
C     *  SUBROUTINES CALLED: NONE                                      *
C     *                                                                *
C     *  AUTHOR: INEKE STOOP                                JUNE 1982  *
C     *                                                                *
C     ******************************************************************
      INTEGER SITER
      LOGICAL LPRE
C----------------------------------------------------------------------C
      IF (ILEV.LT.3.OR.LPRE) GOTO 10
          EPS1=OSIG1-SIG1
          EPS2=OSIG2-SIG2
      IF (ISYM.EQ.0.AND.NR.GT.1)
     1         WRITE(IWRITE,100) SITER,SIG1,EPS1,SIG2,EPS2,SIG3
      IF (ISYM.EQ.1.AND.NR.GT.1)
     1         WRITE(IWRITE,100) SITER,SIG1,EPS1,SIG2,EPS2,SIG3,SIG4
      IF (ISYM.EQ.0.AND.NR.EQ.1)
     1         WRITE(IWRITE,100) SITER,SIG1,EPS1,SIG2,EPS2
      IF (ISYM.EQ.1.AND.NR.EQ.1)
     1         WRITE(IWRITE,100) SITER,SIG1,EPS1,SIG2,EPS2,SIG4
      RETURN
 10   IF (ISYM.EQ.0.AND.NR.GT.1)
     1         WRITE(IWRITE,200) SITER,SIG1,SIG2,SIG3
      IF (ISYM.EQ.1.AND.NR.GT.1)
     1         WRITE(IWRITE,200) SITER,SIG1,SIG2,SIG3,SIG4
      IF (ISYM.EQ.0.AND.NR.EQ.1)
     1         WRITE(IWRITE,200) SITER,SIG1,SIG2
      IF (ISYM.EQ.1.AND.NR.EQ.1)
     1         WRITE(IWRITE,200) SITER,SIG1,SIG2,SIG4
      RETURN
 100  FORMAT(1H0,11X,I3,3X,7F12.6)
 200  FORMAT(1H0,11X,I3,3X,2(F12.6,12X),2F12.6)
      END
      SUBROUTINE STRIT(IREL,ISYM,IWRITE,NR,SITER,
     1                 SIG1,SIG2,SIG3,SIG4,EPS1,EPS2,AHAT,NSTEP)
C     ******************************************************************
C     *                                                                *
C     *  S T R I T                                                     *
C     *                                                                *
C     *  PURPOSE: PRINTS APPROPRIATE STRESSES FOR ITERATION HISTORY    *
C     *                                                                *
C     *  SUBROUTINES CALLED: NONE                                      *
C     *                                                                *
C     *  AUTHOR: INEKE STOOP                                JUNE 1982  *
C     *                                                                *
C     ******************************************************************
      INTEGER SITER
C----------------------------------------------------------------------C
      IF (IREL.EQ.2) GOTO 10
          IF (ISYM.EQ.0.AND.NR.GT.1)
     1        WRITE(IWRITE,100) SITER,SIG1,EPS1,SIG2,EPS2,SIG3
          IF (ISYM.EQ.1.AND.NR.GT.1)
     1        WRITE(IWRITE,100) SITER,SIG1,EPS1,SIG2,EPS2,SIG3,SIG4
          IF (ISYM.EQ.0.AND.NR.EQ.1)
     1        WRITE(IWRITE,100) SITER,SIG1,EPS1,SIG2,EPS2
          IF (ISYM.EQ.1.AND.NR.EQ.1)
     1        WRITE(IWRITE,100) SITER,SIG1,EPS1,SIG2,EPS2,SIG4
       RETURN
 10       IF (ISYM.EQ.0.AND.NR.GT.1) WRITE(IWRITE,200)
     1        SITER,NSTEP,SIG1,EPS1,SIG2,EPS2,SIG3,AHAT
          IF (ISYM.EQ.1.AND.NR.GT.1) WRITE(IWRITE,200)
     1        SITER,NSTEP,SIG1,EPS1,SIG2,EPS2,SIG3,SIG4,AHAT
          IF (ISYM.EQ.1.AND.NR.EQ.1) WRITE(IWRITE,200)
     1        SITER,NSTEP,SIG1,EPS1,SIG2,EPS2,SIG4,AHAT
          IF (ISYM.EQ.0.AND.NR.EQ.1) WRITE(IWRITE,200)
     1        SITER,NSTEP,SIG1,EPS1,SIG2,EPS2,AHAT
      RETURN
 100  FORMAT(1H0,11X,I3,3X,7F12.6)
 200  FORMAT(1H0,8X,2I3,3X,7F12.6)
      END
      SUBROUTINE TITIT(LPRE,IREL,ISYM,IWRITE,NR,ND,TITLE,T1,T2,N1,N2)
C     ******************************************************************
C     *                                                                *
C     *  T I T I T                                                     *
C     *                                                                *
C     *  PURPOSE: PRINTS APPROPRIATE TITLE FOR ITERATION HISTORY       *
C     *                                                                *
C     *  SUBROUTINES CALLED: NONE                                      *
C     *                                                                *
C     *  AUTHOR: INEKE STOOP                                JUNE 1982  *
C     *                                                                *
C     ******************************************************************
      CHARACTER*80 TITLE
      CHARACTER*60 T1,T2
      LOGICAL LPRE
      IF      (LPRE) WRITE(IWRITE,100) ND,TITLE
      IF (.NOT.LPRE) WRITE(IWRITE,200) ND,TITLE
C----------------------------------------------------------------------C
      IF (ISYM.EQ.0.AND.NR.EQ.1.AND.IREL.NE.2) WRITE(IWRITE,1000) T1,T2
      IF (ISYM.EQ.1.AND.NR.EQ.1.AND.IREL.NE.2) WRITE(IWRITE,1010) T1,T2
      IF (ISYM.EQ.0.AND.NR.GT.1.AND.IREL.NE.2) WRITE(IWRITE,1100) T1,T2
      IF (ISYM.EQ.1.AND.NR.GT.1.AND.IREL.NE.2) WRITE(IWRITE,1110) T1,T2
      IF (ISYM.EQ.0.AND.NR.EQ.1.AND.IREL.EQ.2) WRITE(IWRITE,1001) T1,T2
      IF (ISYM.EQ.1.AND.NR.EQ.1.AND.IREL.EQ.2) WRITE(IWRITE,1011) T1,T2
      IF (ISYM.EQ.0.AND.NR.GT.1.AND.IREL.EQ.2) WRITE(IWRITE,1101) T1,T2
      IF (ISYM.EQ.1.AND.NR.GT.1.AND.IREL.EQ.2) WRITE(IWRITE,1111) T1,T2
C----------------------------------------------------------------------C
 100  FORMAT(41H1  HISTORY OF COMPUTATION FOR PRELIMINARY,
     1       12H ANALYSIS IN,I2,13H DIMENSION(S)/6H0     ,A80///)
 200  FORMAT(41H1  HISTORY OF COMPUTATION FOR ANALYSIS IN,
     1       I2,13H DIMENSION(S)/6H0     ,A80///)
 1000 FORMAT(1H0,8X,15A4/9X,15A4)
 1100 FORMAT(1H0,8X,15A4,12HINDIVIDUAL */9X,15A4,12HDIFFERENCES*)
 1010 FORMAT(1H0,8X,15A4,12HDUE TO     */9X,15A4,12HASYMMETRY  *)
 1110 FORMAT(1H0,8X,15A4,24HINDIVIDUAL * DUE TO    */
     2           9X,15A4,24HDIFFERENCES* ASYMMETRY *)
 1001 FORMAT(1H0,8X,15A4,12HSTEPSIZE   */9X,15A4,12HPARAMETER  *)
 1101 FORMAT(1H0,8X,15A4,24HINDIVIDUAL * STEPSIZE  */
     2           9X,15A4,24HDIFFERENCES* PARAMETER *)
 1011 FORMAT(1H0,8X,15A4,24HDUE TO     * STEPSIZE  */
     2           9X,15A4,24HASYMMETRY  * PARAMETER *)
 1111 FORMAT(1H0,8X,15A4,36HINDIVIDUAL * DUE TO    * STEPSIZE  */
     2           9X,15A4,36HDIFFERENCES* ASYMMETRY * PARAMETER *)
      RETURN
      END
      SUBROUTINE VANORW(X,W,N,Q,IWEI,NDAAT,ILEV,IWRITE)
C     ******************************************************************
C     *                                                                *
C     *  V A N O R W                                                   *
C     *                                                                *
C     *  PURPOSE: NORMALIZE X SUCH THAT SSQ(X-M)= Q                    *
C     *           SSQ : (WEIGHTED) SUM OF SQUARES                      *
C     *                                                                *
C     *  SUBROUTINES CALLED: SSVAR                                     *
C     *                                                                *
C     *  AUTHORS : INEKE STOOP                    RELEASED JUNE 1982   *
C     *                                                                *
C     ******************************************************************
      DIMENSION X(N),W(N)
C----------------------------------------------------------------------C
      CALL SSVAR(VS,VM,X,W,N,IWEI,NDAAT)
      IF (VS.LE.1.0E-10) GOTO 40
C----------------------------------------------------------------------C
      ALF=SQRT(Q/VS)
        IF (ILEV.NE.0) GG=VM*(1.-ALF)
        IF (ILEV.EQ.0) THEN
         GG=0.
         ALF=1
        END IF
      DO 10 L=1,N
         X(L)=X(L)*ALF+GG
 10   CONTINUE
      RETURN
C----------------------------------------------------------------------C
 40   WRITE(IWRITE,100)
 100  FORMAT(19H VARIANCE GETS ZERO//24H USE OTHER NORMALIZATION)
      STOP
      END
      SUBROUTINE WMRMNH (DISP,W,N)
C     ******************************************************************
C     *                                                                *
C     *  W M R M N H                                                   *
C     *                                                                *
C     *  PURPOSE: WEIGHTED MONOTONE REGRESSION.                        *
C     *                                                                *
C     *  SUBROUTINES CALLED: NONE.                                     *
C     *                                                                *
C     *  AUTHOR: ERNST VAN WANING, INEKE STOOP                         *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION DISP(N),W(N)
C
      LOVBKH = 0
      WOVBKH = 0.
      LUPH   = 0
      DO 1 IUP = 2, N
         IF (DISP(IUP) .GE. DISP(IUP-1)) GOTO 1
            SDS = DISP(IUP)*W(IUP)
            SW = W(IUP)
            IDOWN = IUP
    5          IDOWN = IDOWN - 1
               IF (LUPH .EQ. IDOWN) GOTO 2
                  SDS = SDS + DISP(IDOWN)*W(IDOWN)
                  SW = SW + W(IDOWN)
               GOTO 3
    2             SDS = SDS + (WOVBKH * DISP(LUPH))
                  SW = SW + WOVBKH
                  IDOWN = IDOWN - LOVBKH + 1
    3          TRIALV = SDS / SW
               IF (IDOWN .EQ. 1) GOTO 4
            IF (DISP(IDOWN-1).GT.TRIALV) GOTO 5
    4       WOVBKH=0.
            DO 6 J = IDOWN, IUP
               DISP(J) = TRIALV
    6             WOVBKH=WOVBKH+W(J)
            LOVBKH = IUP - IDOWN + 1
            LUPH = IUP
    1    CONTINUE
      RETURN
      END
      SUBROUTINE WPRAR(IORD,LBK,DISP,W,N)
C     ******************************************************************
C     *                                                                *
C     *  W P R A R                                                     *
C     *                                                                *
C     *  PURPOSE:  PRIMARY APPROACH WEIGHTED MONOTONE REGRESSION       *
C     *                                                                *
C     *  SUBROUTINES CALLED: SHEL10,WMRMNH                             *
C     *                                                                *
C     ******************************************************************
      DIMENSION DISP(N),IORD(N),LBK(N),W(N)
C
      IDLBK=0
      IDIP=0
      IWIP=1
      JB=1
 10      IDLBK=IDLBK+1
         NLBK=LBK(IDLBK)
         IF (NLBK.GT.1) CALL SHEL10(DISP(JB),W(JB),IORD(JB),NLBK,NLBK,
     1                              IDIP,IWIP)
         JB=JB+NLBK
      IF (JB.LT.N) GO TO 10
      CALL WMRMNH(DISP,W,N)
      RETURN
      END
      SUBROUTINE WSCAR(LBK,DISP,W,N)
C     ******************************************************************
C     *                                                                *
C     *  W S C A R                                                     *
C     *                                                                *
C     *  PURPOSE:  SECONDARY APPROACH AND MONOTONE WEIGHTED REGRESSION *
C     *                                                                *
C     *  SUBROUTINES CALLED: NONE                                      *
C     *                                                                *
C     ******************************************************************
      DIMENSION DISP(N),LBK(N),W(N)
C
      IDLBK=0
      JB=0
 2       IDLBK=IDLBK+1
         NLBK=LBK(IDLBK)
         MAX=JB+NLBK
         MIN=JB+1
         ST=0.
         SW=0.
         DO 1 K=MIN,MAX
            ST=ST+DISP(K)*W(K)
            SW=SW+W(K)
 1       CONTINUE
         DISP(IDLBK)=ST/SW
            W(IDLBK)=SW
         JB=MAX
      IF (JB.LT.N) GOTO 2
C----------------------------------------------------------------------C
C DO A WEIGHTED MONOTONE REGRESSION ON THE FIRST PART OF DISP,         C
C I.E. ON THE WEIGHTED MEANS OF THE DISTANCES BELONGING TO A TIEBLOCK  C
C----------------------------------------------------------------------C
      DO 10 IUP=2,IDLBK
         IF (DISP(IUP).GE.DISP(IUP-1)) GOTO 10
         SDS=DISP(IUP)*W(IUP)
         SW=W(IUP)
         IDOWN=IUP
 30         IDOWN=IDOWN-1
               SDS=SDS+DISP(IDOWN)*W(IDOWN)
               SW=SW+W(IDOWN)
               TRIALV=SDS/SW
               IF (IDOWN.EQ.1) GOTO 20
            IF (DISP(IDOWN-1).GT.TRIALV) GOTO 30
 20      DO 40 J=IDOWN,IUP
            DISP(J)=TRIALV
 40      CONTINUE
 10   CONTINUE
C----------------------------------------------------------------------C
C REPLACE THE REGRESSED MEANS INTO THEIR APPROPRIATE PLACES            C
C----------------------------------------------------------------------C
      IDLBK=IDLBK+1
      JB=N+1
 4       IDLBK=IDLBK-1
         MIN=JB-LBK(IDLBK)
         MAX=JB-1
         DO 3 K=MIN,MAX
            DISP(K)=DISP(IDLBK)
 3       CONTINUE
         JB=MIN
      IF (JB.GT.1) GO TO 4
      RETURN
      END
      SUBROUTINE WTEAR(LBK,DISP,W,N)
C     ******************************************************************
C     *                                                                *
C     *  W T E A R                                                     *
C     *                                                                *
C     *  PURPOSE: TERTIARY APPROACH AND WEIGHTED MONOTONE REGRESSION.  *
C     *                                                                *
C     *  SUBROUTINES CALLED: NONE                                      *
C     *                                                                *
C     *  AUTHORS: ERNST VAN WANING , WILLEM HEISER , INEKE STOOP       *
C     *                                                                *
C     *  RELEASED:  OCTOBER 1981                                       *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION DISP(N),LBK(N),W(N)
C
C----------------------------------------------------------------------C
C     COMPUTE WEIGHTED AVERAGES                                        C
C----------------------------------------------------------------------C
      IDLBK=0
      JBASE=0
 10      IDLBK=IDLBK+1
         NLBK=LBK(IDLBK)
         MAXELM=JBASE+NLBK
         IF (NLBK.EQ.1) GO TO 40
         MINELM=JBASE+1
         ST=0.0
         SW=0.0
         DO 20 K=MINELM,MAXELM
            ST=ST+DISP(K)*W(K)
 20         SW=SW+W(K)
         AVE=ST/SW
         DISP(MAXELM)=AVE
         W(MAXELM)=SW
         MAXMIN=MAXELM-1
         DO 30 K=MINELM,MAXMIN
 30         DISP(K)=DISP(K)-AVE
 40      JBASE=MAXELM
         LBK(IDLBK)=MAXELM
      IF (JBASE.LT.N) GO TO 10
C----------------------------------------------------------------------C
C     PERFORM MONOTONE WEIGHTED REGRESSION ON AVERAGES                 C
C----------------------------------------------------------------------C
      DO 80 IUP=2,IDLBK
         IF (DISP(LBK(IUP)).GE.DISP(LBK(IUP-1))) GOTO 80
            SW=W(LBK(IUP))
            SDS=DISP(LBK(IUP))*SW
            IDOWN=IUP
 50            IDOWN=IDOWN-1
               WEIGHT=W(LBK(IDOWN))
               SDS=SDS+DISP(LBK(IDOWN))*WEIGHT
               SW=SW+WEIGHT
               TRIALV=SDS/SW
               IF (IDOWN.EQ.1) GO TO 60
            IF (DISP(LBK(IDOWN-1)).GT.TRIALV) GOTO 50
 60         DO 70 J=IDOWN,IUP
 70            DISP(LBK(J))=TRIALV
 80      CONTINUE
C----------------------------------------------------------------------C
C     ADD NEW AVERAGES TO DISP                                         C
C----------------------------------------------------------------------C
      IDLBK=0
      JBASE=0
 90      IDLBK=IDLBK+1
         MINELM=JBASE+1
         MAXELM=LBK(IDLBK)
         IF (MINELM.EQ.MAXELM) GO TO 110
            ST=0.0
            SW=W(MAXELM)
            AVE=DISP(MAXELM)
            MAXMIN=MAXELM-1
            DO 100 K=MINELM,MAXMIN
               SW=SW-W(K)
               ST=ST+DISP(K)*W(K)
 100           DISP(K)=DISP(K)+AVE
            DISP(MAXELM)=AVE-ST/SW
 110     JBASE=MAXELM
      IF (JBASE.LT.N) GO TO 90
      RETURN
      END
      SUBROUTINE OUTP1(X,DIST,IND,ZERO,HEAD,                            OUT00010
     X                 IPLO,IPUN,NP,ND,NDAT,IWRITE,ISTORE)              OUT00020
C     ******************************************************************OUT00030
C     *                                                                *OUT00040
C     *  O U T P 1                                                     *OUT00050
C     *                                                                *OUT00060
C     *  PURPOSE: PROVIDES FOR THE PLOTTING, PUNCHING OR               *OUT00070
C     *           STORING OF RESULTS                                   *OUT00080
C     *                                                                *OUT00090
C     *  SUBROUTINES CALLED: PLOT                                      *OUT00100
C     *                                                                *OUT00110
C     ******************************************************************OUT00120
      DIMENSION X(NP,ND),DIST(NDAT),IND(NP),ZERO(NP)                    OUT00130
      CHARACTER*40 HEAD
C                                                                      COUT00140
      NDPL=MIN0(ND,IPLO)                                                OUT00150
      IF (NDPL.EQ.0) GO TO 5                                            OUT00160
         IF (NDPL.EQ.1) GO TO 3                                         OUT00170
            DO 1 I=2,NDPL                                               OUT00180
               IMIN=I-1                                                 OUT00190
               DO 2 J=1,IMIN                                            OUT00200
                  WRITE (IWRITE,110) ND,HEAD,J,I                        OUT00210
                  CALL PLOT(X(1,J),X(1,I),IND,NP,NP,1,IWRITE)           OUT00220
 2                CONTINUE                                              OUT00230
 1             CONTINUE                                                 OUT00240
            GO TO 5                                                     OUT00250
C                                                                      COUT00260
C  ONE-DIMENSIONAL PLOT                                                COUT00270
C                                                                      COUT00280
 3       DO 4 I=1,NP                                                    OUT00290
 4          ZERO(I)=0.0                                                 OUT00300
         WRITE(IWRITE,120) ND,HEAD                                      OUT00310
         CALL PLOT(X(1,1),ZERO,IND,NP,NP,1,IWRITE)                      OUT00320
C                                                                       OUT00330
C  PUNCH/STORE OPTIONS                                                  OUT00340
C                                                                       OUT00350
 5    IF (IPUN.EQ.0) RETURN                                             OUT00360
         WRITE(ISTORE,130) ND,HEAD                                      OUT00370
         IF (IPUN.EQ.2) GO TO 6                                         OUT00380
           OPEN(25,file="smacdist")
           REWIND(25)
            DO 8 I=1,NP                                                 OUT00390
 8             WRITE(25,140) (X(I,J),J=1,ND)                            OUT00400
            CLOSE(25)
            IF (IPUN.EQ.1) RETURN                                       OUT00410
 6       N=0                                                            OUT00420
           OPEN(25,file="smacconf")
           REWIND(25)
         DO 7 I=2,NP                                                    OUT00430
         DO 23 J=1,(I-1)
          TDIST=0
         DO 24 K=1,ND
         TDIST=TDIST+(X(I,K)-X(J,K))**2
00024     CONTINUE
         WRITE(25,227) I,J,SQRT(TDIST)
00023     CONTINUE
C            M=N+1                                                       OUT00440
C            N=N+I-1                                                     OUT00450
C          DO 22 L=N,M
C          WRITE(225,226) I,L,DIST(L)                                    OUT00460
C00022      CONTINUE
00007      CONTINUE
            CLOSE(25)
      RETURN                                                            OUT00470
 110  FORMAT(1H1,3X,I2,A40,23X,                                         OUT00480
     1       10H DIMENSION,I2,26H (X-AXIS) VERSUS DIMENSION,I2,         OUT00490
     2        9H (Y-AXIS))                                              OUT00500
 120  FORMAT(1H1,3X,I2,A40)                                             OUT00510
 130  FORMAT(3X,I2,A40)                                                 OUT00520
 140  FORMAT(8X,6F12.7)                                                 OUT00530
00226 FORMAT(2I10,2F10.5)
00227 FORMAT(2I10,2F10.2)
      END                                                               OUT00540
      SUBROUTINE PLOT(X,Y,IND,NITEM,NRC,IDENT,IWRITE)                   PLO00010
C     ******************************************************************PLO00020
C     *                                                                *PLO00030
C     *  P L O T                                                       *PLO00040
C     *                                                                *PLO00050
C     *  PURPOSE: PRINTPLOT OF Y VERSUS X                              *PLO00060
C     *           IF IDENT.GT.0, THE FIRST NRC POINTS ARE IDENTIFIED BY*PLO00070
C     *           INTEGER NUMBERS 1-9,0,1-9... , THE REST OF THE POINTS*PLO00080
C     *           BY CHARACTERS A-I,J,A-I...                           *PLO00090
C     *           IF IDENT.EQ.0, ALL POINTS ARE PRINTED AS STARS       *PLO00100
C     *           IF IDENT.LT.0, THE FIRST NRC POINTS ARE IDENTIFIED BY*
C     *           STARS, THE OTHERS BY A 'D'                           *
C     *                                                                *PLO00110
C     *  SUBROUTINES CALLED: NONE                                      *PLO00120
C     *                                                                *PLO00130
C     *  AUTHOR: WILLEM HEISER       RELEASED: MARCH 1978              *PLO00140
C     *                                                                *PLO00150
C     ******************************************************************PLO00160
      CHARACTER*1 LABEL(20),BLANK,STAR,MORE,LINE(71)
      DIMENSION XPR(11),X(NITEM),Y(NITEM),IND(NITEM)
C     DATA BLANK,STAR,MORE/1H ,1H*,1HM/,NL/56/                          PLO00190
      DATA BLANK/' '/,STAR/'*'/,MORE/'M'/,NL/56/
C     DATA LABEL/1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9,               PLO00200
C    X           1HJ,1HA,1HB,1HC,1HD,1HE,1HF,1HG,1HH,1HI/               PLO00210
      DATA LABEL/'0','1','2','3','4','5','6','7','8','9',
     X           'J','A','B','C','D','E','F','G','H','I'/
C                                                                      CPLO00220
         DO 10 I=1,NITEM                                                PLO00230
 10         IND(I)=I                                                    PLO00240
C                                                                      CPLO00250
C  THE INDICES ARE ORDERED SUCH THAT THEY WOULD ORDER Y IN DESCENDING  CPLO00260
C  ORDER                                                               CPLO00270
C                                                                      CPLO00280
      M=NITEM                                                           PLO00290
 20      M=M/2                                                          PLO00300
         IF (M.LT.1) GO TO 40                                           PLO00310
            K=NITEM-M                                                   PLO00320
            J=1                                                         PLO00330
 41            I=J                                                      PLO00340
 49               L=I+M                                                 PLO00350
                  IF (Y(IND(I)).GE.Y(IND(L))) GO TO 60                  PLO00360
                     II=IND(I)                                          PLO00370
                     IND(I)=IND(L)                                      PLO00380
                     IND(L)=II                                          PLO00390
                  I=I-M                                                 PLO00400
               IF (I.GE.1) GO TO 49                                     PLO00410
 60         J=J+1                                                       PLO00420
      IF (J-K) 41,41,20                                                 PLO00430
 40   CONTINUE                                                          PLO00440
C                                                                      CPLO00450
C  THE PLOT IS ADJUSTED TO THE RANGE OF THE 'LONGEST' AXIS             CPLO00460
C                                                                      CPLO00470
      XMAX=X(1)                                                         PLO00480
      XMIN=X(1)                                                         PLO00490
      DO 70 I=2,NITEM                                                   PLO00500
         IF (X(I).GT.XMAX) XMAX=X(I)                                    PLO00510
         IF (X(I).LT.XMIN) XMIN=X(I)                                    PLO00520
 70      CONTINUE                                                       PLO00530
      SPANX=XMAX-XMIN                                                   PLO00540
      SPANY=Y(IND(1))-Y(IND(NITEM))                                     PLO00550
      IF (SPANY.LE.SPANX) GO TO 75                                      PLO00560
         XMIN=XMIN-(SPANY-SPANX)/2.0                                    PLO00570
         SPANX=SPANY                                                    PLO00580
 75   STEPX=SPANX/70.0                                                  PLO00590
      STEPY=1.27*STEPX                                                  PLO00600
      HSTEP=STEPY/2.0                                                   PLO00610
      TOP=(Y(IND(NITEM))+Y(IND(1))+SPANX)/2.0                           PLO00620
C                                                                      CPLO00630
C  THE POINTS ARE PLOTTED LINE AFTER LINE                              CPLO00640
C                                                                      CPLO00650
      WRITE(IWRITE,1)                                                   PLO00660
      L=1                                                               PLO00670
      I=1                                                               PLO00680
      YPR=TOP+STEPY                                                     PLO00690
 80      YPR=YPR-STEPY                                                  PLO00700
         IF (I.GT.NITEM) GO TO 110                                      PLO00710
            IF ((YPR-Y(IND(I))).GT.HSTEP) GO TO 110                     PLO00720
 90            DO 95 J=1,71                                             PLO00730
 95               LINE(J)=BLANK                                         PLO00740
 100           JP=(X(IND(I))-XMIN)/STEPX+1.0                            PLO00750
               IF (LINE(JP).EQ.BLANK) GO TO 101                         PLO00760
                  LINE(JP)=MORE                                         PLO00770
                  GO TO 103                                             PLO00780
 101           IF (IDENT.EQ.0) GO TO 102                                PLO00790
               IF (IDENT.GT.0) GO TO 104
                  IF (IND(I).LE.NRC) LINE(JP)=STAR
                  IF (IND(I).GT.NRC) LINE(JP)=LABEL(15)
                  GO TO 103
 104              IF (IND(I).LE.NRC) LINE(JP)=LABEL(MOD(IND(I),10)+1)   PLO00800
                  IF (IND(I).GT.NRC)                                    PLO00810
     X               LINE(JP)=LABEL(MOD((IND(I)-NRC),10)+11)            PLO00820
                  GO TO 103                                             PLO00830
 102           LINE(JP)=STAR                                            PLO00840
 103           I=I+1                                                    PLO00850
               IF (I.GT.NITEM) GO TO 105                                PLO00860
                  IF ((YPR-Y(IND(I))).LT.HSTEP) GO TO 100               PLO00870
 105           WRITE(IWRITE,2) YPR,LINE                                 PLO00880
               GO TO 115                                                PLO00890
 110     WRITE(IWRITE,3) YPR                                            PLO00900
 115     L=L+1                                                          PLO00910
      IF (L.LE.NL) GO TO 80                                             PLO00920
         WRITE(IWRITE,1)                                                PLO00930
C                                                                      CPLO00940
C  VALUES OF X-AXIS ARE PRINTED                                        CPLO00950
C                                                                      CPLO00960
      XPR(1)=XMIN                                                       PLO00970
      DO 130 J=1,10                                                     PLO00980
 130     XPR(J+1)=XPR(J)+STEPX*7.0                                      PLO00990
      WRITE(IWRITE,4) XPR                                               PLO01000
 1    FORMAT(16X,78H.---------------------------------------------------PLO01010
     X-------------------------.)                                       PLO01020
 2    FORMAT(8X,F7.3,1X,1H!,2X,71A1,3X,1H!)                             PLO01030
 3    FORMAT(8X,F7.3,1X,1H!,76X,1H!)                                    PLO01040
 4    FORMAT(15X,11F7.3)                                                PLO01050
      RETURN                                                            PLO01060
      END                                                               PLO01070
      SUBROUTINE POSPLO(X,W,ND,NPL)
C     ******************************************************************
C     *                                                                *
C     *  P O S P L O                                                   *
C     *                                                                *
C     *  INSERT DIAGONAL AND MISSING VALUES                            *
C     *                                                                *
C     ******************************************************************
      DIMENSION X(ND),W(ND)
         LA=ND+1
         LB=NPL+1
         DO 20 I=1,ND
            LA=LA-1
            IF (W(LA).LE.0.1E-10) GOTO 10
               LB=LB-1
               X(LA)=X(LB)
               GOTO 20
  10        X(LA)=0.
  20     CONTINUE
      RETURN
      END
      SUBROUTINE PREPLO(X,W,ND,NPL)
C     ******************************************************************
C     *                                                                *
C     *  P R E P L O                                                   *
C     *                                                                *
C     *  REMOVE DIAGONAL AND MISSING VALUES                            *
C     *                                                                *
C     ******************************************************************
      DIMENSION X(ND),W(ND)
         LA=0
         NPL=0
      DO 10 I=1,ND
         LA=LA+1
         IF (W(LA).LE.0.1E-10) GOTO 10
            NPL=NPL+1
            X(NPL)=X(LA)
  10  CONTINUE
      RETURN
      END
      SUBROUTINE PRIREC(X,M,N,MM,IWRITE)                                REC00010
C     ******************************************************************REC00020
C     *                                                                *REC00030
C     *  P R I R E C                                                   *REC00040
C     *                                                                *REC00050
C     *  PURPOSE: PRINTS A M*N ARRAY WITH ROW- AND COLUMN IDENTIFI-    *REC00060
C     *           CATION, EACH LINE CONTAINING AT MOST 12 VALUES (F9.3)*REC00070
C     *                                                                *REC00080
C     *  SUBROUTINES CALLED: NONE                                      *REC00090
C     *                                                                *REC00100
C     ******************************************************************REC00110
      DIMENSION X(MM,N)                                                 REC00120
C                                                                      CREC00130
C  CHECK HOW MANY TIMES THE NUMBER OF COLUMNS EXCEEDS 12               CREC00140
C                                                                      CREC00150
      NTRU=N/12                                                         REC00160
      NREM=N-NTRU*12                                                    REC00170
      IF (NREM.GT.0) GO TO 1                                            REC00180
         NREM=12                                                        REC00190
         NTRU=NTRU-1                                                    REC00200
 1    NREP=NTRU+1                                                       REC00210
C                                                                      CREC00220
C  PRINT NREP TIMES A BLOCK OF ELEMENTS                                CREC00230
C                                                                      CREC00240
      NK=12                                                             REC00250
      KB=1                                                              REC00260
      DO 2 I=1,NREP                                                     REC00270
         IF (I.EQ.NREP) NK=NREM                                         REC00280
         KE=KB+NK-1                                                     REC00290
         WRITE(IWRITE,60) (K,K=KB,KE)                                   REC00300
         WRITE(IWRITE,61)                                               REC00310
         DO 3 L=1,M                                                     REC00320
 3          WRITE(IWRITE,62) L,(X(L,K),K=KB,KE)                         REC00330
         KB=KB+NK                                                       REC00340
 2       CONTINUE                                                       REC00350
      RETURN                                                            REC00360
 60   FORMAT(1H0,4X,12I9)                                               REC00370
 61   FORMAT(1H )                                                       REC00380
 62   FORMAT(1H ,I5,2X,12F9.3)                                          REC00390
      END                                                               REC00400
      SUBROUTINE VSHEP(DATA,DISP,DIST,HELP,HULP,WT,WS,IORD,LBK,HDIS,IND,
     1           NDAT,NDAAT,NPNP2,NPNP,NDAAT2,NSY1,NP,NR,ISTORE,
     2           IWRITE,ISHE,IPRO,ILEV,IPRJ,INOR,IWEI,ICON,XSTAN)
C     ******************************************************************
C     *                                                                *
C     *  S H E P                                                       *
C     *                                                                *
C     *  PURPOSE: PROVIDES SHEPARD PLOTS FOR DIFFERENT COMBINATIONS    *
C     *           OF DATA , DISP AND DIST. ALSO STORES OR PRINTS DISP. *
C     *           ASYMMETRIC DATA                                      *
C     *                                                                *
C     *  SUBROUTINES CALLED: PLOT,PRIREC,STOREC,REGRE,STRETC,PREPLO,   *
C     *                      POSPLO,PRITRI,STOTRI,REVERS               *
C     *                                                                *
C     ******************************************************************
      DIMENSION HELP(NPNP2),HULP(NPNP2),IORD(NSY1),LBK(NSY1),HDIS(NPNP),
     1          DATA(NP,NP),DISP(NDAT),DIST(NDAT),IND(NDAAT2),
     2          WT(NPNP),WS(NPNP2)
C----------------------------------------------------------------------C
      I=0
      NPLU=NPNP+1
         CALL STRETC(HDIS,DIST,NDAT,NP)
      IF (ISHE.LT.0) GOTO 30
         CALL STRETC(HELP,DISP,NDAT,NP)
         L=0
         DO 20 JL=1,NP
            DO 10 IL=1,NP
               L=L+1
               HULP(L)=DATA(IL,JL)
               HULP(NPNP+L)=DATA(IL,JL)
               HELP(NPNP+L)=HDIS(L)
               WS(L)=WT(L)
               WS(NPNP+L)=WT(L)
 10         CONTINUE
 20      CONTINUE
      CALL PREPLO(HULP,WS,NPNP2,NPLO)
      CALL PREPLO(HELP,WS,NPNP2,NPLO)
      NPLO1=NPLO/2
      WRITE(IWRITE,600)
      CALL PLOT(HULP,HELP,IND,NPLO,NPLO1,-1,IWRITE)
C----------------------------------------------------------------------C
C CHECK PRINTING AND STORING INDIVIDUAL DISPARITIES                    C
C----------------------------------------------------------------------C
 30                                      IPS=0
      IF (IPRJ.GT.   0.AND.IPRJ.LT.1000) IPS=1
      IF (IPRJ.GE.1000.AND.IPRJ.LT.2000) IPS=2
      IF (IPRJ.GE.2000)                  IPS=3
          IPRK=IPRJ-(IPRJ/1000)*1000
      IF (IPS.EQ.0.AND.ISHE.LE.0) RETURN
      IF (IPS.NE.1.AND.IPS .NE.3) GOTO 40
          WRITE(IWRITE,900)
          CALL PRITRI(DISP,NP,NDAT,IWRITE)
 40   IF (IPS.GE.2) CALL STOTRI(DISP,NP,NDAT,ISTORE,IWRITE,I)
      KMAX=MAX0(ISHE,IPRK)
      IF (KMAX.GT.NR) KMAX=NR
      REWIND 2
      IF (ILEV.EQ.2) REWIND 3
      IF (IWEI.EQ.1) REWIND 4
      DO 100 I=1,KMAX
         IF (IWEI.EQ.0) CALL SETW(WS,NPNP,NP)
         IF (IWEI.EQ.1) READ(4) (WS(L),L=1,NPNP)
         IF (ILEV.LT.2) READ(2) (HULP(L),L=1,NPNP)
C---------------------------------------------------------------------C
C  COMPUTE FINAL DISPARITIES FOR THE I'TH REPLICATION                 C
C---------------------------------------------------------------------C
         IF (ICON.NE.0) GOTO 50
         CALL REGRE(HULP(1),DIST,WS(1),IORD,LBK,HULP(NPLU),HDIS,NDAT,
     1        NPNP,NDAAT,NP,IPRO,ILEV,1,INOR,1,ICON,IWRITE,XSTAN)
                        GOTO 70
 50      DO 60 J=1,NP
            L=1+NP*(J-1)
            CALL REGRE(HULP(L),HDIS(L),WS(L),IORD,LBK,HULP(NPLU),
     1                 HULP(NPLU+NP),NP,NP,NP,NP,IPRO,ILEV,
     2                 1,INOR,0,ICON,IWRITE,XSTAN)
 60      CONTINUE
 70      IF (ISHE.LE.0.OR.I.GT.ISHE) GOTO 95
            IF (ILEV.LT.2) BACKSPACE 2
            READ(2) (HELP(L),L=1,NPNP)
            DO 80 L=1,NPNP
               HELP(NPNP+L)=HELP(L)
               HULP(NPNP+L)=HDIS(L)
                 WS(NPNP+L)=  WS(L)
 80         CONTINUE
            CALL PREPLO(HELP,WS,NPNP2,NPLO)
            CALL PREPLO(HULP,WS,NPNP2,NPLO)
            NPLO1=NPLO/2
            WRITE(IWRITE,700) I
            CALL PLOT(HELP,HULP,IND,NPLO,NPLO1,-1,IWRITE)
            CALL POSPLO(HULP,WS,NPNP2,NPLO)
 95      IF (IPRK.LE.0.OR.I.GT.IPRK.OR.IPS.EQ.0) GOTO 100
            IF (ICON.EQ.-1) CALL REVERS(HULP,NP,NP)
         IF (IPS.NE.1.AND.IPS.NE.3) GOTO 90
            WRITE(IWRITE,800) I
            CALL PRIREC(HULP,NP,NP,NP,IWRITE)
 90      IF (IPS.GE.2) CALL STOREC(HULP,NP,NP,NP,ISTORE,IWRITE,I)
 100  CONTINUE
      RETURN
 600  FORMAT(1H1,27H    SHEPARD PLOT AGGREGATED,29X,14H(STANDARDIZED),
     1           43H DATA (X-AXIS) VERSUS DIST (D) AND DISP (*))
 800  FORMAT(///37H0DISPARITY MATRIX OF REPLICATION NO  ,I5)
 900  FORMAT(///37H0AGGREGATED DISPARITY MATRIX         )
 700  FORMAT(1H1,30H SHEPARD PLOT FOR REPLICATION ,I5,33X,
     1       47H DATA (X-AXIS) VERSUS DIST (D) AND DISP (*)    )
      END
      SUBROUTINE WSHEP(DATA,DISP,DIST,HELP,HULP,WT,WS,IORD,LBK,IND,
     1                 NDAT,NDAAT,NP,NR,ISTORE,IWRITE,ISHE,IPRO,ILEV,
     2                 IPRJ,INOR,IWEI,XSTAN)
C     ******************************************************************
C     *                                                                *
C     *  S H E P                                                       *
C     *                                                                *
C     *  PURPOSE: PROVIDES SHEPARD PLOTS FOR DIFFERENT COMBINATIONS    *
C     *           OF DATA , DISP AND DIST. ALSO STORES OR PRINTS DISP. *
C     *                                                                *
C     *  SUBROUTINES CALLED: PLOT,PRITRI,STOTRI,REGRE,PREPLO,POSPLO    *
C     *                                                                *
C     ******************************************************************
      DIMENSION HELP(NDAAT),HULP(NDAAT),IORD(NDAT),LBK(NDAT),WT(NDAT),
     1          DATA(NDAT),DISP(NDAT),DIST(NDAT),IND(NDAAT),WS(NDAAT)
C----------------------------------------------------------------------C
      I=0
      NPLO=NDAAT
      NPLO1=NDAT
      IF (ISHE.LT.0) GOTO 30
      DO 10 L=1,NDAT
         HELP(L)=DATA(L)
         HELP(NDAT+L)=DATA(L)
            HULP(L)=DISP(L)
            HULP(NDAT+L)=DIST(L)
         IF (IWEI.EQ.1) WS(L)=WT(L)
         IF (IWEI.EQ.1) WS(NDAT+L)=WT(L)
 10   CONTINUE
      IF (IWEI.EQ.0) GOTO 20
         CALL PREPLO(HELP,WS,NDAAT,NPLO)
         CALL PREPLO(HULP,WS,NDAAT,NPLO)
         NPLO1=NPLO/2
 20   WRITE(IWRITE,600)
      CALL PLOT(HELP,HULP,IND,NPLO,NPLO1,-1,IWRITE)
C----------------------------------------------------------------------C
C CHECK PRINTING AND STORING INDIVIDUAL DISPARITIES                    C
C----------------------------------------------------------------------C
 30                                      IPS=0
      IF (IPRJ.GT.   0.AND.IPRJ.LT.1000) IPS=1
      IF (IPRJ.GE.1000.AND.IPRJ.LT.2000) IPS=2
      IF (IPRJ.GE.2000)                  IPS=3
          IPRK=IPRJ-(IPRJ/1000)*1000
      IF (IPS.EQ.0.AND.ISHE.LE.0) RETURN
      IF (IPS.NE.1.AND.IPS .NE.3) GOTO 40
          WRITE(IWRITE,900)
          CALL PRITRI(DISP,NP,NDAT,IWRITE)
 40   IF (IPS.GE.2) CALL STOTRI(DISP,NP,NDAT,ISTORE,IWRITE,I)
      KMAX=MAX0(ISHE,IPRK)
      IF (KMAX.GT.NR) KMAX=NR
      REWIND 2
      IF (ILEV.EQ.2) REWIND 3
      IF (IWEI.EQ.1) REWIND 4
      DO 100 I=1,KMAX
         IF (IWEI.EQ.1) READ(4) (WS(L),L=1,NDAT)
         IF (ILEV.LT.2) READ(2) (HULP(L),L=1,NDAT)
C---------------------------------------------------------------------C
C  COMPUTE FINAL DISPARITIES FOR THE I'TH REPLICATION                 C
C---------------------------------------------------------------------C
         CALL REGRE(HULP(1),DIST,WS(1),IORD,LBK,HULP(NDAT+1),
     1              HELP(NDAT+1),NDAT,NDAT,NDAT,NP,
     2              IPRO,ILEV,IWEI,INOR,0,0,IWRITE,XSTAN)
         IF (ISHE.LE.0.OR.I.GT.ISHE) GOTO 80
            IF (ILEV.LT.2) BACKSPACE 2
            READ(2) (HELP(L),L=1,NDAT)
         DO 50 L=1,NDAT
            HELP(NDAT+L)=HELP(L)
            HULP(NDAT+L)=DIST(L)
              WS(NDAT+L)=  WS(L)
 50      CONTINUE
         IF (IWEI.EQ.0) GOTO 60
            CALL PREPLO(HELP,WS,NDAAT,NPLO)
            CALL PREPLO(HULP,WS,NDAAT,NPLO)
            NPLO1=NPLO/2
 60      WRITE(IWRITE,700) I
         CALL PLOT(HELP,HULP,IND,NPLO,NPLO1,-1,IWRITE)
         IF (IWEI.EQ.1) CALL POSPLO(HULP,WS,NDAAT,NPLO)
 80      IF ((IPRK.LE.0.OR.I.GT.IPRK).AND.(IPS.NE.1.OR.IPS.NE.3))
     1                         GOTO 90
         WRITE(IWRITE,800) I
         CALL PRITRI(HULP,NP,NDAT,IWRITE)
 90      IF (IPRK.GT.0.AND.I.LE.IPRK.AND.IPS.GE.2.)
     1        CALL STOTRI(HULP,NP,NDAT,ISTORE,IWRITE,I)
 100  CONTINUE
      RETURN
 600  FORMAT(1H1,31H        SHEPARD PLOT AGGREGATED,37X,
     1           47H DATA (X-AXIS) VERSUS DIST (D) AND DISP (*)    )
 800  FORMAT(///37H0DISPARITY MATRIX OF REPLICATION NO  ,I5)
 900  FORMAT(///37H0AGGREGATED DISPARITY MATRIX         )
 700  FORMAT(1H1,30H SHEPARD PLOT FOR REPLICATION ,I5,33X,
     1       47H DATA (X-AXIS) VERSUS DIST (D) AND DISP (*)    )
      END
      SUBROUTINE RANDU(A,N,IINI)
C     ******************************************************************
C     *                                                                *
C     *  R A N D U                                                     *
C     *                                                                *
C     *  PURPOSE: COMPUTES RANDOM INITIAL CONFIGURATION, DEPENDENT     *
C     *                           ON VALUE OF IINI                     *
C     *                                                                *
C     *  SUBROUTINES CALLED: NONE                                      *
C     *                                                                *
C     *  AUTHOR: ADAPTED FROM SSP                           JUNE 1982  *
C     *                                                                *
C     ******************************************************************
      DIMENSION A(N)
      IX=143315747-20*IINI
      DO 10 I=1,N
        IY=IX*65539
        IF (IY.LT.0) IY=IY+214748367+1
        YFL=IY
        A(I)=YFL*.4656613E-9
        IX=IY
  10  CONTINUE
      RETURN
      END
      SUBROUTINE REVERS(Y,NI,NJ)
C     ******************************************************************
C     *                                                                *
C     *    R E V E R S                                                 *
C     *                                                                *
C     *     PURPOSE : GIVES TRANSPOSE OF MATRIX                        *
C     *                                                                *
C     *    AUTHOR : INEKE STOOP                                        *
C     *                                                                *
C     ******************************************************************
      DIMENSION Y(NI,NJ)
      DO 20 J=2,NJ
         IMIN=J-1
         DO 10 I=1,IMIN
            X=Y(I,J)
            Y(I,J)=Y(J,I)
            Y(J,I)=X
  10     CONTINUE
  20  CONTINUE
      RETURN
      END
      SUBROUTINE RINPUN(DATA,DISS,DHELP,HELP,HULP,IORD,LBK,W,FORM1,NR,
     1                  NP,NDAT,IPRI,INDATA,IWRITE,
     2                  ILEV,IAPT,IDIS,FNR,ICON,ISYM,INO,SS)
C     ******************************************************************
C     *                                                                *
C     *  R I N P U N                                                   *
C     *                                                                *
C     *  PURPOSE: - READ THE DATA ACCORDING TO THE DATA SPECIFICATION  *
C     *             PARAMETERS                                         *
C     *           - PREPARE DATA MATRIX                                *
C     *                                                                *
C     *  SUBROUTINES CALLED: WRANK,PRIREC,SQNORW,VANORW,SETW           *
C     *                                                                *
C     ******************************************************************
      DIMENSION DATA(NDAT),DISS(NP,NP),IORD(NP),LBK(NP),W(NP,NP),
     1          HELP(NP),HULP(NP),DHELP(NP,NP)
      CHARACTER*80 FORM1
C    1          HELP(NP),HULP(NP),FORM1(20),DHELP(NP,NP)
C                                                                      C
      IWD=1
      CALL SETW(W,NP*NP,NP)
      DO 200 K=1,NR
         IF (ISYM.EQ. 1) READ(INDATA,FORM1) ((DISS(I,J),J=1,NP),I=1,NP)
         IF (ISYM.EQ.-1) READ(INDATA,FORM1) (DATA(L),L=1,NDAT)
         IF (ISYM) 50,30,80
  30     LE=0
         DO 40 I=2,NP
            LB=LE+1
            LE=LE+I-1
            READ(INDATA,FORM1) (DATA(L),L=LB,LE)
  40     CONTINUE
  50     L=0
         DO 70 J=2,NP
            IMIN=J-1
            DO 60 I=1,IMIN
               L=L+1
               DISS(I,J)=DATA(L)
               DISS(J,I)=DATA(L)
  60        CONTINUE
  70     CONTINUE
  80     DO 90 I=1,NP
            DISS(I,I)=0.
  90     CONTINUE
         IF (IPRI.LT.K) GO TO 100
C----------------------------------------------------------------------C
C  PRINT THE ORIGINAL DATA                                             C
C----------------------------------------------------------------------C
            WRITE(IWRITE,1000) K
            CALL PRIREC(DISS,NP,NP,NP,IWRITE)
C----------------------------------------------------------------------C
C IF NONMETRIC ITERATIONS HAVE TO BE PERFORMED, PREPARE IORD AND LBK   C
C----------------------------------------------------------------------C
 100  IF (ICON.EQ.-1) CALL REVERS(DISS,NP,NP)
         DO 110 J=1,NP
            IF (ILEV.GT.1) CALL WRANK
     1         (DISS(1,J),HELP,W(1,J),HULP,IORD,LBK,NP,IAPT,IDIS,IWD)
            IF (INO.EQ.1) CALL SQNORW(DISS(1,J),W(1,J),NP,SS,1)
            IF (INO.EQ.2) CALL VANORW
     1               (DISS(1,J),W(1,J),NP,SS,1,NP,ILEV,IWRITE)
 110     CONTINUE
            WRITE(2) ((DISS(I,J),I=1,NP),J=1,NP)
         L=0
         DO 130 J=2,NP
            JMIN=J-1
            DO 120 I=1,JMIN
               DHELP(I,J)=DHELP(I,J)+DISS(I,J)
               DHELP(J,I)=DHELP(J,I)+DISS(J,I)
 120        CONTINUE
 130     CONTINUE
 200  CONTINUE
C----------------------------------------------------------------------C
C  SYMMETRIZE AND AVERAGE DATA AND WEIGHTS                             C
C----------------------------------------------------------------------C
      L=0
      DO 220 J=2,NP
         IMIN=J-1
         DO 210 I=1,IMIN
            L=L+1
            DHELP(I,J)=DHELP(I,J)*FNR
            DHELP(J,I)=DHELP(J,I)*FNR
            DATA(L)=.5*(DHELP(I,J)+DHELP(J,I))
 210     CONTINUE
 220  CONTINUE
         RETURN
 1000 FORMAT(///40H0ORIGINAL DATA MATRIX OF REPLICATION NO  ,I5)
      END
      SUBROUTINE RLAX(X,Y,ITER,IREL,NROW,NCOL,MNROW)                    RLA00010
C     ******************************************************************RLA00020
C     *                                                                *RLA00030
C     *  R L A X                                                       *RLA00040
C     *                                                                *RLA00050
C     *  PURPOSE: COMPUTE THE RELAXED SUCCESSOR AS                     *RLA00060
C     *                                                                *RLA00070
C     *            X    = Y  + (Y  - X )                               *RLA00080
C     *             K+1    K     K    K                                *RLA00090
C     *                                                                *RLA00100
C     *           IF IREL=1 OR ITER<5 JUST COPY Y INTO X               *RLA00110
C     *                                                                *RLA00120
C     *  SUBROUTINES CALLED: NONE                                      *RLA00130
C     *                                                                *RLA00140
C     ******************************************************************RLA00150
      DIMENSION X(MNROW,NCOL),Y(MNROW,NCOL)                             RLA00160
C                                                                      CRLA00170
      IF (IREL.EQ.1.OR.ITER.LT.5) GO TO 10                              RLA00180
         DO 1 K=1,NCOL                                                  RLA00190
            DO 2 I=1,NROW                                               RLA00200
               X(I,K)=Y(I,K)+Y(I,K)-X(I,K)                              RLA00210
 2             CONTINUE                                                 RLA00220
 1          CONTINUE                                                    RLA00230
         RETURN                                                         RLA00240
 10   DO 11 K=1,NCOL                                                    RLA00250
         DO 12 I=1,NROW                                                 RLA00260
            X(I,K)=Y(I,K)                                               RLA00270
 12         CONTINUE                                                    RLA00280
 11      CONTINUE                                                       RLA00290
      RETURN                                                            RLA00300
      END                                                               RLA00310
      SUBROUTINE ROTA(X,Z,D,E,NROW,NCOL,MNROW,IWRITE)
C     ******************************************************************
C     *                                                                *
C     *  R O T A                                                       *
C     *                                                                *
C     *  PURPOSE: ROTATE A CONFIGURATION TO ITS PRINCIPAL AXES         *
C     *                                                                *
C     *  SUBROUTINES CALLED: TRED2, IMTQL2                             *
C     *                                                                *
C     ******************************************************************
      DIMENSION  X(MNROW,NCOL),Z(NCOL,NCOL),D(NCOL),E(NCOL)
C                                                                      C
      DO 1 I=1,NCOL
         DO 2 J=1,I
            Z(I,J)=0.0
            DO 3 K=1,NROW
 3             Z(I,J)=Z(I,J)+X(K,I)*X(K,J)
 2          CONTINUE
 1       CONTINUE
      CALL TRED2(NCOL,NCOL,D,E,Z)
      CALL IMTQL2(NCOL,NCOL,D,E,Z,IERR)
      IF (IERR.GT.0) GO TO 100
         DO 4 I=1,NROW
            DO 5 K=1,NCOL
               E(K)=0.0
               DO 6 L=1,NCOL
 6                E(K)=E(K)+X(I,L)*Z(L,K)
 5             CONTINUE
            DO 7 K=1,NCOL
               X(I,K)=E(K)
 7             CONTINUE
 4          CONTINUE
         RETURN
 100  WRITE(IWRITE,101)
 101  FORMAT(1H1,30HNO CONVERGENCE OF IMTQL2      ,
     X      /1H0,30HNO ROTATION FOUND             )
      STOP
      END
      SUBROUTINE RWINPU(DATA,DISS,DHELP,HELP,HULP,IORD,LBK,W,WT,WH,
     1           FORM1,FORM2,NR,NP,NDAT,IPRI,INDATA,IWRITE,
     2           ILEV,IAPT,IDIS,ICON,ISYM,IWEI,IMIS,VMIS,INO,SS)
C     ******************************************************************
C     *                                                                *
C     *  R W I N P U                                                   *
C     *                                                                *
C     *  PURPOSE: - READ THE DATA ACCORDING TO THE DATA SPECIFICATION  *
C     *             PARAMETERS          WEIGHTED ASYMMETRIC            *
C     *           - PREPARE DATA MATRIX                                *
C     *                                                                *
C     *  SUBROUTINES CALLED: WRANK,PRIREC,PRIMIR,SQNORW,VANORW,SETW    *
C     *                                                                *
C     ******************************************************************
      DIMENSION DATA(NDAT),DISS(NP,NP),IORD(NP),LBK(NP),
     1          HELP(NP),HULP(NP),DHELP(NP,NP),
     2          W(NP,NP),WT(NP,NP),WH(NDAT)
      CHARACTER*80 FORM1,FORM2
C    1          HELP(NP),HULP(NP),FORM1(20),FORM2(20),DHELP(NP,NP),
C----------------------------------------------------------------------C
      IWD=1
      DO 200 K=1,NR
         IF (IWEI.EQ.0) CALL SETW(W,NP*NP,NP)
         IF (ISYM.NE.1) GOTO 20
             READ(INDATA,FORM1) ((DISS(I,J),J=1,NP),I=1,NP)
             IF (IWEI.EQ.0) GOTO 70
                READ(INDATA,FORM2) ((W(I,J),J=1,NP),I=1,NP)
                DO 10 I=1,NP
                   W(I,I)=0.
  10            CONTINUE
            IF (IMIS) 110,110,70
  20     IF (ISYM.EQ.-1) READ(INDATA,FORM1) (DATA(L),L=1,NDAT)
         LE=0
         DO 40 I=2,NP
            LB=LE+1
            LE=LE+I-1
            J=0
            IF (ISYM.EQ.0) READ(INDATA,FORM1) (DATA(L),L=LB,LE)
            DO 30 L=LB,LE
               J=J+1
               DISS(I,J)=DATA(L)
               DISS(J,I)=DATA(L)
  30        CONTINUE
  40     CONTINUE
         IF (IWEI.EQ.0) GOTO 70
            IF (ISYM.EQ.-1) READ(INDATA,FORM2) (WH(L),L=1,NDAT)
            LE=0
            DO 60 I=2,NP
               LB=LE+1
               LE=LE+I-1
               J=0
               IF (ISYM.EQ.0) READ(INDATA,FORM2) (WH(L),L=LB,LE)
               DO 50 L=LB,LE
                  J=J+1
                  W(I,J)=WH(L)
                  W(J,I)=WH(L)
  50           CONTINUE
  60        CONTINUE
            IF (IMIS.LE.0) GOTO 110
  70     NMIS=0
         L=0
         DO 100 J=2,NP
            JMIN=J-1
            DO 90 I=1,JMIN
               L=L+1
               IF (DISS(I,J).GT.VMIS) GOTO 80
                  W(I,J)=0.
                  NMIS=NMIS+1
  80              IF (DISS(J,I).GT.VMIS) GOTO 90
                     W(J,I)=0.
                     NMIS=NMIS+1
  90        CONTINUE
 100     CONTINUE
 110     IF (IPRI.LT.K) GO TO 150
C----------------------------------------------------------------------C
C  PRINT THE ORIGINAL DATA                                             C
C----------------------------------------------------------------------C
            WRITE(IWRITE,1000) K
            IF (IMIS.EQ.0) CALL PRIREC(DISS,NP,NP,NP,IWRITE)
            IF (IMIS.EQ.1) CALL PRIMIR(DISS,NP,NP,VMIS,IWRITE)
            IF (IWEI.EQ.1) WRITE(IWRITE,2000)
            IF (IWEI.EQ.1) CALL PRIREC(   W,NP,NP,NP,IWRITE)
 150        IF (IMIS.EQ.1) WRITE(IWRITE,3000) K,NMIS
C----------------------------------------------------------------------C
C IF NONMETRIC ITERATIONS HAVE TO BE PERFORMED, PREPARE IORD AND LBK   C
C----------------------------------------------------------------------C
         IF (ICON.EQ.-1) CALL REVERS(DISS,NP,NP)
         IF (ICON.EQ.-1) CALL REVERS(   W,NP,NP)
         DO 170 J=1,NP
            IF (ILEV.GT.1) CALL WRANK
     1         (DISS(1,J),HELP,W(1,J),HULP,IORD,LBK,NP,IAPT,IDIS,IWD)
            IF (INO.EQ.1) CALL SQNORW(DISS(1,J),W(1,J),NP,SS,1)
            IF (INO.EQ.2) CALL VANORW
     1                 (DISS(1,J),W(1,J),NP,SS,1,NP,ILEV,IWRITE)
            DO 160 I=1,NP
               IF (I.EQ.J) GOTO 160
                  DHELP(I,J)=DHELP(I,J)+DISS(I,J)*W(I,J)
                  WT(I,J)=   WT(I,J)+   W(I,J)
 160        CONTINUE
 170     CONTINUE
C----------------------------------------------------------------------C
            WRITE(2) ((DISS(I,J),I=1,NP),J=1,NP)
            WRITE(4) ((   W(I,J),I=1,NP),J=1,NP)
 200  CONTINUE
C----------------------------------------------------------------------C
C  SYMMETRIZE AND AVERAGE DATA AND WEIGHTS                             C
C----------------------------------------------------------------------C
      L=0
      DO 300 J=2,NP
         IMIN=J-1
         DO 240 I=1,IMIN
            L=L+1
            IF (WT(I,J).GT.1.0E-6) GOTO 210
               DHELP(I,J)=0.
               WT(I,J)=0.
               IF (WT(J,I).GT.1.0E-6) GOTO 220
                  DHELP(J,I)=0.
                  WT(J,I)=0.
                  DATA(L)=0.
                  WH(L)=0.
                  GOTO 240
 210        IF (WT(J,I).GT.1.0E-6) GOTO 230
               DHELP(J,I)=0.
               WT(J,I)=0.
               DHELP(I,J)=DHELP(I,J)/WT(I,J)
               DATA(L)=DHELP(I,J)
               WH(L)=WT(I,J)
               GOTO 240
 220           DHELP(J,I)=DHELP(J,I)/WT(J,I)
               DATA(L)=DHELP(J,I)
               WH(L)=WT(J,I)
               GOTO 240
 230        DATA(L)=DHELP(I,J)+DHELP(J,I)
            WX=WT(I,J)+WT(J,I)
            DATA(L)=DATA(L)/WX
            WH(L)=.5*WX
            DHELP(I,J)=DHELP(I,J)/WT(I,J)
            DHELP(J,I)=DHELP(J,I)/WT(J,I)
 240     CONTINUE
 300  CONTINUE
      RETURN
 1000 FORMAT(///40H0ORIGINAL DATA MATRIX OF REPLICATION NO  ,I5)
 2000 FORMAT(//8H0WEIGHTS)
 3000 FORMAT(15H0REPLICATION NO,I5,4H HAS,I5,15H MISSING VALUES)
      END
      SUBROUTINE SETW(X,N,N5)
C     ******************************************************************
C     *                                                                *
C     *    S E T W                                                     *
C     *                                                                *
C     *    PURPOSE : COMPUTES WEIGHTS IN UNWEIGHTED CONDITIONS         *
C     *                                                                *
C     ******************************************************************
      DIMENSION X(N)
      L=0
      DO 20 I=1,N5
         DO 10 J=1,N5
            L=L+1
            IF (I.NE.J) X(L)=1.0
            IF (I.EQ.J) X(L)=0.0
  10     CONTINUE
  20  CONTINUE
      RETURN
      END
      SUBROUTINE SET0(X,N)
C     ******************************************************************
C     *                                                                *
C     *    S E T 0                                                     *
C     *                                                                *
C     *    PURPOSE : GIVES ELEMENTS OF X INITIAL VALUE ZERO            *
C     *                                                                *
C     ******************************************************************
      DIMENSION X(N)
      DO 10 I=1,N
         X(I)=0.
  10  CONTINUE
      RETURN
      END
      SUBROUTINE SMASHN(LAB,HELP,HULP,DHELP,WS,H1,H2,H3,H4,H5,H6,H7,H8,
     1           H9,H18,DATA,DIST,DISP,IORD,LBK,X,Y,XY,IND,W,WT,DT,V)
C     ******************************************************************
C     *                                                                *
C     *  S M A S H N                                                   *
C     *                                                                *
C     *  PURPOSE: CONTROLS THE FLOW OF THE SMACOF-IB PROGRAM           *
C     *                                                                *
C     *  SUBROUTINES CALLED: NINPU,INITIA,INITW,OUTP1,                 *
C     *                      PRIREC,PRITEX,DFITV,DFITW,                *
C     *                      UPDAW,RLAX,RLAXN,ROTA,PLOT,VSHEP,WSHEP,   *
C     *                      STRIND,TITIT,STRIT,STRII,STRIE            *
C     *                                                                *
C     *  AUTHOR: INEKE STOOP , WILLEM HEISER   RELEASED:     MAY 1982  *
C     *                                                                *
C     ******************************************************************
      COMMON TITLE,FORM1,FORM2,VMIS,CRII,CRIP,
     1       NP,NR,ISYM,IWEI,IMIS,ILAB,
     2       IPRI,IPRJ,IPLO,IEIG,IHIS,IIND,ISHE,IPUN,
     3       INDATA,INPARA,IWRITE,ISTORE,
     4       NDMA,NDMI,MAXI,MAXP,IINI,IREL,ANUL,
     5       ICON,IPRO,IDIS,INOR,ILEV,
     6       NDAT,ND,NDAAT,NPNP,NV,NSY1,NSY2,NSY3,NSY4,N8
      CHARACTER*4 FORM3(3),FORM4(2)
      CHARACTER*80 TITLE,FORM1,FORM2
      CHARACTER*60 T1,T2
      CHARACTER*40 HINI,HFIN
      DIMENSION  H1(NP),H2(NP),H3(NP),H4(NP),H5(NP),H6(NP),H7(NP),
     1           H8(NP),H9(NP),H18(N8),LAB(NP),V(NV)
      DIMENSION  DATA(NDAT),DISP(NDAT),DIST(NDAT),IND(NSY3),
     1           IORD(NSY1),LBK(NSY1),W(NDAT),WT(NP,NP),DT(NP,NP)
      DIMENSION  X(NP,ND),Y(NP,ND),XY(NP,ND)
      DIMENSION  HULP(NSY2),HELP(NSY2),DHELP(NP,NP),WS(NSY4)
C     DATA HINI/4H-DIM,4HENSI,4HONAL,4H INI,4HTIAL,
C    1          4H SOL,4HUTIO,4HN   ,4H    ,4H    /
C     DATA HFIN/4H-DIM,4HENSI,4HONAL,4H FIN,4HAL S,
C    1          4HOLUT,4HION ,4H    ,4H    ,4H    /
      DATA HINI/'-DIMENSIONAL INITIAL SOLUTION           '/
      DATA HFIN/'-DIMENSIONAL FINAL SOLUTION             '/
C     DATA HINI/'-DIM','ENSI','ONAL',' INI','TIAL',
C    1          ' SOL','UTIO','N   ','    ','    '/
C     DATA HFIN/'-DIM','ENSI','ONAL',' FIN','AL S',
C    1          'OLUT','ION ','    ','    ','    '/
C     DATA T1/4HITER,4HATIO,4HN * ,4H  TO,4HTAL ,4H  * ,4H  TO,4HTAL ,
C    2        4H  * ,4H  PR,4HOPER,4H  * ,4H  PR,4HOPER,4H  * /
C     DATA T2/4H NUM,4HBER ,4H  * ,4H  ST,4HRESS,4H  * ,4HDECR,4HEASE,
C    2        4H  * ,4H  ST,4HRESS,4H  * ,4HDECR,4HEASE,4H  * /
      DATA T1/'ITERATION *  TOTAL   *   TOTAL   *   PROPER  *   PROPER
     2* '/
      DATA T2/' NUMBER   *  STRESS  * DECREASE  *   STRESS  * DECREASE
     2  '/
C     DATA T1/'ITER','ATIO','N * ','  TO','TAL ','  * ','  TO','TAL ',
C    2        '  * ','  PR','OPER','  * ','  PR','OPER','  * '/
C     DATA T2/' NUM','BER ','  * ','  ST','RESS','  * ','DECR','EASE',
C    2        '  * ','  ST','RESS','  * ','DECR','EASE','  * '/
C     DATA FORM3/4H(8X,,4H6F12,4H.7) /
      DATA FORM3/'(8X,','6F12','.7) '/
C     DATA FORM4/4H(10A,4H8)  /
      DATA FORM4/'(10A','8)  '/
      INTEGER SITER
       CHARACTER*16 CHARDATA,CHARPARM
      DOUBLE PRECISION LAB,V
      LOGICAL LPRE
      LOGICAL LAST
C----------------------------------------------------------------------C
      INPARA=11
C      INDATA=12
C      CALL GETARG(1,CHARDATA)
C      CALL GETARG(2,CHARPARM)
      OPEN(INDATA,FILE="smac.data")
      OPEN(INPARA,FILE="smac.parm")
      CALL INITIA(DT,WT,DISP,DIST,W,V,NP,NDAT,NV)
      FINP=1.0/FLOAT(NP)
      FINR=1.0/FLOAT(NR)
      FIPR=1.0/FLOAT(NP*NR)
         IF (ICON.NE.0)               XSTAN=FINP+FINP
         IF (ICON.EQ.0.AND.ISYM.NE.1) XSTAN=1.
         IF (ICON.EQ.0.AND.ISYM.EQ.1) XSTAN=2.
      AHAT=1.
         IF (INOR.LT.0.AND.IMIS.NE.1) IWHT=IWEI
         IF (INOR.GT.0.OR. IMIS.EQ.1) IWHT=1
         IF (ICON.EQ.0.AND.ISYM.NE.1) ISS=0
         IF (ICON.NE.0.OR. ISYM.EQ.1) ISS=1
C----------------------------------------------------------------------C
C  READ, ACCUMULATE AND NORMALIZE DATA (AND WEIGHTS)                   C
C      OPTIONALLY PRINT DATA (AND WEIGHTS)                             C
C----------------------------------------------------------------------C
      CALL NINPU(DATA,DISP,DIST,IORD,LBK,W,WT,DT,HULP,HELP,
     1           DHELP,LAB,FORM1,FORM2,FORM4,
     2           NDAT,NDAAT,NSY1,NSY2,NP,NR,NPNP,
     3           FINR,VMIS,XSTAN,
     4           IWEI,IMIS,IDIS,INDATA,INPARA,IPRI,IWRITE,ILEV,IPRO,
     5           ILAB,INOR,ISYM,ICON)
C----------------------------------------------------------------------C
C  COMPUTE OR READ AN INITIAL CONFIGURATION                            C
C----------------------------------------------------------------------C
      CALL INITW(X,DISP,HELP,W,V,H1,H2,H3,H4,H5,H6,H7,H8,H9,IND,
     1     FORM3,NP,ND,NDAT,NV,IINI,IWEI,INDATA,IWRITE,INOR,DIST)
      IF (IEIG.EQ.0) GO TO 10
         WRITE(IWRITE,3001) TITLE
         CALL PRITEX(X,NP,ND,NP,LAB,IWRITE,ILAB)
         IF (IINI.EQ.0) WRITE(IWRITE,3002) (H4(K),K=1,NP)
 10   IPIN=IPUN/10
      IF (IEIG.NE.0) IEIG=IPLO
      CALL OUTP1(X,DIST,IND,H1,HINI,
     1           IEIG,IPIN,NP,ND,NDAT,IWRITE,ISTORE)
      IPUN=IPUN-IPIN*10
C----------------------------------------------------------------------C
C  START SMACOF ITERATIONS                                             C
C----------------------------------------------------------------------C
 20   LAST=.FALSE.
      IF (ILEV.NE.3) GOTO 80
         IMAX=MAXP
         CRIT=CRIP
         IMEA=1
         LPRE=.TRUE.
 30   SITER=0
C----------------------------------------------------------------------C
      IF (ISS.EQ.0) CALL DFITW
     1        (X,DISP,DIST,HELP(1),HULP,IORD,LBK,SIG1,SIG2,SIG3,W,
     2         HELP(NDAT+1),NDAAT,NP,ND,NDAT,NR,IPRO,IMEA,
     3         FINR,FIPR,FINP,INOR,IWEI,IWHT,IWRITE,XSTAN)
      IF (ISS.EQ.1) CALL DFITV(X,DISP,DHELP,DIST,HELP,HULP(1),
     1         HULP(NPNP+1),IORD,LBK,SIG1,SIG2,SIG3,SIG4,WT,
     2         HELP(NPNP+1),W,NDAAT,NP,ND,NDAT,NR,NPNP,NSY1,NSY2,IPRO,
     3         IMEA,FINR,FIPR,FINP,INOR,IWEI,IWHT,ICON,IWRITE,XSTAN)
      IF (IHIS.EQ.1) CALL TITIT
     1           (LPRE,IREL,ISS,IWRITE,NR,ND,TITLE,T1,T2,20,15)
      IF (IHIS.EQ.1) CALL STRII(ILEV,LPRE,IWRITE,NR,ISS,SITER,
     1           SIG1,SIG2,SIG3,SIG4,OSIG1,OSIG2)
C----------------------------------------------------------------------C
      DO 60 SITER=1,IMAX
         CALL UPDAW(X,Y,DISP,DIST,W,V,HELP,
     1              H1,H2,IWHT,NP,ND,NDAT,NV,FINP,INOR,IWRITE)
         IF (IREL.EQ.2) CALL RLAXN(X,Y,XY,DISP,DIST,W,HELP,
     1       SITER,IWHT,IWRITE,
     2       SIG2,ANUL,AHAT,CRIT,FINR,NP,ND,NDAT,NSTEP)
         IF (IREL.NE.2) CALL RLAX(X,Y,SITER,IREL,NP,ND,NP)
         OSIG1=SIG1
         OSIG2=SIG2
         IF (ISS.EQ.0.AND.ICON.EQ.0) CALL DFITW
     1       (X,DISP,DIST,HELP(1),HULP,IORD,LBK,SIG1,SIG2,SIG3,W,
     2       HELP(NDAT+1),NDAAT,NP,ND,NDAT,NR,IPRO,IMEA,
     3       FINR,FIPR,FINP,INOR,IWEI,IWHT,IWRITE,XSTAN)
         IF (ISS.EQ.1.OR.ICON.NE.0) CALL DFITV
     1       (X,DISP,DHELP,DIST,HELP,HULP(1),HULP(NPNP+1),
     2       IORD,LBK,SIG1,SIG2,SIG3,SIG4,WT,HELP(NPNP+1),
     3       W,NDAAT,NP,ND,NDAT,NR,NPNP,NSY1,NSY2,IPRO,IMEA,
     4       FINR,FIPR,FINP,INOR,IWEI,IWHT,ICON,IWRITE,XSTAN)
         EPS1=OSIG1-SIG1
         EPS2=OSIG2-SIG2
         IF (ABS(EPS1).LT.CRIT) GOTO 70
         IF (IHIS.EQ.1) CALL STRIT(IREL,ISS,IWRITE,NR,SITER,
     1                  SIG1,SIG2,SIG3,SIG4,EPS1,EPS2,AHAT,NSTEP)
 60     CONTINUE
C----------------------------------------------------------------------C
C  MAXIMUM NUMBER OF ITERATIONS EXCEEDED                               C
C----------------------------------------------------------------------C
      IF      (LPRE) WRITE(IWRITE,1001)
      IF (.NOT.LPRE) WRITE(IWRITE,1002)
      GO TO 80
C----------------------------------------------------------------------C
C  NORMAL END SMACOF ITERATIONS                                        C
C----------------------------------------------------------------------C
 70   IF (IHIS.EQ.1) CALL STRIE(IHIS,IWRITE,NR,ISS,SITER,
     1                          SIG1,SIG2,SIG3,SIG4,EPS1,EPS2)
 80   IF (LAST) GOTO 90
          LAST=.TRUE.
          LPRE=.FALSE.
          IMAX=MAXI
          IMEA=ILEV
          IF (ILEV.EQ.3) IMEA=2
          CRIT=CRII
          IF (ILEV.EQ.3) OSIG1=SIG1
          IF (ILEV.EQ.3) OSIG2=SIG2
               GOTO 30
C----------------------------------------------------------------------C
 90   CALL ROTA(X,HELP,H1,H2,NP,ND,NP,IWRITE)
      IF (IHIS.EQ.0) CALL STRIE(IHIS,IWRITE,NR,ISS,SITER,
     1                          SIG1,SIG2,SIG3,SIG4,EPS1,EPS2)
      IF ((NR.EQ.1.AND.ICON.EQ.0).OR.SIG1.LE.1.0E-06.OR.IIND.EQ.0)
     1         GOTO 100
      IF (ISS.EQ.0)
     1    CALL STRIND(DIST,HELP,HULP(1),IORD,LBK,DIST,HULP(NDAT+1),
     2         NDAT,NDAAT,NDAT,NSY1,NDAAT,IPRO,IMEA,IWRITE,NP,NR,
     3         FINP,SIG1,INOR,IWEI,ISS,ICON,H9,H18,8,XSTAN,IIND)
      IF (ISS.EQ.1)
     1    CALL STRIND(DIST,HELP,HULP(1),IORD,LBK,DHELP,HULP(NPNP+1),
     2         NDAT,NDAAT,NPNP,NSY1,NSY2,IPRO,IMEA,IWRITE,NP,NR,
     3         FINP,SIG1,INOR,IWEI,ISS,ICON,H9,H18,8,XSTAN,IIND)
C----------------------------------------------------------------------C
 100  WRITE(IWRITE,4000) ND,TITLE
      CALL PRITEX(X,NP,ND,NP,LAB,IWRITE,ILAB)
      CALL OUTP1(X,DIST,IND,H1,HFIN,
     1           IPLO,IPUN,NP,ND,NDAT,IWRITE,ISTORE)
      IF ((ISHE.GE.0.OR.IPRJ.GE.0).AND.ISS.EQ.0) CALL WSHEP
     1         (DATA,DISP,DIST,HELP,HULP,W,WS,IORD,LBK,IND,NDAT,NDAAT,
     2         NP,NR,ISTORE,IWRITE,ISHE,IPRO,IMEA,IPRJ,INOR,IWEI,XSTAN)
      IF ((ISHE.GE.0.OR.IPRJ.GE.0).AND.ISS.EQ.1) CALL VSHEP
     1         (DT,DISP,DIST,HELP,HULP,WT,WS,IORD,LBK,DHELP,IND,
     2          NDAT,NDAAT,NSY2,NPNP,NSY3,NSY1,NP,NR,ISTORE,IWRITE,
     3          ISHE,IPRO,IMEA,IPRJ,INOR,IWEI,ICON,XSTAN)
      IF (NDMI.GE.ND) RETURN
C----------------------------------------------------------------------C
         ND=ND-1
      DO 110 L=1,NDAT
         DISP(L)=DATA(L)
 110  CONTINUE
          IF (ILEV.EQ.3) ILEV=2
         GO TO 20
C----------------------------------------------------------------------C
C  END OF COMPUTATIONS                                                 C
C----------------------------------------------------------------------C
 1001 FORMAT(50H0MAXIMUM NUMBER OF PRELIMINARY ITERATIONS EXCEEDED)
 1002 FORMAT(44H0MAXIMUM NUMBER OF FINAL ITERATIONS EXCEEDED)
 3001 FORMAT(1H1,21HINITIAL CONFIGURATION,2X,A80//)
 3002 FORMAT(///21H0THE EIGENVALUES ARE://(1H0,7X,10F9.5))
 4000 FORMAT(1H1,5HFINAL,I2,31H-DIMENSIONAL CONFIGURATION FOR ,A80)
      END
      SUBROUTINE SQNORW(X,W,N,Q,IWEI)
C     ******************************************************************
C     *                                                                *
C     *  S Q N O R W                                                   *
C     *                                                                *
C     *  PURPOSE: NORMALIZE X SUCH THAT SSQ(X)=K                       *
C     *           SSQ : (WEIGHTED) SUM OF SQUARES                      *
C     *                                                                *
C     *  SUBROUTINES CALLED: NONE                                      *
C     *                                                                *
C     *  AUTHORS : INEKE STOOP                 RELEASED JANUARY 1981   *
C     *                                                                *
C     ******************************************************************
      DIMENSION X(N),W(N)
C
      STAND=0.
      IF (IWEI.EQ.0) GOTO 20
      DO 10 L=1,N
         STAND=STAND+X(L)*X(L)*W(L)
 10   CONTINUE
      GOTO 40
 20   DO 30 L=1,N
         STAND=STAND+X(L)*X(L)
 30   CONTINUE
 40   IF (STAND.LE.1.0E-06) RETURN
      STAND=SQRT(Q/STAND)
      DO 50 L=1,N
         X(L)=X(L)*STAND
 50   CONTINUE
      RETURN
      END
      SUBROUTINE SSQ(SS,X,W,N,IWEI)
C     ******************************************************************
C     *                                                                *
C     *  S S Q                                                         *
C     *                                                                *
C     *  PURPOSE: COMPUTE (WEIGHTED) SUM OF SQUARES OF X               *
C     *                                                                *
C     *  SUBROUTINES CALLED: NONE                                      *
C     *                                                                *
C     *  AUTHORS : INEKE STOOP                RELEASED FEBRUARY 1982   *
C     *                                                                *
C     ******************************************************************
      DIMENSION X(N),W(N)
C
      SS=0.
      IF (IWEI.EQ.0) GOTO 20
      DO 10 L=1,N
         SS=SS+X(L)*X(L)*W(L)
 10   CONTINUE
      RETURN
 20   DO 30 L=1,N
         SS=SS+X(L)*X(L)
 30   CONTINUE
      RETURN
      END
      SUBROUTINE SSVAR(VS,VM,X,W,N,IWEI,NDAAT)
C     ******************************************************************
C     *                                                                *
C     *  S S V A R                                                     *
C     *                                                                *
C     *  PURPOSE: COMPUTE SSQ(X-M) AND M                               *
C     *                                                                *
C     *  SUBROUTINES CALLED: NONE                                      *
C     *                                                                *
C     *  AUTHORS : INEKE STOOP                     RELEASED MAY 1982   *
C     *                                                                *
C     ******************************************************************
      DIMENSION X(N),W(N)
C
      VM=0.
      VS=0.
      IF (IWEI.EQ.0) GOTO 20
      XW=0.
      DO 10 L=1,N
         XW=XW+W(L)
         VM=VM+X(L)*W(L)
 10   CONTINUE
      IF (XW.GT.1.0E-6) GOTO 40
         VS=0.
      RETURN
 20   DO 30 L=1,N
         VM=VM+X(L)
 30   CONTINUE
      XW=FLOAT(NDAAT)
 40   VM=VM/XW
      DO 50 L=1,N
         DIFF=X(L)-VM
         IF (IWEI.EQ.0) VS=VS+DIFF*DIFF
         IF (IWEI.EQ.1) VS=VS+DIFF*DIFF*W(L)
 50   CONTINUE
      RETURN
      END
      SUBROUTINE STOREC(DATA,NP,NI,NJ,ISTORE,IWRITE,NUM)
C     ******************************************************************
C     *                                                                *
C     *  S T O R E C                                                   *
C     *                                                                *
C     *  PURPOSE: - STORE ASYMMETRIC DISPARITY MATRICES                *
C     *                                                                *
C     *  SUBROUTINES CALLED: NONE                                      *
C     *                                                                *
C     ******************************************************************
      DIMENSION DATA(NI,NJ)
            IF (NUM.EQ.0) WRITE(ISTORE,100)
            IF (NUM.EQ.0) WRITE(IWRITE,150) ISTORE
            IF (NUM.GT.0) WRITE(ISTORE,200) NUM
            IF (NUM.GT.0) WRITE(IWRITE,250) NUM,ISTORE
      WRITE(ISTORE,300) (I,(DATA(I,J),J=1,NJ),I=1,NI)
      RETURN
 100  FORMAT(///28H AGGREGATED DISPARITY MATRIX)
 150  FORMAT(43H0AGGREGATED DISPARITY MATRIX STORED ON UNIT ,I5)
 200  FORMAT(///36H DISPARITY MATRIX OF REPLICATION NO  ,I5)
 250  FORMAT(35H0DISPARITY MATRIX OF REPLICATION NO ,I5,
     1       16H STORED ON UNIT ,I5)
 300  FORMAT(I5,8F9.5,(5X,8F9.5))
      END
      SUBROUTINE STOTRI(DATA,NP,NDAT,ISTORE,IWRITE,NUM)
C     ******************************************************************
C     *                                                                *
C     *  S T O T R I                                                   *
C     *                                                                *
C     *  PURPOSE: - STORE SYMMETRIC DISPARITY MATRICES (LOWER HALF)    *
C     *                                                                *
C     *  SUBROUTINES CALLED: NONE                                      *
C     *                                                                *
C     ******************************************************************
      DIMENSION DATA(NDAT)
C                                                                      C
            IF (NUM.EQ.0) WRITE(ISTORE,100)
            IF (NUM.EQ.0) WRITE(IWRITE,150) ISTORE
            IF (NUM.GT.0) WRITE(ISTORE,200) NUM
            IF (NUM.GT.0) WRITE(IWRITE,250) NUM,ISTORE
      LE=0
      DO 10 I=2,NP
         LB=LE+1
         LE=LE+I-1
         WRITE(ISTORE,300) I,(DATA(L),L=LB,LE)
 10   CONTINUE
      RETURN
 100  FORMAT(///36H AGGREGATED DISPARITY MATRIX         )
 150  FORMAT(43H0AGGREGATED DISPARITY MATRIX STORED ON UNIT ,I5)
 200  FORMAT(///36H DISPARITY MATRIX OF REPLICATION NO  ,I5)
 250  FORMAT(35H0DISPARITY MATRIX OF REPLICATION NO ,I5,
     1       16H STORED ON UNIT ,I5)
 300  FORMAT(I5,8F9.5,(5X,8F9.5))
      END
      SUBROUTINE STRETC(Y,X,NDAT,NP)
C     ******************************************************************
C     *                                                                *
C     *    S T R E T C                                                 *
C     *                                                                *
C     *     PURPOSE : CONVERTS LOWER TRIANGLE VECTOR X TO MATRIX Y     *
C     *                                                                *
C     *    AUTHOR : INEKE STOOP                                        *
C     *                                                                *
C     ******************************************************************
      DIMENSION X(NDAT),Y(NP,NP)
      L=0
      Y(1,1)=0.
      DO 20 J=2,NP
         Y(J,J)=0.
         IMIN=J-1
         DO 10 I=1,IMIN
            L=L+1
            Y(I,J)=X(L)
            Y(J,I)=X(L)
  10     CONTINUE
  20  CONTINUE
      RETURN
      END
      SUBROUTINE STRIE(IHIS,IWRITE,NR,ISYM,SITER,SIG1,SIG2,SIG3,SIG4,
     1                 EPS1,EPS2)
C     ******************************************************************
C     *                                                                *
C     *  S T R I E                                                     *
C     *                                                                *
C     *  PURPOSE: PRINTS APPROPRIATE FINAL STRESSES                    *
C     *                                                                *
C     *  SUBROUTINES CALLED: NONE                                      *
C     *                                                                *
C     *  AUTHOR: INEKE STOOP                                JUNE 1982  *
C     *                                                                *
C     ******************************************************************
      INTEGER SITER
C----------------------------------------------------------------------C
      IF (IHIS.EQ.0) GOTO 10
        WRITE(IWRITE,100)
         IF (ISYM.EQ.0.AND.NR.GT.1)
     1       WRITE(IWRITE,200) SITER,SIG1,EPS1,SIG2,EPS2,SIG3
         IF (ISYM.EQ.1.AND.NR.GT.1)
     1       WRITE(IWRITE,200) SITER,SIG1,EPS1,SIG2,EPS2,SIG3,SIG4
         IF (ISYM.EQ.0.AND.NR.EQ.1)
     1       WRITE(IWRITE,200) SITER,SIG1,EPS1,SIG2,EPS2
         IF (ISYM.EQ.1.AND.NR.EQ.1)
     1       WRITE(IWRITE,200) SITER,SIG1,EPS1,SIG2,EPS2,SIG4
      RETURN
C----------------------------------------------------------------------C
 10                   WRITE(IWRITE,300) SIG1,SIG2
      IF (NR  .GT.1)  WRITE(IWRITE,400) SIG3
      IF (ISYM .EQ.1) WRITE(IWRITE,500) SIG4
                      WRITE(IWRITE,600) SITER
      RETURN
 100  FORMAT(55H0NO FURTHER IMPROVEMENT OF STRESS;THE FINAL VALUES ARE:)
 200  FORMAT(1H0,11X,I3,3X,7F12.6)
 300  FORMAT(///46H0FINAL VALUE OF                  TOTAL STRESS:,F12.6,
     1       ///46H0FINAL VALUE OF                 PROPER STRESS:,F12.6)
 400  FORMAT(///46H0FINAL VALUE OF INDIVIDUAL DIFFERENCES STRESS:,F12.6)
 500  FORMAT(///46H0FINAL VALUE OF              ASYMMETRY STRESS:,F12.6)
 600  FORMAT(///23H0CONVERGENCE REACHED IN,I4,11H ITERATIONS)
      END
      SUBROUTINE TINVIT(NM,N,D,E,E2,M,W,IND,Z,                          TIN00010
     X                  IERR,RV1,RV2,RV3,RV4,RV6)                       TIN00020
C     ******************************************************************TIN00030
C     *                                                                *TIN00040
C     *  T I N V I T                                                   *TIN00050
C     *                                                                *TIN00060
C     *  PURPOSE: DETERMINES M EIGENVECTORS OF A SYMMETRIC TRIDIAGONAL *TIN00070
C     *           MATRIX CORRESPONDING TO A SET OF ORDERED APPROXIMATE *TIN00080
C     *           EIGENVALUES, USING INVERSE ITERATION                 *TIN00090
C     *                                                                *TIN00100
C     *  SUBROUTINES CALLED: NONE                                      *TIN00110
C     *                                                                *TIN00120
C     *  ADAPTED FROM EISPACK                                          *TIN00130
C     *                                                                *TIN00140
C     ******************************************************************TIN00150
      DIMENSION Z(NM,M),D(N),E(N),E2(N),W(M),RV1(N),RV2(N),RV3(N),      TIN00160
     X          RV4(N),RV6(N),IND(M)                                    TIN00170
      REAL NORM,MACHEP                                                  TIN00180
      INTEGER P,Q,R,S,TAG,GROUP                                         TIN00190
C                                                                       TIN00200
C     ********** MACHEP IS A MACHINE DEPENDENT PARAMETER SPECIFYING     TIN00210
C                THE RELATIVE PRECISION OF  FLOATING POINT ARITHMATIC.  TIN00220
C                                                                       TIN00230
C                **********                                             TIN00240
      MACHEP=2.0**(-20)                                                 TIN00250
C                                                                       TIN00260
      IERR=0                                                            TIN00270
      TAG=0                                                             TIN00280
      ORDER = 1.0 - E2(1)                                               TIN00290
      Q = 0                                                             TIN00300
C     ********** ESTABLISH AND PROCESS NEXT SUBMATRIX **********        TIN00310
  100 P = Q + 1                                                         TIN00320
      DO 120 Q = P, N                                                   TIN00330
         IF (Q .EQ. N) GO TO 140                                        TIN00340
         IF (E2(Q+1) .EQ. 0.0) GO TO 140                                TIN00350
  120 CONTINUE                                                          TIN00360
C     ********** FIND VECTORS BY INVERSE ITERATION **********           TIN00370
  140 TAG = TAG + 1                                                     TIN00380
      S = 0                                                             TIN00390
C                                                                       TIN00400
      DO 920 R = 1, M                                                   TIN00410
         IF (IND(R) .NE. TAG) GO TO 920                                 TIN00420
         ITS = 1                                                        TIN00430
         X1 = W(R)                                                      TIN00440
         IF (S .NE. 0) GO TO 510                                        TIN00450
C     ********** CHECK FOR ISOLATED ROOT **********                     TIN00460
         XU = 1.0                                                       TIN00470
         IF (P .NE. Q) GO TO 490                                        TIN00480
         RV6(P) = 1.0                                                   TIN00490
         GO TO 870                                                      TIN00500
  490    NORM = ABS(D(P))                                               TIN00510
         IP = P + 1                                                     TIN00520
C                                                                       TIN00530
         DO 500 I = IP, Q                                               TIN00540
  500    NORM = NORM + ABS(D(I)) + ABS(E(I))                            TIN00550
C     ********** EPS2 IS THE CRITERION FOR GROUPING                     TIN00560
C                EPS3 REPLACES ZERO PIVOTS AND EQUAL                    TIN00570
C                ROOTS ARE MODIFIED BY EPS3,                            TIN00580
C                EPS4 IS TAKEN VERY SMALL TO AVOID OVERFLOW **********  TIN00590
         EPS2 = 1.0E-3 * NORM                                           TIN00600
         EPS3 = MACHEP * NORM                                           TIN00610
         UK = FLOAT(Q-P+1)                                              TIN00620
         EPS4 = UK * EPS3                                               TIN00630
         UK = EPS4 / SQRT(UK)                                           TIN00640
         S = P                                                          TIN00650
  505    GROUP = 0                                                      TIN00660
         GO TO 520                                                      TIN00670
C     ********** LOOK FOR CLOSE OR COINCIDENT ROOTS **********          TIN00680
  510    IF (ABS(X1-X0) .GE. EPS2) GO TO 505                            TIN00690
         GROUP = GROUP + 1                                              TIN00700
         IF (ORDER * (X1 - X0) .LE. 0.0) X1 = X0 + ORDER * EPS3         TIN00710
C     ********** ELIMINATION WITH INTERCHANGES AND                      TIN00720
C                INITIALIZATION OF VECTOR **********                    TIN00730
  520    V = 0.0                                                        TIN00740
C                                                                       TIN00750
         DO 580 I = P, Q                                                TIN00760
            RV6(I) = UK                                                 TIN00770
            IF (I .EQ. P) GO TO 560                                     TIN00780
            IF (ABS(E(I)) .LT. ABS(U)) GO TO 540                        TIN00790
C     ********** WARNING -- A DIVIDE CHECK MAY OCCUR HERE IF            TIN00800
C                E2 ARRAY HAS NOT BEEN SPECIFIED CORRECTLY **********   TIN00810
            XU = U / E(I)                                               TIN00820
            RV4(I) = XU                                                 TIN00830
            RV1(I-1) = E(I)                                             TIN00840
            RV2(I-1) = D(I) - X1                                        TIN00850
            RV3(I-1) = 0.0                                              TIN00860
            IF (I .NE.Q) RV3(I-1) = E(I+1)                              TIN00870
            U = V - XU * RV2(I-1)                                       TIN00880
            V = -XU * RV3(I-1)                                          TIN00890
            GO TO 580                                                   TIN00900
  540       XU = E(I) / U                                               TIN00910
            RV4(I) = XU                                                 TIN00920
            RV1(I-1) = U                                                TIN00930
            RV2(I-1) = V                                                TIN00940
            RV3(I-1) = 0.0                                              TIN00950
  560       U = D(I) - X1 - XU * V                                      TIN00960
            IF (I .NE. Q) V = E(I+1)                                    TIN00970
  580    CONTINUE                                                       TIN00980
C                                                                       TIN00990
         IF (U .EQ.  0.0) U = EPS3                                      TIN01000
         RV1(Q) = U                                                     TIN01010
         RV2(Q) = 0.0                                                   TIN01020
         RV3(Q) = 0.0                                                   TIN01030
C     ********** BACK SUBSTITION                                        TIN01040
C                FOR I=Q STEP -1 UNTIL P DO -- **********               TIN01050
  600    DO 620 II = P, Q                                               TIN01060
            I = P + Q - II                                              TIN01070
            RV6(I) = (RV6(I) - U * RV2(I) - V * RV3(I)) / RV1(I)        TIN01080
            V = U                                                       TIN01090
            U = RV6(I)                                                  TIN01100
  620    CONTINUE                                                       TIN01110
C     ********** ORTHOGONALIZE WITH RESPECT TO PREVIOUS                 TIN01120
C                MEMBERS OF GROUP **********                            TIN01130
         IF (GROUP .EQ.0) GO TO 700                                     TIN01140
         J = R                                                          TIN01150
C                                                                       TIN01160
         DO 680 JJ = 1, GROUP                                           TIN01170
  630       J = J - 1                                                   TIN01180
            IF (IND(J) .NE. TAG) GO TO  630                             TIN01190
            XU = 0.0                                                    TIN01200
C                                                                       TIN01210
            DO 640 I = P, Q                                             TIN01220
  640       XU = XU + RV6(I) * Z(I,J)                                   TIN01230
C                                                                       TIN01240
            DO 660 I = P, Q                                             TIN01250
  660       RV6(I) = RV6(I) - XU * Z(I,J)                               TIN01260
C                                                                       TIN01270
  680    CONTINUE                                                       TIN01280
C                                                                       TIN01290
  700    NORM = 0.0                                                     TIN01300
C                                                                       TIN01310
         DO 720 I = P, Q                                                TIN01320
  720    NORM = NORM + ABS(RV6(I))                                      TIN01330
C                                                                       TIN01340
         IF (NORM .GE. 1.0) GO TO 840                                   TIN01350
C     ********** FORWARD SUBSTITUTION **********                        TIN01360
         IF (ITS .EQ. 5) GO TO 830                                      TIN01370
         IF (NORM .NE. 0.0) GO TO 740                                   TIN01380
         RV6(S) = EPS4                                                  TIN01390
         S = S + 1                                                      TIN01400
         IF (S .GT. Q) S = P                                            TIN01410
         GO TO 780                                                      TIN01420
  740    XU = EPS4 / NORM                                               TIN01430
C                                                                       TIN01440
         DO 760 I = P, Q                                                TIN01450
  760    RV6(I) = RV6(I) * XU                                           TIN01460
C     ********** ELIMINATION OPERATIONS ON NEXT VECTOR                  TIN01470
C                ITERATE **********                                     TIN01480
  780    DO 820 I = IP, Q                                               TIN01490
            U = RV6(I)                                                  TIN01500
C     ********** IF RV1(I-1) .EQ. E(I), A ROW INTERCHANGE               TIN01510
C                WAS PERFORMED EARLIER IN THE                           TIN01520
C                TRIANGULARIZATION PROCESS **********                   TIN01530
            IF (RV1(I-1) .NE. E(I)) GO TO 800                           TIN01540
            U = RV6(I-1)                                                TIN01550
            RV6(I-1) = RV6(I)                                           TIN01560
  800       RV6(I) = U - RV4(I) * RV6(I-1)                              TIN01570
  820    CONTINUE                                                       TIN01580
C                                                                       TIN01590
         ITS = ITS + 1                                                  TIN01600
         GO TO 600                                                      TIN01610
C     ********** SET ERROR -- NON-CONVERGED EIGENVECTOR **********      TIN01620
  830    IERR = -R                                                      TIN01630
         XU = 0.0                                                       TIN01640
         GO TO 870                                                      TIN01650
C     ********* NORMALIZE SO THAT SUM OF SQUARES IS 1 AND EXPAND TO     TIN01660
C              FULL ORDER **********                                    TIN01670
  840    U = 0.0                                                        TIN01680
C                                                                       TIN01690
         DO 860 I = P, Q                                                TIN01700
  860    U = U + RV6(I)**2                                              TIN01710
C                                                                       TIN01720
         XU = 1.0 / SQRT(U)                                             TIN01730
C                                                                       TIN01740
  870    DO 880 I = 1, N                                                TIN01750
  880    Z(I,R) = 0.0                                                   TIN01760
C                                                                       TIN01770
         DO 900 I = P, Q                                                TIN01780
  900    Z(I,R) = RV6(I) * XU                                           TIN01790
C                                                                       TIN01800
         X0 = X1                                                        TIN01810
  920 CONTINUE                                                          TIN01820
C                                                                       TIN01830
      IF (Q .LT. N) GO TO 100                                           TIN01840
      RETURN                                                            TIN01850
      END                                                               TIN01860
      SUBROUTINE TRBAK1(NM,N,A,E,M,Z)                                   TRB00010
C     ******************************************************************TRB00020
C     *                                                                *TRB00030
C     *  T R B A K 1                                                   *TRB00040
C     *                                                                *TRB00050
C     *  PURPOSE: FORMS THE EIGENVECTORS OF A REAL SYMMETRIC MATRIX    *TRB00060
C     *           FROM THE EIGENVECTORS OF THAT SYMMETRIC TRIDIAGONAL  *TRB00070
C     *           MATRIX DETERMINED BY TRED1                           *TRB00080
C     *                                                                *TRB00090
C     *  SUBROUTINES CALLED: NONE                                      *TRB00100
C     *                                                                *TRB00110
C     *  ADAPTED FROM EISPACK                                          *TRB00120
C     *                                                                *TRB00130
C     ******************************************************************TRB00140
      DIMENSION A(NM,N),E(N),Z(NM,M)                                    TRB00150
      REAL H,S                                                          TRB00160
C                                                                       TRB00170
      IF(N.EQ.1) GO TO 200                                              TRB00180
C                                                                       TRB00190
      DO 140 I = 2, N                                                   TRB00200
         L = I - 1                                                      TRB00210
C     ********** H BELOW IS NEGATIVE OF H FORMED IN TRED1 **********    TRB00220
         H = E(I) * A(I,L)                                              TRB00230
      IF (H .EQ. 0.0) GO TO 140                                         TRB00240
C                                                                       TRB00250
         DO 130 J = 1, M                                                TRB00260
            S = 0.0                                                     TRB00270
C                                                                       TRB00280
            DO 110 K = 1, L                                             TRB00290
  110       S = S + A(I,K) * Z(K,J)                                     TRB00300
C                                                                       TRB00310
            S = S / H                                                   TRB00320
C                                                                       TRB00330
            DO 120 K = 1, L                                             TRB00340
  120       Z(K,J) = Z(K,J) + S * A(I,K)                                TRB00350
C                                                                       TRB00360
  130    CONTINUE                                                       TRB00370
C                                                                       TRB00380
  140 CONTINUE                                                          TRB00390
C                                                                       TRB00400
  200 RETURN                                                            TRB00410
      END                                                               TRB00420
      SUBROUTINE TRED1(NM,N,A,D,E,E2)                                   TR100010
C     ******************************************************************TR100020
C     *                                                                *TR100030
C     *  T R E D 1                                                     *TR100040
C     *                                                                *TR100050
C     *  PURPOSE: REDUCES A REAL SYMMETRIC MATRIX TO A SYMMETRIC TRI-  *TR100060
C     *           DIAGONAL MATRIX USING ORTHOGONAL SIMILARITY TRANS-   *TR100070
C     *           FORMATIONS                                           *TR100080
C     *                                                                *TR100090
C     *  SUBROUTINES CALLED: NONE                                      *TR100100
C     *                                                                *TR100110
C     *  ADAPTED FROM EISPACK                                          *TR100120
C     *                                                                *TR100130
C     ******************************************************************TR100140
      DIMENSION A(NM,N),D(N),E(N),E2(N)                                 TR100150
C                                                                      CTR100160
      DO 100 I = 1, N                                                   TR100170
  100 D(I) = A(I,I)                                                     TR100180
C     ********** FOR I=N STEP -1 UNTIL 1 DO **********                  TR100190
      DO 300 II = 1, N                                                  TR100200
         I = N + 1 - II                                                 TR100210
         L = I - 1                                                      TR100220
         H = 0.0                                                        TR100230
         SCALE = 0.0                                                    TR100240
          IF (L .LT. 1) GO TO 130                                       TR100250
C     ********** SCALE ROW ( ALGOL TOL THEN NOT NEEDED **********       TR100260
         DO 120 K = 1, L                                                TR100270
  120    SCALE = SCALE + ABS(A(I,K))                                    TR100280
C                                                                       TR100290
         IF (SCALE .NE. 0.0) GO TO 140                                  TR100300
  130    E(I) = 0.0                                                     TR100310
          E2(I) = 0.0                                                   TR100320
         GO TO 290                                                      TR100330
C                                                                       TR100340
  140    DO 150 K = 1, L                                                TR100350
            A(I,K) = A(I,K) / SCALE                                     TR100360
            H = H + A(I,K) * A(I,K)                                     TR100370
  150    CONTINUE                                                       TR100380
C                                                                       TR100390
         E2(I) = SCALE * SCALE * H                                      TR100400
           F = A(I,L)                                                   TR100410
         G = -SIGN(SQRT(H),F)                                           TR100420
         E(I) = SCALE * G                                               TR100430
         H = H - F * G                                                  TR100440
         A(I,L) = F - G                                                 TR100450
         IF (L .EQ. 1) GO TO 270                                        TR100460
         F = 0.0                                                        TR100470
C                                                                       TR100480
         DO 240 J = 1, L                                                TR100490
            G = 0.0                                                     TR100500
C     ********** FORM ELEMENT OF A *U **********                        TR100510
               DO 180 K = 1, J                                          TR100520
  180          G = G + A(J,K) * A(I,K)                                  TR100530
C                                                                       TR100540
               JP1 = J + 1                                              TR100550
               IF (L .LT. JP1) GO TO 220                                TR100560
               DO 200 K=JP1,L                                           TR100570
  200          G = G + A(K,J) * A(I,K)                                  TR100580
C     ********** FORM ELEMENT OF P **********                           TR100590
  220          E(J) = G / H                                             TR100600
               F = F + E(J) * A(I,J)                                    TR100610
  240     CONTINUE                                                      TR100620
C                                                                       TR100630
         H = F / (H + H)                                                TR100640
C     ********** FORM REDUCED A **********                              TR100650
         DO 260 J = 1, L                                                TR100660
            F = A(I,J)                                                  TR100670
            G = E(J) - H * F                                            TR100680
            E(J) = G                                                    TR100690
C                                                                       TR100700
            DO 260 K = 1, J                                             TR100710
               A(J,K) = A(J,K) - F * E(K) - G * A(I,K)                  TR100720
  260    CONTINUE                                                       TR100730
C                                                                       TR100740
  270    DO 280 K = 1,L                                                 TR100750
  280    A(I,K) = SCALE * A(I,K)                                        TR100760
C                                                                       TR100770
  290    H = D(I)                                                       TR100780
         D(I) = A(I,I)                                                  TR100790
         A(I,I) = H                                                     TR100800
  300 CONTINUE                                                          TR100810
C                                                                       TR100820
      RETURN                                                            TR100830
      END                                                               TR100840
      SUBROUTINE TRED2(NM,N,D,E,Z)                                      TR200010
C     ******************************************************************TR200020
C     *                                                                *TR200030
C     *  T R E D 2                                                     *TR200040
C     *                                                                *TR200050
C     *  PURPOSE: REDUCES A REAL SYMMETRIC MATRIX TO A SYMMETRIC TRI-  *TR200060
C     *           DIAGONAL MATRIX USING ORTHOGONAL SIMILARITY TRANS-   *TR200070
C     *           FORMATIONS; THE ACCUMULATED ORTHOGONAL TRANSFORMA-   *TR200080
C     *           TIONS ARE RETAINED IN Z.                             *TR200090
C     *                                                                *TR200100
C     *  SUBROUTINES CALLED: NONE                                      *TR200110
C     *                                                                *TR200120
C     *  ADAPTED FROM EISPACK                                          *TR200130
C     *                                                                *TR200140
C     ******************************************************************TR200150
      DIMENSION Z(NM,N),D(N),E(N)                                       TR200160
C                                                                       TR200170
      IF (N .EQ. 1) GO TO 320                                           TR200180
C     ********** FOR I=N STEP -1 UNTIL 2 DO -- **********               TR200190
      DO 300 II =2, N                                                   TR200200
         I = N + 2 - II                                                 TR200210
         L = I - 1                                                      TR200220
         H = 0.0                                                        TR200230
         SCALE = 0.0                                                    TR200240
         IF (L .LT.2) GO TO 130                                         TR200250
C     ********** SCALE ROW (ALGOL TOL THEN NOT NEEDED) **********       TR200260
         DO 120 K =1, L                                                 TR200270
  120    SCALE = SCALE + ABS(Z(I,K))                                    TR200280
C                                                                       TR200290
         IF (SCALE .NE. 0.0) GO TO 140                                  TR200300
  130    E(I) = Z(I,L)                                                  TR200310
         GO TO 290                                                      TR200320
C                                                                       TR200330
  140    DO 150 K = 1, L                                                TR200340
            Z(I,K) = Z(I,K) / SCALE                                     TR200350
            H = H + Z(I,K) * Z(I,K)                                     TR200360
  150    CONTINUE                                                       TR200370
C                                                                       TR200380
         F = Z(I,L)                                                     TR200390
         G = -SIGN(SQRT(H),F)                                           TR200400
         E(I) = SCALE * G                                               TR200410
         H = H - F * G                                                  TR200420
         Z(I,L) = F - G                                                 TR200430
         F = 0.0                                                        TR200440
C                                                                       TR200450
         DO 240 J = 1, L                                                TR200460
            Z(J,I) = Z(I,J) / (SCALE * H)                               TR200470
            G = 0.0                                                     TR200480
C     ********** FORM ELEMENT OF A*U **********                         TR200490
            DO 180 K = 1, J                                             TR200500
  180       G = G + Z(J,K) * Z(I,K)                                     TR200510
C                                                                       TR200520
            JP1 = J + 1                                                 TR200530
            IF (L .LT. JP1) GO TO 220                                   TR200540
C                                                                       TR200550
            DO 200 K = JP1, L                                           TR200560
  200       G = G + Z(K,J) * Z(I,K)                                     TR200570
C     ********** FORM ELEMENT OF P **********                           TR200580
  220       E(J) = G / H                                                TR200590
            F = F + E(J) * Z(I,J)                                       TR200600
  240    CONTINUE                                                       TR200610
C                                                                       TR200620
         HH = F / (H + H)                                               TR200630
C     ********** FORM REDUCED A **********                              TR200640
         DO 260 J =1, L                                                 TR200650
            F = Z(I,J)                                                  TR200660
            G = E(J) - HH * F                                           TR200670
            E(J) = G                                                    TR200680
C                                                                       TR200690
            DO 260 K = 1, J                                             TR200700
               Z(J,K) = Z(J,K) - F * E(K) -G * Z(I,K)                   TR200710
  260    CONTINUE                                                       TR200720
C                                                                       TR200730
         DO 280 K = 1, L                                                TR200740
  280    Z(I,K) = SCALE * Z(I,K)                                        TR200750
C                                                                       TR200760
  290    D(I) = H                                                       TR200770
  300 CONTINUE                                                          TR200780
C                                                                       TR200790
  320 D(1) = 0.0                                                        TR200800
      E(1) = 0.0                                                        TR200810
C     ********** ACCUMULATION OF TRANSFORMATION MATRICES **********     TR200820
      DO 500 I = 1, N                                                   TR200830
         L = I - 1                                                      TR200840
         IF (D(I) .EQ. 0.0) GO TO 380                                   TR200850
C                                                                       TR200860
C                                                                       TR200870
         DO 360 J = 1, L                                                TR200880
            G = 0.0                                                     TR200890
            DO 340 K =1, L                                              TR200900
  340       G = G + Z(I,K) * Z(K,J)                                     TR200910
C                                                                       TR200920
            DO 360 K = 1, L                                             TR200930
               Z(K,J) = Z(K,J) - G * Z(K,I)                             TR200940
  360    CONTINUE                                                       TR200950
C                                                                       TR200960
  380    D(I) = Z(I,I)                                                  TR200970
         Z(I,I) = 1.0                                                   TR200980
         IF (L .LT. 1) GO TO 500                                        TR200990
C                                                                       TR201000
         DO 400 J =1, L                                                 TR201010
            Z(I,J) = 0.0                                                TR201020
            Z(J,I) = 0.0                                                TR201030
  400    CONTINUE                                                       TR201040
C                                                                       TR201050
  500 CONTINUE                                                          TR201060
C                                                                       TR201070
      RETURN                                                            TR201080
      END                                                               TR201090
      SUBROUTINE UPDAW(X,Y,DISP,DIST,W,V,Z,H1,H2,
     X                 IWEI,NP,ND,NDAT,NV,FINP,INOR,IWRITE)
C     ******************************************************************
C     *                                                                *
C     *  U P D A 1                                                     *
C     *                                                                *
C     *  PURPOSE: COMPUTE A SUCCESSOR AS                               *
C     *             - IF IWEI=0:  Y = B X                              *
C     *             - IF IWEI=1:  Y = V B X                            *
C     *                                                                *
C     *  SUBROUTINES CALLED: SET0,DINVER                               *
C     *                                                                *
C     ******************************************************************
      DIMENSION  X(NP,ND),Y(NP,ND),DISP(NDAT),DIST(NDAT),W(NDAT),
     X           Z(NP,NP),H1(NP),H2(NP)
      REAL*8 V(NV)
C                                                                      C
      IF (IWEI.EQ.0) GO TO 20
C----------------------------------------------------------------------C
C  WEIGHTED CASE                                                       C
C----------------------------------------------------------------------C
         IF (INOR.GT.0) CALL DINVER(W,V,NP,NDAT,NV,IWRITE)
      NDP=NP*ND
      CALL SET0(Z,NDP)
         L=0
      DO 3 I=2,NP
         IMIN=I-1
         DO 4 J=1,IMIN
            L=L+1
            IF (DIST(L).LT.1.0E-20) GO TO 4
               B=FINP*W(L)*DISP(L)/DIST(L)
            DO 5 K=1,ND
               BSTAR=B*(X(I,K)-X(J,K))
               Z(I,K)=Z(I,K)+BSTAR
               Z(J,K)=Z(J,K)-BSTAR
 5          CONTINUE
 4       CONTINUE
 3    CONTINUE
         L=0
         I=1
 6       DO 7 K=1,ND
 7          Y(I,K)=0.0
            DO 8 J=1,I
               L=L+1
               DO 9 K=1,ND
 9                Y(I,K)=Y(I,K)+SNGL(V(L))*Z(J,K)
 8          CONTINUE
            IMIN=I
            I=I+1
               IF (I.GT.NP) RETURN
            M=L
            DO 10 J=I,NP
               M=M+J-1
               DO 11 K=1,ND
 11               Y(IMIN,K)=Y(IMIN,K)+SNGL(V(M))*Z(J,K)
 10         CONTINUE
         GO TO 6
C----------------------------------------------------------------------C
C  UNWEIGHTED CASE                                                     C
C----------------------------------------------------------------------C
 20   DO 24 I=1,NP
         DO 25 K=1,ND
 25         Y(I,K)=0.0
 24      CONTINUE
      L=0
      DO 21 I=2,NP
         IMIN=I-1
         DO 22 J=1,IMIN
            L=L+1
               IF (DIST(L).LT.1.0E-20) GO TO 22
            B=FINP*DISP(L)/DIST(L)
            DO 23 K=1,ND
               BSTAR=B*(X(I,K)-X(J,K))
               Y(I,K)=Y(I,K)+BSTAR
               Y(J,K)=Y(J,K)-BSTAR
 23         CONTINUE
 22      CONTINUE
 21   CONTINUE
      RETURN
      END
      SUBROUTINE VAREX(DISP,W,N,IWEI,NDAAT,XX,IWRITE)
C     ******************************************************************
C     *                                                                *
C     *  V A R E X                                                     *
C     *                                                                *
C     *  PURPOSE:  NORMALIZATION VARIANCE EXPLICIT                     *
C     *                                                                *
C     *  SUBROUTINES CALLED: SSVAR                                     *
C     *                                                                *
C     *  AUTHORS: INEKE STOOP                  RELEASED: JUNE  1982    *
C     *                                                                *
C     ******************************************************************
      DIMENSION DISP(N),W(N)
C
      CALL SSVAR(VS,VM,DISP,W,N,IWEI,NDAAT)
         IF (VS.LE.1.0E-6) GOTO 999
      ALF=SQRT(XX/VS)
      GG =VM*(1.-ALF)
      DO 10 L=1,N
         DISP(L)=DISP(L)*ALF+GG
 10   CONTINUE
      RETURN
 999  WRITE(IWRITE,1000)
 1000 FORMAT(19H VARIANCE GETS ZERO//24H USE OTHER NORMALIZATION)
      STOP
      END
      SUBROUTINE VARIM(DISP,DIST,W,N,IWEI,NDAAT,XX,IWRITE)
C     ******************************************************************
C     *                                                                *
C     *  V A R I M                                                     *
C     *                                                                *
C     *  PURPOSE:  NORMALIZATION VARIANCE IMPLICIT                     *
C     *                                                                *
C     *  SUBROUTINES CALLED: SSVAR                                     *
C     *                                                                *
C     *  AUTHORS: INEKE STOOP                  RELEASED: JUNE  1982    *
C     *                                                                *
C     ******************************************************************
      DIMENSION DISP(N),DIST(N),W(N)
C
      CALL SSVAR(PS,PM,DISP,W,N,IWEI,NDAAT)
      CALL SSVAR(TS,TM,DIST,W,N,IWEI,NDAAT)
         IF (PS.LE.1.0E-6.OR.TS.LE.1.0E-6) GOTO 999
      ALF=TS/PS
      TK=XX*PS/(TS*TS)
      GG =PM*(1.-ALF)
      DO 10 L=1,N
         DISP(L)=DISP(L)*ALF+GG
         IF (IWEI.EQ.1) W(L)=W(L)*TK
         IF (IWEI.EQ.0) W(L)=TK
 10   CONTINUE
      RETURN
 999  WRITE(IWRITE,1000)
 1000 FORMAT(19H VARIANCE GETS ZERO//24H USE OTHER NORMALIZATION)
      STOP
      END
      SUBROUTINE VINPUN(DATA,DISS,DHELP,HELP,HULP,IORD,LBK,W,
     1                  FORM1,NR,NP,NPNP,NDAT,IPRI,INDATA,IWRITE,
     2                  ILEV,IAPT,IDIS,FNR,INO)
C     ******************************************************************
C     *                                                                *
C     *  V I N P U N                                                   *
C     *                                                                *
C     *  PURPOSE: - READ THE DATA ACCORDING TO THE DATA SPECIFICATION  *
C     *             PARAMETERS                                         *
C     *           - PREPARE DATA MATRIX                                *
C     *                                                                *
C     *  SUBROUTINES CALLED: WRANK,PRIREC,SQNORW,VANORW,SETW           *
C     *                                                                *
C     ******************************************************************
      DIMENSION DATA(NDAT),DISS(NP,NP),IORD(NPNP),LBK(NPNP),W(NP,NP),
     1          DHELP(NP,NP),HELP(NPNP),HULP(NPNP)
      CHARACTER*80 FORM1
C    1          DHELP(NP,NP),HELP(NPNP),HULP(NPNP),FORM1(20)
C----------------------------------------------------------------------C
      IWD=1
      CALL SETW(W,NPNP,NP)
      DO 50 K=1,NR
         READ(INDATA,FORM1) ((DISS(I,J),J=1,NP),I=1,NP)
         DO 10 I=1,NP
            DISS(I,I)=0.
  10     CONTINUE
C----------------------------------------------------------------------C
C IF NONMETRIC ITERATIONS HAVE TO BE PERFORMED, PREPARE IORD AND LBK   C
C----------------------------------------------------------------------C
         IF (ILEV.GT.1) CALL WRANK(DISS,HELP,W,HULP,IORD,LBK,
     1                            NPNP,IAPT,IDIS,IWD)
C                                                                      C
         IF (IPRI.LT.K) GO TO 20
C----------------------------------------------------------------------C
C  PRINT THE ORIGINAL DATA                                             C
C----------------------------------------------------------------------C
            WRITE(IWRITE,1000) K
            CALL PRIREC(DISS,NP,NP,NP,IWRITE)
  20     NDAAT=NPNP-NP
         IF (INO.EQ.1) CALL SQNORW(DISS,W,NPNP,2.,1)
         IF (INO.EQ.2) CALL VANORW
     1                 (DISS,W,NPNP,2.,1,NDAAT,ILEV,IWRITE)
            WRITE(2) ((DISS(I,J),I=1,NP),J=1,NP)
         L=0
         DO 40 J=2,NP
            JMIN=J-1
            DO 30 I=1,JMIN
               DHELP(I,J)=DHELP(I,J)+DISS(I,J)
               DHELP(J,I)=DHELP(J,I)+DISS(J,I)
  30        CONTINUE
  40     CONTINUE
  50  CONTINUE
C----------------------------------------------------------------------C
C  SYMMETRIZE AND AVERAGE DATA AND WEIGHTS                             C
C----------------------------------------------------------------------C
      DO 70 J=2,NP
         IMIN=J-1
         DO 60 I=1,IMIN
            L=L+1
            DHELP(I,J)=DHELP(I,J)*FNR
            DHELP(J,I)=DHELP(J,I)*FNR
            DATA(L)=.5*(DHELP(I,J)+DHELP(J,I))
  60     CONTINUE
  70  CONTINUE
         RETURN
 1000 FORMAT(///40H0ORIGINAL DATA MATRIX OF REPLICATION NO  ,I5)
      END
      SUBROUTINE VWINPU(DATA,DISS,DHELP,HELP,HULP,IORD,LBK,W,WT,WH,
     1                  FORM1,FORM2,NR,NP,NPNP,NDAT,IPRI,INDATA,
     2                  IWRITE,ILEV,IAPT,IDIS,IWEI,IMIS,VMIS,INO)
C     ******************************************************************
C     *                                                                *
C     *  V W I N P U                                                   *
C     *                                                                *
C     *  PURPOSE: - READ THE DATA ACCORDING TO THE DATA SPECIFICATION  *
C     *             PARAMETERS          WEIGHTED ASYMMETRIC            *
C     *           - PREPARE DATA MATRIX                                *
C     *                                                                *
C     *  SUBROUTINES CALLED: WRANK,PRIREC,PRIMIR,SQNORW,VANORW,SETW    *
C     *                                                                *
C     ******************************************************************
      DIMENSION DATA(NDAT),DISS(NP,NP),IORD(NPNP),LBK(NPNP),
     1          HELP(NPNP),HULP(NPNP),DHELP(NP,NP),
     2          W(NP,NP),WT(NP,NP),WH(NDAT)
C    1          HELP(NPNP),HULP(NPNP),FORM1(20),FORM2(20),DHELP(NP,NP),
      CHARACTER*80 FORM1,FORM2
C----------------------------------------------------------------------C
      IWD=1
      DO 100 K=1,NR
         READ(INDATA,FORM1) ((DISS(I,J),J=1,NP),I=1,NP)
         IF (IWEI.EQ.0) CALL SETW(W,NPNP,NP)
         IF (IWEI.EQ.1) READ(INDATA,FORM2) ((W(I,J),J=1,NP),I=1,NP)
         IF (IMIS.LE.0) GOTO 40
            NMIS=0
            DO 30 J=2,NP
               IMIN=J-1
               DO 20 I=1,IMIN
                  IF (DISS(I,J).GT.VMIS) GOTO 10
                     W(I,J)=0.
                     NMIS=NMIS+1
 10               IF (DISS(J,I).GT.VMIS) GOTO 20
                     W(J,I)=0.
                     NMIS=NMIS+1
 20            CONTINUE
 30         CONTINUE
 40      DO 50 I=1,NP
            DISS(I,I)=0.
            IF (IWEI.EQ.1) W(I,I)=0.
 50      CONTINUE
C----------------------------------------------------------------------C
C IF NONMETRIC ITERATIONS HAVE TO BE PERFORMED, PREPARE IORD AND LBK   C
C----------------------------------------------------------------------C
         IF (ILEV.GT.1) CALL WRANK(DISS,HELP,W,HULP,IORD,LBK,
     1                            NPNP,IAPT,IDIS,IWD)
C                                                                      C
         IF (IPRI.LT.K) GO TO 60
C----------------------------------------------------------------------C
C  PRINT THE ORIGINAL DATA                                             C
C----------------------------------------------------------------------C
            WRITE(IWRITE,1000) K
            IF (IMIS.EQ.0) CALL PRIREC(DISS,NP,NP,NP,IWRITE)
            IF (IMIS.EQ.1) CALL PRIMIR(DISS,NP,NP,VMIS,IWRITE)
            IF (IWEI.EQ.1) WRITE(IWRITE,2000)
            IF (IWEI.EQ.1) CALL PRIREC(   W,NP,NP,NP,IWRITE)
 60         IF (IMIS.EQ.1) WRITE(IWRITE,3000) K,NMIS
C----------------------------------------------------------------------C
         NDAAT=NPNP-NP
         IF (INO.EQ.1) CALL SQNORW(DISS,W,NPNP,2.,1)
         IF (INO.EQ.2) CALL VANORW
     1                 (DISS,W,NPNP,2.,1,NDAAT,ILEV,IWRITE)
            WRITE(2) ((DISS(I,J),I=1,NP),J=1,NP)
            WRITE(4) ((   W(I,J),I=1,NP),J=1,NP)
         L=0
         DO 80 J=2,NP
            JMIN=J-1
            DO 70 I=1,JMIN
               DHELP(I,J)=DHELP(I,J)+DISS(I,J)*W(I,J)
                  WT(I,J)=   WT(I,J)+   W(I,J)
               DHELP(J,I)=DHELP(J,I)+DISS(J,I)*W(J,I)
                  WT(J,I)=   WT(J,I)+   W(J,I)
 70         CONTINUE
 80      CONTINUE
 100  CONTINUE
C----------------------------------------------------------------------C
C  SYMMETRIZE AND AVERAGE DATA AND WEIGHTS                             C
C----------------------------------------------------------------------C
      L=0
      DO 200 J=2,NP
         IMIN=J-1
         DO 140 I=1,IMIN
            L=L+1
            IF (WT(I,J).GT.1.0E-6) GOTO 110
               WT(I,J)=0.
               DHELP(I,J)=0.
               IF (WT(J,I).GT.1.0E-6) GOTO 120
                  WT(J,I)=0.
                  DHELP(J,I)=0.
                  DATA(L)=0.
                  WH(L)=0.
                  GOTO 140
 110        IF (WT(J,I).GT.1.0E-6) GOTO 130
               WT(J,I)=0.
               DHELP(J,I)=0.
               DHELP(I,J)=DHELP(I,J)/WT(I,J)
               DATA(L)=DHELP(I,J)
               WH(L)=WT(I,J)
               GOTO 140
 120           DHELP(J,I)=DHELP(J,I)/WT(J,I)
               DATA(L)=DHELP(J,I)
               WH(L)=WT(J,I)
               GOTO 140
 130        DATA(L)=DHELP(I,J)+DHELP(J,I)
            WX=WT(I,J)+WT(J,I)
            DATA(L)=DATA(L)/WX
            WH(L)=.5*WX
            DHELP(I,J)=DHELP(I,J)/WT(I,J)
            DHELP(J,I)=DHELP(J,I)/WT(J,I)
 140     CONTINUE
 200  CONTINUE
      RETURN
 1000 FORMAT(///40H0ORIGINAL DATA MATRIX OF REPLICATION NO  ,I5)
 2000 FORMAT(//8H0WEIGHTS)
 3000 FORMAT(15H0REPLICATION NO,I5,4H HAS,I5,15H MISSING VALUES)
      END
      SUBROUTINE WINPUN(DATA,DISS,HELP,IORD,LBK,FORM1,NR,NP,NDAT,NDAAT,
     1                  WHELP,W,FORM2,IPRI,INDATA,IWRITE,
     2                  ILEV,IAPT,IDIS,IWEI,IMIS,VMIS,INO,ISYM)
C     ******************************************************************
C     *                                                                *
C     *  W I N P U N                                                   *
C     *                                                                *
C     *  PURPOSE: - READ THE DATA ACCORDING TO THE DATA SPECIFICATION  *
C     *             PARAMETERS                                         *
C     *           - PREPARE DATA MATRIX AND WEIGHT MATRIX              *
C     *                                                                *
C     *  SUBROUTINES CALLED: WRANK,PRITRI,SQNORW,VANORW,PRIMIS         *
C     *                                                                *
C     ******************************************************************
      CHARACTER*80 FORM1,FORM2
      DIMENSION DATA(NDAT),DISS(NDAT),IORD(NDAT),LBK(NDAT),HELP(NDAAT),
     1          WHELP(NDAT),W(NDAT)
C    1          FORM1(20),FORM2(20),WHELP(NDAT),W(NDAT)
C----------------------------------------------------------------------C
      IWD=1
      DO 140 K=1,NR
         IF (ISYM.EQ.0) GOTO 10
            READ(INDATA,FORM1) (DISS(L),L=1,NDAT)
            GOTO 30
  10     LE=0
         DO 20 I=2,NP
            LB=LE+1
            LE=LE+I-1
            READ(INDATA,FORM1) (DISS(L),L=LB,LE)
  20     CONTINUE
  30     IF (IWEI.EQ.0) GOTO 70
            IF (ISYM.EQ.0) GOTO 40
               READ(INDATA,FORM2) (WHELP(L),L=1,NDAT)
               GOTO 60
  40        LE=0
            DO 50 I=2,NP
               LB=LE+1
               LE=LE+I-1
               READ(INDATA,FORM2) (WHELP(L),L=LB,LE)
  50        CONTINUE
  60        IF (IMIS) 110,110,90
  70     DO 80 L=1,NDAT
            WHELP(L)=1.
  80     CONTINUE
  90     NMIS=0
         DO 100 L=1,NDAT
            IF (DISS(L).GT.VMIS) GOTO 100
            WHELP(L)=0.
            NMIS=NMIS+1
 100     CONTINUE
C----------------------------------------------------------------------C
C IF NONMETRIC ITERATIONS HAVE TO BE PERFORMED, PREPARE IORD AND LBK   C
C----------------------------------------------------------------------C
 110     IF (ILEV.GT.1) CALL WRANK(DISS,HELP(1),WHELP,HELP(NDAT+1),
     1                             IORD,LBK,NDAT,IAPT,IDIS,IWD)
         IF (IPRI.LT.K) GOTO 120
C----------------------------------------------------------------------C
C  PRINT THE ORIGINAL DATA                                             C
C----------------------------------------------------------------------C
            WRITE(IWRITE,1000) K
            IF (IMIS.EQ.0) CALL PRITRI(DISS,NP,NDAT,IWRITE)
            IF (IMIS.EQ.1) CALL PRIMIS(DISS,NP,NDAT,VMIS,IWRITE)
            IF (IWEI.EQ.1) WRITE(IWRITE,2000)
            IF (IWEI.EQ.1) CALL PRITRI(WHELP,NP,NDAT,IWRITE)
C----------------------------------------------------------------------C
C  NORMALIZE ORIGINAL DATA                                             C
C----------------------------------------------------------------------C
 120     IF (IMIS.EQ.1) WRITE(IWRITE,3000) K,NMIS
         IF (INO.EQ.1) CALL SQNORW(DISS,WHELP,NDAT,1.,1)
         IF (INO.EQ.2) CALL VANORW
     1                 (DISS,WHELP,NDAT,1.,1,NDAT,ILEV,IWRITE)
            WRITE(2) (DISS(L),L=1,NDAT)
            WRITE(4) (WHELP(L),L=1,NDAT)
         DO 130 L=1,NDAT
             DATA(L)=DATA(L)+DISS(L)*WHELP(L)
             W(L)=W(L)+WHELP(L)
 130     CONTINUE
 140  CONTINUE
C----------------------------------------------------------------------C
C  COMPUTE AGGREGATED DISPARITIES                                      C
C----------------------------------------------------------------------C
      DO 200 L=1,NDAT
         IF (W(L).GE.1.0E-06) DATA(L)=DATA(L)/W(L)
         IF (W(L).LT.1.0E-06) W(L)=0.
 200  CONTINUE
      RETURN
 1000 FORMAT(///40H0ORIGINAL DATA MATRIX OF REPLICATION NO  ,I5)
 2000 FORMAT(//8H0WEIGHTS)
 3000 FORMAT(15H0REPLICATION NO,I5,4H HAS,I5,15H MISSING VALUES)
      END
      SUBROUTINE DINVER(W,V,NP,NDAT,NV,IWRITE)
C     ******************************************************************
C     *                                                                *
C     *  D I N V E R                                                   *
C     *                                                                *
C     *  PURPOSE:  COMPUTES THE GENERALIZED INVERSE MATRIX VPLUS FROM  *
C     *            THE CENTERED VERSION V OF THE WEIGHT MATRIX W BY:   *
C     *                                                                *
C     *                                   -1        2                  *
C     *                  VPLUS = (V + EE')  - EE'/NP                   *
C     *                                                                *
C     *            VPLUS IS STORED IN V                                *
C     *                                                                *
C     *  SUBROUTINES CALLED: DSINV                                     *
C     *                                                                *
C     ******************************************************************
      DIMENSION  W(NDAT)
      INTEGER VITER
      REAL*8 V(NV)
C                                                                      C
      SMALL=1.0E-6
      M=0
      DO 1 I=1,NP
         SUM=0.0
         DO 2 J=1,NP
            IF (I-J) 3,2,4
 3          L=I+(J*J-3*J+2)/2
               SUM=SUM+W(L)
               GO TO 2
 4          L=J+(I*I-3*I+2)/2
               M=J+(I*I-I)/2
               SUM=SUM+W(L)
               V(M)=DBLE(-W(L)+1.0)
 2          CONTINUE
 1       V(M+1)=DBLE(SUM+1.0)
      CALL DSINV(V,NV,NP,SMALL,VITER)
      IF (VITER.NE.0) GO TO 10
        DDNP=DBLE(FLOAT(NP))
        DDNQ=1.D0/DDNP
         DO 5 M=1,NV
 5          V(M)=DDNP*V(M)-DDNQ
         RETURN
 10   WRITE(IWRITE,60)
 60   FORMAT(33H0TROUBLES WITH INVERSE OF WEIGHTS )
      STOP
      END
      SUBROUTINE WRANK(DATA,DH,W,WH,IORD,LBK,N,IAPT,IDIS,IWEI)
C     ******************************************************************
C     *                                                                *
C     *  W R A N K                                                     *
C     *                                                                *
C     *  PURPOSE: CONSTRUCT IORD AND LBK FROM DATA. IORD WILL CONTAIN  *
C     *           THE INDICES WHICH WOULD ORDER THE REPLICATION        *
C     *           MATRICES.  LBK WILL CONTAIN THE LENGTHES OF THE COR- *
C     *           RESPONDING TIEBLOCKS, AUGMENTED WITH ZEROES. IORD    *
C     *           AND LBK WILL BE STORED IN UNIT 3. IAPT WILL BE AUG-  *
C     *           MENTED WITH ONE IF THE NUMBER OF TIEBLOCKS           *
C     *           CORRESPONDS WITH THE NUMBER OF ELEMENTS              *
C     *                                                                *
C     *  SUBROUTINES CALLED: SHEL10, WTIEBL                            *
C     *                                                                *
C     *  AUTHORS : WILLEM HEISER , INEKE STOOP RELEASED JANUARY 1981   *
C     *                                                                *
C     ******************************************************************
      DIMENSION DATA(N),IORD(N),LBK(N),W(N),DH(N),WH(N)
      DO 10 L=1,N
         IORD(L)=L
         LBK(L) =0
         DH(L)=DATA(L)
         IF (IWEI.EQ.1) WH(L)=W(L)
 10   CONTINUE
      CALL SHEL10(DH,WH,IORD,N,NW,IDIS,IWEI)
      IF (NW.GT.0) CALL WTIEBL(DH,LBK,N,IDLBK,NW)
      IF (NW.EQ.0) IDLBK=0
      WRITE(3) (IORD(L),L=1,N),(LBK(L),L=1,N)
      IF (IDLBK.EQ.N) IAPT=IAPT+1
      RETURN
      END
      SUBROUTINE WTIEBL (DISM, LBK, N, IDLBK , NW)
C     ******************************************************************
C     *                                                                *
C     *  W T I E B L                                                   *
C     *                                                                *
C     *  PURPOSE:- FIND  A VECTOR LBK SO THAT EACH ELEMENT OF IT       *
C     *            REPRESENTS THE LENGTH OF A TIEBLOCK FOUND IN DISM.  *
C     *          - IDLBK GIVES THE NUMBER OF TIEBLOCKS                 *
C     *          - LAST TIEBLOCK CONTAINS MISSING (WEIGHTS=0.)         *
C     *                                                                *
C     *  SUBROUTINES CALLED: NONE.                                     *
C     *                                                                *
C     *  SEE ACCOMPANYING PAPER SECTIONS: 2.2.                         *
C     *                                                                *
C     *  AUTHOR: ERNST VAN WANING.     RELEASED: JULY 1976             *
C     *                                                                *
C     ******************************************************************
C
C
      DIMENSION DISM(N),LBK(N)
C
      IDLBK = 0
      JBASE = 1
    3    K = 1
    2    IDDISM = JBASE + K
         IF (IDDISM .GT. N) GOTO 1
         IF (DISM(JBASE) .NE. DISM(IDDISM)) GOTO 1
            K = K + 1
            GOTO 2
    1    IDLBK = IDLBK + 1
         LBK(IDLBK) = K
         JBASE = IDDISM
      IF (JBASE .LE. NW) GOTO 3
      IF (NW.EQ.N) RETURN
      IDLBK=IDLBK+1
      LBK(IDLBK)=N-NW
      RETURN
      END
      SUBROUTINE DECLAR(SUBRTN,B,NWORDS)                                DEC00010
C     ******************************************************************DEC00020
C     *                                                                *DEC00030
C     *  D E C L A R                                                   *DEC00040
C     *                                                                *DEC00050
C     *  PURPOSE: A STATIC STORAGE ALLOCATION ROUTINE; IF THE NUMBER   *DEC00060
C     *           OF WORDS TURNS OUT TO BE TOO SMALL, JUST CHANGE THE  *DEC00070
C     *           FIRST TWO STATEMENTS OF THIS ROUTINE.                *DEC00080
C     *           FOR DYNAMIC ALLOCATION, THIS ROUTINE SHOULD BE RE-   *DEC00090
C     *           PLACED BY A (MACHINE DEPENDENT) ASSEMBLER ROUTINE.   *DEC00100
C     *                                                                *DEC00110
C     *  SUBROUTINES CALLED: SUBRTN (IN PARAMETER LIST)                *DEC00120
C     *                                                                *DEC00130
C     ******************************************************************DEC00140
      DIMENSION A(3000000)                                              DEC00150
      COMMON TITLE,VMIS,CRII,CRIP,
     1       NP,NR,ISYM,IWEI,IMIS,ILAB,
     2       IPRI,IPRJ,IPLO,IEIG,IHIS,IIND,ISHE,IPUN,
     3       INDATA,INPARA,IWRITE,ISTORE,
     4       NDMA,NDMI,MAXI,MAXP,IINI,IREL,ANUL,
     5       ICON,IPRO,IDIS,INOR,ILEV,
     6       NDAT,ND,NDAAT,NPNP,NV,NSY1,NSY2,NSY3,NSY4,N8
      CHARACTER*80 TITLE,FORM1,FORM2
      IDIM = 300000                                                     DEC00160
      DATA A/300000*0.0/
      IF (NWORDS.LE.IDIM) GO TO 10                                      DEC00170
         WRITE (6,1000) NWORDS                                          DEC00180
         RETURN                                                         DEC00190
   10 CONTINUE                                                          DEC00200
      CALL SUBRTN(A,NWORDS)                                             DEC00210
      RETURN                                                            DEC00220
 1000 FORMAT (50H0ARRAY 'A' IN SUBROUTINE 'DECLAR' IS TOO SMALL FOR,    DEC00230
     +         9H THIS JOB/21H 'A' MUST BE AT LEAST,I8,5H LONG//        DEC00240
     +        45H EXECUTION OF THE PROGRAM HAS BEEN TERMINATED)         DEC00250
      END                                                               DEC00260
