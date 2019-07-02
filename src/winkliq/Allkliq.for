C        READ(16,24546,END=55155) KNUMSIM,OMAXG,NUMGROU2,OKPGROUP,
C     C  KPGROU2,
C     C  ONUMTEAC,NUMPEOP2,OMAXCHOI,MAXCHOI2,OMAXFREQ,MAXFRE2,SSEED,
C     C USEMARG,OMAXPIW,MAXPIW,OMAXPIB,MAXPIB,REGSIM,BYDENSE,SIMRAND,
C     C OBASEP,BASEP2,ORANGEP,RANGEP2,COMPSIM,
C     C OMAXPWW,MAXPWW,OMAXPWB,MAXPWB,OBASEWP,BASEWP2,OWRANGEP,
C     C WRANGEP2,XOBASEP,XBASEP2,XORANGEP,XRANGEP2,
C     C OPREPWI,PREPWI2,OPREQWI,PREQWI2, 
C     C OPREPBT,PREPBT2,OPREQBT,PREQBT2,PKPG,QKPG,PSIZE,QSIZE

C     FILE CALLED KLIQFIND.F
      PROGRAM kliqmain

C     (INFILE,MATTYPE,PRIORFIL)
       CHARACTER LABEL(251)*9,TITLES(3)*20,APRINT(50)*200,
     C FANCY(20)*200,MATTYPE*1,TITFILE*16,LABFILE*16,
     C HITFILE*16,LISTFILE*16,PARTITLE(50)*200,PARFORM(50)*80,
     C LISTVAR(50)*100,TCHAR(50)*80,KMATTYPE*1,TESTCHAR*10,
     C ONECHAR*1,DISTFILE*12
C       PARAMETER (PAR1=11)
C       PARAMETER (CPAR1=2**PAR1)
       CHARACTER*12 PLACEFILE
       REAL IMAXVAL,FITI(251),ISTRESS(251),ISTRESS2(251),
     C MSTRESS3(251),MSTRESS2(251),
     C ISTRESS3(251),OC1,OUTCOME(251),OUTMUL(5,251),
     C XR1,XR2,XR3,XR4,XR5,ODDSDB1,ODDSDB2,ODDSDA,
     C TNUM,TMAX,PL,XINDEPT

       DOUBLE PRECISION STRESSB,STRESS2B,STRESS3B 

       INTEGER CENTER,DANCHOR2,MOVE2,DANCHOR,ZSYMMAT,MAXINC,
     C BYINC,NUMDIM,RINCREM,MEASURE,EXTREME,STARTINC,HYPERG,
     C NORMAL,NORMALI,CENTERI,DANCHORI,DANCH2I,MOVE2I,ZSYMMATI,
     C STARTINI,BYINCI,MAXINCI,DONEONE,ONE,TWO,THINKING,DOMULT,
     C OTCOUNT(5,251),OUTLOOP,BYLAMN,FOUNDPER,NOTFOUND,THIWT


       REAL
     C OPREPWI,PREPWI2,OPREQWI,PREQWI2, 
     C OPREPBT,PREPBT2,OPREQBT,PREQBT2,
     C PREPWI,PREQWI,PREPBT,PREQBT,PPWIINC,PQWIINC,
     C PPBTINC,PQBTINC,BMISS,EMISS,RMISS,
     C XOBASEP,XOBASEP2,XORANGEP,XORANGEP2,
     C PKPG,QKPG,PSIZE,QSIZE


       REAL IGRATIO,DRADIUSG,DRADIUSI,KEXP,MINVALI,MINVALG,KEXPI,
     C PCTCENG1,PCTCENG2,PCTCENI1,PCTCENI2,
     C NOWMISS,T1,T2
  
       INTEGER NUMDYAD,PERSON1(251),FLAGI1,MBI,NUMOBS,ONUMDYAD,
     C PERSON2(251),SIZE,PERSON3(251),STRUCTEQ,
     C ADDI,ADDJ,ADDK,BOTHP,SYMMAT,NLIST(251),NUMNLIST,
     C USETRIAD,HAVEDEPT(251),NUMHAVE,LB,LOOKT,OD,STARTTRY,
     C DYDTRIAD,NEVAL,PO,PORD(251),NPORD(251),PORDG(251),PID,
     C MBJ,NUMRES,TRYDEPT(251),J,UNO,DUE,RESULTM(251,251),
     C PERGROUP,KNEVAL,TRANSPOS,NEWGRPS,FLAGR,PMADE,TAGALONG,
     C INVERT,HOWWIDE,RECTMAT,QMAXG,QKPGROUP,QNUMTEAC,QMAXCHOI,
     C QMAXFREQ,QNHLIST,QHLIST(251),QR,MUTDYAD,NONEG,MAXSEED,
     C HALFDYAD,DISSOLVE,PAIRUP(251,2),PAIRS,PID2,REGSIM,BYDENSE,
     C O2MAXG,INPLACE,I,HAVEI,HAVEJ,SNONEG,SPCTILE,SNEARVAL,DI
C     C NRESULTM(251,251)

C     ,TRIDLIST(125000,3),DYDTRIAD
       REAL CLOSPAIR(251),CLOSEGRP(251),RMAXDEPT,HIWTEVAL,
     C IEXPECT(251),ISTD(251),ICON(251),HUBSIM,STDHS,ALLCON,
     C HUBM1,HUBSTD1,FIXR,HIWT,NFIXR,INDEPT(251),OUTDEPT(251),
     C PIW,PIB,GINDEPT,GBDEPT,MINPICT,TD3

       REAL*4 PAGB(251,200),OMAXPIW,MAXPIW,OMAXPIB,MAXPIB
      REAL THISTOT,ISOTOT,NEARVAL,STOPVAL,CHANGEC(251),
     C     NOWCLOSE(251),CDEPT,LOWCOMP,LOWVAL,MINVAL,
     C     KSIGN,ZABS,QVAL,LASTCHNG,PROBGRP(251),
     C   THRESHT,DIRECT,BASEVAL,
     C   ACTTIE,VTIE,ETIE,TOPVAL
C          KNEWMAT(251,251),
       REAL DEVIATE(251),INDIVAR(251),
     C THISDEV,PREVE,PREC,COVAR,HGVAR(251),
     C MEANWT(251),VARWT(251),
     C ZSCORE,PVALUE,TOTHGM,TOTHGV,TOTHGV2,
     C TZSCORE,TPVALUE,HGSTD,TOTHGSTD,TOTHGV3,TOTHGV4,
     C ACONN,AVAR,AMEAN,CHNGESIM,AVAR2,
     C  TACONN,CHOSEVR(251),
     C  TOTE, TOTV,OUTOF,WPIK,
     C  NOB,STOTCON,
     C  FCOMPACT,CDIAG,VDIAG,MDIAG,MCON,TWEIGHT,
     C  COMPVAL,ZCOMPMAT(251,251),EXTRES,EXTOP1,EXTOP2
C        REAL XPRIME(251,251),XPRIMEX(251,251)
C          ON EXTERNAL ERROR CALL EXTERR (EXTPAR,EXTRES,EXTOP1,EXTOP2)
      INTEGER INMAT(251,251),NEWMAT(251,251),HITLIST(251),NUMHLIST,
     C PI,PJ,PK,MAXLAB,MAXCH,NETWORK,QUICKEND,QUANTYPE,SQUAREIT,
     C ATTACHI,SIMRAND
      INTEGER NUMTEACH,TGROUP(251),TDEPART,G,IG,G3,EASY,G2,
     C EXTPAR,INTPAR
      INTEGER DEPART(251),NEWDEPT(251),NEWDIM,
     C SOLUT,NETLEV,GCONTIN,CONVID(251)
      INTEGER ZEROVAL,K,D,MAXDEPT,DEPARTN(251)
      REAL KNUMSIM,OMAXG,NUMGROU2,OKPGROUP,KPGROU2,
     C  ONUMTEAC,NUMPEOP2,OMAXCHOI,MAXCHOI2,OMAXFREQ,MAXFRE2
      INTEGER NSIM,USEMARG,MAXCHOIC,MAXFREQ,HIMARG,RECODE(251),
     C IHIVAL
      REAL GRINC,PGINC,NPINC,MCINC,MFINC,RPINC

      INTEGER ALLGROUP(251,251),NUMIT,R,MAXPOS,MAXVAL,MAXG,KTEMPS,
     C        SUMN,R2,R3,S,RCHOICE(251),Q,TPLACE,HIQ,QI,KQMAX,
     C        MAXPICK,ROW,H,COL,HUBERT(251,251),
     C        HUB1,HUB2,Z,M,MADECHNG,MRCHOICE(251),TDEPARTN,SIMO,
     C   LOWMEM,FOUNDONE,NEWMEMS(251),NGROUP,THED(251),KLIST(251),
     C   DMEMBERS(251),ISOLIST(251),ISO,SKIPIT,OLDLOWM,
     C   TEMPS,THISMEM,NEWORDER(251),NO,NO2,LENGTH,
     C   GROUP1,GROUP2,EACHPER,THISPER,OLDEPTS(251),QUITIT,
     C   ANUMIT,NLOCMAX,IER,IK,JK,SUBID(251),NSUBID(251),ONLYF,
     C   DB,NDB,B(251),NKK,NKJ,NKI,NKL,KCOUNT2,ONUMRES,
     C   ONLIST(251),NPO,PRINTO(100),
     C NBESTDEP(251),GOTEM(251),EX,EX2,TNDEPT,NMAXDEPT,EXPERM,
     C HOLDDEPT,PROBGR,PROBI,PROBJ,TNUMSUB,ACTRSQR,REWEIGHT,
     C GUSEMARG,GRPMARG(251),ACTCHAR1,ACTCHAR2,BLANK1,TEMPNUM,
     C BYANGLE,BYSCALE,BYANGLEI,BYSCALEI,OUSETRID,COMPSIM,SDEPT(251)

      DOUBLE PRECISION QSEED,RASEED,SSEED,MSEED

      REAL PCONNECT,PROBPER(251,251),
     C                 SUMWI,SUMPOS,SUMPROP,SUMBET,
     C                 CONNECTI,SUMPOSI,SUMPROPI,MAXPOSI,
     C                 HMEAN,CONVVAL,BOUNDVAL,BLABOUND,
     C                 DSYMMAT,TWXPX,BETWMULT,
     C                 PCTILE,ROWWT,COLWT,RDEPTN(251),TNUMT
       DOUBLE PRECISION
     C MISSPROB,TMISSDA,PFITDA(251),TMISSDB,PFITDB(251)

      INTEGER RANGEC,MINCHOIC,RANGEG,BASEG,ORANGEP,RANGEP2,
     C RANGEP,WRANGEP,OWRANGEP,WRANGEP2,FINAL,MAKEMISS,
     C MDEPT(251),TAKEOUT(251)

      REAL BASEP,OBASEP,BASEP2,BASEPINC,
     C OMAXPWW,MAXPWW,OMAXPWB,MAXPWB,OBASEWP,BASEWP2,FIT(251),
     C ABICOORD(251,2),BBICOORD(251,2),CBICOORD(251,2),
     C DBICOORD(251,2),ADSTRESS(251),TAD,CDSTRESS(251),TBD


C       CALL PISIM2(NUMTEACH,RANGEC,MINCHOIC,RANGEG,BASEG,
C     C BASEP,BETARAT,BETARAT2,INMAT,SSEED,DEPART

      CHARACTER*16 HEADING,HOWSTART,INFILE,PRIORFIL,OUTFILE,
     C BFILE,BFILE2,BFILEB,BFILE2B,TBFILEB,UPFILE,
     C TPFILE
      CHARACTER*24 IDATE
      CHARACTER*12 PFILE

       REAL BLAUC(251,251),DIFF(251,251)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF

C       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT
      CALL GETARG(1,INFILE)
      CALL GETARG(2,MATTYPE)
      CALL GETARG(3,PRIORFIL)
      ONE=1
      NSIM=1
      ZEROVAL=0
      RASEED=4161.D0
      QSEED=5163.D0

C      OPEN(8,file='param.clus')





C      OPEN(38,file='refine.dat')


C      OPEN(42,file='hubert.mat')
C      OPEN(43,file='remove.mat')

C      OPEN(507,file='oneconn.lst')



C      OPEN(539,file='group.bcorr')
C      OPEN(521,file='group.ccorr')








      OPEN(60,file='kliqfind.par')
      OPEN(16,file='simulate.par')

C      OPEN(595,file='MDSREAD.CMD')
C      OPEN(556,file='MDSBREAD.CMD')

      OPEN(15,file='recode.dat')
       Q=1
C      IHIVAL=0
       RECODEN=0
       MAKEMISS=0 
       DOMULT=0
          IF (MATTYPE .EQ. 'x') THEN
          DOMULT=1
          END IF 

          IF (MATTYPE .EQ. 'w') THEN
          PRIORFIL=INFILE(1:6) // ".tplac"
          SIMO=1
          MATTYPE='b'
          END IF
          
          IF (MATTYPE .EQ. 'z' ) THEN
            MATTYPE='s'
            MAKEMISS=1
           OPEN(95,file='missing.par')
           READ(95,24845,END=4141) BMISS,EMISS,RMISS
04141      CLOSE(95)
           BMISS=BMISS/100
           EMISS=EMISS/100
           RMISS=RMISS/100
          END IF
        DO 24245 I=1,999
        RECODE(I)=I
24245    CONTINUE
       DO 12829 I=1,74000
C       RDONEIT(I)=0
       READ(15,24545,END=12827) DB,PO
       IF (DB .GT. 0) THEN
       RECODEN=RECODEN+Q
       RECODE(DB)=PO
C       RDONEIT(DB)=Q
       END IF
12829  CONTINUE
12827  CLOSE(15)
C      DO 5439 I=1,IHIVAL
C      IF (RDONEIT(I) .EQ. 0) THEN
C      RECODE(I)=I
C      END IF
C05439  CONTINUE


 
C      READ(556,101) NDB , (DEBUG(DB) , DB = 1,NDB)
      OPEN(99,file='printo')
      READ(99,101,END=83831) NPO , (PRINTO(PO) , PO = 1,NPO)
83831 READ(99,3331,END=83832) (APRINT(PO) , PO=1,NPO) 
83832 CLOSE(99)
C      READ (5,2109) INFILE

C      OPEN(9,file='matrix.cvar')

      WRITE(33,3339) 
      WRITE(33,3114)
      FANCY(1)= "Welcome to KliqueFinder"
      FANCY(2)= "developed by Ken Frank"
      FANCY(3)="with help and guidance from"
      FANCY(4)="Anthony Bryk and Charles Bidwell."
      FANCY(5) ="Data were read from the following files:"
      FANCY(6) = "kliqfind.par printo hitlist "
      FANCY(7)= " titles labels"
      FANCY(8)= "Your optional output will consist of:"
      FANCY(9)="(All output in clusters unless otherwise specified)"
        IF ((SIMO .EQ. 1) .OR.(MATTYPE .EQ. 's')) THEN
         PRINTO(27)=1
        END IF


      NPARGRP=12
      READ(60,34348)
      DO 6445 I=1,NPARGRP
      READ(60,3335) PARTITLE(I),PARFORM(I)
      IF (I .EQ. 1) THEN
      READ(60,PARFORM(I)) NUMDYAD,DYDTRIAD,USETRIAD,EXPERM,RASEED
      END IF
      IF (I .EQ. 2) THEN
      READ(60,PARFORM(I)) DIRECT,THRESHT,LOOKT,MAXSEED
      END IF
      IF (I .EQ. 3) THEN
      READ(60,PARFORM(I)) BOUNDVAL,FIXR,BLABOUND,BETWMULT
      END IF
      IF (I .EQ. 4) THEN
      READ(60,PARFORM(I)) NEARVAL,PCTILE,MUTDYAD,NONEG,HALFDYAD,
     C DISSOLVE
      END IF

      IF (I .EQ. 5) THEN
      READ(60,PARFORM(I)) STOPVAL,KCOUNT2,QUICKEND,ATTACHI
      END IF

      IF (I .EQ. 6) THEN
      READ(60,PARFORM(I)) STRUCTEQ,NETWORK,ACTRSQR
      END IF

      IF (I .EQ. 7) THEN
      READ(60,PARFORM(I)) QUANTYPE,SQUAREIT,NETLEV,PERGROUP,COLWT,
     C ROWWT,HYPERG
      END IF

      IF (I .EQ. 8) THEN
      READ(60,PARFORM(I)) TRANSPOS,REWEIGHT,SYMMAT,INVERT,RECTMAT,
     C GUSEMARG,TAGALONG
      END IF

      IF (I .EQ. 9) THEN
      READ(60,PARFORM(I)) NEVAL,BASEVAL,TOPVAL,NUMRES,NEWGRPS,
     C HIWTEVAL
      END IF
      IF (I .EQ. 10) THEN
       READ(60,PARFORM(I)) IGRATIO,NUMDIM,MINPICT,
     C CENTER,DANCHOR,DANCHOR2,MOVE2,ZSYMMAT,
     C STARTINC,BYINC,MAXINC,KEXP,NORMAL,MINVALG,BYANGLE,BYSCALE,
     C PCTCENG1,PCTCENG2,DRADIUSG
      END IF

      IF (I .EQ. 11) THEN
       READ(60,PARFORM(I)) CENTERI,DANCHORI,DANCH2I,MOVE2I,ZSYMMATI,
     C STARTINI,BYINCI,MAXINCI,KEXPI,NORMALI,MINVALI,BYANGLEI,
     C BYSCALEI,PCTCENI1,PCTCENI2,DRADIUSI
      END IF
      IF (I .EQ. 12) THEN
       READ(60,PARFORM(I)) RINCREM,MEASURE,EXTREME
      END IF


C NUMDIM,RINCREM,MEASURE,EXTREME,IGRATIO,
C    C DRADIUS,KEXP,NORMAL,NORMALI,MINVALG,MINVALI
C       NUMDIM=2
C       END IF

      READ(60,3337) TCHAR(I),LISTVAR(I)
06445  CONTINUE
        CLOSE(60)


C     EXPERM,NEVAL,DYDTRIAD,USETRIAD,NUMDYAD,
C     C NEARVAL,STOPVAL,
C     C SYMMAT,THRESHT,LOOKT,DIRECT,KCOUNT2,BASEVAL,BOUNDVAL,
C     C PCTILE,STRUCTEQ,NETWORK,ACTRSQR,QUICKEND,ROWWT,COLWT,
C     C QUANTYPE,SQUAREIT,UNWEIGHT,NUMRES,RASEED,NETLEV,PERGROUP,
C     C TOPVAL,TRANSPOS
      
       HITFILE='hitlist'
       LABFILE='labels'
       TITFILE='titles'

08877 IF (INFILE .NE. "") THEN
       OPEN(9,file=INFILE)
      ELSE
      OPEN(9,file='kfake9file')
      END IF
      REWIND(9)

C     
C     READ IN AND ECHO DATA
C

      NUMOBS=0
      MAXG=1
      NUMTEACH=0 

      KMATTYPE='m'
      READ(9,4301,END=3332) TESTCHAR
      IF (TESTCHAR(3:3) .EQ. " ") THEN
      KMATTYPE='l'
      ELSE
      I=3
      ACTCHAR1=0
      BLANK1=0
      ACTCHAR2=0

      DO WHILE ((ACTCHAR1 .EQ. 0) .AND. (BLANK1 .EQ. 0) .AND.
     C(I .LE. 10) .AND. (ACTCHAR2 .EQ. 0))
      IF ((ACTCHAR1 .EQ. 0) .AND. (TESTCHAR(I:I) .NE. " ")) THEN
      ACTCHAR1=I
      ELSE
       IF ((BLANK1 .EQ. 0) .AND. (TESTCHAR(I:I) .EQ. " ")) THEN
       BLANK1=I
       ELSE
      IF ((ACTCHAR2 .EQ. 0) .AND. (TESTCHAR(I:I) .NE. " ")) THEN
         ACTCHAR2=I
      END IF
        END IF
      END IF
      I=I+1
      END DO
      IF (ACTCHAR2 .GT. BLANK1) THEN
      KMATTYPE='l'
      END IF
      IF (KMATTYPE .NE. "l") THEN

      DO 29824 I=1,200000
      READ(9,5101,END=28277) PI
29824  CONTINUE

28277  IF ((I .GT. (BLANK1-3)) .AND. (BLANK1 .GT. 0)) THEN
       KMATTYPE="l"
       END IF
       TEMPNUM=I

       END IF
C             IF (KMATTYPE .NE. "l") THEN

        END IF
C      IF (TESTCHAR(3:1) .EQ. " ") THEN/ELSE
03332  REWIND(9)

        SIMO=0
        IF (MATTYPE .EQ. 's') THEN 
        SIMO=1
         END IF
        INLIST=0
        IF ((MATTYPE .EQ. "l") .OR. (MATTYPE  .EQ. "b")) THEN
        INLIST=1
        END IF
      IF ((KMATTYPE .EQ. "l") .AND. (INLIST .NE. 1)) THEN
      WRITE(6,4301) "You did not indicate that the data were in"
      WRITE(6,4301) "list format, but kliqfinder has detected that it"
      WRITE(6,4301) "can be read only if it is in list format"
      WRITE(6,4301) "and is therefore assuming it is in list format."

      IF (MATTYPE(1:1) .EQ. "g") THEN
       MATTYPE = "b"
      ELSE
      MATTYPE="l"
      END IF
      END IF

      IF ((KMATTYPE .EQ. "m") .AND. (INLIST .EQ. 1) .AND. 
     C (TEMPNUM .NE. 13)) THEN
      WRITE(6,4301) "You indicated that the data were in"
      WRITE(6,4301) "list format, but KliqueFinder has detected that"
      WRITE(6,4301) "the data can be read only if the data"
      WRITE(6,4301) "are assumed to be in matrix format"
      WRITE(6,4301) "and is therefore assuming that they are in "
      WRITE(6,4301) "matrix format."
      IF (MATTYPE .EQ. "b") THEN
       MATTYPE = "g"
      ELSE
       MATTYPE=""
      END IF
      END IF
     


      DO 4454 PI=1,251
      NSUBID(PI)=PI
04454  CONTINUE

      IF ((MATTYPE(1:1) .EQ. 'l')  .OR. (MATTYPE .EQ. 'b')) THEN
      INPLACE=0
      DO 2824 I=1,200000
      READ(9,5101,END=2827) PI,PJ,PK
      IF ((PI .GT. NUMTEACH) .AND. (PRINTO(40) .EQ. 0)) THEN
      NUMTEACH=PI
      END IF
      IF ((PJ .EQ. 99999) .AND. (PRINTO(40) .EQ. 0)) THEN

      IF (PK .GT. 50) THEN
      WRITE(6,4301) "You have indicated an a priori subgroup"
      WRITE(6,4301) "which is larger than 50.  Please limit"
      WRITE(6,4301) "the number of a priori subgroups to 50."
      WRITE(6,4301) "The actor and subgroup which "
      WRITE(6,4301) "triggered this are:"
      WRITE(6,1251) PI,PK
      STOP
      END IF

      IF (PK .GT. 0) THEN
        DEPART(PI)=PK
        IF (PK .GT. MAXG) THEN
        MAXG=PK
        END IF
      END IF
      ELSE

      IF ((PI .GT. 0) .AND. (PJ .GT. 0) .AND. (PK .NE. 0)) THEN
      IF (PRINTO(40) .EQ. 0) THEN
      IF (PJ .GT. NUMTEACH) THEN
      NUMTEACH=PJ
      END IF
      HAVEI=PI
      HAVEJ=PJ
      ELSE

      HAVEI=0
      HAVEJ=0
      IF ((PRINTO(40) .EQ. 1) .AND. (PJ .EQ. 99999)) THEN
      HAVEJ=99999
      END IF

      J=0
      DO WHILE (((HAVEI .EQ. 0) .OR. (HAVEJ .EQ. 0)) .AND.
     C (J .LT. INPLACE))
      J=J+1
      IF (NSUBID(J) .EQ. PI) THEN
      HAVEI=J
      END IF
      IF (NSUBID(J) .EQ. PJ) THEN
      HAVEJ=J
      END IF
       END DO
      IF (HAVEI .EQ. 0) THEN
      INPLACE=INPLACE+1
      NSUBID(INPLACE)=PI
       SUBID(INPLACE)=PI
      HAVEI=INPLACE
      END IF
      IF ((PI .NE. PJ) .AND.(HAVEJ .EQ. 0)) THEN
      INPLACE=INPLACE+1
      NSUBID(INPLACE)=PJ
       SUBID(INPLACE)=PJ
      HAVEJ=INPLACE
      END IF
       NUMTEACH=INPLACE
      END IF
      NUMOBS=NUMOBS+1
      IF (((HAVEI .GT. 251) .OR. (HAVEJ .GT. 251)) .AND. 
     C (HAVEJ .NE. 99999)) THEN
      WRITE(6,4301)
      WRITE(6,4301) "Either a row or column element is "
      WRITE(6,4301) "greater than 251."
      IF (PRINTO(40) .EQ. 0) THEN
      WRITE(6,4301) "Either reduce the number"
      WRITE(6,4301) "of elements in your data or use PRINTO(40) "
      WRITE(6,4301) "to rank your ID's to values below 251"
      ELSE
      WRITE(6,4301) "You must reduce the number of unique ID's"
      WRITE(6,4301) "in your data  file."          
      WRITE(6,4301)
      WRITE(6,4301) "A listing of the unique ID's is in"
      WRITE(6,4301) "keeplist.dat"
      WRITE(6,4301)

      OPEN(37,file='keeplist.dat')
      TOPNUM=HAVEI
      IF (HAVEJ .GT. HAVEI) THEN 
      TOPNUM=HAVEJ
      END IF
      DO 9091 DI=1,TOPNUM
      WRITE(337,1251) NSUBID(DI),DI
09091  CONTINUE
      CLOSE(37)


      END IF
      WRITE(6,4301) "The row and column elements which "
      WRITE(6,4301) "triggered this are:"
      WRITE(6,1251) PI,PJ
      STOP
      END IF
      IF ((HAVEJ .EQ. 99999) .AND. (PRINTO(40) .EQ. 1)) THEN
      IF (PK .GT. 50) THEN
      WRITE(6,4301) "You have indicated an a priori subgroup"
      WRITE(6,4301) "which is larger than 50.  Please limit"
      WRITE(6,4301) "the number of a priori subgroups to 50."
      WRITE(6,4301) "The actor and subgroup which "
      WRITE(6,4301) "triggered this are:"
      WRITE(6,1251) PI,PK
      STOP
      END IF
      IF (PK .GT. 0) THEN
        DEPART(HAVEI)=PK
        IF (PK .GT. MAXG) THEN
        MAXG=PK
        END IF
      END IF
      END IF
      IF (HAVEJ .NE. 99999) THEN 
      INMAT(HAVEI,HAVEJ)=PK
      END IF

      END IF

C      IF (PJ .GT. NUMTEACH) THEN
C      NUMTEACH=PJ
C      END IF

      END IF
02824  CONTINUE
02827  CLOSE(9)


       WRITE(33,1077) "NUMOBS",NUMOBS

02819  WRITE(33,1077) "NUMTEACH",NUMTEACH
       DO 2820 PI=1,NUMTEACH
        IF (PRINTO(40) .EQ. 0) THEN
        NSUBID(PI)=PI
        SUBID(PI)=PI
        END IF

        DO 2821 PJ=1,NUMTEACH
        IF (INMAT(PI,PJ) .LT. 1) THEN
         INMAT(PI,PJ)=0
        END IF
02821    CONTINUE
02820     CONTINUE
      

      END IF
      MAXG=1
      INLIST=0
      IF ((MATTYPE .EQ. 'b') .OR. (MATTYPE .EQ. 'l')) THEN
      INLIST=1
       END IF
      IF (INLIST .EQ. 0) THEN
      NUMTEACH=1
      DO 820 I=1,251
      READ(9,4101,END=819) DEPART(I)
      NUMTEACH=NUMTEACH+1
00820  CONTINUE
00819   REWIND(9)
       NUMTEACH=NUMTEACH-1
      IF (NUMTEACH .GT. 251) THEN
      WRITE(6,4301) "You have more than 251 observations."
      WRITE(6,4301) "This is greater than the maximum which"
      WRITE(6,4301) "this ol' version of KliqueFinder can"
      WRITE(6,4301) "handle.  Please reduce to less than 251."
      STOP
      END IF
      DO 20 I=1,NUMTEACH
      READ(9,101,END=20) DEPART(I), (INMAT(I,J) , J=1,NUMTEACH)
      IF (DEPART(I) .GT. 50) THEN
      WRITE(6,4301) "You have indicated an a priori subgroup"
      WRITE(6,4301) "which is larger than 50.  Please limit"
      WRITE(6,4301) "the number of a priori subgroups to 50."
      WRITE(6,4301) "The actor and subgroup which "
      WRITE(6,4301) "triggered this are:"
      WRITE(6,1251) I,DEPART(I)
      STOP
      END IF
      DO 5116 J=1,NUMTEACH
      IF (INMAT(I,J) .NE. 0) THEN
      NUMOBS=NUMOBS+1
      END IF
05116  CONTINUE
      NSUBID(I)=I
      SUBID(I)=I
      IF (DEPART(I) .GT. MAXG) THEN
       MAXG=DEPART(I)
      END IF
      IF ((MATTYPE .EQ. 'g')  .OR. (MATTYPE .EQ. 'b')) THEN
      DEPART(I)=0
       END IF
   20 CONTINUE
       END IF

      IF ((MATTYPE .EQ. 'g')  .OR. (MATTYPE .EQ. 'b') ) THEN
      O2MAXG=0
      IF ((PRIORFIL .EQ. "") .AND. (PRINTO(24) .EQ. 1)) THEN
      WRITE(6,4301) "You have indicated that groups should be taken"
      WRITE(6,4301) "from a second file but have not given the name"
      WRITE(6,4301) "of the file.  Please indicate the name of the"
      WRITE(6,4301) "of the second file, or hit return to use groups"
      WRITE(6,4301) "from the original data file:"
        READ(5,*) PRIORFIL
      END IF
      OUTLOOP=0
      DO WHILE ((PRIORFIL .NE. "") .AND. (O2MAXG .EQ. 0)
     C .AND. (OUTLOOP .EQ. 0)) 
      OPEN(24,file=PRIORFIL)
      REWIND(24)
      DO 166 PID=1,NUMTEACH
       READ(24,251,END=2166) T1,T2,TD3
      IF ((T2 .LT. 99999) .AND. (TD3 .GT. O2MAXG)) THEN
       O2MAXG=TD3
      END IF
  166   CONTINUE
02166   REWIND(24)
        IF (PRIORFIL(1:8) .EQ. "nogroups") THEN
         O2MAXG=NUMTEACH
        END IF
        IF ((O2MAXG .EQ. 0) .AND. (PRINTO(24) .EQ. 1)) THEN
        CLOSE(24)
        WRITE(6,4301) PRIORFIL, " contains no groups greater than "
         UPFILE=PRIORFIL
        PRIORFIL=""
        WRITE(6,4301) "zero.  Enter a new file with subgroup"
        WRITE(6,4301) "placements or type 'none' to use placements"
        WRITE(6,4301) "from original data file:"
        READ(5,*) PRIORFIL
         IF (PRIORFIL(1:4) .EQ. 'none') THEN
         OUTLOOP=1
         PRIORFIL=INFILE
         END IF
C        ELSE

C        PRIORFIL=""
        END IF
        END DO
        IF (O2MAXG .GT. 0) THEN
      MAXG=0
C      OPEN(214,file=PRIORFIL)
C      REWIND(214)
       TNT=NUMTEACH
      DO 1686 PID=1,NUMTEACH
       READ(214,251,END=2167) T1,T2,TD3
       IF (PRINTO(40) .EQ. 1) THEN
        NOTFOUND=0
        FOUNDPER=1
        DO WHILE ((NOTFOUND .EQ. 0) .AND. (FOUNDPER .LE. NUMTEACH))
        IF (NSUBID(FOUNDPER) .EQ. T2) THEN 
        NOTFOUND=1
        ELSE
        FOUNDPER=FOUNDPER+1
        END IF
        END DO
        IF (NOTFOUND .EQ. 0) THEN
        TNT=TNT+1
        NSUBID(TNT)=T2
        END IF
       
C       DEPART(FOUNDPER)=INT(TD3)

       ELSE
       FOUNDPER=INT(T2)
C        DEPART(T2)=INT(TD3)
       END IF
       DEPART(FOUNDPER)=INT(TD3)
       IF (PRIORFIL(1:8) .EQ. "nogroups") THEN
        MAXG=MAXG+1
        DEPART(FOUNDPER)=MAXG
       END IF
      IF ((FOUNDPER .LE. NUMTEACH) .AND.
     C  (DEPART(FOUNDPER) .GT. MAXG)) THEN
       MAXG=DEPART(FOUNDPER)
      END IF
01686  CONTINUE
       NUMTEACH=TNT
       ELSE
       WRITE(33,4301) UPFILE, " not used because it did not contain"
       WRITE(33,4301) "valid data."
       WRITE(6,4301) UPFILE, " not used because it did not contain"
       WRITE(6,4301) "valid data."
      END IF
02167        CLOSE(214)
        IF (PRIORFIL(1:8) .EQ. "nogroups") THEN
        DO 5772 MAXG=1,NUMTEACH
        DEPART(MAXG)=MAXG
05772    CONTINUE
        END IF
        WRITE(33,1077) "MAXG",MAXG

       END IF

C      WRITE(33,1077) "TNUMSUB",TNUMSUB
       MAXLAB=52
       IF (MAXG .GT. MAXLAB) THEN
       MAXLAB=MAXG
       END IF

       IF ((NUMOBS .LT. 2) .OR.
     C (NUMTEACH .LE. 1) .OR. (MATTYPE .EQ.'p')) THEN
       IF ((INFILE .NE. "") .AND. (MATTYPE .NE. 'p')) THEN
       WRITE(6,4301) "There is no readbale data in your list file"
       WRITE(6,4301) "either input the name of a new data file"
       WRITE(6,4301) "or type 'none' to enter parameters interactively"
       READ(5,*) INFILE
       END IF

       IF ((INFILE .EQ. "none") .OR. (INFILE .EQ. "") .OR. 
     C (MATTYPE .EQ. "p")) THEN
       CALL GETPARS(EXPERM,NEVAL,DYDTRIAD,USETRIAD,NUMDYAD,NUMTEACH,
     C NEARVAL,STOPVAL,WPIK,
     C SYMMAT,THRESHT,LOOKT,DIRECT,KCOUNT2,BASEVAL,BOUNDVAL,
     C PCTILE,STRUCTEQ,NETWORK,ACTRSQR,QUICKEND,ROWWT,COLWT,
     C QUANTYPE,SQUAREIT,REWEIGHT,PRINTO,APRINT,NPO,INFILE,
     C MATTYPE,PRIORFIL,HITFILE,LABFILE,TITFILE,NUMHLIST,TITLES,
     C FANCY,NUMRES,RASEED,NETLEV,PERGROUP,TOPVAL,TRANSPOS,
     C MAXG,GCONTIN,NPARGRP,PARTITLE,PARFORM,LISTVAR,TCHAR,NEWGRPS,
     C FIXR,BLABOUND,INVERT,RECTMAT,BETWMULT,HIWTEVAL,MUTDYAD,
     C NONEG,MAXSEED,ATTACHI,HALFDYAD,DISSOLVE,GUSEMARG,TAGALONG,
     C NUMOBS,
     C  IGRATIO,NUMDIM,
     C CENTER,DANCHOR,DANCHOR2,MOVE2,ZSYMMAT,
     C STARTINC,BYINC,MAXINC,KEXP,NORMAL,MINVALG,
     C CENTERI,DANCHORI,DANCH2I,MOVE2I,ZSYMMATI,
     C STARTINI,BYINCI,MAXINCI,KEXPI,NORMALI,MINVALI,
     C RINCREM,MEASURE,EXTREME,DRADIUSG,DRADIUSI,
     C BYANGLE,BYSCALE,BYANGLEI,BYSCALEI,MINPICT,HYPERG,
     C PCTCENIG,PCTCENG2,PCTCENI1,PCTCENI2)

C     C CENTER,DANCHOR2,MOVE2,DANCHOR,ZSYMMAT,MAXINC,
C     C BYINC,STARTINC,NUMDIM,RINCREM,MEASURE,EXTREME,IGRATIO,HYPERG,
C     C DRADIUS,KEXP,NORMAL,NORMALI,MINVALG,MINVALI)
        ELSE
        GCONTIN=0
       END IF

       IF (GCONTIN .EQ. 0) THEN
       GOTO 8877
       END IF

       END IF 
          
C       BOTHP=2
       ONUMDYAD=NUMDYAD
       DONEONE=0
       CLOSE(9)
       CLOSE(214)
C       IF (DRADIUS .LE. 0) THEN 
C       DRADIUS=1
C       END IF
        OUSETRID=USETRIAD
       IF (HYPERG .LT. 0) THEN 
       HYPERG=0
       END IF
       IF (HYPERG .GT. 1) THEN
       HYPERG=1
       END IF

       IF (RECODEN .GT. 0) THEN 
       DO 21224 I=1,NUMTEACH
       DO 21225 J=1,NUMTEACH
       IF (RECODE(INMAT(I,J)) .NE. INMAT(I,J)) THEN
       INMAT(I,J)=RECODE(INMAT(I,J))
       END IF
21225   CONTINUE
21224    CONTINUE
       END IF
       IF (SYMMAT .GT. 0) THEN 
       ROWWT=1
       COLWT=1
C       BOTHP=1
        DO 45432 I=2,NUMTEACH
         DO 45431 J=1,(I-1)
          INMAT(I,J)=INMAT(I,J)+INMAT(J,I)
          INMAT(J,I)=INMAT(I,J)
45431     CONTINUE
45432     CONTINUE
        END IF

C      IF (NETLEV .EQ. 1) THEN
C       WRITE(33,4301)
C       write(3,4301) "Assigning nearval=0.000 and pctile=2.00 because"
C       WRITE(33,4301) "You assigned netlev =1"
C       WRITE(33,4301)
C       NEARVAL=0.0000
C       PCTILE=2.0
C      END IF
      IF (QUICKEND .EQ. 1) THEN
       WRITE(33,4301)
       WRITE(33,4301) "Assigning startgrp=3 because you assigned"
       WRITE(33,4301) "quickend =1 ."
       WRITE(33,4301)
      USETRIAD=3
      END IF
      
      OPEN(40,file=HITFILE)
      OPEN(51,file=LABFILE)
      OPEN(52,file=TITFILE)
C        READ(16,24546,END=55155) KNUMSIM,OMAXG,NUMGROU2,OKPGROUP,
       NUMHLIST=0
       READ(40,203,END=22203) NUMHLIST , (HITLIST(H) , H=1,NUMHLIST)
22203  READ(51,511,END=22204) (LABEL(LB) , LB=1,MAXLAB)
22204  READ(52,77,END=44333) (TITLES(T) , T=1,3)
       DO 44332 QR=1,NUMHLIST
        QHLIST(QR)=HITLIST(QR)
44332   CONTINUE
44333   QNHLIST=NUMHLIST
        CLOSE(51)
        CLOSE(52)
      OUTFILE=INFILE(1:6) // ".clusters"
      OPEN(3,file=OUTFILE)

      BFILE=INFILE(1:6) // ".boundary"
C      OPEN(555,file=BFILE)
      BFILE2=INFILE(1:6) // ".bound2"
C      OPEN(575,file=BFILE2)
C      TBFILEB=INFILE(1:6) // ".bddat"
C      OPEN(217,file=TBFILEB)

      IF (PRINTO(26) .EQ. 1) THEN
      BFILEB=INFILE(1:6) // ".boundB"
C      OPEN(210,file=BFILEB)
      BFILE2B=INFILE(1:6) // ".bound2B"
C      OPEN(211,file=BFILE2B)
      TBFILEB=INFILE(1:6) // ".Bbddat"
C      OPEN(26,file=TBFILEB)
      END IF

      IF (PRINTO(29) .EQ. 1) THEN
      TBFILEB=INFILE(1:6) // ".BTWdat"
C      OPEN(221,file=TBFILEB)
      END IF

      IF (PRINTO(14) .EQ. 1) THEN
      PFILE=INFILE(1:6) // ".place"
       END IF

       
        WRITE(33,3339) 
      CALL FDATE(idate) 
 
      WRITE(33,4301) "Welcome to KliqueFinder, software for identifying"
      WRITE(33,4301) "cohesive subgroups and mapping relations within "
      WRITE(33,4301) "and between cohesive subgroups.  For references"
      WRITE(33,4301) "see:"
      WRITE(33,4301) "Frank, K.A. (1996). Mapping interactions within" 
      WRITE(33,4301) "and between cohesive subgroups. Social Networks."
       WRITE(33,4301) "Volume 18, pages 93-119."
      WRITE(33,4301)
      WRITE(33,4301) "and"
      WRITE(33,4301)
      WRITE(33,4301) "Frank. K.A. (1995). Identifying Cohesive "
      WRITE(33,4301) "Subgroups. Social Networks, 17, 27-56."
      WRITE(33,4301)

      WRITE(33,1077) "DATE"
      WRITE(33,1077) IDATE
      WRITE(33,1077) "INPUT FILE: "
      WRITE(33,1077) INFILE 
      WRITE(33,1077) "DATA TYPE"
      WRITE(33,1077) MATTYPE
      IF ((MATTYPE .EQ. 'l') .OR. (MATTYPE .EQ. 'b')) THEN
      WRITE(33,1077) "DATA IN LIST FORM"
      ELSE
      WRITE(33,1077) "DATA IN MATRIX FORM"
      END IF
      WRITE(33,1077) "USING PRIOR GROUPS:"
      IF ((MATTYPE .EQ. 'g') .OR. (MATTYPE .EQ. 'b')) THEN
      WRITE(33,1077) PRIORFIL
      ELSE 
      WRITE(33,1077) INFILE
      END IF
C      IF (STRUCTEQ .EQ. 1) THEN
C      DIRECT=0
C      END IF

      WPIK=1.0000
      WRITE(33,1077)
      WRITE(33,1077) "PARAMETERS"
      DO 6446 I=1,NPARGRP
      WRITE(33,3337) PARTITLE(I)
      IF (I .EQ. 1) THEN
      WRITE(33,PARFORM(I)) NUMDYAD,DYDTRIAD,USETRIAD,EXPERM,RASEED
      END IF
      IF (I .EQ. 2) THEN
      WRITE(33,PARFORM(I)) DIRECT,THRESHT,LOOKT,MAXSEED
      END IF
      IF (I .EQ. 3) THEN
      WRITE(33,PARFORM(I)) BOUNDVAL,FIXR,BLABOUND,BETWMULT
      END IF
      IF (I .EQ. 4) THEN
      WRITE(33,PARFORM(I)) NEARVAL,PCTILE,MUTDYAD,NONEG,HALFDYAD,
     C DISSOLVE
      END IF

      IF (I .EQ. 5) THEN
      WRITE(33,PARFORM(I)) STOPVAL,KCOUNT2,QUICKEND,ATTACHI
      END IF

      IF (I .EQ. 6) THEN
      WRITE(33,PARFORM(I)) STRUCTEQ,NETWORK,ACTRSQR
      END IF

      IF (I .EQ. 7) THEN
      WRITE(33,PARFORM(I)) QUANTYPE,SQUAREIT,NETLEV,PERGROUP,COLWT,
     C ROWWT,HYPERG
      END IF

      IF (I .EQ. 8) THEN
      WRITE(33,PARFORM(I)) TRANSPOS,REWEIGHT,SYMMAT,INVERT,RECTMAT,
     C GUSEMARG,TAGALONG
      END IF

      IF (I .EQ. 9) THEN
      WRITE(33,PARFORM(I)) NEVAL,BASEVAL,TOPVAL,NUMRES,NEWGRPS,HIWTEVAL
      END IF
      IF (I .EQ. 10) THEN
      WRITE(33,PARFORM(I)) IGRATIO,NUMDIM,MINPICT,
     C CENTER,DANCHOR,DANCHOR2,MOVE2,ZSYMMAT,
     C STARTINC,BYINC,MAXINC,KEXP,NORMAL,MINVALG,BYANGLE,BYSCALE,
     C PCTCENG1,PCTCENG2,DRADIUSG
      END IF

      IF (I .EQ. 11) THEN
      WRITE(33,PARFORM(I)) CENTERI,DANCHORI,DANCH2I,MOVE2I,ZSYMMATI,
     C STARTINI,BYINCI,MAXINCI,KEXPI,NORMALI,MINVALI,BYANGLEI,
     C BYSCALEI,
     C PCTCENI1,PCTCENI2,DRADIUSI
      END IF
      IF (I .EQ. 12) THEN
      WRITE(33,PARFORM(I)) RINCREM,MEASURE,EXTREME
      END IF

      WRITE(33,3337) "",LISTVAR(I)
06446  CONTINUE

       IF (PRINTO(27) .EQ. 1) THEN
       OPEN(19,file='compmeas.dat')
       WRITE(33,4301) "Writing to End of Files"
       WRITE(33,4301)
       CALL MOVEEND(19)
       OPEN(25,file='compmeas.sas')
        WRITE(25,4301) "options linesize=80;"
        WRITE(25,4301) "options pagesize=59;"
      WRITE(25,4301) 
     C"TITLE1 'Analysis of Compactness Measures from simulated data';"
      WRITE(25,4301) "Title2 '",TITLES(1),TITLES(2),TITLES(3),"';"

       WRITE(25,4301) "data one; infile 'compmeas.dat';"
       WRITE(25,4301) "ATTRIB FILENAME LENGTH=$16; input"
       WRITE(25,4301) "filename 1-16 SSEED 17-36"
       WRITE(25,4301) " (SIMNUM MAXG KPGROUP NUMACT "
       WRITE(25,4301) " MAXCONN MAXWT) (10.5) /"
       WRITE(25,4301) "(KCOMPACT FCOMPACT TZI TZING TGRCOMP "
        WRITE(25,4301) "ACTG XTGRCOMP TMGRCOMP ACTG2"
       WRITE(25,4301) "XTMGRCMP IGRCOMP XIGRCOMP "
       WRITE(25,4301) "IMGRCOMP XIMGRCMP INUM"
       WRITE(25,4301) "FCOMPAC8 ONDIAG3 XONDIAG3"
       WRITE(25,4301) "FCOMPAC9 XONDIAG4 MEDSIZE TOTCENT TOTDENS "
       WRITE(25,4301) "INITCONN CHNGSIM CHNGPOSS CHNGSTD "
       WRITE(25,4301) "PEARSON LRATIO )"
       WRITE(25,4301) " (10.5);"

      WRITE(25,4301)
     C'label '
      WRITE(25,4301)
     C'CHNGSIM="COMMON ASSIGNMENTS -- PRIOR TO EMERGENT"'
      WRITE(25,4301)
     C'CHNGPOSS="POSS COMMON ASSIGNMENTS -- PRIOR TO EMERGENT"'
      WRITE(25,4301)
     C'CHNGSTD="STDIZED COMMON ASSIGNMENTS -- PRIOR TO EMERGENT"'
      WRITE(25,4301)
     C'PEARSON="PEARSONS MEASURE OF FIT"'
      WRITE(25,4301)
     C'LRATIO="LIKELIHOOD RATIO MEASURE OF FIT"'
      WRITE(25,4301)
     C'SIMNUM="SIMULATION NUMBER"'
        WRITE(25,4301)
     C'MAXG="NUMBER OF GROUPS (A PRIORI)"'
        WRITE(25,4301)
     C'KPGROUP="ACTORS PER GROUP (A PRIORI)"'
        WRITE(25,4301)
     C'NUMACT="NUMBER OF ACTORS"'
        WRITE(25,4301)
     C'MAXCONN="MAX CONNECTIONS FROM AN ACTOR"'
        WRITE(25,4301)
     C'MAXWT="MAX WEIGHT ASSIGNED TO A CONN"'
        WRITE(25,4301) 
     C'SSEED="seed value for this simulation"'
      WRITE(25,4301)
     C'KCOMPACT="KLIQFINDS COMPACTNESS -- LIKE HUBERT"'
      WRITE(25,4301)
     C'FCOMPACT="HUBERTS COMPACTNESS"'
      WRITE(25,4301)
     C'TZI="SUM OF THE ZI AT INDIVIDUAL LEVEL"'
      WRITE(25,4301)
     C'TZING="SUM OF ZI (OVER Ng) AT INDIVIDUAL LEVEL"'
      WRITE(25,4301)
     C'IGRCOMP="SUM OF GROUP COMPACTNESS -- I"'
      WRITE(25,4301)
     C'TGRCOMP="SUM OF GROUP COMPACTNESS -- G"'
      WRITE(25,4301)
     C'ACTG="ACTUAL NUMBER OF NONZERO GROUPS"'
      WRITE(25,4301)
     C'XTGRCOMP="Mean GROUP COMPACTNESS -- G"'
      WRITE(25,4301)
     C'XIGRCOMP="Mean GROUP COMPACTNESS -- I"'
      WRITE(25,4301)
     C'IMGRCOMP="SUM OF GROUP  COMPACTNESS (OVER Ng) -- I"'
      WRITE(25,4301)
     C'TMGRCOMP="SUM OF GROUP  COMPACTNESS (OVER Ng) -- G"'
      WRITE(25,4301)
     C'ACTG2="ACTUAL NUMBER OF NON-ZERO GROUPS"'
      WRITE(25,4301)
     C'XTMGRCMP="Mean GROUP COMPACTNESS (OVER Ng) -- G"'
      WRITE(25,4301)
     C'XIMGRCMP="Mean GROUP COMPACTNESS (OVER Ng) -- I"'
       WRITE(25,4301)
     C 'FCOMPAC8 ="Blau: Net-wide proportin conn in groups"'
       WRITE(25,4301)
     C 'ONDIAG3 ="Blau: Sum Diag, Mean Wthin Grp Conn"'
       WRITE(25,4301)
     C 'XONDIAG3 ="Blau: Mean Diag, Mean Wthin Grp Conn"'
       WRITE(25,4301)
     C 'FCOMPAC9 ="Blau: Net-Wide mean within group conn"'
       WRITE(25,4301)
     C 'XONDIAG4 ="Blau: Mean Diag, Prop conn in groups"'
      WRITE(25,4301)
     C'INUM="NUMBER OF NON-ISOLATES"'
        WRITE(25,4301) 
     C'MEDSIZE="MEDIAN SUBGROUP SIZE"'
        WRITE(25,4301) 
     C'TOTCENT="AVERAGE SUBGROUP CENTRALITY -- DENSITY"'
        WRITE(25,4301)
     C'INITCONN="# ACTORS WHO INITIATED CONNECTIONS"'
        WRITE(25,4301)
     C'TOTDENS="TOTAL DENSITY -- NO SUBGROUPS";'
      WRITE(25,4301)
      WRITE(25,4301)
     C ' array cmeas {24} '
      WRITE(25,4301)
     C 'kcompact fcompact tzi tzing tgrcomp '
      WRITE(25,4301)
     C 'xtgrcomp tmgrcomp xtmgrcmp igrcomp xigrcomp FCOMPAC9 XONDIAG4'
      WRITE(25,4301)
     C'imgrcomp ximgrcmp FCOMPAC8 ONDIAG3 XONDIAG3 TOTCENT TOTDENS'
      WRITE(25,4301)
     C 'CHNGSIM CHNGPOSS CHNGSTD PEARSON LRATIO;'
      WRITE(25,4301)
      WRITE(25,4301)
     C '*do i =1 to 24 ;'
      WRITE(25,4301)
     C '*cmeas{i}=cmeas{i}/inum ;'
      WRITE(25,4301)
     C '*end ;'
      WRITE(25,4301) "Proc sort;"
      WRITE(25,4301) "by filename;"
     

      WRITE(25,4301)
     C "data two;"
      WRITE(25,4301) "source='compact   ';"
      WRITE(25,4301) 
     C "infile 'central.dat' missover;"
      WRITE(25,4301) 
     C "input filename $ 1-16 type $ diag $ actg amean avar askew;"
      WRITE(25,4301) "Proc sort;"
      WRITE(25,4301) "by filename;"

      WRITE(25,4301) 
     C "data three;"
      WRITE(25,4301) "source='density';"
      WRITE(25,4301) 
     C "infile 'central.bdat' missover;"
      WRITE(25,4301) 
     C "input filename $ 1-16 type $ diag $ actg bmean bvar bskew;"
      WRITE(25,4301) "Proc sort;"
      WRITE(25,4301) "by filename;"

      WRITE(25,4301) 
     C "data four;"
      WRITE(25,4301) "source='Zi';"
      WRITE(25,4301) 
     C "infile 'central.cdat' missover;"
      WRITE(25,4301) 
     C "input filename $ 1-16 type $ diag $ actg cmean cvar cskew;"
      WRITE(25,4301) "Proc sort;"
      WRITE(25,4301) "by filename;"

      WRITE(25,4301) 
     C "data five;"
      WRITE(25,4301) "source='PctWithin';"
      WRITE(25,4301) 
     C "infile 'central.ddat' missover;"
      WRITE(25,4301) 
     C "input filename $ 1-16 type $ diag $ actg dmean dvar dskew;"
      WRITE(25,4301) "Proc sort;"
      WRITE(25,4301) "by filename;"

      WRITE(25,4301) 
     C "data six;"
      WRITE(25,4301) 
     C "merge one two three four five;"
      WRITE(25,4301) 
     C "by filename;"
      WRITE(25,4301) "Proc print;"
      WRITE(25,4301)
      WRITE(25,4301)
      WRITE(25,4301) 

     C 'proc corr;'
      WRITE(25,4301)
     C 'var maxg kpgroup numact maxconn maxwt actg MEDSIZE initconn;'
      WRITE(25,4301)
     C 'with kcompact fcompact tzi tzing tgrcomp '
      WRITE(25,4301)
     C 'xtgrcomp tmgrcomp xtmgrcmp igrcomp xigrcomp FCOMPAC9 XONDIAG4'
      WRITE(25,4301)
     C'imgrcomp ximgrcmp FCOMPAC8 ONDIAG3 XONDIAG3 TOTCENT TOTDENS'
      WRITE(25,4301)
     C 'CHNGSIM CHNGPOSS CHNGSTD PEARSON LRATIO;'
      WRITE(25,4301)
      WRITE(25,4301)
     C 'proc plot; plot'
      WRITE(25,4301)
     C '(kcompact fcompact tzi tzing tgrcomp '
      WRITE(25,4301)
     C ' xtgrcomp tmgrcomp xtmgrcmp igrcomp xigrcomp '
      WRITE(25,4301)
     C'imgrcomp ximgrcmp FCOMPAC8 ONDIAG3 XONDIAG3'
      WRITE(25,4301) 'FCOMPAC9 XONDIAG4 TOTCENT TOTDENS'
      WRITE(25,4301) ' PEARSON LRATIO)'
      WRITE(25,4301)
     C '*(maxg kpgroup numact maxconn maxwt actg MEDSIZE initconn);'
       CLOSE(25)
      END IF
      WRITE(33,1077)

      WRITE(33,2109)
      WRITE(33,2109) "The objective function to be maximized is:"
      IF (QUANTYPE .EQ. 0) THEN
      QUANTYPE=1
      END IF

      IF (QUANTYPE .EQ. 1) THEN
      WRITE(33,2109) "Hubert's Compactness"
      END IF
 
      IF (QUANTYPE .EQ. 2) THEN
      WRITE(33,2109) "Pearson's Chi-Square from Logit (If squared)"
      END IF

      IF (QUANTYPE .EQ. 3) THEN
      WRITE(33,2109) "Likelihood Ratio Statistic from Logit"
      END IF

      IF (QUANTYPE .EQ. 4) THEN
      WRITE(33,2109) "Density (FACTIONS)"
      END IF

      IF (QUANTYPE .EQ. 5) THEN
      WRITE(33,2109) "Theta1 (see Frank, 1994)"
      END IF

  

      IF (QUANTYPE .EQ. -3) THEN
      WRITE(33,2109) "Likelihood Ratio Statistic from Logit"
      WRITE(33,2109) "Using penalty associated"
      WRITE(33,2109) "with Bayesian Loss Function"
      WRITE(33,2109) "See Raftery, (1986)"
      END IF

      WRITE(33,2109)
      IF (SQUAREIT .EQ. 1) THEN
      WRITE(33,2109) "The distances will be squared"
      END IF

      WRITE(33,2109)
      IF (PERGROUP .EQ. 1) THEN
      WRITE(33,2109) "The distances will be divided by group size"
      END IF
      WRITE(33,2109)
      IF (NETLEV .EQ. 1) THEN
      WRITE(33,2109) "The distances will be taken at"
      WRITE(33,2109) "the network level."
      END IF
      WRITE(33,2109)
       FINAL=1
       IF (RECTMAT .EQ. 1) THEN
       TRANSPOS=1
       END IF

       DO 04333 MBI=1,NUMTEACH

       IF ((INMAT(MBI,MBI) .GT. 0) .AND. (STRUCTEQ .NE. 1)) THEN
        WRITE(33,04334) MBI,INMAT(MBI,MBI)
        INMAT(MBI,MBI) = 0
       END IF

       IF (TRANSPOS .EQ. 1) THEN
       DO 2233 MBJ=1,MBI
       TPLACE=INMAT(MBI,MBJ)
       INMAT(MBI,MBJ)=INMAT(MBJ,MBI)
       INMAT(MBJ,MBI)=TPLACE
02233   CONTINUE
       END IF
04333   CONTINUE

       IF (RECTMAT .GT. 0) THEN
       IF ((STRUCTEQ .EQ. 1) .AND. (NETWORK .EQ. 1)) THEN
        THIWT=0
        DO 93942 I=1,NUMTEACH
        DO 93944 J=1,NUMTEACH
       IF (INMAT(I,J) .GT. THIWT) THEN
        THIWT=INMAT(I,J)
       END IF
93944   CONTINUE
93942   CONTINUE

        DO 93941 I=1,NUMTEACH
        INMAT(I,I)=THIWT
93941    CONTINUE
       END IF

       IF (GUSEMARG .EQ. 1) THEN
       HIMARG=0
       DO 87873 I=1,NUMTEACH
       GRPMARG(I)=0
87873   CONTINUE
       DO 87871 J=1,NUMTEACH
       DO 87872 I=1,NUMTEACH
       IF (INMAT(I,J) .GT. 0) THEN
       GRPMARG(J)=GRPMARG(J)+INMAT(I,J)
       END IF
87872   CONTINUE
        IF (GRPMARG(J) .GT. HIMARG) THEN
        HIMARG=GRPMARG(J)
         END IF
87871    CONTINUE

       END IF
       
       DO 9856 MBI=2,NUMTEACH
       DO 5545 I=1,(MBI-1.)
       DO 5546 J=1,NUMTEACH
       ADDON=INMAT(I,J)*INMAT(MBI,J)
       IF ((GUSEMARG .EQ. 1) .AND. (GRPMARG(J) .GT. 0)) THEN
        ADDON=INT(HIMARG*ADDON/GRPMARG(J))
       END IF
       NEWMAT(I,MBI)=NEWMAT(I,MBI)+ADDON
05546  CONTINUE
05545   CONTINUE
09856    CONTINUE
        DO 9866 I=2,NUMTEACH
        DO 9836 J=1,(I-1.)
        INMAT(I,J)=NEWMAT(J,I)
        INMAT(J,I)=NEWMAT(J,I)
09836    CONTINUE
        INMAT(I,I)=0
09866     CONTINUE
        INMAT(1,1)=0
        END IF



C        INVERTING
        HIWT=0
        MAXCHOIC=0
        DO 8996 I=1,NUMTEACH
        THISMAX=0
        DO 8997 J=1,NUMTEACH
        IF (INMAT(I,J) .NE. 0) THEN
        THISMAX=THISMAX+1
        IF (INMAT(I,J)  .GT. HIWT) THEN
        HIWT=INMAT(I,J)
        END IF
        END IF
08997    CONTINUE
        IF (THISMAX .GT. MAXCHOIC) THEN
        MAXCHOIC=THISMAX
         END IF
08996     CONTINUE
         HOWWIDE=1
         IF (HIWT .GT. 9) THEN
         HOWWIDE=INT(LOG10(HIWT))+2.0
         END IF

       IF (INVERT .EQ. 1) THEN
       DO 28996 I=2,NUMTEACH
       DO 28997 J=1,(I-1.)
       INMAT(I,J)=HIWT-INMAT(I,J)
       INMAT(J,I)=HIWT-INMAT(J,I)
28997   CONTINUE
28996   CONTINUE
        END IF

C      WRITE(60,187) "PREREMOVE", NUMTEACH

      IF (REWEIGHT .NE. 0) THEN
      IF (HIWT .GT. REWEIGHT) THEN
       HIWT=REWEIGHT
      END IF
       DO 9876 I=1,NUMTEACH
       DO 98765 J=1,NUMTEACH
        IF (INMAT(I,J) .GT. REWEIGHT) THEN
         INMAT(I,J)=REWEIGHT
         END IF
98765     CONTINUE
09876      CONTINUE
       END IF
           
        KNUMSIM=1
       QMAXG=MAXG
       IF (MAXG .NE. 0) THEN
       QKPGROUP=INT(NUMTEACH/MAXG)
       END IF
       QNUMTEAC=NUMTEACH
       QMAXCHOI=MAXCHOIC
       QMAXFREQ=HIWT
        IF (SIMO .EQ. 1) THEN
        LISTFILE=INFILE (1:6) // ".tplac"

        PRINTO(27)=1
C       GENERATE SIMULATED DATA
     
C       READ PARAMETERS FOR SIMULATE
        READ(16,24546,END=55155) KNUMSIM,OMAXG,NUMGROU2,OKPGROUP,
     C  KPGROU2,
     C  ONUMTEAC,NUMPEOP2,OMAXCHOI,MAXCHOI2,OMAXFREQ,MAXFRE2,SSEED,
     C USEMARG,OMAXPIW,MAXPIW,OMAXPIB,MAXPIB,REGSIM,BYDENSE,SIMRAND,
     C OBASEP,BASEP2,ORANGEP,RANGEP2,COMPSIM,
     C OMAXPWW,MAXPWW,OMAXPWB,MAXPWB,OBASEWP,BASEWP2,OWRANGEP,
     C WRANGEP2,XOBASEP,XBASEP2,XORANGEP,XRANGEP2,
     C OPREPWI,PREPWI2,OPREQWI,PREQWI2, 
     C OPREPBT,PREPBT2,OPREQBT,PREQBT2,PKPG,QKPG,PSIZE,QSIZE
        CLOSE(16)
55155   IF ((OPREPWI .LE. 0) .OR. (OPREPWI .GT. 999)) THEN
        PSIZE=10
        QSIZE=10
        QKPG=10
        PKPG=10
        PREQBT2=200
        OPREQBT=200
        PREPBT2=200
        OPREPBT=200
        PREQWI2=200
        OPREQWI=200
        PREPWI2=200
        OPREPWI=200
        END IF

        SSEED=416151632
        MSEED=416151633
        PKPG=PKPG/10
        QKPG=QKPG/10
        PSIZE=PSIZE/10.0
        QSIZE=QSIZE/10.0
        OBASEWP=OBASEWP/10.00
        BASEWP2=BASEWP2/10.00
        WRANGEP2=WRANGEP2/10.00
        OWRANGEP=OWRANGEP/10.00
        ORANGEP=ORANGEP/10.00
        RANGEP2=RANGEP2/10.00

        XORANGEP=XORANGEP/10.00
        XRANGEP2=XRANGEP2/10.00

        OBASEP=OBASEP/10.00
        BASEP2=BASEP2/10.00

        XOBASEP=XOBASEP/10.00
        XBASEP2=XBASEP2/10.00

        OPREPWI=OPREPWI/100.00
        OPREQWI=OPREQWI/100.00
        PREPWI2=PREPWI2/100.00
        PREQWI2=PREQWI2/100.00
        OPREPBT=OPREPBT/100.00
        OPREQBT=OPREQBT/100.00
        PREPBT2=PREPBT2/100.00
        PREQBT2=PREQBT2/100.00

        OMAXPWW=OMAXPWW/100.00
        MAXPWW=MAXPWW/100.00
        OMAXPWB=OMAXPWB/100.00
        MAXPWB=MAXPWB/100.00
        MAXPIW=MAXPIW/100.00
        OMAXPIW=OMAXPIW/100.00
        MAXPIB=MAXPIB/100.00
        OMAXPIB=OMAXPIB/100.00
         IF (KNUMSIM .GT. 1) THEN
C     C OMAXPWW,MAXPWW,OMAXPWB,MAXPWB,OBASEWP,BASEWP2)
        IF (RMISS .LT. .00001) THEN
        RMISS=(EMISS-BMISS)/(KNUMSIM-1.)
        END IF
        PPWIINC=(PREPWI2-OPREPWI)/(KNUMSIM-1.) 
        PQWIINC=(PREQWI2-OPREQWI)/(KNUMSIM-1.) 

        PPBTINC=(PREPBT2-OPREPBT)/(KNUMSIM-1.) 
        PQBTINC=(PREQBT2-OPREQBT)/(KNUMSIM-1.) 

        BASEWPIN=(BASEWP2-OBASEWP)/(KNUMSIM-1.)
        BASEPINC=(BASEP2-OBASEP)/(KNUMSIM-1.)
        XBPINC=(XBASEP2-XOBASEP)/(KNUMSIM-1.)
        GRINC=(NUMGROU2-OMAXG)/(KNUMSIM-1.)
        PGINC=(KPGROU2-OKPGROUP)/(KNUMSIM-1.)
        RPINC=(RANGEP2-ORANGEP)/(KNUMSIM-1.)
        XRPINC=(XRANGEP2-XORANGEP)/(KNUMSIM-1.)
        WRPINC=(WRANGEP2-OWRANGEP)/(KNUMSIM-1.)
        NPINC=(NUMPEOP2-ONUMTEAC)/(KNUMSIM-1.)
        MCINC=(MAXCHOI2-OMAXCHOI)/(KNUMSIM-1.)
        MFINC=(MAXFRE2-OMAXFREQ)/(KNUMSIM-1.)
        GINDEPT=(MAXPIW-OMAXPIW)/(KNUMSIM-1.)
        GBDEPT=(MAXPIB-OMAXPIB)/(KNUMSIM-1.)

        GWINDEPT=(MAXPWW-OMAXPWW)/(KNUMSIM-1.)
        GWBDEPT=(MAXPWB-OMAXPWB)/(KNUMSIM-1.)
        
         ELSE
         BASEPINC=0
         BASEWPIN=0

        GRINC=0
        PGINC=0
        XBPINC=0
        XRPINC=0
        RPINC=0
        WRPINC=0
        NPINC=0
        MCINC=0
        MFINC=0
        GINDEPT=0
        GBDEPT=0

        GWINDEPT=0
        GWBDEPT=0
         END IF        

        ONUMRES=NUMRES


       END IF
C       HAVE SIMULATED DATA

       DO 4545 NSIM=1,KNUMSIM 
        IF (SIMO .EQ. 1) THEN
       IF (USEMARG .EQ. 1) THEN
      NUMHLIST=QNHLIST
      DO 4433 QR=1,NUMHLIST
      HITLIST(QR)=QHLIST(QR)
04433  CONTINUE
       ELSE
       NUMHLIST=0
       END IF


C      CALL REMOVEO(INMAT,NUMTEACH,HITLIST,NUMHLIST,DEPART,NEWMAT,
C     C               NEWDEPT,USETRIAD,SUBID,NSUBID,KLIST)

       IF (USEMARG .EQ. 0) THEN     
       IF (MAKEMISS .EQ. 1) THEN   
       NOWMISS=BMISS+RMISS*(NSIM-1.)
       END IF 
C   PREPWI,PREQWI,PREPBT,PREQBT)
       PREPWI=(OPREPWI+PPWIINC*(NSIM-1.))
       PREQWI=(OPREQWI+PQWIINC*(NSIM-1.))
       PREPBT=(OPREPBT+PPBTINC*(NSIM-1.))
       PREQBT=(OPREQBT+PQBTINC*(NSIM-1.))
       MAXG=INT(OMAXG+GRINC*(NSIM-1.))
       RANGEP=(ORANGEP+RPINC*(NSIM-1.))
       XRANGEP=(XORANGEP+XRPINC*(NSIM-1.))
       KPGROUP=INT(OKPGROUP+PGINC*(NSIM-1.))
       NUMTEACH=INT(ONUMTEAC+NPINC*(NSIM-1.))
       MAXCHOIC=INT(OMAXCHOI+MCINC*(NSIM-1.))
       MAXFREQ=INT(OMAXFREQ+MFINC*(NSIM-1.))
       PIW=OMAXPIW+GINDEPT*(NSIM-1.)
       PIB=OMAXPIB+GBDEPT*(NSIM-1.)
       BASEP=OBASEP+BASEPINC*(NSIM-1.)
       XBASEP=XOBASEP+XBPINC*(NSIM-1.)
       BASEWP=OBASEWP+BASEPWIN*(NSIM-1.)
       WPIW=OMAXPWW+GWINDEPT*(NSIM-1.)
       WPIB=OMAXPWB+GWBDEPT*(NSIM-1.) 
       WRANGEP=(OWRANGEP+WRPINC*(NSIM-1.))
C     C OMAXPWW,MAXPWW,OMAXPWB,MAXPWB,OBASEWP,BASEWP2)
       DO 48343 I=1,50
       INDEPT(I)=PIW
        IF (REGSIM .EQ. 4) THEN
       TNUM=NUMTEACH-50
       TMAX=KPGROUP+MAXCHOIC-12+1
       PL=.790360+(.0051540)*TNUM+(-.016847)*TMAX+
     C (-.000027)*TNUM**2+(.002689)*TMAX**2+(-.000424)*TNUM*TMAX
       INDEPT(I)=PIW+PL-2*.0161
       END IF
        IF ((REGSIM .EQ. 0) .AND. (BYDENSE .EQ. 1)) THEN
       TNUM=NUMTEACH-50
       TMAX=KPGROUP+MAXCHOIC-12
       PL=.3170-(.004674)*TNUM+(-.0187395)*TMAX+
     C (.0000715)*TNUM**2-(.0005217)*TMAX**2+(-.000185)*TNUM*TMAX
       INDEPT(I)=PIW+PL-2*.01179
       END IF

       OUTDEPT(I)=PIB
48343   CONTINUE
              
       ELSE
       MAXG=QMAXG
       KPGROUP=QKPGROUP
       NUMTEACH=QNUMTEAC
       MAXCHOIC=QMAXCHOI
       MAXFREQ=QMAXFREQ
       MAXFREQ=INT(HIWT)
       RANGEP=QRANGEP
       END IF
C       IF (USEMARG .EQ. 0) THEN       
       NUMRES=ONUMRES
       FIXR=MAXCHOIC

       
        DO 4457 PID=1,NUMTEACH
        NSUBID(PID)=PID
        SUBID(PID)=PID
04457    CONTINUE
      IF (USETRIAD .EQ. 4) THEN
      OPEN(39,file='holdname2')
      REWIND(39)
      WRITE(339,7760) "sim",NSIM,".slist"
      CLOSE(39)
      OPEN(39,file='holdname2')
      READ(39,7761) LISTFILE
      CLOSE(39)
      OPEN(21,FILE=LISTFILE)

      NUMOBS=1
      MAXG=0
      NUMTEACH=0 
      DO 2822 I=1,74000
      READ(21,5101,END=2227) PI
      NUMOBS=NUMOBS+1
02822  CONTINUE
02227  REWIND(21)
       NUMOBS=NUMOBS-1
       WRITE(33,1077) "NUMOBS",NUMOBS
      DO 28243 I=1,NUMOBS
      READ(21,5101) PI,PJ,PK
      IF (PI .GT. NUMTEACH) THEN
      NUMTEACH=PI
      END IF
      IF (PJ .EQ. 99999) THEN
        DEPART(PI)=PK
        IF (PK .GT. MAXG) THEN
        MAXG=PK
        END IF

      ELSE
      IF ((PI .GT. 0) .AND. (PJ .GT. 0)) THEN
        INMAT(PI,PJ)=PK
      END IF
      IF (PJ .GT. NUMTEACH) THEN
      NUMTEACH=PJ
      END IF

      END IF
28243 CONTINUE
       CLOSE(21)

       WRITE(33,1077) "NUMTEACH",NUMTEACH
       DO 6820 PI=1,NUMTEACH
       NSUBID(PI)=PI
        DO 6821 PJ=1,NUMTEACH
        IF (INMAT(PI,PJ) .LT. 1) THEN
         INMAT(PI,PJ)=0
        END IF
06821    CONTINUE
06820     CONTINUE
      
       ELSE
C       USETRIAD .EQ. 4
C        SSEED=1443728851
C        REGSIM=0
        BYLAMN=0

        IF (REGSIM .EQ. 4) THEN
        BYLAMN=1
        END IF

        IF (REGSIM .EQ. 1) THEN
        CALL SIMULATE(MAXG,KPGROUP,NUMTEACH,MAXCHOIC,MAXFREQ,
     C INMAT,DEPART,SSEED,USEMARG)
        END IF
        IF ((REGSIM .EQ. 2) .OR. (BYLAMN .EQ. 1)) THEN
       BASEG=3
       FINAL=2
        CALL PISIM(MAXG,KPGROUP,NUMTEACH,MAXCHOIC,MAXFREQ,
     C INMAT,DEPART,SSEED,USEMARG,INDEPT,OUTDEPT,BYDENSE,SIMRAND,
     C PSIZE,QSIZE,BASEG,PKPG,QKPG,BYLAMN)

        END IF
        IF (REGSIM .EQ. 3) THEN 
       FINAL=2
       BASEG=3
       CALL PISIM2(NUMTEACH,KPGROUP,MAXCHOIC,MAXG,BASEG,
     C BASEP,PIW,PIB,INMAT,SSEED,DEPART,MAXFREQ,RANGEP,
     C BASEWP,WPIW,WPIB,WRANGEP,NSIM,XBASEP,XRANGEP,
     C PREPWI,PREQWI,PREPBT,PREQBT,PKPG,QKPG,PSIZE,QSIZE)
         END IF

        IF (COMPSIM .GT. 0) THEN
        SPCTILE=PCTILE
        SNEARVAL=NEARVAL
        USETRIAD =3
        END IF

        DONEONE=0
       IF (PRINTO(24) .EQ. 1) THEN
       WRITE(6,4301)
       WRITE(6,4301) "Simulated Data Set #"
       WRITE(6,5101) NSIM
       WRITE(6,4301)
       WRITE(6,4301) "SSEED,NSIM,"
       WRITE(6,4301)" MAXG,KPGROUP,NUMTEACH,MAXCHOIC,MAXFREQ,"
       WRITE(6,4301) "PIWITHIN,PIBETWEEN"
       WRITE(6,76543) SSEED
       WRITE(6,251) (1.0*NSIM),
     C MAXG,KPGROUP,NUMTEACH,MAXCHOIC,MAXFREQ,
     C PIW,PIB,BASEG,BASEP,RANGEP,BASEWP,WPIW,WPIB,WRANGEP

       WRITE(6,4301)
       END IF
       WRITE(33,4301)
       WRITE(33,4301) "Simulated Data Set #"
       WRITE(33,5101) NSIM
       WRITE(33,4301)
       WRITE(33,4301) "SSEED,MAXG,KPGROUP,NUMTEACH,MAXCHOIC,MAXFREQ"
       WRITE(33,4301) "PIWITHIN,PIBETWEEN"
       WRITE(33,4301) "BASEG,BASEP,RANGEP,BASEWP,WPIW,WPIB,WRANGEP"
       WRITE(33,76543) SSEED
       WRITE(33,251) NSIM,MAXG,KPGROUP,NUMTEACH,MAXCHOIC,MAXFREQ,
     C PIW,PIB,BASEG,BASEP,RANGEP,BASEWP,WPIW,WPIB,WRANGEP


       WRITE(33,4301)

       END IF
       IF (PRINTO(14) .EQ. 1) THEN
        OPEN(56,file=LISTFILE)
      DO 8166 PID=1,NUMTEACH
       WRITE(56,251) PID,NSUBID(PID),NEWDEPT(PID),NSIM,1
 8166   CONTINUE
         CLOSE(56)
        END IF

       END IF        
       IF (PRINTO(27) .EQ. 1) THEN
       WRITE(19,7654) INFILE,SSEED
       WRITE(19,1251) NSIM,MAXG,KPGROUP,NUMTEACH,MAXCHOIC,MAXFREQ,
     C PIW,PIB,BASEG,BASEP,RANGEP,BASEWP,WPIW,WPIB,WRANGEP,NOWMISS
       END IF
C      IF (MAKEMISS .EQ. 1) THEN
C       CALL KMISS(NOWMISS,NUMTEACH,SSEED,HITLIST,NUMHLIST)
C      END IF
      IF (REWEIGHT .NE. 0) THEN
       DO 9176 I=1,NUMTEACH
       DO 91765 J=1,NUMTEACH
        IF (INMAT(I,J) .GT. REWEIGHT) THEN
         INMAT(I,J)=REWEIGHT
         END IF
91765     CONTINUE
09176      CONTINUE
       END IF
C       IF (TAGALONG .GT. 0) THEN
      CALL REMOVEO(INMAT,NUMTEACH,HITLIST,NUMHLIST,DEPART,
     C               NEWDEPT,USETRIAD,SUBID,NSUBID,KLIST,PAIRS,PAIRUP,
     C   CONVID,TAGALONG,STRUCTEQ)
C       END IF
      DO 26 OD=1,NUMTEACH
       IF (NEWDEPT(OD) .EQ. 0) THEN
       NEWDEPT(OD)=MAXG+1
       MAXG=MAXG+1
       END IF

       SDEPT(OD)=NEWDEPT(OD)
       B(OD)=MOD(OD,10)
   26 CONTINUE
        IF (DOMULT .EQ. 1) THEN 
         DO 54044 I=1,NUMTEACH
         OUTCOME(I)=-9999
54044     CONTINUE
        PFILE=INFILE(1:6) // ".oc"
        OPEN(57,file=PFILE)
        DO 33202 I=1,251
        READ(57,251,END=52166) T2,OC1
        T1=CONVID(T2)
        IF ((T1 .GT. 0) .AND. (T1 .LT. 251) ) THEN
        OUTCOME(T1)=OC1
        END IF
33202    CONTINUE
        
52166    CLOSE(57)


C         CALL ZM(NUMTEACH,NEWMAT,NEWDEPT,OUTCOME,OUTMUL,
C     C   OTCOUNT)
        PFILE=INFILE(1:6) // ".ml"
        OPEN(57,file=PFILE)
        DO 98981 I=1,NUMTEACH
        WRITE(57,8251) NSUBID(I), (OUTMUL(U,I) , U=1,5) ,
     C  (OTCOUNT(U,I) , U=1,5)
C       READ(57,101) NPO , (PRINTO(PO) , PO = 1,NPO)
98981    CONTINUE
        CLOSE(57)
         STOP
         END IF

      CALL HUBIZE(NEWDEPT,NUMTEACH,HUBERT,STRUCTEQ)

C     INITIALIZE SUMS, MAX DEPT, DEPARTMENT SIZE COUNTER
      MAXDEPT=0

      H=0
      CALL ARCHOICE(NUMTEACH,NEWMAT,RCHOICE,MAXCH,NFIXR,
     C FLAGR,MEANWT,VARWT)
C      CALL MEANVAR(NUMTEACH,PWEIGHTS,RCHOICE,MEANWT,VARWT)
C
C     ASSIGN PEOPLE TO GROUPS ACCORDING TO RAW DATA

      CALL ASSIGN(NUMTEACH,NEWDEPT,MAXDEPT,
     C DEPARTN,RCHOICE,MEANWT,VARWT,
     C 1,2)

C    FIGURE OUT THE CONNECTION BETWEEN EACH PERSON ACCORDING TO COMPACTNESS
      KTEMPS=2
      DO 64 MBI=2,NUMTEACH
       DO 66 MBJ=1,(MBI-1)
         ETIE=0
         EVAR=0
         ACTTIE=0
         ZCOMPMAT(MBI,MBJ)=0
         IF ((RCHOICE(MBI) .GT. 0) .AND. (ROWWT .GT. 0)) THEN
         CALL DISTRIB (RCHOICE(MBI),MEANWT(MBI),
     C    VARWT(MBI),AMEAN,AVAR,KTEMPS,NUMTEACH,MBI,AVAR2,HYPERG)
         ETIE=AMEAN*ROWWT
         VTIE=AVAR*ROWWT**2
         ACTTIE=NEWMAT(MBI,MBJ)
          END IF
         IF ((RCHOICE(MBJ) .GT. 0) .AND. (COLWT .GT. 0)) THEN
         CALL DISTRIB (RCHOICE(MBJ),MEANWT(MBJ),
     C    VARWT(MBJ),AMEAN,AVAR,KTEMPS,NUMTEACH,MBI,AVAR2,HYPERG)
         ETIE=ETIE+AMEAN*COLWT
         VTIE=VTIE+AVAR*COLWT**2
         ACTTIE=ACTTIE+NEWMAT(MBJ,MBI)
          END IF
         IF (VTIE .GT. 0) THEN
        ZCOMPMAT(MBI,MBJ)=(ACTTIE-ETIE)/SQRT(VTIE)
         END IF
        ZCOMPMAT(MBJ,MBI)=ZCOMPMAT(MBI,MBJ)
   66  CONTINUE
         ZCOMPMAT(MBI,MBI)=1.00
   64 CONTINUE
        ZCOMPMAT(1,1)=1.000

      CALL FISHERZ (ZCOMPMAT,NUMTEACH)
      IF ((NUMTEACH .GT. 2) .AND. (PRINTO(21) .EQ. 1)) THEN
      STARTTRY=3
      CALL SASCENT (CHANGEC,STOPVAL,LOWMEM,NUMTEACH,
     C MADECHNG,ISO,ISOLIST,NEWDEPT,MEANWT,VARWT,WPIK,
     C NEARVAL,DEPARTN,MAXDEPT,
     C RCHOICE,DMEMBERS,QUITIT,LASTCHNG,NSUBID,
     C TITLES,KCOUNT2,PRINTO,BOUNDVAL,PCTILE,
     C DIRECT,TWXPX,SYMMAT,STARTTRY,IEXPECT,ISTD,ICON,
     C ACTRSQR,NOWCLOSE,STRUCTEQ,1,ROWWT,COLWT,QUANTYPE,
     C SQUAREIT,SOLUT,NETLEV,PERGROUP,INFILE,BLABOUND,MAXCH,MUTDYAD,
     C NONEG,MAXSEED,ATTACHI,HALFDYAD,DISSOLVE,HYPERG)

      CALL PRINTG(MAXDEPT,DEPARTN,NUMTEACH,
     C RCHOICE,NEWDEPT,MEANWT,VARWT,LABEL,TITLES,1,KLIST,
     C FINAL,SYMMAT,PRINTO,NSUBID,"A PRIORI GROUPS       ",IEXPECT,ISTD,
     C ICON,STRUCTEQ,NOWCLOSE,PERGROUP,QUANTYPE,SQUAREIT,
     C FIXR,INFILE,HOWWIDE,BETWMULT,QUICKEND,NSIM,BOUNDVAL,BLABOUND,
     C HIWT,
     C  IGRATIO,NUMDIM,
     C CENTER,DANCHOR,DANCHOR2,MOVE2,ZSYMMAT,
     C STARTINC,BYINC,MAXINC,KEXP,NORMAL,MINVALG,
     C CENTERI,DANCHORI,DANCH2I,MOVE2I,ZSYMMATI,
     C STARTINI,BYINCI,MAXINCI,KEXPI,NORMALI,MINVALI,
     C RINCREM,MEASURE,EXTREME,DRADIUSG,DRADIUSI,
     C BYANGLE,BYSCALE,BYANGLEI,BYSCALEI,MINPICT,
     C PCTCENG1,PCTCENG2,PCTCENI1,PCTCENI2,ROWWT,COLWT,SIMO,1,
     C ABICOORD,HYPERG)

      END IF

       IF (USETRIAD .EQ. 3) THEN 
       HOWSTART=" A PRIORI GROUPS"
       NUMDYAD=MAXDEPT
C      IF (MATTYPE .EQ. 's') THEN
C      OPEN(39,file='holdname2')
C      REWIND(39)
C      WRITE(339,7760) "sim",NSIM,".splace"
C      CLOSE(39)
C      OPEN(39,file='holdname2')
C      READ(39,7761) PLACEFILE
C      OPEN(210,FILE=PLACEFILE)
C      MAXG=0
C      DO 8111 PID=1,NUMTEACH
C       READ(210,251) KPID,NSUBID(PID),NEWDEPT(PID)
C      IF (NEWDEPT(PID) .GT. MAXG) THEN
C       MAXG=NEWDEPT(PID)
C      END IF
C 8111   CONTINUE
C      END IF

       END IF


       IF ((COMPSIM .EQ. 1) .OR. (USETRIAD .EQ. 1)) THEN
         IF (COMPSIM .EQ. 1) THEN
         NUMDYAD=ONUMDYAD
         END IF
 

        IF (DYDTRIAD .EQ. 1) THEN
         HOWSTART="BEST TRIADS"
       CALL KMULTMA2(NUMTEACH,RCHOICE,DIRECT,COLWT,ROWWT,
     C ONLIST,NUMDYAD,PERSON1,PERSON2,PERSON3)

C      I NOW HAVE NUMDYAD BEST MUTUALLY EXCLUSIVE TRIADS
C       IN TRIAD(90,3)

C     PERSON1, PERSON2, AND PERSON3 ARE ARRAYS OF THE PEOPLE
C      IN THE TRYADS (EXCLUSIVE) INTEGER (85)
C
C      CLOSEPAIR IS AN ARRAY OF THE CLOSENESS OF THE EXCLUSIVE TRIADS
C       REAL (251)
C
C      TRIDLIST IS A FOUR DIMENSIONAL ARRAY OF THE WHOLE TRIAD LIST
C       INTEGER 8000,3
C       CLOSEGRP IS A REAL ONE DIMENSIONAL (27000) ARRAY OF THE CLOSENESS
C          OF EACH GROUP


C       SUBROUTINE INITGRP(NUMDYAD,PERSON1,PERSON2,PERSON3,
C     C NEWDEPT,NUMTEACH,LIST,L)

      CALL INITGRP(NUMDYAD,PERSON1,PERSON2,PERSON3,NEWDEPT,
     C NUMTEACH,NLIST,NUMNLIST)       

       END IF
C        DYDTRIAD = 1

       IF (DYDTRIAD .EQ. 2) THEN
         HOWSTART="BEST DYADS"
       CALL KMULTMAD(ZCOMPMAT,NUMTEACH,RCHOICE,DIRECT,COLWT,ROWWT,
     C ONLIST,NUMDYAD,PERSON1,PERSON2)

C      I NOW HAVE NUMDYAD BEST MUTUALLY EXCLUSIVE TRIADS

      CALL INITGRPD(NUMDYAD,PERSON1,PERSON2,NEWDEPT,
     C NUMTEACH,NLIST,NUMNLIST)       

       END IF
C       DYDTRIAD = 2    

C      EXPERM=1
      STARTG=3
      MAXDEPT=NUMTEACH-2
      IF (EXPERM .EQ. 1) THEN
      DO 00141 EX=1,NUMTEACH
      GOTEM(EX) = 0
00141  CONTINUE
      GOTEM(PERSON1(1))=1
      GOTEM(PERSON2(1))=1
      IF (DYDTRIAD .EQ. 1) THEN
      GOTEM(PERSON3(1))=1
      MAXDEPT=MAXDEPT-1
      END IF
      TNDEPT=2
      DO 00747 EX2=1,NUMTEACH
        IF (GOTEM(EX2) .NE. 1) THEN
          NEWDEPT(EX2)=TNDEPT
          TNDEPT=TNDEPT+1
        END IF
00747    CONTINUE
       END IF

       END IF
C       USETRIAD=1



       IF ((COMPSIM .EQ. 2) .OR. (USETRIAD .EQ. 2)) THEN
       HOWSTART="RANDOM ASSIGN"
C      SUBROUTINE RANDASSG (ELEMENTS,NUMELEM,INSEED,NUMGROUP)
        CALL RANDASSG(NEWDEPT,NUMTEACH,RASEED,NUMDYAD)
       END IF
      WRITE(33,78) HOWSTART,NUMDYAD
      IF (EXPERM .NE. 1) THEN
      MAXDEPT=NUMDYAD
       END IF
C      WRITE(33,251) NEWMAT

      IF ((USETRIAD .NE. 3) .OR. (DONEONE .EQ. 1)) THEN
C      ONE=1
C      TWO=2
C      SUBROUTINE ASSIGN2(SUBJECTS,KAFFIL,MAXGROUP,
C     C COUNTGR)

      CALL ASSIGN2(NUMTEACH,NEWDEPT,MAXDEPT,
     C DEPARTN)
       END IF
       IF (NUMDYAD .LT. (NUMTEACH/3)) THEN
 
      IF (EXPERM .NE. 1) THEN
      CALL ATTACHO2 (CHANGEC,STOPVAL,LOWMEM,NUMTEACH,
     C MADECHNG,ISO,ISOLIST,NEWDEPT,MEANWT,VARWT,COLWT,
     C NEARVAL,DEPARTN,MAXDEPT,
     C RCHOICE,DMEMBERS,QUITIT,LASTCHNG,NSUBID,
     C TITLES,HYPERG)
       END IF

       END IF
C       NUMDYAD LT 
       IF (COMPSIM .GT. 0) THEN
       DO 2441 OD=1,NUMTEACH
       OLDEPTS(OD)=NEWDEPT(OD)
       NEWDEPT(OD)=SDEPT(OD)
02441   CONTINUE
         END IF

C       END IF
C       NUMDYAD LT 


      IF ((NUMTEACH .GT. 2) .AND. (PRINTO(5) .EQ. 1)) THEN
      STARTTRY=3
      CALL SASCENT (CHANGEC,STOPVAL,LOWMEM,NUMTEACH,
     C MADECHNG,ISO,ISOLIST,NEWDEPT,MEANWT,VARWT,WPIK,
     C NEARVAL,DEPARTN,MAXDEPT,
     C RCHOICE,DMEMBERS,QUITIT,LASTCHNG,NSUBID,
     C TITLES,KCOUNT2,PRINTO,BOUNDVAL,PCTILE,
     C DIRECT,TWXPX,SYMMAT,STARTTRY,IEXPECT,ISTD,ICON,
     C ACTRSQR,NOWCLOSE,STRUCTEQ,1,ROWWT,COLWT,QUANTYPE,
     C SQUAREIT,SOLUT,NETLEV,PERGROUP,INFILE,BLABOUND,MAXCH,MUTDYAD,
     C NONEG,MAXSEED,ATTACHI,HALFDYAD,DISSOLVE,HYPERG)

      CALL PRINTG(MAXDEPT,DEPARTN,NUMTEACH,
     C RCHOICE,NEWDEPT,MEANWT,VARWT,LABEL,TITLES,2,KLIST,
     C 1,SYMMAT,PRINTO,NSUBID,"AFTER INITIALIZATION",IEXPECT,ISTD,
     C ICON,STRUCTEQ,NOWCLOSE,PERGROUP,QUANTYPE,SQUAREIT,
     C FIXR,INFILE,HOWWIDE,BETWMULT,QUICKEND,NSIM,BOUNDVAL,BLABOUND,
     C HIWT,
     C  IGRATIO,NUMDIM,
     C CENTER,DANCHOR,DANCHOR2,MOVE2,ZSYMMAT,
     C STARTINC,BYINC,MAXINC,KEXP,NORMAL,MINVALG,
     C CENTERI,DANCHORI,DANCH2I,MOVE2I,ZSYMMATI,
     C STARTINI,BYINCI,MAXINCI,KEXPI,NORMALI,MINVALI,
     C RINCREM,MEASURE,EXTREME,DRADIUSG,DRADIUSI,
     C BYANGLE,BYSCALE,BYANGLEI,BYSCALEI,MINPICT,
     C PCTCENG1,PCTCENG2,PCTCENI1,PCTCENI2,ROWWT,COLWT,SIMO,2,
     C BBICOORD,HYPERG)

C     C CENTER,IGRATIO,DANCHOR2,MOVE2,DANCHOR,ZSYMMAT,MAXINC,
C     C BYINC,STARTINC,NUMDIM,RINCREM,MEASURE,EXTREME,HYPERG,DRADIUS,
C     C KEXP,NORMAL,NORMALI,MINVALI,MINVALG)

       END IF


C      DO WHILE (QUITIT .EQ. 0)

C      CALL DEBUGO(DEBUG(10))

      STARTTRY=3
       IF (STRUCTEQ .EQ. 1) THEN
      DO 6644 I=1,NUMTEACH
      IF ((NEWMAT(I,I) .EQ. 0) .AND. (NETWORK .EQ. 1)) THEN
      NEWMAT(I,I)= MAXCH
      END IF
06644  CONTINUE
      END IF

      SOLUT=1
C       KNEVAL=NEVAL

      DONEONE=0
      CALL SASCENT (CHANGEC,STOPVAL,LOWMEM,NUMTEACH,
     C MADECHNG,ISO,ISOLIST,NEWDEPT,MEANWT,VARWT,WPIK,
     C NEARVAL,DEPARTN,MAXDEPT,
     C RCHOICE,DMEMBERS,QUITIT,LASTCHNG,NSUBID,
     C TITLES,KCOUNT2,PRINTO,BOUNDVAL,PCTILE,
     C DIRECT,TWXPX,SYMMAT,STARTTRY,IEXPECT,ISTD,ICON,
     C ACTRSQR,NOWCLOSE,STRUCTEQ,QUICKEND,ROWWT,COLWT,QUANTYPE,
     C SQUAREIT,SOLUT,NETLEV,PERGROUP,INFILE,BLABOUND,MAXCH,MUTDYAD,
     C NONEG,MAXSEED,ATTACHI,HALFDYAD,DISSOLVE,HYPERG)
C       NEVAL=KNEVAL
       IF (COMPSIM .GT. 0) THEN
       DONEONE=1
       END IF
       IF (DEPARTN(MAXDEPT) .LT. 1) THEN
       MAXDEPT=MAXDEPT-1
       END IF
      IF (NUMTEACH .GT. 2) THEN
      CALL PRINTG(MAXDEPT,DEPARTN,NUMTEACH,
     C RCHOICE,NEWDEPT,MEANWT,VARWT,LABEL,TITLES,2,KLIST,
     C 2,SYMMAT,PRINTO,NSUBID,"   AFTER ASCENT     ",IEXPECT,ISTD,
     C ICON,STRUCTEQ,NOWCLOSE,PERGROUP,QUANTYPE,SQUAREIT,
     C FIXR,INFILE,HOWWIDE,BETWMULT,QUICKEND,NSIM,BOUNDVAL,BLABOUND,
     C HIWT,
     C  IGRATIO,NUMDIM,
     C CENTER,DANCHOR,DANCHOR2,MOVE2,ZSYMMAT,
     C STARTINC,BYINC,MAXINC,KEXP,NORMAL,MINVALG,
     C CENTERI,DANCHORI,DANCH2I,MOVE2I,ZSYMMATI,
     C STARTINI,BYINCI,MAXINCI,KEXPI,NORMALI,MINVALI,
     C RINCREM,MEASURE,EXTREME,DRADIUSG,DRADIUSI,
     C BYANGLE,BYSCALE,BYANGLEI,BYSCALEI,MINPICT,
     C PCTCENG1,PCTCENG2,PCTCENI1,PCTCENI2,ROWWT,COLWT,SIMO,3,
     C CBICOORD,HYPERG)

C     C CENTER,IGRATIO,DANCHOR2,MOVE2,DANCHOR,ZSYMMAT,MAXINC,
C     C BYINC,STARTINC,NUMDIM,RINCREM,MEASURE,EXTREME,HYPERG,DRADIUS,
C     C KEXP,NORMAL,NORMALI,MINVALI,MINVALG)

       END IF

      IF ((NUMRES .GT. 1) .AND. (PRINTO(24) .EQ. 1)) THEN
      QI=0
      HIQ=0
      DO 6765 Q=1,MAXDEPT
      IF (DEPARTN(Q) .GT. 1) THEN
      QI=QI+1
      IF (DEPARTN(Q) .GT. HIQ) THEN
      HIQ=DEPARTN(Q)
      END IF
      END IF
06765  CONTINUE

      WRITE(6,4301) "***********************************************"
      WRITE(6,6764) QI,HIQ
      WRITE(6,4301) "The sizes of the groups are:"
      WRITE(6,24545) (DEPARTN(J) , J=1,MAXDEPT)
      WRITE(6,4301) "***********************************************"
      END IF

      IF (PRINTO(10) .EQ. 1) THEN
      OPEN(30,file='clus.outmat')
      DO 2283 I=1,NUMTEACH
      WRITE(330,101) NEWDEPT(I) , (NEWMAT(I,J) , J=1,NUMTEACH)
02283 CONTINUE
      CLOSE(30)
      END IF



      IF (PRINTO(14) .EQ. 1) THEN
C      DO 8161 PID=1,NUMTEACH
C       WRITE(210,251) PID,NSUBID(PID),NEWDEPT(PID)
C 8161   CONTINUE


      OPEN(58,file=PFILE)

      DO 84166 PID=1,NUMTEACH
       XR1=PID
       XR2=NSUBID(PID)
       XR3=NEWDEPT(PID)
       XR4=NSIM
       XR5=3
       WRITE(58,251) XR1,XR2,XR3,XR4,XR5
84166   CONTINUE
      PMADE=1
      DO WHILE (PMADE .EQ. 1)
      PMADE=0
      DO 91662 PID=1,PAIRS
      DO 91663 PID2=1,PAIRS
      IF (PAIRUP(PID,2) .EQ. PAIRUP(PID2,1)) THEN
      PAIRUP(PID,2)=PAIRUP(PID2,2)
      PMADE=1
      END IF
91663  CONTINUE
91662   CONTINUE
       END DO

      DO 81662 PID=1,PAIRS
       IF (NEWDEPT(CONVID(PAIRUP(PID,2))) .GT. 0) THEN
       XR1=-PAIRUP(PID,2)
       XR2=PAIRUP(PID,1)
       XR3=NEWDEPT(CONVID(PAIRUP(PID,2)))
       XR4=NSIM
       XR5=3
       WRITE(58,251) XR1,XR2,XR3,XR4,XR5
C     -PAIRUP(PID,2),PAIRUP(PID,1),
C     C  NEWDEPT(CONVID(PAIRUP(PID,2))),NSIM,3
       END IF
81662  CONTINUE
        CLOSE(58)

         OPEN(27,file='place.sas')
        WRITE(27,4301) "options linesize=80;"
        WRITE(27,4301) "options pagesize=59;"

      WRITE(27,4301)
     C"TITLE1 'Placement of actors in groups';"
      WRITE(27,4301) "Title2 '",TITLES(1),TITLES(2),TITLES(3),"';"
      WRITE(27,4301) "data one;"
      WRITE(27,4301) 'infile "',PFILE,'" missover;'
      WRITE(27,4301) 
     C'input internid actid group;'
      WRITE(27,4301) 'proc freq;'
      WRITE(27,4301) 'tables group;'
      CLOSE(27)
      END IF

      IF (PRINTO(25) .EQ. 1) THEN
      IF (SIMO .EQ. 1) THEN
      OPEN(39,file='holdname2')
      REWIND(39)
      WRITE(339,7760) "sim",NSIM,".slist"
      CLOSE(39)
      OPEN(39,file='holdname2')
      READ(39,7761) LISTFILE
      ELSE
      LISTFILE=INFILE(1:6) // ".olist"
      END IF

      OPEN(57,file=LISTFILE)
      DO 6789 PI=1,NUMTEACH
       IF (NEWDEPT(PI) .NE. 0) THEN
      WRITE(57,5101) NSUBID(PI) ,99999,NEWDEPT(PI)
       END IF
      DO 6790 PJ=1,NUMTEACH
      IF (NEWMAT(PI,PJ) .NE. 0) THEN
      WRITE(57,5101) NSUBID(PI),NSUBID(PJ),NEWMAT(PI,PJ)
      END IF
06790  CONTINUE
06789   CONTINUE
      CLOSE(57)
      END IF

C       WRITE(219,7652) INFILE

      QVAL=-LASTCHNG

C      THIS IS THE BIG DO LOOP THAT ASKS IF WE SHOULD ACTUALL STOP
C
C           --------------------------------------------------
C

C      CALL HUBIZE(NEWDEPT,NUMTEACH,FINALM,STRUCTEQ)

      IF ((SIMO .EQ. 0) .AND. (PRINTO(6) .EQ. 1)) THEN 
        DO 8888 GR=1,MAXDEPT
         RDEPTN(GR)=DEPARTN(GR)
08888     CONTINUE
        TNUMT=NUMTEACH

       MBI=0
       FLAGI1=1
       CALL PROBS(MAXDEPT,NUMTEACH,DEPARTN,PAGB,
     C NEWDEPT,RCHOICE,FLAGI1,NUMTEACH,MAXDEPT,MBI)
       
      OPEN(99,file='grpprobs.out')
      OPEN(98,file='grpprobs.dat')
         WRITE(99,2102) (TITLES(A) , A=1,3)
         WRITE(99,2102) "PROBABILITIES"

         RMAXDEPT=MAXDEPT
         WRITE(99,2104) "CHANCE=" , 1/RMAXDEPT
         WRITE(99,2107) "GROUP", (ZG, ZG=1,MAXDEPT)
         WRITE(99,2107) "GROUP SIZE", (DEPARTN(ZG2) , ZG2=1,MAXDEPT)
         WRITE(99,2104) "PRIOR", (RDEPTN(ZG2)/TNUMT , 
     C   ZG2=1,MAXDEPT)
         WRITE(99,2101) "ACTOR" , "GROUP", "  PROB"
         PO=0
        DO 848 GROUP=1,MAXDEPT
C         HDEPARTN(GROUP)=DEPARTN(GROUP)
         PROBGRP(GROUP)=1.000
         DO 858 PERS=1,DEPARTN(GROUP)
         PROBGRP(GROUP)=PROBGRP(GROUP)*
     C                  PAGB(ALLGROUP(GROUP,PERS),GROUP)
         PO=PO+1
         PORD(PO)= ALLGROUP(GROUP,PERS)
         WRITE(99,2103) NSUBID(ALLGROUP(GROUP,PERS)), 
     C   GROUP ,
     C   (PAGB(ALLGROUP(GROUP,PERS),GR), 
     C   GR=1,MAXDEPT)
         WRITE(98,251) NSUBID(ALLGROUP(GROUP,PERS)), 
     C   GROUP ,
     C   (PAGB(ALLGROUP(GROUP,PERS),GR), 
     C   GR=1,MAXDEPT)
  858  CONTINUE
  848  CONTINUE
         WRITE(99,2101)
         WRITE(99,2101) "TOTAL" , "GROUP", "  PROB"
         WRITE(99,2103) 999999999,999999999,
     C   (PROBGRP(GR) , GR=1,MAXDEPT)
         WRITE(99,2101)
C       DO 1875 I=1,NUMTEACH
C        PNEWDEPT(I)=NEWDEPT(I)
C01875    CONTINUE

          CLOSE(98)
          CLOSE(99)
          END IF
C          PRINTO(6)
       
      IF ((PRINTO(6) .EQ. 1) .AND. (PRINTO(7) .EQ. 1) .AND. 
     C (SIMO .EQ. 0)) THEN 
      OPEN(99,file='actprobs.dat')
       DO 1876 PROBI=2,NUMTEACH
       DO 1877 PROBJ=1,(PROBI-1)
        PROBPER(PROBI,PROBJ)=0.000
        DO 1888 PROBGR=1,MAXDEPT
          PROBPER(PROBI,PROBJ)=PROBPER(PROBI,PROBJ)+PAGB(PROBJ,PROBGR)*
     C    PAGB(PROBI,PROBGR)
01888    CONTINUE
         PROBPER(PROBJ,PROBI)=PROBPER(PROBI,PROBJ)
         PROBPER(PROBJ,PROBJ)=1.0
         PROBPER(PROBI,PROBI)=1.0
         WRITE(99,251) NSUBID(PROBI),NEWDEPT(PROBI),
     C   NSUBID(PROBJ),NEWDEPT(PROBJ),PROBPER(PROBI,PROBJ)
01877    CONTINUE
         CLOSE(99)


01876    CONTINUE
         RMAXDEPT=MAXDEPT
      OPEN(99,file='actprobs.out')
         WRITE(99,2102) 
         WRITE(99,2102) 
         WRITE(99,2102) (TITLES(A) , A=1,3)
         WRITE(99,2102) "PERSON PROBS"
         WRITE(99,2104) "CHANCE=" , 1/RMAXDEPT
         WRITE(99,2101) "PERSON I" 
         WRITE(99,2107) "       PERSON J", 
     C   (NSUBID(PORD(ZG)), ZG=1,NUMTEACH)
         WRITE(99,2107) "       GROUP ",
     C    (NEWDEPT(PORD(ZG2)) , ZG2=1,NUMTEACH)


        DO 2084 GROUP=1,MAXDEPT

         DO 2085 PERS=1,DEPARTN(GROUP)
               WRITE(99,2103) NSUBID(ALLGROUP(GROUP,PERS)), 
     C   GROUP ,
     C   (PROBPER(ALLGROUP(GROUP,PERS),PORD(GR)), 
     C   GR=1,NUMTEACH)
02085     CONTINUE
02084      CONTINUE
          CLOSE(99)
          END IF
C          PRINTO(7)=1

C      SUBROUTINE EVAL(MAXDEPT,DEPARTN,ALLGROUP,NEWMAT,NUMTEACH,
C     C RCHOICE,OLDDEPT,HUBERT,MEANWT,VARWT,PRINTT,
C     C FINAL,FSYMMAT,PRINTO,FCOMPACT,CHNGSIM,NEVAL,BASEVAL,
CN     C NBESTDEP)
C            CALL DEBUGO(DEBUG(36))
      
      IF (NEVAL .GE. 1) THEN
      OPEN(58,file='eval.dat')
      OPEN(50,file='eval.sas')

C      NUMRES=1
         KQMAX=251
        UNO=1
         DUE=2
C       ON EXTERNAL ERROR CALL EXTERR (EXTPAR,EXTRES,EXTOP1,EXTOP2)
C       ON INTERNAL ERROR CALL INTERR (INTPAR,INTRES)
C       CALL CATCHEM (CPAR1)
      NUMRES=NUMRES+1
      CALL KEVAL(MAXDEPT,DEPARTN,NUMTEACH,
     C RCHOICE,NEWDEPT,MEANWT,VARWT,DUE,UNO,
     C SYMMAT,PRINTO,FCOMPACT,CHNGESIM,NEVAL,BASEVAL,
     C NBESTDEP,STRUCTEQ,NUMRES,TRYDEPT,QUANTYPE,TOPVAL,NEWGRPS,
     C PERGROUP,SQUAREIT,FIXR,HIWTEVAL,HYPERG,ROWWT,COLWT,
     C KQMAX,INFILE,SIMO)
C      IF (NUMRES .GT. 1) THEN
      

        WRITE(50,4301) "options linesize=80;"
        WRITE(50,4301) "options pagesize=59;"

      WRITE(50,4301) 
     C"TITLE1 'Evaluation of Solution Through Simulated Data';"
      WRITE(50,4301)
     C"TITLE2 'Is this Solution Like other Good Solutions?';"
      WRITE(50,4301) "Title2 '",TITLES(1),TITLES(2),TITLES(3),"';"
      WRITE(50,4301) "data one;"
      WRITE(50,4301) 'infile "eval.dat" missover;'
      WRITE(50,4301) 
     C'input (id pct chngcnt chngstd obfun increm hubert'
      WRITE(50,4301) ' hubdiag pearson g2 hubdiagx pearsonx g2x) '
      WRITE(50,4301) '(10.5);'
      WRITE(50,4301) 
     C 'indi=" ";'
      WRITE(50,4301)
     C'if pct=1.000 then indi="F";'

      WRITE(50,4301)
     C'label '
      WRITE(50,4301)
     C'hubert = "Huberts Measure of compactness"'
      WRITE(50,4301)
     C'obfun = "Value of your objective function"'
      WRITE(50,4301)
     C'chngstd="Stdized simlrty with final soltion"'
      WRITE(50,4301)
     C'pct ="pct of changes made from final sltion"'
      WRITE(50,4301)
     C'chngcnt="# in common with final"'
      WRITE(50,4301) 
     C'hubdiag="sum of group compactness measures"'
      WRITE(50,4301) 
     C'pearson="sum of pearson sqrd resids (group lev)"'
      WRITE(50,4301) 
     C'g2="sum of LRT resids (group level)"'
      WRITE(50,4301) 
     C'hubdiagx="sum of group compactness/Ng"'
      WRITE(50,4301) 'pearsonx="sum of pearson diagonals"'
      WRITE(50,4301) 'g2x="sum of LRT diagonals";'
      WRITE(50,4301)
     C'proc means;'
      WRITE(50,4301)
     C'proc reg;'
      WRITE(50,4301)
     C'model obfun hubert hubdiag pearson g2 hubdiagx pearsonx'
      WRITE(50,4301) 'g2x=chngstd;'
      WRITE(50,4301)
     C'proc plot;'
      WRITE(50,4301)
     C'title3 "* for generated, F for final";'
      WRITE(50,4301)
     C'plot (obfun hubert hubdiag pearson g2 hubdiagx pearsonx g2x)*'
      WRITE(50,4301) '(chngstd)=indi;'
      WRITE(50,4301)
     C'plot obfun*hubert; '
      WRITE(50,4301)
     C'endsas;'
      CLOSE(58)
      CLOSE(50)


       WRITE(33,2109) "EVALUATION COMPLETE.  OUTPUT IN eval.out"
       WRITE(33,2109) "SAS CODE IN eval.sas"
       IF (PRINTO(24) .EQ. 1) THEN
       WRITE(6,2109) "EVALUATION COMPLETE.  OUTPUT IN eval.out"
       WRITE(6,2109) "SAS CODE IN eval.sas"
       END IF

       END IF
       

       IF ((COMPSIM .GT. 0) .OR. (NUMRES .GT. 1)) THEN

       WRITE(33,2109)
       WRITE(33,2109) "SECOND SOLUTION"
       WRITE(33,2109)
       IF (PRINTO(24) .EQ. 1) THEN
       WRITE(6,2109)
       WRITE(6,2109) "SECOND SOLUTION"
       WRITE(6,2109)
       END IF
       QUICKEND=0
       SOLUT=2
       IF (COMPSIM .GT. 0) THEN
        PCTILE=SPCTILE
        NEARVAL=SNEARVAL
C        NONEG=SNONEG
       DO 72441 OD=1,NUMTEACH
       TRYDEPT(OD)=OLDEPTS(OD)
72441   CONTINUE

       END IF
      IF (MAKEMISS .EQ. 1) THEN

       DO 20773 OD=1,NUMTEACH
       MDEPT(OD)=NEWDEPT(OD)
       DO 20774 PERS=1,NUMTEACH
       NEWMAT(OD,PERS)=INMAT(OD,PERS)
20774   CONTINUE
20773    CONTINUE

       CALL KMISS(NOWMISS,NUMTEACH,MSEED,TAKEOUT)

       DO 62624 OD=1,NUMTEACH
       IF (TAKEOUT(OD) .EQ. 1) THEN
       DO 62625 PERS=1,NUMTEACH
       NEWMAT(OD,PERS)=0
62625  CONTINUE
        END IF
62624     CONTINUE

C      CALL REMOVEO(INMAT,NUMTEACH,HITLIST,NUMHLIST,MDEPT,
C     C               NEWDEPT,USETRIAD,SUBID,NSUBID,KLIST,PAIRS,PAIRUP,
C     C   CONVID,TAGALONG,STRUCTEQ)

      CALL ARCHOICE(NUMTEACH,NEWMAT,RCHOICE,MAXCH,NFIXR,
     C FLAGR,MEANWT,VARWT)
      END IF   

      CALL ASSIGN2(NUMTEACH,TRYDEPT,MAXDEPT,
     C DEPARTN)

      CALL SASCENT (CHANGEC,STOPVAL,LOWMEM,NUMTEACH,
     C MADECHNG,ISO,ISOLIST,TRYDEPT,MEANWT,VARWT,WPIK,
     C NEARVAL,DEPARTN,MAXDEPT,
     C RCHOICE,DMEMBERS,QUITIT,LASTCHNG,NSUBID,
     C TITLES,KCOUNT2,PRINTO,BOUNDVAL,PCTILE,
     C DIRECT,TWXPX,SYMMAT,STARTTRY,IEXPECT,ISTD,ICON,
     C ACTRSQR,NOWCLOSE,STRUCTEQ,QUICKEND,ROWWT,COLWT,QUANTYPE,
     C SQUAREIT,SOLUT,NETLEV,PERGROUP,INFILE,BLABOUND,MAXCH,MUTDYAD,
     C NONEG,MAXSEED,ATTACHI,HALFDYAD,DISSOLVE,HYPERG)
C      CALL SASCENT (CHANGEC,STOPVAL,LOWMEM,NUMTEACH,
C     C MADECHNG,ISO,ISOLIST,TRYDEPT,MEANWT,VARWT,WPIK,
C     C NEARVAL,DEPARTN,MAXDEPT,
C     C RCHOICE,DMEMBERS,QUITIT,LASTCHNG,NSUBID,
C     C TITLES,KCOUNT2,PRINTO,BOUNDVAL,PCTILE,
C     C DIRECT,TWXPX,SYMMAT,STARTTRY,IEXPECT,ISTD,ICON,
C     C ACTRSQR,NOWCLOSE,STRUCTEQ,QUICKEND,ROWWT,COLWT,QUANTYPE,
C     C SQUAREIT,SOLUT,NETLEV,PERGROUP,INFILE,BLABOUND,MAXCH,MUTDYAD,
C     C NONEG,MAXSEED,ATTACHI,HALFDYAD,DISSOLVE,HYPERG)

       IF (DEPARTN(MAXDEPT) .LT. 1) THEN
       MAXDEPT=MAXDEPT-1
       END IF

       THINKING=0
       IF ((THINKING .EQ. 1) .AND. (MAKEMISS .EQ. 1)) THEN
       DO 24773 OD=1,NUMTEACH
       IF (TAKEOUT(OD) .EQ. 1) THEN
       DO 24774 PERS=1,NUMTEACH
       NEWMAT(OD,PERS)=INMAT(OD,PERS)
24774   CONTINUE
       END IF
24773    CONTINUE

      CALL ARCHOICE(NUMTEACH,NEWMAT,RCHOICE,MAXCH,NFIXR,
     C FLAGR,MEANWT,VARWT)

      CALL SASCENT (CHANGEC,STOPVAL,LOWMEM,NUMTEACH,
     C MADECHNG,ISO,ISOLIST,TRYDEPT,MEANWT,VARWT,WPIK,
     C NEARVAL,DEPARTN,MAXDEPT,
     C RCHOICE,DMEMBERS,QUITIT,LASTCHNG,NSUBID,
     C TITLES,KCOUNT2,PRINTO,BOUNDVAL,PCTILE,
     C DIRECT,TWXPX,SYMMAT,STARTTRY,IEXPECT,ISTD,ICON,
     C ACTRSQR,NOWCLOSE,STRUCTEQ,1,ROWWT,COLWT,QUANTYPE,
     C SQUAREIT,SOLUT,NETLEV,PERGROUP,INFILE,BLABOUND,MAXCH,MUTDYAD,
     C NONEG,MAXSEED,ATTACHI,HALFDYAD,DISSOLVE,HYPERG)

      END IF   

C     THINKING

      IF (PRINTO(6) .EQ. 1) THEN 
        DO 8188 GR=1,MAXDEPT
         RDEPTN(GR)=DEPARTN(GR)
08188     CONTINUE
        TNUMT=NUMTEACH

      OPEN(99,file='grpprobs.out')
      OPEN(98,file='grpprobs.dat')

       MBI=0
       FLAGI1=1
       CALL PROBS(MAXDEPT,NUMTEACH,DEPARTN,PAGB,
     C TRYDEPT,RCHOICE,FLAGI1,NUMTEACH,MAXDEPT,MBI)
       

         WRITE(99,2102) (TITLES(A) , A=1,3)
         WRITE(99,2102) "PROBABILITIES"

         RMAXDEPT=MAXDEPT
         WRITE(99,2104) "CHANCE=" , 1/RMAXDEPT
         WRITE(99,2107) "GROUP", (ZG, ZG=1,MAXDEPT)
         WRITE(99,2107) "GROUP SIZE", (DEPARTN(ZG2) , ZG2=1,MAXDEPT)
         WRITE(99,2104) "PRIOR", (RDEPTN(ZG2)/TNUMT , 
     C   ZG2=1,MAXDEPT)
         WRITE(99,2101) "ACTOR" , "GROUP", "  PROB"
         PO=0
        DO 6848 GROUP=1,MAXDEPT
C         HDEPARTN(GROUP)=DEPARTN(GROUP)
         PROBGRP(GROUP)=1.000
         DO 6858 PERS=1,DEPARTN(GROUP)
         PROBGRP(GROUP)=PROBGRP(GROUP)*
     C                  PAGB(ALLGROUP(GROUP,PERS),GROUP)
         PO=PO+1
         PORD(PO)= ALLGROUP(GROUP,PERS)
         PFITDA(PO)=0
         PFITDB(PO)=0
         WRITE(99,2103) NSUBID(ALLGROUP(GROUP,PERS)), 
     C   GROUP ,
     C   (PAGB(ALLGROUP(GROUP,PERS),GR), 
     C   GR=1,MAXDEPT)
         WRITE(98,251) NSUBID(ALLGROUP(GROUP,PERS)), 
     C   GROUP ,
     C   (PAGB(ALLGROUP(GROUP,PERS),GR), 
     C   GR=1,MAXDEPT)
06858     CONTINUE
06848      CONTINUE
         WRITE(99,2101)
         WRITE(99,2101) "TOTAL" , "GROUP", "  PROB"
         WRITE(99,2103) 999999999,999999999,
     C   (PROBGRP(GR) , GR=1,MAXDEPT)
         WRITE(99,2101)
C       DO 1875 I=1,NUMTEACH
C        PNEWDEPT(I)=NEWDEPT(I)
C01875    CONTINUE

          CLOSE(98)
          CLOSE(99)
          END IF
C          PRINTO(6)
       
      IF ((PRINTO(6) .EQ. 1) .AND. (PRINTO(7) .EQ. 1)) THEN
       TMISSDA=0         
       TMISSDB=0         
       DO 21876 PROBI=2,NUMTEACH
       DO 21877 PROBJ=1,(PROBI-1)
        PROBPER(PROBI,PROBJ)=0.000
        DO 21888 PROBGR=1,MAXDEPT
          PROBPER(PROBI,PROBJ)=PROBPER(PROBI,PROBJ)+PAGB(PROBJ,PROBGR)*
     C    PAGB(PROBI,PROBGR)
21888    CONTINUE
         IF (PROBPER(PROBI,PROBJ) .LT. .05) THEN
         PROBPER(PROBI,PROBJ) =.05
         END IF
         PROBPER(PROBJ,PROBI)=PROBPER(PROBI,PROBJ)
         PROBPER(PROBJ,PROBJ)=1.0
         PROBPER(PROBI,PROBI)=1.0
        
          MISSPROB=((PROBPER(PROBI,PROBJ)-HUBERT(PROBI,PROBJ))**2)/
     C                (PROBPER(PROBI,PROBJ))
         PFITDA(PROBI)=PFITDA(PROBI)+MISSPROB
         PFITDA(PROBJ)=PFITDA(PROBJ)+MISSPROB
         TMISSDA=TMISSDA+MISSPROB/(NUMTEACH*(NUMTEACH-1))
          MISSPROB=((PROBPER(PROBI,PROBJ)-RESULTM(PROBI,PROBJ))**2)/
     C                (PROBPER(PROBI,PROBJ))
         PFITDB(PROBI)=PFITDB(PROBI)+MISSPROB
         PFITDB(PROBJ)=PFITDB(PROBJ)+MISSPROB
         TMISSDB=TMISSDB+MISSPROB/(NUMTEACH*(NUMTEACH-1))
         WRITE(560,251) NSUBID(PROBI),TRYDEPT(PROBI),
     C   NSUBID(PROBJ),TRYDEPT(PROBJ),PROBPER(PROBI,PROBJ)
21877    CONTINUE

21876    CONTINUE

C         TMISSDA=TMISSDA/NUMTEACH
         RMAXDEPT=MAXDEPT
         OPEN(99,file='actprobs.out')
         WRITE(99,2102) 
         WRITE(99,2102) 
         WRITE(99,2102) (TITLES(A) , A=1,3)
         WRITE(99,2102) "PERSON PROBS"
         WRITE(99,2104) "CHANCE=" , 1/RMAXDEPT
         WRITE(99,2101) "PERSON I" 
         WRITE(99,2107) "       PERSON J", 
     C   (NSUBID(PORD(ZG)), ZG=1,NUMTEACH)
         WRITE(99,2107) "       GROUP ",
     C    (TRYDEPT(PORD(ZG2)) , ZG2=1,NUMTEACH)

      
        DO 2984 GROUP=1,MAXDEPT

         DO 2985 PERS=1,DEPARTN(GROUP)
     
         WRITE(99,2103) NSUBID(ALLGROUP(GROUP,PERS)), 
     C   GROUP ,
     C   (PROBPER(ALLGROUP(GROUP,PERS),PORD(GR)), 
     C   GR=1,NUMTEACH)
02985     CONTINUE
02984      CONTINUE
          CLOSE(99)
          END IF
C          PRINTO(7)=1


      IF (NUMTEACH .GT. 2) THEN
      CALL PRINTG(MAXDEPT,DEPARTN,NUMTEACH,
     C RCHOICE,TRYDEPT,MEANWT,VARWT,LABEL,TITLES,2,KLIST,
     C 2,SYMMAT,PRINTO,NSUBID,"   AFTER ASCENT      ",IEXPECT,ISTD,
     C ICON,STRUCTEQ,NOWCLOSE,PERGROUP,QUANTYPE,SQUAREIT,
     C FIXR,INFILE,HOWWIDE,BETWMULT,QUICKEND,NSIM,BOUNDVAL,BLABOUND,
     C HIWT,
     C  IGRATIO,NUMDIM,
     C CENTER,DANCHOR,DANCHOR2,MOVE2,ZSYMMAT,
     C STARTINC,BYINC,MAXINC,KEXP,NORMAL,MINVALG,
     C CENTERI,DANCHORI,DANCH2I,MOVE2I,ZSYMMATI,
     C STARTINI,BYINCI,MAXINCI,KEXPI,NORMALI,MINVALI,
     C RINCREM,MEASURE,EXTREME,DRADIUSG,DRADIUSI,
     C BYANGLE,BYSCALE,BYANGLEI,BYSCALEI,MINPICT,
     C PCTCENG1,PCTCENG2,PCTCENI1,PCTCENI2,ROWWT,COLWT,SIMO,
     C 4,DBICOORD,HYPERG)

C     C CENTER,IGRATIO,DANCHOR2,MOVE2,DANCHOR,ZSYMMAT,MAXINC,
C     C BYINC,STARTINC,NUMDIM,RINCREM,MEASURE,EXTREME,HYPERG,DRADIUS,
C     C KEXP,NORMAL,NORMALI,MINVALI,MINVALG)

       END IF
       IF (MAKEMISS .EQ. 1) THEN
       DO 94773 OD=1,NUMTEACH
       IF (TAKEOUT(OD) .EQ. 1) THEN
       DO 94774 PERS=1,NUMTEACH
       NEWMAT(OD,PERS)=INMAT(OD,PERS)
94774   CONTINUE
       END IF
94773    CONTINUE
      CALL ARCHOICE(NUMTEACH,NEWMAT,RCHOICE,MAXCH,NFIXR,
     C FLAGR,MEANWT,VARWT)
      DISTFILE=INFILE // ".xdist"
      CALL KSTRESS(STRESSB,DBICOORD,NORMALI,NUMTEACH,HIWT,
     C MINVALI,KEXPI,STRESS2B,FITI,ISTRESS,STRESS3B,MSTRESS2,
     C MSTRESS3,ABICOORD,DISTFILE,PRINTO,NEWDEPT,NSUBID,ODDSDA)
      END IF

      CALL DIFFDIST(ABICOORD,DBICOORD,NUMTEACH,NUMDIM,ADSTRESS,TAD)
      CALL DIFFDIST(CBICOORD,DBICOORD,NUMTEACH,NUMDIM,CDSTRESS,TBD)
      CALL HUBIZE(NEWDEPT,NUMTEACH,HUBERT,STRUCTEQ)
      DISTFILE=INFILE // ".ydist"
      CALL KSTRESS(STRESSB,DBICOORD,NORMALI,NUMTEACH,HIWT,
     C MINVALI,KEXPI,STRESS2B,FITI,ISTRESS,STRESS3B,ISTRESS2,
     C ISTRESS3,CBICOORD,DISTFILE,PRINTO,NEWDEPT,NSUBID,ODDSDB1)

       CALL KFIT(NUMTEACH,FIT,ODDSDB2)
      PFILE=INFILE(1:6) // ".lifit"
       OPEN(52,file=PFILE)
       IF (PRINTO(27) .EQ. 1) THEN
       CALL MOVEEND(5382)
       END IF
       DO 22144 PID=1,NUMTEACH
       WRITE(52,66251) INFILE,
     C NSIM,4,PID,NSUBID(PID),FIT(PID),ADSTRESS(PID),
     C CDSTRESS(PID) ,RCHOICE(PID),MEANWT(PID),VARWT(PID),
     C PFITDA(PID),PFITDB(PID),FITI(PID),ISTRESS(PID),
     C MSTRESS2(PID),MSTRESS3(PID),
     C ISTRESS2(PID),ISTRESS3(PID),TAKEOUT(PID)
22144   CONTINUE
       CLOSE(52)
      CALL HUBVAR(HUBERT,RESULTM,NUMTEACH,HUBSIM,STDHS,ALLCON,
     C HUBM1,HUBSTD1,TOTCOUNT)

      WRITE(33,191) HUBSIM/2,ALLCON/2,STDHS,NSIM,4,TAD,TBD,TMISSDA,
     C TMISSDB,STRESSB,STRESS2B,STRESS3B,ODDSDB1,ODDSDB2,ODDSDA
      WRITE(19,251) HUBSIM/2,ALLCON/2,STDHS,NSIM,4,TAD,TBD,TMISSDA,
     C TMISSDB,STRESSB,STRESS2B,STRESS3B,ODDSDB1,ODDSDB2,ODDSDA

      PFILE=INFILE(1:6) // ".2plac"
      OPEN(52,file=PFILE)

      DO 81667 PID=1,NUMTEACH
       WRITE(52,251) PID,NSUBID(PID),TRYDEPT(PID),NSIM,4
81667 CONTINUE

       ELSE
       WRITE(33,2109) "END OUTPUT: ONLY ONE SOLUTION REQUESTED"
       END IF
04545   CONTINUE

      CLOSE(52)
      IF (PRINTO(24) .EQ. 1) THEN
      HIQ=0
      QI=0
      DO 6763 Q=1,MAXDEPT
      IF (DEPARTN(Q) .GT. 1) THEN
      QI=QI+1
      IF (DEPARTN(Q) .GT. HIQ) THEN
      HIQ=DEPARTN(Q)
      END IF
      END IF
06763  CONTINUE

      WRITE(6,4301) "***********************************************"
      WRITE(6,6764) QI,HIQ
      WRITE(6,4301) "The sizes of the groups are:"
      WRITE(6,24545) (DEPARTN(J) , J=1,MAXDEPT)
      WRITE(6,4301) "Main output in ",OUTFILE
      WRITE(6,4301) "Additional output: "
      IF (PRINTO(6) .EQ. 1) THEN
      WRITE(6,4301) "***********************************************"
      WRITE(6,4301) "Actor by group probabilities are in " 
      WRITE(6,4301) "grpprobs.out.  Data for analysis in grpprobs.dat"
      END IF

      IF (PRINTO(7) .EQ. 1) THEN
      WRITE(6,4301) "***********************************************"
      WRITE(6,4301) "Actor by actor probabilities are in " 
      WRITE(6,4301) "actprobs.out.  Data for analysis in actprobs.dat"
      END IF
      IF (PRINTO(9) .EQ. 1) THEN
      WRITE(6,4301) "***********************************************"
      WRITE(6,4301) "Data for logit analysis in logit.dat"
      END IF
      IF (PRINTO(10) .EQ. 1) THEN
      WRITE(6,4301) "***********************************************"
      WRITE(6,4301) "Final solution in matrix form is in"
      WRITE(6,4301) "clus.outmat"
      END IF

      IF (PRINTO(11) .EQ. 1) THEN
      WRITE(6,4301) "***********************************************"
      WRITE(6,4301) "Output on boundary spanners"
      WRITE(6,4301) "(all actors to all groups) is in ",BFILE
      END IF

      IF (PRINTO(12) .EQ. 1) THEN
      WRITE(6,4301) "***********************************************"
      WRITE(6,4301) "Output on boundary spanners"
      WRITE(6,4301) "(only boundary spanning actors) is in ",BFILE2
      END IF

      IF (PRINTO(13) .EQ. 1) THEN
      WRITE(6,4301) "***********************************************"
      WRITE(6,4301) "Raw data for Multidimensional scaling"
      WRITE(6,4301) "is in group.corr .  Systat commands to read"
      WRITE(6,4301) "the data are in MDSREAD.CMD .  Systat commands"
      WRITE(6,4301) "to Scale the Data are in MDSSCALE.CMD"
      END IF

      IF (PRINTO(26) .EQ. 1) THEN
      WRITE(6,4301) " -----------------------------------------------"
      WRITE(6,4301) " | Output --Blau's Measures of density         |"
      WRITE(6,4301) " |                                             |"
      IF (PRINTO(11) .EQ. 1) THEN
      WRITE(6,4301) " | ******************************************* |"
      WRITE(6,4301) " | Output on boundary spanners                 |"
      WRITE(6,4301) " | (all actors to all groups) is in ",BFILEB
      END IF

      IF (PRINTO(12) .EQ. 1) THEN
      WRITE(6,4301) " | ******************************************* |"
      WRITE(6,4301) " | Output on boundary spanners                 |"
      WRITE(6,4301) " | (only boundary spanning actors) is in ",
     C                  BFILE2B
      END IF

      IF (PRINTO(13) .EQ. 1) THEN
      WRITE(6,4301) " | ******************************************* |"
      WRITE(6,4301) " | Raw data for Multidimensional scaling       |"
      WRITE(6,4301) " | based on Blau's Measure of Density          |"
      WRITE(6,4301) " | is in group.bcorr . Systat commands to read |"
      WRITE(6,4301) " | the data are in MDSBREAD.CMD .  Systat      |"
      WRITE(6,4301) " | commands to Scale the Data are in           |"
      WRITE(6,4301) " | MDSSCALE.CMD                                |"

      WRITE(6,4301) " | ******************************************* |"
      WRITE(6,4301) " | Raw data for Multidimensional scaling       |"
      WRITE(6,4301) " | based on Pct in group connections           |"
      WRITE(6,4301) " | is in group.ccorr . Systat commands to read |"
      WRITE(6,4301) " | the data are in MDSCREAD.CMD .  Systat      |"
      WRITE(6,4301) " | commands to Scale the Data are in           |"
      WRITE(6,4301) " | MDSSCALE.CMD                                |"
      END IF
      WRITE(6,4301) " |                                             |"
      WRITE(6,4301) " -----------------------------------------------"
      WRITE(6,4301)
      END IF

      IF (PRINTO(14) .EQ. 1) THEN
      WRITE(6,4301) "***********************************************"
      WRITE(6,4301) "each actor's internal id, external id and"
      WRITE(6,4301) "group placement  is in ", PFILE
      WRITE(6,4301) "data can be read with place.sas ."
      END IF

      IF (NEVAL .GT. 0) THEN
      WRITE(6,4301) "***********************************************"
      WRITE(6,4301) "Data on evaluation of final solution in"
      WRITE(6,4301) "eval.dat .  Data can be read with eval.sas"
      END IF

      END IF
      WRITE(33,4301)
      WRITE(33,4301)"             OUTPUT        "
      WRITE(33,4301)

      WRITE(33,4301) "***********************************************"
      WRITE(33,4301) "This file contains the main output."
      WRITE(33,4301) "It is called: ",OUTFILE
      WRITE(33,4301) "Additional output: "
      IF (PRINTO(6) .EQ. 1) THEN
      WRITE(33,4301) "***********************************************"
      WRITE(33,4301) "Actor by group probabilities are in " 
      WRITE(33,4301) "grpprobs.out.  Data for analysis in grpprobs.dat"
      END IF

      IF (PRINTO(7) .EQ. 1) THEN
      WRITE(33,4301) "***********************************************"
      WRITE(33,4301) "Actor by actor probabilities are in " 
      WRITE(33,4301) "actprobs.out.  Data for analysis in actprobs.dat"
      END IF
      IF (PRINTO(9) .EQ. 1) THEN
      WRITE(33,4301) "***********************************************"
      WRITE(33,4301) "Data for logit analysis in logit.dat"
      END IF
      IF (PRINTO(10) .EQ. 1) THEN
      WRITE(33,4301) "***********************************************"
      WRITE(33,4301) "Final solution in matrix form is in"
      WRITE(33,4301) "clus.outmat"
      END IF
      IF (PRINTO(11) .EQ. 1) THEN
      WRITE(33,4301) "***********************************************"
      WRITE(33,4301) "Output on boundary spanners"
      WRITE(33,4301) "(all actors to all groups) is in ",BFILE
      END IF

      IF (PRINTO(12) .EQ. 1) THEN
      WRITE(33,4301) "***********************************************"
      WRITE(33,4301) "Output on boundary spanners"
      WRITE(33,4301) "(only boundary spanning actors) is in ",BFILE2
      END IF

      IF (PRINTO(13) .EQ. 1) THEN
      WRITE(33,4301) "***********************************************"
      WRITE(33,4301) "Raw data for Multidimensional scaling"
      WRITE(33,4301) "is in group.corr .  Systat commands to read"
      WRITE(33,4301) "the data are in MDSREAD.CMD .  Systat commands"
      WRITE(33,4301) "to Scale the Data are in MDSSCALE.CMD"
      END IF

      IF (PRINTO(26) .EQ. 1) THEN
      WRITE(33,4301) " -----------------------------------------------"
      WRITE(33,4301) " | Output --Blau's Measures of density         |"
      WRITE(33,4301) " |                                             |"
      IF (PRINTO(11) .EQ. 1) THEN
      WRITE(33,4301) " | ******************************************* |"
      WRITE(33,4301) " | Output on boundary spanners                 |"
      WRITE(33,4301) " | (all actors to all groups) is in ",BFILEB
      END IF

      IF (PRINTO(12) .EQ. 1) THEN
      WRITE(33,4301) " | ******************************************* |"
      WRITE(33,4301) " | Output on boundary spanners                 |"
      WRITE(33,4301) " | (only boundary spanning actors) is in ",
     C                  BFILE2B
      END IF

      IF (PRINTO(13) .EQ. 1) THEN
      WRITE(33,4301) " | ******************************************* |"
      WRITE(33,4301) " | Raw data for Multidimensional scaling       |"
      WRITE(33,4301) " | based on Blau's Measure of Density          |"
      WRITE(33,4301) " | is in group.bcorr . Systat commands to read |"
      WRITE(33,4301) " | the data are in MDSBREAD.CMD .  Systat      |"
      WRITE(33,4301) " | commands to Scale the Data are in           |"
      WRITE(33,4301) " | MDSSCALE.CMD                                |"
      WRITE(33,4301) " | ******************************************* |"
      WRITE(33,4301) " | Raw data for Multidimensional scaling       |"
      WRITE(33,4301) " | based on Pct in group connections           |"
      WRITE(33,4301) " | is in group.ccorr . Systat commands to read |"
      WRITE(33,4301) " | the data are in MDSCREAD.CMD .  Systat      |"
      WRITE(33,4301) " | commands to Scale the Data are in           |"
      WRITE(33,4301) " | MDSSCALE.CMD                                |"
      END IF
      WRITE(33,4301) " |                                             |"
      WRITE(33,4301) " -----------------------------------------------"
      WRITE(33,4301)
      END IF

      IF (PRINTO(14) .EQ. 1) THEN
      WRITE(33,4301) "***********************************************"
      WRITE(33,4301) "each actor's internal id, external id and"
      WRITE(33,4301) "group placement  is in ", PFILE
      WRITE(33,4301) "data can be read with place.sas ."
      END IF
       DO 9334 I=15,19
      IF (PRINTO(I) .EQ. 1) THEN
      WRITE(33,4301) "***********************************************"
      WRITE(33,4301) APRINT(I)
      END IF
09334  CONTINUE
      IF (NEVAL .GT. 0) THEN
      WRITE(33,4301) "***********************************************"
      WRITE(33,4301) "Data on evaluation of final solution in"
      WRITE(33,4301) "eval.dat .  Data can be read with eval.sas"
      END IF

      IF (PRINTO(22) .EQ. 1) THEN
      WRITE(33,3339)
      DO 3320 PO=1,9
      WRITE(33,3339) FANCY(PO)
03320  CONTINUE

      WRITE(33,3339) 
      DO 3341 PO=1,NPO
      WRITE(33,3340)  PO,PRINTO(PO),APRINT(PO)
03341  CONTINUE
      WRITE(33,3339) 
      WRITE(33,3339)
      END IF
C      PRINTO (22)
      WRITE(33,3114)



C      DO 283 I=1,NUMTEACH
C     WRITE(586,101) NBESTDEP(I) , (NEWMAT(I,J) , J=1,NUMTEACH)
C  283  CONTINUE

C
C
C       VARIOUS AND SUNDRY FORMATS
C      

06764  FORMAT("KliqueFinder indentified ",I4, " groups.  The largest",
     C " group contains ",I4," members.",/)
07761   FORMAT(A12)
07760   FORMAT(A6,I2.2,A4)
  191 FORMAT(/,"SIMILARITY BETWEEN THE START AND END GROUPS:   ACTUAL 
     C  POSS  STANDARDIZED",/,43X,I7,3X,I7,3X,F10.5)
07654  FORMAT(A16,F20.1$)
07652  FORMAT(A16$)
76543  FORMAT(F20.1$)
04301 FORMAT(20(A))
09101   FORMAT(251(I4.4,1X))
05101   FORMAT(3(I10))
24545   FORMAT(20(I5))
24845   FORMAT(20(F5.0))
24546   FORMAT(12F5.0,I5,4F5.0,3I5,2F5.0,3I5,6F5.0,2I5,16F5.0)
04334   FORMAT("ACTOR ",I4," HAS CHOSEN ITSELF WITH VALUE",I4,
     C ", ZEROING OUT THIS DIAGONAL ELEMENT OF THE MATRIX")
 3338   FORMAT(210("*"))
 3339   FORMAT("*",10X,A200,2X,"*")
 3340   FORMAT("*",2X,I2,2X,I2,2X,A200,2X,"*")


 3331  FORMAT(//////////,140(10X,A200,/))
 3335  FORMAT(A200,/,A80)
34348  FORMAT(/////////////////////)
 3337   FORMAT(10(A100,/))
 3114  FORMAT("---------------------------------------------------------
     C----------------------------")
 2103 FORMAT(I8,I8,251(F5.2,1X))  
 2107 FORMAT(A16,251(I5,1X))  
 2105 FORMAT(A16,251(I5,1X))  
 2104 FORMAT(A16,251(F5.2,1X))  
 2102 FORMAT(10(A30)) 
 2101 FORMAT(251(A8)) 
 2109 FORMAT(A)
21091 FORMAT(I1,",",I3,",",I3,",N,NM")

   78 FORMAT(//,"STARTING WITH ",A20,/,"NUMBER OF TRIADS= ",F5.0,//)  
   77 FORMAT(3(A20,1X))
    7 FORMAT("RESULTING MATRIX OF WITHIN GROUP CONNECTIONS",/,
     C8X,A20," ORDER")   
    9 FORMAT(/,"THE CURRENT COMPACTNESS IS",F10.5)
C  191 FORMAT(/,"SIMILARITY BETWEEN THE START AND END GROUPS:   ACTUAL 
C     C  POSS  STANDARDIZED",/,42X,I9,I10,F15.5)
  301 FORMAT(I2,20(F10.5))
C
  144 FORMAT(2F10.5)
66251 FORMAT(A16,1X,4I15,3F15.5,I15,10F15.5,I15)
  251 FORMAT(20(F10.5))
 1251 FORMAT(6I10,20(F10.5))
 8251 FORMAT(I10,5F10.5,5I10)
  502 FORMAT(A12,1X)
  504 FORMAT(20(F10.5))
  511 FORMAT(251(A9,1X))
07511 FORMAT(251("AC",I3.3,","))
  132 FORMAT("DEPART    MEMBERS")
  133 FORMAT(I4,4X,251(I4,4X))
  134 FORMAT("(",I4,")",4X,251("(",F6.3,")"))
  137 FORMAT(A6,4X,251("(",F6.3,")"))
  136 FORMAT("HAVE MOVED ",I4," FROM ",I4," TO ",I4)
C
  198 FORMAT("TOTHGV1=",F10.4,"TOTHGV2=",F10.4,"TOTHGV3=",F10.4,
     C       "TOTHGV4=",F10.4)
  187 FORMAT(A12,F5.0)
   99 FORMAT(I2,251(F1.0))

  101 FORMAT(I2,251(I1))
 4101 FORMAT(I2)
  102 FORMAT(I2,251(I1))
  109 FORMAT(I2,I3,251(I1))
  103 FORMAT(2I4)
  107 FORMAT(I1,1X,I5,1X,I1,1X,I1,1X,I4,1X,400(F10.5,1X))
  112 FORMAT(I5,1X,I5,1X,I7,1X,I7,3X,F8.7,4(1X,F10.5))
  110 FORMAT("DEPART=",F5.0," DEPT. TOT=",F10.2," CUM TOT =",F10.2,
     C       " ITER=",I5)
  111 FORMAT(I10,1X,I10,1X,F10.5)
  113 FORMAT(/15X,A16/"DEPT   SIZE MAXCONN TOTCONN    PCONN     HMEAN  
     C HSTD      ZSCORE     PVALUE")
  114 FORMAT("--------------------------------------------------------
     C-----------------------TOTAL")
  120 FORMAT("G=",F7.2," IG=",F7.2," DEP=",F7.2," G3=",F7.2)
  135 FORMAT("JPERSON=",I3," DEPART=",I3)
  140 FORMAT(A16)
  160 FORMAT(I3,1X,I3,1X,F10.5,1X,F10.5)
  203 FORMAT(I3,1X,251(I3,1X))
01077 FORMAT(A20,F20.5)
      END

C      CALL ASSIGN(NUMTEACH,NEWDEPT,MAXDEPT,ALLGROUP
C     C,DEPARTN,RCHOICE,ALLGROUR,MEANWT,VARWT,ALLGROUM,ALLGROUV)

C        CALL ASSIGN(NUMTEACH,NEWDEPT,MAXDEPT,ALLGROUP
C     C  ,DEPARTN,RCHOICE,ALLGROUR,MEANWT,VARWT,ALLGROUM,ALLGROUV)

C        CALL ASSIGN(NUMTEACH,NEWDEPT,MAXDEPT,ALLGROUP
C     C  ,DEPARTN,RCHOICE,ALLGROUR,MEANWT,VARWT,ALLGROUM,ALLGROUV,
C     C  NEWMAT,I,THISMEM)
C      CALL ASSIGN(NUMTEACH,NEWDEPT,MAXDEPT,
C     C RCHOICE,MEANWT,VARWT,
C     C 1,2)

      SUBROUTINE ASSIGN(SUBJECTS,KAFFIL,MAXGROUP,
     C COUNTGR,INCHOICE,MEANW,VARW,
     C P1,P2)
c      CALL ASSIGN(NUMTEACH,NEWDEPT,MAXDEPT,
c     C DEPARTN,RCHOICE,MEANWT,VARWT,
c     C 1,2)

C
      INTEGER SUBJECTS,KAFFIL(251),MAXGROUP,
     C        COUNTGR(251),INCHOICE(251)
      INTEGER C,IG,ADEPART,TSUBJECT,P1,P2
      REAL MEANW(251),VARW(251)
       INTEGER HUBERT(251,251),RESULTM(251,251),ASSIGNG(251,251),
     C NEWMAT(251,251)
       REAL BLAUC(251,251),DIFF(251,251),ZCOMPMAT(251,251)
       COMMON ZCOMPMAT,ASSIGNG,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF

C           ASSIGNV(ADEPART,COUNTGR(ADEPART))=VARW(IG)
C   
C      ASSIGN PEOPLE TO DEPARTMENTS ACCORDING TO THEIR INPUT DATA
C
C         NUMTEACH = TOTAL NUMBER OF TEACHERS
C         DEPART IS BASED ON THE INPUT DATA
C         ADEPART IS A TEMPOARARY NUMBER FOR THE DEPARTMENT
C         MAXDEPT IF THE MAXIMUM NUMBER OF DEPARTMENTS
C         DEPARTN IS AN ARRAY OF THE DEPARTMENTAL SIZES
C         ALLGROUP IS AN ARRAY OF DEPARTMENTS BY PEOPLE IN 
C            THE DEPARTMENTS (AHH FOR A PASCAL POINTER)
C
C
C        CALL CHECKVAL(INMAT,SUBJECTS,P1,P2,"BEGIN ASSIGN")

       DO 20 C=1,SUBJECTS
       COUNTGR(C)=0
   20  CONTINUE

       DO 23 IG=1,SUBJECTS
           ADEPART=KAFFIL(IG)
           IF (MAXGROUP .LT. ADEPART) THEN 
             MAXGROUP=ADEPART
           END IF
           IF ((ADEPART .GE. 1) .AND. (ADEPART .LT. 251)) THEN
           COUNTGR(ADEPART)=COUNTGR(ADEPART) + 1
           ASSIGNG(ADEPART,COUNTGR(ADEPART))=IG
           END IF
C           WRITE(17,117) IG,KAFFIL(IG),MAXGROUP,COUNTGR(ADEPART)
   23   CONTINUE
  117 FORMAT("PERSON=",F4.0," DEPT=",F4.0," MAXD=",F4.0," DC=",F4.0)
  777 FORMAT("PERSON=",I3," DEPART=",I3)
C
      RETURN
      END
C     END SUBROUTINE ASSIGN
C

      SUBROUTINE ASSIGN2(SUBJECTS,KAFFIL,MAXGROUP,
     C COUNTGR)

CASSIGNM,ASSIGNV,
C
C                         X          X             X                        
C        CALL ASSIGN2(NUMTEACH[I] ,NEWDEPT[I251] ,MAXDEPT[I] 
C              X
C       ,ALLGROUP[I251,251]
C          X             X              X
C    C  ,DEPARTN[I251],RCHOICE[I251],ALLGROUR[I251,251]
C     ,MEANWT[R251] ,VARWT[R251],
C     C  1,2)

      INTEGER SUBJECTS,KAFFIL(251),MAXGROUP,
     C        COUNTGR(251)
      INTEGER C,IG,ADEPART,TSUBJECT
       INTEGER HUBERT(251,251),RESULTM(251,251),ASSIGNG(251,251),
     C NEWMAT(251,251)
       REAL BLAUC(251,251),DIFF(251,251),ZCOMPMAT(251,251)
       COMMON ZCOMPMAT,ASSIGNG,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF

C      REAL MEANW(251),VARW(251)
C     C   ASSIGNM(251,251),ASSIGNV(251,251)
C           ASSIGNV(ADEPART,COUNTGR(ADEPART))=VARW(IG)
C   
C      ASSIGN PEOPLE TO DEPARTMENTS ACCORDING TO THEIR INPUT DATA
C
C         NUMTEACH = TOTAL NUMBER OF TEACHERS
C         DEPART IS BASED ON THE INPUT DATA
C         ADEPART IS A TEMPOARARY NUMBER FOR THE DEPARTMENT
C         MAXDEPT IF THE MAXIMUM NUMBER OF DEPARTMENTS
C         DEPARTN IS AN ARRAY OF THE DEPARTMENTAL SIZES
C         ALLGROUP IS AN ARRAY OF DEPARTMENTS BY PEOPLE IN 
C            THE DEPARTMENTS (AHH FOR A PASCAL POINTER)
C
C
C        CALL CHECKVAL(INMAT,SUBJECTS,P1,P2,"BEGIN ASSIGN")

       DO 20 C=1,MAXGROUP
       COUNTGR(C)=0
   20  CONTINUE
C

       TSUBJECT=SUBJECTS+1
       DO 23 IG=1,SUBJECTS
C           WRITE(330,777) IG,KAFFIL(IG)
C         IF (KAFFIL(IG) .EQ. 0) THEN
C           KAFFIL(IG)=1
C          END IF
           ADEPART=KAFFIL(IG)
           IF (MAXGROUP .LT. ADEPART) THEN 
             MAXGROUP=ADEPART
           END IF
           IF ((ADEPART .GE. 1) .AND. (ADEPART .LT. 251)) THEN
           COUNTGR(ADEPART)=COUNTGR(ADEPART) + 1
           ASSIGNG(ADEPART,COUNTGR(ADEPART))=IG
C           ASSIGNR(ADEPART,COUNTGR(ADEPART))=INCHOICE(IG)
C           ASSIGNM(ADEPART,COUNTGR(ADEPART))=MEANW(IG)
C           ASSIGNV(ADEPART,COUNTGR(ADEPART))=VARW(IG)
           END IF
C           WRITE(17,117) IG,KAFFIL(IG),MAXGROUP,COUNTGR(ADEPART)
   23   CONTINUE
  117 FORMAT("PERSON=",F4.0," DEPT=",F4.0," MAXD=",F4.0," DC=",F4.0)
  777 FORMAT("PERSON=",I3," DEPART=",I3)
C
      RETURN
      END
C     END SUBROUTINE ASSIGN2
C
C
C     BEGIN FUNCTION TOTWI
C
      SUBROUTINE TOTWI(INGROUP,THISGRP,NUMGROUP,PATTERN,STARTER,
     C                   ICOVAR,IEXPECT,ITOT,ICONNECT)
      REAL NUMTOT,ICOVAR(251),IEXPECT(251)
      REAL CONGROUP(251),PRELATE
      INTEGER MEMBER,EASY,OTHERMEM,INGROUP(251,251),PATTERN(251,251)
      INTEGER STARTER,NUMGROUP,I,J,THISGRP,ICONNECT(251)
      INTEGER TTOTWI
      REAL ITOT
C     INGROUP IS AN ARRAY OF THE ID'S OF THE PEOPLE IN THE GROUPS
C     NUMTOT IS THE TOTAL NUMBER OF TEACHERS
C     NUMGROUP IS THE NUMBER OF PEOPLE IN THE GROUP
C     PATTERN IS THE MATRIX OF RELATION PATTERNS
C     PRELATE IS THE PROPORTION OF PATTERNS WITHIN THE MATRIX
C     
       TTOTWI=STARTER
       DO 200  I =1,NUMGROUP
       CONGROUP(I)=0
       MEMBER=INGROUP(THISGRP,I)
       DO 300 J = 1,NUMGROUP
C         IF (J .NE. I) THEN
         OTHERMEM=INGROUP(THISGRP,J)
         CONGROUP(I)=CONGROUP(I)+PATTERN(MEMBER,OTHERMEM)
C         IF (THISGRP .EQ. 3) THEN 
C         WRITE(16,993) MEMBER,OTHERMEM,PATTERN(MEMBER,OTHERMEM)
C         END IF
C        END IF 
 300   CONTINUE
       ICONNECT(MEMBER)=CONGROUP(I)
       TTOTWI=TTOTWI+CONGROUP(I)
       ICOVAR(MEMBER)=TTOTWI-IEXPECT(MEMBER)
  200  CONTINUE
       ITOT=TTOTWI
  993 FORMAT("MEMBER=",F4.0," OTHER=",F4.0," PATT=",F4.0)
       RETURN
       END
C      END FUNCTION TOTWI
C
C
      FUNCTION ISOGROUP(INGROUP,GROUPI,PATTERN,STARTER,MAXGROUP,
     C                  COUNTGR)
      INTEGER MEMBER,OTHERMEM,INGROUP(251,251),PATTERN(251,251)
      INTEGER STARTER,MAXGROUP,I,J,Z,GROUPI,COUNTGR(251),THISGRP
      INTEGER ISOGROUP,ISOTEMP,MGROUP
C     INGROUP IS AN ARRAY OF THE ID'S OF THE PEOPLE IN THE GROUPS
C     NUMTOT IS THE TOTAL NUMBER OF TEACHERS
C     NUMGROUP IS THE NUMBER OF PEOPLE IN THE GROUP
C     PATTERN IS THE MATRIX OF RELATION PATTERNS
C     PRELATE IS THE PROPORTION OF PATTERNS WITHIN THE MATRIX
C     
       ISOTEMP=STARTER
      DO 200  MGROUP=1,COUNTGR(GROUPI)
      MEMBER=INGROUP(GROUPI,MGROUP)
        DO 250 Z=1,MAXGROUP
          IF (Z .NE. GROUPI) THEN
            DO 300 J = 1,COUNTGR(Z)
              OTHERMEM=INGROUP(Z,J)
              ISOTEMP=ISOTEMP+PATTERN(MEMBER,OTHERMEM)
              IF (THISGRP .EQ. 3) THEN 
C              WRITE(16,993) MEMBER,OTHERMEM,PATTERN(MEMBER,OTHERMEM)
              END IF
  300       CONTINUE
          END IF
  250  CONTINUE
  200  CONTINUE
       ISOGROUP=ISOTEMP
  993 FORMAT("MEMBER=",F4.0," OTHER=",F4.0," PATT=",F4.0)
       RETURN
       END
C      END FUNCTION ISOGROUP
C
C

C     SUBROUTINE FOR RESTORTING ELEMENTS OF GROUP LIST
      SUBROUTINE RESORTO (ELEMENTS,NUMELEM,RSEED)
      INTEGER ELEMENTS(251),NUMELEM,FAKETCH
      INTEGER OELEMENT(251),Z,J,K,I,CONVERT(251),RITER
      DOUBLE PRECISION RSEED
      INTEGER RANDARRY(251),OLDPOS(251),TRUENUM
C
C   NOW THE RANDARRY IS AN ARRAY OF RANDOM NUMBERS OF LENGTH NUMELEM
C
      TRUENUM=1
      DO 3 K=1,NUMELEM
      IF (ELEMENTS(K) .GE. 1) THEN
      OELEMENT(TRUENUM)=ELEMENTS(TRUENUM)
      TRUENUM=TRUENUM+1
       END IF       
    3 CONTINUE
       TRUENUM=TRUENUM-1
      FAKETCH=NUMELEM
      CALL GGPER(RSEED,TRUENUM,RANDARRY)

      DO 4 Z=1,TRUENUM
      ELEMENTS(Z)=OELEMENT(RANDARRY(Z))
C      WRITE(18,219) Z,RANDARRY(Z),OELEMENT(Z),ELEMENTS(Z)
    4 CONTINUE
  219 FORMAT(I3,1X,I5,1X,I5,1X,I3)
      RETURN
      END
C     END SUBROUTINE RESORTO

C      CALL RESORTO3(OLDDEPT,NUMTEACH,QSEED,USEED,UCUT,NEWDEPT)

      SUBROUTINE RESORTO3 (ELEMENTS,NUMELEM,RSEED,USEED,UCUT,
     C  NELEMENT,TMAXDEPT,NEWGRPS)

      INTEGER ELEMENTS(251),NUMELEM,FAKETCH,NELEMENT(251)
      INTEGER OELEMENT(251),Z,J,K,I,CONVERT(251),RITER,LMAXDEPT,
     C LGROUP(251),DR(1),DR2(2),NEWGRPS,TMAXDEPT
      DOUBLE PRECISION RSEED,USEED
      REAL UCUT,COMPVAL(251),LOW,HIGH
      INTEGER RANDARRY(251),OLDPOS(251),TRUENUM,CHANGEIT(251),SUB,
     C NGROUP


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
C     END SUBROUTINE RESORTO3

      SUBROUTINE MEANVAR (NUMSUB,KOBS,COUNTS,KMEAN,KVAR)
      REAL KMEAN(251),KVAR(251),KSUM(251)
      INTEGER I,J,K,NUMSUB,COUNTS(251)
      REAL KOBS(251,251)

      DO 2 I=1,NUMSUB
      KSUM(I)=0
      KVAR(I)=0
      DO 3 J=1,COUNTS(I)
       KSUM(I)=KSUM(I)+KOBS(I,J)
    3 CONTINUE
      IF (KSUM(I) .LE. 0.00000) THEN
      KMEAN(I)=0.0000
      KVAR(I)=0.0000
      ELSE
      KMEAN(I)=KSUM(I)/COUNTS(I)
      DO 4 K=1,COUNTS(I)
      KVAR(I)=KVAR(I)+(KMEAN(I)-KOBS(I,K))**2
    4 CONTINUE
      END IF
    2 CONTINUE
      RETURN
      END
 

      SUBROUTINE MEANVAR2 (NUMSUB,KOBS,COUNTS,KMEAN,KVAR)
      REAL KMEAN(251),KVAR(251),KSUM(251)
      INTEGER I,J,K,NUMSUB,COUNTS(251)
      REAL KOBS(251,251)

      DO 2 I=1,NUMSUB
      KSUM(I)=0
      KVAR(I)=0

      DO 3 J=1,NUMSUB
       IF (KOBS(I,J) .GT. 0) THEN
       COUNTS(I)=COUNTS(I)+1
       KSUM(I)=KSUM(I)+KOBS(I,J)
       END IF
    3 CONTINUE
     
      IF (KSUM(I) .LE. 0.00000) THEN
      KMEAN(I)=0.0000
      KVAR(I)=0.0000
      ELSE
      KMEAN(I)=KSUM(I)/COUNTS(I)
      DO 4 K=1,COUNTS(I)
      KVAR(I)=KVAR(I)+(KMEAN(I)-KOBS(I,K))**2
    4 CONTINUE
      END IF
    2 CONTINUE
      RETURN
      END
 
C      SUBROUTINE REMOVEO     
C CALL REMOVEO(INMAT,NUMTEACH,HITLIST,NUMHLIST,DEPART,NEWMAT,
C     C               NEWDEPT,SUBID,NSUBID)


      SUBROUTINE REMOVEO(OMAT,OLENGTH,BADLIST,LBAD,DEPLIST,
     C NDEPLIST,UTRIAD,OLDID,NEWID,KEEPLIST,PAIRS,PAIRUP,CONVID,
     C TAGALONG,STRUCTEQ)
      INTEGER OMAT(251,251),OLENGTH,BADLIST(251),DEPLIST(251),
     C         LBAD,NDEPLIST(251),DHAVEIT,APPEND,DEGREE,APPENDX,
     C         UTRIAD,ONBAD(251),HAVEWRIT,MADECHNG

       INTEGER HAVEIT,ROW,Z,K,P,P2,Q,KEEPCNT,TAGALONG,
     C         KEEPLIST(251),REMOVE,BL,L,OLDID(251),NEWID(251),PAIRS,
     C         PAIRUP(251,2),CONVID(251)
C
       REAL ZCOMPMAT(251,251)
       INTEGER HUBERT(251,251),RESULTM(251,251),ALLGROUP(251,251),
     C RNEWMAT(251,251),STRUCTEQ,MAKECHOI
       REAL BLAUC(251,251),DIFF(251,251)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,RNEWMAT,
     C BLAUC,DIFF

C       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,RNEWMAT

       OPEN(99,file='oneconn.lst')       

        DO 17 BL=1,OLENGTH
        ONBAD(BL)=0
00017    CONTINUE

        DO 16 BL=1,LBAD
        WRITE(33,237) OLDID(BADLIST(BL)), " on the hitlist."
        ONBAD(BADLIST(BL))=1
   16   CONTINUE

      DO 12 L=1,OLENGTH
       IF ((DEPLIST(L) .EQ. 0) .AND. (UTRIAD .EQ. 0) .AND.
     C  (ONBAD(L) .EQ. 0)) THEN 
        WRITE(33,237) OLDID(L), " is in no a priori group"
         LBAD=LBAD+1
         ONBAD(L)=1
         BADLIST(LBAD)=L
      END IF

   12 CONTINUE

      MADECHNG=1
      HAVEWRIT=0
      PAIRS=0

      DO WHILE (MADECHNG .EQ. 1)
      MADECHNG=0
      DO 25 ROW=1,OLENGTH
C       SUMROW(ROW)=0
C        SUMCOL(ROW)=0
       DEGREE=0
       APPEND=OLDID(ROW)
       APPENDX=ROW
       IF (ONBAD(ROW) .EQ. 0) THEN
       MAKECHOI=0
       DO 26 COL=1,OLENGTH
        IF (ONBAD(COL) .EQ. 0) THEN
        IF (OMAT(ROW,COL) .NE. 0) THEN
        DEGREE=DEGREE+1
        MAKECHOI=1
        APPEND=OLDID(COL)
        APPENDX=COL
        END IF
        IF (OMAT(COL,ROW) .NE. 0) THEN
        DEGREE=DEGREE+1
        APPEND=OLDID(COL)
        APPENDX=COL
        END IF
C
C        SUMROW(ROW)=SUMROW(ROW) + OMAT(ROW,COL) 
C        SUMCOL(ROW) = SUMCOL(ROW) + OMAT(COL,ROW)
      END IF
   26 CONTINUE

      IF (((DEGREE .EQ. 0) .AND. (TAGALONG .GE. 0))
     C  .OR. ((DEGREE .EQ. 1) .AND. (TAGALONG .GT. 0)) 
     C  .OR. ((MAKECHOI .EQ. 0) .AND. 
     C      ((STRUCTEQ .EQ. 1) .OR. (TAGALONG .EQ. 2)))) THEN
       ONBAD(ROW)=1
       MADECHNG=1
       LBAD=LBAD+1
       BADLIST(LBAD)=ROW
       IF ((DEGREE .EQ. 1) .AND. (TAGALONG .GT. 0)) THEN

       HAVEWRIT=1
       WRITE(33,237) OLDID(ROW),
     C " connected to only one other actor in the
     c network.  The actor can be considered in the group of ",APPEND
       WRITE(99,215) OLDID(ROW),APPEND
       PAIRS=PAIRS+1
       PAIRUP(PAIRS,1)=ROW
       PAIRUP(PAIRS,2)=APPENDX
       END IF
       IF (DEGREE .EQ. 0) THEN
       WRITE(33,237) OLDID(ROW),
     C  " connected to no others in the network."
       END IF
       IF ((MAKECHOI .EQ. 0) .AND. (DEGREE .GT. 1)) THEN
       WRITE(33,237) OLDID(ROW),
     C  " initiated no exchanges.  It must be removed because:"
        IF (STRUCTEQ .EQ. 1) THEN
        WRITE(33,291) "This is not allowed under structural equivalence"
        ELSE
        WRITE(33,291) "You specified tagalong=2"
        END IF
       END IF
      END IF
      END IF
   25 CONTINUE

      END DO

       IF (HAVEWRIT .EQ. 1) THEN
       WRITE(33,291) "The ID's of actors with only one connection to
     c the network, and their associate, are listed in 'oneconn.lst'"
       END IF

      KEEPCNT=0
      DO 27 K=1,OLENGTH
       NDEPLIST(K)=0
       IF (ONBAD(K) .EQ. 0) THEN
         KEEPCNT=KEEPCNT+1
         KEEPLIST(KEEPCNT)=K
       END IF
   27 CONTINUE
C      
      OPEN(37,file='keeplist.dat')
      DO 34 ROW=1,KEEPCNT
      NDEPLIST(ROW)=DEPLIST(KEEPLIST(ROW))
      NEWID(ROW)=OLDID(KEEPLIST(ROW))
      CONVID(KEEPLIST(ROW))=ROW
       WRITE(337,215) OLDID(KEEPLIST(ROW)), ROW
       DO 36 COL=1,KEEPCNT
         RNEWMAT(ROW,COL)=OMAT(KEEPLIST(ROW),KEEPLIST(COL))
   36 CONTINUE
C      WRITE(337,215) ROW,KEEPLIST(ROW)
   34 CONTINUE
      OLENGTH=KEEPCNT
      CLOSE(37)
      CLOSE(99)
      RETURN
C
  251 FORMAT(251F1.0)
  101 FORMAT(f4.3/)   
  102 FORMAT("CORR V"I3,251(1X,f7.5))
  103 FORMAT(251(f5.3,1X))
  104 FORMAT(251(f5.0,1X))
  105 FORMAT("CORR V0"I2,251(1X,f7.5))
  215 FORMAT(I5,1X,I3)
  202 FORMAT(251(I2))
  237 FORMAT("Removing actor ",I4," because the actor ",A,I4)
00291 FORMAT(A)

      END
C      END SUBROUTINE REMOVEO
C

C        CALL REMOVEO2(LMAXDEPT,DEPARTN,LGROUP,1)

      SUBROUTINE REMOVEO2(MAXDEPT,DEPARTN,LGROUP,LOWVAL,QUICKEND)
       INTEGER MAXDEPT,DEPARTN(251),I,J,K,LGROUP(251),LOWVAL,QUICKEND

      I=1
      DO 00027 K=1,MAXDEPT
       IF (DEPARTN(K) .GT. LOWVAL)  THEN
        LGROUP(I)=K
        I =I + 1
       END IF
00027    CONTINUE
        MAXDEPT=I-1
      RETURN
  251 FORMAT(251F1.0)
      END
C      END SUBROUTINE REMOVEO2

      SUBROUTINE REMOVEO4(MAXDEPT,DEPARTN,LGROUP,LOWVAL,I,QUICKEND)
       INTEGER MAXDEPT,DEPARTN(251),I,J,K,LGROUP(251),LOWVAL,
     C  LASTLIST(251),LL,LK,QUICKEND
      IF (QUICKEND .EQ. 1) THEN
      DO 99 I=1,MAXDEPT
      LGROUP(I)=I
00099  CONTINUE
      ELSE
      I=1
      LL=1
      TMAXDEPT=MAXDEPT
      DO 00027 K=1,TMAXDEPT
       IF (DEPARTN(K) .GT. LOWVAL)  THEN
        LGROUP(K)=I
        I =I + 1
       END IF
       IF ((DEPARTN(K) .EQ. LOWVAL) .AND. (LOWVAL .GT. 0) ) THEN
        LASTLIST(LL)=K
        LL=LL+1
       END IF
       IF (DEPARTN(K) .EQ. 0) THEN
        LGROUP(K)=MAXDEPT+1
        END IF
00027    CONTINUE
        I=I-1
        DO 00028 LK=1,(LL-1)
         LGROUP(LASTLIST(LK))=I+LK
00028     CONTINUE
        MAXDEPT=I+LL
        LGROUP(MAXDEPT)=MAXDEPT
       END IF
      RETURN
C
  251 FORMAT(251F1.0)
      END
C      END SUBROUTINE REMOVEO4
C



C      CALL HUBVAR(HUBERT,NEWMAT,NUMTEACH,ACTVAL,STDVAL)
        SUBROUTINE HUBVAR(PMAT,QMAT,TMATSIZE,ACTVAL,STDVAL,ONECOUNT,
     C HUBERTM,HUBERTSD,TOTCOUNT)
       INTEGER PMAT(251,251),QMAT(251,251),TMATSIZE,I,J,T,TOTCOUNT
       REAL SUMPJI(251), SUMQJI(251), SUMPIJ(251),
     C    SUMQIJ(251), SUMPIJ2, SUMQIJ2, SUMPJ(251), SUMQJ(251),
     C    SUMPIJJI(251), SUMQIJJI(251), B62(251), B61(251),
     C    B6PART1,B6PART2,B5PART1,B5PART2,B3PART1,B3PART2,D3PART1,
     C    D3PART2,HUBERTM,HUBERTV,HUBERTSD,TOTQIJ,TOTPIJ,MATSIZE,
     C    ACTVAL,STDVAL,ONECOUNT
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
       HUBERTSD=HUBERTV**.5
       STDVAL=(ACTVAL-HUBERTM)/HUBERTSD
C       WRITE(14,150) HUBERTM,TOTPIJ,TOTQIJ,MATSIZE,HUBERTSD,B1,B2,B3,
C     C      B4,B5,B6,B7,CB1,CB23,CB456,CB7,B3PART1,B3PART2
  150 FORMAT("HUBERTS= ",(F40.15))
      RETURN
      END



C         CALL HZOWMANY (DMEMBERS,DEPARTN(J),I,NEWMAT,NUMTEACH,
C     C                  TOTCON)


      SUBROUTINE HOWMANY (MEMBERS,KGRPSIZE,INDIV,PATTERN,TOTSIZE,
     C                  TOTCON,GROUP,WEIGHT,WTB,UWCON,MT,VT)
C                         I       I                I      I       I   
C         CALL HOWMANY (DMEMBERS,DEPARTN(GROUP2),THISPER,NEWMAT,NUMTEACH,
C                           R     I       R     R
C     C                  STOTCON,GROUP2,TWEIGHT,NOB)

      INTEGER MEMBERS(251),KGRPSIZE,INDIV,PATTERN(251,251),TOTSIZE,
     C  GRCHOICE(251),K,THISSIZE,OTHERS,NTEACH,I2,J,ML,GROUP
      REAL GMEANWT(251),GVARWT(251),GROUPVAR,GROUPMN,MT,VT,
     C   TRESULT,TMEAN,TVAR,TOTCON,A,B,WEIGHT,WTB,UWCON
      REAL RESULT,TA,TB,UWTA,UWTB,UWA,UWB
       
       TOTCON=0.0000
        UWCON=0.000
        MT=0.00
      DO 4 I2=1,KGRPSIZE
        TA=PATTERN(INDIV,MEMBERS(I2))
        TB=PATTERN(MEMBERS(I2),INDIV)
           IF (TA .GT. 0) THEN
         MT=MT+TA
         UWTA=1
           ELSE
         UWTA=0
         END IF

         IF (TB .GT. 0) THEN
         UWTB=1
           ELSE
         UWTB=0
         END IF
        UWA=UWTA*WEIGHT
        UWB=UWTB*WTB
        A=TA*WEIGHT
        B=TB*WTB

        TOTCON=TOTCON+A+B
         UWCON=UWCON+UWA+UWB
         IF ((TOTCON .GT. 999) .OR. (TOTCON .LT. -999)) THEN
        WRITE(33,251) INDIV,GROUP,I2,MEMBERS(I2),A,B,TOTCON,WEIGHT,
     C WTB,PATTERN(INDIV,MEMBERS(I2)),PATTERN(MEMBERS(I2),INDIV)
        END IF
    4 CONTINUE
       MT=MT/UWCON
       VT=0
       IF (UWCON .GT. 1) THEN
      DO 44 I2=1,KGRPSIZE
        TA=PATTERN(INDIV,MEMBERS(I2))
        IF (TA .GT. 0) THEN
        VT=VT+(TA-MT)**2
        END IF
00044    CONTINUE
        VT=VT/(UWCON-1.0)
        END IF

      RETURN
  251 FORMAT(20(F10.5))
      END
C         CALL DISTRIB (RCHOICE(THISMEM),MEANWT(THISMEM),
C     C    VARWT(THISMEM),AMEAN,AVAR,1,NUMTEACH)
C         CALL DISTRIB (RCHOICE(I),MEANWT(I),
C     C    VARWT(I),AMEAN,AVAR,TEMPS,NUMTEACH)

      SUBROUTINE DISTRIB (THISR,THISWM,THISWV,MEAN,TVAR,KGRPSIZE,
     C           TOTSIZE,THISPER,AVAR2,HYPERG)
      REAL ATHISR,ANTEACH,ATHISSIZ,AVAR2
      INTEGER THISR,TOTSIZE,KGRPSIZE,NTEACH,THISSIZE,THISPER,
     C HYPERG
      REAL THISWM,THISWV,MEAN,TVAR,OTHERS,MIDPART
      MEAN=0
      TVAR=.000001
      AVAR2=.000001
      ATHISR=THISR
      ATHISSIZ=KGRPSIZE-1
      OTHERS=TOTSIZE-KGRPSIZE
      ANTEACH=TOTSIZE-1

        MEAN=(THISWM*ATHISR*ATHISSIZ)/ANTEACH
         AVAR2=(THISWM**2)*ATHISR*ATHISSIZ*OTHERS/(ANTEACH**2)
         IF (THISWV .GT. 0) THEN
         MIDPART=(ANTEACH*THISWV)/(OTHERS*THISWM**2) 
         ELSE
         MIDPART=0
         END IF
         TVAR=(THISWM*THISWM)*ATHISR*ATHISSIZ*OTHERS*
     C        (1+ MIDPART
     C          -HYPERG*((ATHISR-1)/(ANTEACH-1)))/(ANTEACH*ANTEACH)
      RETURN
  251 FORMAT(15F10.5)
      END

      SUBROUTINE HUBIZE(GROUPS,N,MATRIX,STRUCTEQ)
      INTEGER GROUPS(251),N,MATRIX(251,251),HUB1,HUB2,
     C STRUCTEQ
      DO 25 HUB1=1,N
       DO 27 HUB2=1,N
        IF ((GROUPS(HUB1) .EQ. GROUPS(HUB2)) .AND. (HUB1 .NE.
     C      HUB2) .AND. (GROUPS(HUB1) .NE. 0)) THEN
          MATRIX(HUB1,HUB2)=1
        ELSE
          MATRIX(HUB1,HUB2)=0
        END IF
   27 CONTINUE
      IF (STRUCTEQ .EQ. 1) THEN
       MATRIX(HUB1,HUB1)=1
      END IF
   25 CONTINUE
      RETURN 
      END
      SUBROUTINE PRINTG(MAXDEPT,DEPARTN,NUMTEACH,
     C TCHOICE,NEWDEPT,MEANWT,VARWT,LABEL,TITLES,PRINTT,KEEPLIST,
     C FINAL,FSYMMAT,PRINTO,NSUBID,PHASE,IEXPECT,ISTD,ICON,STRUCTEQ,
     C NOWCLOSE,PERGROUP,QUANTYPE,SQUAREIT,OLDR,INFILE,
     C HOWWIDE,BETWMULT,QUICKEND,NSIM,BOUNDVAL,BLABOUND,HIWT,
     C  IGRATIO,NUMDIM,
     C CENTER,DANCHOR,DANCHOR2,MOVE2,ZSYMMAT,
     C STARTINC,BYINC,MAXINC,KEXP,NORMAL,MINVALG,
     C CENTERI,DANCHORI,DANCH2I,MOVE2I,ZSYMMATI,
     C STARTINI,BYINCI,MAXINCI,KEXPI,NORMALI,MINVALI,
     C RINCREM,MEASURE,EXTREME,DRADIUS,DRADIUSI,
     C BYANGLE,BYSCALE,BYANGLEI,BYSCALEI,MINPICT,
     C PCTCENG1,PCTCENG2,PCTCENI1,PCTCENI2,ROWWT,COLWT,SIMO,PRINTK,
     C BICOORD,HYPERG)

C     C CENTER,IGRATIO,DANCHOR2,MOVE2,DANCHOR,ZSYMMAT,MAXINC,
C     C BYINC,STARTINC,NUMDIM,RINCREM,MEASURE,EXTREME,HYPERG,DRADIUS,
C     C KEXP,NORMAL,NORMALI,MINVALI,MINVALG)
 
       CHARACTER LABEL(251)*9,TITLES(3)*20,PHASE*20,INFILE*16,
     C LINKFILE*16
       CHARACTER*12 UCFILE,COORFILE,DISTFILE
       CHARACTER*8 CTACKON,VLABEL
       INTEGER*2 ITACKON,SJ
       EQUIVALENCE (ITACKON,CTACKON(7:8))
       LOGICAL UPPER
      REAL MEANWT(251),VARWT(251),IEXPECT(251),ISTD(251),ICON(251),
     C STOTCON,AMEAN,AVAR,MDIAG,VDIAG,CDIAG,HIWT,WARP,TWARP,GWARP(251),
     C FCOMPACT,HUBSIM,STDHS,OUTOF,ZABS,PCON,TWARPA,TWARPB,TWARPC,TWARPD,
     C CHNGSIM,CHNGSTD,NOWCLOSE(251),VP,FIXR,OLDR,NOB,TWEIGHT,
     C GRANGE1(251),ODDSR,XTNUM,XTMAX,CPL,CPLRT,
     C FCOMPAC8,FCOMPAC9,BLABOUND,BOUNDVAL,DRADIUS,DRADIUSI,
     C GRANGE2(251),
     C PVAL,PVAL2,PVAL3,NUMG,ISTRESS2(251),ISTRESS3(251),
     C BETWMULT,ALNORM,PREDACC,PREDACC2,BCHNGSIM,BCHNGCON,
     C BCHNGSTD,PREDL,TOTCENT,ROWWT,COLWT,BCENT1(251),
     C BCENT2(251),XR1,LAMNDA(251),TOTLAMND,RCHI,
     C NFIXR,NO,TAI,TAO,TBI,TBO,GLAMNDA,LGLAMNDA,
     C RZERO,KEXP,XLAMNDA,GARBAGE,RANGEEIG(2),TLAMNDA,
     C PCTCENG1,PCTCENG2,PCTCENI1,PCTCENI2,
     C SAMPMEAN,SAMPSD,CSMEAN,CSSD,ZVAL,CZVAL,CPVAL,SPVAL,
     C SAMPMEA2,SAMPSD2,CSMEAN2,CSSD2,ZVAL2,CZVAL2,CPVAL2,
     C SPVAL2,ODDSA,ODDSB,ODDSC,ODDSD,
     C TTAIE,TTAOE,TTBIE,TTBOE,TTAIO,TTAOO,TTBIO,TTBOO

      
      INTEGER
     C CENTERI,DANCHORI,DANCH2I,MOVE2I,ZSYMMATI,SIMO,PRINTK,TFIXR,
     C SORTFLAG,ROOT,SIXTEEN,
     C STARTINI,BYINCI,MAXINCI,TF1,BYANGLE,BYSCALE,BYANGLEI,BYSCALEI,
     C TG1,T3,LB,BH,AOBSCONI(251),AOBSCONO(251),
     C BOBSCONI(251),BOBSCONO(251),WRITSMAC,STATUS,SYMLNK

      REAL KEXPI,RACTG,RPRINTK,RNSIM,ACMEANI(251),ACMEANO(251),
     C BCMEANI(251),BCMEANO(251)

      INTEGER MAXDEPT,DEPARTN(251),NSIM,COMPARE,KPLACE(251),
     C NUMTEACH,RCHOICE(251),NEWDEPT(251),GORDER(251),
     C GROUP1,GROUP2,DMEMBERS(251),THISPER,KTEMPS,I,J,STRUCTEQ,
     C KEEPLIST(251),LMAXDEPT,LGROUP(251),KORDER(251),
     C K,LENGTH,NO2,G2,G3,NEWORDER(251),PRINTT,FINAL,FSYMMAT,
     C PRINTO(100),SAMEG,I2,OBSCONO(251),OBSCONI(251),
     C TCHOICE(251),NSUBID(251),SAFFIL(251),SMAT(251,251),GENID(251),
     C INUM,MAXDEPTN,MAXCH,NDENOM,HOWWIDE,GSMAT(251,251),GAFFIL(251),
     C IER,DDF,IVDF,G1,BOZO,PERGROUP,QUANTYPE,SQUAREIT,ACTG,U,
     C FLAGR,SLOTTD(251),TRUEDEPT(251),MEDDEPT,TD,AG1,AG2,AG3,
     C NOWWIDE,XTCOUNT,TOTCOUNT,MADECHOI,QUICKEND,OLDORD(251),
     C ANCHOR(251),TANCHOR(251),NUMDIM,ZSYMMAT,MAXINC,BYINC,
     C WITHID,DANCHOR,KENO,REVERSE,MAXK,CENTER,IANCHOR(251),
     C DANCHOR2,MOVE2,RINCREM,MEASURE,EXTREME,STARTINC,IKPLACE(251),
     C HYPERG,NORMAL,N3,N2,NORMALI,STRTTYPE,K2,SAMEG1,K1

       REAL ALLCON,CHNGCON,PC,FCOMPAC2,AVECENT(251),
     C GCOORD(251,2),RINUM,
     C ICOORD(251,2),TEMPMAT(50,50),TICOORD(251,2),
     C IANGLE(251),TIANGLE(251),IRADIUS(251),
     C TIRADIUS(251),GANGLE(251),GRADIUS(251),TMEANVAL,
     C IMEANVAL,GMEANVAL,NRATIO,ZICOORD(251,2),FITG(251),
     C ZIRADIUS(251),MINVALI,MINVALG,MINPICT,FITI(251),
     C BICOORD(251,2),RMED,RTOTCENT,RMADECH,TFITI(251),
     C XBIE,XAIE,XBOE,XAOE,
     C TBIE,TAIE,TBOE,TAOE

       REAL KSIGN,KCHISQR,ACHISQRI(251),ACHISQRO(251),
     C BCHISQRI(251),BCHISQRO(251),BCENTRAL(251),BCENTRA2(251),
     C TOTCHII,TOTCHIO,LCHISQR(251),GCENTRAL(251),HUBM1,HUBM2,
     C HUBSTD1,HUBSTD2,TSS,WSS,VSS,VDF,VMS,VF,KCOMPACT,
     C GROUPMAT(251,251),IHIER(251),GHIER(251),ISTRESS(251),
     C ACOMPAC3,ACOMPAC4,BCOMPAC3,BCOMPAC4,THIER(251),
     C FCOMPAC5,TOTPACT,TCOV,CCOV,ONDIAG,EVALFUN,FCOMPAC6,FCOMPAC7,
     C TCOV2,TOTPACT2,ONDIAG2,ONDIAG3,TOTPACT3,ADDPACT,ONDIAG4,
     C TOTPACT4,APEARSON,BPEARSON,AEQUPEAR,BEQUPEAR,ALRT,BLRT,
     C XPC,MEDIATE,GMAXVAL,IMAXVAL,IGRATIO,BOUND,ANLRT,BNLRT,
     C QA,QB,QC,PLRT,PL,TMAX,TNUM,LE,
     C PDELTA,PBETA,PGAMMA,PALPHA

       DOUBLE PRECISION STRESSB,STRESSA,STRESSC,STRESSD,
     C STRESS2B,STRESS2A,STRESS2C,STRESS2D,
     C STRESS3B,STRESS3A,STRESS3C,STRESS3D


      REAL COVMAT(50,50),
     C COVMAT2(50,50),
     C COVMAT3(50,50),
     C COVMAT4(50,50)

       INTEGER HUBERT(251,251),RESULTM(251,251),ALLGROUP(251,251),
     C NEWMAT(251,251),REVMAT(251,251)

       REAL BLAUC(251,251),DIFF(251,251),ZCOMPMAT(251,251)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF

C       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT



       IF (MAXDEPT .LT. 51) THEN
       DO 27 I=1,MAXDEPT
        LGROUP(I)=I
        KPLACE(I)=I
00027    CONTINUE

      TF1=251
       RZERO=0.000000000
      WRITE(33,512)
      WRITE(33,512) "Summary: ", PHASE
      WRITE(33,512)
      WRITE(33,513) "TITLE1","Title2","Title3"
      WRITE(33,513) (TITLES(T3) , T3=1,3)
      WRITE(33,512)

C      CALL REVERSO(NUMTEACH,REVMAT,MEANWT)
C      CALL ARCHOICE(NUMTEACH,REVMAT,RCHOICE,MAXCH,FIXR,FLAGR,
C     C MEANWT,VARWT)
C      CALL MEANVAR(NUMTEACH,PWEIGHTS,RCHOICE,MEANWT,VARWT)

C     QQQQ

      IF (MAXDEPT .GT. 1) THEN
      REVERSE=1
C      SUBROUTINE LOGIT(MAXDEPT,DEPARTN,NEWMAT,NUMTEACH,
C     C RCHOICE,NEWDEPT,MEANWT,VARWT,
C     C FINAL,FSYMMAT,FCOMPACT,KCHISQRI,KCHISQRO,
C     C KCHISQR,MDIAG,VDIAG,GCENTRAL,CDIAG,FCOMPAC2,
C     C FCOMPAC3,FCOMPAC4,ICON,IEXPECT,ISTD,FCOMPAC5,
C     C PERGROUP,QUANTYPE,SQUAREIT,EVALFUN,FCOMPAC6,FCOMPAC7,
C     C FCOMPAC8,FCOMPAC9,FIXR,PEARSON,EQUIPEAR,
C     C LRT,BCENT1,BCENT2,REVERSE,COVMAT,COVMAT2,COVMAT3,COVMAT4,
C     C HYPERG)
      CALL LOGIT(MAXDEPT,DEPARTN,REVMAT,NUMTEACH,
     C RCHOICE,NEWDEPT,MEANWT,VARWT,
     C FINAL,FSYMMAT,FCOMPACT,BCHISQRI,BCHISQRO,
     C KCHISQR,MDIAG,VDIAG,GCENTRAL,CDIAG,FCOMPAC2,
     C BCOMPAC3,BCOMPAC4,ICON,IEXPECT,ISTD,FCOMPAC5,
     C PERGROUP,QUANTYPE,SQUAREIT,EVALFUN,FCOMPAC6,FCOMPAC7,
     C FCOMPAC8,FCOMPAC9,FIXR,BPEARSON,BEQUPEAR,
     C BLRT,BCENT1,BCENT2,REVERSE,COVMAT,COVMAT2,COVMAT3,COVMAT4,
     C HYPERG,MAXDEPT,PRINTO(27),INFILE,SIMO,BNLRT,
     C BCMEANI,BCMEANO,BOBSCONI,BOBSCONO)
C      CALL LOGIT(MAXDEPT,DEPARTN,NEWMAT,NUMTEACH,
C     C RCHOICE,NEWDEPT,MEANWT,VARWT,
C     C FINAL,FSYMMAT,FCOMPACT,ACHISQRI,ACHISQRO,
C     C KCHISQR,MDIAG,VDIAG,GCENTRAL,CDIAG,FCOMPAC2,
C     C ACOMPAC3,ACOMPAC4,ICON,IEXPECT,ISTD,FCOMPAC5,
C     C PERGROUP,QUANTYPE,SQUAREIT,EVALFUN,FCOMPAC6,FCOMPAC7,
C     C FCOMPAC8,FCOMPAC9,FIXR,APEARSON,AEQUPEAR,
C     C ALRT,BCENT1,BCENT2,REVERSE,HYPERG)

      FIXR=OLDR
      CALL ARCHOICE(NUMTEACH,NEWMAT,RCHOICE,MAXCH,FIXR,FLAGR,
     C MEANWT,VARWT)
C      CALL MEANVAR(NUMTEACH,PWEIGHTS,RCHOICE,MEANWT,VARWT)
       INUM=NUMTEACH
       MADECHOI=NUMTEACH
      DO 983 I=1,NUMTEACH
       IF (DEPARTN(NEWDEPT(I)) .EQ. 1 ) THEN
        INUM=INUM-1.
       END IF
       IF (RCHOICE(I) .LE. 0) THEN
       MADECHOI=MADECHOI-1.
       END IF
00983  CONTINUE

      IF (FLAGR .NE. 1) THEN
       NFIXR=FIXR
       FIXR=OLDR
      END IF
      REVERSE=0
      CALL LOGIT(MAXDEPT,DEPARTN,NEWMAT,NUMTEACH,
     C RCHOICE,NEWDEPT,MEANWT,VARWT,
     C FINAL,FSYMMAT,FCOMPACT,ACHISQRI,ACHISQRO,
     C KCHISQR,MDIAG,VDIAG,GCENTRAL,CDIAG,FCOMPAC2,
     C ACOMPAC3,ACOMPAC4,ICON,IEXPECT,ISTD,FCOMPAC5,
     C PERGROUP,QUANTYPE,SQUAREIT,EVALFUN,FCOMPAC6,FCOMPAC7,
     C FCOMPAC8,FCOMPAC9,FIXR,APEARSON,AEQUPEAR,
     C ALRT,BCENT1,BCENT2,REVERSE,COVMAT,COVMAT2,COVMAT3,COVMAT4,
     C HYPERG,MAXDEPT,PRINTO(27),INFILE,SIMO,ANLRT,
     C ACMEANI,ACMEANO,AOBSCONI,AOBSCONO)

      IF (STRUCTEQ .EQ. 1) THEN 

C      SUBROUTINE SLOGIT(MAXDEPT,DEPARTN,ALLGROUP,NEWMAT,NUMTEACH,
C      C RCHOICE,NEWDEPT,
C     C FINAL,FSYMMAT,COVMAT,FCOMPACT,KCHISQRI,KCHISQRO,
C     C KCHISQR,MDIAG,VDIAG,GCENTRAL,CDIAG,TSS,WBSS,GROUPMAT)

      CALL SLOGIT(MAXDEPT,DEPARTN,NUMTEACH,
     C NEWDEPT,
     C COVMAT,FCOMPACT,
     C TSS,WSS,GROUPMAT,
     C ROWWT,COLWT,MAXDEPT)

C      SUBROUTINE SLOGIT(MAXDEPT,DEPARTN,NUMTEACH,
C     C NEWDEPT,COVMAT,FCOMPACT,TSS,WBSS,GROUPMAT,
C     C ROWWT,COLWT)

       END IF
      IF ((PRINTO(36) .GT. 0) .AND. (MAXDEPT .GT. 1)) THEN
      IF (PRINTO(36) .EQ. 1) THEN

      CALL SORTMAT(COVMAT,MAXDEPT,KPLACE,PRINTO(37),GHIER,MAXDEPT)
      END IF
      IF (PRINTO(36) .EQ. 2) THEN
      CALL SORTMAT(COVMAT2,MAXDEPT,KPLACE,PRINTO(37),GHIER,MAXDEPT)
      END IF
      IF (PRINTO(36) .EQ. 3) THEN
      CALL SORTMAT(COVMAT3,MAXDEPT,KPLACE,PRINTO(37),GHIER,MAXDEPT)
      END IF
      IF (PRINTO(36) .EQ. 4) THEN
      CALL SORTMAT(COVMAT4,MAXDEPT,KPLACE,PRINTO(37),GHIER,MAXDEPT)
      END IF
      CALL FIXMAT(COVMAT,MAXDEPT,KPLACE)
      CALL FIXMAT(COVMAT2,MAXDEPT,KPLACE)
      CALL FIXMAT(COVMAT3,MAXDEPT,KPLACE)
      CALL FIXMAT(COVMAT4,MAXDEPT,KPLACE)
      END IF
       END IF

      DO 65678 J=1,MAXDEPT
      GORDER(KPLACE(J))=J
65678 CONTINUE

      TAI=0
      TBO=0
      TBI=0
      TAO=0
      TAIE=0
      TBOE=0
      TBIE=0
      TAOE=0
      SAMEG1=0
      DO 2525 TG1=1,MAXDEPT  
      G1=GORDER(TG1)
      SAMEG1=SAMEG1+(DEPARTN(G1)*(DEPARTN(G1)-1))*MAXCH
      TLAMNDA=AOBSCONI(G1)*BOBSCONO(G1)
      TLAMNDA=TLAMNDA/(BOBSCONI(G1)*AOBSCONO(G1))
      LAMNDA(G1)=LOG(ABS(TLAMNDA))
      TAIE=TAIE+ACMEANI(G1) 
      TAOE=TAOE+ACMEANO(G1) 
      TBIE=TBIE+BCMEANI(G1)  
      TBOE=TBOE+BCMEANO(G1)  
      TAI=TAI+AOBSCONI(G1)
      TBO=TBO+BOBSCONO(G1)
      TBI=TBI+BOBSCONI(G1)
      TAO=TAO+AOBSCONO(G1)
02525  CONTINUE

       K2=0
       DO 6261 TG1=1,NUMTEACH
       K2=K2+RCHOICE(TG1)
06261   CONTINUE

       TNUM=NUMTEACH-50
        XTNUM=TNUM
        IF (NUMTEACH .GT. 80) THEN
        XTNUM=31
        END IF
          
       TMAX=FIXR-12+1
       XTMAX=TMAX
       IF (FIXR .GT. 17) THEN
        XTMAX=7
       END IF  
       
       PL=.790360+(.0051540)*XTNUM+(-.016847)*XTMAX+
     C (-.000027)*XTNUM**2+(.002689)*XTMAX**2+
     C (-.000424)*XTNUM*XTMAX
C       IF (FIXR .GT. 17) THEN
C       PL=PL-(.002689)*TMAX**2
C       END IF
C       IF (NUMTEACH .GT. 80) THEN
C       PL=PL+(.000027)*TNUM**2
C       END IF

C      PL IS THE PREDICTED ODDS RATIO  MAYBE NOT!
C       PL=2*PL
C      NOW PL IS THE PREDICTED THETA1
       ROOT=1

C       K1=NUMTEACH*(NUMTEACH-1)-K2
C       LE=2.718281828**(2*PL)
C       QA=LE - 1
C       QB=-(LE*SAMEG1+LE*K2+K1-SAMEG1)
C       QC=LE*SAMEG1*K2
C       PDELTA=(-QB+SQRT(QB**2-4*QA*QC))/(2*QA)
C       PBETA=K2-PDELTA
C       PGAMMA=SAMEG1-PDELTA
C       PALPHA=K1-PGAMMA
C       IF ((PBETA .LE. 0) .OR. (PALPHA .LE. 0) .OR.
C     C     (PGAMMA .LE. 0)) THEN
C       PDELTA=(-QB-SQRT(QB**2-4*QA*QC))/(2*QA)
C       PBETA=K2-PDELTA
C       PGAMMA=SAMEG1-PDELTA
C       PALPHA=K1-PGAMMA
C       END IF 

C       CALL CONVLAMN(PL,SAMEG1,K2,(NUMTEACH*(NUMTEACH-1)*MAXCH),
C     C PALPHA,PBETA,PGAMMA,PDELTA,ROOT,LE)

C       PLRT=2*(TBO*LOG(TBO/PALPHA)+
C     C TAO*LOG(TAO/PBETA)+
C     C TBI*LOG(TBI/PGAMMA)+
C     C TAI*LOG(TAI/PDELTA))

C       GLAMNDA=TAI*TBO
C       GLAMNDA=GLAMNDA/(TBI*TAO)

       XTOTAL=NUMTEACH*(NUMTEACH-1)
       SAMEG0=XTOTAL-SAMEG1
       XBIE=K1*SAMEG1/XTOTAL
       XAIE=K2*SAMEG1/XTOTAL
       XBOE=K1*SAMEG0/XTOTAL
       XAOE=K2*SAMEG0/XTOTAL
       IF (PRINTO(36) .GT. 0) THEN 
      WRITE(33,4301) "Subgroups have been reordered according to the"
      WRITE(33,4301) "following scheme:"
      WRITE(33,4301)
      WRITE(33,4301) "GROUP"
      WRITE(33,10) "OLD", (I , I=1,MAXDEPT)
      WRITE(33,10) "NEW" ,(KPLACE(I) , I=1,MAXDEPT)
      WRITE(33,4301)
      COORFILE=INFILE(1:6) // ".swtch"
   
      OPEN(58,file=COORFILE)
      DO 894 I=1,MAXDEPT
      WRITE(58,251) I,KPLACE(I)
00894  CONTINUE
      CLOSE(58)

C JJJJJ
      DO 12112 I=1,NUMTEACH
      NEWDEPT(I)=KPLACE(NEWDEPT(I))
12112  CONTINUE
        CALL ASSIGN2(NUMTEACH,NEWDEPT,MAXDEPT,
     C  DEPARTN)

       MAXDEPTN=1
       NO=1
      DO 82 I=1,MAXDEPT
         IF ((DEPARTN(I) .GT. 1) .AND. (DEPARTN(I) .LE. 50)) THEN
      CALL ONEPIECE(I,DEPARTN(I),TEMPMAT,OLDORD,TF1)
      COMPARE=1
      CALL SORTMAT(TEMPMAT,DEPARTN(I),IKPLACE,COMPARE,THIER,TF1)
      DO 6567 J=1,DEPARTN(I)
      KORDER(IKPLACE(J))=OLDORD(J)
      IHIER(OLDORD(J))=THIER(J)
06567  CONTINUE
      END IF
        DO 83 J=1,DEPARTN(I)

         IF ((DEPARTN(I) .GT. 1) .AND. (DEPARTN(I) .LE. 50)) THEN
        ALLGROUP(I,J)=KORDER(J)
      END IF
        NEWORDER(NO)=ALLGROUP(I,J)
        NO=NO+1
C       WRITE(555,251) I,J,NEWORDER(NO)
   83 CONTINUE
       IF (DEPARTN(I) .GT. MAXDEPTN) THEN
        MAXDEPTN=DEPARTN(I)
       END IF
   82 CONTINUE
C     LEFT OFF HERE ZXZXZX
       ELSE
       NO=1
       DO 222 J=1,MAXDEPT
       DO 2221 K=1,DEPARTN(J)
        NEWORDER(NO)=ALLGROUP(J,K)
        NO=NO+1
02221    CONTINUE
00222     CONTINUE
       END IF
      IF (PRINTO(11) .EQ. 1) THEN
      CALL BSPAN(DIFF,NUMTEACH,MAXDEPT,BOUNDVAL,KPLACE,NEWDEPT,
     C TITLES,"COMPACTNESS   ",
     C "OTHER T     ",DEPARTN,NSUBID,INFILE,PRINTO(12),"C",PRINTO(27),
     C NSIM,PRINTK)
      IF (PRINTO(26) .EQ. 1) THEN
      CALL BSPAN(BLAUC,NUMTEACH,MAXDEPT,BLABOUND,KPLACE,NEWDEPT,
     C TITLES,"DENSITY      ",
     C "OTHER T     ",DEPARTN,NSUBID,INFILE,PRINTO(12),"D",PRINTO(27),
     C NSIM,PRINTK)
       END IF

      END IF
C       MAXDEPT .GT. 1


      IF ((PRINTO(19) .EQ. 1) .AND. (FINAL .EQ. 2)) THEN
       OPEN(17,file='ucwn.vec')
      WRITE(17,27511) 3,NUMTEACH,MAXDEPT,MAXDEPTN
      WRITE(17,37511) (DEPARTN(J) , J=1,MAXDEPT)
      WRITE(17,37511) (NEWORDER(J) , J=1,NUMTEACH)
      WRITE(17,4308) "uswn.DAT"
      CLOSE(17)
      END IF

      IF ((PRINTO(18) .EQ. 1) .AND. (FINAL .EQ. 2)) THEN
      CTACKON="ucgrop00"
      DO 8898 SJ=1,MAXDEPT
      OPEN(14,file='holdname')
      REWIND(14)
      WRITE(14,7760) "ucgrop",SJ,".DAT"
      CLOSE(14)
      OPEN(14,file='holdname')
      READ(14,7761) UCFILE
      
C      ITACKON=SJ
C      UCFILE= CTACKON // ".DAT"      
      OPEN(10,file=UCFILE)
      WRITE(10,21092) 4,DEPARTN(SJ),DEPARTN(SJ)
      WRITE(10,7512) (NSUBID(ALLGROUP(SJ,LB)) , LB=1,DEPARTN(SJ))
      DO 7324 G1=1,DEPARTN(SJ)
       WRITE(10,9101) (NEWMAT(ALLGROUP(SJ,G1),ALLGROUP(SJ,G2))
     C  , G2=1,DEPARTN(SJ))
07324   CONTINUE
       CLOSE(10)
08898   CONTINUE
       END IF


      CALL HUBIZE(NEWDEPT,NUMTEACH,RESULTM,STRUCTEQ)

      IF ((PRINTO(4) .EQ. 1) .AND. (FINAL .EQ. 2)) THEN
      OPEN(99,file='place.mat')    
      DO 243 BH = 1,NUMTEACH
      WRITE(99,105) (RESULTM(BH,K) , K=1,NUMTEACH)
  243  CONTINUE
      END IF


       IF (PRINTO(9) .EQ. 1) THEN
      OPEN(58,file='logit.dat')
      DO 1288 I=1,NUMTEACH
       DO 1289 I2=1,NUMTEACH
        SAMEG=0
        IF (NEWDEPT(I) .EQ. NEWDEPT(I2)) THEN
         SAMEG=1
        END IF
        WRITE(58,10251) NSUBID(I),
     C  NSUBID(I2),NEWMAT(I,I2),SAMEG,NEWDEPT(I),NEWDEPT(I2)
01289    CONTINUE
01288     CONTINUE
        CLOSE(58)
        END IF

      LENGTH=NUMTEACH+1
       IF (STRUCTEQ .EQ. 1) THEN
      DO 9277 NO2=1,MAXDEPT
      GAFFIL(NO2)=NO2
       DO 29927 J=1,NUMTEACH
        GSMAT(J,NO2)=INT(10*GROUPMAT(NO2,NEWORDER(J)))
29927   CONTINUE
09277    CONTINUE
       END IF

      DO 92 NO2=1,NUMTEACH
       DO 2992 J=1,NUMTEACH
        GENID(J)=J
        SMAT(NO2,J)=NEWMAT(NEWORDER(NO2),NEWORDER(J))
02992   CONTINUE
        SAFFIL(NO2)=NEWDEPT(NEWORDER(NO2))
        IF (SAFFIL(NO2) .LE. 0) THEN 
         SAFFIL(NO2)=9999
        END IF
00092    CONTINUE


      IF ((PRINTO(16) .EQ. 1) .AND. (FINAL .EQ. 2)) THEN
      OPEN(18,file='ucwnhub.DAT')
      OPEN(54,file='ucwn.DAT')
      WRITE(54,21092) 4,NUMTEACH,NUMTEACH
      WRITE(18,21092) 4,NUMTEACH,NUMTEACH
      WRITE(54,7513) (LB , LB=1,(NUMTEACH-1))
       WRITE(54,7514) NUMTEACH
      WRITE(18,7513) (LB , LB=1,(NUMTEACH-1))
       WRITE(18,7514) NUMTEACH
      DO 8866 I=1,NUMTEACH
      WRITE(18,9101) (RESULTM(I,J) , J=1,NUMTEACH)
      WRITE(54,9101) (NEWMAT(I,J) , J=1,NUMTEACH)
08866 CONTINUE
       CLOSE(18)
        CLOSE(54)
      END IF
      IF ((PRINTO(16) .EQ. 2) .AND. (FINAL .EQ. 2)) THEN
      OPEN(18,file='struchub.DAT')
      OPEN(54,file='struc.DAT')
      WRITE(54,9103) "(",NUMTEACH,"F4.0)"
      WRITE(18,9103)  "(",NUMTEACH,"F4.0)"
      DO 8826 I=1,NUMTEACH
      WRITE(18,9102) (RESULTM(I,J) , J=1,NUMTEACH)
      WRITE(54,9102) (NEWMAT(I,J) , J=1,NUMTEACH)
08826 CONTINUE
       CLOSE(18)
        CLOSE(54)

      END IF


C      SUBROUTINE MATPRINT(EXTRAID,DIVIDO,MAXDEPT,
C     CNUMTEACH,DOIT,AFFI,MATRIX,TITLE,NSUBID,IEXPECT,ISTD,ICON,
C     CNEWORD,STRUCTEQ,NOWCLOSE,NUMCOL,HOWWIDE)

      CALL MATPRINT(NEWORDER,1,MAXDEPT+1,NUMTEACH,PRINTO(2),SAFFIL,
     C SMAT,
     C "CHOICES BY GROUPS                       ",
     C NSUBID,IEXPECT,ISTD,ICON,
     C NEWORDER,STRUCTEQ,NOWCLOSE,NUMTEACH,HOWWIDE,SAFFIL,DEPARTN,
     C PRINTO(42))
     
      IF (STRUCTEQ .EQ. 1) THEN
      NOWWIDE=3

      CALL MATPRINT(NEWORDER,1,MAXDEPT+1,NUMTEACH,PRINTO(20),SAFFIL,
     C GSMAT,
     C "GROUP BLOCKMODELS (VAL=MEAN*10)         ",
     C NSUBID,IEXPECT,ISTD,ICON,
     C NEWORDER,STRUCTEQ,NOWCLOSE,MAXDEPT-1,NOWWIDE,GAFFIL,DEPARTN,
     C PRINTO(42))

       END IF


      CALL MATPRINT(GENID,0,MAXDEPT,NUMTEACH,PRINTO(3),NSUBID,NEWMAT,
     C "CHOICES IN PERSON ORDER                 ",
     C NSUBID,IEXPECT,ISTD,ICON,
     C NEWORDER,STRUCTEQ,NOWCLOSE,NUMTEACH,HOWWIDE,NSUBID,DEPARTN,
     C PRINTO(42))

      CALL MATPRINT(GENID,0,MAXDEPT,NUMTEACH,PRINTO(4),NSUBID,RESULTM,
     C "HUBERT MATRIX                           ",
     C NSUBID,IEXPECT,ISTD,ICON,
     C NEWORDER,STRUCTEQ,NOWCLOSE,NUMTEACH,HOWWIDE,NSUBID,DEPARTN,
     C PRINTO(42))

      CALL HUBVAR(NEWMAT,RESULTM,NUMTEACH,HUBSIM,STDHS,ALLCON,
     C HUBM1,HUBSTD1,TOTCOUNT)
      CALL HUBVAR(HUBERT,RESULTM,NUMTEACH,CHNGSIM,CHNGSTD,CHNGCON,
     C HUBM2,HUBSTD2,XTCOUNT)
      CHNGSIM=CHNGSIM/2
      CHNGCON=CHNGCON/2
      NDENOM=NUMTEACH-1.
      IF (STRUCTEQ .EQ. 1) THEN
       NDENOM=NDENOM+1.
      END IF
      OUTOF=MADECHOI*NDENOM/(FSYMMAT+1.)
C      OUTOF=NUMTEACH*(NUMTEACH-1)
        TOTCHII=0.0000
        TOTCHIO=0.0000
       NUMG=0
       DO 8788 TG1=1,MAXDEPT
       G1=GORDER(TG1)
       IF ((DEPARTN(G1) .GT. 1) .AND. (DEPARTN(G1) .LE. 50)) THEN
C       IF (DEPARTN(G1) .GT. 1) THEN
       TOTCHII=TOTCHII+ACHISQRI(G1)+BCHISQRI(G1)
       TOTCHIO=TOTCHIO+ACHISQRO(G1)+BCHISQRO(G1)
       NUMG=NUMG+1
       END IF
08788   CONTINUE
      XPC=TOTCOUNT/OUTOF
      PC=ALLCON/OUTOF
      WRITE(33,194) "N","N MADE CONN","NON-ISOLATES",
     C"POSS CONNECT"
      WRITE(33,1954) NUMTEACH,MADECHOI,INUM,OUTOF
      WRITE(33,194)
      WRITE(33,194) "UNWEIGHTED"
      WRITE(33,194) "CONNECTIONS","% CONNECT"
      WRITE(33,195) ALLCON,PC
      WRITE(33,194)
      WRITE(33,194) "WEIGHTED"
      WRITE(33,194) "CONNECTIONS","AVG CONNECT"
      XR1=TOTCOUNT
      WRITE(33,195) XR1,XPC

      WRITE(33,194)
      WRITE(33,194) "COMPACTNESS"
      WRITE(33,194) "HUBERT'S"
      WRITE(33,194) "# IN-GROUP CON","EXPECTED","   STD   ",
     C "STANDARDIZED","P-VALUE"

      UPPER=.FALSE.
      PVAL=ALNORM(STDHS,UPPER)
      WRITE(33,195) HUBSIM,HUBM1,HUBSTD1,STDHS,(1.00000-PVAL)
      WRITE(33,194) "/ACTOR","/NON ISOLATE"
      WRITE(33,195) (STDHS/NUMTEACH),(STDHS/INUM)
      WRITE(33,194)
      IF (STRUCTEQ .EQ. 1) THEN

      WRITE(33,194)
      WRITE(33,194) "REGRESSION"
      WRITE(33,194)
      WRITE(33,2194) "SOURCE","SS","DF","MS","F","P-VALUE"
      VLABEL="BETWEEN"
      VSS=TSS-WSS
      VDF=MAXDEPT**2 - 1
      IVDF=VDF
      VMS=VSS/VDF
      DDF=NUMTEACH**2-MAXDEPT**2
      VF=VMS/(WSS/(DDF))
      IF ( (VF .GT. 0) .AND. (IVDF .GT. 0) .AND. (DDF .GT. 0)) THEN
C      CALL CDFF (1,VP,VF,IVDF,DDF,IER,BOUND)
C      CALL MDFD (VF,IVDF,DDF,VP,IER)
      END IF
      WRITE(33,2195) VLABEL,VSS,VDF,VMS,VF,(1.000-VP)

      VLABEL="WITHIN"
      VSS=WSS
      VDF=NUMTEACH**2-MAXDEPT**2 
      VMS=VSS/VDF
      VF=0
      WRITE(33,2195) VLABEL,VSS,VDF,VMS

      VLABEL="TOTAL"
      VSS=TSS
      VDF=NUMTEACH**2
      VMS=VSS/VDF
      VF=0
      WRITE(33,2195) VLABEL,VSS,VDF,VMS
      WRITE(33,194)
      WRITE(33,194) "(R-SQUARE)"
      WRITE(33,195) FCOMPACT
      WRITE(33,194) 
     C"Note for appproximate statistical testing of the fit of the"
      WRITE(33,194) 
     C"groups to the pattern of connections it is"
      WRITE(33,194) 
     C"advisable to use LISREL 7 (by Joreskog and Sorbom, 1989)."
      WRITE(33,194) 
     C"Also, if actors make relatively few connections to other"
      WRITE(33,194) 
     C"actors throughout the network it may be advisable to use"
      WRITE(33,194) 
     C"PRELIS to prepare the essentially dichotimous data for LISREL"

      ELSE
      WRITE(33,194)
      IF (VDIAG .GT. 0) THEN
      WRITE(33,194) "KLIQUE FINDER'S"
      WRITE(33,194) "# IN-GROUP CON","EXPECTED","   STD   ",
     C "STANDARDIZED","P-VALUE"
  
      KCOMPACT= ((CDIAG-MDIAG)/VDIAG**.5)

      UPPER=.FALSE.
      PVAL=ALNORM(KCOMPACT,UPPER)
      WRITE(33,195) CDIAG,MDIAG,(VDIAG**.5),KCOMPACT,(1.00000-PVAL)

      WRITE(33,194) "/ACTOR","/NON ISOLATE"
      WRITE(33,195) (FCOMPACT/NUMTEACH),(FCOMPACT/INUM)
      TFIXR=INT(FIXR)
      CALL SAMPDIST(NUMTEACH,TFIXR,SAMPMEAN,SAMPSD,CSMEAN,CSSD,
     C   KCOMPACT,ZVAL,SPVAL,CZVAL,CPVAL,PREDACC)
      IF (QUANTYPE .EQ. 1) THEN
      WRITE(33,194)
      WRITE(33,4301) "SAMPLING DISTRIBUTION, BASED ON"
      WRITE(33,4301) "ACTUAL NETWORK SIZE"
      IF ((NUMTEACH .LT. 20) .OR. (NUMTEACH .GT. 80))  THEN 
      WRITE(33,4301) "Sampling distributions based on linear" 
      WRITE(33,4301) "relationship between network size and "
      WRITE(33,4301) "compactness.  Quadratic term not used"
      WRITE(33,4301) " because network size was outside of"
      WRITE(33,4301) "range of simulated data sets."
      ELSE
      WRITE(33,4301) "Sampling distributions based on linear and "
      WRITE(33,4301) "quadratic relationship between network size"
      WRITE(33,4301) "and compactness based on simulated data."
      END IF
      WRITE(33,4301)

        IF ((MAXCONN .LT. 3) .OR. (MAXCONN .GT. 12)) THEN 
      WRITE(33,4301) "Sampling distributions based on linear" 
      WRITE(33,4301) "relationship between maximum connections and"
      WRITE(33,4301) "compactness.  Quadratic term not used"
      WRITE(33,4301) " because maximum connections was outside of"
      WRITE(33,4301) "range of simulated data sets."

      ELSE
      WRITE(33,4301) "Sampling distributions based on linear and "
      WRITE(33,4301) "quadratic relationship between maximum"
      WRITE(33,4301) "connections and compactness based on" 
      WRITE(33,4301) "simulated data."
      WRITE(33,4301)     
      END IF
      WRITE(33,4301) "See Frank (1993) 'Identifying Cohesive Subgroups'"
      WRITE(33,4301) "Dissertation, Uiniversity of Chicago"

      WRITE(33,194)
      WRITE(33,194) "PREDICTED"
      WRITE(33,194) "SAMPLE MEAN","SAMPLE SD","Z","P-VALUE"
      WRITE(33,195) SAMPMEAN,SAMPSD,ZVAL,(1.00000-SPVAL)

      WRITE(33,194) "CONSERVATIVE"
      WRITE(33,194) "SAMPLE MEAN","SAMPLE SD","Z","P-VALUE"
      WRITE(33,195) CSMEAN,CSSD,CZVAL,(1.00000-CPVAL)
      WRITE(33,194)
      WRITE(33,4301) "PREDICTED ACCURACY: MEAN PCT TRUE SUBGROUP"
      WRITE(33,4301) "MEMBERS IN OBSERVED SUBGROUP + OR - 30%"
      WRITE(33,195) PREDACC

      CALL SAMPDIST(MADECHOI,TFIXR,SAMPMEA2,SAMPSD2,CSMEAN2,CSSD2,
     C   KCOMPACT,ZVAL2,SPVAL2,CZVAL2,CPVAL2,PREDACC2)
      WRITE(33,194)
      WRITE(33,4301) "SAMPLING DISTRIBUTION, BASED ON"
      WRITE(33,4301) "ACTORS WHICH INITIATED EXCHANGES"
      WRITE(33,194)
      WRITE(33,194) "SAMPLE MEAN","SAMPLE SD","Z","P-VALUE"
      WRITE(33,195) SAMPMEA2,SAMPSD2,ZVAL2,(1.00000-SPVAL2)

      WRITE(33,194) "CONSERVATIVE"
      WRITE(33,194) "SAMPLE MEAN","SAMPLE SD","Z","P-VALUE"
      WRITE(33,195) CSMEAN2,CSSD2,CZVAL2,(1.00000-CPVAL2)
      WRITE(33,194)

      END IF


      IF (QUANTYPE .EQ. 5) THEN
       TAO=TOTCOUNT-TAI
       TBI=SAMEG1-TAI
       TBO=(NUMTEACH*(NUMTEACH-1)*MAXCH)-TOTCOUNT-TBI
       CALL CONVLAMN(PL,SAMEG1,TOTCOUNT,
     C (NUMTEACH*(NUMTEACH-1)*MAXCH),
     C PALPHA,PBETA,PGAMMA,PDELTA,ROOT,LE)

       PLRT=2*(TBO*LOG(TBO/PALPHA)+
     C TAO*LOG(TAO/PBETA)+
     C TBI*LOG(TBI/PGAMMA)+
     C TAI*LOG(TAI/PDELTA))

       GLAMNDA=TAI*TBO
       GLAMNDA=GLAMNDA/(TBI*TAO)

      WRITE(33,194) 
      WRITE(33,4308) "VALUE OF THETA1 FOR THESE SUBGROUPS IS"
      WRITE(33,195) (LOG(GLAMNDA))/2.00
      WRITE(33,194) 
       WRITE(33,4308) "THETA1 IS TAKEN FROM THE FOLLOWING MODEL:"
       WRITE(33,4308) "LOG P(Xii'=xii')=theta0+theta1(samegroup)"
       WRITE(33,4308)
       WRITE(33,4308) "THETA1 ALSO CAN BE INTERPRETED AS HALF THE "
       WRITE(33,4308) "LOG-ODDS OF THE FOLLOWING TABLE:"
       WRITE(33,4308) 
       WRITE(33,4308) "                  CONNECTION"
       WRITE(33,4308) "                 NO        YES"
       WRITE(33,4308) "              ___________________"
       WRITE(33,4308) "              |        |        |"
       WRITE(33,4308) "           NO | ALPHA  | BETA   |"
       WRITE(33,4308) "IN SAME       |        |        |"
       WRITE(33,4308) "GROUP         |--------|--------|"
       WRITE(33,4308) "              |        |        |"
       WRITE(33,4308) "          YES | GAMMA  | DELTA  |"
       WRITE(33,4308) "              |        |        |"
       WRITE(33,4308) "              -------------------"
       WRITE(33,4308)

      WRITE(33,4308) "PREDICTED THETA (1 base) BASED ON SIMULATIONS."
      WRITE(33,4308) "VALUE BASED ON UNWEIGHTED DATA."
      WRITE(33,195) PL
      

       IF ((FIXR .GT. 17) .OR. (NUMTEACH .GT. 80))  THEN
       IF (FIXR .GT. 17) THEN 
         WRITE(33,4308)
       WRITE(33,4308) "MAXIMUM NUMBER OF RELATIONS FOR A GIVEN ACTOR,"
         WRITE(33,195) FIXR
        WRITE(33,4308) "IS GREATER THAN THE VALUE OF 18"
        WRITE(33,4308) "IN THE SIMULATIONS."
       END IF

       IF (NUMTEACH .GT. 80) THEN 
         WRITE(33,4308)
       WRITE(33,4308) "SIZE OF NETWORK,"
       WRITE(33,195) NUMTEACH
       WRITE(33,4308)  "IS GREATER THAN THE MAX OF 80"
        WRITE(33,4308) "IN THE SIMULATIONS."
       END IF
         WRITE(33,4308)
       WRITE(33,4308) "WILL USE PREDICTION FOR THETA1base BASED ON"
C       WRITE(33,4308) "ON LINEAR TERMS AND QUADRATIC TERMS WITH "
       WRITE(33,4308) "MAXIMUM VALUES FROM SIMULATIONS."
       WRITE(33,4308) "INTERPRET WITH CAUTION!"
         WRITE(33,4308)
       WRITE(33,4308) "CONSIDER GENERATING A UNIQUE SAMPLING"
       WRITE(33,4308) "DISTRIBUTION BY TYPING:"
       WRITE(33,4308) "setup sampdist2"
         WRITE(33,4308) "THE FIRST NUMBER IN THE FILE"
         WRITE(33,4308) "simulate.par IS THE NUMBER OF SIMULATIONS"
         WRITE(33,4308) "TO RUN, MODIFY THIS IF YOU WANT."
         WRITE(33,4308) "THEN TYPE:"
         WRITE(33,4308) "nice kliqfind yourfile s"
         WRITE(33,4308) "AFTER IT IS DONE RUNNING, TYPE:"     
         WRITE(33,4308) "sas sampdist"
         WRITE(33,4308) "AND LOOK AT THE HISTOGRAM IN sampdist.lst"
         WRITE(33,4308) "NOTE THAT YOU MIGHT WANT TO DO THIS IN"
         WRITE(33,4308) "A SEPARATE DIRECTORY."
       END IF

       WRITE(33,4308)
        TGLAMNDA=LOG(GLAMNDA)/2.00
      WRITE(33,4308) "ESTIMATE OF THETA (1 subgroup processes)"
      WRITE(33,195) (TGLAMNDA-PL)
       WRITE(33,4308)
      WRITE(33,4308) "THE TOTAL THETA1 IS:"
      WRITE(33,195) TGLAMNDA
       WRITE(33,4308)

C      WRITE(33,4308) "CORRESPONDING ODDS RATIO"
C      WRITE(33,195) LE
       WRITE(33,4308) "OBSERVED VALUES FOR CELLS A,B,C, AND D"
       WRITE(33,25144) TBO,TAO,TBI,TAI
         ODDSR=TAI*TBO/(TAO*TBI)
       WRITE(33,4308)
       WRITE(33,4308) "ODDS RATIO, LOG ODDS, (LOG ODDS/2)"
       WRITE(33,25144) ODDSR,LOG(ODDSR),(LOG(ODDSR))/2
       WRITE(33,4308)
       WRITE(33,4308) "PREDICTED VALUES FOR CELLS A,B,C, AND D"
       WRITE(33,25144) PALPHA,PBETA,PGAMMA,PDELTA
         ODDSR=PDELTA*PALPHA/(PBETA*PGAMMA)
       WRITE(33,4308)
       WRITE(33,4308) "ODDS RATIO, LOG ODDS, (LOG ODDS/2)"
       WRITE(33,25144) ODDSR,LOG(ODDSR),(LOG(ODDSR))/2
       WRITE(33,4308)

       WRITE(33,4308)
      WRITE(33,4308) "LRT BASED ON PREDICTED THETA (1 base)"
      WRITE(33,195) PLRT
       WRITE(33,4308)
      WRITE(33,4308) "COMPARE TO CHI-SQUARE ON 1 DF"


      TG1=1
C      RCHI=PLRT-(ALRT+BLRT)
      RCHI=PLRT
      PVAL=.5
C      IF (RCHI .LE. 2.705) THEN
C      WRITE(33,4301) "P-VALUE > .1"
C      END IF
      WRITE(33,4301) "P-VALUE (LESS THAN OR EQUAL TO):"
      IF (RCHI .GT. 2.705) THEN
       PVAL=.1
      END IF

      IF (RCHI .GT. 3.841) THEN
       PVAL=.05
      END IF

      IF (RCHI .GT. 5.024) THEN
       PVAL=.025
      END IF

      IF (RCHI .GT. 6.635) THEN
       PVAL=.01
      END IF

      IF (RCHI .GT. 7.879) THEN
       PVAL=.005
      END IF

      IF (RCHI .GT. 10.828) THEN
       PVAL=.001
      END IF
      WRITE(33,195) (PVAL)


       WRITE(33,4308)

       WRITE(33,4308) "CONSERVATIVE THETA (1 base)"
       CPL=PL+.032
       WRITE(33,195) CPL
      WRITE(33,4308)
      WRITE(33,4308) "LRT BASED ON CONSERVATIVE PREDICTED THETA (1 base)"
       CALL CONVLAMN(CPL,SAMEG1,TOTCOUNT,(NUMTEACH*(NUMTEACH-1)*MAXCH),
     C PALPHA,PBETA,PGAMMA,PDELTA,ROOT,LE)
       CPLRT=2*(TBO*LOG(TBO/PALPHA)+
     C TAO*LOG(TAO/PBETA)+
     C TBI*LOG(TBI/PGAMMA)+
     C TAI*LOG(TAI/PDELTA))

      WRITE(33,195) CPLRT
       WRITE(33,4308)
      WRITE(33,4308) "COMPARE TO CHI-SQUARE ON 1 DF"
        WRITE(33,4308)
        WRITE(33,4308) "P-VALUE IS:"


      TG1=1
C      RCHI=PLRT-(ALRT+BLRT)
      RCHI=CPLRT
      PVAL=.5
C      IF (RCHI .LE. 2.705) THEN
C      WRITE(33,4301) "P-VALUE > .1"
C      END IF
      IF (RCHI .GT. 2.705) THEN
       PVAL=.1
      END IF

      IF (RCHI .GT. 3.841) THEN
       PVAL=.05
      END IF

      IF (RCHI .GT. 5.024) THEN
       PVAL=.025
      END IF

      IF (RCHI .GT. 6.635) THEN
       PVAL=.01
      END IF

      IF (RCHI .GT. 7.879) THEN
       PVAL=.005
      END IF

      IF (RCHI .GT. 10.828) THEN
       PVAL=.001
      END IF


      WRITE(33,195) (PVAL)
       WRITE(33,4308)
       WRITE(33,4308) "This p-value is based on a pseudo-likelihood"
        WRITE(33,4308) "ratio test between the models:"
       WRITE(33,4308)
       WRITE(33,4308) "LOG P(Xii'=xii')=theta0+theta1base(samegroup)"
       WRITE(33,4308)
       WRITE(33,4308) "LOG P(Xii'=xii')=theta0+theta1base(samegroup) +"
       WRITE(33,4308) " theta1 subgroup processes(samegroup) "
       WRITE(33,4308)
       WRITE(33,4308) "A SMALL P-VALUE INDICATES THAT ONE CAN"
       WRITE(33,4308) "REJECT THE NULL HYPOTHESIS THAT "
       WRITE(33,4308) " theta1 subgroup processes IS ZERO."
       WRITE(33,4308) "AND WE TAKE THIS AS EVIDENCE THAT ACTORS"
       WRITE(33,4308) "ENGAGE IN EXCHANGES WITHIN SUBGROUPS AT "
       WRITE(33,4308) "A RATE THAT IS UNLIKELY TO HAVE OCCURRED"
       WRITE(33,4308) "BY CHANCE ALONE."

      MAXSIZE=0
      DO 62614 J=1,MAXDEPT
      IF (DEPARTN(J) .GT. MAXSIZE) THEN
      MAXSIZE=DEPARTN(J)
      END IF
62614 CONTINUE
      PREDACC=1.4572+.7009*(PL-.8993) + 1.1977*(LOG(GLAMNDA)/2-PL-.3692)
     C + .0867*(MAXSIZE-5.737) + .0038*(NUMTEACH-51.7488)
      WRITE(33,4301) "PREDICTED ACCURACY: LOG ODDS OF COMMON SUBGROUP"
      WRITE(33,4301) "MEMBERSHIP, + OR - .5734 (FOR A 95% CI)"
      WRITE(33,195) PREDACC
       WRITE(33,4308)
       WRITE(33,4308) "The Log odds applies to the following table:"
       WRITE(33,4308)
       WRITE(33,4308) "                  OBSERVED SUBGROUP"
       WRITE(33,4308) "                  DIFFERENT   SAME"
       WRITE(33,4308) "                 ___________________"
       WRITE(33,4308) "                 |        |        |"
       WRITE(33,4308) "       DIFFERENT |   A    |   B    |"
       WRITE(33,4308) "KNOWN            |        |        |"
       WRITE(33,4308) "SUBGROUP         |--------|--------|"
       WRITE(33,4308) "                 |        |        |"
       WRITE(33,4308) "            SAME |   C    |   D    |"
       WRITE(33,4308) "                 |        |        |"
       WRITE(33,4308) "                 -------------------"
       WRITE(33,4308)
      WRITE(33,4301) "THE LOGODDS TRANSLATES TO AN ODDS RATIO OF "
      WRITE(33,195) (EXP(PREDACC))
      WRITE(33,4301) "WHICH INDICATES THE INCREASE IN THE PROBABILITY"
      WRITE(33,4301) "THAT KLIQUEFINDER WILL ASSIGN TWO ACTORS TO"
      WRITE(33,4301) "THE SAME SUBGROUP IF THEY ARE TRULY IN THE "
      WRITE(33,4301) "IN THE SAME SUBGROUP."
      END IF



      END IF

      WRITE(33,194)
       GLAMNDA=TAI*TBO
       GLAMNDA=GLAMNDA/(TBI*TAO)
      WRITE(33,194) "MEASURES OF FIT"
      WRITE(33,194) "PEARSON","LIKE RATIO","SUM (ZI**2)",
     C "ALT LIKE RATIO","ODDS RATIOS","THETA1"
      WRITE(33,195) (APEARSON+BPEARSON),(ALRT+BLRT),
     C (AEQUPEAR+BEQUPEAR)/2.,(ANLRT+BNLRT),GLAMNDA,
     C LOG(GLAMNDA)/2
      WRITE(33,194) "DEGREES OF FREEDOM"
      WRITE(33,195) (NUMG)
      WRITE(33,194) "P-VALUES"
      RCHI=APEARSON+BPEARSON
C      IF (RCHI .GT. 0) THEN
      IF ((RCHI .GT. 1) .AND. (RCHI .LT. 99999)) THEN 
      CALL MDCH(RCHI,NUMG,PVAL,IER)
      END IF
      RCHI=ALRT+BLRT
      IF ((RCHI .GT. 1) .AND. (RCHI .LT. 99999)) THEN 
C      IF (RCHI .GT. 0) THEN
      CALL MDCH(RCHI,NUMG,PVAL2,IER)
      END IF
      RCHI=AEQUPEAR+BEQUPEAR

      IF ((RCHI .GT. 1) .AND. (RCHI .LT. 99999)) THEN 
      CALL MDCH(RCHI,NUMG,PVAL3,IER)
      END IF
      WRITE(33,195) (PVAL),(PVAL2),(PVAL3)


      WRITE(33,194) "SUM OF Zi","P-VALUE"
      UPPER=.FALSE.
      PVAL=ALNORM(FCOMPAC2,UPPER)
      WRITE(33,195) FCOMPAC2,(1.0000-PVAL)
      WRITE(33,194) "/ACTOR","/NON ISOLATE"
      WRITE(33,195) (FCOMPAC2/NUMTEACH),(FCOMPAC2/INUM)
      WRITE(33,194)

      WRITE(33,194) "SUM OF Zi/Ng"
      WRITE(33,195) FCOMPAC5
      WRITE(33,194) "/ACTOR","/NON ISOLATE"
      WRITE(33,195) (FCOMPAC5/NUMTEACH),(FCOMPAC5/INUM)
      WRITE(33,194)

      END IF

      WRITE(33,62)
      IF (PRINTT .EQ. 2) THEN
C  191 FORMAT(/,"SIMILARITY BETWEEN THE START AND END GROUPS:   ACTUAL 
C     C  POSS  STANDARDIZED",/,43X,I7,3X,I7,3X,F10.5)
      WRITE(33,191) CHNGSIM,CHNGCON,CHNGSTD
      WRITE(33,62)
      END IF
      BCHNGSIM=CHNGSIM
      BCHNGCON=CHNGCON
      BCHNGSTD=CHNGSTD

      IF (STRUCTEQ .EQ. 1) THEN
      WRITE(33,77) " SUM OF SQUARES "
      ELSE
      WRITE(33,77) " ASSOCIATIONS -- COMPACTNESS"
      END IF

       LMAXDEPT=MAXDEPT
        CALL REMOVEO2(LMAXDEPT,DEPARTN,LGROUP,1,QUICKEND)

       WRITE(33,4301)
C      IF ((FINAL .EQ. 2) .AND. (PRINTO(13) .EQ. 1)) THEN
C      DO 3293 G1=2,LMAXDEPT
C       WRITE(510,103) (COVMAT(LGROUP(G1),LGROUP(U)) ,
C     C  U=1,G1-1)
C 3293  CONTINUE
C       END IF

      IF ((PRINTO(17) .EQ. 1) .AND. (FINAL .EQ. 2)) THEN
      OPEN(4,file='ucgc.DAT')
      WRITE(4,21091) 4,LMAXDEPT,LMAXDEPT
      WRITE(4,7511) (LB , LB=1,LMAXDEPT)
      DO 7323 G1=2,LMAXDEPT
       WRITE(4,7103) (COVMAT(LGROUP(G1),LGROUP(U)) ,
     C  U=1,G1-1)
07323   CONTINUE
      END IF

       IF ((SIMO .EQ. 1) .OR. (PRINTO(27) .EQ. 1)) THEN
      TD=0
      DO 3113 G1=1,LMAXDEPT

      IF ((DEPARTN(LGROUP(G1)) .GT. 1) .AND. 
     C (DEPARTN(LGROUP(G1)) .LE. 50))  THEN
      TD=TD+1
      TRUEDEPT(TD)=DEPARTN(LGROUP(G1))
      SLOTTD(TD)=TD
      END IF
03113   CONTINUE
      ACTG=TD
      SORTFLAG=2
      IF (TD .GT. 1) THEN
      CALL SPSORT(TRUEDEPT,TD,SLOTTD,SORTFLAG,IER)
      END IF
      MEDDEPT=TRUEDEPT(INT(TD/2.))
       ONDIAG=0
       ONDIAG2=0
       ONDIAG3=0
       ONDIAG4=0
       TOTPACT=0
       TOTPACT2=0
       DO 7787  G2=1,LMAXDEPT
       ONDIAG=ONDIAG+COVMAT(G2,G2)
       ONDIAG2=ONDIAG2+COVMAT2(G2,G2)
       ONDIAG3=ONDIAG3+COVMAT3(G2,G2)
       ONDIAG4=ONDIAG4+COVMAT4(G2,G2)
       IF ((DEPARTN(G2) .GT. 0) .AND. (DEPARTN(G2) .LE. 50)) THEN
       TOTPACT=TOTPACT+COVMAT(G2,G2)/DEPARTN(G2)
       TOTPACT2=TOTPACT2+COVMAT2(G2,G2)/DEPARTN(G2)
       END IF
07787   CONTINUE


       END IF
      NUMDIM=2
      IF ((FINAL .EQ. 2) .AND. (PRINTO(38) .EQ. 1) .AND. 
     C (NUMDIM .GT. 0))  THEN
      LINKFILE="inlist.list"
      SIXTEEN=16
      SIXTEEN=UNLINK(LINKFILE)
      STATUS=SYMLNK(INFILE,LINKFILE)

C      DANCHOR=1
C      ZSYMMAT=1
C      STARTINC=6
C      MAXINC=22
C      BYINC=4
C      IGRATIO=7.5

      IMEANVAL=0
C      CENTER=1

C      MOVE2=0
      IMAXVAL=HIWT

      DO 7723 I=1,MAXDEPT
      DO 77231 J=1,NUMDIM
      GCOORD(I,J)=0
77231  CONTINUE
07723   CONTINUE

      IF (PRINTO(39) .EQ. 1) THEN

      IF (NORMAL .EQ. 1) THEN
      N2=1
      ELSE
      N2=0
      END IF

      DO 7721 I=1,NUMTEACH
      DO 77212 J=1,NUMDIM
      ICOORD(I,J)=0
      ZICOORD(I,J)=0
77212 CONTINUE
      IANGLE(I)=0
      IRADIUS(I)=0

      ZIRADIUS(I)=0
      IANCHOR(I)=1
C       MINVALI=.75
C       MINVALG=.10
07721  CONTINUE


      DO 82821 I=1,MAXDEPT

         IF ((DEPARTN(I) .GT. 1) .AND. (DEPARTN(I) .LE. 50)) THEN
      CALL ONEPIECE(I,DEPARTN(I),TEMPMAT,OLDORD,TF1)
       IF (BYANGLE .EQ. 2) THEN
       WRITSMAC=1
      CALL PLOTMAT(TEMPMAT,DEPARTN(I),DANCHORI,NUMDIM,
     C STARTINI,ZSYMMATI,TICOORD,MAXINCI,BYINCI,TMEANVAL,NUMTEACH,
     C TIANGLE,TIRADIUS,CENTERI,TANCHOR,DANCH2I,MOVE2I,IMAXVAL,
     C DRADIUSI,
     C KEXPI,NORMALI,MINVALI,TF1,BYANGLEI,BYSCALEI,MINPICT,
     C PCTCENI1,PCTCENI2,WRITSMAC,TWARP,RANGEEIG)
C       CALL RUNSMAC
       WRITSMAC=2
      CALL PLOTMAT(TEMPMAT,DEPARTN(I),DANCHORI,NUMDIM,
     C STARTINI,ZSYMMATI,TICOORD,MAXINCI,BYINCI,TMEANVAL,NUMTEACH,
     C TIANGLE,TIRADIUS,CENTERI,TANCHOR,DANCH2I,MOVE2I,IMAXVAL,
     C DRADIUSI,
     C KEXPI,NORMALI,MINVALI,TF1,BYANGLEI,BYSCALEI,MINPICT,
     C PCTCENI1,PCTCENI2,WRITSMAC,TWARP,RANGEEIG)
       END IF
       TWARP=1
C       IF (IMAXVAL .LE. 1) THEN
C       TWARP=0
C       END IF 
       WRITSMAC=3
      CALL PLOTMAT(TEMPMAT,DEPARTN(I),DANCHORI,NUMDIM,
     C STARTINI,ZSYMMATI,TICOORD,MAXINCI,BYINCI,TMEANVAL,NUMTEACH,
     C TIANGLE,TIRADIUS,CENTERI,TANCHOR,DANCH2I,MOVE2I,IMAXVAL,
     C DRADIUSI,
     C KEXPI,NORMALI,MINVALI,TF1,BYANGLEI,BYSCALEI,MINPICT,
     C PCTCENI1,PCTCENI2,WRITSMAC,TWARP,RANGEEIG)

     
       GWARP(I)=TWARP
       GRANGE1(I)=RANGEEIG(1)
       GRANGE2(I)=RANGEEIG(2)

      IF (TMEANVAL .GT. IMEANVAL) THEN
      IMEANVAL=TMEANVAL
      END IF
      DO 6769 J=1,DEPARTN(I)
      DO 6799 K=1,NUMDIM
      ZICOORD(OLDORD(J),K)=TICOORD(J,K)
06799 CONTINUE
      IANGLE(OLDORD(J))=TIANGLE(J)
      IANCHOR(OLDORD(J))=TANCHOR(J)
      ZIRADIUS(OLDORD(J))=TIRADIUS(J)
06769  CONTINUE
       END IF
82821   CONTINUE
      IF (NORMALI .EQ. 0) THEN
      IMEANVAL=HIWT
      END IF
      IF (NORMALI .EQ. 2) THEN
      IMEANVAL=-LOG(.01)
      END IF
      IF (NORMALI .EQ. 3) THEN
      IMEANVAL=LOG(HIWT)-LOG(ABS(MINVALI))
       END IF
      IF (NORMALI .EQ. -3) THEN
      IMEANVAL=HIWT
       END IF

      END IF
C      IF (PRINTO(39) .EQ. 1) THEN

      IF (MAXDEPT .GT. 1)  THEN
      
      IF  (PRINTO(30) .EQ. 1) THEN
      GMAXVAL=0
       IF (BYANGLE .EQ. 2) THEN
       WRITSMAC=1
      CALL PLOTMAT(COVMAT,MAXDEPT,DANCHOR,NUMDIM,
     C STARTINC,ZSYMMAT,GCOORD,MAXINC,BYINC,GMEANVAL,MAXDEPT,
     C GANGLE,GRADIUS,CENTER,ANCHOR,DANCHOR2,MOVE2,GMAXVAL,DRADIUS,
     C KEXP,N2,MINVALG,MAXDEPT,BYANGLE,BYSCALE,MINPICT,
     C PCTCENG1,PCTCENG2,WRITSMAC,TWARPA,RANGEEIG)

       WRITSMAC=2
      CALL PLOTMAT(COVMAT,MAXDEPT,DANCHOR,NUMDIM,
     C STARTINC,ZSYMMAT,GCOORD,MAXINC,BYINC,GMEANVAL,MAXDEPT,
     C GANGLE,GRADIUS,CENTER,ANCHOR,DANCHOR2,MOVE2,GMAXVAL,DRADIUS,
     C KEXP,N2,MINVALG,MAXDEPT,BYANGLE,BYSCALE,MINPICT,
     C PCTCENG1,PCTCENG2,WRITSMAC,TWARPA,RANGEEIG)

       END IF
       TWARPA=1
       WRITSMAC=3
      CALL PLOTMAT(COVMAT,MAXDEPT,DANCHOR,NUMDIM,
     C STARTINC,ZSYMMAT,GCOORD,MAXINC,BYINC,GMEANVAL,MAXDEPT,
     C GANGLE,GRADIUS,CENTER,ANCHOR,DANCHOR2,MOVE2,GMAXVAL,DRADIUS,
     C KEXP,N2,MINVALG,MAXDEPT,BYANGLE,BYSCALE,MINPICT,
     C PCTCENG1,PCTCENG2,WRITSMAC,TWARPA,RANGEEIG)


      DISTFILE=INFILE(1:6) // ".agang"
      COORFILE=INFILE(1:6) // ".acord"
      WITHID=0
      CALL OUTCOORD(COORFILE,MAXDEPT,NUMDIM,GCOORD,WITHID,LGROUP,
     C GANGLE,GRADIUS,LGROUP,ANCHOR,GHIER,FITG,NSIM,PRINTK,PRINTO(27),
     C ISTRESS,
     C ISTRESS2,ISTRESS3,PRINTO(43),DISTFILE,GWARP,GRANGE1,GRANGE2)


      IF (PRINTO(39) .EQ. 1) THEN
      NRATIO=GMAXVAL/(IGRATIO*IMEANVAL)
      IF (PRINTO(41) .EQ. 1) THEN
C      RINCREM=10
C      MEASURE=2
C      EXTREME=1

      CALL ROTATE(ICOORD,GCOORD,NUMTEACH,MAXDEPT,IANGLE,ZIRADIUS,
     C RINCREM,MEASURE,DEPARTN,NRATIO,EXTREME,IMEANVAL,IRADIUS)

      ELSE

        DO 8783 I=1,MAXDEPT
        DO 8785 J=1,DEPARTN(I)
        DO 8784 K=1,NUMDIM
      ICOORD(ALLGROUP(I,J),K)=GCOORD(I,K)+
     C ZICOORD(ALLGROUP(I,J),K)*NRATIO
08784  CONTINUE
      IRADIUS(ALLGROUP(I,J))=ZIRADIUS(ALLGROUP(I,J))
     C *GMEANVAL/(IGRATIO*IMEANVAL)

08785   CONTINUE
08783    CONTINUE
      END IF

      COORFILE=INFILE(1:6) // ".aicrd"
      DISTFILE=INFILE(1:6) // ".adist"
      WITHID=1

      CALL KSTRESS(STRESSA,ICOORD,NORMALI,NUMTEACH,IMAXVAL,
     C MINVALI,KEXPI,STRESS2A,FITI,ISTRESS,STRESS3A,
     C ISTRESS2,ISTRESS3,ZICOORD,DISTFILE,PRINTO,NEWDEPT,NSUBID,
     C ODDSA)
      WRITE(33,7474) "The stress associated with ",COORFILE," is "
     C ,STRESSA

      WRITE(33,7474) "The warp associated with ",COORFILE," is "
     C ,TWARPA

      DISTFILE=INFILE(1:6) // ".aiang"
      CALL OUTCOORD(COORFILE,NUMTEACH,NUMDIM,ICOORD,
     C WITHID,NSUBID,IANGLE,IRADIUS,NEWDEPT,IANCHOR,IHIER,FITI,
     C NSIM,PRINTK,PRINTO(27),ISTRESS,
     C ISTRESS2,ISTRESS3,PRINTO(43),DISTFILE,GWARP,GRANGE1,GRANGE2)
      END IF
C      PRINTO(39)
      END IF
C      (PRINTO(30) .EQ. 1) THEN

C to here


      IF (PRINTO(32) .EQ. 1) THEN 
      GMAXVAL=HIWT
       IF (BYANGLE .EQ. 2) THEN
       WRITSMAC=1
       CALL PLOTMAT(COVMAT3,MAXDEPT,DANCHOR,NUMDIM,
     C STARTINC,ZSYMMAT,GCOORD,MAXINC,BYINC,GMEANVAL,MAXDEPT,
     C GANGLE,GRADIUS,CENTER,ANCHOR,DANCHOR2,MOVE2,GMAXVAL,DRADIUS,
     C KEXP,NORMAL,MINVALG,MAXDEPT,BYANGLE,BYSCALE,MINPICT,
     C PCTCENG1,PCTCENG2,WRITSMAC,TWARPB,RANGEEIG)

       WRITSMAC=2
       CALL PLOTMAT(COVMAT3,MAXDEPT,DANCHOR,NUMDIM,
     C STARTINC,ZSYMMAT,GCOORD,MAXINC,BYINC,GMEANVAL,MAXDEPT,
     C GANGLE,GRADIUS,CENTER,ANCHOR,DANCHOR2,MOVE2,GMAXVAL,DRADIUS,
     C KEXP,NORMAL,MINVALG,MAXDEPT,BYANGLE,BYSCALE,MINPICT,
     C PCTCENG1,PCTCENG2,WRITSMAC,TWARPB,RANGEEIG)
       END IF
       TWARPB=1
       WRITSMAC=3
       CALL PLOTMAT(COVMAT3,MAXDEPT,DANCHOR,NUMDIM,
     C STARTINC,ZSYMMAT,GCOORD,MAXINC,BYINC,GMEANVAL,MAXDEPT,
     C GANGLE,GRADIUS,CENTER,ANCHOR,DANCHOR2,MOVE2,GMAXVAL,DRADIUS,
     C KEXP,NORMAL,MINVALG,MAXDEPT,BYANGLE,BYSCALE,MINPICT,
     C PCTCENG1,PCTCENG2,WRITSMAC,TWARPB,RANGEEIG)

      COORFILE=INFILE(1:6) // ".bcord"
      DISTFILE=INFILE(1:6) // ".bgang"
      WITHID=0
      CALL OUTCOORD(COORFILE,MAXDEPT,NUMDIM,GCOORD,WITHID,LGROUP,
     C GANGLE,GRADIUS,LGROUP,ANCHOR,GHIER,FITG,NSIM,PRINTK,PRINTO(27),
     C ISTRESS,
     C ISTRESS2,ISTRESS3,PRINTO(43),DISTFILE,GWARP,GRANGE1,GRANGE2)


      IF (PRINTO(39) .EQ. 1) THEN
      IF (NORMAL .EQ. 2) THEN
      GMAXVAL=-LOG(ABS(MINVALG))
      END IF
      IF (NORMAL .EQ. 3) THEN
      GMAXVAL=LOG(HIWT)-LOG(MINVALG)
       END IF

      IF (NORMAL .EQ. -3) THEN
       GMAXVAL=HIWT
       END IF

      NRATIO=GMAXVAL/(IGRATIO*IMEANVAL)
      IF (PRINTO(41) .EQ. 1) THEN
C      RINCREM=10
C      MEASURE=2
C      EXTREME=1

      CALL ROTATE(ICOORD,GCOORD,NUMTEACH,MAXDEPT,IANGLE,ZIRADIUS,
     C RINCREM,MEASURE,DEPARTN,NRATIO,EXTREME,IMEANVAL,IRADIUS)

      ELSE

        DO 28783 I=1,MAXDEPT
        DO 28785 J=1,DEPARTN(I)
        DO 28784 K=1,NUMDIM
      ICOORD(ALLGROUP(I,J),K)=GCOORD(I,K)+
     C ZICOORD(ALLGROUP(I,J),K)*NRATIO
28784  CONTINUE
      IRADIUS(ALLGROUP(I,J))=ZIRADIUS(ALLGROUP(I,J))
     C *GMEANVAL/(IGRATIO*IMEANVAL)

28785   CONTINUE
28783    CONTINUE
      END IF

      COORFILE=INFILE(1:6) // ".bicrd"
      DISTFILE=INFILE(1:6) // ".bdist"
      WITHID=1

      CALL KSTRESS(STRESSB,ICOORD,NORMALI,NUMTEACH,IMAXVAL,
     C MINVALI,KEXPI,STRESS2B,FITI,ISTRESS,STRESS3B,
     C ISTRESS2,ISTRESS3,ZICOORD,DISTFILE,PRINTO,NEWDEPT,NSUBID,
     C ODDSB)
      WRITE(33,7474) "The stress associated with ",COORFILE," is ",
     C STRESSB
      WRITE(33,7474) "The warp associated with ",COORFILE," is ",
     C TWARPB

      DISTFILE=INFILE(1:6) // ".biang"
      CALL OUTCOORD(COORFILE,NUMTEACH,NUMDIM,ICOORD,
     C WITHID,NSUBID,IANGLE,IRADIUS,NEWDEPT,IANCHOR,IHIER,FITI,
     C NSIM,PRINTK,PRINTO(27),ISTRESS,
     C ISTRESS2,ISTRESS3,PRINTO(43),DISTFILE,GWARP,GRANGE1,GRANGE2)

      DO 7272 I=1,NUMTEACH
       DO 72721 J=1,NUMDIM
      BICOORD(I,J)=ICOORD(I,J)
72721  CONTINUE
07272   CONTINUE

      END IF
C      PRINTO(39)

      END IF


      IF (PRINTO(31) .EQ. 1) THEN 
       GMAXVAL=0
      IF (BYANGLE .EQ. 2) THEN
      WRITSMAC=1
      CALL PLOTMAT(COVMAT2,MAXDEPT,DANCHOR,NUMDIM,
     C STARTINC,ZSYMMAT,GCOORD,MAXINC,BYINC,GMEANVAL,MAXDEPT,
     C GANGLE,GRADIUS,CENTER,ANCHOR,DANCHOR2,MOVE2,GMAXVAL,DRADIUS,
     C KEXP,N2,MINVALG,MAXDEPT,BYANGLE,BYSCALE,MINPICT,
     C PCTCENG1,PCTCENG2,WRITSMAC,TWARPC,RANGEEIG)

      WRITSMAC=2
      CALL PLOTMAT(COVMAT2,MAXDEPT,DANCHOR,NUMDIM,
     C STARTINC,ZSYMMAT,GCOORD,MAXINC,BYINC,GMEANVAL,MAXDEPT,
     C GANGLE,GRADIUS,CENTER,ANCHOR,DANCHOR2,MOVE2,GMAXVAL,DRADIUS,
     C KEXP,N2,MINVALG,MAXDEPT,BYANGLE,BYSCALE,MINPICT,
     C PCTCENG1,PCTCENG2,WRITSMAC,TWARPC,RANGEEIG)
      END IF
       TWARPC=1
      WRITSMAC=3
      CALL PLOTMAT(COVMAT2,MAXDEPT,DANCHOR,NUMDIM,
     C STARTINC,ZSYMMAT,GCOORD,MAXINC,BYINC,GMEANVAL,MAXDEPT,
     C GANGLE,GRADIUS,CENTER,ANCHOR,DANCHOR2,MOVE2,GMAXVAL,DRADIUS,
     C KEXP,N2,MINVALG,MAXDEPT,BYANGLE,BYSCALE,MINPICT,
     C PCTCENG1,PCTCENG2,WRITSMAC,TWARPC,RANGEEIG)
      COORFILE=INFILE(1:6) // ".ccord"
      DISTFILE=INFILE(1:6) // ".cgang"
      WITHID=0
      CALL OUTCOORD(COORFILE,MAXDEPT,NUMDIM,GCOORD,WITHID,LGROUP,
     C GANGLE,GRADIUS,LGROUP,ANCHOR,GHIER,FITG,NSIM,PRINTK,PRINTO(27),
     C ISTRESS,
     C ISTRESS2,ISTRESS3,PRINTO(43),DISTFILE,GWARP,GRANGE1,GRANGE2)


      IF (PRINTO(39) .EQ. 1) THEN
C      IMEANVAL=HIWT
      NRATIO=GMAXVAL/(IGRATIO*IMEANVAL)
      IF (PRINTO(41) .EQ. 1) THEN
C      RINCREM=10
C      MEASURE=2
C      EXTREME=1

      CALL ROTATE(ICOORD,GCOORD,NUMTEACH,MAXDEPT,IANGLE,ZIRADIUS,
     C RINCREM,MEASURE,DEPARTN,NRATIO,EXTREME,IMEANVAL,IRADIUS)

      ELSE

        DO 38783 I=1,MAXDEPT
        DO 38785 J=1,DEPARTN(I)
        DO 38784 K=1,NUMDIM
      ICOORD(ALLGROUP(I,J),K)=GCOORD(I,K)+
     C ZICOORD(ALLGROUP(I,J),K)*NRATIO
38784  CONTINUE
      IRADIUS(ALLGROUP(I,J))=ZIRADIUS(ALLGROUP(I,J))
     C *GMEANVAL/(IGRATIO*IMEANVAL)

38785   CONTINUE
38783    CONTINUE
      END IF

      COORFILE=INFILE(1:6) // ".cicrd"
      DISTFILE=INFILE(1:6) // ".cdist"
      WITHID=1
      CALL KSTRESS(STRESSC,ICOORD,NORMALI,NUMTEACH,IMAXVAL,
     C MINVALI,KEXPI,STRESS2C,FITI,ISTRESS,STRESS3C,
     C ISTRESS2,ISTRESS3,ZICOORD,DISTFILE,PRINTO,NEWDEPT,NSUBID,
     C ODDSC)
      WRITE(33,7474) "The stress associated with ",COORFILE," is ",
     C STRESSC
      WRITE(33,7474) "The warp associated with ",COORFILE," is ",
     C TWARPC

      DISTFILE=INFILE(1:6) // ".ciang"
      CALL OUTCOORD(COORFILE,NUMTEACH,NUMDIM,ICOORD,
     C WITHID,NSUBID,IANGLE,IRADIUS,NEWDEPT,IANCHOR,IHIER,FITI,
     C NSIM,PRINTK,PRINTO(27),ISTRESS,
     C ISTRESS2,ISTRESS3,PRINTO(43),DISTFILE,GWARP,GRANGE1,GRANGE2)
      END IF
C      PRINTO(39)

      END IF

      IF (PRINTO(33) .EQ. 1) THEN
      GMAXVAL=1
      IF (BYANGLE .EQ. 2) THEN
       WRITSMAC=1
      CALL PLOTMAT(COVMAT4,MAXDEPT,DANCHOR,NUMDIM,
     C STARTINC,ZSYMMAT,GCOORD,MAXINC,BYINC,GMEANVAL,MAXDEPT,
     C GANGLE,GRADIUS,CENTER,ANCHOR,DANCHOR2,MOVE2,GMAXVAL,DRADIUS,
     C KEXP,NORMAL,MINVALG,MAXDEPT,BYANGLE,BYSCALE,MINPICT,
     C PCTCENG1,PCTCENG2,WRITSMAC,TWARPD,RANGEEIG)

       WRITSMAC=2
      CALL PLOTMAT(COVMAT4,MAXDEPT,DANCHOR,NUMDIM,
     C STARTINC,ZSYMMAT,GCOORD,MAXINC,BYINC,GMEANVAL,MAXDEPT,
     C GANGLE,GRADIUS,CENTER,ANCHOR,DANCHOR2,MOVE2,GMAXVAL,DRADIUS,
     C KEXP,NORMAL,MINVALG,MAXDEPT,BYANGLE,BYSCALE,MINPICT,
     C PCTCENG1,PCTCENG2,WRITSMAC,TWARPD,RANGEEIG)
       END IF 
       TWARPD=1
       WRITSMAC=3
      CALL PLOTMAT(COVMAT4,MAXDEPT,DANCHOR,NUMDIM,
     C STARTINC,ZSYMMAT,GCOORD,MAXINC,BYINC,GMEANVAL,MAXDEPT,
     C GANGLE,GRADIUS,CENTER,ANCHOR,DANCHOR2,MOVE2,GMAXVAL,DRADIUS,
     C KEXP,NORMAL,MINVALG,MAXDEPT,BYANGLE,BYSCALE,MINPICT,
     C PCTCENG1,PCTCENG2,WRITSMAC,TWARPD,RANGEEIG)


      COORFILE=INFILE(1:6) // ".dcord"
      DISTFILE=INFILE(1:6) // ".dgang"
      WITHID=0
      CALL OUTCOORD(COORFILE,MAXDEPT,NUMDIM,GCOORD,WITHID,LGROUP,
     C GANGLE,GRADIUS,LGROUP,ANCHOR,GHIER,FITG,NSIM,PRINTK,PRINTO(27),
     C ISTRESS,ISTRESS2,ISTRESS3,PRINTO(43),DISTFILE,GWARP,GRANGE1,
     C GRANGE2)


      IF (PRINTO(39) .EQ. 1) THEN
      IF (NORMAL .EQ. 2) THEN
      GMAXVAL=-LOG(ABS(MINVALG))
      END IF
      IF (NORMAL .EQ. 3) THEN
      GMAXVAL=LOG(GMAXVAL)-LOG(ABS(MINVALG))
       END IF
      IF (NORMAL .EQ. -3) THEN
       GMAXVAL=GMAXVAL/ABS(MINVALG)
       END IF

      NRATIO=GMAXVAL/(IGRATIO*IMEANVAL)
      IF (PRINTO(41) .EQ. 1) THEN
C      RINCREM=10
C      MEASURE=2
C      EXTREME=1

      CALL ROTATE(ICOORD,GCOORD,NUMTEACH,MAXDEPT,IANGLE,ZIRADIUS,
     C RINCREM,MEASURE,DEPARTN,NRATIO,EXTREME,IMEANVAL,IRADIUS)

      ELSE

        DO 48783 I=1,MAXDEPT
        DO 48785 J=1,DEPARTN(I)
        DO 48784 K=1,NUMDIM
      ICOORD(ALLGROUP(I,J),K)=GCOORD(I,K)+
     C ZICOORD(ALLGROUP(I,J),K)*NRATIO
48784  CONTINUE
      IRADIUS(ALLGROUP(I,J))=ZIRADIUS(ALLGROUP(I,J))
     C *GMEANVAL/(IGRATIO*IMEANVAL)

48785   CONTINUE
48783    CONTINUE
      END IF

      COORFILE=INFILE(1:6) // ".dicrd"
      DISTFILE=INFILE(1:6) // ".ddist"
      WITHID=1
      CALL KSTRESS(STRESSD,ICOORD,NORMALI,NUMTEACH,IMAXVAL,
     C MINVALI,KEXPI,STRESS2D,FITI,ISTRESS,STRESS3D,
     C ISTRESS2,ISTRESS3,ZICOORD,DISTFILE,PRINTO,NEWDEPT,NSUBID,
     C ODDSD)
      WRITE(33,7474) "The stress associated with ",COORFILE," is ",
     C STRESSD
      WRITE(33,7474) "The warp associated with ",COORFILE," is ",
     C TWARPD

      DISTFILE=INFILE(1:6) // ".diang"
      CALL OUTCOORD(COORFILE,NUMTEACH,NUMDIM,ICOORD,
     C WITHID,NSUBID,IANGLE,IRADIUS,NEWDEPT,IANCHOR,IHIER,FITI,
     C NSIM,PRINTK,PRINTO(27),ISTRESS,
     C ISTRESS2,ISTRESS3,PRINTO(43),DISTFILE,GWARP,GRANGE1,GRANGE2)
      END IF
C      PRINTO(39)
 
      END IF

       END IF
C   IF (MAXDEPT .GT. 1) 
       END IF
C      IF ((FINAL .EQ. 2) .AND. (PRINTO(38) .EQ. 1) .AND. 
C     C (NUMDIM .GT. 0))  THEN

      IF ((FINAL .EQ. 2) .AND. (PRINTO(13) .EQ. 1)) THEN
      OPEN(59,file='MDSSCALE.CMD')
      WRITE(59,4301) "NOTE '",TITLES(1),TITLES(2),TITLES(3),
     C "  ",INFILE,
     C "'"
      WRITE(59,4301) "USE MDSD"
      WRITE(59,4301) "OUTPUT MDSO"
      WRITE(59,4301) "SCALE"
      CLOSE(59)
      IF (PRINTO(30) .EQ. 1) THEN
      CALL MDSCMD("Compactness","corr ","A",TITLES,LMAXDEPT,INFILE,
     C LGROUP,COVMAT)
      END IF
      IF (PRINTO(32) .EQ. 1) THEN 
      CALL MDSCMD("Density    ","bcorr ","B",TITLES,LMAXDEPT,INFILE,
     C LGROUP,COVMAT3)
      END IF
      IF (PRINTO(31) .EQ. 1) THEN 
      CALL MDSCMD("Sum of Zi  ","ccorr ","C",TITLES,LMAXDEPT,INFILE,
     C LGROUP,COVMAT2)
      END IF
      IF (PRINTO(33) .EQ. 1) THEN 
      CALL MDSCMD("Within grp ","dcorr ","D",TITLES,LMAXDEPT,INFILE,
     C LGROUP,COVMAT4)
      END IF

      END IF

      IF ((PRINTO(30) .EQ. 1) .AND. (LMAXDEPT .GT. 0)) THEN
      CALL GROUPASC (COVMAT,LMAXDEPT,LGROUP,DEPARTN,
     C 0,1,INUM,NUMTEACH,INFILE,"dat ",PRINTO(34),LABEL,PRINTO(27),0,
     C NSIM,FINAL,MAXDEPT,PRINTK,LAMNDA)

      IF (PRINTO(35) .EQ. 1) THEN
      CALL PERXPER(COVMAT,NUMTEACH,NEWDEPT,"dat ",NSUBID,LGROUP,
     C LMAXDEPT,MAXDEPT)
      END IF

       WRITE(33,4301)
      WRITE(33,3308) "NETLEV"
      WRITE(33,3307) (GCENTRAL(GORDER(G1)) , G1=1,LMAXDEPT)
       WRITE(33,4301)
      END IF
      IF ((PRINTO(31) .EQ. 1) .AND. (LMAXDEPT .GT. 0)) THEN
       WRITE(33,4301)
      WRITE(33,77) " ASSOCIATIONS (SUM OF Zi) "
      CALL GROUPASC (COVMAT2,LMAXDEPT,LGROUP,DEPARTN,
     C 0,1,INUM,NUMTEACH,INFILE,"cdat",PRINTO(34),LABEL,PRINTO(27),0,
     C NSIM,FINAL,MAXDEPT,PRINTK,LAMNDA)
      IF (PRINTO(35) .EQ. 1) THEN
      CALL PERXPER(COVMAT2,NUMTEACH,NEWDEPT,"cdat",NSUBID,LGROUP,
     C LMAXDEPT,MAXDEPT)
      END IF
      END IF

       IF ((PRINTO(32) .EQ. 1) .AND. (LMAXDEPT .GT. 0)) THEN
          WRITE(33,4301)
       WRITE(33,4301) "MEAN WITHIN GROUP CONNECTION (DENSITY)"
       WRITE(33,251)
      IF (FLAGR .EQ. 1) THEN
       WRITE(33,8282) FIXR," GREATER THAN OR EQUAL TO ",OLDR
     C ," FORMER "
      ELSE
       WRITE(33,8282) NFIXR," LESS THAN ",OLDR," LATTER "
       FIXR=OLDR
      END IF
       WRITE(33,251)
   
      CALL GROUPASC (COVMAT3,LMAXDEPT,LGROUP,DEPARTN,
     C 0,1,INUM,NUMTEACH,INFILE,"bdat",PRINTO(34),LABEL,PRINTO(27),1,
     C NSIM,FINAL,MAXDEPT,PRINTK,LAMNDA)
      IF (PRINTO(35) .EQ. 1) THEN
      CALL PERXPER(COVMAT3,NUMTEACH,NEWDEPT,"bdat",NSUBID,LGROUP,
     C LMAXDEPT,MAXDEPT)
      END IF
       WRITE(33,62)
       WRITE(33,4301) "NETWORK WIDE MEAN WITHIN GROUP CONNECTION"
       WRITE(33,195) FCOMPAC9
       WRITE(33,62)
       END IF

       WRITE(33,62)
       IF ((PRINTO(33) .EQ. 1) .AND. (LMAXDEPT .GT. 0)) THEN
       WRITE(33,4301)
       WRITE(33,4301) "PCT CONNECTIONS THAT OCCUR WITHIN GROUP"
      CALL GROUPASC (COVMAT4,LMAXDEPT,LGROUP,DEPARTN,
     C 0,0,INUM,NUMTEACH,INFILE,"ddat",PRINTO(34),LABEL,PRINTO(27),1,
     C NSIM,FINAL,MAXDEPT,PRINTK,LAMNDA)
      IF (PRINTO(35) .EQ. 1) THEN
      CALL PERXPER(COVMAT4,NUMTEACH,NEWDEPT,"ddat",NSUBID,LGROUP,
     C LMAXDEPT,MAXDEPT)
      END IF
      WRITE(33,4301) "NETWORK WIDE PCT CONNECTIONS IN GROUPS"
       WRITE(33,195) FCOMPAC8
       END IF

       WRITE(33,62)
       WRITE(33,4301)
       
       IF ((FINAL .EQ. 2) .AND. (PRINTO(27) .EQ. 1)) THEN
       PREDL=1.0620+3.8052*ONDIAG3/ACTG+.0041*NUMTEACH+
     C .2711*NUMTEACH/ACTG-.1424*TFIXR
        RINUM=INUM
         RNSIM=NSIM
        RPRINTK=PRINTK
        RACTG=ACTG
        RMED=MEDDEPT
        RTOTCENT=TOTCENT
        RMADECH=MADECHOI 


      TTAIE=0
      TTAOE=0
      TTBIE=0
      TTBOE=0
      TTAIO=0
      TTAOO=0
      TTBIO=0
      TTBOO=0

      DO 3353 TG1=1,MAXDEPT
          G1=GORDER(TG1)

       TTAIE=TTAIE+ACMEANI(G1)
       TTAOE=TTAOE+ACMEANO(G1)
       TTBIE=TTBIE+BCMEANI(G1)
       TTBOE=TTBOE+BCMEANO(G1)
       TTAIO=TTAIO+AOBSCONI(G1)
       TTAOO=TTAOO+AOBSCONO(G1)
       TTBIO=TTBIO+BOBSCONI(G1)
       TTBOO=TTBOO+BOBSCONO(G1)
03353   CONTINUE

       CALL KFIT(NUMTEACH,TFITI,ODDSB)
       WRITE(19,25144) KCOMPACT,STDHS,FCOMPAC2,FCOMPAC5,ONDIAG,
     C RACTG,(ONDIAG/ACTG),
     C TOTPACT,RACTG,(TOTPACT/ACTG),ONDIAG2,(ONDIAG2/ACTG),
     C TOTPACT2,(TOTPACT2/ACTG),RINUM,
     C FCOMPAC8,ONDIAG3,(ONDIAG3/ACTG),
     C FCOMPAC9,(ONDIAG4/ACTG),RMED,RTOTCENT,XPC,RMADECH,
     C CHNGSIM,CHNGCON,CHNGSTD,(APEARSON+BPEARSON),(ALRT+BLRT),
     C STRESSA,STRESSB,STRESSC,STRESSD,
     C STRESS2A,STRESS2B,STRESS2C,STRESS2D,
     C BCHNGSIM,BCHNGCON,BCHNGSTD,RNSIM,RPRINTK,
     C STRESS3A,STRESS3B,STRESS3C,STRESS3D,
     C SAMPMEAN,SAMPSD,CSMEAN,CSSD,ZVAL,CZVAL,CPVAL,SPVAL,
     C SAMPMEA2,SAMPSD2,CSMEAN2,CSSD2,ZVAL2,CZVAL2,CPVAL2,
     C SPVAL2,PREDACC,PREDACC2,PREDL,(ANLRT+BNLRT),
     C TTAIE,TTAOE,TTBIE,TTBOE,TTAIO,TTAOO,TTBIO,TTBOO,
     C GLAMNDA,ODDSB,TWARPA,TWARPB,TWARPC,TWARPD
        OPEN(15,FILE="sampdist.dat")
        CALL MOVEEND(152) 
        WRITE(15,25144) (LOG(GLAMNDA)/2),GLAMNDA
        CLOSE(15)  


        END IF

       IF (PRINTO(15) .EQ. 1) THEN 
       WRITE(33,517)
       WRITE(33,4308)
       WRITE(33,517) "LOGIT ANALYSIS"
       WRITE(33,4308)
       WRITE(33,4308) "BELOW ARE REPORTED RESIDUALS FROM A "
       WRITE(33,4308) "LOGIT ANALYSIS.  YOU WILL SEE FIVE COLUMNS"
       WRITE(33,4308) " ASSOCIATED WITH EACH GROUP."
       WRITE(33,4308) "THE FIRST FOUR COLUMNS CORRESPOND TO THE CELLS"
       WRITE(33,4308) "OF THE BOX BELOW:"
       WRITE(33,4308) 
       WRITE(33,4308) "                  CONNECTION"
       WRITE(33,4308) "                 NO        YES"
       WRITE(33,4308) "              ___________________"
       WRITE(33,4308) "              |        |        |"
       WRITE(33,4308) "           NO | OUTNOC | OUTCH  |"
       WRITE(33,4308) "IN SAME       |        |        |"
       WRITE(33,4308) "GROUP         |--------|--------|"
       WRITE(33,4308) "              |        |        |"
       WRITE(33,4308) "          YES | INNOC  | INCH   |"
       WRITE(33,4308) "              |        |        |"
       WRITE(33,4308) "              -------------------"
       WRITE(33,4308) 
       WRITE(33,4308) "THUS, INCHI FOR GROUP 1"
       WRITE(33,4308) "REPRESENTS THE SQUARED RESIDUAL ASSOCIATED"
       WRITE(33,4308) "FOR CONNECTIONS MADE BY ACTORS IN GROUP 1"
       WRITE(33,4308) "WITH ACTORS IN GROUP 1."
       WRITE(33,4308)
       WRITE(33,4308) "THE LOGIT MODEL FOR A SINGLE GROUP, G, WOULD BE:" 
       WRITE(33,4308) "CONNECTION (1 OR 0) = B1*SAMEG + e"
       WRITE(33,4308) 

       WRITE(33,4308) "THE LOGLINEAR MODEL FOR A SINGLE GROUP, G,"
       WRITE(33,4308) " WOULD BE:" 
       WRITE(33,4308) "COUNT (ACTUAL #) = B1*CONNECT + B2*SAMEG + e"
       WRITE(33,4308) 
       WRITE(33,4308) "WHERE ACTOR I, THE INITIATOR, IS IN GROUP G"
       WRITE(33,4308) 
       WRITE(33,4308) "AND"
       WRITE(33,4308) "SAMEG =1 IF ACTOR J, THE OBJECT, IS IN GROUP G"
       WRITE(33,4308) "SAMEG =0 IF ACTOR J IS NOT IN GROUP G"
       WRITE(33,4308) 
       WRITE(33,4308) "CONNECT =1 IF ACTOR I AND ACTOR J ARE CONNECTED"
       WRITE(33,4308) "CONNECT =0  OTHERWISE"
       WRITE(33,4308) 
       WRITE(33,4308) "THE RESIDUALS CORRESPOND TO THE LOGLINEAR MODEL"
       WRITE(33,4308) 

       WRITE(33,4308) "THE LAST COLUMN REPRESENTS THE SUM OF THE"
       WRITE(33,4308) "PRECEEDING FOUR COLUMNS"

       WRITE(33,4308) 
       WRITE(33,517) "SQUARED RESIDS"
       WRITE(33,517)
       WRITE(33,2517) "GROUP","INCH","OUTCH","INNOC","OUTNOC","TOTCHI"



      TOTCHII=0.0000
      TOTCHIO=0.0000
      DO 334 TG1=1,MAXDEPT
       G1=GORDER(TG1)
       IF ((DEPARTN(G1) .GT. 1) .AND. (DEPARTN(G1) .LE. 50)) THEN
       LCHISQR(G1)= ACHISQRI(G1)+ACHISQRO(G1)+BCHISQRI(G1)+
     C BCHISQRO(G1)
       WRITE(33,2301) G1,ACHISQRI(G1),ACHISQRO(G1),BCHISQRI(G1),
     C BCHISQRO(G1),LCHISQR(G1)
       TOTCHII=TOTCHII+ACHISQRI(G1)+BCHISQRI(G1)
       TOTCHIO=TOTCHIO+ACHISQRO(G1)+BCHISQRO(G1)
       END IF
  334 CONTINUE

       WRITE(33,517) 
       WRITE(33,517) "SQUARE ROOT RESIDS"
       WRITE(33,2517) "GROUP","INCH","OUTCH","INNOC","OUTNOC","TOTCHI"
      DO 3340 TG1=1,MAXDEPT
          G1=GORDER(TG1)
       IF ((DEPARTN(G1) .GT. 1) .AND. (DEPARTN(G1) .LE. 50)) THEN
       WRITE(33,2301) G1, SQRT(ACHISQRI(G1)),
     C SQRT(ACHISQRO(G1)),SQRT(BCHISQRI(G1)),
     C SQRT(BCHISQRO(G1)),SQRT(LCHISQR(G1))
       END IF
 3340 CONTINUE

       WRITE(33,517) 
       WRITE(33,517) "TOTALS"
      WRITE(33,4308)
      WRITE(33,2517) "INCHI","OUTCHI","TOT","NORMSQ","KCHISQR"
      WRITE(33,2518) TOTCHII,TOTCHIO,(TOTCHII+TOTCHIO),
     C FCOMPACT**2,KCHISQR
      WRITE(33,4308)
      WRITE(33,4308) "INCHI  = INCH  + INNOC"
      WRITE(33,4308) "OUTCHI = OUTCH + OUTNOC"
      WRITE(33,4308) "TOT    = INCHI + OUTCHI"
       WRITE(33,4308)  "********************************************"
       WRITE(33,4308) "NOTE THAT TOT = THE CHI-SQUARE VALUE"
       WRITE(33,4308)  "ASSOCIATED WITH THE LOGIT OR LOGLINEAR MODELS"
       WRITE(33,4308)  "DESCRIBED ABOVE"
       WRITE(33,4308)  "********************************************"
      WRITE(33,4308) "NORMSQ = COMPACTNESS * COMPACTNESS"
      WRITE(33,4308) "KCHISQR = SUM OVER ALL GROUPS"
      WRITE(33,4308) "            COMPACTNESS(g)*COMPACTNESS(g)"
       WRITE(33,517) 

       WRITE(33,517) "LRT"
      OPEN(52,file="glim.dat")
       WRITE(33,2517) "GROUP","INCH","OUTCH","INNOC","OUTNOC","TOT*2",
     C  "LAMNDA"

      TOTLAMND=0
      
      DO 3343 TG1=1,MAXDEPT
          G1=GORDER(TG1)


       IF ((DEPARTN(G1) .GT. 1) .AND. (DEPARTN(G1) .LE. 50)) THEN
       TAI=AOBSCONI(G1)/ACMEANI(G1)
       TAO=AOBSCONO(G1)/ACMEANO(G1)
       TBI=BOBSCONI(G1)/BCMEANI(G1)
       TBO=BOBSCONO(G1)/BCMEANO(G1)
       XLAMNDA=
     C 2*(ABS(AOBSCONI(G1))*LOG(TAI)+
     C ABS(AOBSCONO(G1))*LOG(TAO)+
     C ABS(BOBSCONI(G1))*LOG(TBI)+
     C ABS(BOBSCONO(G1))*LOG(TBO))
       TOTLAMND=TOTLAMND+XLAMNDA
       WRITE(33,2301) G1, 
     C ABS(AOBSCONI(G1))*LOG(TAI),
     C ABS(AOBSCONO(G1))*LOG(TAO),
     C ABS(BOBSCONI(G1))*LOG(TBI),
     C ABS(BOBSCONO(G1))*LOG(TBO),
     C XLAMNDA,LAMNDA(G1)
       SAMEG=1
       WRITE(52,10251) G1,SAMEG,SAMEG*G1,AOBSCONI(G1),
     C (AOBSCONI(G1)+BOBSCONI(G1))
       SAMEG=0
       WRITE(52,10251) G1,SAMEG,SAMEG*G1,AOBSCONO(G1),
     C (BOBSCONO(G1)+AOBSCONO(G1))

       END IF
C       GARBAGE=9999999999
 3343 CONTINUE
       WRITE(33,2517) "","","","","","TOTAL"
       WRITE(33,2517) "*********","*********","*********","*********",
     C TOTLAMND
      CLOSE(52)


       WRITE(33,517) "LRT OBJECTIVE FUNCTION"
       WRITE(33,2517) "GROUP","INCH","OUTCH","INNOC","OUTNOC","TOT*2"
      TOTLAMND=0
      DO 7343 TG1=1,MAXDEPT
          G1=GORDER(TG1)

       IF ((DEPARTN(G1) .GT. 1) .AND. (DEPARTN(G1) .LE. 50)) THEN
C       IF (DEPARTN(G1) .GT. 1) THEN
       TAI=AOBSCONI(G1)/ACMEANI(G1)
       TAO=AOBSCONO(G1)/ACMEANO(G1)
       TBI=BOBSCONI(G1)/BCMEANI(G1)
       TBO=BOBSCONO(G1)/BCMEANO(G1)
       XLAMNDA=
     C ABS(2*(ABS(AOBSCONI(G1)*LOG(TAI))+
     C ABS(AOBSCONO(G1)*LOG(TAO))+
     C ABS(BOBSCONI(G1)*LOG(TBI))+
     C ABS(BOBSCONO(G1)*LOG(TBO))))
       TOTLAMND=TOTLAMND+XLAMNDA
       WRITE(33,2301) G1, 
     C ABS(AOBSCONI(G1)*LOG(TAI)),
     C ABS(AOBSCONO(G1)*LOG(TAO)),
     C ABS(BOBSCONI(G1)*LOG(TBI)),
     C ABS(BOBSCONO(G1)*LOG(TBO)),
     C XLAMNDA
       END IF
C       GARBAGE=9999999999
 7343 CONTINUE
       WRITE(33,2517) "","","","","","TOTAL"
       WRITE(33,2517) "*********","*********","*********","*********",
     C TOTLAMND

       
       END IF
       END IF

07474  FORMAT(A27,A12,A4,F10.5)
11221  FORMAT(I5," MEDIATES BETWEEN ",I5," AND ",I5)
08282  FORMAT(" THE LARGEST NUMBER",
     C  " OF CONNECTIONS INITIATED BY ANY ACTOR IS ",F8.4,/,
     C  "WHICH IS ",A,
     C  "THE PRESPECIFIED MAXIMUM OF ",F8.4," .",/,
     C  "THE",A,"WILL BE"
     C  ," USED IN CALCULATING DENSITY MEASURES.")
07761   FORMAT(A12)
07760   FORMAT(A6,I2.2,A4)
09102   FORMAT(251(I4))
09103    FORMAT(A1,I3.3,A5)
09101   FORMAT(251(I4.4,1X))
07513 FORMAT(251("AC",I3.3,","$))
07514  FORMAT ("AC",I3.3)
07511 FORMAT(251("GP",I3.3,","))
27511 FORMAT(I1,",",251(I3.3,","))
37511 FORMAT(251(I3.3,","))
07512 FORMAT(251("AC",I3.3,","))
21091 FORMAT(I1,",",I3,",",I3,",N,TM")
21092 FORMAT(I1,",",I3,",",I3,",N,NM")
04301 FORMAT(20(A))
04302 FORMAT(20(A$))
04303 FORMAT("V",I3.3,",")
04304 FORMAT(") (",I3,"*#7)")

  
   62 FORMAT(I2)
  512 FORMAT(3(A20,1X))
  513 FORMAT(5(A20,1X))
  517 FORMAT(10X,A40)
 2518 FORMAT(20(F10.2))
 2517 FORMAT(20(A10))
 2301 FORMAT(I9,1X,251(F9.2,1X))
  301 FORMAT(251(F7.2))
03311 FORMAT(I6,1X,10(25F7.4,/))

03308 FORMAT(A7$)
04308 FORMAT(A)
    7 FORMAT("MATRIX OF WITHIN GROUP ASSOCIATIONS",/,
     C8X,A20," ORDER")   
   77 FORMAT("MATRIX OF GROUP",A)
  102 FORMAT(I2,251(I1))
  105 FORMAT(251(I1))
02194 FORMAT(25(A10))
02195 FORMAT(A10,25(F10.3))
  193 FORMAT(/,A3,4(2X,A25),/)
  192 FORMAT(I3,6X,I10,6X,6X,I10,6X,12X,F10.5,6X,6X,F10.5,6X,/)
  191 FORMAT(/,"SIMILARITY BETWEEN THE START AND END GROUPS:   ACTUAL 
     C  POSS  STANDARDIZED",/,43X,F7.0,3X,F7.0,3X,F10.5)

03307 FORMAT(50(F7.2$))
03301 FORMAT(I6,1X,10(25F7.2,/))
    8 FORMAT(10(25(A6,1X),/))
   10 FORMAT(A7,10(25(I6,1X),/))
 2302 FORMAT(A4,3X,10(25(I6,1X),/))
  194 FORMAT(/,10(2X,A15),/)
  195 FORMAT(/,10(2X,F15.5),/)   
01954 FORMAT(/,3I15,10(2X,F15.5),/)   


  302 FORMAT(A4,1X,251(F5.2,1X))

  103 FORMAT(251(F7.3))
04404 FORMAT(251(F7.4))
07103 FORMAT(BZ,251(F7.3,1X),BN)
    9 FORMAT(/,"THE CURRENT COMPACTNESS IS",F10.5)
  101 FORMAT(I3,251(I1))
  251 FORMAT(25(F10.5)) 
10251 FORMAT(25I10)
25144 FORMAT(150(F15.5)) 
      RETURN
      END

C      CALL LOGIT(MAXDEPT,DEPARTN,ALLGROUP,REVMAT,NUMTEACH,
C     C RCHOICE,NEWDEPT,MEANWT,VARWT,
C     C FINAL,FSYMMAT,COVMAT,FCOMPACT,BCHISQRI,BCHISQRO,
C     C KCHISQR,MDIAG,VDIAG,GCENTRAL,CDIAG)

      SUBROUTINE LOGIT(MAXDEPT,DEPARTN,NEWMAT,NUMTEACH,
     C RCHOICE,NEWDEPT,MEANWT,VARWT,
     C FINAL,FSYMMAT,FCOMPACT,KCHISQRI,KCHISQRO,
     C KCHISQR,MDIAG,VDIAG,GCENTRAL,CDIAG,FCOMPAC2,
     C FCOMPAC3,FCOMPAC4,ICON,IEXPECT,ISTD,FCOMPAC5,
     C PERGROUP,QUANTYPE,SQUAREIT,EVALFUN,FCOMPAC6,FCOMPAC7,
     C FCOMPAC8,FCOMPAC9,FIXR,PEARSON,EQUIPEAR,
     C LRT,BCENT1,BCENT2,REVERSE,COVMAT,COVMAT2,COVMAT3,COVMAT4,
     C HYPERG,KQMAX,ADDEND,INFILE,SIMO,NLRT,
     C CHIMEANI,CHIMEANO,OBSCONI,OBSCONO)

C LABEL,TITLES,PRINTT,KEEPLIST,
         CHARACTER INFILE*16
      REAL MEANWT(251),VARWT(251),
     C NOB,TWEIGHT,PEARSON,
     C FCOMPAC6,FCOMPAC7,
     C STOTCON,AMEAN,AVAR,MDIAG,VDIAG,CDIAG,
     C FCOMPACT,
     C OPCOMP,XDENOM1
C     C ACHISQRI,ACHISQRO,
C     C KCHISQR,GCENTRAL,
C     C ACOMPAC3,ACOMPAC4,ICON,IEXPECT,ISTD

      
      INTEGER MAXDEPT,DEPARTN(251),PERGROUP,QUANTYPE,SQUAREIT,
     C NUMTEACH,RCHOICE(251),NEWDEPT(251),MACTG,
     C GROUP1,GROUP2,DMEMBERS(251),THISPER,KTEMPS,I,J,
     C BDEPTN(251),SIMO,NO,MULTI,MULTO,
     C FINAL,FSYMMAT,BDENOM,COMPD,
     C OBSCONO(251),OBSCONI(251),NEWMAT(251,251),
     C REVERSE,HYPERG,KQMAX,ADDEND,M,EACHPER,MAXCH,FLAGR

       REAL PCOMP,AVAR2,LRT,FIXR,BETAP,BETAQ,NLRT

       REAL KSIGN,KCHISQR,KCHISQRI(251),KCHISQRO(251),
     C CHIMEANI(251),CHIMEANO(251),CHIVARI(251),CHIVARO(251),
     C FCOMPAC2,FCOMPAC3,FCOMPAC4,UTOTCON,MT,VT,
     C GEXPECT(251),GVAR(251),GCENTRAL(251),
     C GCON(251),IEXPECT(251),ICON(251),ISTD(251),FCOMPAC5,
     C EVALFUN,KSIGN2,ADDIN,PCOMP3,TOTCHOI(251),
     C FCOMPAC8,NCOMPAC8,DCOMPAC8,DCOMPAC9,FCOMPAC9,DENOM1,
     C DENOM2,EQUIPEAR,BLAUDEN1(251),BLAUDEN2(251),BLAUNUM(251),
     C BCENT1(251),BCENT2(251),
     C GRMEAN(50,50),GRVAR(50,50),
     C GTOTCON(50,50)
C                         I       I                I      I       I   
C         CALL HOWMANY (DMEMBERS,DEPARTN(GROUP2),THISPER,NEWMAT,NUMTEACH,
C                           R     I       R     R
C     C                  STOTCON,GROUP2,TWEIGHT,NOB)

      REAL ZCOMPMAT(251,251),
     C COVMAT(50,50),
     C COVMAT2(50,50),
     C COVMAT3(50,50),
     C COVMAT4(50,50)


        REAL MU,VARP,MIBETAP,MIBETAQ,MOBETAP,TCHII,TCHIO,
     C MOBETAQ,RATIO,MIRATIO,MORATIO,ADD3,ADD4,ADD3A,ADD4A

       INTEGER HUBERT(251,251),RESULTM(251,251),ALLGROUP(251,251),
     C KNEWMAT(251,251)
       REAL BLAUC(251,251),DIFF(251,251)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,KNEWMAT,
     C BLAUC,DIFF

C       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,KNEWMAT


       IF (MAXDEPT .LT. 51) THEN
        MULTI=1
        IF (REVERSE .EQ. 1) THEN
        MULTI=-1
        END IF
         
C         BIG LOOP FOR COVMAT
       IF (SIMO .EQ. 0) THEN
       OPEN(52,file='betap')
       OPEN(55,file='wtbetap')
       IF (ADDEND .EQ. 1) THEN
        CALL MOVEEND(52)
        CALL MOVEEND(55)
         END IF 
        END IF
      DO 70002 I=1,NUMTEACH
      DO 70003 J=1,NUMTEACH
      IF (REVERSE .EQ. 1) THEN
       IF (KNEWMAT(I,J) .GT. 0) THEN
         NEWMAT(I,J)=0
       ELSE
         TMW=INT(MEANWT(I))
         IF (MEANWT(I) .LT. 1) THEN
         TMW=1
         END IF
         NEWMAT(I,J)=TMW
       END IF
      ELSE
       NEWMAT(I,J)=KNEWMAT(I,J)
      END IF
70003     CONTINUE
       NEWMAT(I,I)=0
70002     CONTINUE
      IF (REVERSE .EQ. 1) THEN      
      CALL ARCHOICE(NUMTEACH,NEWMAT,RCHOICE,MAXCH,FIXR,FLAGR,
     C MEANWT,VARWT)
      END IF

      NO=1
      NOB=0.0000
      TWEIGHT=1.0000
      DCOMPAC8=0.000
      NCOMPAC8=0.0000
      DCOMPAC9=0.0000
      FCOMPAC5=0.000
      FCOMPAC2=0.000
      EVALFUN=0.000
      MIBETAP=0
      MIBETAQ=0
      MOBETAP=0
      MOBETAQ=0
      MORATIO=0
      MIRATIO=0   

      DO 119 GROUP1=1,MAXDEPT
        BLAUDEN1(GROUP1)=0.000
        BLAUDEN2(GROUP1)=0.000
        BLAUNUM(GROUP1)=0.000
        BCENT1(GROUP1)=0.000
        BCENT2(GROUP1)=0.0000
        CHIMEANI(GROUP1)=0.000
        CHIVARI(GROUP1)=0.0000
        CHIMEANO(GROUP1)=0.000
        CHIVARO(GROUP1)=0.000
        OBSCONO(GROUP1)=0.000
        OBSCONI(GROUP1)=0.000
        TOTCHOI(GROUP1)=0.000
        BDEPTN(GROUP1)=0
C         FOR EACH GROUP
        DO 121 GROUP2=1,MAXDEPT
           GRMEAN(GROUP1,GROUP2)=0.000
           GRVAR(GROUP1,GROUP2)=0.000
           GTOTCON(GROUP1,GROUP2)=0.000
           COVMAT2(GROUP1,GROUP2)=0.000
           COVMAT(GROUP1,GROUP2)=0.0000
           COVMAT3(GROUP1,GROUP2)=0.0000
            BETAP=0
            BETAQ=0
            MU=0
            VARP=0 
            MACTG=0
C
C          AND EVERY OTHER GROUP
C          MAKE VECTOR OF OTHER MEMBERS
C
         TOTCHOI(GROUP2)=0.000
         DO 122 M=1,DEPARTN(GROUP2)
          DMEMBERS(M)=ALLGROUP(GROUP2,M)
         TOTCHOI(GROUP2)=TOTCHOI(GROUP2)+RCHOICE(DMEMBERS(M))*
     C   MEANWT(DMEMBERS(M))
  122    CONTINUE

         DO 123 EACHPER=1,DEPARTN(GROUP1)
          THISPER=ALLGROUP(GROUP1,EACHPER)

C        ZZZ

         CALL HOWMANY (DMEMBERS,DEPARTN(GROUP2),THISPER,NEWMAT,NUMTEACH,
     C                  STOTCON,GROUP2,TWEIGHT,NOB,UTOTCON,MT,VT)
       IF ((REVERSE .EQ. 0) .AND. (SIMO .EQ. 0)) THEN
         WRITE(55,251) NUMTEACH,THISPER,GROUP1,GROUP2,MT,VT,
     C    STOTCON,UTOTCON,RCHOICE(THISPER)
       END IF
         BDENOM=DEPARTN(GROUP2)
         IF (GROUP1 .EQ. GROUP2) THEN
         BDENOM=BDENOM-1.
         END IF
         BDENOM=RCHOICE(THISPER) 
         IF (RCHOICE(THISPER) .GT. 0) THEN
          MACTG=MACTG+1.0
         MU=MU+UTOTCON/BDENOM
         VARP=VARP+(UTOTCON/BDENOM)**2
          END IF
         GTOTCON(GROUP1,GROUP2)=GTOTCON(GROUP1,GROUP2)+STOTCON
         IF (GROUP1 .EQ. GROUP2) THEN
         OBSCONI(GROUP1)=OBSCONI(GROUP1)+STOTCON
         ELSE
         OBSCONO(GROUP1)=OBSCONO(GROUP1)+STOTCON
         END IF

         KTEMPS=DEPARTN(GROUP2)+1
         IF (GROUP1 .EQ. GROUP2) THEN
          KTEMPS=KTEMPS-1
         END IF
         IF (RCHOICE(THISPER) .GT. 0) THEN
C      SUBROUTINE DISTRIB (THISR,THISWM,THISWV,MEAN,TVAR,KGRPSIZE,
C     C           TOTSIZE,THISPER,AVAR2)

         CALL DISTRIB (RCHOICE(THISPER),MEANWT(THISPER),
     C    VARWT(THISPER),AMEAN,AVAR,KTEMPS,NUMTEACH,THISPER,AVAR2,
     C    HYPERG)
         GRMEAN(GROUP1,GROUP2)=GRMEAN(GROUP1,GROUP2)+AMEAN
         GRVAR(GROUP1,GROUP2)=GRVAR(GROUP1,GROUP2)+AVAR

          OPCOMP=0
          PCOMP=0
         IF (AVAR .GT. 0) THEN
          OPCOMP=(STOTCON-AMEAN)/(AVAR**.5)
         ELSE 
         OPCOMP=0
         END IF
         IF ((QUANTYPE .EQ. 1) .OR. (QUANTYPE .GE. 4)) THEN
         PCOMP=OPCOMP
         END IF

         IF ((QUANTYPE .EQ. 2)
     C            .AND. (AMEAN .GT. 0)) THEN
         PCOMP=(STOTCON-AMEAN)/AMEAN**.5
         END IF
         IF (QUANTYPE .EQ. 3) THEN
         KSIGN=STOTCON/AMEAN
         IF (KSIGN .GT. 0) THEN
         PCOMP=2*STOTCON*LOG(KSIGN)
         ELSE
         PCOMP=0
         END IF
         END IF

         IF (SQUAREIT .EQ. 1) THEN
          IF (PCOMP .GT. 0) THEN
            KSIGN=1
          ELSE
            KSIGN=-1
          END IF
          PCOMP=KSIGN*PCOMP*PCOMP
         END IF
         IF (PERGROUP .EQ. 1) THEN
         IF (KTEMPS .GT. 1) THEN
         PCOMP=PCOMP/(KTEMPS-1.)
         ELSE
         PCOMP=0
         END IF
         END IF
         
         COVMAT2(GROUP1,GROUP2)=COVMAT2(GROUP1,GROUP2) + PCOMP

C         WRITE(555,251) THISPER,GROUP2,PCOMP 
         IF (GROUP1 .EQ. GROUP2) THEN
          EVALFUN=EVALFUN+PCOMP
          IEXPECT(THISPER)=AMEAN
          ISTD(THISPER)=AVAR**.5
          ICON(THISPER)=STOTCON
         FCOMPAC2=FCOMPAC2+ OPCOMP
         IF (DEPARTN(GROUP1) .GT. 1) THEN
         FCOMPAC5=FCOMPAC5+OPCOMP/(DEPARTN(GROUP1)-1.)
         END IF
         CHIMEANI(GROUP1)=CHIMEANI(GROUP1)+AMEAN
         CHIVARI(GROUP1)=CHIVARI(GROUP1)+AVAR2
         ELSE
         CHIMEANO(GROUP1)=CHIMEANO(GROUP1)+AMEAN
         CHIVARO(GROUP1)=CHIVARO(GROUP1)+AVAR2
         END IF

         END IF

  123   CONTINUE
        VARP=(VARP-MU**2/MACTG)/(MACTG-1.)
        MU=MU/MACTG
C        VARP=MU*(1.-MU)
        IF (MU .LT. .01) THEN
        MU=.01
         END IF
         IF (MU .GT. .99) THEN
          MU =.99
           END IF

C         FMU=(1.0-MU)/MU
C         BETAP=(1+FMU)**2
C         BETAP=BETAP*VARP
C         TBETAP=BETAP
C         BETAP=FMU/TBETAP
C         BETAP=BETAP-1.0
C         FMU=1.0+FMU
C         TFMU=FMU
C         FMU=1.0/TFMU 
C          TBETAP=BETAP
C         BETAP=TBETAP*FMU
        BETAP=(((1.0-MU)/MU)/(VARP*(1.0+(1.0-MU)/MU)**2) -1.0)*
     C  (1.0/(1.0+(1.0-MU)/MU))
C         FMU=(1.0-MU)
C         FMU=FMU/MU
        BETAQ=BETAP*(1.0-MU)/MU
        RATIO=0
        IF ((BETAP .GT. 0) .AND. (BETAQ .GT. 0)) THEN
        RATIO=BETAP/BETAQ
        END IF
        IF (SIMO .EQ. 0) THEN
        WRITE(52,544) INFILE,1,GROUP1,GROUP2,MU,VARP,BETAP,BETAQ,RATIO
        END IF
        IF (GROUP1 .EQ. GROUP2) THEN
        MIRATIO=MIRATIO+RATIO/MAXDEPT 
        MIBETAP=MIBETAP+BETAP/MAXDEPT 
        MIBETAQ=MIBETAQ+BETAQ/MAXDEPT
        ELSE
        MORATIO=MORATIO+RATIO/MAXDEPT
        MOBETAP=MOBETAP+BETAP/(MAXDEPT*(MAXDEPT-1.0)) 
        MOBETAQ=MOBETAQ+BETAQ/(MAXDEPT*(MAXDEPT-1.0)) 
        END IF 
  121   CONTINUE
  119  CONTINUE
        IF (SIMO .EQ. 0) THEN
        WRITE(52,544) INFILE,2,MIBETAP,MIBETAQ,MIRATIO,MOBETAP,MOBETAQ,
     C  MORATIO
        END IF
       DO 678 I=1,NUMTEACH
         IF (RCHOICE(I) .GT. 0) THEN 
          BDEPTN(NEWDEPT(I))=BDEPTN(NEWDEPT(I))+1.0000
         END IF
00678     CONTINUE

       MDIAG = 0.0000
       VDIAG = 0.0000
       CDIAG=0.0000
       KCHISQR=0.000
       FCOMPAC3=0.00000
       FCOMPAC4=0.00000
         NLRT=0.00
         LRT=0.00
         PEARSON=0.00
         EQUIPEAR=0.00

       FCOMPAC6=0.00000
       FCOMPAC7=0.00000
       FCOMPAC8=0.00000
       DO 221 I=1,MAXDEPT
         KCHISQRI(I)=0.000
         KCHISQRO(I)=0.000
         GEXPECT(I)=0.000
         GVAR(I)=0.000
         GCON(I)=0.000
         MDIAG=MDIAG+GRMEAN(I,I)
         CDIAG=CDIAG+GTOTCON(I,I)
         VDIAG=VDIAG+GRVAR(I,I)
        DO 222 J=1,MAXDEPT

        IF (((QUANTYPE .EQ. 1) .OR. (QUANTYPE .GE. 4))
     C  .AND. (GRVAR(I,J) .GT. 0))  THEN
         COVMAT(I,J)=(GTOTCON(I,J)-GRMEAN(I,J))/
     C                (GRVAR(I,J))**.5
        END IF
        IF ((QUANTYPE .EQ. 2) .AND. (GRMEAN(I,J) .GT. 0)) THEN
         COVMAT(I,J)=(GTOTCON(I,J)-GRMEAN(I,J))/
     C                (GRMEAN(I,J))**.5
        END IF
        IF (QUANTYPE .EQ. 3) THEN
        KSIGN=GTOTCON(I,J)/GRMEAN(I,J)
        IF (KSIGN .GT. 0) THEN
         COVMAT(I,J)=2*GTOTCON(I,J)*LOG(KSIGN)
        ELSE
         COVMAT(I,J)=0
         END IF
        END IF
        IF (SQUAREIT .EQ. 1) THEN
          IF (COVMAT(I,J) .GT. 0) THEN
            KSIGN=1
          ELSE
            KSIGN=-1
          END IF
        COVMAT(I,J)=KSIGN*COVMAT(I,J)**2
        END IF
        IF ((PERGROUP .EQ. 1) .AND. (DEPARTN(I) .GT. 0)) THEN
        COVMAT(I,J)=COVMAT(I,J)/DEPARTN(I)
        END IF
          IF (FIXR .LT. DEPARTN(J)) THEN
          DENOM1=BDEPTN(I)*FIXR
          ELSE
          DENOM1=BDEPTN(I)*DEPARTN(J)
          END IF
          IF (FIXR .LT. DEPARTN(I)) THEN
          DENOM2=BDEPTN(J)*FIXR
          ELSE
          DENOM2=BDEPTN(J)*DEPARTN(I)
          END IF
          BLAUDEN1(I)=BLAUDEN1(I)+DENOM1+DENOM2
          BLAUNUM(I)=BLAUNUM(I)+GTOTCON(I,J)+GTOTCON(J,I)
          BLAUDEN2(I)=BLAUDEN2(I)+(TOTCHOI(I)+TOTCHOI(J))
          BLAUDEN1(J)=BLAUDEN1(J)+DENOM1+DENOM2
          BLAUNUM(J)=BLAUNUM(J)+GTOTCON(I,J)+GTOTCON(J,I)
          BLAUDEN2(J)=BLAUDEN2(I)+(TOTCHOI(I)+TOTCHOI(J))
          COVMAT3(I,J)=GTOTCON(I,J)/DENOM1
          COVMAT4(I,J)=GTOTCON(I,J)/TOTCHOI(I)
          IF (DENOM1 .LE. 0) THEN
          COVMAT3(I,J)=0
          END IF
          IF (ABS(TOTCHOI(I)) .LT. .000001) THEN
          COVMAT4(I,J)=0
          END IF
  222    CONTINUE
         PCOMP3=0.0000
         IF ((DEPARTN(I) .GT. 1) .AND. (TOTCHOI(I) .GT. 0))  THEN
         DENOM1=DEPARTN(I)-1.
         IF (FIXR .LT. DENOM1) THEN
         DENOM1=FIXR
         END IF
         PCOMP3=(GTOTCON(I,I)/(BDEPTN(I)*(DENOM1)))
         COVMAT3(I,I)=PCOMP3
         IF (ABS(BDEPTN(I)*DENOM1) .LT. 0000001) THEN
         COVMAT3(I,I)=0
         END IF
         COVMAT4(I,I)=GTOTCON(I,I)/TOTCHOI(I)
          IF (ABS(TOTCHOI(I)) .LT. .000001) THEN
         COVMAT4(I,I)=0
         END IF
         DCOMPAC8=DCOMPAC8+(TOTCHOI(I))**(SQUAREIT+1.)
          COMPD=DEPARTN(I)-1.
          IF (FIXR .LT. COMPD) THEN
          XDENOM1=BDEPTN(I)*FIXR
          ELSE
          XDENOM1=BDEPTN(I)*COMPD
          END IF
         DCOMPAC9=DCOMPAC9+(XDENOM1)
         NCOMPAC8=NCOMPAC8+(GTOTCON(I,I))**(SQUAREIT+1.)
         END IF

         IF (GRVAR(I,I) .GT. 0) THEN
C         EQUIPEAR=(GTOTCON(I,I)-GRMEAN(I,I))**2/GRVAR(I,I)
         IF ((QUANTYPE .EQ. 1) .OR. (QUANTYPE .GE. 4)) THEN
         COVMAT(I,I)=(GTOTCON(I,I)-GRMEAN(I,I))/GRVAR(I,I)**.5
         END IF
         END IF

         IF ((QUANTYPE .EQ. 2) .AND. (GRMEAN(I,I) .GT. 0)) THEN
         COVMAT(I,I)=(GTOTCON(I,I)-GRMEAN(I,I))/GRMEAN(I,I)**.5
         END IF

         IF (QUANTYPE .EQ. 3) THEN
         KSIGN=GTOTCON(I,I)/GRMEAN(I,I)
         IF (KSIGN .GT. 0) THEN
         COVMAT(I,I)=2*GTOTCON(I,I)*LOG(GTOTCON(I,I)/GRMEAN(I,I))
         END IF
         END IF
         IF (SQUAREIT .EQ. 1) THEN
          IF (COVMAT(I,I) .GT. 0) THEN
            KSIGN=1
          ELSE
            KSIGN=-1
          END IF
         COVMAT(I,I)=KSIGN*COVMAT(I,I)**2
         END IF
         IF (PERGROUP .EQ. 1) THEN
         COVMAT(I,I)=COVMAT(I,I)/(DEPARTN(I)-1.)
         END IF

          DO 2512 J=1,MAXDEPT
          IF (J .NE. I) THEN
          GEXPECT(I)=GEXPECT(I)+GRMEAN(I,J)
          GVAR(I)=GVAR(I)+GRVAR(I,J)
          GCON(I)=GCON(I)+GTOTCON(I,J)
          END IF
02512      CONTINUE
          GCENTRAL(I)=0.000
         IF (GVAR(I) .GT. 0) THEN
         GCENTRAL(I)=(GCON(I)-GEXPECT(I))/(GVAR(I)**.5)
         END IF
        IF (DEPARTN(I) .GT. 1) THEN
         ADD3=0
         ADD4=0
         ADD3A=0
         ADD4A=0
          ADDIN=0
          IF (CHIMEANI(I) .GT. 0) THEN
          ADDIN=ADDIN+1
         ADD3=ADD3+
     C  ((OBSCONI(I)-CHIMEANI(I))/CHIMEANI(I)**.5)**(SQUAREIT + 1.)
         ADD3A=ADD3
          END IF
         IF (CHIMEANO(I) .GT. 0) THEN
          ADDIN=ADDIN+1
          ADD3=ADD3+
     C  ((OBSCONO(I)-CHIMEANO(I))/CHIMEANO(I)**.5)**(SQUAREIT + 1.)
         END IF
C         IF (ADDIN .GT. 0) THEN
C         ADD3=ADD3/ADDIN
C          END IF
        TCHII=CHIMEANI(I)
        IF (TCHII .LT. .0001) THEN
        TCHII=.0001
        END IF
        TCHIO=CHIMEANO(I)
        IF (TCHIO .LT. .0001) THEN
        TCHIO=.0001
        END IF
 
        KSIGN=OBSCONI(I)/TCHII
        KSIGN2=OBSCONO(I)/TCHIO
        IF ((KSIGN .GT. 0) .AND. (KSIGN2 .GT. 0)) THEN
        ADD4=
     C 2*((OBSCONI(I) * LOG(KSIGN))**(SQUAREIT+1.)+
     C    (OBSCONO(I) * LOG(KSIGN2))**(SQUAREIT+1.))
         END IF
        IF (KSIGN .GT. 0) THEN
        NLRT=NLRT+
     C 2*MULTI*(OBSCONI(I) * LOG(KSIGN))
        LRT=LRT+
     C 2*(OBSCONI(I) * LOG(KSIGN))
        ADD4A=
     C 2*((OBSCONI(I) * LOG(KSIGN))**(SQUAREIT+1.))
        END IF
        IF (KSIGN2 .GT. 0) THEN
        LRT=LRT+
     C  2*(OBSCONO(I) * LOG(KSIGN2))
        NLRT=NLRT+
     C -2*MULTI*(OBSCONO(I) * LOG(KSIGN2))
        END IF

        IF (PERGROUP .EQ. 1) THEN
         ADD3=ADD3/(DEPARTN(I)-1.)
         ADD4=ADD4/(DEPARTN(I)-1.)
         ADD3A=ADD3A/(DEPARTN(I)-1.)
         ADD4A=ADD4A/(DEPARTN(I)-1.)
        END IF
         FCOMPAC6=FCOMPAC6+ADD3A
         FCOMPAC7=FCOMPAC7+ADD4A
         FCOMPAC3=FCOMPAC3+ADD3
         FCOMPAC4=FCOMPAC4+ADD4
        KCHISQRI(I)=((OBSCONI(I)-CHIMEANI(I))**2)/CHIMEANI(I) +
     C  KCHISQRI(I)
        KCHISQRO(I)=((OBSCONO(I)-CHIMEANO(I))**2)/CHIMEANO(I) +
     C  KCHISQRO(I)
        KCHISQR=KCHISQR+COVMAT(I,I)*COVMAT(I,I)

        IF ((CHIMEANI(I) .GT. 0) .AND. (CHIVARI(I) .GT. 0)) THEN
        PEARSON=PEARSON+((OBSCONI(I)-CHIMEANI(I))**2)/CHIMEANI(I) 
        EQUIPEAR=EQUIPEAR+((OBSCONI(I)-CHIMEANI(I))**2)/CHIVARI(I)
        END IF

        IF ((CHIMEANO(I) .GT. 0) .AND. (CHIVARO(I) .GT. 0)) THEN
        PEARSON=PEARSON+((OBSCONO(I)-CHIMEANO(I))**2)/CHIMEANO(I) 
        EQUIPEAR=EQUIPEAR+((OBSCONO(I)-CHIMEANO(I))**2)/CHIVARI(I)
c        INTENTIONAL USE OF CHIVARI INSTEAD OF CHIVARO. 
        END IF

        END IF
  221  CONTINUE
       DO 876 I=1,MAXDEPT
       IF (BLAUNUM(I) .GT. 0) THEN
       IF (BLAUDEN1(I) .GT. 0) THEN
       BCENT1(I)=BLAUNUM(I)/BLAUDEN1(I)
       END IF
       IF (BLAUDEN2(I) .GT. 0) THEN
       BCENT2(I)=BLAUNUM(I)/BLAUDEN2(I)
       END IF
       END IF
C       DO 877 J=1,(I-1.)
C       COVMAT(J,I)=COVMAT(I,J)
C       COVMAT2(J,I)=COVMAT2(I,J)
C       COVMAT3(J,I)=COVMAT3(I,J)
C       COVMAT4(J,I)=COVMAT4(I,J)
C00877   CONTINUE
00876   CONTINUE

       FCOMPAC9=NCOMPAC8/DCOMPAC9
       FCOMPAC8=NCOMPAC8/DCOMPAC8
       FCOMPACT=(CDIAG-MDIAG)/VDIAG**.5
       CLOSE(55)
       END IF
   62 FORMAT(I2)
  512 FORMAT(3(A20,1X))
  513 FORMAT(10X,3(A20,1X))
  517 FORMAT(10X,A40)
    7 FORMAT("RESULTING MATRIX OF WITHIN GROUP CONNECTIONS",/,
     C8X,A20," ORDER")   
   77 FORMAT("MATRIX OF GROUP ASSOCIATIONS",/)
   10 FORMAT(5X,251(I5,1X))
  102 FORMAT(I2,251(I1))
  105 FORMAT(251(I1))
  193 FORMAT(/,A3,4(2X,A25),/)
  194 FORMAT(/,10(2X,A15),/)
  195 FORMAT(/,10(2X,F15.5),/)   
  192 FORMAT(I3,6X,I10,6X,6X,I10,6X,12X,F10.5,6X,6X,F10.5,6X,/)
  191 FORMAT(/,"SIMILARITY BETWEEN THE START AND END GROUPS:   ACTUAL 
     C  POSS  STANDARDIZED",/,45X,I10,I10,F10.5)
    8 FORMAT(5X,251(A5,1X))
  301 FORMAT(I4,1X,251(F5.2,1X))
  302 FORMAT(A4,1X,251(F5.2,1X))
  103 FORMAT(20(F7.3))
    9 FORMAT(/,"THE FINAL COMPACTNESS IS",F10.5)
  101 FORMAT(I2,251(I1))
  251 FORMAT(25(F10.5)) 
 0544 FORMAT(A16,25(F10.5)) 
      RETURN
      END

      SUBROUTINE SLOGIT(MAXDEPT,DEPARTN,NUMTEACH,
     C NEWDEPT,COVMAT,FCOMPACT,TSS,WBSS,GROUPMAT,
     C ROWWT,COLWT,KQMAX)

C LABEL,TITLES,PRINTT,KEEPLIST,

      REAL SS(251,251),GROUPMAT(251,251),
     C GRMEAN(251,251),GRVAR(251,251),NOB,TWEIGHT,
     C STOTCON,AMEAN,AVAR,MDIAG,VDIAG,CDIAG,
     C COVMAT(50,50),COLWT,ROWWT,
     C FCOMPACT,HUBSIM,STDHS,OUTOF,ZABS,PCON,
     C CHNGSIM,CHNGSTD,WBSS,TSS,WSS
      
      INTEGER MAXDEPT,DEPARTN(251),KQMAX,
     C NUMTEACH,RCHOICE(251),NEWDEPT(251),
     C GROUP1,GROUP2,DMEMBERS(251),THISPER,KTEMPS,I,J,
     C LMAXDEPT,LGROUP(251),WANTSS,KDEPT,DMAXD,
     C K,LENGTH,NO2,G2,G3,NEWORDER(251),PRINTT,FINAL,FSYMMAT,
     C SAMEG,I2,OBSCONO(251),OBSCONI(251),DUMDEPT(251),THISNI,THISNJ

       REAL ALLCON,CHNGCON,PC,DGROUPMA(251,251)

       REAL KSIGN,KCHISQR,KCHISQRI(251),KCHISQRO(251),
     C CHIMEANI(251),CHIMEANO(251),CHIVARI(251),CHIVARO(251),
     C TOTCHII,TOTCHIO,GEXPECT(251),GVAR(251),GCENTRAL(251),GCON(251)
       REAL ZCOMPMAT(251,251)
       INTEGER HUBERT(251,251),RESULTM(251,251),ALLGROUP(251,251),
     C NEWMAT(251,251)
       REAL BLAUC(251,251),DIFF(251,251)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF

C       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT

      NO=1
      NOB=0.0000
      TWEIGHT=1.0000
      WANTSS=1
      
      DO 11 I=1,MAXDEPT
        DO 13 J=1,MAXDEPT
       COVMAT(I,J)=0
00013 CONTINUE 
      DO 12 J=1,NUMTEACH
       GRMEAN(I,J)=0.0
       GRVAR(I,J)=0.0
00012   CONTINUE
00011   CONTINUE
      
      CALL GETMEAN(NEWDEPT,NUMTEACH,GROUPMAT,MAXDEPT,
     C DEPARTN,WANTSS,SS,WSS,ROWWT,COLWT)

       DO 98 G1=1,MAXDEPT
       DO 998 G2=1,G1
       DO 99 J=1,NUMTEACH
       COVMAT(G1,G2)=COVMAT(G1,G2)+
     C  (GROUPMAT(G1,J)-GROUPMAT(G2,J))**2
00099   CONTINUE
00998    CONTINUE
00098     CONTINUE

       WBSS=0.00       
      DO 04 I=1,MAXDEPT
      DO 05 J=1,NUMTEACH
       GRMEAN(I,NEWDEPT(J))=GRMEAN(I,NEWDEPT(J))+GROUPMAT(I,J)
       GRVAR(I,NEWDEPT(J))=SS(I,NEWDEPT(J))+SS(I,J)
        WBSS=WBSS+SS(I,J)
00005   CONTINUE
00004   CONTINUE

       DO 06 I=1,MAXDEPT
       COVMAT(I,I)=GRVAR(I,I)/(DEPARTN(I)*MAXDEPT)
00006     CONTINUE

        DO 008 I=1,NUMTEACH
         DUMDEPT(I)=1
00008     CONTINUE
        
         TSS=0
         DMAXD=1
         KDEPT=DEPARTN(1)
         DEPARTN(1)=NUMTEACH
      CALL GETMEAN(DUMDEPT,NUMTEACH,DGROUPMA,DMAXD,
     C DEPARTN,WANTSS,SS,WSS,1.0,0.0)
          DEPARTN(1)=KDEPT
          DO 010 J=1,NUMTEACH
          TSS=TSS+SS(1,J)
00010      CONTINUE

           FCOMPACT=1-WBSS/TSS

       
   62 FORMAT(I2)
  512 FORMAT(3(A20,1X))
  513 FORMAT(10X,3(A20,1X))
  517 FORMAT(10X,A40)
   77 FORMAT("MATRIX OF GROUP ASSOCIATION",/)
  102 FORMAT(I2,251(I1))
  105 FORMAT(251(I1))
  193 FORMAT(/,A3,4(2X,A25),/)
  194 FORMAT(/,10(2X,A15),/)
  195 FORMAT(/,10(2X,F15.5),/)   
  192 FORMAT(I3,6X,I10,6X,6X,I10,6X,12X,F10.5,6X,6X,F10.5,6X,/)
  191 FORMAT(/,"SIMILARITY BETWEEN THE START AND END GROUPS:   ACTUAL 
     C  POSS  STANDARDIZED",/,45X,I10,I10,F10.5)
  301 FORMAT(I4,1X,251(F5.2,1X))
  302 FORMAT(A4,1X,251(F5.2,1X))
  103 FORMAT(20(F7.3))
  101 FORMAT(I2,251(I1))
  251 FORMAT(25(F10.5)) 
      RETURN
      END
      SUBROUTINE KEVAL(MAXDEPT,DEPARTN,NUMTEACH,
     C RCHOICE,OLDDEPT,MEANWT,VARWT,PRINTT,
     C FINAL,FSYMMAT,PRINTO,FCOMPACT,CHNGSIM,NEVAL,BASEVAL,
     C NBESTDEP,STRUCTEQ,NUMRES,TRYDEPT,QUANTYPE,TOPVAL,NEWGRPS,
     C PERGROUP,SQUAREIT,FIXR,HIWTEVAL,HYPERG,ROWWT,COLWT,KQMAX,
     C INFILE,SIMO)

C      
C     C 
C     C COVMAT,ACHISQRI,ACHISQRO,
C     C KCHISQR,MDIAG,VDIAG,GCENTRAL,CDIAG,COVMAT2,FCOMPAC2,
C     C ACOMPAC3,ACOMPAC4,ICON,IEXPECT,ISTD,FCOMPAC5)

        REAL FCOMPAC2,FCOMPAC5,
     C KCHISQR,ACOMPAC3,ACOMPAC4,ACHISQRI(251),ACHISQRO(251),
     C GCENTRAL(251),ICON(251),ISTD(251),IEXPECT(251),
     C FIXR,INDEPT,GLAMNDA,TABLE1A,TABLE1B,TABLE1C,TABLE1D
          CHARACTER INFILE*16
C       CHARACTER LABEL(251)*9,TITLES(3)*20

C      CALL HUBVAR(HUBERT,RESULTM,NUMTEACH,CHNGSIM,CHNGSTD,CHNGCON)
C      CALL HUBVAR(NEWMAT,RESULTM,NUMTEACH,HUBSIM,STDHS,ALLCON)

      REAL MEANWT(251),VARWT(251),AVAR2,
     C NOB,TWEIGHT,BESTVAL,
     C STOTCON,AMEAN,AVAR,PEARSON,EQUIPEAR,
     C FCOMPACT,HUBSIM,STDHS,OUTOF,ZABS,PCON,
     C CHNGSIM,CHNGSTD,TOPVAL
      
      INTEGER
     C  MAXDEPT,DEPARTN(251),HYPERG,REVERSE,SIMO,
     C NUMTEACH,RCHOICE(251),NEWDEPT(251),
     C GROUP1,GROUP2,DMEMBERS(251),THISPER,KTEMPS,I,J,
     C LMAXDEPT,STRUCTEQ,NEWGRPS,
     C K,LENGTH,NO2,G2,G3,PRINTT,FINAL,FSYMMAT,OLDDEPT(251),
     C NEVAL,ENUFVAL,NBESTDEP(251),FC,PEDRO1,PRINTO(100),
     C LGROUP(251),RP1,RP2,NUMRES,IMAX(1000),TRYDEPT(251),
     C ORDER(1000),NUMPRED,CRES,QUANTYPE,PERGROUP,SQUAREIT,
     C BOBSCONI(251),BOBSCONO(251)

       REAL ALLCON,CHNGCON,PC,UCUT,BASEVAL,INCREM,TM,TSTD,
     C OBFUN(1000),PREDICT(1000,1)
       DOUBLE PRECISION USEED,QSEED,
     C ISEEDS(1000)
       REAL ICUTS(1000),MDIAG,VDIAG,CDIAG,
     C BCMEANI(251),BCMEANO(251)

C        CALL ASSIGN2(NUMTEACH[I] ,NEWDEPT[I251] ,MAXDEPT[I] 
C       ,ALLGROUP[I251,251]
C    C  ,DEPARTN[I251],RCHOICE[I251],ALLGROUR[I251,251]
C     ,MEANWT[R251] ,VARWT[R251],
C     C  1,2)



       REAL KSIGN,TEMP(2),XYBAR(2),A(3),ALFA,ANOVA(14),B(2,7),
     C VARB(1),BETA(2),RES(1000,4),MYRES(1000),TCOMPACT,
     C TSS,WSS,EVALFUN,FCOMPAC6,FCOMPAC7,FCOMPAC8,FCOMPAC9,
     C LRT,BCENT1(251),BCENT2(251),TINCREM,NLRT,
     C HIWTEVAL,REAL
       INTEGER NBR(6),IER,IB,IH(2),HOLDOFF,NUMIN,HIGHRES,XTCOUNT,
     C KQMAX
C       PARAMETER (PAR1=11)
C       PARAMETER (CPAR1=2**PAR1)
       REAL ZCOMPMAT(251,251),
     C COVMAT(50,50),COVMAT2(50,50),
     C COVMAT3(50,50),COVMAT4(50,50)

       INTEGER HUBERT(251,251),RESULTM(251,251),ALLGROUP(251,251),
     C NEWMAT(251,251)

C      HUBERT AND RESULTM SWITCHED INTENTIONALLY
       REAL BLAUC(251,251),DIFF(251,251)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF

C       COMMON ZCOMPMAT,ALLGROUP,RESULTM,HUBERT,NEWMAT


       BESTVAL=0.000
      ENUFVAL=1
      INCREM=(TOPVAL-BASEVAL)/(NEVAL)
      QSEED=5163516.D0
      USEED=4161416.D0

      CRES=1
      SUMWT=0
      DO 987 I=1,NEVAL
      SUMWT=SUMWT+INCREM/I
00987  CONTINUE
      UCUT=TOPVAL
C      SUMWT=HIWT*(NEVAL+1)*NEVAL/2
      DO WHILE (ENUFVAL .LE. (NEVAL+1.))
      IF (ENUFVAL .GT. 1) THEN
      TINCREM=(INCREM/(ENUFVAL-1.))/SUMWT
      ELSE
      TINCREM=0
       END IF
      ORDER(ENUFVAL)=ENUFVAL
      PEDRO1=1
      UCUT=UCUT-TINCREM
        ISEEDS(ENUFVAL)=USEED
        ICUTS(ENUFVAL)=UCUT
        IMAX(ENUFVAL)=MAXDEPT
      CALL RESORTO3(OLDDEPT,NUMTEACH,QSEED,USEED,UCUT,NEWDEPT,
     C MAXDEPT,NEWGRPS)

C      SUBROUTINE RESORTO3 (ELEMENTS,NUMELEM,RSEED,USEED,UCUT,
C     C  NELEMENT,TMAXDEPT,NEWGRPS)

        CALL ASSIGN2(NUMTEACH,NEWDEPT,MAXDEPT,
     C  DEPARTN)
        CALL REMOVEO4(MAXDEPT,DEPARTN,LGROUP,1,CUTOFF,0)
        NUMIN=0
        DO 56 RP1=1,NUMTEACH
          IF (DEPARTN(NEWDEPT(RP1)) .GT. 1) THEN
          NUMIN=NUMIN+1
          END IF
          NEWDEPT(RP1)=LGROUP(NEWDEPT(RP1))
00056    CONTINUE

        CALL ASSIGN2(NUMTEACH,NEWDEPT,MAXDEPT,
     C  DEPARTN) 



C       MDIAG = 0
C       VDIAG = 0
C       CDIAG=0
C       IF (MAXDEPT .GT. 250) THEN 
C         STOP
C       END IF
C
C      NOB=0.0000
C      TWEIGHT=1.0000
C
       REVERSE=0
      IF (STRUCTEQ .EQ. 1) THEN
      CALL SLOGIT(MAXDEPT,DEPARTN,NUMTEACH,
     C NEWDEPT,
     C COVMAT,FCOMPACT,
     C TSS,WSS,GROUPMAT,
     C ROWWT,COLWT,KQMAX)
C      SUBROUTINE SLOGIT(MAXDEPT,DEPARTN,NUMTEACH,
C     C NEWDEPT,COVMAT,FCOMPACT,TSS,WBSS,GROUPMAT,
C     C ROWWT,COLWT)

       FCOMPACT=(TSS-WSS)*(NUMTEACH**2-MAXDEPT**2)/
     C ((MAXDEPT**2 -1)*WSS)

      CALL LOGIT(MAXDEPT,DEPARTN,NEWMAT,NUMTEACH,
     C RCHOICE,NEWDEPT,MEANWT,VARWT,
     C FINAL,FSYMMAT,TCOMPACT,ACHISQRI,ACHISQRO,
     C KCHISQR,MDIAG,VDIAG,GCENTRAL,CDIAG,FCOMPAC2,
     C ACOMPAC3,ACOMPAC4,ICON,IEXPECT,ISTD,FCOMPAC5,
     C PERGROUP,QUANTYPE,SQUAREIT,EVALFUN,FCOMPAC6,FCOMPAC7,
     C FCOMPAC8,FCOMPAC9,FIXR,PEARSON,EQUIPEAR,
     C LRT,BCENT1,BCENT2,REVERSE,XCOVMAT,COVMAT2,COVMAT3,COVMAT4,
     C HYPERG,KQMAX,PRINTO(27),INFILE,SIMO,NLRT,
     C BCMEANI,BCMEANO,BOBSCONI,BOBSCONO)
       ELSE

      CALL LOGIT(MAXDEPT,DEPARTN,NEWMAT,NUMTEACH,
     C RCHOICE,NEWDEPT,MEANWT,VARWT,
     C FINAL,FSYMMAT,TCOMPACT,ACHISQRI,ACHISQRO,
     C KCHISQR,MDIAG,VDIAG,GCENTRAL,CDIAG,FCOMPAC2,
     C ACOMPAC3,ACOMPAC4,ICON,IEXPECT,ISTD,FCOMPAC5,
     C PERGROUP,QUANTYPE,SQUAREIT,EVALFUN,FCOMPAC6,FCOMPAC7,
     C FCOMPAC8,FCOMPAC9,FIXR,PEARSON,EQUIPEAR,
     C LRT,BCENT1,BCENT2,REVERSE,COVMAT,COVMAT2,COVMAT3,
     C COVMAT4,
     C HYPERG,KQMAX,PRINTO(27),INFILE,SIMO,NLRT,
     C BCMEANI,BCMEANO,BOBSCONI,BOBSCONO)
C  QQQ


         FCOMPACT=TCOMPACT
      END IF

      IF (FCOMPACT .GT. BESTVAL) THEN 
       DO 00019 FC=1,NUMTEACH
        NBESTDEP(FC)=NEWDEPT(FC)
00019    CONTINUE
        BESTVAL=FCOMPACT
       END IF

      TM=0.00
      TSTD=0.00
      CALL HUBIZE(NEWDEPT,NUMTEACH,RESULTM,STRUCTEQ)

      CALL HUBVAR(HUBERT,RESULTM,NUMTEACH,CHNGSIM,CHNGSTD,CHNGCON,
     C TM,TSTD,XTCOUNT)
      TM=0.00
      TSTD=0.00
      CALL HUBVAR(NEWMAT,RESULTM,NUMTEACH,HUBSIM,STDHS,ALLCON,TM,
     C TSTD,XTCOUNT)
      CHNGSIM=CHNGSIM/2
C   input (id pct chngcnt chngstd fcompact increm hubert
C   hubdiag pearson g2 hubdiagx pearsonx g2x) 

      TABLE1D=0
      TABLE1B=0
      INDEPT=0
      DO 2525 TG1=1,MAXDEPT  
      IF (DEPARTN(TG1) .GT. 0) THEN
      INDEPT=INDEPT+DEPARTN(TG1)*(DEPARTN(TG1)-1)
      TABLE1B=TABLE1B+BOBSCONO(TG1)
      TABLE1D=TABLE1D+BOBSCONI(TG1)
       END IF

02525  CONTINUE

       INDEPT=INDEPT*HIWTEVAL
      TABLE1C=INDEPT-TABLE1D
      TABLE1A=NUMTEACH*(NUMTEACH-1)*HIWTEVAL
     C  - (TABLE1B + TABLE1D)-TABLE1C

       GLAMNDA=TABLE1D*TABLE1A
       GLAMNDA=GLAMNDA/(TABLE1B*TABLE1C)
       GLAMNDA=LOG(GLAMNDA)/2

       WRITE(585,251) ENUFVAL,UCUT,CHNGSIM,CHNGSTD,FCOMPACT,TINCREM,
     C STDHS,FCOMPAC2,ACOMPAC3,ACOMPAC4,FCOMPAC5,FCOMPAC6,FCOMPAC7,
     C KCOMPACT,GLAMNDA
        OBFUN(ENUFVAL)=FCOMPACT
        PREDICT(ENUFVAL,CRES)=CHNGSTD
        ENUFVAL=ENUFVAL+1

       END DO
       HOLDOFF=1
       IF (NUMRES .GT. 1) THEN
        NUMPRED=1
        CALL KGETRES(OBFUN,PREDICT,NUMPRED,NEVAL,HIGHRES)


C       NBR(1)=2
C       NBR(2)=NEVAL
C       NBR(3)=NEVAL
C       NBR(4)=1
C       NBR(5)=1
C       NBR(6)=1
C
C       CALL BECOVM(OBFUN,NEVAL,NBR,TEMP,XYBAR,A,IER)
C       ALFA=0.05
C       IB=2
C       IH(1)=3
C       IH(2)=1
C       BETA(2)=B(1,1)
C       BETA(1)=B(2,1)
C       CALL RLMUL (A,XYBAR,NEVAL,1,ALFA,ANOVA,B,IB,VARB,IER)
C       CALL RLRES (OBFUN,NEVAL,2,ENUFVAL,IH,2,BETA,
C     C ANOVA(12),RES,ENUFVAL,IER)
C       DO 99 I=1,NEVAL
C       MYRES(I)=RES(I,3)
C00099   CONTINUE

C      CALL SPSORT(ORDCLOSE,NUMIN,SLOTA)
C       CALL SPSORT(RESIDS,NEVAL,ORDER)
C      CALL RESORTO3(OLDDEPT,NUMTEACH,QSEED,USEED,UCUT,NEWDEPT,
C     C MAXDEPT)

      CALL RESORTO3(OLDDEPT,NUMTEACH,QSEED,
     C ISEEDS(HIGHRES),ICUTS(HIGHRES),TRYDEPT,
     C IMAX(HIGHRES),NEWGRPS)

C      SUBROUTINE RESORTO3 (ELEMENTS,NUMELEM,RSEED,USEED,UCUT,
C     C  NELEMENT,TMAXDEPT,NEWGRPS)

        CALL ASSIGN2(NUMTEACH,TRYDEPT,MAXDEPT,
     C  DEPARTN)
        CALL REMOVEO4(MAXDEPT,DEPARTN,LGROUP,1,CUTOFF,0)
        DO 856 RP1=1,NUMTEACH
          NEWDEPT(RP1)=LGROUP(NEWDEPT(RP1))
00856    CONTINUE

        
       ELSE 
       DO 9988 RP2=1,NUMTEACH
       TRYDEPT(RP2)=NEWDEPT(RP2)
09988   CONTINUE

       END IF
        CALL ASSIGN2(NUMTEACH,TRYDEPT,MAXDEPT,
     C  DEPARTN)
  251 FORMAT(25(F10.5)) 

      RETURN

      END

       SUBROUTINE KGETRES(OBFUN,PREDICT,NUMPRED,NEVAL,HIGHRES)
       INTEGER NEVAL,NUMPRED,I,J,IER,IAINV,IBETAHAT,IER2,C,HIGHRES
       REAL OBFUN(1000),PREDICT(1000,2),TRESIDS,
     C BESTRES,
     C  SXX(1000,1000),XBAR(1000),YBAR,SXY(1000),
     C S(1000),WK(2*1000),INVSXX(1000,1000),
     C BETAHAT(1000,1),PREDVAL(1000),BETA0,TBETA

C     C BETAHAT,IBETAHAT,IER2)

C     C IAINV,S,WK,IER)
       
       DO 02 I=1,NEVAL
       C=I-1
       DO 03 J=1,NUMPRED
       DO 04 K=1,(J-1)
       SXX(J,K)=SXX(J,K)+(PREDICT(I,J)-XBAR(J))*
     C  (PREDICT(I,K)-XBAR(K))*C/(C+1)
       SXX(K,J)=SXX(J,K)
 0004   CONTINUE
       SXX(J,J)=SXX(J,J)+(C/(C+1.))*(PREDICT(I,J)-XBAR(J))**2
       SXY(J)=SXY(J)+(C/(C+1.))*(PREDICT(I,J)-XBAR(J))*
     C                         (OBFUN(I)-YBAR)
       XBAR(J)=XBAR(J)+(1./(C+1.))*(PREDICT(I,J)-XBAR(J))
00003   CONTINUE
       YBAR=YBAR+(1./(C+1.))*(OBFUN(I)-YBAR) 
 0002    CONTINUE
       BESTRES=0
       IF (NUMPRED .GT. 1) THEN
C       FIND OTHER REGRESSION ROUTINE
C       CALL LGINF(SXX,NUMPRED,NUMPRED,NUMPRED,.0,INVSXX,
C     C IAINV,S,WK,IER)

C      FIND OTHER REGRESSION ROUTINE 
C       CALL VMULFF(INVSXX,SXY,NUMPRED,NUMPRED,1,NUMPRED,NUMPRED,
C     C BETAHAT,IBETAHAT,IER2)
       DO 08 I=1,NEVAL
       DO 09 J=1,NUMPRED
       PREDVAL(I)=PREDVAL(I)+BETAHAT(J,1)*PREDICT(I,J)
 0009  CONTINUE
       TRESIDS=OBFUN(I)-PREDVAL(I)
       IF (TRESIDS .GT. BESTRES) THEN
       HIGHRES=I
        BESTRES=TRESIDS
        END IF
 0008   CONTINUE
       ELSE
       TBETA=SXY(1)/SXX(1,1)
       BETA0=YBAR-TBETA*XBAR(1)
       DO 11 I=1,NEVAL
       TRESIDS=OBFUN(I)-(BETA0+ TBETA*PREDICT(I,1))
       IF (TRESIDS .GT. BESTRES) THEN
        HIGHRES=I
        BESTRES=TRESIDS
        END IF
 0011   CONTINUE
       END IF


       RETURN
       END

      SUBROUTINE FISHERZ (COVMAT,SIZE)

      INTEGER G2,G3,SIZE,NODO
      REAL KSIGN
      REAL  COVMAT(251,251),ZABS
      DO 336 G2=1,SIZE
       DO 338 G3=1,SIZE

        IF (COVMAT(G2,G3) .GE. 0) THEN
         KSIGN=1
        ELSE 
         KSIGN=-1
        END IF

         NODO=0
        IF (COVMAT(G2,G3) .GT. 10) THEN 
         COVMAT(G2,G3) = 1
         NODO=1
        END IF
        IF (COVMAT(G2,G3) .LT. -10) THEN 
         COVMAT(G2,G3) = -1
         NODO=1
        END IF
        
        IF (NODO .EQ. 0) THEN
C        ZEXP(G2,G3)=EXP(COVMAT(G2,G3))
        ZABS=EXP(ABS(COVMAT(G2,G3)*2))
        COVMAT(G2,G3)=KSIGN*(ZABS-1)/(ZABS+1)
        END IF
C        IF (G2 .EQ. G3) THEN
C         ZMAT(G2,G3)=0.000
C        END IF
  338 CONTINUE
  336 CONTINUE
      RETURN
      END 

       SUBROUTINE KMULTMA2(SIZE,RCHOICE,KDIRECT,WTXPX,WTXXP,
     C ONLIST,NUMDYAD,PERSON1,PERSON2,PERSON3)
C                                                      SYMMAT,WT     
        INTEGER SIZE,I,J,K,A,RCHOICE(251),ONLIST(251),NUMDYAD,
     C  OCOUNT,H,IHAVE(251),PERSON1(251),PERSON2(251),PERSON3(251)
          REAL WTXPX,WTXXP,BESTTRID

        REAL VINMAT(251,251),CONNECTO
        DOUBLE PRECISION TXPX,TXXP
        REAL KMAT(251,251),INMAT(251,251),KDIRECT

       INTEGER HUBERT(251,251),RESULTM(251,251),ALLGROUP(251,251),
     C NEWMAT(251,251)
       REAL BLAUC(251,251),DIFF(251,251)
       COMMON VINMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF

C       COMMON VINMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT


       DO 2 G=1,SIZE
       IHAVE(G)=0
    2     CONTINUE

       DO 5 H=1,NUMDYAD
        BESTTRID=-999999  
C       WRITE(503,251) 999
      DO 10 I=1,SIZE
      DO 20 J=1,SIZE
        INMAT(I,J)=VINMAT(I,J)
        KMAT(I,J)=INMAT(I,J)
        IF (I .EQ. J) THEN
         KMAT(I,J)=KDIRECT
        INMAT(I,J)=KDIRECT
        END IF
   20 CONTINUE
   10 CONTINUE

         DO 30 I=3,SIZE
           IF ((IHAVE(I) .EQ. 0) .AND. (RCHOICE(I) .GT. 0)) THEN
           DO 40 J=2,(I-1)

           IF ((IHAVE(J) .EQ. 0) .AND. (RCHOICE(J) .GT. 0)) THEN
            DO 50 K=1,(J-1)
           IF ((IHAVE(K) .EQ. 0) .AND. (RCHOICE(K) .GT. 0)) THEN
         TXPX=0
         TXXP=0
C          IF ((RCHOICE(I) .GT. 0)  .AND. (RCHOICE(J) .GT. 0)
C     C .AND. (RCHOICE(K) .GT. 0)) THEN
         CONNECTO=0
         DO 60 A=1,SIZE
         TXPX=TXPX+KMAT(A,I)*INMAT(A,J)+
     C                                 KMAT(A,I)*INMAT(A,K)+
     C                                 KMAT(A,J)*INMAT(A,K)
          TXXP=TXXP+INMAT(I,A)*KMAT(J,A)+
     C                                  INMAT(I,A)*KMAT(K,A)+
     C                                  INMAT(J,A)*KMAT(K,A)
   60    CONTINUE

          CONNECTO=TXPX*WTXPX+TXXP*WTXXP

          IF (CONNECTO .GT. BESTTRID) THEN
          PERSON1(H)=I
          PERSON2(H)=J
          PERSON3(H)=K
          BESTTRID=CONNECTO
          END IF

          END IF
C         WRITE(504,504) I,J,K, TXXPRIME(I,J,K), TXPRIMEX(I,J,K)
   50    CONTINUE
         END IF
   40  CONTINUE
         END IF
   30 CONTINUE
      OCOUNT=3*(H-1)
      ONLIST(OCOUNT+1)=PERSON1(H)
      ONLIST(OCOUNT+2)=PERSON2(H)
      ONLIST(OCOUNT+3)=PERSON3(H)
      IHAVE(PERSON1(H))=1
      IHAVE(PERSON2(H))=1
      IHAVE(PERSON3(H))=1
    5 CONTINUE

      RETURN
  502 FORMAT(A12,1X)
  504 FORMAT(20(F10.5))
  251 FORMAT(20(F10.5))
      END

C         CALL KMULTMA3(ZCOMPMAT,ELGC,RCHOICE,DIRECT,TWXPX,DSYMMAT,
C     C ONLIST,1,PERSON1,PERSON2,ELGLIST,LOWMEM)

       SUBROUTINE KMULTMA3(SIZE,RCHOICE,KDIRECT,WTXPX,WTXXP,
     C ONLIST,NUMDYAD,PERSON1,PERSON2,ELGLIST,LOWMEM,TOTSIZE)
C                                                      SYMMAT,WT     
        INTEGER SIZE,I,J,K,A,RCHOICE(251),ONLIST(251),NUMDYAD,
     C  OCOUNT,H,IHAVE(251),PERSON1,PERSON2,TI,TJ,TK,TA,
     C  ELGLIST(251),LOWMEM,TOTSIZE
          REAL WTXPX,WTXXP,BESTTRID

        REAL VINMAT(251,251),CONNECTO
        DOUBLE PRECISION TXPX,TXXP
        REAL KDIRECT,TIA,TAI,TJA,TAJ,TAK,TKA

       INTEGER ALLGROUP(251,251),HUBERT(251,251),RESULTM(251,251),
     C NEWMAT(251,251)
       REAL BLAUC(251,251),DIFF(251,251)
       COMMON VINMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF

C       COMMON VINMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT

       DO 2 G=1,SIZE
       IHAVE(G)=0
    2     CONTINUE

        PERSON1=LOWMEM
        PERSON2=LOWMEM
C       DO 5 H=1,NUMDYAD

        BESTTRID=-999999  

           I=LOWMEM
           DO 40 TJ=2,SIZE
           J=ELGLIST(TJ)
           IF ((IHAVE(TJ) .EQ. 0) .AND. (RCHOICE(J) .GT. 0) .AND.
     C      (J .NE. I)) THEN
            DO 50 TK=1,(TJ-1)
            K=ELGLIST(TK)
           IF ((IHAVE(TK) .EQ. 0) .AND. (RCHOICE(K) .GT. 0) .AND.
     C     (K .NE. I)) THEN
         TXPX=0
         TXXP=0
         CONNECTO=0
         DO 60 A=1,TOTSIZE
         IF (A .EQ. I) THEN
         TAI=KDIRECT
         TIA=KDIRECT
         ELSE
         TAI=VINMAT(A,I)
         TIA=VINMAT(I,A)
         END IF
         IF (A .EQ. J) THEN
         TAJ=KDIRECT
         TJA=KDIRECT
         ELSE
         TAJ=VINMAT(A,J)
         TJA=VINMAT(J,A)
         END IF
         IF (A .EQ. K) THEN
         TAK=KDIRECT
         TKA=KDIRECT
         ELSE
         TAK=VINMAT(A,K)
         TKA=VINMAT(K,A)
         END IF
         TXPX=TXPX+TAI*TAJ+TAI*TAK+TAJ*TAK
         TXXP=TXXP+TIA*TJA+TIA*TKA+TJA*TKA
   60    CONTINUE

          CONNECTO=TXPX*WTXPX+TXXP*WTXXP

          IF (CONNECTO .GT. BESTTRID) THEN
          PERSON1=J
          PERSON2=K
          BESTTRID=CONNECTO
          END IF

          END IF
C         WRITE(504,504) I,J,K, TXXPRIME(I,J,K), TXPRIMEX(I,J,K)
   50    CONTINUE
         END IF
   40  CONTINUE
      RETURN
  502 FORMAT(A12,1X)
  504 FORMAT(20(F10.5))
  251 FORMAT(20(F10.5))
      END

       SUBROUTINE KMULTMAD(INMAT,SIZE,RCHOICE,KDIRECT,WTXPX,WTXXP,
     C ONLIST,NUMDYAD,PERSON1,PERSON2)
C                                                      SYMMAT,WT     
        INTEGER SIZE,I,J,K,A,RCHOICE(251),ONLIST(251),NUMDYAD,
     C  OCOUNT,H,IHAVE(251),PERSON1(251),PERSON2(251)
     
          REAL WTXPX,WTXXP

        REAL CONNECTO,BESTTRID

C       REAL VINMAT(251,251),KMAT(251,251)

        DOUBLE PRECISION TXPX,TXXP
        REAL INMAT(251,251),KDIRECT

C       WRITE(33,251) WTXPX,WTXXP

       DO 2 G=1,SIZE
       IHAVE(G)=0
       INMAT(G,G)=KDIRECT  
    2     CONTINUE

       DO 5 H=1,NUMDYAD
        BESTTRID=-999999  
C       WRITE(503,251) 999

         DO 30 I=2,SIZE
           IF ((IHAVE(I) .EQ. 0) .AND. (RCHOICE(I) .GT. 0)) THEN

           DO 40 J=1,(I-1)

           IF ((IHAVE(J) .EQ. 0) .AND. (RCHOICE(J) .GT. 0)) THEN
         TXPX=0
         TXXP=0
         CONNECTO=0
         DO 60 A=1,SIZE
         
         TXPX=TXPX+INMAT(A,I)*INMAT(A,J)
         TXXP=TXXP+INMAT(I,A)*INMAT(J,A)
   60    CONTINUE


          CONNECTO=TXPX*WTXPX+TXXP*WTXXP
C          LOOKMAT(I,J)=CONNECTO

          IF (CONNECTO .GT. BESTTRID) THEN
          PERSON1(H)=I
          PERSON2(H)=J
          BESTTRID=CONNECTO
          END IF
C
           END IF

C         WRITE(504,504) I,J,K, TXXPRIME(I,J,K), TXPRIMEX(I,J,K)
   40  CONTINUE
         END IF
   30 CONTINUE
      OCOUNT=2*(H-1)
      ONLIST(OCOUNT+1)=PERSON1(H)
      ONLIST(OCOUNT+2)=PERSON2(H)
      IHAVE(PERSON1(H))=1
      IHAVE(PERSON2(H))=1
    5 CONTINUE


      RETURN
  502 FORMAT(A12,1X)
  504 FORMAT(50(F10.5))
  251 FORMAT(50(F10.5))
      END


C       CALL INITGRP(NUMDYAD,PERSON1,PERSON2,PERSON3,NEWDEPT,
C     C NUMTEACH,NLIST,NUMNLIST)       

       SUBROUTINE INITGRP(NUMDYAD,PERSON1,PERSON2,PERSON3,
     C NEWDEPT,NUMTEACH,LIST,L)
     
       INTEGER NUMDYAD,PERSON1(251),PERSON2(251),PERSON3(251),
     C NEWDEPT(251),ND,NEWA(251),LIST(251),L,J,NUMTEACH
       
         
      DO 5 I=1,NUMTEACH
        NEWDEPT(I)=0
    5 CONTINUE

        WRITE (3,3252) 
        WRITE (3,2252) "INITIALIZE" ,"GROUPS"

        WRITE (3,2252) "STARTING","GROUPS"
        WRITE(33,2252)
        WRITE (3,2252) "TRIAD","ACTOR 1","ACTOR 2","ACTOR 3"
      DO 10 ND=1,NUMDYAD
        WRITE (3,2251) ND,PERSON1(ND),PERSON2(ND),PERSON3(ND)
        NEWDEPT(PERSON1(ND))=ND
C        NEWA(PERSON1(ND))=1
        NEWDEPT(PERSON2(ND))=ND
C        NEWA(PERSON2(ND))=1
        NEWDEPT(PERSON3(ND))=ND
C        NEWA(PERSON3(ND))=1
   10  CONTINUE
  101 FORMAT(A15)
  251 FORMAT(251(F10.5))
02252 FORMAT(251(A10))
02251 FORMAT(251(I10))
03252 FORMAT(//,"---------------------------------------------",//)

        RETURN
        END

       SUBROUTINE INITGRPD(NUMDYAD,PERSON1,PERSON2,
     C NEWDEPT,NUMTEACH,LIST,L)
     
       INTEGER NUMDYAD,PERSON1(251),PERSON2(251),
     C NEWDEPT(251),ND,NEWA(251),LIST(251),L,J,NUMTEACH
       
         
      DO 5 I=1,NUMTEACH
        NEWDEPT(I)=0
    5 CONTINUE

      DO 10 ND=1,NUMDYAD
        NEWDEPT(PERSON1(ND))=ND
        NEWDEPT(PERSON2(ND))=ND
   10  CONTINUE
  101 FORMAT(A15)
  251 FORMAT(20(F10.5))
        RETURN
        END

C      CALL ATTACHO2 (CHANGEC,STOPVAL,LOWMEM,NUMTEACH,
C     C MADECHNG,ISO,ISOLIST,NEWDEPT,MEANWT,VARWT,WPIK,
C     C ALLGROUM,ALLGROUV,NEARVAL,ALLGROUR,DEPARTN,MAXDEPT,
C     C ALLGROUP,RCHOICE,NEWMAT,DMEMBERS,QUITIT,LASTCHNG,NSUBID,
C     C TITLES,DEBUG)

      SUBROUTINE ATTACHO2 (CHANGEC,STOPVAL,LOWMEM,NUMTEACH,
     C MADECHNG,ISO,ISOLIST,NEWDEPT,MEANWT,VARWT,WPIK,
     C NEARVAL,DEPARTN,MAXDEPT,
     C RCHOICE,DMEMBERS,QUITIT,LASTCHNG,NSUBID,
     C ATITLE,HYPERG)
C        CALL ASSIGN(NUMTEACH,NEWDEPT,MAXDEPT,ALLGROUP
C     C  ,DEPARTN,RCHOICE,ALLGROUR,MEANWT,VARWT,ALLGROUM,ALLGROUV)


      CHARACTER ATITLE(3)*20
      REAL AVAR2,MEANWT(251),VARWT(251),WPIK,NOB


      DOUBLE PRECISION SUMCLOSE
      REAL CHOSEMN(251),
     C CHOSEVR(251),AMEAN,AVAR,
     C TOTE,TOTCON,TOTV,UTOTCON
      REAL CDEPT,NEARVAL,BESTD(251),CHANGEC(251),
     C NOWCLOSE(251),LOWCOMP,STOPVAL,LASTCHNG,ALLC(251,251)
C
      INTEGER LOWMEM,NUMTEACH,ISO,ISOLIST(251),NSUBID(251),
     C DEPARTN(251),NEWDEPT(251),MAXDEPT,
     C RCHOICE(251),MADECHNG,
     C Z,IDD,BJ,NUMIN,HYPERG

 
      INTEGER SKIPIT,DMEMBERS(251),THISMEM,TEMPS,OLDLOWM,
     C FOUNDONE,NEWMEMS(251),OLDDEPT,LDEPART,LDEPTN,NGROUP,
     C THED(251),NOPICK,DNUM(251),ONLIST(251),AB,ON,
     C KNODO,KSTOP,KCOUNT
       REAL ZCOMPMAT(251,251),MT,VT
       INTEGER HUBERT(251,251),RESULTM(251,251),ALLGROUP(251,251),
     C NEWMAT(251,251)
       REAL BLAUC(251,251),DIFF(251,251)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF

C       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT

  
       WRITE(33,102) "ATTACH"
       ON=1
       DO 666 AB=1,NUMTEACH
       ISOLIST(AB)=0
       IF (NEWDEPT(AB) .LT. 1) THEN
         ONLIST(ON)=AB
         ON=ON+1
       END IF
  666    CONTINUE
      ON=ON-1
      KNODO=0
      IF (ON .LT. 1) THEN 
      KNODO=1
      END IF
      IF (KNODO .EQ. 0) THEN
      NOB=1.00000
      MADECHNG=1
      OLDLOWM=0
      ISO=1
      ISOLIST(1)=0
      LOWMEM=1  
      CHANGEC(LOWMEM)=251.00
      BJ=0
       KCOUNT=1
      KSTOP=0
      DO WHILE ((KCOUNT .LT. 251) .AND. (KSTOP .EQ. 0))
      KCOUNT=KCOUNT+1
      LASTCHNG=CHANGEC(LOWMEM)
      LOWCOMP=-200.000
      NOPRINT=0
      NUMIN=0
      SUMCLOSE=0.0 
      DO 28, IDD=1,ON
       I=ONLIST(IDD)
       IF (NEWDEPT(I) .LT. 1) THEN
       SKIPIT=0
       IF (MADECHNG .EQ. 0 )THEN
         NOPRINT=1
         DO 18 CHECK=1,ISO
          IF (I .EQ. ISOLIST(CHECK)) THEN
            SKIPIT=1
          END IF
   18    CONTINUE
        ELSE
         DO 19 FIXIT=1,ISO
          ISOLIST(FIXIT)=0
   19    CONTINUE
         ISO=1
       END IF
       IF (SKIPIT .EQ. 0) THEN
       BESTD(I)=-10.0
       THED(I)=NEWDEPT(I)
      DO 34 J=1,MAXDEPT
         CHOSEMN(J)=0.000
         CHOSEVR(J)=0.00
       DO 40 K=1,DEPARTN(J)
         THISMEM=ALLGROUP(J,K)
         DMEMBERS(K)=THISMEM
C WORKING K

         IF (THISMEM .GT. 0) THEN
         IF ((THISMEM .NE. I) .AND. (RCHOICE(THISMEM) .GT. 0)) THEN
C      SUBROUTINE DISTRIB (THISR,THISWM,THISWV,MEAN,TVAR,KGRPSIZE,
C     C           TOTSIZE,THISPER)
         CALL DISTRIB (RCHOICE(THISMEM),MEANWT(THISMEM),
     C    VARWT(THISMEM),AMEAN,AVAR,2,NUMTEACH,THISMEM,AVAR2,HYPERG)
         CHOSEMN(J)=CHOSEMN(J)+AMEAN
         CHOSEVR(J)=CHOSEVR(J)+AVAR
         END IF
         END IF

   40  CONTINUE
C      SUBROUTINE HOWMANY (MEMBERS,KGRPSIZE,INDIV,PATTERN,TOTSIZE,
C     C                  TOTCON,GROUP,WEIGHT,WTB)
         CALL HOWMANY (DMEMBERS,DEPARTN(J),I,NEWMAT,NUMTEACH,
     C                  TOTCON,J,WPIK,NOB,UTOTCON,MT,VT)
         IF (TOTCON .GT. 99) THEN 
         END IF
         TEMPS=DEPARTN(J)
         IF (NEWDEPT(I) .NE. J) THEN
         TEMPS=TEMPS+1
         END IF 
         IF (DEPARTN(J) .EQ. 1) THEN
         TEMPS=2
         END IF

           AMEAN=0.000
           AVAR=0.000
         IF (RCHOICE(I) .GT. 0) THEN 
           CALL DISTRIB (RCHOICE(I),MEANWT(I),
     C      VARWT(I),AMEAN,AVAR,TEMPS,NUMTEACH,I,AVAR2,HYPERG)
         END IF
            TOTE=CHOSEMN(J)+AMEAN
            TOTV=CHOSEVR(J)+AVAR
         CDEPT=(TOTCON-TOTE)/(TOTV**.5)
         ALLC(I,J)=CDEPT
        IF (J .EQ. NEWDEPT(I)) THEN
         NOWCLOSE(I)=CDEPT
         SUMCLOSE=SUMCLOSE+CDEPT
         NUMIN=NUMIN+1
         IF (DEPARTN(NEWDEPT(I)) .LT. 2) THEN 
         SUMCLOSE=SUMCLOSE-CDEPT
         NUMIN=NUMIN-1
          NOWCLOSE(I)=0.0
          ALLC(I,J)=0.0
          DUMBO=0
         END IF
        END IF
        IF (CDEPT .GT. BESTD(I)) THEN
         THED(I)=J
         BESTD(I)=CDEPT
         END IF
C FIRST CLOSEDEP
   34 CONTINUE
       CHANGEC(I)=BESTD(I)-NOWCLOSE(I)
       IF (CHANGEC(I) .GT. LOWCOMP) THEN
        LOWCOMP=CHANGEC(I)
        LOWMEM=I
       END IF
      END IF
      END IF
   28 CONTINUE
    
        NOPRINT=1        
      OLDLOWM=LOWMEM       
        IF (NOPRINT .EQ. 0) THEN
        DO 80 Z=1,MAXDEPT
          IF (DEPARTN(Z) .GT. 0) THEN
          WRITE(33,133) Z , (NSUBID(ALLGROUP(Z,P)) , P=1,DEPARTN(Z))
          WRITE(33,134) DEPARTN(Z),
     C         (NOWCLOSE(ALLGROUP(Z,P2)), P2=1,DEPARTN(Z))
          WRITE(33,137) "BEST G",
     C         (BESTD(ALLGROUP(Z,P2)), P2=1,DEPARTN(Z))
          WRITE(33,137) "GROUP",
     C         (THED(ALLGROUP(Z,P2)), P2=1,DEPARTN(Z))
         END IF
   80   CONTINUE
        END IF
        WRITE(33,139) LOWMEM,NEWDEPT(LOWMEM),THED(LOWMEM)
C
C
C       NOW I HAVE FOUND THE BEST PERSON TO CHANGE (LOWMEM) AND THE 
C        BEST DEPARTMENT FOR THAT PERSON THED(LOWMEM)
C        AND THE DISTANCE TO THAT DEPARTMENT (BESTD(LOWMEM))
C        IS IT GOOD ENOUGH? 
C
        WRITE(33,251) LOWMEM,THED(LOWMEM),ALLGROUP(THED(LOWMEM),1),
     C  RCHOICE(ALLGROUP(THED(LOWMEM),1)),DEPARTN(THED(LOWMEM)),
     C NOPICK

         NOPICK=0
      IF ((RCHOICE(LOWMEM) .EQ. 0) .AND. 
     C   (DEPARTN(THED(LOWMEM)) .LT. 2)) THEN
         NOPICK = 1
         DUMBO=0
      END IF
      IF ((DEPARTN(THED(LOWMEM)) .LT. 2)  .AND. 
     C    (RCHOICE(ALLGROUP(THED(LOWMEM),1)) .LT. 1)) THEN
       NOPICK=1
       DUMBO=0
      END IF
        IF ((NOPICK .EQ. 1) .OR. (LOWMEM .LT. 1)) THEN
        MADECHNG=0
        ISOLIST(ISO)=LOWMEM
        ISO=ISO+1
        WRITE(33,251) LOWMEM,DEPARTN(THED(LOWMEM)),
     C  RCHOICE(LOWMEM),RCHOICE(ALLGROUP(THED(LOWMEM),1))
       ELSE
      IF ((BESTD(LOWMEM) .GT. NEARVAL) .AND. (THED(LOWMEM) .NE. 
     C NEWDEPT(LOWMEM))) THEN
          MADECHNG=1
          BJ=BJ+1
          LONER=0
          OLDDEPT=NEWDEPT(LOWMEM)
          LDEPART=NEWDEPT(LOWMEM)
          LDEPTN=DEPARTN(NEWDEPT(LOWMEM))
          NEWDEPT(LOWMEM)=THED(LOWMEM)
          DEPARTN(THED(LOWMEM))=DEPARTN(THED(LOWMEM))+1
         ELSE
             KSTOP=1
             MADECHNG=0
             ISOLIST(ISO)=LOWMEM
             ISO=ISO+1
              WRITE(33,251) LOWMEM,BESTD(LOWMEM),THED(LOWMEM),
     C       NEWDEPT(LOWMEM)
         END IF
        END IF
      IF (MADECHNG .EQ. 1) THEN

        CALL ASSIGN(NUMTEACH,NEWDEPT,MAXDEPT,
     C  DEPARTN,RCHOICE,MEANWT,VARWT,
     C  I,THISMEM)
        WRITE(33,136) LOWMEM,OLDDEPT,NEWDEPT(LOWMEM)
        WRITE(33,132) 
      END IF
C    1 CONTINUE
       END DO
       DO 88 Z=1,NUMTEACH
       IF (NEWDEPT(Z) .EQ. 0) THEN 
        MAXDEPT=MAXDEPT+1
        NEWDEPT(Z)=MAXDEPT
       END IF
   88     CONTINUE
      END IF
        DO 82 Z=1,MAXDEPT
          IF (DEPARTN(Z) .GT. 0) THEN
          WRITE(33,133) Z , (ALLGROUP(Z,P) , P=1,DEPARTN(Z))
          WRITE(33,133) Z , (NSUBID(ALLGROUP(Z,P)) , P=1,DEPARTN(Z))
          WRITE(33,134) DEPARTN(Z),
     C         (NOWCLOSE(ALLGROUP(Z,P2)), P2=1,DEPARTN(Z))
          WRITE(33,137) "BEST G",
     C         (BESTD(ALLGROUP(Z,P2)), P2=1,DEPARTN(Z))
          WRITE(33,137) "GROUP",
     C         (THED(ALLGROUP(Z,P2)), P2=1,DEPARTN(Z))
           END IF
          DNUM(Z)=Z
   82   CONTINUE
       RETURN
  144 FORMAT(2F10.5)
  251 FORMAT(20(F10.5))
  103 FORMAT(20(F6.2,2X))  
  102 FORMAT(10(A30)) 
  101 FORMAT(20(A8)) 
  132 FORMAT("DEPART    MEMBERS")
  133 FORMAT(I4,4X,251(I6,4X))
  134 FORMAT("(",I4,")",4X,251("(",F8.3,")"))
  137 FORMAT(A6,4X,251("(",F8.3,")"))
  136 FORMAT("HAVE MOVED ",I4," FROM ",I4," TO ",I4)
  139 FORMAT("WANT TO MOVE ",I4," FROM ",I4," TO ",I4)
      END

      SUBROUTINE DEBUGO(DVAL)
       INTEGER DVAL
       IF (DVAL .EQ. 1) THEN 
        WRITE(33,100) DVAL
        END IF
       RETURN
 0100   FORMAT("WOULD HAVE STOPPED AT DEBUG ",I3)
       END

      SUBROUTINE CHECKVAL(INMAT,SIZE,P1,P2,WHERE)
       REAL INMAT(251,251)
       INTEGER SIZE,P1,P2,I,J
       CHARACTER WHERE*20

      DO 10 I=1,SIZE
         DO 20 J=1,SIZE
         IF (INMAT(I,J) .GT. 5) THEN
C          WRITE(217,102) WHERE,I,J,INMAT(I,J)
          STOP
         END IF
   20       CONTINUE
   10        CONTINUE

       RETURN
  102 FORMAT(A20,500(F10.5))
      END
              
    

C      
C
C     SUBROUTINE FOR RANDOMLY ASSIGNING ACTORS TO GROUPS
      SUBROUTINE RANDASSG (ELEMENTS,NUMELEM,RSEED,NUMGROUP)
      INTEGER ELEMENTS(251),NUMELEM,FAKETCH,NUMGROUP
      INTEGER OELEMENT(251),Z,J,K,I,CONVERT(251),RITER
      DOUBLE PRECISION RSEED
      INTEGER RANDARRY(251),OLDPOS(251),D,PERGROUP
C
C   NOW THE RANDARRY IS AN ARRAY OF RANDOM NUMBERS OF LENGTH NUMELEM
C
      PERGROUP=3
      D=1

      DO 2 I=1,NUMGROUP
       DO 3 J=1,PERGROUP
       ELEMENTS(D)=I
       D=D+1
    3 CONTINUE
    2 CONTINUE

      DO WHILE (D .LE. NUMELEM)
      ELEMENTS(D)=99
      D=D+1
      END DO

C   

      DO 4 K=1,NUMELEM
      OELEMENT(K)=ELEMENTS(K)
    4 CONTINUE
      FAKETCH=NUMELEM
      CALL GGPER(RSEED,FAKETCH,RANDARRY)
      DO 5 Z=1,NUMELEM
      IF (OELEMENT(RANDARRY(Z)) .EQ. 99) THEN
      OELEMENT(RANDARRY(Z)) = 0
      END IF
      ELEMENTS(Z)=OELEMENT(RANDARRY(Z))
      
      WRITE(18,219) Z,RANDARRY(Z),OELEMENT(Z),ELEMENTS(Z)
    5 CONTINUE
  219 FORMAT(I3,1X,I5,1X,I5,1X,I3)
      RETURN
      END
C     END SUBROUTINE RESORTO2


C      CALL ARCHOICE(NUMTEACH,NEWMAT,RCHOICE,PWEIGHTS,MAXCH)
      SUBROUTINE ARCHOICE(NUMTEACH,NEWMAT,RCHOICE,
     C MAXCH,FIXR,HIFLAG,MEANW,VARW)
      INTEGER RCHOICE(251),NUMTEACH,S,R3,R2,NEWMAT(251,251),
     C MAXCH,HIFLAG,MAXR
      REAL OBS(251),VARW(251),MEANW(251),FIXR,SUMW(251),TR


      DO 30 S=1,NUMTEACH
       RCHOICE(S)=0
   30 CONTINUE
C      WRITE(27,140) "R IS ZERO"
        MAXR=0
        MAXCH=-99
       HIFLAG=0
      DO 32 R3=1,NUMTEACH
        SUMW(R3)=0
        VARW(R3)=0
        MEANW(R3)=0
        DO 33 R2=1,NUMTEACH
         IF (NEWMAT(R3,R2) .GT. 0) THEN 
            RCHOICE(R3)=RCHOICE(R3)+1
            SUMW(R3)=SUMW(R3)+NEWMAT(R3,R2)
            OBS(RCHOICE(R3))=NEWMAT(R3,R2)
         END IF
         IF (NEWMAT(R3,R2) .GT. MAXCH) THEN
           MAXCH=NEWMAT(R3,R2)
         END IF
   33   CONTINUE
       IF (RCHOICE(R3) .GT. MAXR) THEN
         MAXR=RCHOICE(R3)
         END IF
       IF (RCHOICE(R3) .GT. 0) THEN
        TR=RCHOICE(R3)
         MEANW(R3)=SUMW(R3)/RCHOICE(R3)
       DO 29 V=1,RCHOICE(R3)
       VARW(R3)=VARW(R3)+((OBS(V)-MEANW(R3))**2)/TR
00029   CONTINUE
       END IF
   32  CONTINUE

         IF (MAXR .GT. FIXR) THEN
         HIFLAG=1
         END IF
         FIXR=MAXR

       RETURN 
       END


      SUBROUTINE DIFFMEAN(I,GROUPMAT,G,NUMTEACH,DIFF,THISN,
     C ACTRSQR,TSS,MAXDEPT,ROWWT,COLWT,NEWDEPT)
      INTEGER NUMTEACH,I,J,G,THISN,ACTRSQR,MAXDEPT,
     C NEWDEPT(251)
      REAL DIFF,GROUPMAT(251,251),TREAL1,TREAL2,TSS,
     C ROWWT,COLWT,WDENOM
       REAL ZCOMPMAT(251,251)
       INTEGER HUBERT(251,251),RESULTM(251,251),ALLGROUP(251,251),
     C NEWMAT(251,251)
       REAL BLAUC(251,251),KDIFF(251,251)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C BLAUC,KDIFF

C       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT
        WDENOM=ROWWT+COLWT
      DIFF=0.00
      DO 04 J=1,NUMTEACH
      TREAL1=NEWMAT(I,J)
       IF ((J .EQ. I) .OR. (NEWDEPT(J) .EQ. G)) THEN
       WT=ROWWT
       ELSE
       WT=COLWT
       END IF
       WT=WT/WDENOM
      DIFF=DIFF+WT*(TREAL1-GROUPMAT(G,J))**2
00004  CONTINUE
       IF (ACTRSQR .EQ. 1) THEN
       TREAL2=THISN
       DIFF=DIFF*TREAL2/(TREAL2+1.)
       END IF
        DIFF=TSS-DIFF
C       DIFF=1000/DIFF
       RETURN
      END

      SUBROUTINE GETMEAN(NEWDEPT,NUMTEACH,GROUPMAT,MAXDEPT,
     C DEPARTN,WANTSS,SS,WSS,ROWWT,COLWT)

      INTEGER NEWDEPT(251),NUMTEACH,I,J,MAXDEPT,
     C DEPARTN(251),TN,WANTSS,G
      REAL GROUPMAT(251,251),SS(251,251),TREAL,TDENOM,WSS,
     C ROWWT,COLWT,WT,WDENOM
       REAL ZCOMPMAT(251,251)
       INTEGER HUBERT(251,251),RESULTM(251,251),ALLGROUP(251,251),
     C NEWMAT(251,251)
       REAL BLAUC(251,251),DIFF(251,251)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF

C       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT


      
      DO 4 I=1,MAXDEPT
       DO 5 J=1,NUMTEACH
      GROUPMAT(I,J)=0.000
       SS(I,J)=0.000
00005  CONTINUE
00004  CONTINUE
      
      DO 06 I=1,NUMTEACH
        TDENOM=DEPARTN(NEWDEPT(I))
       DO 07 J=1,NUMTEACH
        TREAL=NEWMAT(I,J)
        GROUPMAT(NEWDEPT(I),J)=GROUPMAT(NEWDEPT(I),J)+TREAL/TDENOM
00007    CONTINUE
00006     CONTINUE

       IF (WANTSS .EQ. 1) THEN
       WSS=0
       WDENOM=ROWWT+COLWT
       DO 08 I=1,NUMTEACH
       G=NEWDEPT(I)
       DO 09 J=1,NUMTEACH
        TREAL=NEWMAT(I,J)
        IF (NEWDEPT(J) .EQ. G) THEN
          WT=ROWWT
         ELSE
         WT=COLWT
         END IF
         WT=WT/WDENOM    
       SS(G,J)=SS(G,J)+WT*(TREAL-GROUPMAT(G,J))**2
       WSS=WSS+WT*(TREAL-GROUPMAT(G,J))**2
00009    CONTINUE
00008     CONTINUE


       END IF
        RETURN
        END
     
C             CALL LMEAN(NEWMAT,NEWDEPT,NUMTEACH,GROUPMAT,MAXDEPT,
C     C DEPARTN,I,DIFF(I,J))

      SUBROUTINE LMEAN(NEWDEPT,NUMTEACH,GROUPMAT,MAXDEPT,
     C DEPARTN,THISPER,THISDIFF,ACTRSQR,TSS,ROWWT,COLWT,ZDEPT)
      INTEGER NEWDEPT(251),NUMTEACH,I,J,MAXDEPT,ZDEPT(251),
     C DEPARTN(251),THISPER,THISGRP,ACTRSQR
      REAL GROUPMAT(251,251),THISDIFF,TG,THISN,TREAL1,TSS,
     C ROWWT,COLWT,WDENOM
       REAL ZCOMPMAT(251,251)
       INTEGER HUBERT(251,251),RESULTM(251,251),ALLGROUP(251,251),
     C NEWMAT(251,251)
       REAL BLAUC(251,251),DIFF(251,251)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF

C       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT
       WDENOM=ROWWT+COLWT

       THISGRP=NEWDEPT(THISPER)      
        THISDIFF=0.00
       THISN=DEPARTN(THISGRP)
       DO 5 J=1,NUMTEACH
       TREAL1=NEWMAT(THISPER,J)
      TG=GROUPMAT(THISGRP,J)*THISN/(THISN-1.)
     C - (TREAL1)/(THISN-1.)
       IF ((J .EQ. THISPER) .OR. (ZDEPT(J) .EQ. THISGRP))
     C  THEN
       WT=ROWWT
        ELSE
       WT=COLWT
       END IF
       WT=WT/WDENOM
c      DIFF=DIFF+WT*(TREAL1-GROUPMAT(G,J))**2
       THISDIFF=THISDIFF+WT*(TG-TREAL1)**2
00005  CONTINUE
       IF (ACTRSQR .EQ. 1) THEN
       THISDIFF=THISDIFF*(THISN-1.)/THISN
       END IF

       THISDIFF=TSS-THISDIFF
        RETURN
        END
     

       SUBROUTINE SASCENT (CHANGEC,STOPVAL,LOWMEM,NUMTEACH,
     C MADECHNG,ISO,ISOLIST,NEWDEPT,MEANWT,VARWT,WPIK,
     C NEARVAL,DEPARTN,MAXDEPT,
     C RCHOICE,DMEMBERS,QUITIT,LASTCHNG,NSUBID,
     C ATITLE,MKCOUNT2,PRINTO,BOUNDVAL,PCTILE,
     C DIRECT,TWXPX,DSYMMAT,STARTTRY,IEXPECT,ISTD,ICON,
     C ACTRSQR,NOWCLOSE,STRUCTEQ,QUICKEND,ROWWT,COLWT,TQUANT,
     C SQUAREIT,SOLUT,NETLEV,PERGROUP,INFILE,BLABOUND,MAXCH,MUTDYAD,
     C NONEG,MAXSEED,ATTACHI,HALFDYAD,DISSOLVE,HYPERG)


      CHARACTER ATITLE(3)*20, IHEAD(4)*30,INFILE*16
      REAL IEXPECT(251),ISTD(251),ICON(251),
     C MEANWT(251),VARWT(251),WPIK,NOB,
     C ROWWT,COLWT,FINEVAL,DVAL

      DOUBLE PRECISION SUMCLOSE
      REAL CHOSEMN(251),ONEARVAL,SS(251,251),
     C CHOSEVR(251),AMEAN,AVAR,SRIU2(251),GMEAN(251),GVAR(251),
     C TOTE,TOTCON,TOTV,CURVAR(251),CURMEAN(251),
     C PLUSVAR(251),PLUSMEAN(251),OLDGZ(251),
     C CHANGEZ,IADDCON,XNEWCON,NEWZ,TADDCON,
     C INDIVZ,IE,IVAR,KSIGN,BDENOM,TARGETR,DELTA5

      REAL CDEPT,NEARVAL,BESTD(251),CHANGEC(251),BOUNDVAL,
     C NOWCLOSE(251),LOWCOMP,STOPVAL,LASTCHNG,
     C ORDCLOSE(251),PCTILE,DIRECT,TWXPX,GPCTILE,GNEARVAL,
     C SYMMAT,DSYMMAT,LOWCONN,RBESTD(251),
     C GROUPMAT(251,251),LOWNOW,PENALTY,PENNUM,
     C DIFF(251,251),SRI(251),SSRI(251),XSRI(251),KRATIO,
     C KELGC,KNUMTEAC,XEXPECT,XICON,XISTD,USEBOUND,
     C GVPLUS(251),GMPLUS(251),GVSUB(251),IVSUB(251),
     C GMSUB(251),IMSUB(251),TOBFUN,TSS,WSS,WBSS,XNEARVAL,
     C TIDELTKA(251),IDELTKA,IDELTKB,IDELTKC,IDELTMA,IDELTMB,
     C IDELTMC,IDELTVA,IDELTVB,IDELTVC,DTOTV,DTOTK,DTOTE,
     C TBLAUC,BLAUC(251,251),BLABOUND,
     C TEMPDEN,GCHOICE,NEXTPREF(251),LONER,LOOKP,RPI,DUMBO,
     C TNEWCON,ADDCON,DMAXD,NOPRINT,QUITIT

C     C ALLC2(251,251),TOTPC(251),PROBCON(251,251)
C
C        CALL KMULTMA3(ZCOMPMAT,ELGC,RCHOICE,DIRECT,TWXPX,DSYMMAT,
C     C ONLIST,1,PERSON1,PERSON2,ELGLIST,LOWMEM)

      INTEGER LOWMEM,NUMTEACH,ISO,ISOLIST(251),NSUBID(251),
     C DEPARTN(251),NEWDEPT(251),MAXDEPT,USESEED(251),MAXSEED,
     C RCHOICE(251),MADECHNG,B,HAVEISO,HYPERG,
     C Z,KCOUNT2,MKCOUNT2,PRINTO(100),NUMIN,ELGLIST(251),ELGC,
     C ONLIST(251),DONEO(251),CONMEM,RP1,RP2,ACTRSQR,WANTSS,
     C SOLUT,UNO,NETLEV,KDEPT,PERGROUP,IAMELG(251),BBOUNDG(251,251),
     C BCOUNTG(251),IR,TCHOICE,GC,DONEIT,PAIRUP(251,251),DISBAND(251),
     C KGB,I,J,CHECK,FIXIT,P,P2,BLT,K1,K2,SAMEG1,CHANGEN

      INTEGER SKIPIT,DMEMBERS(251),THISMEM,TEMPS,OLDLOWM,STRUCTEQ,
     C FOUNDONE,NEWMEMS(251),OLDDEPT,LDEPART,LDEPTN,NGROUP,
     C THED(251),NOPICK,DNUM(251),OLOWMEM,INEWGRP(251),HALFDYAD,
     C LGROUP(251),LMAXDEPT,JB,IB,BD,BG,COUNTG(251),BOUNDG(251,251),
     C SLOTA(251),THEVAL,PERSON1,PERSON2,CREATOG(251),TRY,DOIT,
     C RESTART(251),ATTACHL(251),DIDIT,NOTYET,TPER,SLOTB(251),
     C STARTTRY,IH,NOWSTOP,CUTOFF,NEEDNEW,CINEWGRP(251),
     C CCREATOG(251),OLDCON(251),NEWCON(251,251),NEWONE,
     C QUICKEND,CONTIN,HAVESEED,QUANTYPE,SQUAREIT,MAXCH,MUTDYAD,
     C HADTO,NONEG,TEMPCON,THISDEPT,CZ,BZ,ATTACHI,DPLUS1,DISSOLVE,
     C DS,THISND,NEXTTHED(251),OTHER,TKC,ZDEPT(251),
     C SORTFLAG,IER,TQUANT
         REAL ZCOMPMAT(251,251)
       INTEGER ALLGROUP(251,251),HUBERT(251,251),RESULTM(251,251),
     C NEWMAT(251,251)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF


C       WRITE(521,102) "BEGINNING OF ASCENT"
C       WRITE(521,251) NEWMAT
        OPEN(20,file='obfun.dat')
        QUANTYPE=TQUANT
        BLT=0
        PENALTY=0
        IF (QUANTYPE .LT. 0) THEN
        QUANTYPE=ABS(QUANTYPE)
        BLT=1
        PENNUM=NUMTEACH*(NUMTEACH-1)
        PENALTY=-LOG(PENNUM)
        END IF 
        GPCTILE=PCTILE
        GNEARVAL=NEARVAL
         USEBOUND=NEARVAL
         TKC=MKCOUNT2
         LOWNOW=0
C        IF (QUANTYPE .EQ. 4) THEN
C         LOWNOW=-999
C        END IF

          IHEAD(1)="ATTACHING ISOLATES"
          IHEAD(2)="FINISHING ASCENT"
          IHEAD(3)="FINAL PLACEMENTS"

       WRITE(33,03252) 
       WRITE(33,102) "ASCENT"
       WRITE(33,2102) 

       DO 874 KGB=1,MAXDEPT
         INEWGRP(KGB)=0
         CREATOG(KGB)=0
  874  CONTINUE
         K1=0
         K2=0
        DO 00876 KGB=1,NUMTEACH
         K2=K2+RCHOICE(KGB)*MEANWT(KGB)
         USESEED(KGB)=0
         RESTART(KGB)=0
         IEXPECT(KGB)=0
         ICON(KGB)=0
         ISTD(KGB)=0
00876     CONTINUE
         K1=MAXCH*NUMTEACH*(NUMTEACH-1)-K2

C      IF (DEBUG(21) .EQ. 1) THEN 
C      IF (DEBUG(24) .EQ. 1) THEN

      TRY=1
        DO 717 I=1,NUMTEACH
        ATTACHL(I)=1
        DO 7173 GC=1,NUMTEACH
        PAIRUP(I,GC)=0
07173    CONTINUE
00717    CONTINUE

      ONEARVAL=NEARVAL
      NOB=1.00000
      IF (STRUCTEQ .EQ. 1) THEN
         TSS=0
         WANTSS=1
         DMAXD=1
         KDEPT=DEPARTN(1)
         DEPARTN(1)=NUMTEACH
      CALL GETMEAN(ATTACHL,NUMTEACH,GROUPMAT,MAXDEPT,
     C DEPARTN,WANTSS,SS,WSS,1.0,0.0)
          DEPARTN(1)=KDEPT
          DO 010 J=1,NUMTEACH
          TSS=TSS+SS(1,J)
00010      CONTINUE

           TOBFUN=1-WSS/TSS
       END IF

      DO WHILE (TRY .LE. STARTTRY)
      KCOUNT2=0
      MADECHNG=1
      OLDLOWM=0
      ISO=1
      ISOLIST(1)=0
      CONMEM=1
      LOWMEM=1  
      LOWCONN=200.000      
      CHANGEC(LOWMEM)=251.00
      DIDIT=1
      NOWSTOP=0


      DO WHILE ((NOWSTOP .EQ. 0) .AND.
     C (KCOUNT2 .LT. MKCOUNT2) .AND. (DIDIT  .GT. 0))

      LASTCHNG=CHANGEC(LOWMEM)
      LOWCOMP=-200.000
      NOPRINT=0
      SUMCLOSE=0.0      
      NUMIN=0
      ELGC=1
      DIDIT=0

      WANTSS=0
      IF (PRINTO(23) .EQ. 1) THEN
      WANTSS=1
      END IF

      IF (STRUCTEQ .EQ. 1) THEN
      CALL GETMEAN(NEWDEPT,NUMTEACH,GROUPMAT,MAXDEPT,
     C DEPARTN,WANTSS,SS,WSS,ROWWT,COLWT)
      TOBFUN=1-WSS/TSS
      ELSE
C      SUBROUTINE GETRS(NEWMAT,NEWDEPT,NUMTEACH,MAXDEPT,
C     C DEPARTN,WANTSS,RCHOICE,SRI,SSRI,OLDCON,ALLGROUP,XSRI,
C     C MEANWT,VARWT,SRIU2,GMEAN,GVAR)

      CALL GETRS2(NEWDEPT,NUMTEACH,MAXDEPT,
     C DEPARTN,WANTSS,RCHOICE,SRI,SSRI,OLDCON,XSRI,
     C MEANWT,VARWT,SRIU2,GMEAN,GVAR,GVPLUS,GMPLUS,GVSUB,
     C IVSUB,GMSUB,IMSUB,DTOTK,DTOTE,DTOTV,TIDELTKA,QUANTYPE,
     C TOBFUN,SQUAREIT,NETLEV,PERGROUP,PRINTO(23),CURVAR,CURMEAN,
     C PLUSVAR,PLUSMEAN,OLDGZ,HYPERG,BLT,MAXCH,SAMEG1,K1,K2,
     C COLWT,ROWWT)
      END IF
       IF ((QUANTYPE .EQ. 5) .AND. (NETLEV .EQ. 1)) THEN
       LOWNOW=TOBFUN
       END IF 

         HAVESEED=0
      IF (MADECHNG .EQ. 1) THEN
      IF (PRINTO(23) .EQ. 1) THEN
      WRITE(33,89898) "THE OBJECTIVE FUNCTION AT ITERATION",
     C KCOUNT2,  " IS ",TOBFUN
      IF (PRINTO(24) .EQ. 1) THEN
      WRITE(6,89898) "THE OBJECTIVE FUNCTION AT ITERATION",
     C KCOUNT2,  " IS ",TOBFUN
      END IF
C      WRITE(6,89898) "THE OBJECTIVE FUNCTION IS ",TOBFUN
      WRITE(20,251) KCOUNT2,TOBFUN
       END IF
       END IF

      DO 28, I=1,NUMTEACH
         IDELTMB=0
         NEXTPREF(I)=-999
         NEXTTHED(I)=NEWDEPT(I)
         IDELTVB=0
         IDELTKB=0

         IDELTMA=0
         IDELTVA=0
         IDELTKA=0
         SLOTB(I)=I
         SLOTA(I)=I
        J=NEWDEPT(I)
        IF (DEPARTN(J) .GT. 1) THEN
        NEWONE=0
        IDELTVA=GVSUB(J)-IVSUB(I)-GVAR(J)
        IDELTMA=GMSUB(J)-IMSUB(I)-GMEAN(J)
        IDELTKA=-TIDELTKA(I)

C       CALL DIFFCOMP(NEWMAT,I,J,NUMTEACH,NEWCON(I,J),
C     C DEPARTN(J),ACTRSQR,SSRI(J),SRI(J),OLDCON(J),
C     C ALLGROUP,RCHOICE(I),DIFF(I,J),NEWONE,GMEAN(J),GVAR(J),
C     C SRIU2(J),XSRI(J),MEANWT(I),VARWT(I),ROWWT,COLWT,QUANTYPE,
C     C SQUAREIT,IEXPECT(I),ICON(I),ISTD(I),(GVSUB(J)-IVSUB(I)),
C     C (GMSUB(J)-IMSUB(I)),IDELTKA,IDELTMA,IDELTVA,MAXCH)

       NOWCLOSE(I)=TOBFUN - 
     C (DTOTK + IDELTKA+IDELTKB - DTOTE - IDELTMA - IDELTMB)/
     C  (DTOTV + IDELTVA + IDELTVB)**.5

         END IF

       IF (DEPARTN(NEWDEPT(I)) .EQ. 1) THEN 
       NOWCLOSE(I)=LOWNOW+PENALTY
         IF (RESTART(I) .EQ. 0) THEN
         HAVESEED=1
         END IF
       END IF
       IF (ATTACHL(I) .EQ. 1) THEN
        DIDIT=DIDIT+1
       SKIPIT=0
C       WRITE(214,251) MADECHNG
       IF (MADECHNG .EQ. 0 )THEN
         NOPRINT=1
         DO 18 CHECK=1,ISO
          IF ((ISOLIST(CHECK) .GT. 0) .AND.
     C (ISOLIST(CHECK) .LE. 251)) THEN
          NOWCLOSE(ISOLIST(CHECK))=LOWNOW
           END IF
          IF (I .EQ. ISOLIST(CHECK)) THEN
            SKIPIT=1
            DIDIT=DIDIT-1
          END IF
   18    CONTINUE
        ELSE
         DO 19 FIXIT=1,ISO
          ISOLIST(FIXIT)=0
   19    CONTINUE
         ISO=1
       END IF
       IF (SKIPIT .EQ. 0) THEN
       BESTD(I)=-100000.0
       THED(I)=NEWDEPT(I)
      DO 34 J=1,MAXDEPT
       TCHOICE=RCHOICE(I)+SRI(J)
       IF ((TRY .EQ. 1) .OR. (DEPARTN(J) .GE. 1)) THEN
         CHOSEMN(J)=0.000
         CHOSEVR(J)=0.00
       IF (((TRY .NE. 2) .AND.
     C   (((RCHOICE(I) .EQ. 0) .AND. (DEPARTN(J) .LT. 2) .AND.
     C      (MUTDYAD .EQ. 1) .AND. (TRY .NE. 2)) .OR.
     C   ((RCHOICE(I) .EQ. 0) .AND. (DEPARTN(J) .LT. 2) .AND.
     C    (PAIRUP(I,ALLGROUP(J,1)) .EQ. 1)) .OR.
     C ((RCHOICE(ALLGROUP(J,1)) .EQ. 0) .AND. (DEPARTN(J) .LT. 2) .AND.
     C    (PAIRUP(ALLGROUP(J,1),I) .EQ. 1)))) .OR.
     C    (((J .EQ. NEWDEPT(I)) .AND. (DEPARTN(NEWDEPT(I)) .EQ. 1)))
     C   .OR. (DEPARTN(J) .LT. 1) .OR. (TCHOICE .LE. 0))
     C    THEN
         CDEPT=-251.00
         IF (RESTART(I) .EQ. 0) THEN
         HAVESEED=1
         END IF
         IF (J .EQ. NEWDEPT(I)) THEN
          NOWCLOSE(I)=LOWNOW+PENALTY
         END IF
       ELSE
     
       IF (STRUCTEQ .EQ. 1) THEN
C      SUBROUTINE DIFFMEAN(I,GROUPMAT,G,NUMTEACH,DIFF,THISN,
C     C ACTRSQR,TSS,MAXDEPT,ROWWT,COLWT,THISD,NEWDEPT)

       CALL DIFFMEAN(I,GROUPMAT,J,NUMTEACH,DIFF(I,J),
     C DEPARTN(J),ACTRSQR,TSS,MAXDEPT,ROWWT,COLWT,ZDEPT)
       ELSE
       NEWONE=1
C       SUBROUTINE DIFFCOMP(NEWMAT,I,J,NUMTEACH,NEWCON,
C     C DEPARTN,ACTRSQR,SSRI,SRI,OLDCON,ALLGROUP,
C     C RCHOICE,CONNECT,NEWONE,GMEAN,GVAR,SRIU2,XSRI,THISMW,
C     C THISVW)
       IF (NETLEV .NE. 2) THEN
       CALL DIFFCOMP(I,J,NUMTEACH,NEWCON(I,J),
     C DEPARTN(J),ACTRSQR,SSRI(J),SRI(J),OLDCON(J),
     C RCHOICE(I),DIFF(I,J),NEWONE,GMEAN(J),GVAR(J),
     C SRIU2(J),XSRI(J),MEANWT(I),VARWT(I),ROWWT,COLWT,QUANTYPE,
     C SQUAREIT,XEXPECT,XICON,XISTD,GVPLUS(J),GMPLUS(J),IDELTKB,
     C IDELTMB,IDELTVB,PERGROUP,TBLAUC,DISBAND(I),HYPERG,MAXCH)
       
       BLAUC(I,J)=TBLAUC
       END IF
C       NETLEV .NE. 2

       END IF

C  NETLEV .EQ. 2 IF NOT IN SAME GROUP
       IF (NETLEV .EQ. 2) THEN
        DIFF(I,J)=0.000
        CHANGEZ=0.0000
        IADDCON=0.000
        XNEWCON=0.000
         NEWZ=0.0000
        DO 9889 P=1,DEPARTN(J)
         NEWZ=0.0000
        THISMEM=ALLGROUP(J,P)
        IF (RCHOICE(THISMEM) .GT. 0.000) THEN
        IADDCON=IADDCON + NEWMAT(I,THISMEM)
        TADDCON=NEWMAT(THISMEM,I)
         XNEWCON=XNEWCON+TADDCON

        
        IF ((QUANTYPE .EQ. 1) .AND. (PLUSVAR(THISMEM) .GT. 0.000))
     C   THEN
        NEWZ=((TIDELTKA(THISMEM)+TADDCON)-PLUSMEAN(THISMEM)) /
     C              (PLUSVAR(THISMEM)**.5)
        END IF        
        IF ((QUANTYPE .EQ. 2) .AND. (PLUSMEAN(THISMEM) .GT. 0)) THEN
        NEWZ=((TIDELTKA(THISMEM)+TADDCON)-PLUSMEAN(THISMEM)) /
     C              (PLUSMEAN(THISMEM))**.5
        END IF
         IF (QUANTYPE .EQ. 3) THEN
         KSIGN=TADDCON/PLUSMEAN(THISMEM)
         IF (KSIGN .GT. 0) THEN
         NEWZ=2*TADDCON*LOG(KSIGN)
         END IF
         END IF
        CHANGEZ=CHANGEZ+NEWZ
        END IF
09889    CONTINUE
       INDIVZ=0.000
       IF (RCHOICE(I) .GT. 0)  THEN
       IE=MEANWT(I)*RCHOICE(I)*DEPARTN(J)/(NUMTEACH-1.)
       IVAR=(RCHOICE(I)*DEPARTN(J)*((NUMTEACH-1.)-DEPARTN(J))*
     C (MEANWT(I)**2/(NUMTEACH-1.)**2) *
     C  (1. + (NUMTEACH-1.)*VARWT(I)/((NUMTEACH-1.-DEPARTN(J))*
     C  MEANWT(I)**2) - HYPERG*((RCHOICE(I)-1.)/(NUMTEACH-2.))))
        IF ((QUANTYPE .EQ. 1) .AND. (IVAR .GT. 0)) THEN
        INDIVZ=(IADDCON-IE)/SQRT(IVAR)
         END IF
        IF ((QUANTYPE .EQ. 2) .AND. (IE .GT. 0)) THEN
        INDIVZ=(IADDCON-IE)/IE**.5
         END IF
         IF (QUANTYPE .EQ. 3) THEN
         KSIGN=IADDCON/IE
         IF (KSIGN .GT. 0) THEN
         INDIVZ=2*IADDCON*LOG(KSIGN)
         END IF
         END IF
   
         END IF
          IF (RCHOICE(I) .LT. DEPARTN(J)) THEN
           BDENOM=RCHOICE(I)
          ELSE
           BDENOM=DEPARTN(J)
          END IF

          BLAUC(I,J)=(COLWT*XNEWCON+ROWWT*IADDCON)/
     C     (BDENOM + DEPARTN(J))
         TEMPCON=XNEWCON+IADDCON
         DIFF(I,J)=(CHANGEZ+INDIVZ)-OLDGZ(J)
         IF (ABS(TEMPCON) .LT. .0001) THEN
         DIFF(I,J) =-999
         END IF
         END IF
C        IF NETLEV .EQ. 2
C  NETLEV .EQ. 2 IF NOT IN SAME GROUP

       IF (NETLEV .EQ. 1) THEN
        IF (QUANTYPE .EQ. 1) THEN
        DIFF(I,J)=-TOBFUN +
     C  (DTOTK + IDELTKA+IDELTKB - DTOTE - IDELTMA - IDELTMB)/
     C  (DTOTV + IDELTVA + IDELTVB)**.5
        END IF
        IF (QUANTYPE .EQ. 5) THEN
        DELTA5=IDELTKA+IDELTKB+DTOTK
        CHANGEN=(SAMEG1+2*MAXCH*(DEPARTN(J)-DEPARTN(NEWDEPT(I))+1))
     C   -DELTA5*COLWT
        IF (COLWT .LT. .0001) THEN
        DIFF(I,J)=DELTA5/CHANGEN
        ELSE
        DIFF(I,J)= DELTA5*(K1-CHANGEN)/((K2-DELTA5)*CHANGEN)
        END IF
        END IF
        IF (ABS(XICON) .LT. .0001) THEN
        DIFF(I,J)=-999
        END IF
       END IF
        IF ((DEPARTN(NEWDEPT(I)) .LT. 2) .AND. (BLT .EQ. 1)) THEN
         DIFF(I,J)=DIFF(I,J)-PENALTY
        END IF

        IF (DEPARTN(J) .EQ. 1) THEN
        IF (HALFDYAD .EQ. 1) THEN
        DIFF(I,J)=DIFF(I,J)/2.
        END IF
        IF (HALFDYAD .EQ. 2) THEN
        DIFF(I,J)=-999
        END IF
        END IF
   
         CDEPT=DIFF(I,J)
       IF ((J .NE. NEWDEPT(I)) .AND. (CDEPT .GT. NEXTPREF(I))) THEN
       NEXTPREF(I)=CDEPT
       NEXTTHED(I)=J
       END IF

        IF (J .EQ. NEWDEPT(I)) THEN
        IF (DEPARTN(J) .GT. 1) THEN
        IF (STRUCTEQ .EQ. 1) THEN
       CALL LMEAN(NEWDEPT,NUMTEACH,GROUPMAT,MAXDEPT,
     C DEPARTN,I,DIFF(I,J),ACTRSQR,TSS,ROWWT,COLWT,ZDEPT)
       ELSE
C  NETLEV 2 FOR SAME GROUP
       IF (NETLEV .EQ. 2) THEN
        CHANGEZ=0.000
        XNEWCON=0.000
        IF (DEPARTN(J) .GT. 2) THEN
        DO 9887 P=1,DEPARTN(J)
        THISMEM=ALLGROUP(J,P)
        IF ((RCHOICE(THISMEM) .GT. 0) .AND. (THISMEM .NE. I)) THEN
        ADDCON=NEWMAT(THISMEM,I)
         XNEWCON=XNEWCON+ADDCON
        NEWZ=0.000
        IF ( (QUANTYPE .EQ. 1) .AND. (IVSUB(THISMEM) .GT. 0)) THEN
        NEWZ=((TIDELTKA(THISMEM)-ADDCON)-IMSUB(THISMEM)) /
     C              (IVSUB(THISMEM)**.5)
        END IF
        IF ( (QUANTYPE .EQ. 2) .AND. (IMSUB(THISMEM) .GT. 0)) THEN
        NEWZ=((TIDELTKA(THISMEM)-ADDCON)-IMSUB(THISMEM)) /
     C              (IMSUB(THISMEM)**.5)
        END IF      
         IF (QUANTYPE .EQ. 3) THEN
         KSIGN=(TIDELTKA(THISMEM)-ADDCON)/IMSUB(THISMEM)
         IF (KSIGN .GT. 0) THEN
         NEWZ=2*(TIDELTKA(THISMEM)-ADDCON)*LOG(KSIGN)
         END IF
         END IF
        CHANGEZ=CHANGEZ+(NEWZ)
         END IF
09887    CONTINUE

         END IF
C         IF (DEPARTN(J) .GT. 2) THEN

         INDIVZ=0.000
         IF ((RCHOICE(I) .GT. 0) .AND. (CURVAR(I) .GT. 0)) THEN
         INDIVZ=(TIDELTKA(I)-CURMEAN(I))/SQRT(CURVAR(I))
         END IF
         DIFF(I,J)=OLDGZ(J) -(CHANGEZ)
         IF (DEPARTN(J) .EQ. 2) THEN
         IF (ALLGROUP(J,1) .EQ. I) THEN
         XNEWCON=NEWMAT(ALLGROUP(J,2),I)
          ELSE
         XNEWCON=NEWMAT(ALLGROUP(J,1),I)
         END IF
          END IF
          
         TNEWCON=XNEWCON+TIDELTKA(I)
         IF (ABS(TNEWCON) .LT. .0001) THEN
         DIFF(I,J)=-999
         END IF

          TEMPDEN=DEPARTN(J) - 1.
          IF (RCHOICE(I) .LT. TEMPDEN) THEN
           BDENOM=RCHOICE(I)
          ELSE
           BDENOM=TEMPDEN
          END IF
          TBLAUC=(COLWT*XNEWCON+ROWWT*TIDELTKA(I))/
     C                    (BDENOM + TEMPDEN)
         END IF
C       IF (NETLEV .EQ. 2) THEN
C  NETLEV 2 FOR SAME GROUP

       IF (NETLEV .EQ. 1) THEN
        IDELTKA=-TIDELTKA(I)
        IF (QUANTYPE .EQ. 1) THEN
        IDELTVA=GVSUB(J)-IVSUB(I)-GVAR(J)
        IDELTMA=GMSUB(J)-IMSUB(I)-GMEAN(J)
        DIFF(I,J)=TOBFUN - (DTOTK + IDELTKA - DTOTE - IDELTMA)/
     C  (DTOTV + IDELTVA)**.5
        END IF
        IF (QUANTYPE .EQ. 5) THEN
        DIFF(I,J)=TOBFUN
C        DELTA5=IDELTKA+DTOTK
C        CHANGEN=SAMEG1-2*(DEPARTN(NEWDEPT(I))-1)-DELTA5
C        DIFF(I,J)=TOBFUN-
C     C  DELTA5*(K1-CHANGEN)/((K2-DELTA5)*CHANGEN)
        END IF
        IF (ABS(IDELTKA) .LT. .0001) THEN
         DIFF(I,J)=-999
        END IF
          TEMPDEN=DEPARTN(J) - 1.
          IF (RCHOICE(I) .LT. TEMPDEN) THEN
           BDENOM=RCHOICE(I)
          ELSE
           BDENOM=TEMPDEN
          END IF
          TBLAUC=(TIDELTKA(I))/
     C                    (BDENOM + TEMPDEN)
C       TBLAUC TO HERE

       ELSE
        NEWONE=0
       IF (NETLEV .NE. 2) THEN
         DISBAND(I)=0
       CALL DIFFCOMP(I,J,NUMTEACH,NEWCON(I,J),
     C DEPARTN(J),ACTRSQR,SSRI(J),SRI(J),OLDCON(J),
     C RCHOICE(I),DIFF(I,J),NEWONE,GMEAN(J),GVAR(J),
     C SRIU2(J),XSRI(J),MEANWT(I),VARWT(I),ROWWT,COLWT,QUANTYPE,
     C SQUAREIT,IEXPECT(I),ICON(I),ISTD(I),(GVSUB(J)-IVSUB(I)),
     C (GMSUB(J)-IMSUB(I)),IDELTKA,IDELTMA,IDELTVA,PERGROUP,TBLAUC,
     C  DISBAND(I),HYPERG,MAXCH)
        END IF
C         NETLEV .NE. 2

        END IF
         END IF
        ELSE
         DIFF(I,J)=0.0

         IF (RESTART(I) .EQ. 0) THEN
         HAVESEED=1
         END IF
        END IF
        IF ((DIFF(I,J) .GT. 999999) .OR. (DIFF(I,J) .LT. -999999))
     C THEN
        DIFF(I,J) = 0.0000000
         END IF
        IF (DEPARTN(J) .EQ. 2) THEN
         IF (BLT .EQ. 1) THEN
         DIFF(I,J)=DIFF(I,J)+PENALTY
          END IF

        IF (HALFDYAD .EQ. 1) THEN
        DIFF(I,J)=DIFF(I,J)/2.
        END IF
        IF (HALFDYAD .EQ. 2) THEN
        DIFF(I,J)=-999
        END IF
        END IF

         CDEPT=DIFF(I,J)
         BLAUC(I,J)=TBLAUC
C         ALLC(I,J)=DIFF(I,J)
         NUMIN=NUMIN+1
         ORDCLOSE(NUMIN)=CDEPT
         NOWCLOSE(I)=CDEPT
         SUMCLOSE=SUMCLOSE+NOWCLOSE(I)
         IF (DEPARTN(NEWDEPT(I)) .LT. 2) THEN 
         SUMCLOSE=SUMCLOSE-NOWCLOSE(I)+PENALTY
          NUMIN=NUMIN-1
          NOWCLOSE(I)=LOWNOW+PENALTY
C          ALLC(I,J)=0.0
          BLAUC(I,J)=0.0
          DUMBO=0
         IF (RESTART(I) .EQ. 0) THEN
         HAVESEED=1
         END IF


         END IF
        END IF
        END IF
C        IF RHCOICE .GT. 0 AND DEPARTN .LT. 2
        IF ((CDEPT .GT. BESTD(I)) .AND. (CDEPT .LT. 999999)) THEN
         IF ((NETLEV .EQ. 1) .AND. (J .EQ. NEWDEPT(I)) .AND. 
     C   (QUANTYPE .NE. 5)) THEN
         DUMBO=0
         ELSE
         THED(I)=J
         BESTD(I)=CDEPT
         IF (DEPARTN(J) .GE. 2) THEN
         RBESTD(I)=CDEPT
         END IF
         END IF
C        NETLEV EQ 1
         END IF
C         CDEPT GT BESTD
         END IF
C          IF ((TRY .EQ. 1) .OR. (DEPARTN(J) .GT. 1)) THEN
   34 CONTINUE
       IF ((NETLEV .EQ. 1) .AND. (QUANTYPE .NE. 5)) THEN
       CHANGEC(I)=BESTD(I)
       ELSE
       CHANGEC(I)=BESTD(I)-NOWCLOSE(I)
        END IF
C       WRITE(212,251) I,CHANGEC(I),BESTD(I),NOWCLOSE(I),LOWCOMP,
C     C  THED(I),NEWDEPT(I)
C       IF (BESTD(I) .LT. LOWCONN) THEN
C       CONMEM=I
C       LOWCONN=BESTD(I)
C       END IF

       IF (CHANGEC(I) .GT. LOWCOMP) THEN
        LOWCOMP=CHANGEC(I)
        LOWMEM=I
       END IF
      END IF
      END IF
   28 CONTINUE

      SORTFLAG=2
      IF (NUMIN .GT. 1) THEN
      CALL SPSORT(ORDCLOSE,NUMIN,SLOTA,SORTFLAG,IER)
      END IF
      IF (PCTILE .LE. 1) THEN
        THEVAL=INT(PCTILE*NUMIN)
        IF (THEVAL .GE. 1) THEN
          NEARVAL=ORDCLOSE(THEVAL)
          FINEVAL=ORDCLOSE(NUMIN-THEVAL)
        END IF
      END IF
C      NEARVAL=SUMCLOSE/NUMIN
       XNEARVAL=.0001
       IF (NEARVAL .GT. XNEARVAL) THEN
       XNEARVAL=NEARVAL
       END IF
         DO 2228 I=1,NUMTEACH
         IAMELG(I)=0
       IF ((USESEED(I) .LT. MAXSEED) .AND. 
     C (RCHOICE(I) .GT. 0) .AND. ((NOWCLOSE(I) .LE. XNEARVAL) .OR. 
     C        (DEPARTN(NEWDEPT(I)) .LE. 1))) THEN
          ELGLIST(ELGC)=I
          ELGC=ELGC+1
          IAMELG(I)=1
         END IF
02228     CONTINUE
      ELGC=ELGC-1
C      CHECK HERE
       IF ((TRY .EQ. 2) .OR. 
     C ((TRY .GT. 1) .AND. (STRUCTEQ .EQ. 0))) THEN 
       NEARVAL=-999
       END IF
       IF ((QUANTYPE .EQ. 4) .AND. (TRY .EQ. 3)) THEN
       NEARVAL=0
       END IF

       KELGC=ELGC
       KNUMTEAC=NUMTEACH
      KRATIO=KELGC/KNUMTEAC
       KELGC=NUMTEACH-NUMIN
      CONTIN=0
      IF (((THED(LOWMEM) .EQ. NEWDEPT(LOWMEM)) .OR. 
     C     (CHANGEC(LOWMEM) .LE. STOPVAL)) .AND. 
     C  (KELGC .GE. 3) .AND. (HAVESEED .EQ. 1) .AND. (TRY .LE. 1))
     C  THEN 
C        BESTD(LOWMEM)=NEARVAL-.2
        CONTIN=1
      END IF

      IF ((CHANGEC(LOWMEM) .GT. STOPVAL) .OR. (CONTIN .EQ. 1) .OR.
     C (DEPARTN(NEWDEPT(LOWMEM)) .EQ. 1)) THEN
      OLDLOWM=LOWMEM       
        IF ((NOPRINT .EQ. 0) .AND. (PRINTO(1) .EQ. 1)) THEN
        WRITE(33,132)
       WRITE(33,2101) " (N)    "
        DO 80 Z=1,MAXDEPT
          IF (DEPARTN(Z) .GT. 0) THEN
          WRITE(33,133) Z , (NSUBID(ALLGROUP(Z,P)) , P=1,DEPARTN(Z))
          WRITE(33,134) DEPARTN(Z),
     C         (NOWCLOSE(ALLGROUP(Z,P2)), P2=1,DEPARTN(Z))
          WRITE(33,137) "BEST G",
     C         (BESTD(ALLGROUP(Z,P2)), P2=1,DEPARTN(Z))
          WRITE(33,2137) "GROUP",
     C         (THED(ALLGROUP(Z,P2)), P2=1,DEPARTN(Z))
         END IF
   80   CONTINUE
        END IF

        IF (QUICKEND .LT. 1) THEN



        IF (PRINTO(8) .EQ. 1) THEN
        WRITE(33,139) NSUBID(LOWMEM),NEWDEPT(LOWMEM),THED(LOWMEM)
        IF (CONTIN .EQ. 1) THEN
      WRITE(33,01349) NSUBID(LOWMEM),THED(LOWMEM),CHANGEC(LOWMEM)
       END IF
        END IF

C
C
C       NOW I HAVE FOUND THE BEST PERSON TO CHANGE (LOWMEM) AND THE 
C        BEST DEPARTMENT FOR THAT PERSON THED(LOWMEM)
C        AND THE DISTANCE TO THAT DEPARTMENT (BESTD(LOWMEM))
C        IS IT GOOD ENOUGH? 
C
C         MUTDYAD=0
         NOPICK=0
       TCHOICE=RCHOICE(LOWMEM)+SRI(THED(LOWMEM))
      IF (TCHOICE .LE. 0) THEN
       NOPICK=1
       DUMBO=0
       END IF
      IF ((TRY .NE. 2) .AND. (MUTDYAD .EQ. 1)) THEN
      IF ((RCHOICE(LOWMEM) .EQ. 0) .AND. 
     C   (DEPARTN(THED(LOWMEM)) .LT. 2)) THEN
         NOPICK = 1
         DUMBO=0
      END IF
      IF ((DEPARTN(THED(LOWMEM)) .LT. 2)  .AND. 
     C    (RCHOICE(ALLGROUP(THED(LOWMEM),1)) .LT. 1)) THEN
       NOPICK=1
       DUMBO=0
      END IF
      END IF


C        IF ((QUANTYPE .NE. 5) .AND. 
      
        IF ((NOPICK .EQ. 1) .OR. (LOWMEM .LT. 1) .OR. 
     C (LOWMEM .EQ. OLOWMEM)) THEN
        MADECHNG=0
        ISOLIST(ISO)=LOWMEM
        ISO=ISO+1
      ELSE
      IF ((BESTD(LOWMEM) .GT. NEARVAL) .AND. (CONTIN .EQ. 0)) THEN
C    .AND.  (CREATOG(NEWDEPT(LOWMEM)) .EQ. 0)) THEN
          NEEDNEW=0
          MADECHNG=1
          OLOWMEM=LOWMEM
          LONER=0
          OLDDEPT=NEWDEPT(LOWMEM)
        IF ((DISSOLVE .GT. 1) .AND. (DEPARTN(OLDDEPT) .LE. DISSOLVE))
     C     THEN
          DO 5844 DS=1,DEPARTN(OLDDEPT)
          OTHER= ALLGROUP(OLDDEPT,DS)
          IF (OTHER .NE. LOWMEM) THEN
          THISND=NEXTTHED(OTHER)
          IF ((PRINTO(23) .EQ. 1) .OR. (PRINTO(8) .EQ. 1)) THEN
          WRITE(33,5843) NSUBID(OTHER),THISND,OLDDEPT
          IF (PRINTO(24) .EQ. 1) THEN
          WRITE(6,5843) NSUBID(OTHER),THISND,OLDDEPT
          END IF
          END IF

          NEWDEPT(OTHER)=THISND
          DEPARTN(THISND)=DEPARTN(THISND)+1
          INEWGRP(THISND) = 0
          INEWGRP(THISND)=0
          CREATOG(THISND)=0
          END IF
05844      CONTINUE
         END IF
C          LDEPART=NEWDEPT(LOWMEM)
           GCHOICE=SRI(OLDDEPT)-RCHOICE(LOWMEM)*MEANWT(LOWMEM)
           DONEIT=0
          IF ((GCHOICE .LT. 1) .AND. (STRUCTEQ .EQ. 0)) THEN
          DO 2423 GC=1,DEPARTN(OLDDEPT)
          PAIRUP(ALLGROUP(OLDDEPT,GC),LOWMEM)=1
          IF (ALLGROUP(OLDDEPT,GC) .NE. LOWMEM) THEN
           IF (DONEIT .EQ. 1) THEN
          MAXDEPT=MAXDEPT+1
          NEWDEPT(ALLGROUP(OLDDEPT,GC))=MAXDEPT
          ELSE
          DONEIT =1
          END IF
          END IF
02423      CONTINUE
          END IF
          LDEPTN=DEPARTN(NEWDEPT(LOWMEM))
          NEWDEPT(LOWMEM)=THED(LOWMEM)
          DEPARTN(THED(LOWMEM))=DEPARTN(THED(LOWMEM))+1
          INEWGRP(OLDDEPT) = 0
          INEWGRP(THED(LOWMEM))=0
          CREATOG(THED(LOWMEM))=0
         ELSE 
        IF ((PRINTO(8) .EQ. 1) .AND. (CONTIN .EQ. 0)) THEN
         WRITE(33,2339) NSUBID(LOWMEM),BESTD(LOWMEM),
     C   THED(LOWMEM),NEARVAL
        END IF

         IF (TRY .LE. 1) THEN 
         NOTYET=0
         IF (ELGC .GT. 0) THEN
         SORTFLAG=2
         IF (NUMTEACH .GT. 1) THEN
         CALL SPSORT(RBESTD,NUMTEACH,SLOTB,SORTFLAG,IER)
         END IF
         LOOKP=1
         DO WHILE ((NOTYET .EQ. 0) .AND. (LOOKP .LE. NUMTEACH))
           TPER=SLOTB(LOOKP)
           IF  ((RESTART(TPER) .EQ. 1) .OR. (TRY .GT. 1)) THEN
            LOOKP=LOOKP + 1
           ELSE
            NOTYET=1
            CONMEM=TPER
           END IF
          END DO
          END IF
                   
            IF (NOTYET .EQ. 0) THEN
             MKCOUNT2=KCOUNT2-5
             WRITE(33,89898) "ITERATIONS ENDED BECAUSE A NEW GROUP WAS"
             WRITE(33,89898) "CALLED FOR BUT THERE ARE NOT ENOUGH "
             WRITE(33,89898) "ELIGIBLE SEED ACTORS."
           ELSE
         IF (ELGC .GT. 2) THEN
         
         CALL KMULTMA3(ELGC,RCHOICE,DIRECT,COLWT,ROWWT,
     C             ONLIST,1,PERSON1,PERSON2,ELGLIST,CONMEM,NUMTEACH)
         ELSE
         PERSON1=ELGLIST(1)
         IF (ELGC .GT. 1) THEN
         PERSON2=ELGLIST(2)
         ELSE
         PERSON2=CONMEM
         END IF
         END IF

             RESTART(CONMEM)=1
             USESEED(CONMEM)=USESEED(CONMEM)+1
             NEEDNEW=1
             OLDDEPT=NEWDEPT(CONMEM)
             OLOWMEM=CONMEM
             MADECHNG=1
             MAXDEPT=MAXDEPT+1
             IF (PERSON1 .GT. 0) THEN
             USESEED(PERSON1)=USESEED(PERSON1)+1
             NEWDEPT(PERSON1)=MAXDEPT
             END IF
             IF (PERSON2 .GT. 0) THEN
             USESEED(PERSON2)=USESEED(PERSON2)+1
             NEWDEPT(PERSON2)=MAXDEPT
             END IF
             NEWDEPT(CONMEM)=MAXDEPT
             INEWGRP(MAXDEPT)=1
             CREATOG(MAXDEPT)=1
        IF (PRINTO(8) .EQ. 1) THEN
             WRITE(33,1339) (CUTOFF+1),NSUBID(CONMEM),
     C NSUBID(PERSON1),NSUBID(PERSON2)
        END IF
        IF ((PRINTO(23) .EQ. 1) .AND. (PRINTO(24) .EQ. 1)) THEN
             WRITE(6,1339) (CUTOFF+1),NSUBID(CONMEM),
     C NSUBID(PERSON1),NSUBID(PERSON2)
        END IF


           END IF
C           ENDING ON ELSE CALL KMULTMAT
C            WHICH IS REALLY IF NOTYET .EQ. 0

         END IF
C          ENDING ON IF TRY .LE. 1
         IF (TRY .GT. 1) THEN
             MADECHNG=0
             ISOLIST(ISO)=LOWMEM
C              WRITE(214,251) ISO,ISOLIST(ISO)
             ISO=ISO+1
         END IF

         END IF
C        IF (BESTD .GT. NEARVAL)


        END IF

C        IF NOPICK

      IF (MADECHNG .EQ. 1) THEN
        ISO=1
C       IF (DEBUG(23) .EQ. 1) THEN
        KCOUNT2=KCOUNT2+1

        IF (TRY .EQ. 2) THEN
        ATTACHL(LOWMEM)=0
        END IF

        CALL ASSIGN(NUMTEACH,NEWDEPT,MAXDEPT,
     C  DEPARTN,RCHOICE,MEANWT,VARWT,
     C  1,2) 
        CALL REMOVEO4(MAXDEPT,DEPARTN,LGROUP,1,CUTOFF,QUICKEND)
        DO 56 RP1=1,NUMTEACH
          NEWDEPT(RP1)=LGROUP(NEWDEPT(RP1))
00056    CONTINUE
        CALL ASSIGN(NUMTEACH,NEWDEPT,MAXDEPT,
     C  DEPARTN,RCHOICE,MEANWT,VARWT,
     C  1,2) 
C        END IF

C     IF ((DEPARTN(THED(LOWMEM)) .LT. 2)  .AND. 
C     C    (RCHOICE(ALLGROUP(THED(LOWMEM),1)) .LT. 1)) THEN NOPICK=1

        IF ((PRINTO(8) .EQ. 1) .AND. (NEEDNEW .EQ. 0) ) THEN
        WRITE(33,136) NSUBID(LOWMEM),OLDDEPT,NEWDEPT(LOWMEM),KCOUNT2
        WRITE(33,102) 
        IF (DONEIT .EQ. 1) THEN
        WRITE(33,1364) OLDDEPT
        WRITE(33,102) 
        END IF
        END IF

         OLDDEPT=LGROUP(OLDDEPT)
C         LDEPART=LGROUP(LDEPART)

        DO 780 Z=1,MAXDEPT
         CINEWGRP(Z)=INEWGRP(Z)
         CCREATOG(Z)=CREATOG(Z)
00780     CONTINUE
 
       DO 480 Z=1,MAXDEPT
         INEWGRP(Z)=CINEWGRP(LGROUP(Z))
         CREATOG(Z)=CCREATOG(LGROUP(Z))
00480     CONTINUE

      END IF
C    madechnge = 1
      ELSE 
      NOWSTOP=1
      END IF
C      QUICKEND
      ELSE
       NOWSTOP=1
      END IF
C      (CHANGEC .GT. STOPVAL)
      END DO
C     WQWQ
        
          WRITE(33,1747) KCOUNT2, " ITERATIONS FOR THIS PHASE"
          WRITE(33,102)
        IF (TRY .EQ. 1) THEN 
       HAVEISO=0
       B=MAXDEPT
       DO WHILE ((HAVEISO .EQ. 0) .AND. (B .GT. 0) )
       IF (DEPARTN(B) .EQ. 1) THEN
       HAVEISO=1
       END IF
       B=B-1
       END DO
       END IF



        IF ((QUICKEND .NE. 1) .AND. (STRUCTEQ .NE. 1)) THEN
        HADTO=0
        DO 987 CZ=1,NUMTEACH
         IF (DEPARTN(NEWDEPT(CZ)) .GT. 1) THEN
         TEMPCON=0
         THISDEPT=NEWDEPT(CZ)
         DO 3368 BZ=1,DEPARTN(THISDEPT)
         IF (ALLGROUP(THISDEPT,BZ) .NE. CZ) THEN
         TEMPCON=TEMPCON+NEWMAT(CZ,ALLGROUP(THISDEPT,BZ))+
     C   NEWMAT(ALLGROUP(THISDEPT,BZ),CZ)
         END IF
03368    CONTINUE
         IF (TEMPCON .LE. 0) THEN
         HADTO=1
         MAXDEPT=MAXDEPT+1
         IF (PRINTO(8) .EQ. 1) THEN
         WRITE(33,3341) CZ,NEWDEPT(CZ)
         END IF
         NEWDEPT(CZ)=MAXDEPT
         END IF
         END IF
C        DEPARTN(NEWDEPT(CZ) .GT. 1)

        IF ((TRY .GE. 2) .OR. (DEPARTN(NEWDEPT(CZ)) .LT. 2)) THEN
        ATTACHL(CZ)=1
        ELSE
        ATTACHL(CZ)=0
        END IF
00987    CONTINUE

         IF (HADTO .EQ. 1) THEN
        CALL ASSIGN(NUMTEACH,NEWDEPT,MAXDEPT,
     C  DEPARTN,RCHOICE,MEANWT,VARWT,
     C  1,2) 

           UNO=1
        CALL REMOVEO4(MAXDEPT,DEPARTN,LGROUP,UNO,CUTOFF,QUICKEND)
        DO 6651 RP1=1,NUMTEACH
         NEWDEPT(RP1)=LGROUP(NEWDEPT(RP1))
         ZDEPT(RPI)=NEWDEPT(RPI)
06651    CONTINUE

        CALL ASSIGN(NUMTEACH,NEWDEPT,MAXDEPT,
     C  DEPARTN,RCHOICE,MEANWT,VARWT,
     C  1,2) 
         END IF
        END IF
        IF ((QUICKEND .EQ. 1) .OR. 
     C     ((HADTO .EQ. 0) .AND. (HAVEISO .EQ. 0))) THEN
        TRY=TRY+4
        IF (QUICKEND .EQ. 1) THEN
        WRITE(33,102) "ASKED FOR QUICK END"
        ELSE
        WRITE(33,102) "NO ISOLATES, PHASES COMPLETE"
        END IF

        ELSE

          WRITE(33,102) IHEAD(TRY)
          WRITE(33,102)
          IF (PRINTO(24) .EQ. 1) THEN
          WRITE(6,102) IHEAD(TRY)
          WRITE(6,102)
          END IF
        END IF

        DVAL=ABS(BOUNDVAL-FINEVAL)
C        IF ((PCTILE .LT. 1) .AND. (DVAL .LT. BOUNDVAL) .AND.
C     C   (USEBOUND .LT. BOUNDVAL)) THEN
C        USEBOUND=FINEVAL
C        ELSE
        USEBOUND=BOUNDVAL
C        END IF

C        BLABOUND=.05
         IF (STRUCTEQ .EQ. 0) THEN
        PCTILE=2.0
        NEARVAL=-999
        END IF
        IF (QUANTYPE .EQ. 4) THEN
        PCTILE=2.0
        IF (TRY .EQ. 1) THEN
        NEARVAL=-999
        ELSE
        NEARVAL=0
        END IF
        END IF

        
        TRY=TRY+1
        IF (ATTACHI .EQ. 0) THEN
        TRY=TRY+4
         END IF

C         QUICKEND=1

        DIDIT=1
        CHANGEC(LOWMEM)=STOPVAL+10000.000
         END DO

          WRITE(33,132)
       WRITE(33,2101) " (N)    "
        DO 82 Z=1,MAXDEPT
          IF (DEPARTN(Z) .GT. 0) THEN
          WRITE(33,133) Z , (NSUBID(ALLGROUP(Z,P)) , P=1,DEPARTN(Z))
          WRITE(33,134) DEPARTN(Z),
     C         (NOWCLOSE(ALLGROUP(Z,P2)), P2=1,DEPARTN(Z))
          WRITE(33,137) "BEST G",
     C         (BESTD(ALLGROUP(Z,P2)), P2=1,DEPARTN(Z))
          WRITE(33,2137) "GROUP",
     C         (THED(ALLGROUP(Z,P2)), P2=1,DEPARTN(Z))
           END IF
          DNUM(Z)=Z
   82   CONTINUE
       LMAXDEPT=MAXDEPT
         HADTO=0
         DO 00088 IB=1,NUMTEACH
        IF ((NONEG .EQ. 1) .AND. (NOWCLOSE(IB) .LT. 0)) THEN
        HADTO=1
        MAXDEPT=MAXDEPT+1
        WRITE(33,1365) IB,NEWDEPT(IB)
        NEWDEPT(IB)=MAXDEPT
        END IF
00088           CONTINUE
         IF (HADTO .EQ. 1) THEN
        CALL ASSIGN(NUMTEACH,NEWDEPT,MAXDEPT,
     C  DEPARTN,RCHOICE,MEANWT,VARWT,
     C  1,2) 
         END IF

           UNO=1
        CALL REMOVEO4(MAXDEPT,DEPARTN,LGROUP,UNO,CUTOFF,QUICKEND)
        DO 656 RP1=1,NUMTEACH
         NEWDEPT(RP1)=LGROUP(NEWDEPT(RP1))
00656    CONTINUE

        CALL ASSIGN(NUMTEACH,NEWDEPT,MAXDEPT,
     C  DEPARTN,RCHOICE,MEANWT,VARWT,
     C  1,2) 
       MKCOUNT2=TKC
        NEARVAL=GNEARVAL
        PCTILE=GPCTILE 
C        END SASCENT

       CLOSE(20)
       RETURN
05843  FORMAT("Reassigning actor ",I4," to subgroup ",I4,
     C " because the actor's current group, ",I4,
     C " was dissolved.")
03252 FORMAT(//,"---------------------------------------------",//)

    
 2105 FORMAT(A16,251(I5,1X))  
 2104 FORMAT(A16,251(F5.2,1X))  
 2102 FORMAT(10(A30)) 
89898 FORMAT(A,I5,30(A,F10.5))
 2101 FORMAT(251(A8)) 
 1747 FORMAT(I5,A)
  144 FORMAT(2F10.5)
C
  251 FORMAT(20(F15.5))
 2103 FORMAT(I8,I8,251(F5.2,1X))  
 2107 FORMAT(A16,251(I5,1X))  
  103 FORMAT(F8.1,251(F5.2,1X))  
  105 FORMAT(A15,251(I5,1X))  
  104 FORMAT(A15,251(F5.2,1X))  
  102 FORMAT(10(A30)) 
  101 FORMAT(251(A8)) 
89897   FORMAT(10(A))
  132 FORMAT(" GROUP    ACTORS")
  133 FORMAT(I4,4X,251(2X,I8,2X))
C 2102 FORMAT(251(A8))
  134 FORMAT("(",I4,")",4X,251(:,"(",:,E10.4,")"))
  137 FORMAT(A6,4X,251(:,"(",:,E10.4,")"))
 2137 FORMAT(A6,4X,251(:,"(",:,I10,")"))
  136 FORMAT("HAVE MOVED ",I4," FROM ",I4," TO ",I4," ITERATION #",I5)
 1365  FORMAT ("HAVE REASSIGNED ",I4," FROM GROUP ",I4,
     C " BECAUSE OF NEGATIVE ",
     C "CONBTRIBUTION TO THE OBJECTIVE FUNCTION")
03341  FORMAT("DETACHING ",I4, " BECAUSE IT IS NOT CONNECTED TO ",
     C "MEMBERS OF ITS GROUP, GROUP ",I4)
 1364 FORMAT("HAVE DISBANDED MEMBERS OF ",I4," DUE TO ZERO ROW",
     C " MARGINALS")
  139 FORMAT("WANT TO MOVE ",I4," FROM ",I4," TO ",I4)
02339 FORMAT("ACTOR ",I5," HAS ASSOCIATION OF ",E10.2,
     C " WITH GROUP",I5,
     C/,"  WHICH IS NOT GREATER THAN THE CURRENT NEARVAL OF ",E10.2)
01339 FORMAT("STARTING NEW GROUP # ",I5," AROUND ",I5," WITH ",
     CI5," AND ",I5)
01349  FORMAT("THERE ARE MORE THAN THREE ISOLATED ACTORS SO A",/,
     C "GROUP WILL BE FORMED RATHER THAN CONVERGING AT THIS POINT",/
     C "WITH MOVE OF ACTOR ",I5," TO GROUP ",I5," AND CHANGE VALUE",
     C /,"OF ",F10.5, " .")

      END

C       CALL DIFFCOMP(NEWMAT,I,J,NUMTEACH,NEWCON(I,J),
C     C DEPARTN(J),ACTRSQR,SSRI(J),SRI(J),OLDCON(J),
C     C ALLGROUP,RCHOICE(I),DIFF(I,J),NEWONE,GRMEAN,GRVAR,
C     C SRIU2,XSRI)

       SUBROUTINE DIFFCOMP(I,J,NUMTEACH,NEWCON,
     C DEPARTN,ACTRSQR,SSRI,SRI,OLDCON,
     C RCHOICE,CONNECT,NEWONE,GMEAN,GVAR,SRIU2,XSRI,THISMW,
     C THISVW,ROWWT,COLWT,QUANTYPE,SQUAREIT,IEXPECT,ICON,ISTD,
     C ADDVAR,ADDE,DELTAK,DELTAM,DELTAV,PERGROUP,TBLAUC,DISBAND,
     C HYPERG,MAXWT)
       
       INTEGER I,J,NUMTEACH,OLDCON,DEPARTN,HYPERG,
     C  ACTRSQR,PI,RCHOICE,NEWONE,TCON,TI,MAXWT,
     C QUANTYPE,SQUAREIT,PERGROUP,DISBAND,BADOLD,BADNEW
   
       REAL SSRI, SRI ,TOP,BOTTOM,CONNECT,NG,N,GAM,
     C ADDVAR,ADDE,XSRI,SRIU2,GMEAN,GVAR,IE,IVAR,THISMW,THISVW,
     C ADDSUB,TNG,XNEWCON,NEWCON,NEWCOMP,OLDCOMP,TOLDCON,
     C TXSRI,TSRIU2,TSRI,TSSRI,ROWWT,COLWT,PNEWCON,NEWE,TBLAUC,
     C NDIFF,ODIFF,NSIGN,OSIGN,NRAT,ORAT,IEXPECT,ICON,ISTD,TUFFY,
     C TUFFY2,INDVSUB,INDMSUB,DELTAK,DELTAM,DELTAV,NEWV,TIVAR,TIE,
     C BDENOM,TX,LNEWE(4),LPNEWCON(4),LGMEAN(4),LTOLDCON(4)

       REAL ZCOMPMAT(251,251)
       INTEGER ALLGROUP(251,251),HUBERT(251,251),RESULTM(251,251),
     C NEWMAT(251,251)
       REAL BLAUC(251,251),DIFF(251,251)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF

      NG=DEPARTN-1
      N=NUMTEACH-1



        IE=0.000000
        IVAR=0.000000
       IF (NEWONE .EQ. 1) THEN
        ADDSUB=1
        TNG=NG+1
       IF ((QUANTYPE .NE. 5) .AND. (RCHOICE .GT. 0))  THEN
       IE=THISMW*RCHOICE*TNG/N
       IVAR=(RCHOICE*TNG*(N-TNG)*THISMW**2/N**2) *
     C  (1. + N*THISVW/((N-TNG)*THISMW**2) - 
     C  HYPERG*((RCHOICE-1.)/(N-1.)))
       END IF       
       ELSE
        ADDSUB=-1
        TNG=NG
       END IF

        NEWCON=0
        XNEWCON=0
          TOLDCON=OLDCON

       DO 02 PI=1,DEPARTN
         NEWCON=NEWCON+NEWMAT(ALLGROUP(J,PI),I)
         XNEWCON=XNEWCON+NEWMAT(I,ALLGROUP(J,PI))
00002     CONTINUE


          TNCON= (COLWT*NEWCON+ROWWT*XNEWCON)
          DISBAND=0
          IF ((ADDSUB .LT. 1) .AND. (TNCON .LE. 0)) THEN
           DISBAND=1
          END IF

          NEWE=   COLWT*ADDE+ROWWT*ADDSUB*IE
          NCHOICE=COLWT*SRI+ROWWT*ADDSUB*RCHOICE*THISWM

          PNEWCON= (TOLDCON+ADDSUB*(COLWT*NEWCON+ROWWT*XNEWCON)) 

          IF (QUANTYPE .EQ. 3) THEN
          LNEWE(3)=NEWE
          LNEWE(4)=NCHOICE-NEWE
          LNEWE(2)=TNG*(TNG-1.0)*MAXWT - NEWE
          LNEWE(1)=TNG*(N-TNG)*MAXWT - LNEWE(2)


          LPNEWCON(3)=PNEWCON 
          LPNEWCON(4)=NCHOICE-PNEWCON
          LPNEWCON(2)=TNG*(TNG-1.0)*MAXWT - PNEWCON
          LPNEWCON(1)=TNG*(N-TNG)*MAXWT - LPNEWCON(2)

          LGMEAN(3)=GMEAN
          LGMEAN(4)=SRI-GMEAN
          LGMEAN(2)=DEPARTN*(DEPARTN-1.0)*MAXWT-GMEAN
          LGMEAN(1)=DEPARTN*(N-DEPARTN)*MAXWT - LGMEAN(2)

          LTOLDCON(3)=TOLDCON
          LTOLDCON(4)=SRI-TOLDCON
          LTOLDCON(2)=DEPARTN*(DEPARTN-1.0)*MAXWT-TOLDCON
          LTOLDCON(1)=DEPARTN*(N-DEPARTN)*MAXWT - LTOLDCON(2)
          END IF

          IF (RCHOICE .LT. TNG) THEN
           BDENOM=RCHOICE
          ELSE
           BDENOM=TNG
          END IF

          TBLAUC=(COLWT*NEWCON+ROWWT*XNEWCON)/(BDENOM+TNG)

           DELTAK=PNEWCON-TOLDCON
           ICON=-ADDSUB*(COLWT*NEWCON+ROWWT*XNEWCON)
           IF (QUANTYPE .NE. 5) THEN
           ODIFF=TOLDCON-GMEAN
           NDIFF=PNEWCON-NEWE
           IEXPECT=-ADDSUB*(COLWT*ADDE+ROWWT*IE)
           ISTD=-ADDSUB*(COLWT*ADDVAR +ROWWT*IVAR)**.5
           DELTAM=NEWE-GMEAN
           NEWV=COLWT*ADDVAR + ROWWT*ADDSUB*IVAR
           DELTAV= NEWV-GVAR
           END IF          
           IF (ODIFF .GT. 0.00000) THEN 
             OSIGN=1
           ELSE
             OSIGN=-1
           END IF

           IF (NDIFF .GT. 0.00000) THEN 
             NSIGN=1
           ELSE
             NSIGN=-1
           END IF


           ORAT=TOLDCON/GMEAN
           NRAT=TOLDCON/NEWE



           IF (QUANTYPE .EQ. 1) THEN
          OLDCOMP= (ODIFF)/GVAR**.5
          NEWCOMP=    (NDIFF) / NEWV**.5
C     C    (ADDVAR+ ADDSUB*IVAR)**.5
          END IF
            IF (QUANTYPE .EQ. 2) THEN
           OLDCOMP=(ODIFF)/GMEAN**.5
           NEWCOMP=(NDIFF)/NEWE**.5
           END IF
           IF (QUANTYPE .EQ. 3) THEN
           OLDCOMP=0
           BADOLD=0
           NEWCOMP=0
           BADNEW=0
           DO 8282 TI=1,4
           IF ((TI .EQ. 1) .OR. (TI .EQ. 3)) THEN
           LADD=1
           ELSE
           LADD=-1
           END IF
           IF (LGMEAN(TI) .LT. .0001) THEN
           LGMEAN(TI)=.0001
           END IF
           IF (LTOLDCON(TI) .LT. .0001) THEN
           LTOLDCON(TI)=.0001
           END IF

            OLDCOMP=OLDCOMP+LADD*2*LTOLDCON(TI)*
     C     LOG(LTOLDCON(TI)/LGMEAN(TI))

           IF (LNEWE(TI) .LT. .0001) THEN
           LNEWE(TI)=.0001
           END IF
           IF (LPNEWCON(TI) .LT. .0001) THEN
           LPNEWCON(TI)=.0001
           END IF

            NEWCOMP=NEWCOMP+2*LADD*LPNEWCON(TI)*
     C     LOG(LPNEWCON(TI)/LNEWE(TI))
08282       CONTINUE

C          IF ((PNEWCON .GT. 0.000000) .AND. (NEWE .GT. 0.00000)) THEN
C            NEWCOMP=2*PNEWCON*LOG(PNEWCON/NEWE)
C           ELSE
C            NEWCOMP=-999999
C          END IF

           END IF
          IF (QUANTYPE .EQ. 4)  THEN
           NEWCOMP=0
           OLDCOMP=0
           IF (NG .GT. 0) THEN
           OLDCOMP=TOLDCON/(DEPARTN*(DEPARTN-1.00))
           END  IF

           TX=DEPARTN+ADDSUB
           IF (TX .GT. 1) THEN
           NEWCOMP=PNEWCON/(TX*(TX-1.00))
           END IF
           END IF
C           IF (NEWONE .EQ. 1) THEN
C           NEWCOMP=(TOLDCON+TNCON)/(NG*(NG+1.000))
C          ELSE
C          NEWCOMP=(TOLDCON-TNCON)/(NG*(NG-1.000))
C           END IF
C           IF (NG .GT. 1) THEN
C           OLDCOMP=TOLDCON/(NG*(NG-1.000))
C          END IF
C           ELSE
C           IF (NG .GT. 1) THEN
C          OLDCOMP=TOLDCON/(NG*(NG-1.000))
C           END IF
C           IF (NG .GT. 2) THEN
C           NEWCOMP=(TOLDCON-NEWCON-XNEWCON)/((NG-1.00)*(NG-2.00))
C           END IF
C           END IF
C          END IF
C          END IF
      
           IF ((SQUAREIT .EQ. 1) .AND. (QUANTYPE .LT. 3)) THEN
           NEWCOMP=NSIGN*NEWCOMP**2
           OLDCOMP=OSIGN*OLDCOMP**2
           END IF
           IF (PERGROUP .EQ. 1) THEN
            NEWCOMP=NEWCOMP/(DEPARTN-1.+ADDSUB)
            OLDCOMP=OLDCOMP/(DEPARTN-1.)
           END IF
           IF ((DEPARTN .LT. 2) .AND. (NEWONE .EQ. 1)) THEN
           OLDCOMP=0
           END IF
           IF ((NEWONE .EQ. 0) .AND. (DEPARTN .EQ. 2)) THEN
           NEWCOMP=0
           END IF

           CONNECT=ADDSUB*(NEWCOMP-OLDCOMP)

           IF ((QUANTYPE .NE. 3) .AND. (TNCON .LE. 0)) THEN
           CONNECT=-999
           END IF

       RETURN
       END

       SUBROUTINE PROBS (MAXGROUP,NUMTEACH,DEPARTN,PAGB,
     C NEWDEPT,RCHOICE,WHOLEGRP,INPER,INGROUP2,MBI)

       INTEGER I,J,PERS,GROUP,IER,MBI,IER2,MAXDEPT

       INTEGER  MAXGROUP,NUMTEACH,DEPARTN(251),
     C  RCHOICE(251),NEWDEPT(251),THISN,
     C  WHOLEGRP,BEGPER,ENDPER,BEGGRP,ENDGRP,INGROUP2,INPER,
     C NEWNUM,TARGET,OUTCON(251),G
       REAL ZCOMPMAT(251,251)
       INTEGER HUBERT(251,251),RESULTM(251,251),ALLGROUP(251,251),
     C NEWMAT(251,251)

C       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT
       REAL BLAUC(251,251),DIFF(251,251)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF


       REAL*4 PAGB(251,200),TOTNUM(251)

       REAL PK,PS,KTHETA,NUMCHOIC,INGROUP,OSLOTS,RTHISN,
     C GCONN(251),TCONN,TSLOTS,TCONN2,GCONN2(251),
     C KTHETA2,TTHETA,PK2,PS2,INCM,
     C TOTP(251),KTHETA3,THETA1(251),THETA2(251),
     C G1,G2,ACHOICE(251,251),TCHOICE(251)

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
        G1=(THETA1(G)**ACHOICE(I,G)) *  
     C  (1-THETA1(G))**(TCHOICE(I)-ACHOICE(I,G))

        G2=(THETA2(G)**ACHOICE(I,G)) *  
     C  (1-THETA2(G))**(TCHOICE(I)-ACHOICE(I,G))

        PAGB(I,G)=DEPARTN(G)*G1/(G1+G2)
        IF ((PAGB(I,G) .GE. 0 ) .AND. (PAGB(I,G) .LT. 999999)) THEN
        HAPPY=1
        ELSE
        PAGB(I,G)=0.000
        END IF

        TOTP(I)=TOTP(I)+PAGB(I,G)
        END IF
00010    CONTINUE
00009     CONTINUE

         DO 13 I=1,NUMTEACH
         DO 12 J=1,MAXDEPT
          
          PAGB(I,J)=PAGB(I,J)/TOTP(I)
00012      CONTINUE
00013       CONTINUE
  251 FORMAT(20(F10.5))
         RETURN
         END

       SUBROUTINE GETPARS(NOATTACH,NEVAL,DYDTRIAD,USETRIAD,
     C NUMDYAD,NUMTEACH,
     C NEARVAL,STOPVAL,WPIK,
     C SYMMAT,THRESHT,LOOKT,DIRECT,KCOUNT2,BASEVAL,BOUNDVAL,
     C PCTILE,STRUCTEQ,NETWORK,ACTRSQR,QUICKEND,ROWWT,COLWT,
     C QUANTYPE,SQUAREIT,REWEIGHT,PRINTO,APRINT,NPO,INFILE,
     C MATTYPE,PRIORFIL,HITFILE,LABFILE,TITFILE,NUMHLIST,TITLES,
     C FANCY,NUMRES,RASEED,NETLEV,PERGROUP,TOPVAL,TRANSPOS,
     C MAXDEPT,GCONTIN,NPARGRP,PARTITLE,PARFORM,LISTVAR,TCHAR,
     C NEWGRPS,FIXR,BLABOUND,INVERT,RECTMAT,BETWMULT,HIWTEVAL,
     C MUTDYAD,NONEG,MAXSEED,ATTACHI,HALFDYAD,DISSOLVE,GUSEMARG,
     C TAGALONG,NUMOBS,
     C  IGRATIO,NUMDIM,
     C CENTER,DANCHOR,DANCHOR2,MOVE2,ZSYMMAT,
     C STARTINC,BYINC,MAXINC,KEXP,NORMAL,MINVALG,
     C CENTERI,DANCHORI,DANCH2I,MOVE2I,ZSYMMATI,
     C STARTINI,BYINCI,MAXINCI,KEXPI,NORMALI,MINVALI,
     C RINCREM,MEASURE,EXTREME,DRADIUSG,DRADIUSI,
     C BYANGLE,BYSCALE,BYANGLEI,BYSCALEI,MINPICT,HYPERG,
     C PCTCENIG,PCTCENG2,PCTCENI1,PCTCENI2)

C    C CENTER,DANCHOR2,MOVE2,DANCHOR,ZSYMMAT,MAXINC,
C    C BYINC,NUMDIM,RINCREM,MEASURE,EXTREME,STARTINC,IGRATIO,
C    C HYPERG,DRADIUS,KEXP,NORMAL,NORMALI,MINVALG,MINVALI)

       
       INTEGER SQUAREIT,QUANTYPE,QUICKEND,ACTRSQR,NOATTACH,NEVAL,
     C DYDTRIAD,USETRIAD,NUMDYAD,SYMMAT,LOOKT,KCOUNT2,NUMRES,
     C STRUCTEQ,NETWORK,PRINTO(100),NPO,P,REWEIGHT,NUMHLIST,
     C NETLEV,PERGROUP,TRANSPOS,NUMTEACH,MAXDEPT,GCONTIN,NPARGRP,
     C NEWGRPS,INVERT,RECTMAT,MUTDYAD,NONEG,MAXSEED,ATTACHI,HALFDYAD,
     C DISSOLVE,GUSEMARG,TAGALONG,NUMOBS,NORMAL,NORMALI,
     C CENTER,DANCHOR2,MOVE2,DANCHOR,ZSYMMAT,MAXINC,
     C BYINC,NUMDIM,RINCREM,MEASURE,EXTREME,STARTINC,HYPERG,ONE,
     C CENTERI,DANCHORI,DANCH2I,MOVE2I,ZSYMMATI,STARTINI,BYINCI,
     C MAXINCI,BYANGLE,BYSCALE,BYANGLEI,BYSCALEI

       REAL IGRATIO,DRADIUSG,MINVALG,MINVALI,KEXPI,MINPICT,
     C PCTCENIG,PCTCENG2,PCTCENI1,PCTCENI2,DRADIUSI

     
       REAL NEARVAL,STOPVAL,WPIK,THRESHT,DIRECT,BASEVAL,HIWTEVAL,
     C BOUNDVAL,PCTILE,ROWWT,COLWT,TOPVAL,FIXR,BLABOUND,BETWMULT,
     C KEXP
       CHARACTER*8 INDICAT
       CHARACTER APRINT(50)*200,MATTYPE*1,INFILE*16,PRIORFIL*16,
     C PARMFILE*16,GRPTYPE*1,HITFILE*16,LABFILE*16,TITFILE*16,
     C TITLES(3)*20,FANCY(20)*200,PARTITLE(50)*200,PARFORM(50)*80,
     C LISTVAR(50)*100,TCHAR(50)*80
       DOUBLE PRECISION RASEED

        DO 88 P=1,4
        WRITE(6,100) FANCY(P)
00088    CONTINUE

       
       IF (NUMTEACH .LE. 1) THEN
       WRITE(6,100) 'You have submitted a file with no data'
       END IF
       IF (NUMOBS .LT. 2) THEN
       WRITE(6,100) "You have submitted a file with less than 2"
       WRITE(6,100) "non-zero data points."
       END IF
       WRITE(6,100) 'Would you like to enter the parameters'
       WRITE(6,100) 'Interactively (Y or N -- USE UPPER CASE!)'
       READ (5,*) INDICAT
       IF (INDICAT(1:1) .EQ. 'Y') THEN
       WRITE(6,100) 'Ok, here we go'
       WRITE(6,100) 'You will be asked a series of questions'
       WRITE(6,100) 'regarding values to be assigned to various'
       WRITE(6,100) 'parameters.  The information behind each'
       WRITE(6,100) 'parameter will be in lower case. '
       WRITE(6,100) 'PROMPTS FOR RRESPONSES WILL APPEAR IN'
       WRITE(6,100) 'UPPER CASE'
       WRITE(6,100) 
       WRITE(6,100) 'At the begiining of each section, defaults'
       WRITE(6,100) 'will be listed.  Values for the defaults'
       WRITE(6,100) 'are taken from the file "kliqfind.par" .'
       WRITE(6,100)  'You may bypass a section'

       WRITE(6,100) 'by accepting the defaults'


       WRITE(6,104)
       WRITE(6,100) 'Structural Equivalence versus Connectivity'
       WRITE(6,100) 'The defaults are:'
       WRITE(6,200) 'STRUCTEQ',STRUCTEQ
       WRITE(6,200) 'NETWORK',NETWORK
       WRITE(6,200) 'ACTRSQR',ACTRSQR
       WRITE(6,100) 'Accept the defaults?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN
       WRITE(6,100) 'Would you like define groups based on '
       WRITE(6,100) 'Structural equivalence or connectivity? '
       WRITE(6,100) 'ENTER S OR C'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'S') THEN
          STRUCTEQ =1
       WRITE(6,100) 'Are these network data?  That is, should'
       WRITE(6,100) 'The diagonals of the matrix be positive?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        NETWORK=1
        ELSE
        NETWORK=0
        END IF
       WRITE(6,100) 'Do you want to maximize based on Actual'
       WRITE(6,100) 'Change in R-square or based on Euclidean'
       Write(5,100) 'Distance'
       WRITE(6,100) 'ENTER R OR E'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'R') THEN
       ACTRSQR=1
        ELSE
       ACTRSQR=0
         END IF
       END IF
C      STRUCTURAL EQUIVALENCE
        END IF
C        DEFAULTS

         WRITE(6,104)
       Write(5,100) 'Objective Function to be maximized:'
       WRITE(6,100) 'The defaults are:'
       WRITE(6,200) 'QUANTYPE',QUANTYPE
       WRITE(6,200) 'SQUAREIT',SQUAREIT
       WRITE(6,200) 'REWEIGHT',REWEIGHT
       WRITE(6,200) 'NETLEV',NETLEV
       WRITE(6,200) 'PERGROUP',PERGROUP

       WRITE(6,100) 'Accept the defaults?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN

       Write(5,100) "1)Hubert's Compactness"
       Write(5,100) '2)Pearson Goodness of Fit'
       Write(5,100) '3)Likelihood Ratio Criterion'
       WRITE(6,100) 'ENTER 1,2, OR 3'
       READ(5,102) QUANTYPE

       
       WRITE(6,100) "Should the function be evaluated at the network"
       WRITE(6,100) 'ENTER Y OR N'

       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        NETLEV=1
        ELSE
        NETLEV=0
        END IF
       IF (NETLEV .EQ. 0) THEN
       WRITE(6,100) "Should the function value in each group be "
       WRITE(6,100) "averaged over the number of group members?"
       WRITE(6,100) 'ENTER Y OR N'

       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        PERGROUP=1
        ELSE
        PERGROUP=0
        END IF
       END IF

       WRITE(6,100) "Should the function be squared?"       
       WRITE(6,100) 'ENTER Y OR N'

       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        SQUAREIT=1
        ELSE
        SQUAREIT=0
        END IF

       WRITE(6,100) "Do you want to trim the highest valued weights?"
       WRITE(6,100) 'ENTER Y OR N'

       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        WRITE(6,100) "What should the highest valued weight be?"
       WRITE(6,100) 'ENTER AN INTEGER'
       READ(5,102) REWEIGHT
        ELSE 
        REWEIGHT=0
        END IF
        END IF
C        DEFAULTS

       WRITE(6,104)
       WRITE(6,100) "Initiating Groups"       
       WRITE(6,100) 'The defaults are:'

       WRITE(6,200) 'NUMDYAD',NUMDYAD
       WRITE(6,200) 'DYDTRIAD',DYDTRIAD
       WRITE(6,200) 'STARTGRP',STARTGRP
       WRITE(6,200) 'NOATTACH',NOATTACH
       WRITE(6,200) 'ROWWT',ROWWT
       WRITE(6,200) 'COLWT',COLWT
       WRITE(6,200) 'HYPERG',HYPERG
       WRITE(6,200) 'SYMMAT',SYMMAT
       WRITE(6,200) 'TRANSPOS',TRANSPOS
       WRITE(6,200) 'INVERT',INVERT
       WRITE(6,200) 'RECTMAT',RECTMAT
       WRITE(6,200) 'GUSEMARG',GUSEMARG
       WRITE(6,200) 'TAGALONG',TAGALONG

       WRITE(6,100) 'Accept the defaults?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN

       WRITE(6,100) "Would you like the seed groups to be based on"       
       WRITE(6,100) "1)Best seeds based on matrix multiplication"       
       WRITE(6,100) "2)Random Assignments"       
       WRITE(6,100) "3)A priori assignments"       
       WRITE(6,100) 'ENTER 1,2, OR 3'
       READ(5,102) USETRIAD
       IF (USETRIAD .EQ. 2) THEN
        WRITE(6,100) "What seed value should be used?"
       WRITE(6,100) "ENTER A REAL VALUE"
       READ(5,*) RASEED
       END IF

       IF (USETRIAD .EQ. 1) THEN
       WRITE(6,100) "Use Triads or Dyads?"       
       WRITE(6,100) "ENTER T OR D"       
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'T') THEN
       DYDTRIAD=1
        ELSE
       DYDTRIAD=0
         END IF
       WRITE(6,100) "Please enter the number of seed groups"
       WRITE(6,100) "with which the algorithm should be started"
       WRITE(6,100) "ENTER AN INTEGER"
       READ(5,102) NUMDYAD
       END IF
C       USETRIAD=1

        WRITE(6,100) "Should actors be attached to groups before"       
       WRITE(6,100) "proceeding with the formal ascent?"       
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       NOATTACH=0
        ELSE
       NOATTACH=1
        END IF

       IF (STRUCTEQ .NE. 1) THEN
       WRITE(6,104) 

       WRITE(6,100) "Should the data be considered symmetric?"
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       SYMMAT=1
       ROWWT=1
       COLWT=1
        ELSE
       SYMMAT=0
C       WRITE(6,100) "  *****WARNING*****"
C       WRITE(6,100) "Before assigning unequal weights to" 
C       WRITE(6,100) "rows and columns you must consider"
C       WRITE(6,100) "that the optimal move is based on "
C       WRITE(6,100) "maximization of an objective function."
C       WRITE(6,100) "It is sensible that this maximization"
C       WRITE(6,100) "should reflect changes across the entire network"
C       WRITE(6,100) " as the result of a single move.  Thus,"
C       WRITE(6,100) " discounting either the row or column"
C       WRITE(6,100) " weight may not be consistent with the"
C       WRITE(6,100) " idea of maximization of a function applied"
C       WRITE(6,100) "to all the data.  "
C       WRITE(6,100) "The result is that you should carefully"
C       WRITE(6,100) " moniter and limit the number of iterations"
C       WRITE(6,100) "if you choose to have unequal weights"
C       WRITE(6,100) "for rows and columns.  Convergence is NOT"
C       WRITE(6,100) "cetrain!"



       WRITE(6,100) "How much weight should be applied to "       
       WRITE(6,100) "the row data"       
       WRITE(6,100) "ENTER A REAL VALUE"       
       READ(5,*) ROWWT

       WRITE(6,100) "How much weight should be applied to "       
       WRITE(6,100) "the column data"       
       WRITE(6,100) "ENTER A REAL VALUE"       
       READ(5,*) COLWT
       ONE=1
       WRITE(6,200) "Which probability model should be used?"
       WRITE(6,200) "ENTER 1 FOR BINOMIAL, 2 FOR HYPERGEOMETRIC"
       READ(5,*) HYPERG
       HYPERG=HYPERG-ONE

       WRITE(6,100) "Should the transpose of the raw data be used?"
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       TRANSPOS=1
        ELSE
       TRANSPOS=0
        END IF
C         FOR TRANSPOSE

       WRITE(6,100) "Do the rows and columns of your matrix"
       WRITE(6,100) "represent identical attributes (ACTORS)?"
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        RECTMAT=0
        ELSE
       WRITE(6,100) "Do you want to work with X'X OR XX' ?"
       WRITE(6,100) "ENTER 1 FOR X'X OR"
       WRITE(6,100)"       2 FOR XX'"
       READ (5,*) RECTMAT
        END IF
       WRITE(6,100) "Do you want to standardize weights by the "
       WRITE(6,100) "row marginals ?"
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        GUSEMARG=1
        ELSE
        GUSEMARG=0
        END IF


       WRITE(6,100) "Should the weights be inverted?"
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       INVERT=1
        ELSE
       INVERT=0
        END IF
C         FOR TRANSPOSE
        END IF
C       INDICATE ='Y' FOR SYMMAT
      END IF
C      STRUCTEQ .NE. 1

       WRITE(6,100) "Should actors who are connected to only one other"
       write(6,100) "actor in the network (tagalongs) be removed and"
       write(6,100) " assigned to the group of the actor with whom "
       write(6,100)  "they are connected?"
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       TAGALONG=1
        ELSE
       TAGALONG=0
        END IF
       END IF
C        DEFAULTS

       WRITE(6,104)

       WRITE(6,100) "Finding best Groups"       
       WRITE(6,100) 'The defaults are:'
       WRITE(6,200) 'DIRECT',DIRECT
       WRITE(6,200) 'THRESHT',THRESHT
       WRITE(6,200) 'LOOKT',LOOKT
       WRITE(6,200) 'MAXSEED',MAXSEED
       WRITE(6,100) 'Accept the defaults?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN


       WRITE(6,100) "When establishing seed groups OR when"       
       WRITE(6,100) "finding a new group during the algorithm,"        
       WRITE(6,100) "matrix multiplication is used to find the"       
       WRITE(6,100) "best groups"       
       
       WRITE(6,100) "How much weight should be assigned to"       
       WRITE(6,100) "direct versus indirect connections"       
       READ (5,*) DIRECT

       WRITE(6,100) "What is the threshold of similarity for looking"       
       WRITE(6,100) "at a triad (triads with below this threshold will"       
       WRITE(6,100) "not be considered for seed dyads (or triads)"
       WRITE(6,100) "ENTER A REAL VALUE (2.000 SEEMS TO WORK)"       
       READ (5,*) THRESHT
       
       WRITE(6,100) "What is the maximum number of seeds?"

       WRITE(6,100) "ENTER A REAL VALUE (BETWEEN 0 AND 9999)"       
       READ(5,*) LOOKT
       WRITE(6,100) "How many times can a given actor be used as"
       WRITE(6,100) " a seed for a subgroup?"
       WRITE(6,100) "ENTER AN INTEGER "
       READ (5,*) MAXSEED

       END IF
C       DEFAULTS

       WRITE(6,104) 
        WRITE(6,100) "Determining Proximity for Group Assignment"       
       WRITE(6,100) 'The defaults are:'
       WRITE(6,200) 'NEARVAL',NEARVAL
       WRITE(6,200) 'PCTILE',PCTILE
       WRITE(6,200) 'MUTDYAD',MUTDYAD
       WRITE(6,200) 'NONEG',NONEG
       WRITE(6,200) 'MUTDYAD',MUTDYAD
       WRITE(6,200) 'HALFDYAD',HALFDYAD
       WRITE(6,200) 'DISSOLVE',DISSOLVE
       WRITE(6,100) 'Accept the defaults?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN
       WRITE(6,100) "When assigning an actor to a group, how"       
       WRITE(6,100) "large must the baseline standardized measure of "       
       WRITE(6,100) "association be for the group assignment to"       
       WRITE(6,100) "occur?"       
       WRITE(6,100) "ENTER A REAL VALUE"
       READ(5,*) NEARVAL
       WRITE(6,100) "Should this value be determined as a "       
       WRITE(6,100) "percentile of the proximites of actors"       
       WRITE(6,100) "currently assigned to groups?"       
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       WRITE(6,100) "What should that percentile be?"       
       WRITE(6,100) "ENTER A REAL VALUE (BETWEEN 0 AND 1)"       
       READ(5,*) PCTILE
       ELSE
       PCTILE=1.5
       END IF
       WRITE(6,104)
        WRITE(6,100) "Should dyads be built if only one actor"
        WRITE(6,100) "initiates connections across the network?"
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       MUTDYAD=0
        ELSE
       MUTDYAD=1
        END IF
        WRITE(6,100) "Should assignments which result in a negative"
        WRITE(6,100) " contribution to the objective function be "
        WRITE(6,100) " invalidated?"
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       NONEG=1
        ELSE
       NONEG=0
        END IF
        WRITE(6,100) "Should difference between dyad and isolate"
        WRITE(6,100) "be halved when considering an actor's "
        WRITE(6,100) " removeal?"
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       HALFDYAD=1
        ELSE
       HALFDYAD=0
        END IF
        WRITE(6,100) "Groups below what size should be dissolved?"
       WRITE(6,100) "ENTER AN INTEGER"
       READ (5,*) DISSOLVE

       WRITE(6,104)
       END IF
C       DEFAULTS

       
       WRITE(6,100) "Convergence"              
       WRITE(6,100) 'The defaults are:'
       WRITE(6,200) 'QUICKEND',QUICKEND
       WRITE(6,200) 'STOPVAL',STOPVAL
       WRITE(6,200) 'KCOUNT2',KCOUNT2
       WRITE(6,200) 'ATTACHI',ATTACHI
       WRITE(6,100) 'Accept the defaults?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN
       WRITE(6,100) "Would you like quick convergence,"       
       WRITE(6,100) "Just to get output from a priori groups?"              
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        PRINTO(21)=0
        QUICKEND=1
        ELSE
        QUICKEND=0
        WRITE(6,100) "What should the convergence criteria be?"       
       WRITE(6,100) "That is, after a change results in less than"
       WRITE(6,100) "this value, the ascent stops"       
       WRITE(6,100) "ENTER A REAL VALUE"
       READ(5,*) STOPVAL

       WRITE(6,100) "After How many iterations should the ascent stop" 
       WRITE(6,100) "If it has not yet converged?"       
       WRITE(6,100) "ENTER AN INTEGER"              
       READ(5,*) KCOUNT2
       WRITE(6,100) "Should isolates be attached after ascent,"
       WRITE(6,100) "and then a final ascent executed?"
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        ATTACHI=1
        ELSE
        ATTACHI=0
        END IF
       END IF
C       QUICKEND
       END IF
C       DEFUALTS

        WRITE(6,104) 
        NUMRES=0
       WRITE(6,100) "Monte Carlo (Plus) Evaluation"              
       WRITE(6,100) 'The defaults are:'
       WRITE(6,200) 'NEVAL',NEVAL
       WRITE(6,200) 'BASEVAL',BASEVAL
       WRITE(6,200) 'TOPVAL',TOPVAL
       WRITE(6,200) 'NEWGRPS',NEWGRPS
       WRITE(6,200) 'HIWTEVAL',HIWTEVAL
       WRITE(6,100) 'Accept the defaults?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN
       WRITE(6,100) "How many observations should be used?"       
       WRITE(6,100) "ENTER AN INTEGER"              
       READ(5,*) NEVAL
       WRITE(6,100) "Starting how low?"
       WRITE(6,100) "ENTER A REAL VALUE (BETWEEN 0 AND 1)"
       READ(5,*) BASEVAL

       WRITE(6,100) "Ending how high?"
       WRITE(6,100) "ENTER A REAL VALUE (BETWEEN 0 AND 1)"
       READ(5,*) TOPVAL
       WRITE(6,100) "Should it be possible to create new groups"
       WRITE(6,100) "when rival solutions are being generated?"
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        NEWGRPS=0
        ELSE
        NEWGRPS=1
        END IF
       WRITE(6,100) "How Much More Weight on Largest # of "
       WRITE(6,100) "Reassignments compared with smallest ?"
       WRITE(6,100) "ENTER A REAL VALUE "
       READ(5,*) HIWTEVAL


       END IF
C       DEFAULTS
       IF (NEVAL .GT. 0) THEN
       WRITE(6,100) "Would you like to use the simulation for"
       WRITE(6,100) "obtaining a second solution?"
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        NUMRES=1
        END IF
       END IF

  
       WRITE(6,104) 

       WRITE(6,100) "Boundary Spanners"
       WRITE(6,100) 'The defaults are:'
       WRITE(6,200) 'BOUNDVAL',BOUNDVAL
       WRITE(6,200) 'BLABOUND',BLABOUND
       WRITE(6,200) 'FIXR',FIXR
        WRITE(6,200) 'BETWMULT',BETWMULT
       WRITE(6,100) 'Accept the defaults?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN
       WRITE(6,100) "What is the cut-off value for boundary spanners?"
       WRITE(6,100) "ENTER A REAL VALUE "
       READ(5,*) BOUNDVAL
       WRITE(6,100) "What is the cut-off value for Blau boundary"
       WRITE(6,100) " spanners?"
       WRITE(6,100) "ENTER A REAL VALUE "
       READ(5,*) BLABOUND

       WRITE(6,100) "What is the multiplier for betweenness?"
       WRITE(6,100) "ENTER A REAL VALUE "
       READ(5,*) BETWMULT
       WRITE(6,100) "Was there a fixed number of connections "
       WRITE(6,100) "each actor could initiate?"
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN
        FIXR=99999
        ELSE
       WRITE(6,100) "What was the maximum number of connections"
       WRITE(6,100) "an actor could initiate?"
       WRITE(6,100) "ENTER A REAL VALUE "
       READ(5,*) FIXR
         END IF
       END IF
C       ACCEP THE DEFAULTS ON BOUNDARY SPANNERS

       WRITE(6,100) "Anchored MDS plot"
       WRITE(6,100) "Between Groups"
       WRITE(6,100) 'The defaults are:'

       WRITE(6,200) 'IGRATIO',IGRATIO
       WRITE(6,200) 'NUMDIM',NUMDIM
       WRITE(6,200) 'MINPICT',MINPICT
       WRITE(6,200) 'CENTER',CENTER
       WRITE(6,200) 'DANCHOR',DANCHOR
       WRITE(6,200) 'DANCHOR2',DANCHOR2
       WRITE(6,200) 'MOVE2',MOVE2
       WRITE(6,200) 'ZSYMMAT',ZSYMMAT
       WRITE(6,200) 'STARTINC',STARTINC
       WRITE(6,200) 'BYINC',BYINC
       WRITE(6,200) 'KEXP',KEXP
       WRITE(6,200) 'MAXINC',MAXINC
       WRITE(6,200) 'NORMAL',NORMAL
       WRITE(6,200) 'MINVALG',MINVALG
       WRITE(6,200) 'BYANGLE',BYANGLE
       WRITE(6,200) 'BYSCALE',BYSCALE
       WRITE(6,200) 'DRADIUSG',DRADIUSG
       WRITE(6,200) 'PCTCENG1',PCTCENG1
       WRITE(6,200) 'PCTCENG2',PCTCENG2


       WRITE(6,100) 'Accept the defaults?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN

       WRITE(6,100) "What should is the ratio of within group"
       WRITE(6,100) "distances to between group distances?"
       WRITE(6,100) "ENTER A REAL VALUE"
       READ(5,*) IGRATIO
       WRITE(6,100) "How many dimensions should the plot be in?"
       WRITE(6,100) "NOTE THAT THE CURRENT VERSION OF THE PROGRAM"

       WRITE(6,100) "CAN PLOT ONLY IN TWO DIMENSIONS"
       WRITE(6,100) "I AM ASSIGNING NUMDIM=2"
         NUMDIM=2
       WRITE(6,100) "What is the minimum distance which "
       WRITE(6,100) "separates two actors?"
       WRITE(6,100) "ENTER A REAL VALUE AS A PERCENTAGE OF THE"
       WRITE(6,100) "THE MAXIMUM VALUE IN A GRAPH"
       READ(5,*) MAKEPIC

       WRITE(6,100) "Should the points be recentered about the group"
       WRITE(6,100) "coordinates?"
       WRITE(6,100) "ENTER Y or N"
       READ(5,*) INDICATE
        IF (INDICAT(1:1) .EQ. 'N') THEN
        CENTER=0
        ELSE
        CENTER=1
        END IF
       WRITE(6,100) "Which group should be used for the anchoring?"
       WRITE(6,100) "ENTER THE NUMBER OF THE GROUP IN "
       WRITE(6,100) "TERMS OF IT'S ORDER IN CENTRALITY"
       WRITE(6,100) "A VALUE GREATER THAN THE NUMBER OF POINTS"
       WRITE(6,100) "WILL BE CONVERTED TO THE MAXIMUM."
       WRITE(6,100) "A VALUE OF ZERO WILL INDICATE A RANDOM"
       WRITE(6,100) "CHOICE, A VALUE OF LESS THAN 0 WILL"
       WRITE(6,100) "INDICATE THAT PERCENTILES (I.E. MEDIANS,"
       WRITE(6,100) "QUARTILES, ETC) SHOULD BE USED TO "
       WRITE(6,100) "ESTABLISH THE MEASURE OF CENTRALITY"
       READ(5,*) DANCHOR
       IF (DANCHOR .LT. 0) THEN
       WRITE(6,100) "What percentiel should be used?"
       WRITE(6,100) "ENTER A REAL VALUE BETWEEN 0 AND 1"
       READ(5,*) PCTCENG1
       END IF
       WRITE(6,100) "Which group should be used for the second"
       WRITE(6,100) "anchor?"
       WRITE(6,100) "ENTER THE NUMBER OF THE GROUP IN "
       WRITE(6,100) "TERMS OF IT'S ORDER IN CENTRALITY"
       WRITE(6,100) "A VALUE GREATER THAN THE NUMBER OF POINTS"
       WRITE(6,100) "WILL BE CONVERTED TO THE MAXIMUM."
       WRITE(6,100) "A VALUE LESS THAN 1 WILL INDICATE A RANDOM"
       WRITE(6,100) "CHOICE, A VALUE OF LESS THAN 0 WILL"
       WRITE(6,100) "INDICATE THAT PERCENTILES (I.E. MEDIANS,"
       WRITE(6,100) "QUARTILES, ETC) SHOULD BE USED TO "
       WRITE(6,100) "ESTABLISH THE MEASURE OF CENTRALITY"
       READ(5,*) DANCHOR2
       IF (DANCHOR .LT. 0) THEN
       WRITE(6,100) "What percentiel should be used?"
       WRITE(6,100) "ENTER A REAL VALUE BETWEEN 0 AND 1"
       READ(5,*) PCTCENG2
       END IF
       WRITE(6,100) "Should the second anchor be repositioned?"
       WRITE(6,100) "ENTER Y OR N"
       READ(5,*) INDICAT
       IF (INDICAT .EQ. "Y") THEN
       MOVE2=1
         ELSE
        MOVE2=0
       END IF

       WRITE(6,100) "Should the relations between points be"
       WRITE(6,100) "considered symmetric?"
       WRITE(6,100) "ENTER Y OR N"
       READ(5,*) INDICAT
       IF (INDICAT .EQ. "Y") THEN
       ZSYMMAT=1
         ELSE
       ZSYMMAT=0
       END IF

       WRITE(6,100) "Into how many increments should the circle"
       WRITE(6,100) "be divided to start?"
       WRITE(6,100) "ENTER AN INTEGER"
       READ(5,*) STARTINC

       WRITE(6,100) "How many increments should be added at"
       WRITE(6,100) "each iteration?"
       WRITE(6,100) "ENTER AN INTEGER"
       READ(5,*) BYINC

       WRITE(6,100) "What is the maximal number of increments"
       WRITE(6,100) "into which the circle can be be divided start?"
       WRITE(6,100) "ENTER AN INTEGER"
       READ(5,*) MAXINC

       WRITE(6,100) "What should the exponent be in determining"
       WRITE(6,100) "distances?"
       WRITE(6,100) "ENTER A REAL VALUE, 0 --> LOG,"
       WRITE(6,100) "NEGATIVE NUMBER --> ABSOLUTE VALUE"
       READ(5,*) KEXP

       WRITE(6,100) "Should between group non-radius distances be"
       WRITE(6,100) "normalized?"
       WRITE(6,100) "ENTER 0 FOR NO"
       WRITE(6,100) "1 FOR YES,"
       WRITE(6,100) "2 FOR TAKING LOGS,"
       WRITE(6,100) "3 FOR TAKING LOG(MAX) - LOG(VAL)"
       READ(5,*) NORMAL
       IF (NORMAL .GE. 2) THEN
       WRITE(6,100) "What is the minimum value for a between group"
       WRITE(6,100) "Element?"
       WRITE(6,100) "ENTER A REAL VALUE"
       READ(5,*) MINVALG
       END IF
       WRITE(6,100) "Should the scaling be based on angles or"
       WRITE(6,100) "Euclidean distances?"
       WRITE(6,100) "ENTER 1 FOR ANGLES, 0 FOR DISTANCES"
       READ(5,*) BYANGLE
       WRITE(6,100) "Should the distances be scaled by"
       WRITE(6,100) "the radius of the relevant actors?"
       WRITE(6,100) "ENTER 1 FOR SCALED, 0 FOR NOT SCALED"
       READ(5,*) BYSCALE
       WRITE(6,100) "By what nymber should the radius be divided?"
       WRITE(6,100) "ENTER A REAL VALUE"
       READ(5,*) DRADIUSG


       END IF
C      ACCEPT THE DEFAULTS BETWEEN GROUPS

       WRITE(6,100) "Within Groups"
       WRITE(6,100) 'The defaults are:'

       WRITE(6,200) 'CENTERI',CENTERI
       WRITE(6,200) 'DANCHORI',DANCHORI
       WRITE(6,200) 'DANCH2I',DANCH2I
       WRITE(6,200) 'MOVE2I',MOVE2I
       WRITE(6,200) 'ZSYMMATI',ZSYMMATI
       WRITE(6,200) 'STARTINI',STARTINI
       WRITE(6,200) 'BYINCI',BYINCI
       WRITE(6,200) 'KEXPI',KEXPI
       WRITE(6,200) 'MAXINCI',MAXINCI
       WRITE(6,200) 'NORMALI',NORMALI
       WRITE(6,200) 'MINVALI',MINVALI
       WRITE(6,200) 'BYANGLEI',BYANGLEI
       WRITE(6,200) 'BYSCALEI',BYSCALEI
       WRITE(6,200) 'DRADIUSI',DRADIUSI



       WRITE(6,100) 'Accept the defaults?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN

       WRITE(6,100) "Should the points be recentered about the group"
       WRITE(6,100) "coordinates?"
       WRITE(6,100) "ENTER Y or N"
       READ(5,*) INDICATE
        IF (INDICAT(1:1) .EQ. 'N') THEN
        CENTERI=0
        ELSE
        CENTERI=1
        END IF
       WRITE(6,100) "Which actor should be used for the anchoring?"
       WRITE(6,100) "ENTER THE NUMBER OF THE ACTOR IN "
       WRITE(6,100) "TERMS OF IT'S ORDER IN CENTRALITY"
       WRITE(6,100) "A VALUE GREATER THAN THE NUMBER OF POINTS"
       WRITE(6,100) "WILL BE CONVERTED TO THE MAXIMUM."
       WRITE(6,100) "A VALUE LESS THAN 1 WILL INDICATE A RANDOM"
       WRITE(6,100) "CHOICE, A VALUE OF LESS THAN 0 WILL"
       WRITE(6,100) "INDICATE THAT PERCENTILES (I.E. MEDIANS,"
       WRITE(6,100) "QUARTILES, ETC) SHOULD BE USED TO "
       WRITE(6,100) "ESTABLISH THE MEASURE OF CENTRALITY"
       READ(5,*) DANCHORI
       IF (DANCHOR .LT. 0) THEN
       WRITE(6,100) "What percentiel should be used?"
       WRITE(6,100) "ENTER A REAL VALUE BETWEEN 0 AND 1"
       READ(5,*) PCTCENI1
       END IF
       WRITE(6,100) "Which group should be used for the second"
       WRITE(6,100) "anchor?"
       WRITE(6,100) "ENTER THE NUMBER OF THE GROUP IN "
       WRITE(6,100) "TERMS OF IT'S ORDER IN CENTRALITY"
       WRITE(6,100) "A VALUE GREATER THAN THE NUMBER OF POINTS"
       WRITE(6,100) "WILL BE CONVERTED TO THE MAXIMUM."
       WRITE(6,100) "A VALUE LESS THAN 1 WILL INDICATE A RANDOM"
       WRITE(6,100) "CHOICE, A VALUE OF LESS THAN 0 WILL"
       WRITE(6,100) "INDICATE THAT PERCENTILES (I.E. MEDIANS,"
       WRITE(6,100) "QUARTILES, ETC) SHOULD BE USED TO "
       WRITE(6,100) "ESTABLISH THE MEASURE OF CENTRALITY"
       READ(5,*) DANCH2I
       IF (DANCHOR .LT. 0) THEN
       WRITE(6,100) "What percentiel should be used?"
       WRITE(6,100) "ENTER A REAL VALUE BETWEEN 0 AND 1"
       READ(5,*) PCTCENI2
       END IF
       WRITE(6,100) "Should the second anchor be repositioned?"
       WRITE(6,100) "ENTER Y OR N"
       READ(5,*) INDICAT
       IF (INDICAT .EQ. "Y") THEN
       MOVE2I=1
         ELSE
        MOVE2I=0
       END IF

       WRITE(6,100) "Should the relations between points be"
       WRITE(6,100) "considered symmetric?"
       WRITE(6,100) "ENTER Y OR N"
       READ(5,*) INDICAT
       IF (INDICAT .EQ. "Y") THEN
       ZSYMMATI=1
         ELSE
       ZSYMMATI=0
       END IF

       WRITE(6,100) "Into how many increments should the circle"
       WRITE(6,100) "be divided to start?"
       WRITE(6,100) "ENTER AN INTEGER"
       READ(5,*) STARTINI

       WRITE(6,100) "How many increments should be added at"
       WRITE(6,100) "each iteration?"
       WRITE(6,100) "ENTER AN INTEGER"
       READ(5,*) BYINCI

       WRITE(6,100) "What is the maximal number of increments"
       WRITE(6,100) "into which the circle can be be divided start?"
       WRITE(6,100) "ENTER AN INTEGER"
       READ(5,*) MAXINCI

       WRITE(6,100) "What should the exponent be in determining"
       WRITE(6,100) "distances?"
       WRITE(6,100) "ENTER A REAL VALUE, 0 --> LOG,"
       WRITE(6,100) "NEGATIVE NUMBER --> ABSOLUTE VALUE"
       READ(5,*) KEXPI

       WRITE(6,100) "Should within group non-radius distances be"
       WRITE(6,100) "normalized?"
       WRITE(6,100) "ENTER 0 FOR NO"
       WRITE(6,100) "1 FOR YES,"
       WRITE(6,100) "2 FOR TAKING LOGS,"
       WRITE(6,100) "3 FOR TAKING LOG(MAX) - LOG(VAL)"
       READ(5,*) NORMALI
       IF (NORMAL .GE. 2) THEN
       WRITE(6,100) "What is the minimum value for a within group"
       WRITE(6,100) "Element?"
       WRITE(6,100) "ENTER A REAL VALUE"
       READ(5,*) MINVALI
       END IF
       WRITE(6,100) "Should the scaling be based on angles or"
       WRITE(6,100) "Euclidean distances?"
       WRITE(6,100) "ENTER 1 FOR ANGLES, 0 FOR DISTANCES"
       READ(5,*) BYANGLEI
       WRITE(6,100) "Should the distances be scaled by"
       WRITE(6,100) "the radius of the relevant actors?"
       WRITE(6,100) "ENTER 1 FOR SCALED, 0 FOR NOT SCALED"
       READ(5,*) BYSCALEI
       WRITE(6,100) "By what nymber should the radius be divided?"
       WRITE(6,100) "ENTER A REAL VALUE"
       READ(5,*) DRADIUSI

       END IF
C       ACCEPT THE DEFAULTS


       WRITE(6,100) "Rotating the Circles:"
       WRITE(6,200) 'RINCREM',RINCREM
       WRITE(6,200) 'MEASURE',MEASURE
       WRITE(6,200) 'EXTREME',EXTREME
       WRITE(6,100) 'Accept the defaults?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN

       WRITE(6,100) "How many increments should be considered in "
       WRITE(6,100) "rotating the groups?"
       WRITE(6,100) "ENTER AN INTEGER"
       READ(5,*) RINCREM
       WRITE(6,100) "Which measure should be used as a basis"
       WRITE(6,100) "of association between actors and other"
       WRITE(6,100) "subgroups?"
       WRITE(6,100) "ENTER 1 FOR COMPACTNESS, 2 FOR DENSITY"
       READ(5,*) MEASURE
       WRITE(6,100) "Should actors with a larger radius be"
       WRITE(6,100) "given more weight in determining the rotation?"
       WRITE(6,100) "ENTER Y OR N"
       READ(5,*) INDICAT
       IF (INDICAT .EQ. "Y") THEN
       EXTREME=1
         ELSE
       EXTREME=0
       END IF

       END IF
C       ACCEPT THE DEFAULTS ON ROTATION

       END IF
       

       WRITE(6,104)
       WRITE(6,100) "Would you like these parameters saved to a file?"
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        WRITE(6,100) "ENTER A LEGITIMATE FILENAME BETWEEN 1 AND 16"
        WRITE(6,100) "CHARACTERS"
        READ (5,*) PARMFILE
        OPEN(77,file=PARMFILE)
        WRITE(6,100) "Note that you may use these parameters for"
        WRITE(6,100) "other runs on the same or different data"
        WRITE(6,100) "by placing the contents of "
        WRITE(6,100) PARMFILE
        WRITE(6,100) "into 'kliqfind.par' ."
      WRITE(77,34348)
      DO 6445 I=1,NPARGRP
      WRITE(77,3335) PARTITLE(I),PARFORM(I)
      IF (I .EQ. 1) THEN
      WRITE(77,PARFORM(I)) NUMDYAD,DYDTRIAD,USETRIAD,NOATTACH,RASEED
      END IF
      IF (I .EQ. 2) THEN
      WRITE(77,PARFORM(I)) DIRECT,THRESHT,LOOKT,MAXSEED
      END IF
      IF (I .EQ. 3) THEN
      WRITE(77,PARFORM(I)) BOUNDVAL,FIXR,BLABOUND,BETWMULT
      END IF
      IF (I .EQ. 4) THEN
      WRITE(77,PARFORM(I)) NEARVAL,PCTILE,MUTDYAD,NONEG,HALFDYAD,
     C DISSOLVE
      END IF

      IF (I .EQ. 5) THEN
      WRITE(77,PARFORM(I)) STOPVAL,KCOUNT2,QUICKEND
      END IF

      IF (I .EQ. 6) THEN
      WRITE(77,PARFORM(I)) STRUCTEQ,NETWORK,ACTRSQR
      END IF

      IF (I .EQ. 7) THEN
      WRITE(77,PARFORM(I)) QUANTYPE,SQUAREIT,NETLEV,PERGROUP,COLWT,
     C ROWWT,HYPERG
      END IF

      IF (I .EQ. 8) THEN
      WRITE(77,PARFORM(I)) TRANSPOS,REWEIGHT,SYMMAT,INVERT,RECTMAT,GUSEMAT,
     C TAGALONG
      END IF

      IF (I .EQ. 9) THEN
      WRITE(77,PARFORM(I)) NEVAL,BASEVAL,TOPVAL,NUMRES,NEWGRPS
      END IF
      IF (I .EQ. 10) THEN
      WRITE(77,PARFORM(I)) IGRATIO,NUMDIM,MINPICT,
     C CENTER,DANCHOR,DANCHOR2,MOVE2,ZSYMMAT,
     C STARTINC,BYINC,MAXINC,KEXP,NORMAL,MINVALG,BYANGLE,BYSCALE,
     C PCTCENG1,PCTCENG2,DRADIUSG
      END IF

      IF (I .EQ. 11) THEN
      WRITE(77,PARFORM(I)) CENTERI,DANCHORI,DANCH2I,MOVE2I,ZSYMMATI,
     C STARTINI,BYINCI,MAXINCI,KEXPI,NORMALI,MINVALI,BYANGLEI,
     C BYSCALEI,
     C PCTCENI1,PCTCENI2,DRADIUSI
      END IF
      IF (I .EQ. 12) THEN
      WRITE(77,PARFORM(I)) RINCREM,MEASURE,EXTREME
      END IF
C BSBS
      WRITE(77,3337) TCHAR(I),LISTVAR(I)
C      WRITE(77,100) " "
06445  CONTINUE

C        WRITE(77,107) NOATTACH,NEVAL,DYDTRIAD,USETRIAD,
C     C  NUMDYAD,
C     C NEARVAL,STOPVAL,
C     C SYMMAT,THRESHT,LOOKT,DIRECT,KCOUNT2,BASEVAL,BOUNDVAL,
C     C PCTILE,STRUCTEQ,NETWORK,ACTRSQR,QUICKEND,ROWWT,COLWT,
C     C QUANTYPE,SQUAREIT,UNWEIGHT,NUMRES,RASEED,NETLEV,PERGROUP,
C     C TOPVAL,TRANSPOS
        CLOSE(77)
        END IF

       WRITE(6,104)
       WRITE(6,100) "Execute a clustering:"
       INDICAT(1:1) ='N'
       IF (MATTYPE .EQ. 'p') THEN
       WRITE(6,100) "Your current data file is",INFILE
       WRITE(6,205) "It contains ",NUMTEACH
       WRITE(6,205) "observations, in ",MAXDEPT
       WRITE(6,100) "groups. given in ",NUMOBS
       WRITE(6,100) "non-zero observations."
       WRITE(6,100) "It is assumed that the data are in matrix"
       WRITE(6,100) "format and that the a priori groups come"
       WRITE(6,100) "from this file.  If you do not wish to make"
       WRITE(6,100) "these assumptions or if you do not wish"
       WRITE(6,100) "to continue with this file then respond"
       WRITE(6,100) "'N' to the next question:"
       WRITE(6,100) "Would you like to continue with this file?"
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
       END IF
      
        IF (INDICAT(1:1) .EQ. 'Y') THEN
         GCONTIN=1
         RETURN
        ELSE
       WRITE(6,100) "Would you like to cluster some data?"
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN
        STOP
         ELSE
       WRITE(6,100) "Which file contains the data?"
        WRITE(6,100) "ENTER A LEGITIMATE FILENAME BETWEEN 1 AND 16"
       READ(5,*) INFILE
       WRITE(6,100) "**********************"              
       WRITE(6,100) "FORMAT OF DATA FILE"
       WRITE(6,100) "If the data are in list format, a typical"
       WRITE(6,100) "line would look like this:"
       WRITE(6,100)
       WRITE(6,100) "Choser ID Chosen ID     Weight "
       WRITE(6,100) "123456789012345678901234567890"
       WRITE(6,100)
       WRITE(6,100) "That is, in format 3I10."
       WRITE(6,100) "For example, the line:"
       WRITE(6,100)
       WRITE(6,100) "         1         3         6"
       WRITE(6,100) "123456789012345678901234567890"
       WRITE(6,100)
       WRITE(6,100) "indicates that actor 1 directs a connection to"
       WRITE(6,100) "actor 3 with a value of 6."
       WRITE(6,100)
       WRITE(6,100) "ENTER ANY NUMBER TO CONTINUE"
       READ(5,*) TEMP

        WRITE(6,100) "A priori placements of actors in groups"
      WRITE(6,100) "Are indicated by assigning the 'directed to' ID"
       WRITE(6,100) "equal to 99999.  For example, the line"
       WRITE(6,100)
       WRITE(6,100) "         1     99999         4"
       WRITE(6,100) "123456789012345678901234567890"
       WRITE(6,100)
       WRITE(6,100) "indicates that actor 1 is in a priori group 4."
       WRITE(6,100)
       WRITE(6,100) "ENTER ANY NUMBER TO CONTINUE"
       READ(5,*) TEMP
        WRITE(6,100) "**********************"              


       WRITE(6,100) "In matrix format, the above would"
       WRITE(6,100) "be represented by the first row of the matrix"
       WRITE(6,100) "which would look like the following:"
       WRITE(6,100)
       WRITE(6,100) " 40060000000000000"
       WRITE(6,100) "XX1234567890123456"
       WRITE(6,100)
       WRITE(6,100) "the first two columns indicate"
       WRITE(6,100) "the apriori placement of the actor. Actor"
       WRITE(6,100) "1 is in group 4.  In the following columns"
       WRITE(6,100) "there is one column allocated for the"
       WRITE(6,100) "connection directed by the row actor"
       WRITE(6,100) "to the actor represented in each column."
       WRITE(6,100) "In this case we see that actor 1"
       WRITE(6,100) "(because the row given above is the first row"
       WRITE(6,100) "in the matrix)"
       WRITE(6,100) "initiates a connection to actor 3 with a value"
       WRITE(6,100) "of 6 and directs 0 valued connections to all"
       WRITE(6,100) "other actors."
       WRITE(6,100)
       WRITE(6,100) "Are the data in list or matrix format?"

       WRITE(6,100) "ENTER l OR m (use lower case)"
       READ(5,*) MATTYPE
       WRITE(6,100)
       WRITE(6,100) "Is there another file with grouping information?"
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        IF (MATTYPE .EQ. "l") THEN 
        MATTYPE='b'
        ELSE
        MATTYPE='g'
        END IF
       WRITE(6,100) "What is the name of that file?"
       WRITE(6,100) "ENTER A LEGITIMATE FILENAME BETWEEN 1 AND 16"
       WRITE(6,100) "CHARACTERS"
       READ(5,*) PRIORFIL
       END IF
       WRITE(6,104)
       WRITE(6,100) 'Auxilliary information:'
       WRITE(6,100) 'The defaults are:'
       WRITE(6,203) 'HITFILE',HITFILE
       WRITE(6,203) 'LABFILE',LABFILE
       WRITE(6,203) 'TITLFILE',TITFILE
       WRITE(6,100) 'Accept the defaults?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN
       
       WRITE(6,100) "Is there another file with hitlist information?"
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       WRITE(6,100) "What is the name of that file?"
       WRITE(6,100) "ENTER A LEGITIMATE FILENAME BETWEEN 1 AND 16"
       WRITE(6,100) "CHARACTERS"
       READ(5,*) HITFILE
        ELSE
       NUMHLIST=0
        END IF

       WRITE(6,100) "Is there another file with labels information?"
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       WRITE(6,100) "What is the name of that file?"
       WRITE(6,100) "ENTER A LEGITIMATE FILENAME BETWEEN 1 AND 16"
       WRITE(6,100) "CHARACTERS"
       READ(5,*) LABFILE
        END IF

       WRITE(6,100) "Is there another file with titles information?"
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       WRITE(6,100) "What is the name of that file?"
       WRITE(6,100) "ENTER A LEGITIMATE FILENAME BETWEEN 1 AND 16"
       WRITE(6,100) "CHARACTERS"
       READ(5,*) TITFILE
       WRITE(6,100) "Would you like to enter the titles"
         WRITE(6,100) " interactively?"
       WRITE(6,100) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        DO 99 I=1,3
       WRITE(6,201) "INPUT TITLE",I
       READ(5,*) TITLES(I)
00099 CONTINUE
        END IF

         END IF
C        DEFAULTS

       WRITE(6,104)
       WRITE(6,100) "Control of output"
       WRITE(6,100) 'The defaults (which come from the file printo)'
       WRITE(6,100) 'are:'
       DO 7 P=1,NPO
       WRITE(6,100) APRINT(P)
       WRITE(6,100) PRINTO(P)
00007   CONTINUE
       WRITE(6,100) 'Accept the defaults?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN
       WRITE(6,100) "For each of the following pieces of output"
       WRITE(6,100) "ENTER A 1 IF YOU WOULD LIKE THAT OUTPUT,"
       WRITE(6,100) "A 0 OTHERWISE"
       DO 3 P=1,NPO
       WRITE(6,100) APRINT(P)
       READ(5,*) PRINTO(P)
00003   CONTINUE
        END IF
C       DEFAULTS
        END IF
        END IF
        END IF
        WRITE(6,100) "In the future, to run the program, you must"
        WRITE(6,100) "type:"
        WRITE(6,100) "kliquefind datafile [option] placements"
        WRITE(6,100) "Where option = 'l' if data are in list "
        WRITE(6,100) "format, 'g' if you want a priori placements"
        WRITE(6,100) "to come from a second file (placements)"
        WRITE(6,100) "and 'b' if data are in list format and you"
        WRITE(6,100) "have data in a second file for a priori groups."
        WRITE(6,100)
       WRITE(6,100) "Would you like to see examples?"
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN

        WRITE(6,100)  "For example:"
        WRITE(6,100)  "kliqfind sampson.mat"
        WRITE(6,100)
        WRITE(6,100)  "Tells kliquefinder to use matrix data from "
        WRITE(6,100)  "sampson.mat"
        WRITE(6,100)
        WRITE(6,100)   "But if the data are in list format:"
        WRITE(6,100)   "kliqfind sampson.list l"
        WRITE(6,100)  
        WRITE(6,100)  "Would be the appropriate command."
        WRITE(6,100)  
        WRITE(6,100) "Note that it is not necessary to use the "
        WRITE(6,100) "'.list' suffix for files in list format"
        WRITE(6,100) "and the '.mat' suffix for files in matrix"
        WRITE(6,100) "format, although it might help you to do so."
        WRITE(6,100)

       WRITE(6,100) "ENTER ANY NUMBER TO CONTINUE"
       READ(5,*) TEMP
        WRITE(6,100) "**********************"              

        WRITE(6,100) "If you wanted to use a priori groups in a file"
        WRITE(6,100) "called: 'sampson.place' then you would type:"
        WRITE(6,100) 
        WRITE(6,100) "kliqfind sampson.mat g sampson.place"
        WRITE(6,100)
        WRITE(6,100)  "If the data were in list format *and* you"
        WRITE(6,100) "wanted to use the groups in sampson.place,"
        WRITE(6,100) "then you would type:"
        WRITE(6,100)
        WRITE(6,100) "kliqfind sampson.list b sampson.place"
        WRITE(6,100)
        WRITE(6,100) "Use the option 'p' to indicate that you want"
        WRITE(6,100) "Enter the parameters and designate the"
        WRITE(6,100) "appropriate files interactively." 
        WRITE(6,100)
       WRITE(6,100) "ENTER ANY NUMBER TO CONTINUE"
       READ(5,*) TEMP
        WRITE(6,100) "**********************"              
        WRITE(6,100) "For example:"
        WRITE(6,100) 
        WRITE(6,100) "kliqfind sampson.mat p"
        WRITE(6,100)
        WRITE(6,100) "Allows you to enter the parameters"
        WRITE(6,100) "interactively"
        END IF

C       WRITE(6,100) 'Stopping processing'
C       WRITE(6,100) "**********************"              
C       WRITE(6,100)
C       Stop
C       END IF
C        WRITE(6,100)
C        WRITE(6,100) "**********************"              
C        WRITE(6,100) "CLUSTERING BEGINS"
C        WRITE(6,100)

 3335  FORMAT(A200,/,A80)
 3337   FORMAT(10(A100,/))
34348  FORMAT(/////////////////////)
00100   FORMAT(A)
00102     FORMAT(I1)
00104      FORMAT ("************************************")
00201       FORMAT(A,I3)
00203        FORMAT(10(A,2X))
00200        FORMAT(A,F10.5)
00205        FORMAT(A,I3)
  107 FORMAT(I1,1X,I5,1X,I1,1X,I1,1X,I4,1X,400(F10.5,1X))

         RETURN 
         END


C                        I   I       I        i        I
C        CALL SIMULATE(MAXG,KPGROUP,NUMTEACH,MAXCHOIC,MAXFREQ,
C         I     I     ?     I
C     C INMAT,DEPART,SSEED,USEMARG)


      SUBROUTINE SIMULATE(NUMGROUP,PERGROUP,NUMPEOPL,ACHOICE,MAXFREQ,
     C OUTMAT,DEPART,QSEED,USEMARG)
      INTEGER D,I,J,DEPART(251),D2,D3,CHOICES(251),OUTMAT(251,251),
     C        D4,D5,COL,NUMGROUP,PERGROUP,NUMPEOPL,ONELESS,ACHOICE,
     C        MAXFREQ,MV,FREQS(251),USEMARG,TID
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
      SUBROUTINE PISIM(MAXSIZE,PERGROUP,NUMPEOPL,TCHOICE,MAXFREQ,
     C OUTMAT,DEPART,QSEED,USEMARG,INDEPT,OUTDEPT,BYDENSE,SIMRAND,
     C PSIZE,QSIZE,BASEG,PKPG,QKPG,BYLAMN)


      INTEGER D,I,J,DEPART(251),D2,D3,OUTMAT(251,251),SIMRAND,THISD,
     C        D4,D5,COL,NUMGROUP,PERGROUP,NUMPEOPL,ONELESS,ACHOICE,
     C        MAXFREQ,MV,FREQS(251),USEMARG,TID,NR,AC,DIDG,DIDNG,
     C       OPENS,HAVEMEM,FILLG,FILLNG,COUNTING,COUNTNNG,FG,NFG,
     C  COUNTG(251),COUNTNON(251),ING,GOTTEM,TG,IR(1),BYDENSE,
     C SORTEM(251),U,TCHOICE,DIDEM,RANGEG,BASEG,RANGEC,MINCHOI,
     C ROOT,BYLAMN,MAXSIZE,OPENSX
       INTEGER*4
     C  MEMBERS(100,50),NONMEM(100,251)
       DOUBLE PRECISION QSEED
       REAL DEVIATE(1),INDEPT(251),OUTDEPT(251),EXTRA,PORTION,KPOSS,
     C  LOW,HIGH,PSIZE,QSIZE,PKPG,QKPG,KPOSS2,
     C ALPHA,BETA,GAMMA,DELTA,PROPIN,PROPOUT,R(1),LE

C       QSEED=5163.D0


       NR=1
       LOW=0
       HIGH=1
       DO 22 I=1,NUMPEOPL
       DO 33 J=1,NUMPEOPL
       OUTMAT(I,J)=0
00033   CONTINUE
00022    CONTINUE




      DO 84 MV=1,MAXFREQ
       FREQS(MV)=MV
   84 CONTINUE

       ONE=1

       IF ((PSIZE .GT. 0) .AND. (QSIZE .GT. 0)) THEN
       D=0
       DIDEM=0

       I=0
       DO WHILE (DIDEM .EQ. 0)
       I=I+1
       R(1)=GENBET(PSIZE,QSIZE)
       COUNTG(I)=0
       COUNTG(I)=INT(R(1)*MAXSIZE+ BASEG + .9999)
       IF (COUNTG(I) .GT. (NUMPEOPL-(D+BASEG))) THEN
       DIDEM=1
       COUNTG(I)=NUMPEOPL-D
       END IF

        DO 44 J=1,COUNTG(I) 
        D=D+1
        DEPART(D)=I
00044    CONTINUE

       END DO
       NUMGROUP=I

       ELSE

       D=0
      DO 2 I=1,NUMGROUP
       DO 3 J=1,PERGROUP
       D=D+1
       DEPART(D)=I
    3 CONTINUE
    2 CONTINUE

      CALL SRESORTO(DEPART,NUMPEOPL,QSEED)
        END IF
C       ON RAND COUNT N
 
       DO 333 J=1,NUMGROUP
       COUNTG(J)=0
       COUNTNON(J)=0
00333   CONTINUE

      DO 77 I=1,NUMPEOPL
      DO 88 J=1,(NUMGROUP-1.)
      TG=J
      IF (TG .GE. DEPART(I)) THEN 
       TG=TG+1
      END IF
      COUNTNON(TG)=COUNTNON(TG)+1.
      NONMEM(TG,COUNTNON(TG))=I
00088  CONTINUE
00077   CONTINUE

      DO 277 I=1,NUMPEOPL
      COUNTG(DEPART(I))=COUNTG(DEPART(I))+1.
      MEMBERS(DEPART(I),COUNTG(DEPART(I)))=I      
00277  CONTINUE

       ROOT=1
      DO 6 D4=1,NUMPEOPL
       IF ((PKPG .GT. 0) .AND. (QKPG .GT. 0)) THEN
       R(1)= GENBET(PKPG,QKPG)
       ACHOICE=INT(R(1)*PERGROUP+TCHOICE+.99999)
       ELSE
       ACHOICE=TCHOICE
        END IF

      THISD=DEPART(D4)


 
      IF (BYDENSE .EQ. 0) THEN
       KPOSS=ACHOICE
       KPOSS2=ACHOICE
      ELSE
       KPOSS=COUNTG(THISD)-1.
       KPOSS2=NUMPEOPL-COUNTG(THISD)
      END IF
       IF (BYLAMN .EQ. 1) THEN
       CALL CONVLAMN(INDEPT(THISD),(COUNTG(THISD)-1),
     C ACHOICE,NUMPEOPL,
     C ALPHA,BETA,GAMMA,DELTA,ROOT,LE)
       PROPIN=DELTA/KPOSS
       PROPOUT=BETA/KPOSS2


       ELSE
       PROPIN=INDEPT(THISD)
       PROPOUT=OUTDEPT(THISD)
       END IF

       IF (SIMRAND .EQ. 0) THEN
      PORTION=KPOSS*INDEPT(THISD)
      FG=INT(PORTION)
      EXTRA=PORTION-FG
      DEVIATE(1)=GENUNF(LOW,HIGH)
C       CALL GGUBS(QSEED,NR,DEVIATE)
      IF (DEVIATE(1) .LE. EXTRA) THEN
       THEN FG=FG+1
      END IF
      IF (FG .GT. (COUNTG(THISD) -1.)) THEN
      FG=COUNTG(THISD)-1.
      END IF
      NFG=ACHOICE-FG
      DO 8872 U=1,NUMPEOPL
      SORTEM(U)=IGNUIN(1,NUMPEOPL)
08872  CONTINUE
C      CALL GGUD(QSEED,NUMPEOPL,NUMPEOPL,SORTEM)
      COUNTK=1
      COUNTING=0
      COUNTNNG=0
      FILLG=0
      FILLNG=0
      DO WHILE ((COUNTK .LE. NUMPEOPL) .AND.
     C ((FILLG .EQ. 0) .OR. (FILLNG .EQ. 0)))
      IF ((FILLG .EQ. 0) .AND. (DEPART(SORTEM(COUNTK)) .EQ. THISD))
     C   THEN
       IF (COUNTING .LT. FG) THEN
        CALL SRESORTO(FREQS,MAXFREQ,QSEED)
        OUTMAT(D4,SORTEM(COUNTK))=FREQS(1)
        COUNTING=COUNTING+1
       ELSE
       FILLG=1
       END IF
      END IF
      IF ((DEPART(SORTEM(COUNTK)) .NE. THISD) .AND. 
     C (FILLNG .EQ. 0)) THEN
        IF (COUNTNNG .LT. NFG) THEN 
          CALL SRESORTO(FREQS,MAXFREQ,QSEED)
          OUTMAT(D4,SORTEM(COUNTK))=FREQS(1)
          COUNTNNG=COUNTNNG+1
        ELSE
          FILLNG=1
        END IF

      END IF

      COUNTK=COUNTK+1
      END DO

      ELSE
C       SIMRAND=1

      IF (BYDENSE .EQ. 0) THEN      
      OUTMAT(D4,D4)=99
      DIDG=1
      DIDNG=0
      IF (D4 .GT. 1) THEN
       OUTMAT((D4-1.),(D4-1.))=0
      END IF
      DO 99 AC=1,ACHOICE
      DEVIATE(1)=GENUNF(LOW,HIGH)
C       CALL GGUBS(QSEED,NR,DEVIATE)
      OPENS=COUNTG(THISD)-DIDG
      OPENSX=COUNTNON(THISD)-DIDNG
      IF ((OPENS .GT. 0) .AND. ((DEVIATE(1) .LE. PROPIN) .OR.
     C (OPENSX .LT. 1))) THEN
      ING=1
      IR(1)=IGNUIN(1,OPENS)
C      CALL GGUD(QSEED,OPENS,NR,IR)
      ELSE
      ING=0
      IR(1)=IGNUIN(1,OPENSX)
C      CALL GGUD(QSEED,(COUNTNON(THISD)-DIDNG),NR,IR)
      END IF

      GOTTEM=0
      DO WHILE (GOTTEM .EQ. 0)
       IF (ING .EQ. 1) THEN
      HAVEMEM=MEMBERS(THISD,IR(1))
       ELSE
        HAVEMEM=NONMEM(THISD,IR(1))
       END IF
        IF (OUTMAT(D4,HAVEMEM) .GT. 0) THEN
         IR(1)=IR(1)+1
        ELSE
        GOTTEM=1
        CALL SRESORTO(FREQS,MAXFREQ,QSEED)
        OUTMAT(D4,HAVEMEM)=FREQS(1)
        IF (ING .EQ. 1) THEN
        DIDG=DIDG+1
        ELSE 
        DIDNG=DIDNG+1
        END IF
        END IF
       END DO
00099   CONTINUE

      ELSE
C      BYDENSE

      DO 10 D3=1,NUMPEOPL
      IF (D4 .NE. D3) THEN
       IF (DEPART(D3) .EQ. DEPART(D4)) THEN
        PI=PROPIN
       ELSE
        PI=PROPOUT
       END IF
       DEVIATE(1)=GENUNF(LOW,HIGH)
C       CALL GGUBS(QSEED,NR,DEVIATE)
       IF (DEVIATE(1) .LE. PI) THEN 
       OUTMAT(D3,D4)=1
       ELSE 
       OUTMAT(D3,D4)=0
       END IF
      END IF
   10 CONTINUE
      END IF
C      BYDENSE
       END IF
C      SIMRAND

    6 CONTINUE
      OUTMAT(NUMPEOPL,NUMPEOPL)=0
      RETURN
      END

C
C     SUBROUTINE FOR RESTORTING ELEMENTS OF GROUP LIST
      SUBROUTINE SRESORTO (ELEMENTS,NUMELEM,RSEED)
      INTEGER ELEMENTS(251),NUMELEM,FAKETCH
      INTEGER OELEMENT(251),Z,J,K,I,CONVERT(251),RITER
      DOUBLE PRECISION RSEED
      INTEGER RANDARRY(251),OLDPOS(251)
C
C   NOW THE RANDARRY IS AN ARRAY OF RANDOM NUMBERS OF LENGTH NUMELEM
C
      DO 3 K=1,NUMELEM
      OELEMENT(K)=ELEMENTS(K)
    3 CONTINUE
      FAKETCH=NUMELEM
      CALL GGPER(RSEED,FAKETCH,RANDARRY)
      DO 4 Z=1,NUMELEM
      ELEMENTS(Z)=OELEMENT(RANDARRY(Z))
C      WRITE(18,219) Z,RANDARRY(Z),OELEMENT(Z),ELEMENTS(Z)
    4 CONTINUE
  219 FORMAT(I3,1X,I5,1X,I5,1X,I3)
      RETURN
      END
C     END SUBROUTINE RESORTO




      SUBROUTINE GETRS2(NEWDEPT,NUMTEACH,MAXDEPT,
     C DEPARTN,WANTSS,RCHOICE,SRI,SSRI,OLDCON,XSRI,
     C MEANWT,VARWT,SRIU2,GMEAN,GVAR,GVPLUS,GMPLUS,GVSUB,
     C IVSUB,GMSUB,IMSUB,DTOTK,DTOTE,DTOTV,IDELTKA,QUANTYPE,
     C TOBFUN,SQUAREIT,NETLEV,PERGROUP,WANTFUN,CURVAR,CURMEAN,
     C PLUSVAR,PLUSMEAN,OLDGZ,HYPERG,BLT,MAXCH,SAMEG1,K1,K2,
     C COLWT,ROWWT)


      INTEGER NEWDEPT(251),NUMTEACH,I,J,MAXDEPT,
     C DEPARTN(251),TN,WANTSS,OLDCON(251),RCHOICE(251),
     C QUANTYPE,IFSQUARE,SQUAREIT,NETLEV,MAXCH,SAMEG1,
     C PERGROUP,WANTFUN,ACTG,HYPERG,TI,LADD,BLT,K1,K2

      REAL TREAL,TDENOM,SRI(251),GMEAN(251),GVAR(251),NG,N,
     C SSRI(251),XSRI(251),MEANWT(251),VARWT(251),SRIU2(251),
     C TUFFY,TUFFY2,ADDVAR,GVAR2,GVPLUS(251),GMPLUS(251),
     C GVSUB(251),IVSUB(251),GMSUB(251),IMSUB(251),
     C DTOTK,DTOTE,DTOTV,KADDVAR,KADDM,IDELTKA(251),TOBFUN,
     C DENOM,CURVAR(251),CURMEAN(251),PLUSVAR(251),PLUSMEAN(251),
     C OLDGZ(251),SIDEWT,KSIGN,LGMEAN(4),LTOLDCON(4),PENNUM,
     C COLWT,ROWWT
       REAL ZCOMPMAT(251,251)
       INTEGER HUBERT(251,251),RESULTM(251,251),ALLGROUP(251,251),
     C NEWMAT(251,251)

C       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT
       REAL BLAUC(251,251),DIFF(251,251)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF

       IFSQUARE=SQUAREIT+1
       DTOTE=0
       DTOTK=0
       DTOTV=0
       SAMEG1=0 
      DO 4 I=1,MAXDEPT
        SAMEG1=SAMEG1+DEPARTN(I)*(DEPARTN(I)-1)
C       SRIU2(I)=0.000
C       XSRI(I)=0.000
       SRI(I)=0.000
C
C       SSRI(I)=0.000
       GVAR(I)=0.000
       GMEAN(I)=0.000
       GVPLUS(I)=0.000
       GMPLUS(I)=0.000
       GVSUB(I)=0.0000
       GMSUB(I)=0.0000
       OLDGZ(I)=0.000
       OLDCON(I)=0
00004   CONTINUE
       SAMEG1=SAMEG1*MAXCH
       N=NUMTEACH-1
      DO 06 I=1,NUMTEACH
        PLUSMEAN(I)=0.000
        PLUSVAR(I)=0.000
        CURMEAN(I)=0.0000
        CURVAR(I)=0.000
        IDELTKA(I)=0.000
        IVSUB(I)=0.000
        IMSUB(I)=0.000
        IF (RCHOICE(I) .GT. 0) THEN
        NG=DEPARTN(NEWDEPT(I))-1
        SRI(NEWDEPT(I))=SRI(NEWDEPT(I))+RCHOICE(I)*MEANWT(I)
        IF (NG .GT. 0) THEN
C        SRIU2(NEWDEPT(I))=SRIU2(NEWDEPT(I))+
C     C  RCHOICE(I)*MEANWT(I)**2
C        SSRI(NEWDEPT(I))=SSRI(NEWDEPT(I))+ 
C     C  (MEANWT(I)*RCHOICE(I))**2 -
C     C  MEANWT(I)**2 * RCHOICE(I)
C        XSRI(NEWDEPT(I))=XSRI(NEWDEPT(I)) + 
C     C   RCHOICE(I)*VARWT(I)


         KADDVAR=
     C   (RCHOICE(I)*NG*(N-NG)*MEANWT(I)**2 /N**2) *
     C    (1 + (N*VARWT(I))/((N-NG)*MEANWT(I)**2) - 
     C   HYPERG*((RCHOICE(I) - 1)/ (N-1)))
         CURVAR(I)=KADDVAR
         GVAR(NEWDEPT(I))=GVAR(NEWDEPT(I)) +KADDVAR
         KADDM=
     C  MEANWT(I)*RCHOICE(I)*NG/N
         CURMEAN(I)=KADDM
        GMEAN(NEWDEPT(I))= GMEAN(NEWDEPT(I)) +KADDM
        DTOTE=DTOTE+ KADDM
        DTOTV=DTOTV+KADDVAR
         END IF
        TNG=NG+1
 
C     GVPLUS(251),GMPLUS(251),
C     C GVSUB(251),IVSUB(251),GMSUB(251),IMSUB(251)

         PLUSVAR(I)=
     C   (RCHOICE(I)*TNG*(N-TNG)*MEANWT(I)**2 /N**2) *
     C    (1 + (N*VARWT(I))/((N-TNG)*MEANWT(I)**2) - 
     C   HYPERG*((RCHOICE(I) - 1)/ (N-1)))
         GVPLUS(NEWDEPT(I))=GVPLUS(NEWDEPT(I)) + PLUSVAR(I)


        PLUSMEAN(I)=
     C  MEANWT(I)*RCHOICE(I)*TNG/N
         GMPLUS(NEWDEPT(I))=GMPLUS(NEWDEPT(I))+PLUSMEAN(I)
          IF (NG .GT. 1) THEN
          TNG=TNG-2
        IVSUB(I)=
     C   (RCHOICE(I)*TNG*(N-TNG)*MEANWT(I)**2 /N**2) *
     C    (1 + (N*VARWT(I))/((N-TNG)*MEANWT(I)**2) - 
     C   HYPERG*((RCHOICE(I) - 1)/ (N-1)))
        IMSUB(I)=
     C  MEANWT(I)*RCHOICE(I)*TNG/N
          ELSE
C        CURVAR(I)=999999
C        CURMEAN(I)=0
        IMSUB(I)=0.000
        IVSUB(I)=0.000
          END IF
         GMSUB(NEWDEPT(I))=GMSUB(NEWDEPT(I))+IMSUB(I)
         GVSUB(NEWDEPT(I))=GVSUB(NEWDEPT(I))+IVSUB(I)
        END IF

00006    CONTINUE
      TOBFUN=0
      DTOTK=0
      ACTG=0
      DO 012 G=1,MAXDEPT
      IF (DEPARTN(G) .GT. 1) THEN
      ACTG=ACTG+1
      END IF
00012  CONTINUE
       SIDEWT=1.000
       IF (NETLEV .EQ. 2) THEN 
       SIDEWT=0.000
       END IF
      DO 08 G=1,MAXDEPT
       OLDGZ(G)=0.0000
      DO 09 I=1,DEPARTN(G)
       DO 10 J=1,DEPARTN(G)
        OLDCON(G)=OLDCON(G)+NEWMAT(ALLGROUP(G,I),ALLGROUP(G,J))
        DTOTK=DTOTK+NEWMAT(ALLGROUP(G,I),ALLGROUP(G,J))
        IDELTKA(ALLGROUP(G,I))=IDELTKA(ALLGROUP(G,I)) +
     C  NEWMAT(ALLGROUP(G,I),ALLGROUP(G,J)) +
     C  NEWMAT(ALLGROUP(G,J),ALLGROUP(G,I)) * SIDEWT
00010  CONTINUE


        THISMEM=ALLGROUP(G,I)
        IF (CURVAR(THISMEM) .GT. 0) THEN
        OLDGZ(G)=OLDGZ(G)+(IDELTKA(THISMEM) - CURMEAN(THISMEM)) /
     C   SQRT(CURVAR(THISMEM))
         END IF
00009   CONTINUE

        IF ((DEPARTN(G) .GT. 0) .AND. ((WANTFUN .EQ. 1) 
     C                           .OR. (NETLEV .EQ. 1))) THEN
        IF (PERGROUP .EQ. 0) THEN
        DENOM=1
        ELSE
        DENOM=(DEPARTN(G)-1.)*ACTG
        END IF

        IF ((QUANTYPE .EQ. 1) .AND. (NETLEV .EQ. 0) .AND.
     C  (GVAR(G) .GT. 0.000000)) THEN
        TOBFUN=TOBFUN+(((OLDCON(G) - GMEAN(G))/GVAR(G)**.5)
     C **IFSQUARE) / DENOM
         END IF

        IF ((GMEAN(G) .GT. 0.00000) .AND. (QUANTYPE .EQ. 2)) THEN
        TOBFUN=TOBFUN+(((OLDCON(G)-GMEAN(G))/GMEAN(G)**.5)**IFSQUARE)
     C / DENOM
        END IF

        IF ((QUANTYPE .EQ. 3) .AND. (OLDCON(G) .GT. 0.00000) .AND.
     C   (GMEAN(G) .GT. 0.00000)) THEN

          LGMEAN(3)=GMEAN(G)
          LGMEAN(4)=SRI(G)-GMEAN(G)
          LGMEAN(2)=MAXCH*DEPARTN(G)*(DEPARTN(G)-1.0)-GMEAN(G)
          LGMEAN(1)=MAXCH*DEPARTN(G)*(NUMTEACH-DEPARTN(G)) - LGMEAN(2)

          LTOLDCON(3)=OLDCON(G)
          LTOLDCON(4)=SRI(G)-OLDCON(G)
          LTOLDCON(2)=MAXCH*DEPARTN(G)*(DEPARTN(G)-1.0)-OLDCON(G)
          LTOLDCON(1)=MAXCH*DEPARTN(G)*(NUMTEACH-DEPARTN(G)) 
     C    - LTOLDCON(2)

           DO 8282 TI=1,4
           IF ((TI .EQ. 1) .OR. (TI .EQ. 3)) THEN
           LADD=1
           ELSE
           LADD=-1
           END IF
           IF (LGMEAN(TI) .LT. .0001) THEN
           LGMEAN(TI)=.0001
           END IF
           IF (LTOLDCON(TI) .LT. .0001) THEN
           LTOLDCON(TI)=.0001
           END IF

            TOBFUN=TOBFUN+LADD*2*LTOLDCON(TI)*
     C      LOG(LTOLDCON(TI)/LGMEAN(TI))

08282       CONTINUE
         IF (BLT .EQ. 1) THEN
          PENNUM=NUMTEACH*(NUMTEACH-1)
          TOBFUN=TOBFUN-MAXDEPT*LOG(PENNUM)
         END IF
        END IF

        IF (QUANTYPE .EQ. 4) THEN
         IF (DEPARTN(G) .GT. 1) THEN
         TOBFUN=TOBFUN+OLDCON(G)/(DEPARTN(G)*(DEPARTN(G)-1.0000))
         END IF
        END IF
        END IF
00008    CONTINUE
       
       IF (NETLEV .EQ. 1) THEN
       IF (QUANTYPE .EQ. 1) THEN
       TOBFUN=(DTOTK-DTOTE)/DTOTV**.5
       END IF
       IF (QUANTYPE .EQ. 5) THEN
       IF (COLWT .LT. .0001) THEN
       TOBFUN=DTOTK/SAMEG1
       ELSE
       TOBFUN=(DTOTK*(K1-(SAMEG1-DTOTK)))/
     C ((SAMEG1-DTOTK)*(K2-DTOTK))
       END IF
       END IF
        END IF
        IF (NETLEV .EQ. 2) THEN
        TOBFUN=0
        DO 987 I=1,NUMTEACH
         CHANGE=0.0000
        IF ((QUANTYPE .EQ. 1) .AND. (CURVAR(I) .GT. 0)) THEN
        CHANGE=(IDELTKA(I)-CURMEAN(I))/SQRT(CURVAR(I))
        END IF

        IF ((QUANTYPE .EQ. 2) .AND. (CURMEAN(I) .GT. 0)) THEN
        CHANGE=(IDELTKA(I)-CURMEAN(I))/SQRT(CURMEAN(I))
        END IF
        IF (QUANTYPE .EQ. 3) THEN
        KSIGN=IDELTKA(I)/CURMEAN(I)
        IF (KSIGN .GT. 0) THEN
        CHANGE=2*IDELTKA(I)*LOG(KSIGN)
        END IF
        END IF
        TOBFUN=TOBFUN+CHANGE
00987    CONTINUE
         END IF
   
        RETURN
        END

C IN GETRS NOTE THS SIDEWT

      SUBROUTINE MATPRINT(EXTRAID,DIVIDO,MAXDEPT,
     CNUMTEACH,DOIT,AFFIL,MATRIX,TITLE,NSUBID,IEXPECT,ISTD,ICON,
     CNEWORD,STRUCTEQ,NOWCLOSE,NUMCOL,HOWWIDE,CAFFIL,DEPARTN,WEXTRA)
C                                   PRINTO,NSUBID

      INTEGER NUMTEACH, DOIT,AFFIL(251),MATRIX(251,251),P,K,I,
     C DIVIDO,MAXDEPT,OLDAFFIL,OAFFIL2,EXTRAID(251),ROWMARG,
     C COLMARG(251),THISVAL,PI,TOTMARG,TVAL,NSUBID(251),NEWORD(251),
     C STRUCTEQ,NUMCOL,HOWWIDE,HW,CAFFIL(251),DEPARTN(251),
     C DIGIT(3), DECI,ZZ,EXTRA,WEXTRA,GTP
   
       REAL IEXPECT(251),ISTD(251),ICON(251),NOWCLOSE(251)
       CHARACTER TITLE*40
       CHARACTER LABELS(100)*2

      LABELS(1)="A"
      LABELS(2)="B"
      LABELS(3)="C"
      LABELS(4)="D"
      LABELS(5)="E"
      LABELS(6)="F"
      LABELS(7)="G"
      LABELS(8)="H"
      LABELS(9)="I"
      LABELS(10)="J"
      LABELS(11)="K"
      LABELS(12)="L"
      LABELS(13)="M"
      LABELS(14)="N"
      LABELS(15)="O"
      LABELS(16)="P"
      LABELS(17)="Q"
      LABELS(18)="R"
      LABELS(19)="S"
      LABELS(20)="T"
      LABELS(21)="U"
      LABELS(22)="V"
      LABELS(23)="W"
      LABELS(24)="X"
      LABELS(25)="Y"
      LABELS(26)="Z"
      LABELS(27)="a"
      LABELS(28)="b"
      LABELS(29)="c"
      LABELS(30)="d"
      LABELS(31)="e"
      LABELS(32)="f"
      LABELS(33)="g"
      LABELS(34)="h"
      LABELS(35)="i"
      LABELS(36)="j"
      LABELS(37)="k"
      LABELS(38)="l"
      LABELS(39)="m"
      LABELS(40)="n"
      LABELS(41)="o"
      LABELS(42)="p"
      LABELS(43)="q"
      LABELS(44)="r"
      LABELS(45)="s"
      LABELS(46)="t"
      LABELS(47)="u"
      LABELS(48)="v"
      LABELS(49)="w"
      LABELS(50)="x"
      LABELS(51)="y"
      LABELS(52)="z"
      LABELS(53)="AA"
      LABELS(54)="BB"
      LABELS(55)="CC"
      LABELS(56)="DD"
      LABELS(57)="EE"
      LABELS(58)="FF"
      LABELS(59)="GG"
      LABELS(60)="HH"


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
      WRITE(33,1111) "Each Cell Represents The Number of"
      WRITE(33,1111) " Transactions (Also Known as Connections or"
      WRITE(33,101)
      WRITE(33,1111) " Exchanges) "
      WRITE(33,1111) "Between Actor i (Row) and Actor j (Column)"
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
      WRITE(33,1111) "Each Cell Represents The Number of"
      WRITE(33,1111) " Transactions (Also Known as Connections or"
      WRITE(33,101)
      WRITE(33,1111) " Exchanges) "
      WRITE(33,1111) "Between Actor i (Row) and Actor j (Column)"
      WRITE(33,1111)
      WRITE(33,101)
      WRITE(33,7513) "N"," Group And Actor Id"
        DO 4420 TP=1,(HOWWIDE*NUMCOL+MAXDEPT-20)
       WRITE (3,108) " "
04420   CONTINUE
        IF (WEXTRA .EQ. 1) THEN  
        WRITE(33,3512) "Actor Marginals","           ",
     C "Relation To Group"
         ELSE
         WRITE(33,101)
         END IF
       DO 987 DECI=1,4
      IF (DECI .EQ. 2) THEN
      WRITE(33,1067) "   ","   "
      END IF
      
      IF (DECI .EQ. 1) THEN
      WRITE(33,1065) NUMTEACH,"       "
      END IF
      IF (DECI .EQ. 3) THEN
      WRITE(33,1067) "   ","   "
      END IF
      IF (DECI .EQ. 4) THEN
      WRITE(33,1067) "Group","ID"
      END IF
      WRITE(33,108) "|"

      OLDAFFIL=CAFFIL(1)
      DO 17 P = 1,NUMCOL
       IF (CAFFIL(P) .NE. OLDAFFIL) THEN
       WRITE (3,108) "|"
       END IF
C      NSUBID(EXTRAID(I))
c       Here
       IF (DECI .GT. 1) THEN
       DIGIT(1)=INT(NSUBID(EXTRAID(P))/100.)
       DIGIT(2)=INT((NSUBID(EXTRAID(P))-DIGIT(1)*100.)/10.)
       DIGIT(3)=INT(NSUBID(EXTRAID(P))-(DIGIT(1)*100. + DIGIT(2)*10.))

       TVAL=DIGIT(DECI-1.)

       
*       IF (CAFFIL(P) .LE. 251) THEN
*       TVAL=MOD(CAFFIL(P),10)
*       ELSE
*       TVAL=9999
*       END IF
       IF (NSUBID(EXTRAID(P)) .GE. (10.**(4.-DECI))) THEN
       WRITE (3,104) TVAL 
       ELSE
       WRITE(33,1087) " "
       END IF
        ELSE
C        DECI GT 1
       DO 77 ZZ=1,(HOWWIDE-1)
       WRITE(33,108) " "
00077   CONTINUE
        WRITE(33,108) LABELS(CAFFIL(P))
        END IF
        OLDAFFIL=CAFFIL(P)
00017   CONTINUE
      WRITE(33,108) "|"
         EXTRA=12
        IF (WEXTRA .EQ. 1) THEN
        EXTRA=69
        IF (DECI .EQ. 3) THEN
        WRITE(33,108) " "
        WRITE(33,111) "GRP","ROW","COL","TOT",
     C  "EXP","STD","CON","Z","Z/N","DELTA" 
        ELSE
        WRITE(33,513)
        END IF
           ELSE
        WRITE(33,107)
        END IF
00987    CONTINUE
    
C        HOWWIDE*NUMCOL+MAXDEPT+EXTRA)
        DO 420 TP=1,12
       WRITE (3,5108) "-"
00420   CONTINUE
       WRITE(33,5108) "+"
       DO 6420 TP=1,MAXDEPT
       IF (DEPARTN(TP) .GT. 0) THEN
       DO 7420 GTP=1,(DEPARTN(TP)*HOWWIDE)
       WRITE(33,5108) "-"
07420   CONTINUE
       WRITE(33,5108) "+"
         END IF
06420   CONTINUE
       DO 8420 TP=1,(EXTRA-12)
       WRITE(33,5108) "-"
08420   CONTINUE
     
        WRITE(33,513)

       OAFFIL2=AFFIL(1)

      DO 19 PI=1,NUMTEACH
       IF (AFFIL(PI) .NE. OAFFIL2) THEN
        DO 10420 TP=1,12
       WRITE (3,5108) "-"
10420   CONTINUE
       WRITE(33,5108) "+"
       DO 16420 TP=1,MAXDEPT
       IF (DEPARTN(TP) .GT. 0) THEN
       DO 17420 GTP=1,(DEPARTN(TP)*HOWWIDE)
       WRITE(33,5108) "-"
17420   CONTINUE
       WRITE(33,5108) "+"
        END IF
16420   CONTINUE
       DO 18420 TP=1,(EXTRA-12)
       WRITE(33,5108) "-"
18420   CONTINUE
     
        WRITE(33,513)

C        DO 20 TP=1,(HOWWIDE*NUMCOL+MAXDEPT+EXTRA)
C       WRITE (3,108) "_"
C00020   CONTINUE
C       WRITE(33,101)

       END IF
       WRITE(33,106) AFFIL(PI),LABELS(AFFIL(PI)),NSUBID(EXTRAID(PI))
       WRITE(33,108) "|"
       OAFFIL2=AFFIL(PI)      
        OLDAFFIL=CAFFIL(1)
       ROWMARG=0
       COLMARG(PI)=0
      DO 18 P = 1,NUMCOL
       IF (CAFFIL(P) .NE. OLDAFFIL) THEN
       WRITE (3,108) "|"
       END IF
       IF ((PI .EQ. P) .AND. (STRUCTEQ .NE. 1)) THEN 
       WRITE (3,1087) LABELS(AFFIL(PI))
       THISVAL=-9999
       ELSE
       THISVAL= INT(MATRIX(PI,P))
       ROWMARG=ROWMARG+MATRIX(PI,P)
       COLMARG(PI)=COLMARG(PI)+MATRIX(P,PI)
       END IF
       IF (THISVAL .GT. 0) THEN
       WRITE (3,104) THISVAL
       END IF
       IF (THISVAL .EQ. 0) THEN
       WRITE (3,1087) "."
       END IF
        OLDAFFIL=CAFFIL(P)
00018   CONTINUE

       TOTMARG=ROWMARG+COLMARG(PI)
       WRITE(33,108) "|"
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
  513  FORMAT(A3)
  512 FORMAT(3(A20,1X))
 3512 FORMAT(3(A))
  101 FORMAT(I3,251(I1))
  109 FORMAT(I4,1X,$)
  102 FORMAT(2I3$)
01067 FORMAT(A6,1X,A5,$)
  106 FORMAT(I3,1X,A2,1X,I5,$)
01066  FORMAT(A7,$)
01065 FORMAT(I3,A9,$)
  104 FORMAT(I1$)
  108 FORMAT(A1$)
05108 FORMAT(A1$)
01087 FORMAT(A1$)
  110 FORMAT(A6$)
  103 FORMAT(2I3)
01111 FORMAT(A$)
  107 FORMAT(1X,5(I3,1X),3X,6(F5.1,1X))
  111 FORMAT(4(A3,1X),7X,7(A5,1X))
      END

      SUBROUTINE GROUPASC (ZCOVMAT,LMAXDEPT,LGROUP,DEPARTN,
     C LOGIT,ZIT,INUM,NUMTEACH,INFILE,TACKON,PRINTGRP,LABEL,
     C ADDEND,ONLYPOS,NSIM,FINAL,MAXDEPT,PRINTK,LAMNDA)

      INTEGER LMAXDEPT,LGROUP(251),DEPARTN(251),G1,G2,LOGIT,ZIT,
     C AG1,AG2,AG3,INUM,NUMTEACH,PRINTGRP,G3,ADDEND,ADDW,DIVW,ONLYPOS,
     C NSIM,FINAL,MAXDEPT,PRINTK
      REAL ZCOVMAT(50,50),
     C TOTRELC(50,50),BETWEENC(50,50),
     C GDEGREE(50,1),GCLOSE(50,1),GBETW(50,1),
     C ICOMP(50),LAMNDA(50)

       CHARACTER*16 INFILE,OUTFILE,OUTFILE2
       CHARACTER LABEL(251)*9,TACKON*4


       IF (PRINTGRP .EQ. 1) THEN
       OUTFILE2="central." // TACKON
       OPEN(56,file=OUTFILE2)
       IF (ADDEND .EQ. 1) THEN
       CALL MOVEEND(56)
       END IF
       END IF
       DO 88 I=1,LMAXDEPT
       DO 86 J=1,LMAXDEPT
       BETWEENC(I,J)=ZCOVMAT(LGROUP(I),LGROUP(J))
00086   CONTINUE
00088    CONTINUE

       CALL PRINTCOV(BETWEENC,LGROUP,LMAXDEPT,DEPARTN,
     C "               DIRECT ASSOCIATIONS                   ",
     C INUM,NUMTEACH,1,1,GDEGREE,LABEL,0,"DIRECT ",INFILE,PRINTGRP,
     C 0)
       IF (ONLYPOS .EQ. 1) THEN
       ADDW=0
       DIVW=1
       ELSE
       ADDW=1
       DIVW=2
       END IF
      DO 321 G1=1,LMAXDEPT
       ICOMP(G1)=ZCOVMAT(LGROUP(G1),LGROUP(G1))
       DO 4321 G2=1,LMAXDEPT
         TOTRELC(G1,G2)=0
         BETWEENC(G1,G2)=0
         IF (LOGIT .EQ. 1) THEN
         ZCOVMAT(LGROUP(G1),LGROUP(G2))=
     C   LOG(ZCOVMAT(LGROUP(G1),LGROUP(G2))+1.)
         END IF
         IF (ZIT .EQ. 1) THEN
         LG1=LGROUP(G1)
         LG2=LGROUP(G2)
        IF (ZCOVMAT(LG1,LG2) .GE. 0) THEN
         KSIGN=1
        ELSE 
         KSIGN=-1
        END IF
         NODO=0
        IF (ZCOVMAT(LG1,LG2) .GT. 10) THEN 
         ZCOVMAT(LG1,LG2) = 1
         NODO=1
        END IF
       IF (ZCOVMAT(LG1,LG2) .LT. -10) THEN 
         ZCOVMAT(LG1,LG2) = -1
         NODO=1
        END IF
        IF (NODO .EQ. 0) THEN
        ZABS=EXP(ABS(ZCOVMAT(LG1,LG2)*2))
        ZCOVMAT(LG1,LG2)=(ADDW+KSIGN*(ZABS-1)/(ZABS+1))/DIVW
        END IF

        END IF
04321    CONTINUE
  321 CONTINUE

C    TOTAL RELATION

       DO 88771 AG1=2,LMAXDEPT
        G1=LGROUP(AG1)
       DO 88772 AG2=1,(AG1-1)
        G2=LGROUP(AG2)
       TOTRELC(AG1,AG2)=TOTRELC(AG1,AG2)+ZCOVMAT(G1,G2)
       DO 88773 AG3=1,LMAXDEPT
        G3=LGROUP(AG3)
       IF ((G3 .NE. G1) .AND. (G3 .NE. G2)) THEN
       TOTRELC(AG1,AG2)=TOTRELC(AG1,AG2)
     C  +ZCOVMAT(G1,G3)*ZCOVMAT(G2,G3)
       END IF
88773   CONTINUE
       TOTRELC(AG2,AG1)=TOTRELC(AG1,AG2)
88772   CONTINUE
88771   CONTINUE

C        BETWEENNESS

       DO 89771 AG1=2,LMAXDEPT
        G1=LGROUP(AG1)
       DO 89772 AG2=1,(AG1-1)
        G2=LGROUP(AG2)
       DO 89773 AG3=1,LMAXDEPT
        G3=LGROUP(AG3)
       IF ((G3 .NE. G1) .AND. (G3 .NE. G2)) THEN
       IF (ABS(TOTRELC(AG1,AG2)) .GT. .00001) THEN
       BETWEENC(AG3,AG2)=BETWEENC(AG3,AG2)+
     C  ZCOVMAT(G1,G3)*ZCOVMAT(G2,G3)/TOTRELC(AG1,AG2)
       BETWEENC(AG3,AG1)=BETWEENC(AG3,AG1)+
     C  ZCOVMAT(G1,G3)*ZCOVMAT(G2,G3)/TOTRELC(AG1,AG2)
       END IF
       END IF
89773   CONTINUE
89772   CONTINUE
89771   CONTINUE

       CALL PRINTCOV(TOTRELC,LGROUP,LMAXDEPT,DEPARTN,
     C "       TOTAL RELATIONS (FREEMAN'S CLOSENESS)         ",
     C  INUM,NUMTEACH,0,1,GCLOSE,LABEL,1,"TOTAL  ",INFILE,PRINTGRP,
     C  1)

       CALL PRINTCOV(BETWEENC,LGROUP,LMAXDEPT,DEPARTN,
     C "BETWEENNESS / TOTAL RELATIONS (FREEMAN'S BETWEENNESS)",
     C  INUM,NUMTEACH,0,1,GBETW,LABEL,0,"BETWEEN",INFILE,PRINTGRP,
     C  0)

        IF (PRINTGRP .EQ. 1) THEN
        OUTFILE="compgrp." // TACKON
        OPEN(28,file=OUTFILE)
       IF (ADDEND .EQ. 1) THEN
       CALL MOVEEND(28)
       END IF
        DO 3174 AG1=1,LMAXDEPT
        G1=LGROUP(AG1)
        WRITE(28,2195) INFILE,NSIM,FINAL,G1,DEPARTN(G1),
     C  ICOMP(AG1),GDEGREE(AG1,1),GCLOSE(AG1,1),GBETW(AG1,1),
     C  NSIM,PRINTK,LAMNDA(G1)
03174    CONTINUE
       CLOSE(28)
       CLOSE(56)
       END IF

       RETURN
02195 FORMAT(A10,4I10,4F10.3,2I10,F10.5)
       END

       SUBROUTINE PRINTCOV (COVMAT,LGROUP,LMAXDEPT,DEPARTN,GLABEL,
     C  INUM,NUMTEACH,PRINTD,PRINTOFF,AVECENT,LABEL,NODIAG,TYPE,
     C  INFILE,PRINTGRP,SYMMAT)
       REAL COVMAT(50,50),ONDIAG,AVECENT(50,1),
     C OUTCENT(50,1)
       INTEGER LGROUP(251),LMAXDEPT,DEPARTN(251),INUM,NUMTEACH,
     C DP,ACTG,G1,U,G2,PRINTD,PRINTOFF,NODIAG,PRINTGRP,SYMMAT
       REAL STATS(8,1),WK(6),XMISS(1),WT(50)
       INTEGER NBR(5),IX,IS,IER,CDNOM,AG1,AG2,ELM1,ELM2
       DOUBLE PRECISION MEAN,VAR,SKEW,MEAN2

       CHARACTER GLABEL*53
       CHARACTER LABEL(251)*9,TACKON*4,INFILE*16,OUTFILE*16,TYPE*7

      

       WRITE(33,3388)
       WRITE(33,3388) GLABEL
       WRITE(33,10) "GROUP", (LGROUP(DP) , DP=1,LMAXDEPT)
       WRITE(33,8) "LABEL",(LABEL(LGROUP(DP)) , DP=1,LMAXDEPT)
       WRITE(33,2302) "N" , (DEPARTN(LGROUP(DP)) , DP=1,LMAXDEPT)
       WRITE(33,10) "GROUP"

       ACTG=0
       ONDIAG=0
       CDNOM=0
       DO 76 G1=1,LMAXDEPT
       AVECENT(G1,1)=0
       OUTCENT(G1,1)=0
       IF (DEPARTN(G1) .GT. 0) THEN
       CDNOM=CDNOM+1
       END IF
00076   CONTINUE
       DO 577 AG1=1,LMAXDEPT
        G1=LGROUP(AG1)
        IF (DEPARTN(G1) .GT. 1) THEN
        ONDIAG=ONDIAG+COVMAT(AG1,AG1)
        ACTG=ACTG+1
        END IF
        IF (SYMMAT .EQ. 1) THEN

       IF (NODIAG .EQ. 1) THEN
        LASTONE=AG1-1.
       ELSE
        LASTONE=AG1
       END IF
         ELSE
        LASTONE=LMAXDEPT
         END IF
       IF ((SYMMAT .EQ. 0) .OR. (AG1 .GT. 1)) THEN
       WRITE(33,3301) G1, (COVMAT(AG1,U) ,
     C  U=1,LASTONE)
C       DO 8282 U=1,LASTONE
C       WRITE(225,6262) G1, LGROUP(U),COVMAT(AG1,U)
C08282   CONTINUE
       END IF
       IF (DEPARTN(G1) .GT. 0) THEN
       DO 578 AG2=1,LMAXDEPT
         IF ((AG1 .NE. AG2) .AND. (DEPARTN(LGROUP(AG2)) .GT. 0)) THEN
C         IF (AG2 .GT. AG1) THEN
C         ELM1=AG2
C         ELM2=AG1
C         ELSE
C         ELM1=AG1
C         ELM2=AG2
C         END IF

       AVECENT(AG1,1)=AVECENT(AG1,1)+ 
     C COVMAT(AG1,AG2)/(CDNOM-1.)

       OUTCENT(AG1,1)=OUTCENT(AG1,1)+ 
     C COVMAT(AG2,AG1)/(CDNOM-1.)
       END IF
00578   CONTINUE
        END IF
00577   CONTINUE
       CLOSE(225)
      IF (PRINTD .EQ. 1) THEN
      WRITE(33,3308)
      WRITE(33,3308) "            DIAGONAL ELEMENTS     "
      WRITE(33,4301) "SUM",ONDIAG
      WRITE(33,194) "/ # GROUPS","/ACTOR","/NON ISOLATE"
      WRITE(33,195) (ONDIAG/ACTG),(ONDIAG/NUMTEACH),ONDIAG/INUM
      WRITE(33,3308)
      WRITE(33,3308) "DIAGONAL PER ACTOR"
      WRITE(33,3302) (COVMAT(U,U)/
     C DEPARTN(U), U=1,LMAXDEPT)
      WRITE(33,3308)      
      END IF
       MEAN=ONDIAG/ACTG
       VAR=0
       SKEW=0
       MEAN2=0
       OMEAN2=0
       DO 878 AG1=1,LMAXDEPT
       G=LGROUP(AG1)
       IF (DEPARTN(G) .GT. 0) THEN
       OMEAN2=OUTCENT(AG1,1)/CDNOM+OMEAN2
       MEAN2=AVECENT(AG1,1)/CDNOM+MEAN2
       END IF
       IF (DEPARTN(G) .GT. 1) THEN
       VAR=VAR+(COVMAT(AG1,AG1)-MEAN)**2/(ACTG-1.)
       SKEW=SKEW+((COVMAT(AG1,AG1)-MEAN)**3)
       END IF
00878   CONTINUE
       SKEW=SKEW*ACTG/((ACTG-1.)*(ACTG-2.)*VAR**1.5)
      IF (PRINTD .EQ. 1) THEN
      WRITE(33,3308) "   UNIVARIATE STATISTICS ON DIAGONALS"
      WRITE(33,194) "MEAN","VAR","STD","SKEW"
      WRITE(33,195) MEAN,VAR,VAR**.5,SKEW
      WRITE(33,3308)
      IF (PRINTGRP .EQ. 1) THEN
      WRITE(506,201) INFILE,TYPE,"ON-DIAGONAL",ACTG,MEAN,VAR,SKEW
      END IF
      END IF
  
      IF (PRINTOFF .EQ. 1) THEN
      WRITE(33,3308)
      WRITE(33,3308) "            OFF-DIAGONAL ELEMENTS     "      


      WRITE(33,3308) "MEAN, IN CENTRALITY (FREEMAN'S IN-DEGREE)"
      WRITE(33,3308) "TAKEN AT SUBGROUP LEVEL "
      WRITE(33,3302) (OUTCENT(U,1), U=1,LMAXDEPT)

      WRITE(33,3308)
      WRITE(33,3308) "MEAN PER ACTOR"
      WRITE(33,3302) (OUTCENT(U,1)/DEPARTN(LGROUP(U)), 
     C                                  U=1,LMAXDEPT)
      WRITE(33,3308)      
       IF (SYMMAT .EQ. 0) THEN
      WRITE(33,3308) "MEAN, OUT CENTRALITY (FREEMAN'S OUT-DEGREE)"
       WRITE(33,3308) "TAKEN AT SUBGROUP LEVEL "
      WRITE(33,3302) (AVECENT(U,1), U=1,LMAXDEPT)

      WRITE(33,3308)
      WRITE(33,3308) "MEAN PER ACTOR"
      WRITE(33,3302) (AVECENT(U,1)/DEPARTN(LGROUP(U)), 
     C                                  U=1,LMAXDEPT)

      WRITE(33,3308)      
      WRITE(33,3308) "RATIO IN/OUT CENTRALITY"
      WRITE(33,3302) (OUTCENT(U,1)/AVECENT(U,1), U=1,LMAXDEPT)

      WRITE(33,3308)      
      WRITE(33,3308) "RATIO IN/OUT CENTRALITY PER ACTOR"
      WRITE(33,3302) (OUTCENT(U,1)/(DEPARTN(LGROUP(U))*
     C AVECENT(U,1)), U=1,LMAXDEPT)
         END IF

       VAR=0
       SKEW=0
       DO 879 G=1,LMAXDEPT
       IF (DEPARTN(LGROUP(G)) .GT. 1) THEN
       VAR=VAR+(OUTCENT(G,1)-MEAN2)**2/(CDNOM-1.)
       SKEW=SKEW+((OUTCENT(G,1)-MEAN2)**3)
       END IF
00879   CONTINUE
       SKEW=SKEW*CDNOM/((CDNOM-1.)*(CDNOM-2.)*VAR**1.5)
      WRITE(33,3308)
      WRITE(33,3308) "   UNIVARIATE STATISTICS OFF-DIAGONALS"
      WRITE(33,3308) "IN-CENTRALITY"
      WRITE(33,194) "MEAN","VAR","STD","SKEW"
      WRITE(33,195) MEAN2,VAR,VAR**.5,SKEW
      WRITE(33,3308)
      IF (PRINTGRP .EQ. 1) THEN
      WRITE(506,201) INFILE,TYPE,"OFF-DIAGONAL-IN",ACTG,MEAN2,
     C VAR,SKEW
      END IF
       IF (SYMMAT .EQ. 0) THEN
       VAR=0
       SKEW=0
       DO 5879 G=1,LMAXDEPT
       IF (DEPARTN(LGROUP(G)) .GT. 1) THEN
       VAR=VAR+(AVECENT(G,1)-OMEAN2)**2/(CDNOM-1.)
       SKEW=SKEW+((AVECENT(G,1)-OMEAN2)**3)
       END IF
05879   CONTINUE
       SKEW=SKEW*CDNOM/((CDNOM-1.)*(CDNOM-2.)*VAR**1.5)
      WRITE(33,3308) "   UNIVARIATE STATISTICS OFF-DIAGONALS"
      WRITE(33,3308) "OUT-CENTRALITY"
      WRITE(33,194) "MEAN","VAR","STD","SKEW"
      WRITE(33,195) OMEAN2,VAR,VAR**.5,SKEW
      WRITE(33,3308)
      IF (PRINTGRP .EQ. 1) THEN
      WRITE(506,201) INFILE,TYPE,"OFF-DIAGONAL-OUT",ACTG,OMEAN2,
     C VAR,SKEW
      END IF
       END IF
      END IF

      RETURN
00201 FORMAT(3(A16,1X),25(F10.5,1X))
03302 FORMAT(7X,251(F7.2$))
03301 FORMAT(I6,1X,10(25F7.2/))
    8 FORMAT(10(25(A6,1X),/))
   10 FORMAT(A7,10(25(I6,1X),/))
 2302 FORMAT(A4,3X,10(25(I6,1X),/))
  194 FORMAT(10(2X,A15))
  195 FORMAT(10(2X,F15.5))  
00199 FORMAT(F10.6)
03308  FORMAT(A)
03388  FORMAT(10X,A,10X)
04301  FORMAT(A,F10.5)
04307  FORMAT(A,F10.5$)
06262  FORMAT(2I10,F10.5)
       END 

       SUBROUTINE MOVEEND (THISFILE)
       CHARACTER FNAME*16
       INTEGER G,THISFILE,LENGTH
       DO 1991 G=1,1000000
        READ(THISFILE,4301,END=1094) FNAME
01991    CONTINUE
01094  LENGTH=G
        RETURN
04301   FORMAT(20(A))
        END

      SUBROUTINE PERXPER(COVMAT,NUMTEACH,NEWDEPT,EXT,NSUBID,LGROUP,
     C LMAXDEPT,MAXDEPT)
      REAL COVMAT(50,50)
      INTEGER NUMTEACH,NEWDEPT(251),I,J,NSUBID(251),LGROUP(251),
     C LMAXDEPT,MAXDEPT
      CHARACTER EXT*4,OUTFILE*12

      OUTFILE="grpxgrp." // EXT
      OPEN(8,file=OUTFILE)
      DO 4 I=1,LMAXDEPT
      DO 5 J=1,I
      WRITE(8,100) LGROUP(I),LGROUP(J),COVMAT(LGROUP(I),LGROUP(J))
00005  CONTINUE
00004   CONTINUE
      CLOSE(8)

      OUTFILE="perxper." // EXT
      OPEN(8,file=OUTFILE)

      DO 2 I=2,NUMTEACH
      DO 3 J=1,(I-1.)
      WRITE(8,100) NSUBID(I),NSUBID(J),COVMAT(NEWDEPT(I),NEWDEPT(J))
00003  CONTINUE
00002   CONTINUE
      CLOSE(8)
       RETURN
00100   FORMAT(2(I5,1X),F10.5)
       END

      SUBROUTINE MDSCMD(EXTNOTE,EXT,MID,TITLES,LMAXDEPT,INFILE,
     C LGROUP,COVMAT)
      CHARACTER EXTNOTE*11,EXT*5,TITLES(3)*20,INFILE*16,MID*1,
     C OUTFILE*12,DATFILE*11
      INTEGER LMAXDEPT,G1,LGROUP(251),U
      REAL COVMAT(50,50)
      OUTFILE="MDS"//MID//"READ.CMD"
      OPEN(59,FILE=OUTFILE)
      DATFILE="group."//EXT
      OPEN(53,FILE=DATFILE)
   
      WRITE(59,4301) "NOTE '",EXTNOTE," ",
     C TITLES(1),TITLES(2),TITLES(3),
     C " ",INFILE,
     C "'"
      WRITE(59,4301) "SAVE MDSD" 
      WRITE(59,4301) "GET '",DATFILE,"'"
      WRITE(59,4301) "LRECL=2000"
      WRITE(59,4301) "DIAGONAL=ABSENT"
      WRITE(59,4302) "input (V001,"
      DO 3235 G1=2,LMAXDEPT
      WRITE(59,4303) G1
       WRITE(53,103) (COVMAT(LGROUP(G1),LGROUP(U)) ,
     C  U=1,G1-1)
03235  CONTINUE
      CLOSE(53)
C      WRITE(59,4301) "),"
      WRITE(59,4304) LMAXDEPT
      WRITE(59,4301) "TYPE=SIMILARITY"
      WRITE(59,4301) "RUN"
      CLOSE(59)
      RETURN
04301 FORMAT(20(A))
04302 FORMAT(20(A$))
04303 FORMAT("V",I3.3,",")
04304 FORMAT(") (",I3,"*#7)")
  103 FORMAT(251(F7.3))
      END 

      SUBROUTINE SORTMAT(INMAT,MAXG,CURPLACE,COMPARE,CENT,TSIZE)
      INTEGER MAXG,ORDER(50),CHANGE,COMPARE,I,J,CURPLACE(MAXG),H,
     C INORD,MOVEG,TOG,KTOG,THISG,TOPVAL,TSIZE,USESORT,
     C SORTFLAG,IER
      REAL INMAT(50,50),COMPMAT(50,50),HIVAL,
     C NEWCONT,DENOM,IMPVAL,CURVAL,CENT(251),MISSVAL

      HIVAL=-999999
      TOPVAL=999
      MISSVAL=20

      
      DO 2 I=2,MAXG
      DO 3 J=1,(I-1)
      IF (J .NE. I) THEN

      IF (ABS(COMPARE) .EQ. 1) THEN
      COMPMAT(I,J)=INMAT(I,J)-INMAT(J,I)
      END IF

      IF (ABS(COMPARE) .EQ. 2) THEN
      DENOM=INMAT(J,I)
      IF (ABS(INMAT(J,I)) .LT. .0001) THEN
      DENOM=.01
      END IF
      COMPMAT(I,J)=LOG(INMAT(I,J)/DENOM)
       IF (ABS(INMAT(I,J)) .LE. .0000001) THEN
       IF (INMAT(J,I) .LT. 0) THEN
       COMPMAT(I,J)=MISSVAL
       ELSE
C    GGG
       COMPMAT(I,J)=-MISSVAL
       END IF
        END IF
      END IF

C      IF (ABS(COMPMAT(I,J)) .GT. HIVAL) THEN
C      HIVAL=ABS(COMPMAT(I,J))
C      IF (COMPMAT(I,J) .GT. 0) THEN
C      ORDER(1)=I
C      ORDER(2)=J
C      ELSE
C      ORDER(1)=J
C      ORDER(2)=I
C     END IF
C     END IF

       END IF
      IF (COMPARE .GT. 0) THEN
      COMPMAT(I,J)=-COMPMAT(I,J)
      END IF
      COMPMAT(J,I)=-COMPMAT(I,J)

      
00003  CONTINUE
       COMPMAT(I,I)=0
        CURPLACE(I)=TOPVAL
00002   CONTINUE
       COMPMAT(1,1)=0
C       CURPLACE(ORDER(1))=1
C       CURPLACE(ORDER(2))=2

      DO 22 I=1,MAXG
      CENT(I)=0
      ORDER(I)=I
      DO 33 J=1,MAXG
      CENT(I)=CENT(I)-COMPMAT(I,J)
00033  CONTINUE
00022   CONTINUE
         USESORT=0
       IF (MAXG .GT. 1) THEN
       SORTFLAG=2
       CALL SPSORT(CENT,MAXG,ORDER,SORTFLAG,IER)
        END IF
       DO 23 I=1,MAXG
       CURPLACE(ORDER(I))=I
00023   CONTINUE


       INORD=MAXG-1

C       DO 77 I=1,MAXG
C       IF (CURPLACE(I) .EQ. TOPVAL) THEN
C       INORD=INORD+1
C       CURPLACE(I)=INORD
C       ORDER(INORD)=I
C      END IF
C00077  CONTINUE

      CHANGE=1
      DO WHILE (CHANGE .EQ. 1)
      CHANGE=0
      HIVAL=0
      DO 4 I=1,MAXG
      CURVAL=0
      IF (CURPLACE(I) .LT. MAXG) THEN
      DO 322 H=(CURPLACE(I)+1),(INORD)
      CURVAL=CURVAL+COMPMAT(I,ORDER(H))
00322  CONTINUE
      END IF
      DO 5 J=1,(INORD+1)
      IF ((J .NE. (CURPLACE(I) +1)) .AND. 
     C (J .NE. CURPLACE(I))) THEN
      NEWCONT=0      
      DO 6 H=J,(INORD)
      NEWCONT=NEWCONT+COMPMAT(I,ORDER(H))
00006 CONTINUE
      IMPVAL=NEWCONT-CURVAL
      IF (IMPVAL .GT. HIVAL) THEN
      HIVAL=IMPVAL
      MOVEG=I
      KTOG=J
      CHANGE=1
      END IF
      END IF
00005  CONTINUE
00004   CONTINUE
      IF (CHANGE .EQ. 1) THEN
       DO 7 I=1,INORD
        THISG=ORDER(I)
        IF (THISG .NE. MOVEG) THEN
         IF ((I .LT. CURPLACE(MOVEG)) 
     C  .AND. (I .GE. KTOG)) THEN
         CURPLACE(THISG)=CURPLACE(THISG)+1
         END IF

         IF ((I .GT. CURPLACE(MOVEG))
     C   .AND. (I .LT. KTOG)) THEN
         CURPLACE(THISG)=CURPLACE(THISG) -1
         END IF
        END IF
00007   CONTINUE
        IF (KTOG .GT. CURPLACE(MOVEG)) THEN
        KTOG=KTOG-1
        END IF
        CURPLACE(MOVEG)=KTOG

       INORD=0
       DO 8 I=1,MAXG
       IF ((CURPLACE(I) .GT. 0) .AND. 
     C (CURPLACE(I) .LE. MAXG)) THEN
       ORDER(CURPLACE(I))=I
       IF (CURPLACE(I) .LT. MAXG) THEN
       INORD=INORD+1
       END IF
       END IF
00008   CONTINUE
       IF (INORD .EQ. (MAXG-1)) THEN
       INORD=MAXG
       END IF
       END IF

       END DO
       RETURN
       END
       
      SUBROUTINE FIXMAT(INMAT,MAXG,CURPLACE)
      REAL INMAT(50,50),COMPMAT(50,50)
      INTEGER MAXG,I,J,CURPLACE(251)
       DO 9 I=1,MAXG
       DO 10 J=1,MAXG
       COMPMAT(I,J)=INMAT(I,J)
00010   CONTINUE
00009    CONTINUE
       DO 11 I=1,MAXG
       DO 12 J=1,MAXG
       INMAT(CURPLACE(I),CURPLACE(J))=COMPMAT(I,J)
00012   CONTINUE
00011    CONTINUE
       RETURN
       END

      SUBROUTINE PLOTMAT(TINMAT,MAXG,UANCHOR,NUMDIM,
     C STARTINC,SYMMAT,COORD,MAXINC,BYINC,MEANVAL,MAXK,ANGLE,
     C RADIUS,CENTER,IANCHOR,UANCHOR2,MOVE2,MAXVAL,DRADIUS,
     C KEXP,XNORMAL,XMINVAL,TF1,BYANGLE,BYSCALE,XMINPICT,XCTCENT1,
     C XCTCENT2,WRITSMAC,WARP,RANGEEIG)


      INTEGER MAXG,CHANGE,COMPARE,I,J,H,ORDER(251),
     C INORD,MOVEG,TOG,KTOG,THISG,TOPVAL,ANCHOR,ANCHOR2,MEDIAN1,
     C NUMDIM,PLACED(251),NINCREM,STARTINC,SYMMAT,MEDIAN2,
     C MAXINC,BYINC,DANCHOR,MAXK,CENTER,IANCHOR(251),
     C DANCHOR2,TANCH2,MOVE2,FANCHOR(1),ONE,UANCHOR,UANCHOR2,
     C PORDER(251),ORDER2(251),NORMAL,TF1,SPREADEM,MUSTP,
     C BYANGLE,BYSCALE,TB1,TB2,DEGREE(251),HIGHDEGR,CD,GOTANCH,
     C BYDEGREE,PLACEI,SORTFLAG,IER,IGNUIN,K,WRITSMAC,
     C MAXJ,BASEI,NOLOG,XNORMAL,GOTEM,TOTVAL,MATZ,IERR,
     C XMAXG,NORS,DIMEN,DOGAL,APMIN,TB3,IV1(50)

      REAL TINMAT(50,50),INMAT(50,50),HIVAL,SV,POSVAL,
     C NEWCONT,DENOM,IMPVAL,CURVAL,CENT(251),RADIUS(251),
     C ANGLE(251),COORD(251,2),KEXP,TANGLE,TCOORD(251),
     C KANGLE,KINCREM,OMAXVAL,LMAX,MAXVAL,TDIST,LOVAL,RMAXG,
     C MEANVAL,MEANDIM,TMAXVAL,DRADIUS,CENT2(251),PCENT(251),
     C TI,TH,MINVAL,ZINCREM,ZANGLE,KSCALE,ACTDIST,KMAXVAL,
     C MINPICT,XMINPICT,PCTCENT1,PCTCENT2,XCTCENT1,XCTCENT2,
     C KMINVAL,LMIN,TD,ANUL,WARP,Z(50,50),FV1(50),FV2(50),
     C W(50),XINMAT(50,50),TOTPOS,TOTVAR,VAL1,TVAL1,XMAXVAL,
     C SUMK(50),SUMJ(50),SUMALL,EWEIGHT,TOTDYAD,XMINVAL,
     C RANGEEIG(2),MINDIM,MAXDIM,WR(50),WI(50),MAXEIG,MINEIG


       DOUBLE PRECISION PDIST,SQPDIST,SCALEFAC,STRESS,SUMPDIST 


       INTEGER DOSMAC,CI,SI,SJ,SPARAM(5,10),NPAR(5)

       CHARACTER*40 FORM(5)
       CHARACTER*32 NAME1/'smac.dat2'/, NAME2/'smac.data'/
       INTEGER LINK,STATUS

        APMIN=0
        IF (XMINVAL .LT. 0) THEN
        APMIN=1
        END IF
        MINVAL=ABS(XMINVAL)
        
         NOLOG=0
        IF (XNORMAL .LT. 0) THEN
        NOLOG=1
        END IF
        NORMAL=ABS(XNORMAL)


C        DOSMAC=1
       DOGAL=0
       IF (BYANGLE .EQ. 3) THEN
       DOGAL=1
       END IF
        DOSMAC=0
       IF (BYANGLE .EQ. 2) THEN
       DOSMAC=1
       END IF

       SPREADEM=0
        PCTCENT1=ABS(XCTCENT1)
        PCTCENT2=ABS(XCTCENT2)
        BYDEGREE=1   
        IF (XCTCENT1 .LT. 0)  THEN
        BYDEGREE=0
         END IF 
       IF (STARTINC .LT. 0) THEN
        SPREADEM=1
        STARTINC=-STARTINC
       END IF
 

      IF (ABS(MAXVAL) .LT. .00001) THEN
      MAXVAL=-999
      LMAX=-999
       END IF

       LMIN=9999
      DO 61 I=1,MAXG
      DO 62 J=1,MAXG
      IF ((TINMAT(I,J) .GT. MAXVAL) .AND. (J .NE. I)) THEN
      MAXVAL=TINMAT(I,J)
       END IF
      IF ((TINMAT(I,J) .LT. LMIN) .AND.
     C  (ABS(TINMAT(I,J)) .GT. .0001) .AND. (J .NE. I)) THEN
      LMIN=TINMAT(I,J)
       END IF
00062   CONTINUE
00061    CONTINUE
      IF (ABS(MAXVAL) .LT. .00001) THEN
       MAXVAL=MAXVAL+MAXVAL/MAXG
      END IF

        KMINVAL=LMIN/2.000
        IF ((MINVAL .LT. KMINVAL) .OR. (APMIN .EQ. 1)) THEN
        KMINVAL=MINVAL
        END IF

       DENOM=MAXG*(MAXG-1)
C      IF (SYMMAT .EQ. 1) THEN
C      DENOM=DENOM/2
C      END IF
      MEANVAL=0
C       MAXVAL=TMAXVAL*(2-ZSYMMAT)

        MEDIAN1=0
       IF (UANCHOR .LT. 0) THEN 
       MEDIAN1=1   
       DANCHOR=-UANCHOR
       ELSE
       MEDIAN1=0
       DANCHOR=UANCHOR
       END IF

        MEDIAN2=0
       IF (UANCHOR2 .LT. 0) THEN 
       MEDIAN2=1   
       DANCHOR2=-UANCHOR2
       ELSE
       MEDIAN2=0
       DANCHOR2=UANCHOR2
       END IF


      TH=1
      IF ((MAXVAL .GT. 0) .AND. (NORMAL .EQ. 3)) THEN
      TH=MAXVAL
      END IF
       MINPICT=XMINPICT*MAXVAL
      KMAXVAL=MAXVAL
      IF ((NORMAL .EQ. 2) .OR. (NORMAL .EQ. 3)) THEN 
      KMAXVAL=LOG(TH)-LOG(KMINVAL)
      IF (NOLOG .EQ. 1) THEN
      KMAXVAL=TH/KMINVAL
      END IF
      END IF
 
      DO 44 I=1,MAXG
      IANCHOR(I)=0
      INMAT(I,I)=0
      RADIUS(I)=0
      ANGLE(I)=0
      PLACED(I)=0
      DO 66 J=1,NUMDIM
      COORD(I,J)=0
00066  CONTINUE


        DO 55 J=1,MAXG
      IF (J .NE. I) THEN

      IF (SYMMAT .EQ. 1) THEN
      INMAT(I,J)=(TINMAT(I,J)+TINMAT(J,I))/2
      ELSE
       INMAT(I,J)=TINMAT(I,J)
      END IF

      IF ((NORMAL .GT. 3) .OR. (NORMAL .LT. 2)) THEN
      INMAT(I,J)=MAXVAL-INMAT(I,J)
      END IF
      IF ((NORMAL .EQ. 2) .OR. (NORMAL .EQ. 3)) THEN
      IF (INMAT(I,J) .LT. KMINVAL) THEN 
      INMAT(I,J)=KMINVAL
      END IF
      IF (NOLOG .EQ. 0) THEN
      INMAT(I,J)=LOG(TH) - LOG(INMAT(I,J))
      ELSE
      INMAT(I,J)=TH/INMAT(I,J)
      END IF

      END IF

      END IF
       MEANVAL=MEANVAL+INMAT(I,J)/DENOM
00055  CONTINUE
00044  CONTINUE
       HIGHDEGR=0
      IF (DANCHOR .GT. MAXG) THEN
      DANCHOR=MAXG
      END IF
      IF ((DANCHOR .GT. 0) .OR. (DANCHOR2 .GT. 0)) THEN
      DO 22 I=1,MAXG
      CENT(I)=0
      ORDER(I)=I
      ORDER2(I)=I
       DEGREE(I)=0
      DO 33 J=1,MAXG
      IF (J .NE. I) THEN
      IF (INMAT(I,J) .LT. KMAXVAL) THEN
      DEGREE(I) = DEGREE(I)+1
       END IF
      CENT(I)=CENT(I)+INMAT(I,J)

      IF ((MEDIAN1 .EQ. 1) .OR. (MEDIAN2 .EQ. 1)) THEN
      PCENT(J)=INMAT(I,J)
      PORDER(J)=J
      END IF
      END IF
00033  CONTINUE
      CENT2(I)=CENT(I)
      IF ((MEDIAN2 .EQ. 1) .OR. (MEDIAN1 .EQ. 1)) THEN
      IF (DEGREE(I) .GE. HIGHDEGR) THEN
      HIGHDEGR=DEGREE(I)
        END IF
       IF (BYDEGREE .EQ. 1) THEN
       PLACEI=DEGREE(I)
        ELSE
       PLACEI=MAXG
       END IF
      IF (MAXG .GT. 1) THEN
      SORTFLAG=2
      CALL SPSORT(PCENT,MAXG,PORDER,SORTFLAG,IER)
      END IF
      IF (MEDIAN1 .EQ. 1) THEN
      TB1=INT(PCTCENT1*PLACEI)
      IF (TB1 .LT. 1) THEN
      TB1=1
      END IF
      IF (TB1 .GT. MAXG) THEN
      TB1=MAXG
      END IF
      CENT(I)=PCENT(TB1)
      END IF

      IF (MEDIAN2 .EQ. 1) THEN
       IF (BYDEGREE .EQ. 1) THEN
       PLACEI=DEGREE(I)
         ELSE
       PLACEI=MAXG
       END IF

      TB2=INT(PCTCENT2*PLACEI)
      IF (TB2 .LT. 1) THEN
      TB2=1
      END IF
      IF (TB2 .GT. MAXG) THEN
      TB2=MAXG
      END IF
      IF (TB2 .EQ. TB1) THEN
      IF (TB1 .LT. MAXG) THEN
      TB2=TB2+1
       ELSE
      TB2=1
      END IF
      END IF
      CENT(I)=PCENT(TB2)
      END IF
       END IF
00022   CONTINUE
        DO 7755 TB2=1,MAXG
        CENT(TB2)=CENT(TB2)+TB2*.0001
07755    CONTINUE
       END IF
      IF (MAXG .EQ. 2) THEN
       COORD(1,1)=-INMAT(1,2)/2
       COORD(2,1)=INMAT(1,2)/2
       COORD(1,2)=0
       COORD(2,2)=0
       RADIUS(1)=INMAT(1,2)/2
       RADIUS(2)=INMAT(1,2)/2
       ANGLE(1)=3.1415926
       ANGLE(2)=0
       WARP=1.00
      END IF
      IF (MAXG .GT. 2) THEN
       IF (WARP .GT. 0) THEN
      MATZ=1
      NORS=1
      SV=.0001
      POSVAL=0

      DO 972 SJ=1,50
      DO 973 TB2=1,50
      XINMAT(SJ,TB2)=0
00973  CONTINUE
C      XINMAT(SJ,SJ)=MAXVAL
00972   CONTINUE
      SUMALL=0
      DO 93 SJ=1,MAXG
      SUMJ(SJ)=0
      SUMK(SJ)=0
      DO 94 SK=1,MAXG
      SUMJ(SJ)=SUMJ(SJ)+INMAT(SJ,SK)**2
      SUMK(SJ)=SUMK(SJ)+INMAT(SK,SJ)**2
      SUMALL=SUMALL+INMAT(SK,SJ)**2
00094  CONTINUE
00093   CONTINUE
       RMAXG=MAXG
      
      DO 72 SJ=1,MAXG
      DO 73 TB2=1,SJ
      TVAL1=(INMAT(SJ,TB2) + INMAT(TB2,SJ))/(2.00)
      IF ((ABS(TVAL1) .GT. SV) .AND. (ABS(POSVAL) .LT. SV)) THEN
      POSVAL=TVAL1
      END IF 
      XINMAT(SJ,TB2)=SUMJ(TB2)/RMAXG + SUMK(SJ)/RMAXG-
     C                          (SUMALL/RMAXG**2)-
     C                          INMAT(SJ,TB2)**2
      XINMAT(SJ,TB2)=XINMAT(SJ,TB2)/2.000
      IF ((TVAL1 .NE. POSVAL) .AND. (ABS(POSVAL) .GT. SV) .AND.
     C    (ABS(TVAL1) .GT. SV) .AND.
     C    (NORS .EQ. 1) .AND. (SJ .NE. TB2))  THEN
      NORS=0
      END IF
00073  CONTINUE
00072  CONTINUE


      DIMEN=50
      XMAXG=MAXG

      IF ((NORS .EQ. 0) .AND. (WRITSMAC .EQ. 3))  THEN
      CALL RG(DIMEN,XMAXG,INMAT,WR,WI,MATZ,Z,IV1,FV1,IERR)
      OPEN(62,FILE="oeigen.dat")
      OPEN(64,FILE="eigvals.dat")
      MAXEIG=0
      MINEIG=0
      DO 523 I=1,XMAXG
      IF (WR(I) .GT. MAXEIG) THEN
      MAXEIG=WR(I)
      END IF
 
      IF (WR(I) .LT. MINEIG) THEN
      MINEIG=WR(I)
      END IF
      WRITE(62,6262) I,WR(I),WI(I)
      WRITE(64,6262) I,WR(I),WI(I)
      DO 5231 J=1,XMAXG
      WRITE(62,6362) Z(J,I),Z(J,I+1)
05231  CONTINUE
00523    CONTINUE
      CLOSE(62)
      CLOSE(64)
      OPEN(62,file="exteig.dat")
      WRITE(62,6362) MAXEIG,MINEIG
      CLOSE(62)


*      CALL RS(DIMEN,XMAXG,TINMAT,W,MATZ,Z,FV1,FV2,IERR)
*      OPEN(62,FILE="oeigen.dat")
*      DO 7543 TB2=1,XMAXG
*      WRITE(62,6262) TB2,W(TB2)
*07543 CONTINUE
*      CLOSE(62)
      
      CALL RS(DIMEN,XMAXG,XINMAT,W,MATZ,Z,FV1,FV2,IERR)


      XMAXG=MAXG
      TOTVAR=0
      TOTPOS=0 
       DIMEN=0      
       RANGEEIG(1)=0
      OPEN(62,FILE="eigen.dat")
      DO 543 TB2=1,XMAXG
      WRITE(62,6262) TB2,W(TB2)
      DO 5431 TB3=1,XMAXG
      WRITE(62,6261) Z(TB2,TB3)
05431 CONTINUE
      SJ=XMAXG-TB2+1
      IF ((DIMEN .LT. NUMDIM) .AND. (ABS(W(SJ)) .GT. .0001) .AND. 
     C (DOGAL .EQ. 1)) THEN
      DIMEN=DIMEN+1
      EWEIGHT=SQRT(ABS(W(SJ)))
      DO 544 SI=1,XMAXG
      COORD(SI,DIMEN)=Z(SI,SJ)*EWEIGHT
00544  CONTINUE
      END IF
      INMAT(SJ,SJ)=0
      TOTVAR=TOTVAR + W(SJ)
      IF (W(SJ) .GT. .0000) THEN 
      TOTPOS=TOTPOS+W(SJ)
      END IF
       IF (ABS(RANGEEIG(1)) .LT. .0000010) THEN
       RANGEEIG(1)=W(SJ)
       END IF

00543  CONTINUE
      CLOSE(62)

       RANGEEIG(2)=W(SJ)
       WARP=TOTPOS/TOTVAR
      END IF
       END IF

      IF (DOSMAC .EQ. 1) THEN
       IF (WRITSMAC .EQ. 1) THEN
        OPEN(62,FILE="coeff.fix")
        WRITE(62,6261) DRADIUS
        CLOSE(62)
       ANUL=0
       CALL WRITPARM(MAXG,SYMMAT,NUMDIM,ANUL)
       CALL WRITDATA(INMAT,MAXG,SYMMAT)
       RETURN
      END IF
C      WRITESMAC
      IF (WRITSMAC .EQ. 2) THEN  
      CALL RUNSMAC 
      RETURN
      END IF

      IF (WRITSMAC .EQ. 3) THEN
      GOTEM=0
      OPEN(24,file="smacdist")
       DO 841 SI=1,MAXG
      READ(24,140,END=849) (COORD(SI,SJ) , SJ=1,NUMDIM)
00841 CONTINUE
       GOTEM=1

       IF (GOTEM .EQ. 0) THEN
00849  CLOSE(24)
       GOTEM=1
       ANUL=1
       CALL WRITPARM(MAXG,SYMMAT,NUMDIM,ANUL)
       CALL RUNSMAC 
       OPEN(24,FILE="smacdist")
       DO 881 SI=1,MAXG
      READ(24,140,END=2899) (COORD(SI,SJ) , SJ=1,NUMDIM)
00881 CONTINUE
      IF (GOTEM .EQ. 0) THEN
02899 CLOSE(24)
       GOTEM=1
       ANUL=0
        OPEN(62,FILE="coeff.fix")
        WRITE(62,6261) ANUL
        CLOSE(62)
       CALL WRITPARM(MAXG,SYMMAT,NUMDIM,ANUL)
       CALL RUNSMAC 
       OPEN(24,FILE="smacdist")
       DO 681 SI=1,MAXG
      READ(24,140,END=3899) (COORD(SI,SJ) , SJ=1,NUMDIM)
00681 CONTINUE
      IF (GOTEM .EQ. 0) THEN
03899 WRITE(6,*) "Smacof failed to produce coordinates, will use AMDS"
      DOSMAC=0
      END IF
      END IF
      END IF
       
       CLOSE(24)
       OPEN(24,file="coeff")
       READ(24,6261,END=8888) COEFF
08888  CLOSE(24)

       DO 842 SI=1,MAXG
       DO 843 SJ=1,NUMDIM
       COORD(SI,SJ)=COORD(SI,SJ)/COEFF
00843   CONTINUE
00842   CONTINUE
        END IF
C        WRITSMAC= 3

C        ELSE

C       DO SMACOF       

       SORTFLAG=2
       CALL SPSORT(CENT,MAXG,ORDER,SORTFLAG,IER)
C       END IF
       END IF



       IF (DANCHOR .NE. 0) THEN
       GOTANCH=0
       CD=DANCHOR
       DO WHILE (GOTANCH .EQ. 0)
       ANCHOR=ORDER(CD)
       IF ((BYDEGREE .EQ. 0) .OR. 
     C (DEGREE(ANCHOR) .GE. HIGHDEGR)) THEN
        IF (DOSMAC .EQ. 0) THEN
       IANCHOR(ANCHOR)=1
         END IF
       GOTANCH=1 
       ELSE
       CD=CD+1
       END IF
        END DO

       ELSE
C       DANCHOR .NE. 0

       ONE=1
       FANCHOR(1)=IGNUIN(1,(MAXG-ONE))
C       CALL GGUD(FANCHOR,MAXG,ONE)
       ANCHOR=FANCHOR(1)
       END IF

C      DANCHOR .NE. 0 
       IF ((DOSMAC .NE. 1) .AND. (DOGAL .NE. 1)) THEN

       IF (DANCHOR2 .GT. MAXG) THEN
       DANCHOR2=MAXG
       END IF
       IF (DANCHOR2 .EQ. DANCHOR) THEN
       IF (DANCHOR .LT. MAXG) THEN
       DANCHOR2=DANCHOR2+1
       ELSE
       DANCHOR2=1
       END IF
       END IF


       IF (DANCHOR2 .NE. 0) THEN
       IF (MAXG .GT. 2) THEN
       SORTFLAG=2
       CALL SPSORT(CENT2,MAXG,ORDER2,SORTFLAG,IER)
       ANCHOR2=ORDER2(DANCHOR2)
       END IF
       ELSE
       ONE=1
       FANCHOR(1)=IGNUIN(1,(MAXG-ONE))
C       CALL GGUD(FANCHOR,(MAXG-ONE),ONE)
       IF (FANCHOR(1) .EQ. ANCHOR) THEN
        IF (ANCHOR .EQ. MAXG) THEN
        ANCHOR2=1
        ELSE
        ANCHOR2=FANCHOR(1)+1
        END IF
       ELSE
       ANCHOR2=FANCHOR(1)
       END IF
       END IF

C      DANCHOR NE 0       

       COORD(ANCHOR,1)=0
       COORD(ANCHOR,2)=0

C       MAXVAL=4
C       DRADIUS=2
       TD=1
       IF (DRADIUS .GT. 0) THEN 
       TD=DRADIUS
       ELSE
       TD=-DRADIUS
       END IF

       DO 34 I=1,MAXG
       IF (I .NE. ANCHOR) THEN
       RADIUS(I)=INMAT(I,ANCHOR)/TD
       IF (RADIUS(I) .LT. MINPICT) THEN
       RADIUS(I)=MINPICT
       END IF
       END IF
00034   CONTINUE

C       PLACED(ANCHOR)=1
       PLACED(ANCHOR2)=1
       COORD(ANCHOR2,1)=RADIUS(ANCHOR2)
       COORD(ANCHOR2,2)=0
       IF (MOVE2 .EQ. 0) THEN
       TANCH2=ANCHOR2
        ELSE
       TANCH2=0
       END IF
       ONE=1
       IF (SPREADEM .EQ. 1) THEN
        MUSTP=MAXG-ONE
       IF (DANCHOR2 .NE. 0) THEN
        MUSTP=MUSTP-ONE
       END IF

       ZINCREM=6.28318/MUSTP
       ZANGLE=0
       DO 98 I=1,MAXG
       IF ((I .NE. DANCHOR) .AND. (I .NE. DANCHOR2)) THEN
       ZANGLE=ZANGLE+ZINCREM
       ANGLE(I)=ZANGLE       
       PLACED(I)=1
      COORD(I,1)=RADIUS(I)*COS(ZANGLE)
      COORD(I,2)=RADIUS(I)*SIN(ZANGLE)
       END IF
00098  CONTINUE
       END IF

C      SPREADEM .EQ. 1

      IF (DRADIUS .LT. 0) THEN
       DO 733 I=1,MAXG
       IF (I .NE. DANCHOR) THEN
       DO 734 J=1,MAXG
       IF (J .NE. DANCHOR) THEN
       INMAT(I,J)=INMAT(I,J)*(RADIUS(I)+RADIUS(J))/2
        END IF
00734   CONTINUE
        END IF
00733    CONTINUE
        END IF

C      KEXP=0
      LOVAL=0
      CHANGE=1
      KSCALE=1
       MOVEG=0
      NINCREM=STARTINC
      DO WHILE (CHANGE .EQ. 1)
      IF (NINCREM .LE. MAXINC) THEN
      NINCREM=NINCREM+BYINC
      END IF
      KINCREM=6.28318/NINCREM
      CHANGE=0
      HIVAL=-.0001

      DO 4 I=1,MAXG
      IF ((I .NE. MOVEG) .AND. (I .NE. ANCHOR) .AND. 
     C (I .NE. TANCH2)) THEN
      CURVAL=999999
      IF (PLACED(I) .EQ. 1) THEN
      CURVAL=0
      DO 322 H=1,MAXG
      IF ((H .NE. I) .AND. (PLACED(H) .EQ. 1)) THEN
      ACTDIST=INMAT(I,H)
      IF (BYANGLE .EQ. 1) THEN
      ACTDIST=ACTDIST*3.14159/KMAXVAL
      END IF
      KSCALE=1
      IF (BYSCALE .EQ. 1) THEN 
      KSCALE=2*INMAT(I,H)/(RADIUS(I)+RADIUS(H))
      END IF
      CALL KDIST(I,H,COORD,MAXK,NUMDIM,
     C KEXP,ACTDIST,TDIST,NORMAL,MINPICT,KSCALE,ANGLE(I),ANGLE(H),
     C BYANGLE)
      CURVAL=CURVAL+TDIST
      END IF
00322  CONTINUE
      END IF
      DO 9 H=1,NUMDIM
      TCOORD(H)=COORD(I,H)
00009  CONTINUE
      DO 5 J=1,NINCREM
      TANGLE=(J-1)*KINCREM
      IF ((PLACED(I) .EQ. 0) .OR. 
     C (ABS(TANGLE-ANGLE(I)) .GT. .0001)) THEN
      COORD(I,1)=RADIUS(I)*COS(TANGLE)
      COORD(I,2)=RADIUS(I)*SIN(TANGLE)
      NEWCONT=0      
      DO 6 H=1,MAXG
      IF ((H .NE. I) .AND. (PLACED(H) .EQ. 1)) THEN 
      ACTDIST=INMAT(I,H)
      IF (BYANGLE .EQ. 1) THEN
      ACTDIST=ACTDIST*3.14159/KMAXVAL
      END IF
      IF (BYSCALE .EQ. 1) THEN 
      KSCALE=2*INMAT(I,H)/(RADIUS(I)+RADIUS(H))
      END IF
      CALL KDIST(I,H,COORD,MAXK,NUMDIM,
     C KEXP,ACTDIST,TDIST,NORMAL,MINPICT,KSCALE,TANGLE,ANGLE(H),
     C BYANGLE)
      NEWCONT=NEWCONT+TDIST
      END IF
00006 CONTINUE
      IMPVAL=NEWCONT-CURVAL
      IF (IMPVAL .LT. HIVAL) THEN
      HIVAL=IMPVAL
      MOVEG=I
      KANGLE=TANGLE
      CHANGE=1
      END IF
      END IF
00005  CONTINUE
      DO 99 H=1,NUMDIM
      COORD(I,H)=TCOORD(H)
00099  CONTINUE
      END IF
00004   CONTINUE

      IF (CHANGE .EQ. 1) THEN
      PLACED(MOVEG)=1
      ANGLE(MOVEG)=KANGLE
      COORD(MOVEG,1)=RADIUS(MOVEG)*COS(KANGLE)
      COORD(MOVEG,2)=RADIUS(MOVEG)*SIN(KANGLE)
      END IF
       END DO
      IF (CENTER .EQ. 1) THEN
      DO 627 I =1,MAXG
      RADIUS(I)=0
00627 CONTINUE

      DO 677 K=1,NUMDIM
      MINDIM=99999
      MAXDIM=-MINDIM
      MEANDIM=0
      DO 788 I=1,MAXG
      IF (COORD(I,K) .GT. MAXDIM) THEN
      MAXDIM=COORD(I,K)
      END IF
      IF (COORD(I,K) .LT. MINDIM) THEN
      MINDIM=COORD(I,K)
      END IF
C      MEANDIM=MEANDIM+COORD(I,K)/MAXG
00788  CONTINUE
       MEANDIM=(MAXDIM-MINDIM)/2.000
      DO 899 I=1,MAXG
      COORD(I,K)=COORD(I,K)-MEANDIM
      RADIUS(I)=RADIUS(I)+COORD(I,K)**2
00899  CONTINUE
00677   CONTINUE
      DO 8787 I=1,MAXG
      RADIUS(I)=RADIUS(I)**.5
      ANGLE(I)=ATAN2(COORD(I,2),COORD(I,1))
      IF (ANGLE(I) .LT. 0) THEN 
       ANGLE(I)=ANGLE(I)+6.28318
      END IF
      IF (ANGLE(I) .GT. 6.28318) THEN 
       ANGLE(I)=ANGLE(I)-6.28318
      END IF
08787  CONTINUE
       END IF
C       CENTER .EQ. 1
       END IF
C       DO SMACOF
       END IF
C       MAXG GT 1
       IF ((MAXG .GT. 1) .AND.
     C ((DOSMAC .EQ. 1) .OR. (DOGAL .EQ. 1))) THEN
       DO 8484 I=1,MAXG
       RADIUS(I)=SQRT(COORD(I,1)**2+COORD(I,2)**2)
       ANGLE(I)=ATAN2(COORD(I,2),COORD(I,1))
C       ANGLE(I)=ACOSD(COORD(I,2)/RADIUS(I))
08484   CONTINUE
       END IF
        SQPDIST=0
        SUMPDIST=0
        STRESS=0
        DO 8221 I=2,MAXG
        DO 8222 J=1,I
        PDIST=SQRT((COORD(I,1)-COORD(J,1))**2 + 
     C (COORD(I,2)-COORD(J,2))**2)
        SQPDIST=SQPDIST+PDIST**2
        SUMPDIST=SUMPDIST+PDIST
        STRESS=STRESS+(PDIST-COEFF*INMAT(I,J))**2
08222    CONTINUE
08221     CONTINUE
        TOTDYAD=(MAXG*(MAXG-1))/2
        SCALEFAC=SQPDIST-SUMPDIST/TOTDYAD
        STRESS=STRESS/SCALEFAC
C      WRITE(33,7474) "The stress associated with ",MATLEN," is "
C     C ,STRESS
       RETURN
07474  FORMAT(A27,I5,A4,F10.5)
 140  FORMAT(8X,6F12.7)
 2469 FORMAT(F5.2$)
 2470 FORMAT(F5.2)
 6767 FORMAT(20A8)
 6768 FORMAT(20A11)
 6261  FORMAT(F10.5)
 6262  FORMAT(I10,2F10.5)
 6362      FORMAT(5F10.5)

       END
      
       SUBROUTINE KDIST(I,J,COORD,MAXG,NUMDIM,KEXP,TRUEDIST,
     C OUTDIST,NORMAL,MINDIST,KSCALE,ANGLEI,ANGLEH,BYANGLE)
       INTEGER I,J,NUMDIM,MAXG,K,NORMAL,BYANGLE
       REAL COORD(MAXG,NUMDIM),KEXP,TKDIST,TRUEDIST,OUTDIST,VDIST,
     C MINDIST,KSCALE,ANGLEI,ANGLEH,DIFFANGL

       TKDIST=0
       DO 2 K=1,NUMDIM
       TKDIST=TKDIST+(COORD(I,K)-COORD(J,K))**2
00002   CONTINUE
       TKDIST=(TKDIST**.5)*KSCALE
       IF (TKDIST .LT. MINDIST) THEN
       VDIST=999
       ELSE
      IF (BYANGLE .EQ. 1) THEN
      TKDIST=ABS(ANGLEI-ANGLEH)
      IF (TKDIST .GT. 3.14159) THEN
      TKDIST=3.14159*2-TKDIST
      END IF
      END IF
       VDIST=TRUEDIST-TKDIST
       END IF

       IF (NORMAL .EQ. 1) THEN
        VDIST=VDIST/TRUEDIST
       END IF
       IF (KEXP .LT. 0) THEN
       OUTDIST=ABS(VDIST)
       END IF
       IF (ABS(KEXP) .LT .00001) THEN
       OUTDIST=LOG(ABS(VDIST))
       END IF
       IF (KEXP .GT. 0) THEN
       OUTDIST=VDIST**KEXP
       END IF
       RETURN
       END 

      SUBROUTINE OUTCOORD(COORFILE,MAXDEPT,NUMDIM,COORD,WITHID,
     C NSUBID,ANGLE,RADIUS,NEWDEPT,ANCHOR,HIER,FIT,NSIM,PRINTK,ADDEND,
     C ISTRESS,ISTRESS2,ISTRESS3,PDIST,DISTFILE,WARP,GRANGE1,GRANGE2)
      INTEGER MAXDEPT,U,Z,NUMDIM,WITHID,NSUBID(251),TU,NEWDEPT(251),
     C ANCHOR(251),DU,NSIM,PRINTK,ADDEND,PDIST,TH,HU,SYMLNK,STATUS,
     C SIXTEEN

      REAL COORD(251,2),ANGLE(251),RADIUS(251),
     C HIER(251),FIT(251),ISTRESS(251),ISTRESS2(251),ISTRESS3(251),
     C DIFFANG,PI,RPDIST,WARP(251),GRANGE1(251),GRANGE2(251)

      CHARACTER*12 COORFILE,SFILE,DISTFILE,LINKFILE
      CHARACTER*6 INFILE

      IF (COORFILE(9:9) .EQ. "i") THEN
      DISTFILE="compgrp.iang"
      ELSE
      DISTFILE="compgrp.gang"
      END IF
      LINKFILE="inlist" // COORFILE(7:12)
      SIXTEEN=16
      SIXTEEN=UNLINK(LINKFILE)

      STATUS=SYMLNK(COORFILE,LINKFILE)

      INFILE=COORFILE(1:6)
       PI=3.1415926
      OPEN(56,file=COORFILE)
      SFILE=COORFILE(1:11) // "s"
      OPEN(57,file=SFILE)
      IF (PDIST .EQ. 1) THEN
      OPEN(23,file=DISTFILE)
      END IF

       IF (ADDEND .EQ. 1) THEN
        CALL MOVEEND(56)
        CALL MOVEEND(57)
        IF (PDIST .EQ. 1) THEN
        CALL MOVEEND(23)
        END IF
        END IF

       DO 28283 TU=1,MAXDEPT
        IF (WITHID .EQ. 1) THEN
        U=NSUBID(TU)
        ELSE
        U=TU
        END IF
      WRITE(56,445) NEWDEPT(TU),U,ANCHOR(TU),ANGLE(TU),RADIUS(TU),
     C HIER(TU), (COORD(TU,Z) , Z=1,NUMDIM) ,WARP(TU), GRANGE1(TU),
     C GRANGE2(TU)
      WRITE(57,251) NEWDEPT(TU),U,FIT(TU),NSIM,PRINTK,ISTRESS(TU),
     C ISTRESS2(TU),ISTRESS3(TU)
       IF (PDIST .EQ. 1) THEN
        DO 28282 TH=1,TU
        IF ((ANCHOR(TH) .EQ. 0) .AND. (ANCHOR(TU) .EQ. 0) .AND.
     C  (TH .NE. TU)) THEN
        IF (WITHID .EQ. 1) THEN
        HU=NSUBID(TH)
        ELSE
        HU=TH
        END IF
        DIFFANG=ABS(ANGLE(TU)-ANGLE(TH))
        IF (DIFFANG .GT. PI) THEN
        DIFFANG=2.0*PI-DIFFANG
        END IF
        RPDIST=SQRT((COORD(TU,1)-COORD(TH,1))**2+
     C            (COORD(TU,2)-COORD(TH,2))**2)

        WRITE(23,443) INFILE,U,NEWDEPT(TU),HU,NEWDEPT(TH),
     C  ANGLE(TU),ANGLE(TH),DIFFANG,MAXDEPT,RPDIST
        END IF
28282    CONTINUE
        END IF
28283  CONTINUE
      
      CLOSE(56)
      CLOSE(57)
      IF (PDIST .EQ. 1) THEN
      CLOSE(23)
      END IF
      
      RETURN
00445  FORMAT(I5,2I10,8F20.10)
00443  FORMAT(A6,4I10,3F10.5,I10,F10.5) 
00251  FORMAT(I5,I10,F10.5,2I10,3F10.5)
      END

      SUBROUTINE ONEPIECE(I,COUNTN,TEMPMAT,OLDORD,TSIZE)
      INTEGER I,J,K,COUNTN,OLDORD(251),TSIZE
      REAL TEMPMAT(50,50)

      REAL ZCOMPMAT(251,251)

       INTEGER HUBERT(251,251),RESULTM(251,251),ALLGROUP(251,251),
     C NEWMAT(251,251)
       REAL BLAUC(251,251),DIFF(251,251)
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

      SUBROUTINE BSPAN(DIFF,NUMTEACH,MAXDEPT,BOUND,CHANGEC,NEWDEPT,
     C ATITLE,XTITLE1,XTITLE2,DEPARTN,NSUBID,INFILE,ABBREV,TYPE,ADDEND,
     C NSIM,PRINTK)
      INTEGER NUMTEACH,MAXDEPT,CHANGEC(251),ID,COUNTG(50),
     C IB,JB,NEWDEPT(251),TJB,BOUNDG(50,251),ADDEND,
     C DEPARTN(251),PERS,NSUBID(251),GROUP,TG,THISPER,NSIM,
     C GR,ABBREV,PRINTK

      REAL DIFF(251,251),BOUND
      CHARACTER ATITLE(3)*20,INFILE*16,XTITLE1*16,XTITLE2*16,TYPE*1,
     C BFILE*13,BFILE2*14,TBFILEB*11

      REAL ZCOMPMAT(251,251)

       INTEGER HUBERT(251,251),RESULTM(251,251),ALLGROUP(251,251),
     C NEWMAT(251,251)
       REAL TBLAUC(251,251),TDIFF(251,251)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C TBLAUC,TDIFF

      BFILE=INFILE(1:6) // ".bound" // TYPE 
      OPEN(20,file=BFILE)
      BFILE=INFILE(1:6) // ".bndat" // TYPE
      OPEN(23,file=BFILE)
      IF (ADDEND .EQ. 1) THEN
      CALL MOVEEND(20)
      CALL MOVEEND(23) 
      END IF
        DO 00087 ID=1,MAXDEPT
         COUNTG(ID) = 0
00087     CONTINUE

         DO 8247 IB=1,NUMTEACH
         DO 8248 TJB=1,MAXDEPT
            JB=CHANGEC(TJB)
            IF (NEWDEPT(IB) .NE. JB) THEN
            IF (DIFF(IB,JB) .GT. BOUND) THEN
              COUNTG(JB)=COUNTG(JB) + 1
              BOUNDG(JB,COUNTG(JB))=IB
            END IF
            END IF
08248      CONTINUE
08247       CONTINUE


         WRITE(20,102) (ATITLE(A) , A=1,3)
         WRITE(20,89897) XTITLE1,INFILE
         WRITE(20,2107) XTITLE2,
     C       (CHANGEC(IB) , IB=1,MAXDEPT)
         WRITE(20,2107) "GROUP SIZE",
     C   (DEPARTN(IB) , IB=1,MAXDEPT)
         WRITE(20,101) "ACTOR" , "GROUP", "CONNECT"

        DO 84 GROUP=1,MAXDEPT
        TG=CHANGEC(GROUP)
         DO 85 PERS=1,DEPARTN(GROUP)
        THISPER=ALLGROUP(GROUP,PERS)
         WRITE(20,2103) NSUBID(THISPER),GROUP,
     C   (DIFF(THISPER,CHANGEC(GR)), GR=1,MAXDEPT)
         WRITE(23,21033) NSIM,PRINTK,NSUBID(THISPER),GROUP,
     C   (DIFF(THISPER,CHANGEC(GR)), GR=1,MAXDEPT)
   85    CONTINUE
   84   CONTINUE

        CLOSE(20)
        CLOSE(23)
      BFILE2=INFILE(1:6) // ".bound" // TYPE // "2"
      OPEN(20,file=BFILE2)
      IF (ADDEND .EQ. 1) THEN
      CALL MOVEEND(20)
      END IF

      TBFILEB=INFILE(1:6) // TYPE //".ddat"
      OPEN(27,file=TBFILEB)
      IF (ADDEND .EQ. 1) THEN
      CALL MOVEEND(27)
      END IF

          IF (ABBREV .EQ. 1) THEN
          WRITE(20,89897) "ABBREVIATED BOUNDARY -- COMPACTNESS ",
     C    INFILE
         WRITE(20,102) (ATITLE(A) , A=1,3)
         WRITE(20,102) "CUT-OFF VALUE"
         WRITE(20,251) BOUND
         DO 00111 GROUP=1,MAXDEPT
          TG=CHANGEC(GROUP)
          IF ((DEPARTN(TG) .GT. 0) .AND. (TG .LE. MAXDEPT)) THEN
          WRITE(20,2107) "---------------"
          WRITE(20,105) "Group ",TG
          WRITE(20,105) "# OF SPANNERS " ,COUNTG(GROUP)
          WRITE(20,103) 
           WRITE(20,105) "ACTOR ",(NSUBID(BOUNDG(GROUP,ZG)) , 
     C     ZG=1,COUNTG(GROUP))
           WRITE(20,104) "CONNECTION " , (DIFF(BOUNDG(GROUP,ZG),TG),
     C     ZG=1,COUNTG(GROUP))
           WRITE(20,105) "CURRENT GROUP ",
     C     (NEWDEPT(BOUNDG(GROUP,ZG)) , ZG=1,COUNTG(GROUP))
        DO 6464 BG=1,COUNTG(GROUP)
           WRITE(27,251) GROUP, NEWDEPT(BOUNDG(GROUP,BG))
06464    CONTINUE
         END IF
00111      CONTINUE
          
            END IF
        CLOSE(20)
        CLOSE(27)
       RETURN
  251 FORMAT(20(F15.5))
 2103 FORMAT(2I8,F8.0,251(F5.2,1X))  
21033 FORMAT(3I8,I6,251(F5.2,1X))  
 2107 FORMAT(A16,251(I5,1X))  
  103 FORMAT(F8.1,251(F5.2,1X))  
  105 FORMAT(A15,251(I5,1X))  
  104 FORMAT(A15,251(F5.2,1X))  
  102 FORMAT(10(A30)) 
  101 FORMAT(251(A8)) 
89897 FORMAT(10(A))
      END

      SUBROUTINE ROTATE(OUTCOORD,GCOORD,NUMTEACH,MAXG,ANGLE,
     C IRADIUS,
     C NINCREM,
     C MEASURE,DEPARTN,NRATIO,TEXTREME,HIWT,ORADIUS)

       INTEGER NUMTEACH,MAXG,THISPER,I,J,K,L,NINCREM,MEASURE,
     C DEPARTN(251),EXTREME,BT,NOTDONE,TEXTREME

       REAL IRADIUS(251),GCOORD(251,2),GMAT(50,50),
     C DIFFX,DIFFY,ADIFF,BESTDIFF,BESTANGL,ANGLE(251),
     C ORADIUS(251),TDIFF,TANGLE,KANGLE,KINCREM,NRATIO,
     C HIWT,OUTCOORD(251,2),TCOORD1,TCOORD2

       INTEGER HUBERT(251,251),RESULTM(251,251),ALLGROUP(251,251),
     C NEWMAT(251,251)

       REAL BLAUC(251,251),DIFF(251,251),ZCOMPMAT(251,251)
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

      SUBROUTINE PISIM2(NUMPEOPL,RANGEC,MINCHOI,RANGEG,BASEG,
     C BASEP,BETARAT,BETARAT2,OUTMAT,QSEED,DEPART,MAXFREQ,RANGEP,
     C BASEWP,WPIW,WPIB,WRANGEP,NSIM,XBASEP,XRANGEP,
     C PREPWI,PREQWI,PREPBT,PREQBT,PKPG,QKPG,PSIZE,QSIZE)


      INTEGER D,I,J,DEPART(251),D2,D3,OUTMAT(251,251),SIMRAND,THISD,
     C        D4,D5,COL,NUMGROUP,PERGROUP,NUMPEOPL,ONELESS,ACHOICE,
     C        MAXFREQ,MV,FREQS(251),USEMARG,TID,NR,AC,DIDG,DIDNG,
     C       OPENS,HAVEMEM,FILLG,FILLNG,COUNTING,COUNTNNG,FG,NFG,
     C  COUNTG(251),ING,GOTTEM,TG,IR(1),BYDENSE,TWT,NSIM,ELEM(251),
     C KCHOICE(251),MINCHOI,ONE,BASEG,RANGEG,DMEM,TARGETS,TN,
     C KOCHOICE(251),DIDEM,RANGEC,NUMG,IDEV(1),NSUBID(251),ZERO,
     C RANGEP,WRANGEP,BADONE
       INTEGER*4
     C  MEMBERS(100,50),NONMEM(100,251),BI,BG
       DOUBLE PRECISION QSEED
       REAL DEVIATE(1),INDEPT(251),OUTDEPT(251),EXTRA,PORTION,KPOSS,
     C R(1),INP(251),INQ(251),OUTP(251),XBASEP,XRANGEP,
     C OUTQ(251),WOUTQ(251),REM,TCOUNT,PSIZE,QSIZE,LOW,HIGH,
     C WOUTP(251),BASE,RANGE,PREPWI,PREQWI,PREPBT,PREQBT,
     C SUMB,PP,PQ,PREP,PREQ,WPREP,WPREQ,PKPG,QKPG,
     C BETARAT,TEMPP(251),BETARAT2,BASEP,IEXPECT(251),ISTD(251),
     C ICON(251),NOWCLOSE(251),BASEWP,WPIW,WPIB

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

      
      
      SUBROUTINE KSTRESS(STRESS,COORD,XNORMAL,MATLEN,
     C MAXVAL,XMINVAL,KEXP,STRESS2,FIT,ISTRESS,STRESS3,
     C ISTRESS2,ISTRESS3,COORD2,DISTFILE,PRINTO,NEWDEPT,
     C ID,ODDSRATI)

      INTEGER NUMTEACH,NORMAL,MATLEN,PRINTO(100),ADDEND,
     C NEWDEPT(251),ID(251),XNORMAL,NOLOG

      CHARACTER*12 DISTFILE
      CHARACTER*6 INFILE

      REAL COORD(251,2),PDIST,MAXVAL,MINVAL,TH,TEMPCELL,KEXP,
     C TCELLI,TCELLJ,FIT(251),ACTJ,POSSJ,ISTRESS(251),XSTRESS,
     C COMPA,COMPB,COMPC,COMPD,COMP2A,COMP2B,COMP2C,COMP2D,
     C ISTRESS2(251),ISTRESS3(251),COORD2(MATLEN,2),ODDSRATI,
     C COEFF,TOTDYAD,TEMPVAL,XMINVAL

      DOUBLE PRECISION STRESS,STRESS2,STRESS3,SCALEFAC,SQDIST,
     C SUMDIST

       INTEGER HUBERT(251,251),RESULTM(251,251),ALLGROUP(251,251),
     C NEWMAT(251,251)
       REAL BLAUC(251,251),DIFF(251,251),ZCOMPMAT(251,251)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF
        MINVAL=ABS(XMINVAL)
        NOLOG=0
        IF (XNORMAL .LT. 0) THEN
        NOLOG=1
        END IF
        NORMAL=ABS(XNORMAL)


      IF (PRINTO(43) .EQ. 1) THEN
      INFILE=DISTFILE(1:6)
      DISTFILE="compgrp.idis"
      OPEN(29,file=DISTFILE)

       IF (PRINTO(27) .EQ. 1) THEN
        CALL MOVEEND(29)
       END IF 
      END IF

      DO 18 I=1,MATLEN
       ISTRESS(I)=0
00018   CONTINUE
      
      TH=1
      IF ((MAXVAL .GT. 0) .AND. (NORMAL .EQ. 3)) THEN
      TH=MAXVAL
      END IF
       COMPA=0
       COMPB=0
       COMPC=0
       COMPD=0 
       COMP2A=0
       COMP2B=0
       COMP2C=0
       COMP2D=0 

       STRESS=0
       STRESS2=0
       SCALEFAC=0
       MEANDIST=0
       SQDIST=0
       SUMDIST=0
      
       CALL KFIT(MATLEN,FIT,ODDSRATI)
       OPEN(24,file="coeff")
       REWIND(24)
       COEFF=1
       READ(24,6261,END=8888) COEFF
08888  CLOSE(24)

       DO 2 I=2,MATLEN

       DO 3 J=1,(I-1.)
       PDIST=SQRT((COORD(I,1)-COORD(J,1))**2+
     C            (COORD(I,2)-COORD(J,2))**2)
       
       PDIST2=SQRT((COORD2(I,1)-COORD2(J,1))**2+
     C            (COORD2(I,2)-COORD2(J,2))**2)

       IF (PRINTO(43) .EQ. 1) THEN
       WRITE(29,443) INFILE,ID(I),ID(J),PDIST,PDIST2,
     C MATLEN,NEWDEPT(I),NEWDEPT(J)
       END IF
       IF (HUBERT(I,J) .EQ. 1) THEN

       IF (RESULTM(I,J) .EQ. 1) THEN
        COMPA=COMPA+1
        COMP2A=COMP2A+(MAXVAL-PDIST)*(MAXVAL-PDIST2)
       ELSE
        COMPB=COMPB+1
        COMP2B=COMP2B+PDIST*(MAXVAL-PDIST2)
       END IF
       ELSE
       IF (RESULTM(I,J) .EQ. 1) THEN
        COMPC=COMPC+1
        COMP2C=COMP2C+(MAXVAL-PDIST)*PDIST2
       ELSE
        COMPD=COMPD+1
        COMP2D=COMP2D+PDIST*PDIST2
       END IF
       END IF
      TEMPVAL=(NEWMAT(I,J)+NEWMAT(J,I))/2.0000

      IF ((NORMAL .GT. 3) .OR. (NORMAL .LT. 2)) THEN
      TEMPVAL=MAXVAL-TEMPVAL
      END IF
      IF ((NORMAL .EQ. 2) .OR. (NORMAL .EQ. 3)) THEN
      IF (TEMPVAL .LT. MINVAL) THEN 
      TEMPVAL=MINVAL
      END IF
      IF (NOLOG .EQ. 0) THEN
      TEMPVAL=LOG(TH)-LOG(TEMPVAL)
      ELSE
      TEMPVAL=TH/TEMPVAL 
      END IF
      END IF

       XSTRESS=(PDIST-TEMPVAL)**ABS(KEXP)
       SUMDIST=SUMDIST+PDIST
       SQDIST=SQDIST+PDIST**2
       STRESS=STRESS+XSTRESS
       ISTRESS(I)=ISTRESS(I)+XSTRESS/(MATLEN-1)
       ISTRESS(J)=ISTRESS(J)+XSTRESS/(MATLEN-1) 

00003  CONTINUE
00002   CONTINUE
        TOTDYAD=MATLEN*(MATLEN-1)/2
        SCALEFAC=SQDIST-(SUMDIST**2)/TOTDYAD
       
       STRESS=STRESS/SCALEFAC
       IF (COMPA .LT. 1) THEN
       COMPA=1
       END IF 
       IF (COMPB .LT. 1) THEN
       COMPB=1
       END IF 
       IF (COMPC .LT. 1) THEN
       COMPC=1
       END IF 
       IF (COMPD .LT. 1) THEN
       COMPD=1
       END IF 
       IF (COMP2A .LT. .01) THEN
       COMP2A=.01
       END IF 
       IF (COMP2B .LT. .01) THEN
       COMP2B=.01
       END IF 
       IF (COMP2C .LT. .01) THEN
       COMP2C=.01
       END IF 
       IF (COMP2D .LT. .01) THEN
       COMP2D=.01
       END IF 
       STRESS2=LOG(COMPA) +LOG(COMPD) - LOG(COMPC)-LOG(COMPB)
       STRESS3=LOG(COMP2A) +LOG(COMP2D) - LOG(COMP2C)-LOG(COMP2B)
C       STRESS=STRESS*2/(MATLEN*(MATLEN-1.))

       DO 22 I=1,MATLEN
       COMPA=0
       COMPD=0
       COMPC=0
       COMPD=0
       COMP2A=0
       COMP2D=0
       COMP2C=0
       COMP2D=0

       DO 23 J=1,MATLEN
       PDIST=SQRT((COORD(I,1)-COORD(J,1))**2+
     C            (COORD(I,2)-COORD(J,2))**2)
       PDIST2=SQRT((COORD2(I,1)-COORD2(J,1))**2+
     C            (COORD2(I,2)-COORD2(J,2))**2)
       IF (HUBERT(I,J) .EQ. 1) THEN
       IF (RESULTM(I,J) .EQ. 1) THEN
        COMPA=COMPA+1
        COMP2A=COMP2A+(MAXVAL-PDIST)*(MAXVAL-PDIST2)
       ELSE
        COMPB=COMPB+1
        COMP2B=COMP2B+PDIST*(MAXVAL-PDIST2)
       END IF
       ELSE
       IF (RESULTM(I,J) .EQ. 1) THEN
        COMPC=COMPC+1
        COMP2C=COMP2C+(MAXVAL-PDIST)*PDIST2
       ELSE
        COMPD=COMPD+1
        COMP2D=COMP2D+PDIST*PDIST2
       END IF
       END IF

00023  CONTINUE
       IF (COMPA .LT. 1) THEN
       COMPA=1
       END IF 
       IF (COMPB .LT. 1) THEN
       COMPB=1
       END IF 
       IF (COMPC .LT. 1) THEN
       COMPC=1
       END IF 
       IF (COMPD .LT. 1) THEN
       COMPD=1
       END IF 
       IF (COMP2A .LT. .01) THEN
       COMP2A=.01
       END IF 
       IF (COMP2B .LT. .01) THEN
       COMP2B=.01
       END IF 
       IF (COMP2C .LT. .01) THEN
       COMP2C=.01
       END IF 
       IF (COMP2D .LT. .01) THEN
       COMP2D=.01
       END IF 
       ISTRESS2(I)=LOG(COMPA) +LOG(COMPD) - LOG(COMPC)-LOG(COMPB)
       ISTRESS3(I)=LOG(COMP2A) +LOG(COMP2D) - LOG(COMP2C)-LOG(COMP2B)
00022   CONTINUE
      IF (PRINTO(43) .EQ. 1) THEN
      CLOSE(29)
      END IF

 6261  FORMAT(F10.5)
00251    FORMAT (2I10,2F10.5)
00443    FORMAT (A6,2I10,2F10.5,3I10)
       RETURN

       END

        SUBROUTINE KFIT(MATLEN,FIT,ODDSRATI)
       INTEGER I,J
       REAL FIT(251),POSSJ,ACTJ,ALPHA,BETA,GAMMA,DELTA,
     C ODDSRATI,TOTAL,TACTJ

       INTEGER HUBERT(251,251),RESULTM(251,251),ALLGROUP(251,251),
     C NEWMAT(251,251)
       REAL BLAUC(251,251),DIFF(251,251),ZCOMPMAT(251,251)
       COMMON ZCOMPMAT,ALLGROUP,HUBERT,RESULTM,NEWMAT,
     C BLAUC,DIFF

        TOTAL=MATLEN*(MATLEN-1.0)
        TACTJ=0
        XPOSSJ=0
        XACTJ=0
       DO 22 I=1,MATLEN
       POSSJ=0                  
       ACTJ=0
       DO 33 J=1,MATLEN
       IF (RESULTM(I,J) .EQ. 1) THEN
       TACTJ=TACTJ+1.0
       END IF
       IF (HUBERT(I,J) .EQ. 1) THEN
       POSSJ=POSSJ+1.0
       IF (RESULTM(I,J) .EQ. 1) THEN
       ACTJ=ACTJ+1.0
       END IF
       END IF
00033   CONTINUE
        XACTJ=XACTJ+ACTJ
        XPOSSJ=XPOSSJ+POSSJ
        FIT(I)=0
          IF ((ACTJ .GT. 0) .AND. (POSSJ .GT. 0)) THEN
         FIT(I)=ACTJ/POSSJ
          END IF
00022    CONTINUE

          BETA=TACTJ-XACTJ
          GAMMA=XPOSSJ-XACTJ
          ALPHA=TOTAL-BETA-GAMMA-XACTJ
          IF (BETA .LT. .5) THEN
          BETA=.5
          END IF
          IF (ALPHA .LT. .5) THEN
          ALPHA=.5
          END IF
          IF (GAMMA .LT. .5) THEN
          GAMMA=.5
          END IF
          IF (XACTJ .LT. .5) THEN
          XACTJ=.5
          END IF

          ODDSRATI=(XACTJ*ALPHA)/(BETA*GAMMA)
          IF (ODDSRATI .GT. 9999) THEN
          ODDSRATI=9999
          END IF
          RETURN
          END
      SUBROUTINE DIFFDIST(COORDA,COORDB,NUMTEACH,NUMDIM,
     C ISTRESS,TAD)
       REAL COORDA(251,2),COORDB(251,2),ISTRESS(251),TAD,TEMP
        INTEGER NUMTEACH,NUMDIM,I,J,K

        TAD=0
       DO 6 I=1,NUMTEACH
       ISTRESS(I)=0
00006   CONTINUE

       DO 2 I=2,NUMTEACH 
       DO 3 J=1,(I-1)
       DISTA=0
       DISTB=0
       DO 4 K=1,NUMDIM
       DISTA=DISTA+(COORDA(I,K)-COORDA(J,K))**2
       DISTB=DISTB+(COORDB(I,K)-COORDB(J,K))**2
00004   CONTINUE
       TEMP=(SQRT(DISTA)-SQRT(DISTB))**2
       ISTRESS(I)=ISTRESS(I)+TEMP/(NUMTEACH-1.000)
       ISTRESS(J)=ISTRESS(J)+TEMP/(NUMTEACH-1.000)
       TAD=TAD+TEMP
00003   CONTINUE
00002    CONTINUE
        TAD=TAD*2/(NUMTEACH*(NUMTEACH-1))
        RETURN
        END

      
       SUBROUTINE KMISS(NOWMISS,NUMTEACH,SSEED,TAKEOUT)
          INTEGER NUMTEACH,TAKEOUT(251),NREMOVE,
     C INLIST(251),I
          REAL NOWMISS
          DOUBLE PRECISION SSEED

          DO 21 I=1,NUMTEACH
          INLIST(I)=I
          TAKEOUT(I)=0
00021      CONTINUE
          CALL RESORTO (INLIST,NUMTEACH,SSEED)
          NREMOVE=INT(NOWMISS*NUMTEACH)

          DO 22 I=1,NREMOVE

          IF ((INLIST(I) .GT. 0) .AND. (INLIST(I) .LE. 251)) THEN
          TAKEOUT(INLIST(I))=1
          END IF

00022     CONTINUE
          RETURN
           END
    
        SUBROUTINE SAMPDIST(NUMTEACH,MAXCONN,SAMPMEAN,SAMPSD,CSMEAN,
     C   CSSD,ACTVAL,ZVAL,PVAL,CZVAL,CPVAL,PREDACC)
        INTEGER NUMTEACH,MAXCONN

        REAL SAMPMEAN,SAMPSD,CSMEAN,CSSD,ACTVAL,PVAL,ZVAL,CZVAL,CPVAL,
     C  TN,TM,PREDACC

        LOGICAL UPPER

        TN=NUMTEACH-66.51515
        TM=MAXCONN-9.30303
        SAMPMEAN=14.3463+.1881*TN-.2519*TM
        IF ((NUMTEACH .GE. 20) .AND. (NUMTEACH .LE. 80)) THEN
        SAMPMEAN=SAMPMEAN -.000414*TN**2
        IF ((MAXCONN .GE. 3) .AND. (MAXCONN .LE. 12)) THEN 
        SAMPMEAN=SAMPMEAN -.00946*TN*TM
        END IF
        END IF
        IF ((MAXCONN .GE. 3) .AND. (MAXCONN .LE. 12)) THEN 
         SAMPMEAN=SAMPMEAN + .008706*TM**2
        END IF
        SAMPSD=.91109+.00975*TN-.011579*TM
        ZVAL=(ACTVAL-SAMPMEAN)/SAMPSD
        UPPER=.FALSE.
        PVAL=ALNORM(ZVAL,UPPER)
        CSMEAN=SAMPMEAN+2*.319
        CSSD=SAMPSD+2*.0911
        CZVAL=(ACTVAL-CSMEAN)/CSSD
        UPPER=.FALSE.
        CPVAL=ALNORM(CZVAL,UPPER)
        PREDACC=.4572-.000176*TN -.00419*TM+.029787*ZVAL
        RETURN
        END 

        SUBROUTINE MATMUL(NUMTEACH,NEWMAT,NEWDEPT,OUTCOME,OUTMUL,COUNT)
        INTEGER NUMTEACH,NEWMAT(251,251),NEWDEPT(251),I,J,
     C COUNT(5,251)
        REAL OUTMUL(5,251),OUTCOME(251),ADDON
        
        DO 2 I=1,NUMTEACH
        IF (OUTCOME(I) .NE. -9999) THEN
          DO 4 J=1,5
          OUTMUL(J,I)=0
          COUNT(J,I)=0
00004      CONTINUE
       
        DO 3 J=1,NUMTEACH
        IF ((I .NE. J) .AND.  (OUTCOME(J) .NE. -9999)) THEN
        ADDON=NEWMAT(I,J)*OUTCOME(J)
        COUNT(3,I)=COUNT(3,I)+1
        OUTMUL(3,I)=OUTMUL(3,I)+ADDON
        IF (NEWDEPT(I) .EQ. NEWDEPT(J)) THEN
        IF (NEWMAT(I,J) .EQ. 0) THEN
        COUNT(4,I)=COUNT(4,I) +1
        OUTMUL(4,I)=OUTMUL(4,I)+OUTCOME(J)
         ELSE
        COUNT(5,I)=COUNT(5,I) +1
       OUTMUL(5,I)=OUTMUL(5,I)+OUTCOME(J) 
        END IF
        COUNT(1,I)=COUNT(1,I) +1
        OUTMUL(1,I)=OUTMUL(1,I)+ADDON
        ELSE
        COUNT(2,I)=COUNT(2,I) +1
        OUTMUL(2,I)=OUTMUL(2,I)+ADDON
        END IF
        END IF
00003    CONTINUE
        END IF
00002     CONTINUE
        RETURN
        END

      SUBROUTINE GGPER(QSEED,K,IPER)
      INTEGER K,IPER(251),SORTFLAG,IER,I
      DOUBLE PRECISION QSEED
      REAL ORDER(251),LOW,HIGH
  
      LOW=0
      HIGH=1

      IF (K .GT.  0) THEN
      DO 2 I=1,K
       ORDER(I)=GENUNF(LOW,HIGH)
00002  CONTINUE
      SORTFLAG=1
      CALL SPSORT(ORDER,K,IPER,SORTFLAG,IER)     
      END IF
      RETURN
      END
CSTART OF AS 91

 

      SUBROUTINE MDCH(CHI,DF,PVAL,IER)
      REAL CHI,PVAL,BOUND,RDF
      INTEGER DF,IER,ONE
      RDF=DF
      ONE=1
      IER=0
      IF ((CHI .GT. 0) .AND. (DF .GT. 0)) THEN
C      CALL CDFCHI(ONE,PVAL,CHI,RDF,IER,BOUND)
      PVAL=0
      ELSE
      PVAL=1
      END IF
      PVAL=999999
      RETURN
      END
      subroutine runsmac
c
c   This program tests the ability to call a FORTRAN program
c   from within another FORTRAN program.
c
c   Mark Riordan   22 Apr 94
c
      character *(*) prognm
      parameter (prognm='/home5/edstat/frank/cluster/develop/smacof1b')
      integer retcod
      call unlink( 'smacdist')
C      call unlink( 'coeff')
      retcod = system(prognm)

C      write(*,*) prognm,' returned code ',retcod
       RETURN
      end
      
       SUBROUTINE WRITDATA(INMAT,MAXG,SYMMAT)

       INTEGER BASEI,SYMMAT,MAXJ,MAXG,SI,SJ,STATUS,SYMLNK
       CHARACTER*32 NAME1/'smac.dat2'/,NAME2/'smac.data'/

       REAL INMAT(50,50),TM

       OPEN(62,FILE="smac.data")
       OPEN(63,FILE="group.dist")
       REWIND(62)
      BASEI=SYMMAT+1
       COUNT=0
      DO 6263 SI=BASEI,MAXG
       MAXJ=MAXG
       IF (SYMMAT .EQ. 1) THEN
        MAXJ=SI-1
       END IF
       DO 6262 SJ=1,MAXJ
       TM=INMAT(SI,SJ)
       IF (TM .LT. -99999) THEN
       TM=-99999
       END IF

       IF (TM .GT. 99999) THEN
       TM=99999
       END IF
       
       IF (SI .EQ. SJ) THEN
       TM=1
       END IF 
      WRITE(62,2469) TM
      COUNT=COUNT+1
      IF (COUNT .EQ. (MAXG -1)) THEN
       WRITE(62,2470)
       COUNT=0
      END IF 
      WRITE(63,2480) SI,SJ,TM
06262  CONTINUE
       IF (SYMMAT .NE. 1) THEN
       WRITE(62,2470)
        END IF
06263   CONTINUE
       WRITE(62,6767)
       REWIND(62) 
       CLOSE(UNIT=62)
       CLOSE(63)
       

C       CALL UNLINK(NAME2)
C       STATUS=SYMLNK(NAME1,NAME2)
       RETURN
 2480   FORMAT(2I10,F10.2)
 140  FORMAT(8X,6F12.7)
 2469 FORMAT(F9.2$)
 2470 FORMAT(F9.2)
 6767 FORMAT(20A8)
 6768 FORMAT(20A11)
 6261  FORMAT(F10.5)
       END

       SUBROUTINE WRITPARM(MAXG,SYMMAT,NUMDIM,ANUL)
       INTEGER MAXG,SYMMAT,NUMDIM,SJ,SI
       INTEGER DOSMAC,CI,SPARAM(5,10),NPAR(5)
       REAL ANUL

       CHARACTER*40 FORM(5)
       CHARACTER*8 OUTFORM
       INTEGER*2 LMAX
       OPEN(11,FILE="smac.parm")
       REWIND(11)
      SPARAM(2,1)=MAXG
      SPARAM(2,2)=1
      SPARAM(2,3)=1
      IF (SYMMAT .EQ. 1) THEN
      SPARAM(2,3)=-1
      END IF
      SPARAM(2,4)=0
      SPARAM(2,5)=0
      SPARAM(2,6)=0
      SPARAM(2,7)=0
      DO 59 SJ=1,7
      SPARAM(3,SJ)=0
00059  CONTINUE
       SPARAM(3,1)=2
       SPARAM(3,3)=2
       SPARAM(3,5)=1
       SPARAM(3,6)=1
       SPARAM(3,7)=1
       SPARAM(3,8)=3
       SPARAM(3,9)=8
        SPARAM(3,10)=0
       SPARAM(4,1)=NUMDIM
       SPARAM(4,2)=NUMDIM
       SPARAM(4,3)=0
       SPARAM(4,4)=0
       SPARAM(4,9)=ANUL
       DO 76 SJ=5,8
       SPARAM(4,SJ)=0
00076   CONTINUE
       SPARAM(5,1)=0
       SPARAM(5,2)=-2
       SPARAM(5,3)=0
       SPARAM(5,4)=2
       SPARAM(5,5)=0

       NPAR(2)=7
       NPAR(3)=10
       NPAR(4)=8
       NPAR(5)=5

       FORM(2)="(6I5,F10.1)"
       FORM(3)="(10I5)"
       FORM(4)="(6I5,2F10.1$)"
       FORM(5)="(5I5)"


      WRITE(11,6768) "KLIQUEFINDE"
      DO 85 SI=2,5
      WRITE(11,FORM(SI)) (SPARAM(SI,SJ) , SJ=1,NPAR(SI))
      IF (SI .EQ. 4) THEN
      WRITE(11,6271) ANUL
      END IF 
00085  CONTINUE
       LMAX=MAXG-1
      IF (LMAX .LT. 10) THEN
      WRITE(11,6759) "(",LMAX, "F9.2)"
      ELSE
      WRITE(11,6758) "(",LMAX, "F9.2)"
      END IF
C      WRITE(11,6767)
      REWIND(11)
      CLOSE(11)
      RETURN

 140  FORMAT(8X,6F12.7)
 2469 FORMAT(F5.2$)
 2470 FORMAT(F5.2)
 6759  FORMAT(A1,I1,A5)
 6758  FORMAT(A1,I2,A5)
 6767 FORMAT(20A8)
 6768 FORMAT(20A11)
 6261  FORMAT(F10.5)
 6271   FORMAT(F10.1)
       END
