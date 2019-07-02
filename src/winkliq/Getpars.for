
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

       INCLUDE 'PARAM.H'
       
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
       CHARACTER APRINT(MAXGR)*200,MATTYPE*1,INFILE*16,PRIORFIL*16,
     C PARMFILE*16,GRPTYPE*1,HITFILE*16,LABFILE*16,TITFILE*16,
     C TITLES(3)*20,FANCY(20)*200,PARTITLE(MAXGR)*200,PARFORM(MAXGR)*80,
     C LISTVAR(MAXGR)*100,TCHAR(MAXGR)*80
       DOUBLE PRECISION RASEED

        DO 88 P=1,4
        WRITE(6,100) FANCY(P)
00088    CONTINUE

       
       IF (NUMTEACH .LE. 1) THEN
       WRITE(6,100) 'You have submitted a file with no data'
       END IF
       IF (NUMOBS .LT. 2) THEN
       WRITE(6,100) 'You have submitted a file with less than 2'
       WRITE(6,100) 'non-zero data points.'
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
       WRITE(6,100) 'are taken from the file ''kliqfind.par'' .'
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
       Write(6,100) 'Distance'
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
       Write(6,100) 'Objective Function to be maximized:'
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

       Write(6,100) '1)Hubert''s Compactness'
       Write(6,100) '2)Pearson Goodness of Fit'
       Write(6,100) '3)Likelihood Ratio Criterion'
       WRITE(6,100) 'ENTER 1,2, OR 3'
       READ(5,102) QUANTYPE

       
       WRITE(6,100) 'Should the function be evaluated at the network'
       WRITE(6,100) 'ENTER Y OR N'

       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        NETLEV=1
        ELSE
        NETLEV=0
        END IF
       IF (NETLEV .EQ. 0) THEN
       WRITE(6,100) 'Should the function value in each group be '
       WRITE(6,100) 'averaged over the number of group members?'
       WRITE(6,100) 'ENTER Y OR N'

       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        PERGROUP=1
        ELSE
        PERGROUP=0
        END IF
       END IF

       WRITE(6,100) 'Should the function be squared?'       
       WRITE(6,100) 'ENTER Y OR N'

       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        SQUAREIT=1
        ELSE
        SQUAREIT=0
        END IF

       WRITE(6,100) 'Do you want to trim the highest valued weights?'
       WRITE(6,100) 'ENTER Y OR N'

       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        WRITE(6,100) 'What should the highest valued weight be?'
       WRITE(6,100) 'ENTER AN INTEGER'
       READ(5,102) REWEIGHT
        ELSE 
        REWEIGHT=0
        END IF
        END IF
C        DEFAULTS

       WRITE(6,104)
       WRITE(6,100) 'Initiating Groups'       
       WRITE(6,100) 'The defaults are:'
       STARTGRP = USETRIAD
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

       WRITE(6,100) 'Would you like the seed groups to be based on'       
       WRITE(6,100) '1)Best seeds based on matrix multiplication'       
       WRITE(6,100) '2)Random Assignments'       
       WRITE(6,100) '3)A priori assignments'       
       WRITE(6,100) 'ENTER 1,2, OR 3'
       READ(5,102) USETRIAD
       IF (USETRIAD .EQ. 2) THEN
        WRITE(6,100) 'What seed value should be used?'
       WRITE(6,100) 'ENTER A REAL VALUE'
       READ(5,*) RASEED
       END IF

       IF (USETRIAD .EQ. 1) THEN
       WRITE(6,100) 'Use Triads or Dyads?'       
       WRITE(6,100) 'ENTER T OR D'       
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'T') THEN
       DYDTRIAD=1
        ELSE
       DYDTRIAD=0
         END IF
       WRITE(6,100) 'Please enter the number of seed groups'
       WRITE(6,100) 'with which the algorithm should be started'
       WRITE(6,100) 'ENTER AN INTEGER'
       READ(5,102) NUMDYAD
       END IF
C       USETRIAD=1

        WRITE(6,100) 'Should actors be attached to groups before'       
       WRITE(6,100) 'proceeding with the formal ascent?'       
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       NOATTACH=0
        ELSE
       NOATTACH=1
        END IF

       IF (STRUCTEQ .NE. 1) THEN
       WRITE(6,104) 

       WRITE(6,100) 'Should the data be considered symmetric?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       SYMMAT=1
       ROWWT=1
       COLWT=1
        ELSE
       SYMMAT=0
C       WRITE(6,100) '  *****WARNING*****'
C       WRITE(6,100) 'Before assigning unequal weights to' 
C       WRITE(6,100) 'rows and columns you must consider'
C       WRITE(6,100) 'that the optimal move is based on '
C       WRITE(6,100) 'maximization of an objective function.'
C       WRITE(6,100) 'It is sensible that this maximization'
C       WRITE(6,100) 'should reflect changes across the entire network'
C       WRITE(6,100) ' as the result of a single move.  Thus,'
C       WRITE(6,100) ' discounting either the row or column'
C       WRITE(6,100) ' weight may not be consistent with the'
C       WRITE(6,100) ' idea of maximization of a function applied'
C       WRITE(6,100) 'to all the data.  '
C       WRITE(6,100) 'The result is that you should carefully'
C       WRITE(6,100) ' moniter and limit the number of iterations'
C       WRITE(6,100) 'if you choose to have unequal weights'
C       WRITE(6,100) 'for rows and columns.  Convergence is NOT'
C       WRITE(6,100) 'cetrain!'



       WRITE(6,100) 'How much weight should be applied to '       
       WRITE(6,100) 'the row data'       
       WRITE(6,100) 'ENTER A REAL VALUE'       
       READ(5,*) ROWWT

       WRITE(6,100) 'How much weight should be applied to '       
       WRITE(6,100) 'the column data'       
       WRITE(6,100) 'ENTER A REAL VALUE'       
       READ(5,*) COLWT
       ONE=1
       WRITE(6,200) 'Which probability model should be used?'
       WRITE(6,200) 'ENTER 1 FOR BINOMIAL, 2 FOR HYPERGEOMETRIC'
       READ(5,*) HYPERG
       HYPERG=HYPERG-ONE

       WRITE(6,100) 'Should the transpose of the raw data be used?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       TRANSPOS=1
        ELSE
       TRANSPOS=0
        END IF
C         FOR TRANSPOSE

       WRITE(6,100) 'Do the rows and columns of your matrix'
       WRITE(6,100) 'represent identical attributes (ACTORS)?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        RECTMAT=0
        ELSE
       WRITE(6,100) 'Do you want to work with X''X OR XX'' ?'
       WRITE(6,100) 'ENTER 1 FOR X''X OR'
       WRITE(6,100)'       2 FOR XX'''
       READ (5,*) RECTMAT
        END IF
       WRITE(6,100) 'Do you want to standardize weights by the '
       WRITE(6,100) 'row marginals ?'
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        GUSEMARG=1
        ELSE
        GUSEMARG=0
        END IF


       WRITE(6,100) 'Should the weights be inverted?'
       WRITE(6,100) 'ENTER Y OR N'
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

       WRITE(6,100) 'Should actors who are connected to only one other'
       write(6,100) 'actor in the network (tagalongs) be removed and'
       write(6,100) ' assigned to the group of the actor with whom '
       write(6,100)  'they are connected?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       TAGALONG=1
        ELSE
       TAGALONG=0
        END IF
       END IF
C        DEFAULTS

       WRITE(6,104)

       WRITE(6,100) 'Finding best Groups'       
       WRITE(6,100) 'The defaults are:'
       WRITE(6,200) 'DIRECT',DIRECT
       WRITE(6,200) 'THRESHT',THRESHT
       WRITE(6,200) 'LOOKT',LOOKT
       WRITE(6,200) 'MAXSEED',MAXSEED
       WRITE(6,100) 'Accept the defaults?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN


       WRITE(6,100) 'When establishing seed groups OR when'       
       WRITE(6,100) 'finding a new group during the algorithm,'        
       WRITE(6,100) 'matrix multiplication is used to find the'       
       WRITE(6,100) 'best groups'       
       
       WRITE(6,100) 'How much weight should be assigned to'       
       WRITE(6,100) 'direct versus indirect connections'       
       READ (5,*) DIRECT

       WRITE(6,100) 'What is the threshold of similarity for looking'       
       WRITE(6,100) 'at a triad (triads with below this threshold will'       
       WRITE(6,100) 'not be considered for seed dyads (or triads)'
       WRITE(6,100) 'ENTER A REAL VALUE (2.000 SEEMS TO WORK)'       
       READ (5,*) THRESHT
       
       WRITE(6,100) 'What is the maximum number of seeds?'

       WRITE(6,100) 'ENTER A REAL VALUE (BETWEEN 0 AND 9999)'       
       READ(5,*) LOOKT
       WRITE(6,100) 'How many times can a given actor be used as'
       WRITE(6,100) ' a seed for a subgroup?'
       WRITE(6,100) 'ENTER AN INTEGER '
       READ (5,*) MAXSEED

       END IF
C       DEFAULTS

       WRITE(6,104) 
        WRITE(6,100) 'Determining Proximity for Group Assignment'       
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
       WRITE(6,100) 'When assigning an actor to a group, how'       
       WRITE(6,100) 'large must the baseline standardized measure of '       
       WRITE(6,100) 'association be for the group assignment to'       
       WRITE(6,100) 'occur?'       
       WRITE(6,100) 'ENTER A REAL VALUE'
       READ(5,*) NEARVAL
       WRITE(6,100) 'Should this value be determined as a '       
       WRITE(6,100) 'percentile of the proximites of actors'       
       WRITE(6,100) 'currently assigned to groups?'       
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       WRITE(6,100) 'What should that percentile be?'       
       WRITE(6,100) 'ENTER A REAL VALUE (BETWEEN 0 AND 1)'       
       READ(5,*) PCTILE
       ELSE
       PCTILE=1.5
       END IF
       WRITE(6,104)
        WRITE(6,100) 'Should dyads be built if only one actor'
        WRITE(6,100) 'initiates connections across the network?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       MUTDYAD=0
        ELSE
       MUTDYAD=1
        END IF
        WRITE(6,100) 'Should assignments which result in a negative'
        WRITE(6,100) ' contribution to the objective function be '
        WRITE(6,100) ' invalidated?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       NONEG=1
        ELSE
       NONEG=0
        END IF
        WRITE(6,100) 'Should difference between dyad and isolate'
        WRITE(6,100) 'be halved when considering an actor''s '
        WRITE(6,100) ' removal?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       HALFDYAD=1
        ELSE
       HALFDYAD=0
        END IF
        WRITE(6,100) 'Groups below what size should be dissolved?'
       WRITE(6,100) 'ENTER AN INTEGER'
       READ (5,*) DISSOLVE

       WRITE(6,104)
       END IF
C       DEFAULTS

       
       WRITE(6,100) 'Convergence'              
       WRITE(6,100) 'The defaults are:'
       WRITE(6,200) 'QUICKEND',QUICKEND
       WRITE(6,200) 'STOPVAL',STOPVAL
       WRITE(6,200) 'KCOUNT2',KCOUNT2
       WRITE(6,200) 'ATTACHI',ATTACHI
       WRITE(6,100) 'Accept the defaults?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN
       WRITE(6,100) 'Would you like quick convergence,'       
       WRITE(6,100) 'Just to get output from a priori groups?'              
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        PRINTO(21)=0
        QUICKEND=1
        ELSE
        QUICKEND=0
        WRITE(6,100) 'What should the convergence criteria be?'       
       WRITE(6,100) 'That is, after a change results in less than'
       WRITE(6,100) 'this value, the ascent stops'       
       WRITE(6,100) 'ENTER A REAL VALUE'
       READ(5,*) STOPVAL

       WRITE(6,100) 'After How many iterations should the ascent stop' 
       WRITE(6,100) 'If it has not yet converged?'       
       WRITE(6,100) 'ENTER AN INTEGER'              
       READ(5,*) KCOUNT2
       WRITE(6,100) 'Should isolates be attached after ascent,'
       WRITE(6,100) 'and then a final ascent executed?'
       WRITE(6,100) 'ENTER Y OR N'
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
       WRITE(6,100) 'Monte Carlo (Plus) Evaluation'              
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
       WRITE(6,100) 'How many observations should be used?'       
       WRITE(6,100) 'ENTER AN INTEGER'              
       READ(5,*) NEVAL
       WRITE(6,100) 'Starting how low?'
       WRITE(6,100) 'ENTER A REAL VALUE (BETWEEN 0 AND 1)'
       READ(5,*) BASEVAL

       WRITE(6,100) 'Ending how high?'
       WRITE(6,100) 'ENTER A REAL VALUE (BETWEEN 0 AND 1)'
       READ(5,*) TOPVAL
       WRITE(6,100) 'Should it be possible to create new groups'
       WRITE(6,100) 'when rival solutions are being generated?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        NEWGRPS=0
        ELSE
        NEWGRPS=1
        END IF
       WRITE(6,100) 'How Much More Weight on Largest # of '
       WRITE(6,100) 'Reassignments compared with smallest ?'
       WRITE(6,100) 'ENTER A REAL VALUE '
       READ(5,*) HIWTEVAL


       END IF
C       DEFAULTS
       IF (NEVAL .GT. 0) THEN
       WRITE(6,100) 'Would you like to use the simulation for'
       WRITE(6,100) 'obtaining a second solution?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        NUMRES=1
        END IF
       END IF

  
       WRITE(6,104) 

       WRITE(6,100) 'Boundary Spanners'
       WRITE(6,100) 'The defaults are:'
       WRITE(6,200) 'BOUNDVAL',BOUNDVAL
       WRITE(6,200) 'BLABOUND',BLABOUND
       WRITE(6,200) 'FIXR',FIXR
        WRITE(6,200) 'BETWMULT',BETWMULT
       WRITE(6,100) 'Accept the defaults?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN
       WRITE(6,100) 'What is the cut-off value for boundary spanners?'
       WRITE(6,100) 'ENTER A REAL VALUE '
       READ(5,*) BOUNDVAL
       WRITE(6,100) 'What is the cut-off value for Blau boundary'
       WRITE(6,100) ' spanners?'
       WRITE(6,100) 'ENTER A REAL VALUE '
       READ(5,*) BLABOUND

       WRITE(6,100) 'What is the multiplier for betweenness?'
       WRITE(6,100) 'ENTER A REAL VALUE '
       READ(5,*) BETWMULT
       WRITE(6,100) 'Was there a fixed number of connections '
       WRITE(6,100) 'each actor could initiate?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN
        FIXR=99999
        ELSE
       WRITE(6,100) 'What was the maximum number of connections'
       WRITE(6,100) 'an actor could initiate?'
       WRITE(6,100) 'ENTER A REAL VALUE '
       READ(5,*) FIXR
         END IF
       END IF
C       ACCEP THE DEFAULTS ON BOUNDARY SPANNERS

       WRITE(6,100) 'Anchored MDS plot'
       WRITE(6,100) 'Between Groups'
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

       WRITE(6,100) 'What should is the ratio of within group'
       WRITE(6,100) 'distances to between group distances?'
       WRITE(6,100) 'ENTER A REAL VALUE'
       READ(5,*) IGRATIO
       WRITE(6,100) 'How many dimensions should the plot be in?'
       WRITE(6,100) 'NOTE THAT THE CURRENT VERSION OF THE PROGRAM'

       WRITE(6,100) 'CAN PLOT ONLY IN TWO DIMENSIONS'
       WRITE(6,100) 'I AM ASSIGNING NUMDIM=2'
         NUMDIM=2
       WRITE(6,100) 'What is the minimum distance which '
       WRITE(6,100) 'separates two actors?'
       WRITE(6,100) 'ENTER A REAL VALUE AS A PERCENTAGE OF THE'
       WRITE(6,100) 'THE MAXIMUM VALUE IN A GRAPH'
       READ(5,*) MAKEPIC

       WRITE(6,100) 'Should the points be recentered about the group'
       WRITE(6,100) 'coordinates?'
       WRITE(6,100) 'ENTER Y or N'
       READ(5,*) INDICATE
        IF (INDICAT(1:1) .EQ. 'N') THEN
        CENTER=0
        ELSE
        CENTER=1
        END IF
       WRITE(6,100) 'Which group should be used for the anchoring?'
       WRITE(6,100) 'ENTER THE NUMBER OF THE GROUP IN '
       WRITE(6,100) 'TERMS OF ITS ORDER IN CENTRALITY'
       WRITE(6,100) 'A VALUE GREATER THAN THE NUMBER OF POINTS'
       WRITE(6,100) 'WILL BE CONVERTED TO THE MAXIMUM.'
       WRITE(6,100) 'A VALUE OF ZERO WILL INDICATE A RANDOM'
       WRITE(6,100) 'CHOICE, A VALUE OF LESS THAN 0 WILL'
       WRITE(6,100) 'INDICATE THAT PERCENTILES (I.E. MEDIANS,'
       WRITE(6,100) 'QUARTILES, ETC) SHOULD BE USED TO '
       WRITE(6,100) 'ESTABLISH THE MEASURE OF CENTRALITY'
       READ(5,*) DANCHOR
       IF (DANCHOR .LT. 0) THEN
       WRITE(6,100) 'What percentiel should be used?'
       WRITE(6,100) 'ENTER A REAL VALUE BETWEEN 0 AND 1'
       READ(5,*) PCTCENG1
       END IF
       WRITE(6,100) 'Which group should be used for the second'
       WRITE(6,100) 'anchor?'
       WRITE(6,100) 'ENTER THE NUMBER OF THE GROUP IN '
       WRITE(6,100) 'TERMS OF IT''S ORDER IN CENTRALITY'
       WRITE(6,100) 'A VALUE GREATER THAN THE NUMBER OF POINTS'
       WRITE(6,100) 'WILL BE CONVERTED TO THE MAXIMUM.'
       WRITE(6,100) 'A VALUE LESS THAN 1 WILL INDICATE A RANDOM'
       WRITE(6,100) 'CHOICE, A VALUE OF LESS THAN 0 WILL'
       WRITE(6,100) 'INDICATE THAT PERCENTILES (I.E. MEDIANS,'
       WRITE(6,100) 'QUARTILES, ETC) SHOULD BE USED TO '
       WRITE(6,100) 'ESTABLISH THE MEASURE OF CENTRALITY'
       READ(5,*) DANCHOR2
       IF (DANCHOR .LT. 0) THEN
       WRITE(6,100) 'What percentiel should be used?'
       WRITE(6,100) 'ENTER A REAL VALUE BETWEEN 0 AND 1'
       READ(5,*) PCTCENG2
       END IF
       WRITE(6,100) 'Should the second anchor be repositioned?'
       WRITE(6,100) 'ENTER Y OR N'
       READ(5,*) INDICAT
       IF (INDICAT .EQ. 'Y') THEN
       MOVE2=1
         ELSE
        MOVE2=0
       END IF

       WRITE(6,100) 'Should the relations between points be'
       WRITE(6,100) 'considered symmetric?'
       WRITE(6,100) 'ENTER Y OR N'
       READ(5,*) INDICAT
       IF (INDICAT .EQ. 'Y') THEN
       ZSYMMAT=1
         ELSE
       ZSYMMAT=0
       END IF

       WRITE(6,100) 'Into how many increments should the circle'
       WRITE(6,100) 'be divided to start?'
       WRITE(6,100) 'ENTER AN INTEGER'
       READ(5,*) STARTINC

       WRITE(6,100) 'How many increments should be added at'
       WRITE(6,100) 'each iteration?'
       WRITE(6,100) 'ENTER AN INTEGER'
       READ(5,*) BYINC

       WRITE(6,100) 'What is the maximal number of increments'
       WRITE(6,100) 'into which the circle can be be divided start?'
       WRITE(6,100) 'ENTER AN INTEGER'
       READ(5,*) MAXINC

       WRITE(6,100) 'What should the exponent be in determining'
       WRITE(6,100) 'distances?'
       WRITE(6,100) 'ENTER A REAL VALUE, 0 --> LOG,'
       WRITE(6,100) 'NEGATIVE NUMBER --> ABSOLUTE VALUE'
       READ(5,*) KEXP

       WRITE(6,100) 'Should between group non-radius distances be'
       WRITE(6,100) 'normalized?'
       WRITE(6,100) 'ENTER 0 FOR NO'
       WRITE(6,100) '1 FOR YES,'
       WRITE(6,100) '2 FOR TAKING LOGS,'
       WRITE(6,100) '3 FOR TAKING LOG(MAX) - LOG(VAL)'
       READ(5,*) NORMAL
       IF (NORMAL .GE. 2) THEN
       WRITE(6,100) 'What is the minimum value for a between group'
       WRITE(6,100) 'Element?'
       WRITE(6,100) 'ENTER A REAL VALUE'
       READ(5,*) MINVALG
       END IF
       WRITE(6,100) 'Should the scaling be based on angles or'
       WRITE(6,100) 'Euclidean distances?'
       WRITE(6,100) 'ENTER 1 FOR ANGLES, 0 FOR DISTANCES'
       READ(5,*) BYANGLE
       WRITE(6,100) 'Should the distances be scaled by'
       WRITE(6,100) 'the radius of the relevant actors?'
       WRITE(6,100) 'ENTER 1 FOR SCALED, 0 FOR NOT SCALED'
       READ(5,*) BYSCALE
       WRITE(6,100) 'By what nymber should the radius be divided?'
       WRITE(6,100) 'ENTER A REAL VALUE'
       READ(5,*) DRADIUSG


       END IF
C      ACCEPT THE DEFAULTS BETWEEN GROUPS

       WRITE(6,100) 'Within Groups'
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

       WRITE(6,100) 'Should the points be recentered about the group'
       WRITE(6,100) 'coordinates?'
       WRITE(6,100) 'ENTER Y or N'
       READ(5,*) INDICATE
        IF (INDICAT(1:1) .EQ. 'N') THEN
        CENTERI=0
        ELSE
        CENTERI=1
        END IF
       WRITE(6,100) 'Which actor should be used for the anchoring?'
       WRITE(6,100) 'ENTER THE NUMBER OF THE ACTOR IN '
       WRITE(6,100) 'TERMS OF IT''S ORDER IN CENTRALITY'
       WRITE(6,100) 'A VALUE GREATER THAN THE NUMBER OF POINTS'
       WRITE(6,100) 'WILL BE CONVERTED TO THE MAXIMUM.'
       WRITE(6,100) 'A VALUE LESS THAN 1 WILL INDICATE A RANDOM'
       WRITE(6,100) 'CHOICE, A VALUE OF LESS THAN 0 WILL'
       WRITE(6,100) 'INDICATE THAT PERCENTILES (I.E. MEDIANS,'
       WRITE(6,100) 'QUARTILES, ETC) SHOULD BE USED TO '
       WRITE(6,100) 'ESTABLISH THE MEASURE OF CENTRALITY'
       READ(5,*) DANCHORI
       IF (DANCHOR .LT. 0) THEN
       WRITE(6,100) 'What percentiel should be used?'
       WRITE(6,100) 'ENTER A REAL VALUE BETWEEN 0 AND 1'
       READ(5,*) PCTCENI1
       END IF
       WRITE(6,100) 'Which group should be used for the second'
       WRITE(6,100) 'anchor?'
       WRITE(6,100) 'ENTER THE NUMBER OF THE GROUP IN '
       WRITE(6,100) 'TERMS OF IT''S ORDER IN CENTRALITY'
       WRITE(6,100) 'A VALUE GREATER THAN THE NUMBER OF POINTS'
       WRITE(6,100) 'WILL BE CONVERTED TO THE MAXIMUM.'
       WRITE(6,100) 'A VALUE LESS THAN 1 WILL INDICATE A RANDOM'
       WRITE(6,100) 'CHOICE, A VALUE OF LESS THAN 0 WILL'
       WRITE(6,100) 'INDICATE THAT PERCENTILES (I.E. MEDIANS,'
       WRITE(6,100) 'QUARTILES, ETC) SHOULD BE USED TO '
       WRITE(6,100) 'ESTABLISH THE MEASURE OF CENTRALITY'
       READ(5,*) DANCH2I
       IF (DANCHOR .LT. 0) THEN
       WRITE(6,100) 'What percentiel should be used?'
       WRITE(6,100) 'ENTER A REAL VALUE BETWEEN 0 AND 1'
       READ(5,*) PCTCENI2
       END IF
       WRITE(6,100) 'Should the second anchor be repositioned?'
       WRITE(6,100) 'ENTER Y OR N'
       READ(5,*) INDICAT
       IF (INDICAT .EQ. 'Y') THEN
       MOVE2I=1
         ELSE
        MOVE2I=0
       END IF

       WRITE(6,100) 'Should the relations between points be'
       WRITE(6,100) 'considered symmetric?'
       WRITE(6,100) 'ENTER Y OR N'
       READ(5,*) INDICAT
       IF (INDICAT .EQ. 'Y') THEN
       ZSYMMATI=1
         ELSE
       ZSYMMATI=0
       END IF

       WRITE(6,100) 'Into how many increments should the circle'
       WRITE(6,100) 'be divided to start?'
       WRITE(6,100) 'ENTER AN INTEGER'
       READ(5,*) STARTINI

       WRITE(6,100) 'How many increments should be added at'
       WRITE(6,100) 'each iteration?'
       WRITE(6,100) 'ENTER AN INTEGER'
       READ(5,*) BYINCI

       WRITE(6,100) 'What is the maximal number of increments'
       WRITE(6,100) 'into which the circle can be be divided start?'
       WRITE(6,100) 'ENTER AN INTEGER'
       READ(5,*) MAXINCI

       WRITE(6,100) 'What should the exponent be in determining'
       WRITE(6,100) 'distances?'
       WRITE(6,100) 'ENTER A REAL VALUE, 0 --> LOG,'
       WRITE(6,100) 'NEGATIVE NUMBER --> ABSOLUTE VALUE'
       READ(5,*) KEXPI

       WRITE(6,100) 'Should within group non-radius distances be'
       WRITE(6,100) 'normalized?'
       WRITE(6,100) 'ENTER 0 FOR NO'
       WRITE(6,100) '1 FOR YES,'
       WRITE(6,100) '2 FOR TAKING LOGS,'
       WRITE(6,100) '3 FOR TAKING LOG(MAX) - LOG(VAL)'
       READ(5,*) NORMALI
       IF (NORMAL .GE. 2) THEN
       WRITE(6,100) 'What is the minimum value for a within group'
       WRITE(6,100) 'Element?'
       WRITE(6,100) 'ENTER A REAL VALUE'
       READ(5,*) MINVALI
       END IF
       WRITE(6,100) 'Should the scaling be based on angles or'
       WRITE(6,100) 'Euclidean distances?'
       WRITE(6,100) 'ENTER 1 FOR ANGLES, 0 FOR DISTANCES'
       READ(5,*) BYANGLEI
       WRITE(6,100) 'Should the distances be scaled by'
       WRITE(6,100) 'the radius of the relevant actors?'
       WRITE(6,100) 'ENTER 1 FOR SCALED, 0 FOR NOT SCALED'
       READ(5,*) BYSCALEI
       WRITE(6,100) 'By what nymber should the radius be divided?'
       WRITE(6,100) 'ENTER A REAL VALUE'
       READ(5,*) DRADIUSI

       END IF
C       ACCEPT THE DEFAULTS


       WRITE(6,100) 'Rotating the Circles:'
       WRITE(6,200) 'RINCREM',RINCREM
       WRITE(6,200) 'MEASURE',MEASURE
       WRITE(6,200) 'EXTREME',EXTREME
       WRITE(6,100) 'Accept the defaults?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN

       WRITE(6,100) 'How many increments should be considered in '
       WRITE(6,100) 'rotating the groups?'
       WRITE(6,100) 'ENTER AN INTEGER'
       READ(5,*) RINCREM
       WRITE(6,100) 'Which measure should be used as a basis'
       WRITE(6,100) 'of association between actors and other'
       WRITE(6,100) 'subgroups?'
       WRITE(6,100) 'ENTER 1 FOR COMPACTNESS, 2 FOR DENSITY'
       READ(5,*) MEASURE
       WRITE(6,100) 'Should actors with a larger radius be'
       WRITE(6,100) 'given more weight in determining the rotation?'
       WRITE(6,100) 'ENTER Y OR N'
       READ(5,*) INDICAT
       IF (INDICAT .EQ. 'Y') THEN
       EXTREME=1
         ELSE
       EXTREME=0
       END IF

       END IF
C       ACCEPT THE DEFAULTS ON ROTATION

       END IF
       

       WRITE(6,104)
       WRITE(6,100) 'Would you like these parameters saved to a file?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        WRITE(6,100) 'ENTER A LEGITIMATE FILENAME BETWEEN 1 AND 16'
        WRITE(6,100) 'CHARACTERS'
        READ (5,*) PARMFILE
        OPEN(77,file=PARMFILE)
        WRITE(6,100) 'Note that you may use these parameters for'
        WRITE(6,100) 'other runs on the same or different data'
        WRITE(6,100) 'by placing the contents of '
        WRITE(6,100) PARMFILE
        WRITE(6,100) 'into ''kliqfind.par'' .'
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
      WRITE(77,PARFORM(I)) TRANSPOS,REWEIGHT,SYMMAT,INVERT,RECTMAT,
     C GUSEMARG,TAGALONG
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
C      WRITE(77,100) ' '
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
       WRITE(6,100) 'Execute a clustering:'
       INDICAT(1:1) ='N'
       IF (MATTYPE .EQ. 'p') THEN
       WRITE(6,100) 'Your current data file is',INFILE
       WRITE(6,205) 'It contains ',NUMTEACH
       WRITE(6,205) 'observations, in ',MAXDEPT
       WRITE(6,100) 'groups. given in ',NUMOBS
       WRITE(6,100) 'non-zero observations.'
       WRITE(6,100) 'It is assumed that the data are in matrix'
       WRITE(6,100) 'format and that the a priori groups come'
       WRITE(6,100) 'from this file.  If you do not wish to make'
       WRITE(6,100) 'these assumptions or if you do not wish'
       WRITE(6,100) 'to continue with this file then respond'
       WRITE(6,100) '''N'' to the next question:'
       WRITE(6,100) 'Would you like to continue with this file?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
       END IF
      
        IF (INDICAT(1:1) .EQ. 'Y') THEN
         GCONTIN=1
         RETURN
        ELSE
       WRITE(6,100) 'Would you like to cluster some data?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN
        CALL MYSTOP
         ELSE
       WRITE(6,100) 'Which file contains the data?'
        WRITE(6,100) 'ENTER A LEGITIMATE FILENAME BETWEEN 1 AND 16'
       READ(5,*) INFILE
       WRITE(6,100) '**********************'              
       WRITE(6,100) 'FORMAT OF DATA FILE'
       WRITE(6,100) 'If the data are in list format, a typical'
       WRITE(6,100) 'line would look like this:'
       WRITE(6,100)
       WRITE(6,100) 'Choser ID Chosen ID     Weight '
       WRITE(6,100) '123456789012345678901234567890'
       WRITE(6,100)
       WRITE(6,100) 'That is, in format 3I10.'
       WRITE(6,100) 'For example, the line:'
       WRITE(6,100)
       WRITE(6,100) '         1         3         6'
       WRITE(6,100) '123456789012345678901234567890'
       WRITE(6,100)
       WRITE(6,100) 'indicates that actor 1 directs a connection to'
       WRITE(6,100) 'actor 3 with a value of 6.'
       WRITE(6,100)
       WRITE(6,100) 'ENTER ANY NUMBER TO CONTINUE'
       READ(5,*) TEMP

        WRITE(6,100) 'A priori placements of actors in groups'
      WRITE(6,100) 'Are indicated by assigning the ''directed to'' ID'
       WRITE(6,100) 'equal to 99999.  For example, the line'
       WRITE(6,100)
       WRITE(6,100) '         1     99999         4'
       WRITE(6,100) '123456789012345678901234567890'
       WRITE(6,100)
       WRITE(6,100) 'indicates that actor 1 is in a priori group 4.'
       WRITE(6,100)
       WRITE(6,100) 'ENTER ANY NUMBER TO CONTINUE'
       READ(5,*) TEMP
        WRITE(6,100) '**********************'              


       WRITE(6,100) 'In matrix format, the above would'
       WRITE(6,100) 'be represented by the first row of the matrix'
       WRITE(6,100) 'which would look like the following:'
       WRITE(6,100)
       WRITE(6,100) ' 40060000000000000'
       WRITE(6,100) 'XX1234567890123456'
       WRITE(6,100)
       WRITE(6,100) 'the first two columns indicate'
       WRITE(6,100) 'the apriori placement of the actor. Actor'
       WRITE(6,100) '1 is in group 4.  In the following columns'
       WRITE(6,100) 'there is one column allocated for the'
       WRITE(6,100) 'connection directed by the row actor'
       WRITE(6,100) 'to the actor represented in each column.'
       WRITE(6,100) 'In this case we see that actor 1'
       WRITE(6,100) '(because the row given above is the first row'
       WRITE(6,100) 'in the matrix)'
       WRITE(6,100) 'initiates a connection to actor 3 with a value'
       WRITE(6,100) 'of 6 and directs 0 valued connections to all'
       WRITE(6,100) 'other actors.'
       WRITE(6,100)
       WRITE(6,100) 'Are the data in list or matrix format?'

       WRITE(6,100) 'ENTER l OR m (use lower case)'
       READ(5,*) MATTYPE
       WRITE(6,100)
       WRITE(6,100) 'Is there another file with grouping information?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        IF (MATTYPE .EQ. 'l') THEN 
        MATTYPE='b'
        ELSE
        MATTYPE='g'
        END IF
       WRITE(6,100) 'What is the name of that file?'
       WRITE(6,100) 'ENTER A LEGITIMATE FILENAME BETWEEN 1 AND 16'
       WRITE(6,100) 'CHARACTERS'
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
       
       WRITE(6,100) 'Is there another file with hitlist information?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       WRITE(6,100) 'What is the name of that file?'
       WRITE(6,100) 'ENTER A LEGITIMATE FILENAME BETWEEN 1 AND 16'
       WRITE(6,100) 'CHARACTERS'
       READ(5,*) HITFILE
        ELSE
       NUMHLIST=0
        END IF

       WRITE(6,100) 'Is there another file with labels information?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       WRITE(6,100) 'What is the name of that file?'
       WRITE(6,100) 'ENTER A LEGITIMATE FILENAME BETWEEN 1 AND 16'
       WRITE(6,100) 'CHARACTERS'
       READ(5,*) LABFILE
        END IF

       WRITE(6,100) 'Is there another file with titles information?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
       WRITE(6,100) 'What is the name of that file?'
       WRITE(6,100) 'ENTER A LEGITIMATE FILENAME BETWEEN 1 AND 16'
       WRITE(6,100) 'CHARACTERS'
       READ(5,*) TITFILE
       WRITE(6,100) 'Would you like to enter the titles'
         WRITE(6,100) ' interactively?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'Y') THEN
        DO 99 I=1,3
       WRITE(6,201) 'INPUT TITLE',I
       READ(5,*) TITLES(I)
00099 CONTINUE
        END IF

         END IF
C        DEFAULTS

       WRITE(6,104)
       WRITE(6,100) 'Control of output'
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
       WRITE(6,100) 'For each of the following pieces of output'
       WRITE(6,100) 'ENTER A 1 IF YOU WOULD LIKE THAT OUTPUT,'
       WRITE(6,100) 'A 0 OTHERWISE'
       DO 3 P=1,NPO
       WRITE(6,100) APRINT(P)
       READ(5,*) PRINTO(P)
00003   CONTINUE
        END IF
C       DEFAULTS
        END IF
        END IF
        END IF
        WRITE(6,100) 'In the future, to run the program, you must'
        WRITE(6,100) 'type:'
        WRITE(6,100) 'kliquefind datafile [option] placements'
        WRITE(6,100) 'Where option = ''l'' if data are in list '
        WRITE(6,100) 'format, ''g'' if you want a priori placements'
        WRITE(6,100) 'to come from a second file (placements)'
        WRITE(6,100) 'and ''b'' if data are in list format and you'
        WRITE(6,100) 'have data in a second file for a priori groups.'
        WRITE(6,100)
       WRITE(6,100) 'Would you like to see examples?'
       WRITE(6,100) 'ENTER Y OR N'
       READ (5,*) INDICAT
        IF (INDICAT(1:1) .EQ. 'N') THEN

        WRITE(6,100)  'For example:'
        WRITE(6,100)  'kliqfind sampson.mat'
        WRITE(6,100)
        WRITE(6,100)  'Tells kliquefinder to use matrix data from '
        WRITE(6,100)  'sampson.mat'
        WRITE(6,100)
        WRITE(6,100)   'But if the data are in list format:'
        WRITE(6,100)   'kliqfind sampson.list l'
        WRITE(6,100)  
        WRITE(6,100)  'Would be the appropriate command.'
        WRITE(6,100)  
        WRITE(6,100) 'Note that it is not necessary to use the '
        WRITE(6,100) '''.list'' suffix for files in list format'
        WRITE(6,100) 'and the ''.mat'' suffix for files in matrix'
        WRITE(6,100) 'format, although it might help you to do so.'
        WRITE(6,100)

       WRITE(6,100) 'ENTER ANY NUMBER TO CONTINUE'
       READ(5,*) TEMP
        WRITE(6,100) '**********************'              

        WRITE(6,100) 'If you wanted to use a priori groups in a file'
        WRITE(6,100) 'called: ''sampson.place'' then you would type:'
        WRITE(6,100) 
        WRITE(6,100) 'kliqfind sampson.mat g sampson.place'
        WRITE(6,100)
        WRITE(6,100)  'If the data were in list format *and* you'
        WRITE(6,100) 'wanted to use the groups in sampson.place,'
        WRITE(6,100) 'then you would type:'
        WRITE(6,100)
        WRITE(6,100) 'kliqfind sampson.list b sampson.place'
        WRITE(6,100)
        WRITE(6,100) 'Use the option ''p'' to indicate that you want'
        WRITE(6,100) 'Enter the parameters and designate the'
        WRITE(6,100) 'appropriate files interactively.' 
        WRITE(6,100)
       WRITE(6,100) 'ENTER ANY NUMBER TO CONTINUE'
       READ(5,*) TEMP
        WRITE(6,100) '**********************'              
        WRITE(6,100) 'For example:'
        WRITE(6,100) 
        WRITE(6,100) 'kliqfind sampson.mat p'
        WRITE(6,100)
        WRITE(6,100) 'Allows you to enter the parameters'
        WRITE(6,100) 'interactively'
        END IF

C       WRITE(6,100) 'Stopping processing'
C       WRITE(6,100) '**********************'              
C       WRITE(6,100)
C       Stop
C       END IF
C        WRITE(6,100)
C        WRITE(6,100) '**********************'              
C        WRITE(6,100) 'CLUSTERING BEGINS'
C        WRITE(6,100)

 3335  FORMAT(A200,/,A80)
 3337   FORMAT(10(A100,/))
34348  FORMAT(/////////////////////)
00100   FORMAT(1X,A80)
00102     FORMAT(I1)
00104      FORMAT (' ************************************')
00201       FORMAT(A,I3)
00203        FORMAT(10(A,2X))
00200        FORMAT(A,F10.5)
00205        FORMAT(A,I3)
  107 FORMAT(I1,1X,I5,1X,I1,1X,I1,1X,I4,1X,400(F10.5,1X))

         RETURN 
         END
