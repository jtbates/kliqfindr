      INTEGER FUNCTION ignuin(low,high)
C**********************************************************************
C
C     INTEGER FUNCTION IGNUIN( LOW, HIGH )
C
C               GeNerate Uniform INteger
C
C
C                              Function
C
C
C     Generates an integer uniformly distributed between LOW and HIGH.
C
C
C                              Arguments
C
C
C     LOW --> Low bound (inclusive) on integer value to be generated
C                         INTEGER LOW
C
C     HIGH --> High bound (inclusive) on integer value to be generated
C                         INTEGER HIGH
C
C
C                              Note
C
C
C     If (HIGH-LOW) > 2,147,483,561 prints error message on * unit and
C     stops the program.
C
C**********************************************************************

C     IGNLGI generates integers between 1 and 2147483562
C     MAXNUM is 1 less than maximum generable value
C     .. Parameters ..
      INTEGER maxnum
      PARAMETER (maxnum=2147483561)
      CHARACTER*(*) err1,err2
      PARAMETER (err1='LOW > HIGH in IGNUIN',
     +          err2=' ( HIGH - LOW ) > 2,147,483,561 in IGNUIN')
C     ..
C     .. Scalar Arguments ..
      INTEGER high,low
C     ..
C     .. Local Scalars ..
      INTEGER err,ign,maxnow,range,ranp1
C     ..
C     .. External Functions ..
      INTEGER ignlgi
      EXTERNAL ignlgi
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC mod
C     ..
C     .. Executable Statements ..
      IF (.NOT. (low.GT.high)) GO TO 10
      err = 1
C      ABORT-PROGRAM
      GO TO 80

   10 range = high - low
      IF (.NOT. (range.GT.maxnum)) GO TO 20
      err = 2
C      ABORT-PROGRAM
      GO TO 80

   20 IF (.NOT. (low.EQ.high)) GO TO 30
      ignuin = low
      RETURN

C     Number to be generated should be in range 0..RANGE
C     Set MAXNOW so that the number of integers in 0..MAXNOW is an
C     integral multiple of the number in 0..RANGE

   30 ranp1 = range + 1
      maxnow = (maxnum/ranp1)*ranp1
   40 ign = ignlgi() - 1
      IF (.NOT. (ign.LE.maxnow)) GO TO 40
      ignuin = low + mod(ign,ranp1)
      RETURN

   80 IF (.NOT. (err.EQ.1)) GO TO 90
      WRITE (*,*) err1
      GO TO 100

C     TO ABORT-PROGRAM
   90 WRITE (*,*) err2
  100 WRITE (*,*) ' LOW: ',low,' HIGH: ',high
      WRITE (*,*) ' Abort on Fatal ERROR'
      IF (.NOT. (err.EQ.1)) GO TO 110
      STOP 'LOW > HIGH in IGNUIN'

  110 STOP ' ( HIGH - LOW ) > 2,147,483,561 in IGNUIN'

  120 END
      REAL FUNCTION genbet(aa,bb)
C**********************************************************************
C
C     REAL FUNCTION GENBET( A, B )
C               GeNerate BETa random deviate
C
C
C                              Function
C
C
C     Returns a single random deviate from the beta distribution with
C     parameters A and B.  The density of the beta is
C               x^(a-1) * (1-x)^(b-1) / B(a,b) for 0 < x < 1
C
C
C                              Arguments
C
C
C     A --> First parameter of the beta distribution
C                         REAL A
C     JJV                 (A > 1.0E-37)
C
C     B --> Second parameter of the beta distribution
C                         REAL B
C     JJV                 (B > 1.0E-37)
C
C
C                              Method
C
C
C     R. C. H. Cheng
C     Generating Beta Variates with Nonintegral Shape Parameters
C     Communications of the ACM, 21:317-322  (1978)
C     (Algorithms BB and BC)
C
C**********************************************************************
C     .. Parameters ..
C     Close to the largest number that can be exponentiated
      REAL expmax
C     JJV changed this - 89 was too high, and LOG(1.0E38) = 87.49823
      PARAMETER (expmax=87.49823)
C     Close to the largest representable single precision number
      REAL infnty
      PARAMETER (infnty=1.0E38)
C     JJV added the parameter minlog
C     Close to the smallest number of which a LOG can be taken.
      REAL minlog
      PARAMETER (minlog=1.0E-37)
C     ..
C     .. Scalar Arguments ..
      REAL aa,bb
C     ..
C     .. Local Scalars ..
      REAL a,alpha,b,beta,delta,gamma,k1,k2,olda,oldb,r,s,t,u1,u2,v,w,y,
     +     z
      LOGICAL qsame
C     ..
C     .. External Functions ..
      REAL ranf
      EXTERNAL ranf
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC exp,log,max,min,sqrt
C     ..
C     .. Save statement ..
C     JJV added a,b
      SAVE olda,oldb,alpha,beta,gamma,k1,k2,a,b
C     ..
C     .. Data statements ..
C     JJV changed these to ridiculous values
      DATA olda,oldb/-1.0E37,-1.0E37/
C     ..
C     .. Executable Statements ..
      qsame = (olda.EQ.aa) .AND. (oldb.EQ.bb)
      IF (qsame) GO TO 20
C     JJV added small minimum for small log problem in calc of W
      IF (.NOT. (aa.LT.minlog.OR.bb.LT.minlog)) GO TO 10
      WRITE (*,*) ' AA or BB < ',minlog,' in GENBET - Abort!'
      WRITE (*,*) ' AA: ',aa,' BB ',bb
      STOP ' AA or BB too small in GENBET - Abort!'

   10 olda = aa
      oldb = bb
   20 IF (.NOT. (min(aa,bb).GT.1.0)) GO TO 100


C     Alborithm BB

C
C     Initialize
C
      IF (qsame) GO TO 30
      a = min(aa,bb)
      b = max(aa,bb)
      alpha = a + b
      beta = sqrt((alpha-2.0)/ (2.0*a*b-alpha))
      gamma = a + 1.0/beta
   30 CONTINUE
   40 u1 = ranf()
C
C     Step 1
C
      u2 = ranf()
      v = beta*log(u1/ (1.0-u1))
C     JJV altered this
      IF (v.GT.expmax) GO TO 55
C     JJV added checker to see if a*exp(v) will overflow
C     JJV 50 _was_ w = a*exp(v); also note here a > 1.0
   50 w = exp(v)
      IF (w.GT.infnty/a) GO TO 55
      w = a*w
      GO TO 60
 55   w = infnty

   60 z = u1**2*u2
      r = gamma*v - 1.3862944
      s = a + r - w
C
C     Step 2
C
      IF ((s+2.609438).GE. (5.0*z)) GO TO 70
C
C     Step 3
C
      t = log(z)
      IF (s.GT.t) GO TO 70
C
C     Step 4
C
C     JJV added checker to see if log(alpha/(b+w)) will 
C     JJV overflow.  If so, we count the log as -INF, and
C     JJV consequently evaluate conditional as true, i.e.
C     JJV the algorithm rejects the trial and starts over
C     JJV May not need this here since ALPHA > 2.0
      IF (alpha/(b+w).LT.minlog) GO TO 40

      IF ((r+alpha*log(alpha/ (b+w))).LT.t) GO TO 40
C
C     Step 5
C
   70 IF (.NOT. (aa.EQ.a)) GO TO 80
      genbet = w/ (b+w)
      GO TO 90

   80 genbet = b/ (b+w)
   90 GO TO 230


C     Algorithm BC

C
C     Initialize
C
  100 IF (qsame) GO TO 110
      a = max(aa,bb)
      b = min(aa,bb)
      alpha = a + b
      beta = 1.0/b
      delta = 1.0 + a - b
      k1 = delta* (0.0138889+0.0416667*b)/ (a*beta-0.777778)
      k2 = 0.25 + (0.5+0.25/delta)*b
  110 CONTINUE
  120 u1 = ranf()
C
C     Step 1
C
      u2 = ranf()
      IF (u1.GE.0.5) GO TO 130
C
C     Step 2
C
      y = u1*u2
      z = u1*y
      IF ((0.25*u2+z-y).GE.k1) GO TO 120
      GO TO 170
C
C     Step 3
C
  130 z = u1**2*u2
      IF (.NOT. (z.LE.0.25)) GO TO 160
      v = beta*log(u1/ (1.0-u1))

C     JJV instead of checking v > expmax at top, I will check
C     JJV if a < 1, then check the appropriate values

      IF (a.GT.1.0) GO TO 135
C     JJV A < 1 so it can help out if EXP(V) would overflow
      IF (v.GT.expmax) GO TO 132
      w = a*exp(v)
      GO TO 200
 132  w = v + log(a)
      IF (w.GT.expmax) GO TO 140
      w = exp(w)
      GO TO 200

C     JJV in this case A > 1
 135  IF (v.GT.expmax) GO TO 140
      w = exp(v)
      IF (w.GT.infnty/a) GO TO 140
      w = a*w
      GO TO 200
 140  w = infnty
      GO TO 200

  160 IF (z.GE.k2) GO TO 120
C
C     Step 4
C
C
C     Step 5
C
  170 v = beta*log(u1/ (1.0-u1))

C     JJV same kind of checking as above
      IF (a.GT.1.0) GO TO 175
C     JJV A < 1 so it can help out if EXP(V) would overflow
      IF (v.GT.expmax) GO TO 172
      w = a*exp(v)
      GO TO 190
 172  w = v + log(a)
      IF (w.GT.expmax) GO TO 180
      w = exp(w)
      GO TO 190

C     JJV in this case A > 1
 175  IF (v.GT.expmax) GO TO 180
      w = exp(v)
      IF (w.GT.infnty/a) GO TO 180
      w = a*w
      GO TO 190

  180 w = infnty

C     JJV here we also check to see if log overlows; if so, we treat it
C     JJV as -INF, which means condition is true, i.e. restart
  190 IF (alpha/(b+w).LT.minlog) GO TO 120
      IF ((alpha* (log(alpha/ (b+w))+v)-1.3862944).LT.log(z)) GO TO 120
C
C     Step 6
C
  200 IF (.NOT. (a.EQ.aa)) GO TO 210
      genbet = w/ (b+w)
      GO TO 220

  210 genbet = b/ (b+w)
  220 CONTINUE
  230 RETURN

      END
      REAL FUNCTION genunf(low,high)
C**********************************************************************
C
C     REAL FUNCTION GENUNF( LOW, HIGH )
C
C               GeNerate Uniform Real between LOW and HIGH
C
C
C                              Function
C
C
C     Generates a real uniformly distributed between LOW and HIGH.
C
C
C                              Arguments
C
C
C     LOW --> Low bound (exclusive) on real value to be generated
C                         REAL LOW
C
C     HIGH --> High bound (exclusive) on real value to be generated
C                         REAL HIGH
C
C**********************************************************************
C     .. Scalar Arguments ..
      REAL high,low
C     ..
C     .. External Functions ..
      REAL ranf
      EXTERNAL ranf
C     ..
C     .. Executable Statements ..
      IF (.NOT. (low.GT.high)) GO TO 10
      WRITE (*,*) 'LOW > HIGH in GENUNF: LOW ',low,' HIGH: ',high
      WRITE (*,*) 'Abort'
      STOP 'LOW > High in GENUNF - Abort'

   10 genunf = low + (high-low)*ranf()

      RETURN

      END
      REAL FUNCTION ranf()
C**********************************************************************
C
C     REAL FUNCTION RANF()
C                RANDom number generator as a Function
C
C     Returns a random floating point number from a uniform distribution
C     over 0 - 1 (endpoints of this interval are not returned) using the
C     current generator
C
C     This is a transcription from Pascal to Fortran of routine
C     Uniform_01 from the paper
C
C     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
C     with Splitting Facilities." ACM Transactions on Mathematical
C     Software, 17:98-111 (1991)
C
C**********************************************************************
C     .. External Functions ..
      INTEGER ignlgi
      EXTERNAL ignlgi
C     ..
C     .. Executable Statements ..
C
C     4.656613057E-10 is 1/M1  M1 is set in a data statement in IGNLGI
C      and is currently 2147483563. If M1 changes, change this also.
C
      ranf = ignlgi()*4.656613057E-10
      RETURN

      END
      INTEGER FUNCTION ignlgi()
C**********************************************************************
C
C     INTEGER FUNCTION IGNLGI()
C               GeNerate LarGe Integer
C
C     Returns a random integer following a uniform distribution over
C     (1, 2147483562) using the current generator.
C
C     This is a transcription from Pascal to Fortran of routine
C     Random from the paper
C
C     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
C     with Splitting Facilities." ACM Transactions on Mathematical
C     Software, 17:98-111 (1991)
C
C**********************************************************************
C     .. Parameters ..
      INTEGER numg
      PARAMETER (numg=32)
C     ..
C     .. Scalars in Common ..
      INTEGER a1,a1vw,a1w,a2,a2vw,a2w,m1,m2
C     ..
C     .. Arrays in Common ..
      INTEGER cg1(numg),cg2(numg),ig1(numg),ig2(numg),lg1(numg),
     +        lg2(numg)
      LOGICAL qanti(numg)
C     ..
C     .. Local Scalars ..
      INTEGER curntg,k,s1,s2,z
      LOGICAL qqssd
C     ..
C     .. External Functions ..
      LOGICAL qrgnin
      EXTERNAL qrgnin
C     ..
C     .. External Subroutines ..
      EXTERNAL getcgn,inrgcm,rgnqsd,setall
C     ..
C     .. Common blocks ..
      COMMON /globe/m1,m2,a1,a2,a1w,a2w,a1vw,a2vw,ig1,ig2,lg1,lg2,cg1,
     +       cg2,qanti
C     ..
C     .. Save statement ..
      SAVE /globe/
C     ..
C     .. Executable Statements ..
C
C     IF THE RANDOM NUMBER PACKAGE HAS NOT BEEN INITIALIZED YET, DO SO.
C     IT CAN BE INITIALIZED IN ONE OF TWO WAYS : 1) THE FIRST CALL TO
C     THIS ROUTINE  2) A CALL TO SETALL.
C
      IF (.NOT. (qrgnin())) CALL inrgcm()
      CALL rgnqsd(qqssd)
      IF (.NOT. (qqssd)) CALL setall(1234567890,123456789)
C
C     Get Current Generator
C
      CALL getcgn(curntg)
      s1 = cg1(curntg)
      s2 = cg2(curntg)
      k = s1/53668
      s1 = a1* (s1-k*53668) - k*12211
      IF (s1.LT.0) s1 = s1 + m1
      k = s2/52774
      s2 = a2* (s2-k*52774) - k*3791
      IF (s2.LT.0) s2 = s2 + m2
      cg1(curntg) = s1
      cg2(curntg) = s2
      z = s1 - s2
      IF (z.LT.1) z = z + m1 - 1
      IF (qanti(curntg)) z = m1 - z
      ignlgi = z
      RETURN

      END
      LOGICAL FUNCTION qrgnin()
C**********************************************************************
C
C     LOGICAL FUNCTION QRGNIN()
C               Q Random GeNerators INitialized?
C
C     A trivial routine to determine whether or not the random
C     number generator has been initialized.  Returns .TRUE. if
C     it has, else .FALSE.
C
C**********************************************************************
C     .. Scalar Arguments ..
      LOGICAL qvalue
C     ..
C     .. Local Scalars ..
      LOGICAL qinit
C     ..
C     .. Entry Points ..
      LOGICAL qrgnsn
C     ..
C     .. Save statement ..
      SAVE qinit
C     ..
C     .. Data statements ..
      DATA qinit/.FALSE./
C     ..
C     .. Executable Statements ..
      qrgnin = qinit
      RETURN

      ENTRY qrgnsn(qvalue)
C**********************************************************************
C
C     LOGICAL FUNCTION QRGNSN( QVALUE )
C               Q Random GeNerators Set whether iNitialized
C
C     Sets state of whether random number generator is initialized
C     to QVALUE.
C
C     This routine is actually an entry in QRGNIN, hence it is a
C     logical function.  It returns the (meaningless) value .TRUE.
C
C**********************************************************************
      qinit = qvalue
      qrgnsn = .TRUE.
      RETURN

      END
      SUBROUTINE getcgn(g)
      INTEGER g
C**********************************************************************
C
C      SUBROUTINE GETCGN(G)
C                         Get GeNerator
C
C     Returns in G the number of the current random number generator
C
C
C                              Arguments
C
C
C     G <-- Number of the current random number generator (1..32)
C                    INTEGER G
C
C**********************************************************************
C
      INTEGER curntg,numg
      SAVE curntg
      PARAMETER (numg=32)
      DATA curntg/1/
C
      g = curntg
      RETURN

      ENTRY setcgn(g)
C**********************************************************************
C
C     SUBROUTINE SETCGN( G )
C                      Set GeNerator
C
C     Sets  the  current  generator to G.    All references to a generat
C     are to the current generator.
C
C
C                              Arguments
C
C
C     G --> Number of the current random number generator (1..32)
C                    INTEGER G
C
C**********************************************************************
C
C     Abort if generator number out of range
C
      IF (.NOT. (g.LT.0.OR.g.GT.numg)) GO TO 10
      WRITE (*,*) ' Generator number out of range in SETCGN:',
     +  ' Legal range is 1 to ',numg,' -- ABORT!'
      STOP ' Generator number out of range in SETCGN'

   10 curntg = g
      RETURN

      END
      SUBROUTINE inrgcm()
C**********************************************************************
C
C     SUBROUTINE INRGCM()
C          INitialize Random number Generator CoMmon
C
C
C                              Function
C
C
C     Initializes common area  for random number  generator.  This saves
C     the  nuisance  of  a  BLOCK DATA  routine  and the  difficulty  of
C     assuring that the routine is loaded with the other routines.
C
C**********************************************************************
C     .. Parameters ..
      INTEGER numg
      PARAMETER (numg=32)
C     ..
C     .. Scalars in Common ..
      INTEGER a1,a1vw,a1w,a2,a2vw,a2w,m1,m2
C     ..
C     .. Arrays in Common ..
      INTEGER cg1(numg),cg2(numg),ig1(numg),ig2(numg),lg1(numg),
     +        lg2(numg)
      LOGICAL qanti(numg)
C     ..
C     .. Local Scalars ..
      INTEGER i
      LOGICAL qdum
C     ..
C     .. External Functions ..
      LOGICAL qrgnsn
      EXTERNAL qrgnsn
C     ..
C     .. Common blocks ..
      COMMON /globe/m1,m2,a1,a2,a1w,a2w,a1vw,a2vw,ig1,ig2,lg1,lg2,cg1,
     +       cg2,qanti
C     ..
C     .. Save statement ..
      SAVE /globe/
C     ..
C     .. Executable Statements ..
C     V=20;                            W=30;
C
C     A1W = MOD(A1**(2**W),M1)         A2W = MOD(A2**(2**W),M2)
C     A1VW = MOD(A1**(2**(V+W)),M1)    A2VW = MOD(A2**(2**(V+W)),M2)
C
C   If V or W is changed A1W, A2W, A1VW, and A2VW need to be recomputed.
C    An efficient way to precompute a**(2*j) MOD m is to start with
C    a and square it j times modulo m using the function MLTMOD.
C
      m1 = 2147483563
      m2 = 2147483399
      a1 = 40014
      a2 = 40692
      a1w = 1033780774
      a2w = 1494757890
      a1vw = 2082007225
      a2vw = 784306273
      DO 10,i = 1,numg
          qanti(i) = .FALSE.
   10 CONTINUE
C
C     Tell the world that common has been initialized
C
      qdum = qrgnsn(.TRUE.)
      RETURN

      END
      SUBROUTINE setall(iseed1,iseed2)
C**********************************************************************
C
C      SUBROUTINE SETALL(ISEED1,ISEED2)
C               SET ALL random number generators
C
C     Sets the initial seed of generator 1 to ISEED1 and ISEED2. The
C     initial seeds of the other generators are set accordingly, and
C     all generators states are set to these seeds.
C
C     This is a transcription from Pascal to Fortran of routine
C     Set_Initial_Seed from the paper
C
C     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
C     with Splitting Facilities." ACM Transactions on Mathematical
C     Software, 17:98-111 (1991)
C
C
C                              Arguments
C
C
C     ISEED1 -> First of two integer seeds
C                                   INTEGER ISEED1
C
C     ISEED2 -> Second of two integer seeds
C                                   INTEGER ISEED1
C
C**********************************************************************
C     .. Parameters ..
      INTEGER numg
      PARAMETER (numg=32)
C     ..
C     .. Scalar Arguments ..
      INTEGER iseed1,iseed2
      LOGICAL qssd
C     ..
C     .. Scalars in Common ..
      INTEGER a1,a1vw,a1w,a2,a2vw,a2w,m1,m2
C     ..
C     .. Arrays in Common ..
      INTEGER cg1(numg),cg2(numg),ig1(numg),ig2(numg),lg1(numg),
     +        lg2(numg)
      LOGICAL qanti(numg)
C     ..
C     .. Local Scalars ..
      INTEGER g,ocgn
      LOGICAL qqssd
C     ..
C     .. External Functions ..
      INTEGER mltmod
      LOGICAL qrgnin
      EXTERNAL mltmod,qrgnin
C     ..
C     .. External Subroutines ..
      EXTERNAL getcgn,initgn,inrgcm,setcgn
C     ..
C     .. Common blocks ..
      COMMON /globe/m1,m2,a1,a2,a1w,a2w,a1vw,a2vw,ig1,ig2,lg1,lg2,cg1,
     +       cg2,qanti
C     ..
C     .. Save statement ..
      SAVE /globe/,qqssd
C     ..
C     .. Data statements ..
      DATA qqssd/.FALSE./
C     ..
C     .. Executable Statements ..
C
C     TELL IGNLGI, THE ACTUAL NUMBER GENERATOR, THAT THIS ROUTINE
C      HAS BEEN CALLED.
C
      qqssd = .TRUE.
      CALL getcgn(ocgn)
C
C     Initialize Common Block if Necessary
C
      IF (.NOT. (qrgnin())) CALL inrgcm()
      ig1(1) = iseed1
      ig2(1) = iseed2
      CALL initgn(-1)
      DO 10,g = 2,numg
          ig1(g) = mltmod(a1vw,ig1(g-1),m1)
          ig2(g) = mltmod(a2vw,ig2(g-1),m2)
          CALL setcgn(g)
          CALL initgn(-1)
   10 CONTINUE
      CALL setcgn(ocgn)
      RETURN

      ENTRY rgnqsd(qssd)
C**********************************************************************
C
C     SUBROUTINE RGNQSD
C                    Random Number Generator Query SeeD set?
C
C     Returns (LOGICAL) QSSD as .TRUE. if SETALL has been invoked,
C     otherwise returns .FALSE.
C
C**********************************************************************
      qssd = qqssd
      RETURN

      END
      INTEGER FUNCTION mltmod(a,s,m)
C**********************************************************************
C
C     INTEGER FUNCTION MLTMOD(A,S,M)
C
C                    Returns (A*S) MOD M
C
C     This is a transcription from Pascal to Fortran of routine
C     MULtMod_Decompos from the paper
C
C     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
C     with Splitting Facilities." ACM Transactions on Mathematical
C     Software, 17:98-111 (1991)
C
C
C                              Arguments
C
C
C     A, S, M  -->
C                         INTEGER A,S,M
C
C**********************************************************************
C     .. Parameters ..
      INTEGER h
      PARAMETER (h=32768)
C     ..
C     .. Scalar Arguments ..
      INTEGER a,m,s
C     ..
C     .. Local Scalars ..
      INTEGER a0,a1,k,p,q,qh,rh
C     ..
C     .. Executable Statements ..
C
C     H = 2**((b-2)/2) where b = 32 because we are using a 32 bit
C      machine. On a different machine recompute H
C
      IF (.NOT. (a.LE.0.OR.a.GE.m.OR.s.LE.0.OR.s.GE.m)) GO TO 10
      WRITE (*,*) ' A, M, S out of order in MLTMOD - ABORT!'
      WRITE (*,*) ' A = ',a,' S = ',s,' M = ',m
      WRITE (*,*) ' MLTMOD requires: 0 < A < M; 0 < S < M'
      STOP ' A, M, S out of order in MLTMOD - ABORT!'

   10 IF (.NOT. (a.LT.h)) GO TO 20
      a0 = a
      p = 0
      GO TO 120

   20 a1 = a/h
      a0 = a - h*a1
      qh = m/h
      rh = m - h*qh
      IF (.NOT. (a1.GE.h)) GO TO 50
      a1 = a1 - h
      k = s/qh
      p = h* (s-k*qh) - k*rh
   30 IF (.NOT. (p.LT.0)) GO TO 40
      p = p + m
      GO TO 30

   40 GO TO 60

   50 p = 0
C
C     P = (A2*S*H)MOD M
C
   60 IF (.NOT. (a1.NE.0)) GO TO 90
      q = m/a1
      k = s/q
      p = p - k* (m-a1*q)
      IF (p.GT.0) p = p - m
      p = p + a1* (s-k*q)
   70 IF (.NOT. (p.LT.0)) GO TO 80
      p = p + m
      GO TO 70

   80 CONTINUE
   90 k = p/qh
C
C     P = ((A2*H + A1)*S)MOD M
C
      p = h* (p-k*qh) - k*rh
  100 IF (.NOT. (p.LT.0)) GO TO 110
      p = p + m
      GO TO 100

  110 CONTINUE
  120 IF (.NOT. (a0.NE.0)) GO TO 150
C
C     P = ((A2*H + A1)*H*S)MOD M
C
      q = m/a0
      k = s/q
      p = p - k* (m-a0*q)
      IF (p.GT.0) p = p - m
      p = p + a0* (s-k*q)
  130 IF (.NOT. (p.LT.0)) GO TO 140
      p = p + m
      GO TO 130

  140 CONTINUE
  150 mltmod = p
C
      RETURN

      END
      SUBROUTINE initgn(isdtyp)
C**********************************************************************
C
C     SUBROUTINE INITGN(ISDTYP)
C          INIT-ialize current G-e-N-erator
C
C     Reinitializes the state of the current generator
C
C     This is a transcription from Pascal to Fortran of routine
C     Init_Generator from the paper
C
C     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
C     with Splitting Facilities." ACM Transactions on Mathematical
C     Software, 17:98-111 (1991)
C
C
C                              Arguments
C
C
C     ISDTYP -> The state to which the generator is to be set
C
C          ISDTYP = -1  => sets the seeds to their initial value
C          ISDTYP =  0  => sets the seeds to the first value of
C                          the current block
C          ISDTYP =  1  => sets the seeds to the first value of
C                          the next block
C
C                                   INTEGER ISDTYP
C
C**********************************************************************
C     .. Parameters ..
      INTEGER numg
      PARAMETER (numg=32)
C     ..
C     .. Scalar Arguments ..
      INTEGER isdtyp
C     ..
C     .. Scalars in Common ..
      INTEGER a1,a1vw,a1w,a2,a2vw,a2w,m1,m2
C     ..
C     .. Arrays in Common ..
      INTEGER cg1(numg),cg2(numg),ig1(numg),ig2(numg),lg1(numg),
     +        lg2(numg)
      LOGICAL qanti(numg)
C     ..
C     .. Local Scalars ..
      INTEGER g
C     ..
C     .. External Functions ..
      LOGICAL qrgnin
      INTEGER mltmod
      EXTERNAL qrgnin,mltmod
C     ..
C     .. External Subroutines ..
      EXTERNAL getcgn
C     ..
C     .. Common blocks ..
      COMMON /globe/m1,m2,a1,a2,a1w,a2w,a1vw,a2vw,ig1,ig2,lg1,lg2,cg1,
     +       cg2,qanti
C     ..
C     .. Save statement ..
      SAVE /globe/
C     ..
C     .. Executable Statements ..
C     Abort unless random number generator initialized
      IF (qrgnin()) GO TO 10
      WRITE (*,*) ' INITGN called before random number generator ',
     +  ' initialized -- abort!'
      STOP ' INITGN called before random number generator initialized'

   10 CALL getcgn(g)
      IF ((-1).NE. (isdtyp)) GO TO 20
      lg1(g) = ig1(g)
      lg2(g) = ig2(g)
      GO TO 50

   20 IF ((0).NE. (isdtyp)) GO TO 30
      CONTINUE
      GO TO 50
C     do nothing
   30 IF ((1).NE. (isdtyp)) GO TO 40
      lg1(g) = mltmod(a1w,lg1(g),m1)
      lg2(g) = mltmod(a2w,lg2(g),m2)
      GO TO 50

   40 STOP 'ISDTYP NOT IN RANGE'

   50 cg1(g) = lg1(g)
      cg2(g) = lg2(g)
      RETURN

      END
