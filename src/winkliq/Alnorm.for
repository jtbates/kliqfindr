
      REAL FUNCTION ALNORM(X, UPPER)
C
C        ALGORITHM AS 66  APPL. STATIST. (1973) VOL.22, P.424
C
C        EVALUATES THE TAIL AREA OF THE STANDARDIZED NORMAL CURVE
C        FROM X TO INFINITY IF UPPER IS .TRUE. OR
C        FROM MINUS INFINITY TO X IF UPPER IS .FALSE.
C
      REAL LTONE, UTZERO, ZERO, HALF, ONE, CON, A1, A2, A3,
     $  A4, A5, A6, A7, B1, B2, B3, B4, B5, B6, B7, B8, B9,
     $  B10, B11, B12, X, Y, Z, ZEXP
      LOGICAL UPPER, UP
C
C        LTONE AND UTZERO MUST BE SET TO SUIT THE PARTICULAR COMPUTER
C        (SEE INTRODUCTORY TEXT)
C
      DATA LTONE, UTZERO /7.0, 18.66/
      DATA ZERO, HALF, ONE, CON /0.0, 0.5, 1.0, 1.28/
      DATA           A1,             A2,            A3,
     $               A4,             A5,            A6,
     $               A7
     $  /0.398942280444, 0.399903438504, 5.75885480458,
     $    29.8213557808,  2.62433121679, 48.6959930692,
     $    5.92885724438/
      DATA           B1,            B2,             B3,
     $               B4,            B5,             B6,
     $               B7,            B8,             B9,
     $              B10,           B11,            B12
     $  /0.398942280385,     3.8052E-8,  1.00000615302,
     $    3.98064794E-4, 1.98615381364, 0.151679116635,
     $    5.29330324926,  4.8385912808,  15.1508972451,
     $   0.742380924027,  30.789933034,  3.99019417011/
C
      ZEXP(Z) = EXP(Z)
C
      UP = UPPER
      Z = X
      IF (Z .GE. ZERO) GOTO 10
      UP = .NOT. UP
      Z = -Z
   10 IF (Z .LE. LTONE .OR. UP .AND. Z .LE. UTZERO) GOTO 20
      ALNORM = ZERO
      GOTO 40
   20 Y = HALF * Z * Z
      IF (Z .GT. CON) GOTO 30
C
      ALNORM = HALF - Z * (A1 - A2 * Y / (Y + A3 - A4 / (Y + A5 +
     $  A6 / (Y + A7))))
      GOTO 40
C
   30 ALNORM = B1 * ZEXP(-Y) / (Z - B2 + B3 / (Z + B4 + B5 / (Z -
     $  B6 + B7 / (Z + B8 - B9 / (Z + B10 + B11 / (Z + B12))))))
C
   40 IF (.NOT. UP) ALNORM = ONE - ALNORM
      RETURN
      END
