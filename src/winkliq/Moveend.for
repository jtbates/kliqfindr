
      SUBROUTINE MOVEEND (THISFILE)
C Next 7 for g77(unix)
       CHARACTER FNAME*16
       INTEGER G,THISFILE,LENGTH
       DO 1991 G=1,1000000
        READ(THISFILE,4301,END=1094) FNAME
01991    CONTINUE
01094  LENGTH=G
04301   FORMAT(20(A))
       BACKSPACE(THISFILE)
CNext 3 for Lahey (PC)
C     USE SERVICE_ROUTINES
C     INTEGER THISFILE,I
C     I = FSEEK(THISFILE,0,2)
      RETURN
      END
