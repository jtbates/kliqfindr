      PROGRAM KLIQFIND 
C     This program is copywritten and may not be reproduced without
c the expressed written consent of the author, Ken Frank.

C       This program identifies cohesive subgroups in social 
C       network data by maximizing theta1.  For details, see
C
C      Frank, K.A. (1996). Mapping interactions within
C      and between cohesive subgroups. Social Networks.
C      Volume 18, pages 93-119.
C      
C      Frank. K.A. (1995). Identifying Cohesive 
C      Subgroups. Social Networks, 17, 27-56.

C      This program takes as input a file (as named on input 
C      into "infile" which contains an edgelist list of 
C      chooser,chosen,frequency of contact (3I10))
C      chooser and chosen must be < 100, frequency of contact must be a 
C      positive integer.


       REAL NEWTHETA,CELL(4),TMAX,FIXR,XTMAX,PL,XTNUM,TNUM
C      the vars in the preceeding line are for calculating predicted 
C      theta1 and likelihood ratio test of observed theta1

c     Newtheta contains the value of theta1 being maximized by the
c program
c     Cell contains the values of the cells in table1
c     Pl contains the predicted value for theta1
c     Fixr contains the mximum number of connections that
c any actor initiates. 
c 

       Integer change,newgroup,connect(100,100),group(100),
     C datamat(100,100),i,j,numact,numgroup,iso(100),
     C marginal(5),groupn(100),madechng,numseed,rchoice(100),
     C actor1,actor2,tgroup,maxwt,tempconn,pregroup,tactor,
     C seed(3),initriad,useseed(100),s,removed,maxiter,
     c found,xgroup,doneiso,attachi,writemul,domul,onbad(100),
     c tnumact,pair(100),preseed,attached(100),numiso,
     c numiter,g,groupid(100)
c      g is a counter
c     groupid is the id of the groups for making
c consecutive groups at the end.

c      maxiter is the maximum number of iterations to go through
c before stopping on account of non convergence.
c      removed indicates whether at least one actor was removed
c        or not
c      Tactor is a temporary actor for processing arrays
c      Pregroup indicator for whether there are prior groups in
c       The data
c       Maxwt is the maximum weight of any given connection
c       Actor1,actor2 are counters for actors
c       Connect contains the connections from actors to groups
c       datamat contains the matrix with the data
c       Numact is number of actors
c       group indicates the group membership of each actor
c       Groupn indicates the sizes of the groups
c       Change indicates the actor to be changed
c       Newgroup indciates the group the actor should change to
c       Oldgroup is the current group of the change actor
c       Newtheta is the new optimal value of theta1 that will
c       Tempconn is a temporary holder for connections between
c         two actors
c       I and j are used as counters

c      Numgroup represents the number of groups
c      Iso is a list indicating whether a given actor is currently 
c an isolate (unaffiliated) or not.  Used on Kmultma2
c      marginal contains the marginals and total for Table 1 
c (in Frank, 1995).  See output in kliqfind.out.
C      Groupn contains the sizes of the groups
c      Madechng is an ndicator for whether the algorithm 
c has made a change in group membership or not
c      Numseed indicates the number of actors eligible to be seeds
c      Rchoice indicates the number of others that each 
c actor has connected to. 
C      Tgroup is a temporary holder for group membership when 
c reading in the data
c      Seed contains the actors selected as seeds for a new group
c      Initriad is a parameter, set=1 here, indicating that groups
c should be started with triads. 
C      Useseed indicates whether a given actor has ever been used as
c a seed for a subgroup or not.
C      S is a counter
c      Found is an indicator for whether a procedure has
c found a member of a newly formed dyad (down from a triad) to
c be reassigned
c      Xgroup is the group to which the actor described in "found" should
c be reassigned
c      doneiso indicates whether the add isolate mode has been 
c engaged or not.
C attachi is a parameter, =1 is isolates, or unaffiliates, are to 
c be added, even if doing so results in a decrease in theta1
c      writemul is a parameter, indicating whether the results of 
c matrix multiplication should be written out to kliqfind.xxp.
C      Domul is an indicator, works with writemul, indicating whether
c matrix multiplication should be performed or read from a file
c      Onbad is a list of indicators for whether the actor is
c on the badlist or not, indicating whether it should be used
c in the clustering process
c      Tnumact indicates the number of actors actually used in the 
c clustering
c      Pair indicates the actor with whom an actor not in the 
c clustering is paired.
C      Preseed indicates to kmultma2 whether this is a start up seed
c or a seed during the ascent (only affects output).
C
      CHARACTER infile*256,cmdfile*256
c      character*256 CMNAM@
C      CHARACTER*24 IDATE
      CHARACTER*8 INDICAT
c      character*8 DATE@
   
c    Infile contains the edgelist of data
c    idate contains the date, for output.
     
        common datamat,connect 
C
C ***************************************************************
c    setting parameters
       writemul=0
       attachi=1
       initriad=1
       maxiter=-1
c       infile=CMNAM@()
c       cmdfile=CMNAM@()
       cmdfile=""
       infile=""
       write(6,4308) "Welcome to KliqueFinder, software for"
       write(6,4308) "identifying cohesive subgroups."
       write(6,4308) 

       write(6,4308) "Enter the name of the command file"
       write(6,4308) "Enter 'none' if you want to enter"
       write(6,4308) "parameters interactively."
       read(5,*) cmdfile

      if ((cmdfile .ne. "none") .and. (cmdfile .ne. "")) then
      OPEN(9,file=cmdFILE)
       read(9,101,end=111) maxiter,attachi
00111   close(9)
       end if
       if (maxiter .lt. 0) then  
      
       write(6,4308) "No command file used, enter parameters"
       write(6,4308) "interactively."
       WRITE(6,4308) "After How many iterations should the ascent stop" 
       WRITE(6,4308) "If it has not yet converged?"       
       WRITE(6,4308) "ENTER AN INTEGER"              
       READ(5,*) maxiter
       WRITE(6,4308) "Should isolates be attached after ascent,"
       WRITE(6,4308) "and then a final ascent executed?"
       WRITE(6,4308) "ENTER Y OR N"
       READ (5,*) INDICAT
        IF ((indicat(1:1) .eq. 'y') .or. 
     c      (INDICAT(1:1) .EQ. 'Y')) THEN
        ATTACHI=1
        ELSE
        ATTACHI=0
        END IF
       write(6,4308)
       write(6,4308) "Parameters have been written to kliqfind.cmd."   
       write(6,4308)     
       end if
       open(9,file="kliqfind.cmd")
       write(9,101) maxiter,attachi
       close(9)
c     attachi is a parameter.  =1 if you want to attach isolates at end of
c      run, regardless of whether they improve theta.  0 otherwise.
c     As isolates are attached program will continue to seek to maximize
c    theta1 for other actors.  That is, it continues to seek to ascend, 
c    if possible.  Run is complete when there are no isolates and when
c     ascent is not possible.


c   initializing arrays
   
      Do 00014 tactor=1,100
      Groupn(tactor)=0
      Group(tactor)=tactor
      useseed(tactor)=0
      attached(tactor)=0
      Do 00015 tgroup=1,100
      Datamat(tactor,tgroup)=0
00015 continue
00014 continue


c initializing parameters of characterizing the network

        Maxwt=0
        Numact=0
        Numgroup=0
        pregroup=0

c      Infile=cmnam(1)
      do 71 while (infile .eq. "")
      write(6,4308)
      write(6,4308)
      write(6,4308) "In a moment I will ask the name of your data"
      write(6,4308) "file.  Data must be in a list:"
      write(6,4308) "(chooser, chosen, frequency of exchange)"
      write(6,4308) "format must be 3I10, meaning each column"
      write(6,4308) "is 10 integers wide, for example:"
      write(6,4308) "         1        21         3"
      write(6,4308) "123456789012345678901234567890"
      write(6,4308)
      write(6,4308) "indicates that actor 1 indicates exchanging"
      write(6,4308) "with actor 21 at a level of 3"
      write(6,4308) "(the line 1234... should not appear in your"
      write(6,4308) "file)."
      write(6,4308)
      write(6,4308)
00073 write(6,4308) "Enter the name of your data file:"
      read(5,*,end=73) infile
00071 continue

      OPEN(9,file=INFILE)

c      Reading in data, finding maxwt and numact

      DO 00007 I=1,10000
      READ(9,103,END=10) actor1,actor2,tempconn
      If ((actor2 .le. 99) .and. (actor1 .le. 99)) then
c note restrictions on chooser and chosen
         Datamat(actor1,actor2)=tempconn
         If (tempconn .gt. Maxwt) then
          Maxwt=tempconn
         End if
         If (actor1 .gt. Numact) then 
          Numact=actor1
         End if
         If (actor2 .gt. Numact) then
          Numact=actor2
         End if
      End if
c     If (actor2 .le 99) .and. (actor1 .le. 99)

00007 continue

00010 close(9)
       
      if (numact .le. 3) then
      write(6,4308) "There are fewer than four actors in your"
      write(6,4308) "data file.  It doesn't make sense to run"
      write(6,4308) "KliqueFinder on such a small data set."
      write(6,4308) "Are you sure you specified the correct"
      write(6,4308) "file?"
      write(6,4308) infile
      stop
      end if

       Open(3, file="kliqfind.log")
       write(3,4308) "This file is kliqfind.log"
c removing actors who are disconnected or who are connected
c to only one other (and therefore must be in the other's
c subgroup.  Badlist(actor)=1 if actor should be removed.

      call REMOVEO(numact,onbad,tnumact,pair,removed)
c
c    Calculating rchoice and iso.
       numseed=0
       do 32 i=1,numact
       rchoice(i)=0
          iso(i)=0
      if (onbad(i) .eq. 0) then
         iso(i)=1
      end if
c if iso=1 then actor is eligible to be a seed, which 
c shouldn't apply to actors who are on badlist


       do 33 j=1,numact
       if (datamat(i,j) .gt. 0) then
       rchoice(i)=rchoice(i)+1
       end if
00033   continue
00032    continue


c      Identifying first seed by calling kmultma2
       numgroup=numact
       numdyad=1
       domul=1
       preseed=1
       write(6,4308)
       write(6,4308) "processing ..."
       write(6,4308)
       call KMULTMA2 (numact,useseed,seed,iso,rchoice,
     C domul,writemul,preseed)
       preseed=0

       if (writemul .eq. 1) then
       domul=0
       end if
c never have to calculate again if you write it out.

C assigning the seeds to the group of the first actor in the seed
       numseed=3
       do 17 s=2,3
       group(seed(s))=group(seed(1))
       useseed(seed(s))=1
00017   continue
       numgroup=numact
       groupn(group(seed(1)))=3

c      Calculating initial marginals and cells of table 1

       Marginal(5)=tnumact*(tnumact-1)*maxwt
       Marginal(4)=0
       Cell(4)=0
       Do 00002 actor1=1,numact
         Do 00022 tgroup=1,numgroup
         Connect(actor1,tgroup)=0
00022    continue
       Do 00003 actor2=1,numact
       If (actor1 .ne. actor2) then 
       Tempconn= Datamat(actor1,actor2)+datamat(actor2,actor1)
       Connect(actor1,group(actor2))=
     C connect(actor1,group(actor2)) + tempconn
        If (group(actor1) .eq. Group(actor2)) then 
       Cell(4)=cell(4)+Datamat(actor1,actor2)
        End if
       Marginal(4)=marginal(4)+Datamat(actor1,actor2)
       End if
c      Ending of (if actor1 .ne. Actor2)
00003 continue
00002 continue

c      Now get the other marginals
       Marginal(3)=marginal(5)-marginal(4)

       Do 11 i=1,numgroup
       Groupn(i)=0
00011   continue
        Do 12 i=1,numact
        Groupn(group(i))=groupn(group(i))+1
00012 continue

       Marginal(2)=0
       Do 6 j=1,numgroup
       Marginal(2)=Marginal(2)+groupn(j)*(groupn(j)-1)
00006 continue
       Marginal(2)=Marginal(2)*maxwt
       Marginal(1)=Marginal(5)-marginal(2)

c now get the other cells

       Cell(3)=marginal(2)-cell(4)
       Cell(1)=marginal(3)-cell(3)
       Cell(2)=marginal(4)-cell(4)
       newtheta=0
       if ((cell(2) .gt. 0) .and. (cell(3) .gt. 0)) then
c Here it is, calculating theta
       Newtheta=(cell(1)*cell(4))/(cell(2)*cell(3))
       end if
     
c      Finding the groups
c initializing conditions
       Change=0
       Newgroup=0
       Madechng=1
       doneiso=0
       numiter=0

       Do 00004 while ((madechng .eq. 1) .and.
     c (numiter .le. maxiter))
        numiter=numiter+1
c ascent identifies the best possible move
         Call ascent
     C  (change,newgroup,group,numact,
     C   numgroup,newtheta,cell,marginal,groupn,madechng,maxwt,
     C   doneiso,iso,onbad,attached)

       If (madechng .eq. 1) then 
c if there is a move which increases theta1, make it
        Write(3,104) numiter,change,group(change),newgroup,
     c log(newtheta)/2
        Write(6,104) numiter,change,group(change),newgroup,
     c  log(newtheta)/2
c if change reassigns a member of a triad, then the remaining
c dyad is dissolved by reassigning one of the members of the dyad.
        if (groupn(group(change)) .eq. 3) then
        found=0
        s=0
        do 00034 while (found .eq. 0)
        s=s+1
        if (s .eq. change) then
        s=s+1
        end if
        if (group(s) .eq. group(change)) then
        found=1
c opengrp finds an available group.  Output in xgroup
        call opengrp(groupn,xgroup,numgroup)

c reassign reassigns an actor to a new gorup, and updates marginals
c and cells of table 1.

        Call reassign
     C  (s,xgroup,group,numact,groupn,marginal,
     c   cell,maxwt,newtheta)
         end if
c ending on if (group(s) .eq. group(change)) then

00034     continue
        end if
c ending on if (groupn(group(change)) .eq. 3) then

c make the reassignment that improves theta1
        Call reassign
     C  (change,newgroup,group,numact,groupn,marginal,
     c   cell,maxwt,newtheta)

       End if

       If (madechng .eq. 0) then
       write(3,4308) "Could not increase theta1"
       write(6,4308) "Could not increase theta1"

c identify isolates to be seeds for new subgroup
       numseed=0
       numiso=0
       do 30 s=1,numact
       iso(s)=0
       if ((groupn(group(s)) .le. 1) .and. (onbad(s) .eq. 0)) then
         iso(s)=1
         if (attached(s) .eq. 0) then  
         numiso=numiso+1 
         end if
c only counts as an isolate for attaching if it hasn't been 
c attached before

         if (useseed(s) .eq. 0) then 
           numseed=numseed+1
         end if
       end if
00030   continue

        IF ((NUMSEED .GT. 2) .AND. (DONEISO .EQ. 0)) THEN
       newtheta=0
c identify a new seed
       call KMULTMA2 (numact,useseed,seed,iso,rchoice,
     c domul,writemul,preseed)

       if ((seed(1) .gt. 0) .and. (seed(2) .gt. 0) .and.
     c  (seed(3) .gt. 0)) then
        madechng=1
c reassign members of seed to the group of the first seed actor
        do 16 s=2,3

        Call reassign
     C  (seed(s),group(seed(1)),group,
     C  numact,groupn,marginal,cell,maxwt,newtheta)
00016    continue

        end if
c ending on if ((seed(1) .gt. 0) .and. (seed(2) .gt. 0) .and.
c          c  seed(3) .gt. 0)) then
       End if
c    ending on if ((doneiso .eq. 0) .and. (numseed .ge. 3)) then

        if ((madechng .eq. 0) .and. (numiso .gt. 0) .and.
     c  (attachi .eq. 1)) then
c if there is no seed, and attachi=1, then 
c attach unaffiliated actors

          write(3,4308)
          write(3,4308) "****************************************"
          if (doneiso .eq. 0) then
          write(3,4308) "ATTACHING UNAFFILIATEDS"
          write(6,4308) "ATTACHING UNAFFILIATEDS"
          end if
          if (doneiso .eq. 1) then
          write(3,4308) "IDENTIFIED AND ATTACHING NEW UNAFFILIATEDS"
          write(6,4308) "IDENTIFIED AND ATTACHING NEW UNAFFILIATEDS"
          end if
          WRITE(3,4308) "CONTINUING ASCENT"
          write(3,4308) "****************************************"
          write(3,4308)
      write(3,4308)
      write(3,4308)  "     |      | old | new |"   
      write(3,4308)  "iter-|change|sub- |sub- |"  
      write(3,4308)  "ation|actor |group|group|         theta1" 
      write(3,4308)  "----------------------------------------"
      wrITE(6,4308) "CONTINUING ASCENT"
      write(6,4308) "****************************************"
      write(6,4308)
      write(6,4308)
      write(6,4308)  "     |      | old | new |"   
      write(6,4308)  "iter-|change|sub- |sub- |"  
      write(6,4308)  "ation|actor |group|group|         theta1" 
      write(6,4308)  "----------------------------------------"

          doneiso=1
c sets up ascent to attach unaffiliates, even if it doesn't
c improve theta1.

          madechng=1
        end if
       end if
c        ending on if madechnge .eq. 0
00004 end do
       write(3,4308)
       write(3,4308) "Ascent completed"
       if (removed .eq. 1) then
       write(3,4308) "attaching tag alongs"
       write(3,4308) "(connected to only one other)"
       write(3,4308) "tag,actor,subgroup"

        do 191 s=1,numact
        if (pair(s) .gt. 0) then
        write(3,101) s,pair(s),group(pair(s))
        Call reassign
     C  (s,group(pair(s)),group,numact,groupn,marginal,
     c   cell,maxwt,newtheta)
        end if
00191 continue
         end if
c  ending on if removed .eq. 1


       close(3)

c for UCINET: at this point actors have been placed in groups,
c data contained in the array: group, where group(actor) indicates
c the group of which the actor is a member.

       write(6,4308) 
       write(6,4308) "KliqueFinder has produced:" 
       write(6,4308) "kliqfind.log: containing the sequence of"
       write(6,4308) "steps executed to maximize theta1,"
       write(6,4308)
       write(6,4308) "kliqfind.out: containing the main output"
       write(6,4308) "indicating subgroup memberships, and"
       write(6,4308) "evaluation of the subgroups," 
       write(6,4308)
       write(6,4308) "kliqfind.plc: a datafile containing actor ID's" 
       write(6,4308) "and corresponding subgroup memberships, and"
       write(6,4308) 
       write(6,4308) "kliqfind.uci: containing the original data in" 
       write(6,4308) "ucinet3.0 format."
       write(6,4308)
       write(6,4308) "KliqueFinder is copywritten.  Output and source"
       write(6,4308) "code may not be reproduced without the expressed"
       write(6,4308) "written consent of the author, Ken Frank."
        open(3,file="kliqfind.out")

c      CALL FDATE(idate) 
         
      WRITE(3,4308) "Welcome to KliqueFinder, software for identifying"
      WRITE(3,4308) "cohesive subgroups.  For references see:"
      write(3,4308)
      WRITE(3,4308) "Frank, K.A. (1996). Mapping interactions within" 
      WRITE(3,4308) "and between cohesive subgroups. Social Networks."
       WRITE(3,4308) "Volume 18, pages 93-119."
      WRITE(3,4308)
      WRITE(3,4308) "and"
      WRITE(3,4308)
      WRITE(3,4308) "Frank. K.A. (1995). Identifying Cohesive "
      WRITE(3,4308) "Subgroups. Social Networks, 17, 27-56."
      WRITE(3,4308)
        
c      WRITE(3,4308) "Date"
c      WRITE(3,4308) DATE@()
      WRITE(3,4308) "Input file:"
      WRITE(3,4308) INFILE 
      WRITE(3,4308)
      write(3,4308) "This file is called kliqfind.out."
      write(3,4308) 
      write(3,4308) "***********************************************"
      write(3,4308) 
      write(3,4308) "Subgroup Sizes"
      write(3,4308) "Note that subgroups have been consecutively"
      write(3,4308) "reordered from their working subgroup numbers"
      write(3,4308) "as given in kliqfind.log.  Both the working"
      write(3,4308) "and final subgroup numbers are given below."
       g=0
       do 75 i=1,numgroup
       if (groupn(i) .gt. 0) then
        g=g+1
       groupid(i)=g
       end if
00075  continue
        write(3,4308)
        write(3,4308) "wrkng|final|"
        write(3,4308) "sub- |sub- |" 
        write(3,4308) "group|group|size"
       Do 78 i=1,numgroup
       if (groupn(i) .gt. 0) then
        write(3,101) i,groupid(i),groupn(i)
       end if
00078    continue
      WRITE(3,4301)
      write(3,4308) "***********************************************"
      write(3,4308) 
      write(3,4308) "Placement of Actors in Subgroups"
       write(3,4308)
        write(3,4308) "     |sub- |tagged"
        write(3,4308) "actor|group|  to"
        write(3,4308) 
       Open(33,file="kliqfind.plc") 
       Do 77 i=1,numact
       if (onbad(i) .eq. 0) then
       Write(3,101) i,groupid(group(i))
       Write(33,101) i,groupid(group(i))
       end if
       if (onbad(i) .eq. 1) then
       Write(3,101) i,groupid(group(i)),-1
       Write(33,101) i,groupid(group(i)),-1
       end if
       if (onbad(i) .eq. 2) then
       Write(3,101) i, groupid(group(i)),pair(i)
       Write(33,101) i, groupid(group(i)),pair(i)
       end if
00077 continue
       Close(33)
       write(3,4308)
       write(3,4308) "These data have also been written to"
       write(3,4308) "the file: kliqfind.plc"
       write(3,4308)
c      calculating predicted value of theta1 and whether 
c      there is evidence that the predicted value is exceeded

c centering choices and size of network, and using cut-offs based
c on range in simulations.
c         Call matprint (group,groupid,groupn,numact,maxwt,numgroup)
c matprint was a procedure for printing out data in matrix format,
c partitioned by subgroups (like FACTIONS).

      Write(3,4308) "Original network data rewritten in ucinet3.0"
      Write(3,4308) "format in kliqfind.uci."
      OPEN(11,file='kliqfind.uci')
      WRITE(11,21092) 4,NUMact,numact
     
      WRITE(11,7513) (i , i=1,(NUMact-1))
       WRITE(11,7514) NUMact
       DO 8866 I=1,NUMact
      WRITE(11,9101) (datamat(I,J) , J=1,NUMact)
08866 CONTINUE
        CLOSE(11)

        TNUM=tnumact-50
        XTNUM=TNUM
        IF (tNUMact .GT. 80) THEN
        XTNUM=31
        END IF
c     calculating the maximal choices that any actor makes
       fixr=0
       do 19 i=1,numact
         tempr=0
         do 20 j=1,numact
         if (datamat(i,j) .gt. 0) then
         tempr=tempr+1
         end if
        if (tempr .gt. fixr) then
        fixr=tempr
        end if
00020    continue
00019     continue

           
       TMAX=FIXR-12+1
       XTMAX=TMAX
       IF (FIXR .GT. 17) THEN
        XTMAX=7
       END IF  
c calculating predicted value of theta1 (see Frank, 1995, p. 39)
       
       PL=.790360+(.0051540)*XTNUM+(-.016847)*XTMAX+
     C (-.000027)*XTNUM**2+(.002689)*XTMAX**2+
     C (-.000424)*XTNUM*XTMAX

c use predicted value of theta1 to get predicted value for 
c cell(4) in Table 2.  See Frank, 1995, page 42.
       
       CALL CONVLAMN(PL,marginal(2),marginal(4),marginal(5),
     C PALPHA,PBETA,PGAMMA,PDELTA)
c plrt is the difference in deviances between
c model based on predicted value of theta1 and 
c observed value for theta1

       PLRT=2*(cell(1)*LOG(cell(1)/PALPHA)+
     C cell(2)*LOG(cell(2)/PBETA)+
     C cell(3)*LOG(cell(3)/PGAMMA)+
     C cell(4)*LOG(cell(4)/PDELTA))

      WRITE(3,4301)
      write(3,4308) "***********************************************"
      write(3,4308) 
      write(3,4308) "Concentration of exchanges within subgroups"
      WRITE(3,194) 
      WRITE(3,4308) "Predicted theta (1 base) based on simulations."
      WRITE(3,195) PL

       IF ((FIXR .GT. 17) .OR. (NUMact .GT. 80))  THEN
       IF (FIXR .GT. 17) THEN 
         WRITE(3,4308)
       WRITE(3,4308) "MAXIMUM NUMBER OF RELATIONS FOR A GIVEN ACTOR,"
         WRITE(3,195) FIXR
        WRITE(3,4308) "IS GREATER THAN THE VALUE OF 18"
        WRITE(3,4308) "IN THE SIMULATIONS."
       END IF

       IF (NUMact .GT. 80) THEN 
         WRITE(3,4308)
       WRITE(3,4308) "SIZE OF NETWORK,"
       WRITE(3,101) NUMact
       WRITE(3,4308)  "IS GREATER THAN THE MAX OF 80"
        WRITE(3,4308) "IN THE SIMULATIONS."
       END IF
         WRITE(3,4308)
       WRITE(3,4308) "WILL USE PREDICTION FOR THETA (1 base)
     c BASED ON"
C       WRITE(3,4308) "ON LINEAR TERMS AND QUADRATIC TERMS WITH "
       WRITE(3,4308) "MAXIMUM VALUES FROM SIMULATIONS."
       WRITE(3,4308) "INTERPRET WITH CAUTION!"
         WRITE(3,4308)
       WRITE(3,4308) "CONSIDER GENERATING A UNIQUE SAMPLING "
       WRITE(3,4308) "DISTRIBUTION.  SEE FRANK, 1995, PAGE 38."
       END IF

       WRITE(3,4308)
        TGLAMNDA=LOG(newtheta)/2.00
      WRITE(3,4308) "Estimate of theta (1 subgroup processes)"
      WRITE(3,195) (TGLAMNDA-PL)
       WRITE(3,4308)
      WRITE(3,4308) "The total theta1 is:"
      WRITE(3,195) TGLAMNDA
       WRITE(3,4308)


       WRITE(3,4308) "Theta (1 base) can be interpreted as half"
       WRITE(3,4308) "the log-odds of the predicted values,"
       WRITE(3,4308) "and the total theta1 as half the "
       WRITE(3,4308) "log-odds of the observed values, in Table 2."


       WRITE(3,4308) 
       Write(3,4308) "                         Table 2"
       Write(3,4308) "    Association between subgoup membership and"
       Write(3,4308) "              exchange between actors"
       Write(3,4308)
       Write(3,4308) "See Frank, 1995, page 33"

       Write(3,4308)
       Write(3,4308)
       WRITE(3,4308) "(PREDICTED)             CONNECTION    
     cMARGINAL"
       WRITE(3,4308) "OBSERVED             NO            YES"
       WRITE(3,4308) "              _______________________________"
       WRITE(3,4308) "             |              |                |"
       WRITE(3,4338) "          NO |(",pALPHA,")  | (",pbeta,")   |",
     c  marginal(1)
       WRITE(3,4348) "             |",cell(1),"    | ",cell(2),"     |"
       WRITE(3,4308) "IN SAME      |              |                |"
       WRITE(3,4308) "SUBGROUP     |--------------+----------------|"
       WRITE(3,4308) "             |              |                |"
       WRITE(3,4338) "         YES |(",pgamma,")  | (",pdelta,")   |",
     c  marginal(2)
       WRITE(3,4348) "             |",cell(3),"    | ",cell(4),"     |"
       WRITE(3,4308) "             |              |                |"
       WRITE(3,4308) "             --------------------------------"
       WRITE(3,4358) " MARGINAL",marginal(3),marginal(4),marginal(5)
         write(3,4308) 

       WRITE(3,4308) "For the observed values:"
       WRITE(3,4308) "      Odds ratio      Log odds       (Log odds/2)"
       WRITE(3,25144) newtheta,LOG(newtheta),(LOG(newtheta))/2
       WRITE(3,4308)
       WRITE(3,4308) "For the predicted values:"
         ODDSR=PDELTA*PALPHA/(PBETA*PGAMMA)
       WRITE(3,4308)
       WRITE(3,4308) "      Odds Ratio      Log odds      (Log odds/2)"
       WRITE(3,25144) ODDSR,LOG(ODDSR),(LOG(ODDSR))/2
       WRITE(3,4308)
       write(3,4308) "************************************"
        WRITE(3,4308)
       WRITE(3,4308) "Test of theta (1 subgroup processes)"
        WRITE(3,4308)
      WRITE(3,4308) "Change in deviance between models with"
      Write(3,4308) "and without theta (1 subgroup processes):"
      WRITE(3,195) PLRT
       WRITE(3,4308)
      WRITE(3,4308) "Compare to a chi-square on 1 df."
       WRITE(3,4308)
       WRITE(3,4308) "This p-value is based on the deviances"
        WRITE(3,4308) "of the models:"
       WRITE(3,4308)
       WRITE(3,4308) "Log P(Xii'=xii')=theta0+theta1base(samegroup)"
       WRITE(3,4308)
       WRITE(3,4308) "Log P(Xii'=xii')=theta0+theta1base(samegroup) +"
       WRITE(3,4308) " theta1 subgroup processes(samegroup) "
       WRITE(3,4308)
       WRITE(3,4308) "A small p-value indicates that one can"
       WRITE(3,4308) "reject the null hypothesis that "
       WRITE(3,4308) "theta (1 subgroup processes) is zero, -->"
       WRITE(3,4308) "actors engage in exchanges within subgroups at"
       WRITE(3,4308) "a rate that is unlikely to have occurred"
       WRITE(3,4308) "by application of the algorithm to random data."
       Write(3,4308) "See Frank, 1995, pages 38-39."
       write(3,4308) 
       write(3,4308)  "A conservative (see Frank, 1995, page 39)"
        write(3,4308) "value of theta (1 subgroup processes) is"
        write(3,195) (pl-.032)
        write(3,4308) 
       write(3,4308) "This is associated with change in deviance of"
       write(3,4308)
        CALL   CONVLAMN((PL+.032),marginal(2),marginal(4),marginal(5),
     C PALPHA,PBETA,PGAMMA,PDELTA)

       PLRT=2*(cell(1)*LOG(cell(1)/PALPHA)+
     C cell(2)*LOG(cell(2)/PBETA)+
     C cell(3)*LOG(cell(3)/PGAMMA)+
     C cell(4)*LOG(cell(4)/PDELTA))
          write(3,195) plrt
          write(3,4308)
          write(3,4308) "This can also be compared to a chi-square"
          write(3,4308) "distribution on one degree of freedom."


      WRITE(3,4301)


      MAXSIZE=0
      DO 62614 J=1,numgroup
      IF (groupn(J) .GT. MAXSIZE) THEN
      MAXSIZE=groupn(j)
      END IF
62614 CONTINUE

c   predacc is the predicted accuracy of the algorithm (see Frank,
c 1995, page 44).
      PREDACC=1.4572+.7009*(PL-.8993) + 
     c 1.1977*(LOG(newtheta)/2-PL-.3692)
     C + .0867*(MAXSIZE-5.737) + .0038*(tNUMact-51.7488)
   
      WRITE(3,4301)
      write(3,4308) "***********************************************"
      write(3,4308) 
      WRITE(3,4308) "Predicted accuracy: Log-odds of common subgroup"
      WRITE(3,4308) "membership, + OR - .5734 (FOR A 95% CI)"
      WRITE(3,195) PREDACC
       WRITE(3,4308)
       WRITE(3,4308) "The Log odds applies to the following table (3):"
       WRITE(3,4308)
       Write(3,4308) "                       Table 3"
       Write(3,4308) "            Predicted Accuracy of Algorithm" 
       write(3,4308)
       WRITE(3,4308) "                  OBSERVED SUBGROUP"
       write(3,4308)
       WRITE(3,4308) "                  DIFFERENT   SAME"
       WRITE(3,4308) "                 ___________________"
       WRITE(3,4308) "                 |        |        |"
       WRITE(3,4308) "       DIFFERENT |   A    |   B    |"
       WRITE(3,4308) "KNOWN            |        |        |"
       WRITE(3,4308) "SUBGROUP         |--------|--------|"
       WRITE(3,4308) "                 |        |        |"
       WRITE(3,4308) "            SAME |   C    |   D    |"
       WRITE(3,4308) "                 |        |        |"
       WRITE(3,4308) "                 -------------------"
       WRITE(3,4308)
      WRITE(3,4308) "The logodds translates to an odds ratio of "
      WRITE(3,195) (EXP(PREDACC))
      WRITE(3,4308) "which indicates the increase in the  probability"
      WRITE(3,4308) "that KliqueFinder will assign two actors to"
      WRITE(3,4308) "the same subgroup if they are known to be in the"
      WRITE(3,4308) "same subgroup."
      Write(3,4308) "(see Frank, 1995, page 44)."
      write(3,4308)
      write(3,4308) "KliqueFinder is copywritten.  Output and source"
      write(3,4308) "code may not be reproduced without the expressed"
      write(3,4308) "written consent of the author, Ken Frank."

      
       Close(3)
07513 FORMAT(251("AC",I3.3,",":))
07514  FORMAT ("AC",I3.3)
21092 FORMAT(I1,",",I3,",",I3,",N,NM")
09101   FORMAT(251(I4.4,1X))
25144 FORMAT(150(F15.5)) 
04348 format(A,F10.0,A,F10.0,A)
04338 format(A,F10.2,A,F10.2,A,I8)
04358 format(A,I14,I16,I15)
00104 format (4I5,F20.10)
00100 format (3I5,F20.10)
00101 format (5I5)
00254  FORMAT (5I5)
00103 format (5I10)
04301 FORMAT(20(A))
  194 FORMAT(/,10(2X,A15),/)
  195 FORMAT(/,10(2X,F15.5),/)   
04308 FORMAT(A)
        End

         Subroutine reassign
     C  (change,newgroup,group,numact,groupn,marginal,
     c   cell,maxwt,newtheta)
c this procedure reassigns actors to a new subgroup and updates 
c marginals and cells of Table 2

       Integer change,newgroup,connect(100,100),group(100),
     C datamat(100,100),i,numact,oldgroup,groupn(100),marginal(5),
     c maxwt
        real newtheta,cell(4)
c       Connect contains the connections from actors to groups
c       datamat contains the matrix with the data
c       Numact is number of actors
c       group indicates the group membership of each actor
c       Change indicates the actor to be changed
c       Newgroup indciates the group the actor should change to
c       Oldgroup is the current group of the change actor
c
c cell, numact,groupn,marginal,maxwt are all defined as in main program

        common datamat,connect 

       oldgroup=group(change)
c updating connections to groups -- only for group which changed
      do 2 i=1,numact
       connect(i,oldgroup)=connect(i,oldgroup)-
     C (datamat(i,change) + datamat(change,i))
       connect(i,newgroup)=connect(i,newgroup)+
     C (datamat(i,change) + datamat(change,i))
00002 continue

c updating marginals and cells
      marginal(2)=marginal(2)+
     C      maxwt*2*(groupn(newgroup)-groupn(oldgroup)+1)
      marginal(1)=marginal(5)-marginal(2)
      cell(4)=cell(4)-connect(change,oldgroup)+
     c                connect(change,newgroup)
      cell(3)=marginal(2)-cell(4)
      cell(1)=marginal(3)-cell(3)
      cell(2)=marginal(4)-cell(4)

c updating theta and group sizes
      newtheta=99999
      if ((cell(3) .gt. 0) .and. (cell(2) .gt. 0)) then
      newtheta=cell(4)*cell(1)/(cell(3)*cell(2))
      end if
      Groupn(newgroup)=groupn(newgroup)+1
      Groupn(oldgroup)=groupn(oldgroup)-1
      group(change)=newgroup
      Return
      End

      Subroutine ascent
     C (change,newgroup,group,numact,
     C  numgroup,newtheta,cell,marginal,groupn,madechng,
     C  maxwt,doneiso,iso,onbad,attached)
       Real newtheta,ttheta,oldtheta,cell(4),tempcell(4),
     C isotheta
       Integer change,newgroup,connect(100,100),group(100),
     C datamat(100,100),i,j,numact,numgroup,doneiso,iso(100),
     C marginal(5),tmarg2,groupn(100),madechng,onbad(100),
     C maxwt,atcell4,isochang,isogroup,attached(100),oldchng
c       Connect contains the connections from actors to groups
c       datamat contains the matrix with the data
c       Numact is number of actors
c       group indicates the group membership of each actor
c       Groupn indicates the sizes of the groups
c       Change indicates the actor to be changed
c       Newgroup indciates the group the actor should change to
c       Oldgroup is the current group of the change actor
c       Newtheta is the new optimal value of theta1 that will
c       emerge from this procedure
c numgroup, cell, marginal, groupn, madechng, maxwt,
c doneiso, iso and onbad are all defined as in main program

c        Cell contain the values of cells A through D as defined
c        in the table below
c        Marginals contains the marginals from the table below
c       (fifth element is the total)
c
c                       Connect
c                    No          Yes    Marginal
c                +---------+----------+
c                |         |          |
c            No  |   A     |     B    |  1
c    Same        |  (1)    |    (2)   |
c    Group       +---------+----------+---
c                |         |          |
c            Yes |   C     |     D    |  2
c                |  (3)    |    (4)   |
c                +---------+----------+---
c    Marginal    |   3     |     4    |  5
c
c        Tempcell = temporary values of cells
c        Tmarg2 = temporary values of marginal 2
c
c setting up base values

        common datamat,connect 
        oldchng=change
        Oldtheta=newtheta+.000001
        isotheta=-999999
c isotheta is lower because any change for an siolate will be made.
        isochang=-1
        isogroup=-1

        Do 2 i=1,numact
        if ((i .ne. oldchng) .and. (onbad(i) .eq. 0)) then
c don't process for actors on badlist
        atcell4=cell(4)-connect(i,group(i))
        Do 3 j=1,numgroup
         If ((groupn(j) .gt. 1) .and. (j .ne. Group(i))) then
            Tempcell(4)=atcell4+connect(i,j)
            Tmarg2=marginal(2)+
     C      maxwt*2*(groupn(j)-groupn(group(i))+1)
c    calculating new marginal with 
c    The changes in group sizes.

            Tempcell(3)=tmarg2-tempcell(4)
            Tempcell(1)=marginal(3)-tempcell(3)
            Tempcell(2)=marginal(4)-tempcell(4)
            ttheta=9999999
            if ((tempcell(3) .gt. 0) .and. (tempcell(2) .gt. 0)) then
            Ttheta=tempcell(1)*tempcell(4)/
     C               (tempcell(3)*tempcell(2))
c now have new value of theta if actor i were changed to group j
            end if 
c
c identifying the best isolate
            if ((doneiso .eq. 1) .and. (iso(i) .eq. 1) .and. 
     c          (ttheta .gt. isotheta) .and.
     c          (attached(i) .eq. 0)) then
              isotheta=ttheta
              isochang=i
              isogroup=j
            end if
c identifying the best overall
            If (ttheta .gt. Newtheta) then
             Newtheta=ttheta
             Change=i
             Newgroup=j
             if (ttheta .eq. 9999999) then
              whathap=1
c just a flag for debugging
             end if
            End if
c    ending on If (ttheta .gt. Newtheta) then

          End if
c          Ending on only if group(i) ne j
00003     Continue
          end if
00002     Continue
          Madechng=0
          If (Newtheta .gt. oldtheta) then 
          Madechng=1
          End if
          If ((madechng .eq. 0) .and. (isogroup .ne. -1) .and.
     c    (attached(isochang) .eq. 0)) then
c make the isolate change if you're doing isolates and no other
c change could be made.
          attached(isochang)=1
          madechng=1
          change=isochang
          newgroup=isogroup
          iso(change)=0
          end if    
          
          Return
          End

       SUBROUTINE KMULTMA2(SIZE,ONLIST,person,
     c  iso,rchoice,domul,writemul,preseed)
c this procedure performs a matrix multiplication to identify
c triads of actors who are connected to each other and common others.
C see Frank, 1995, page 34.  Members of the best triad will be 
c placed in person(3).

        INTEGER SIZE,I,J,K,A,ONLIST(100),PERSON(3),
     c  datamat(100,100),iso(100),rchoice(100),
     c  domul,writemul,preseed,connect(100,100)

c all incoming variables as defined in main procedure.
C onlist=useseed.  Person=seed.  Don't ask why.

          REAL WTXPX,WTXXP,BESTTRID,connecto,kdirect
        DOUBLE PRECISION TXPX,TXXP

        common datamat,connect 

c initializing seeds.

        PERSON(1)=0
        PERSON(2)=0
        PERSON(3)=0
        BESTTRID=0
c for reading back in results.  Turned out to hurt efficiency.

      if (domul .eq. 0) then
      open(33,file='kliqfind.xxp')
      found=0
      do 73 while (found .eq. 0)
      read(33,502,end=72) i,j,k,connecto
          IF ((CONNECTO .GT. BESTTRID) .and.
     C     (iso(i) .eq. 1) .and. (onlist(I) .EQ. 0) .and.
     C     (iso(j) .eq. 1) .and. (onlist(j) .EQ. 0) .and.
     C     (iso(k) .eq. 1) .and. (onlist(k) .EQ. 0)) then

          PERSON(1)=I
          PERSON(2)=J
          PERSON(3)=K
          BESTTRID=CONNECTO
          END IF
00073   continue
00072   close(33)
       end if

       if (domul .eq. 1) then  
       if (writemul .eq. 1) then
       open(33,file="kliqfind.xxp")  
       end if
       KDIRECT=1
c count the direct connections with a weight of 1

       WTXPX=1
c count values in xpx with a weight of 1

       WTXXP=1
c count values in xxp with a weight of 1
     

         DO 30 I=3,SIZE
           IF ((iso(i) .eq. 1) .and. (onlist(I) .EQ. 0)) then 
           DO 40 J=2,(I-1)

           IF ((iso(j) .eq. 1) .and. (onlist(j) .EQ. 0)) then 
            DO 50 K=1,(J-1)
           IF ((iso(k) .eq. 1) .and. (onlist(k) .EQ. 0)) then 
c only process eligible actors
         TXPX=0
         TXXP=0
         CONNECTO=0
         DO 60 A=1,SIZE
         if (rchoice(a) .gt. 0) then
         TXPX=TXPX+dataMAT(A,I)*dataMAT(A,J)+
     C                        dataMAT(A,I)*datamat(A,K)+
     C                        dataMAT(A,J)*datamat(A,K)
         if (a .eq. i) then
          txpx=txpx+(datamat(a,j)+datamat(a,k))*kdirect
         end if
c kdirect indicates the extent to which direct connections should be 
c counted

         end if
          TXXP=TXXP+dataMAT(I,A)*dataMAT(J,A)+
     C                           dataMAT(I,A)*datamAT(K,A)+
     C                           dataMAT(J,A)*dataMAT(K,A)
         if (a .eq. j) then
          txxp=txxp+datamat(i,a)*kdirect
          txpx=txpx+datamat(a,j)*kdirect
         end if
          if (a .eq. k) then
          txxp=txxp+datamat(i,a)+datamat(j,a)*kdirect
          end if
   60    CONTINUE

          CONNECTO=TXPX*WTXPX+TXXP*WTXXP
c the matrix multiplication and sum.
          if (writemul .eq. 1) then
           write(33,502) i,j,k,connecto
          end if
c identifying the best triad
          IF (CONNECTO .GT. BESTTRID) THEN
          PERSON(1)=I
          PERSON(2)=J
          PERSON(3)=K
          BESTTRID=CONNECTO
          END IF

          END IF
   50    CONTINUE
         END IF
   40  CONTINUE
         END IF
   30 CONTINUE
      end if
c     domul=1
      if (writemul .eq. 1) then 
      close(33)
      end if 
c assigning the best triad to the variable person
       if (person(1) .gt. 0) then
      onlist(person(1))=1
        end if
       if (person(2) .gt. 0) then
      onlist(person(2))=1
        end if
       if (person(3) .gt. 0) then
      onlist(person(3))=1
       end if
      write(3,4308)
      write(3,4308) "*************************************"
      write(3,4308) "Starting new subgroup"
      write(3,4308) "actor|actor|actor|"
      write(3,4308) "  1  |  2  |  3  |connection"
      write(3,503) person(1),person(2),person(3),besttrid 
      write(6,4308) "*************************************"
      write(6,4308) "Starting new subgroup"
      write(6,4308) "actor|actor|actor|"
      write(6,4308) "  1  |  2  |  3  |connection"
      write(6,503) person(1),person(2),person(3),besttrid 
      write(3,4308) "*************************************"
      write(3,4308)
      if (preseed .eq. 0) then  
      write(3,4308) "reinitiating ascent"
      write(6,4308) "reinitiating ascent" 
      end if
      if (preseed .eq. 1) then
      write(3,4308) "Begin Ascent"
      write(6,4308) "Begin Ascent" 
      end if 
      write(3,4308)
      write(3,4308)  "     |      | old | new |"   
      write(3,4308)  "iter-|change|sub- |sub- |"  
      write(3,4308)  "ation|actor |group|group|         theta1" 
      write(3,4308)  "----------------------------------------"
      write(6,4308)
      write(6,4308)  "     |      | old | new |"   
      write(6,4308)  "iter-|change|sub- |sub- |"  
      write(6,4308)  "ation|actor |group|group|         theta1" 
      write(6,4308)  "----------------------------------------"


      RETURN
04308 format(A)
  502 FORMAT(3I3,F20.10)
00503  format(3I5,F13.5)
      END

       SUBROUTINE CONVLAMN(PL,M2X,MX2,TOTAL,
     C ALPHA,BETA,GAMMA,DELTA)

       REAL PL,ALPHA,BETA,GAMMA,DELTA,le,
     C QA,QB,QC,INROOT,TMX2,THETA1,THETA2,
     C LTHETA1,LTHETA2,DELTA1,DELTA2,GAMMA1,GAMMA2,
     C BETA1,BETA2,ALPHA1,ALPHA2

       INTEGER M2X,MX2,TOTAL,MX1,NEGO1,NEGO2,datamat(100,100),
     c connect(100,100)
c this procedure uses values of theta1 and marginals
c of table1 to identify value in cells of table 2.
C m2x is marginal(2), mx2 is marginal(4), total is marginal(5)
c pl is predicted value of theta1,
c alpha, beta, gamma, and delta are cells of table 2.
C see Frank, 1995, page 42.  QA=a, QB=b

        common datamat,connect

       IF ((M2X .GT. 0) .AND. (MX2 .GT. 0) .AND. (TOTAL .GT. 0)) THEN
       MX1=TOTAL-MX2
       LE=2.718281828**(2*PL)
       QA=LE - 1
       QB=-(LE*M2X+LE*MX2+MX1-M2X)
       QC=LE*M2X*MX2
       INROOT=QB**2-4*QA*QC

       IF (INROOT .GT. 0) THEN
       DELTA1=(-QB+SQRT(INROOT))/(2.0000*QA)

       BETA1=MX2-DELTA1
       GAMMA1=M2X-DELTA1
       ALPHA1=MX1-GAMMA1
       THETA1=DELTA1*ALPHA1/(BETA1*GAMMA1)

       DELTA2=(-QB-SQRT(QB**2-4*QA*QC))/(2*QA)
       BETA2=MX2-DELTA2
       GAMMA2=M2X-DELTA2
       ALPHA2=MX1-GAMMA2
       THETA2=DELTA2*ALPHA2/(BETA2*GAMMA2)

       DELTA=DELTA1
       ALPHA=ALPHA1
       BETA=BETA1
       GAMMA=GAMMA1
       LTHETA1=LOG(THETA1)/2.00
       LTHETA2=LOG(THETA2)/2.00

       TMX2=MX2
       IF (TMX2 .GT. M2X) THEN
       TMX2=M2X
       END IF

       TMX2=TMX2-.05

       NEGO1=0
        IF ((DELTA .LT. 0) .OR. (ALPHA .LT. 0) .OR.
     C (BETA .LT. 0) .OR. (GAMMA .LT. 0)) THEN
       NEGO1=1
       END IF

       NEGO2=0
        IF ((DELTA2 .LT. 0) .OR. (ALPHA2 .LT. 0) .OR.
     C (BETA2 .LT. 0) .OR. (GAMMA2 .LT. 0)) THEN
       NEGO2=1
       END IF
c avoiding negative values in cells 1 through 4

       IF ((DELTA1 .GT. TMX2) .OR.
     C (ABS(LTHETA1-PL) .GT. ABS(LTHETA2-PL)) .OR.
     C ((NEGO1 .EQ. 1) .AND. (NEGO2 .EQ. 0))) THEN 
c using the root which closest genaerates the predicted
c value of theta1, providing no negative cells.
       DELTA=DELTA2
       ALPHA=ALPHA2
       BETA=BETA2
       GAMMA=GAMMA2
       END IF
        IF ((NEGO1 .EQ. 1) .AND. (NEGO2 .EQ. 1)) THEN 
        DELTA=TMX2-1.0
       BETA=MX2-DELTA
       GAMMA=M2X-DELTA
       ALPHA=MX1-GAMMA
       THETA=DELTA*ALPHA/(BETA*GAMMA)
       END IF
       END IF

C       INROOT=0

       IF ((INROOT .LE. 0) .OR. (DELTA .LT. 0)) THEN
c Bail out, use marginals!
       DELTA=MX2*M2X/TOTAL
       BETA=MX2-DELTA
       GAMMA=M2X-DELTA
       ALPHA=MX1-GAMMA
       END IF       


       END IF

       RETURN
       END

        subroutine opengrp(groupn,newgroup,numgroup)
        integer groupn(100),newgroup,numgroup,s,datamat(100,100),
     c  connect(100,100)
c this subgroutine finds the next available (empty) group 
c newgroup will contain the number of the available group

        common datamat,connect

        s=0
        newgroup=0
        do while (newgroup .eq. 0)
          s=s+1
          if (groupn(s) .eq. 0) then 
            newgroup=s
          end if
          if (s .gt. numgroup) then
            newgroup=s
            numgroup=numgroup+1
          end if 
        end do
        return
        end


      SUBROUTINE REMOVEO(OLENGTH,onbad,tnumact,pair,removed)
c this subroutine identifies actors to be removed from
c the matric because they are connected to zero or
c only one other.
C olength=numact, omat=datamat, onbad, tnumact and pair
c are all defined as in main program.

      INTEGER OMAT(100,100),OLENGTH,onbad(100),col,row,appendx,
     C removed,madechng,degree,tnumact,pair(100),connect(100,100)
c appendx will be temporary number of actor on whom
c i tags along.
       common omat,connect

        removed=0
       tnumact=olength
      do 17 row=1,olength
       onbad(row)=0
       pair(row)=0
00017   continue
   
c will have to keep looping as actors are removed.   

      MADECHNG=1

      DO 24 WHILE (MADECHNG .EQ. 1)

      MADECHNG=0
      DO 25 ROW=1,OLENGTH
       if (onbad(row) .eq. 0) then
       DEGREE=0
       DO 26 COL=1,OLENGTH
        
        IF ((onbad(col) .eq. 0) .and.
     c       (omat(col,row) .gt. 0) .or. (OMAT(ROW,COL) .gt. 0)) THEN
        DEGREE=DEGREE+1
        APPENDX=COL

c appendx will be the actor with whom i is paired.

        END IF
   26 CONTINUE


      if (degree .lt. 2) then  
       removed=1

       MADECHNG=1
       if (degree .eq. 1) then
       ONBAD(ROW)=2
         pair(row)=appendx
         WRITE(3,237) ROW,
     C " connected to only one other actor in the
     c network.  The actor can be considered in the subgroup of ",
     c APPENDX
       end if
       IF (DEGREE .EQ. 0) THEN
       onbad(row)=1
       tnumact=tnumact-1
         WRITE(3,237) ROW,
     C  " connected to no others in the network."
       END IF
      END IF
c        if (degree .lt. 2) then
      end if
c      onbad (row) eq 0)
   25 CONTINUE

00024  continue

      RETURN

  215 FORMAT(I5,1X,I3)
  216 FORMAT(10i5) 
  237 FORMAT("Removing actor ",I4," because the actor ",A,I4)

      END
