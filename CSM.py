# CROPPING SYSTEM MODEL
#USE ModuleData
#USE HeaderMod

from ModuleDefs import *
from DATES import *

def CSM():
# EXTERNAL CHECKRUNMODE, ERROR, FIND, IGNORE, INCYD, INFO,
# &  LAND, OPNAMES, PATHD, RUNLIST, TIMDIF, YR_DOY

    from UTILS import OPCLEAR, INQUIRE
    from INPUT_SUB import INPUT_SUB
    from ERROR import ERROR
    from RunList import RunList
    from LAND import LAND
    # from DATES import YR_DOY,TIMDIF,INCYD

#-----------------------------------------------------------------------
#      CHARACTER*78  MSG(10)

    YRPLT : int = -99
    FEXIST = False

    ERRKEY = 'CSM   '
    BLANK  = ' '

# Define constructed variable types based on definitions in ModuleDefs.for.
    CONTROL = ControlType()
    ISWITCH = SwitchType()

#-----------------------------------------------------------------------

    DONE = False
    YRDOY_END = 9999999

# Pick up model version for setting the name of some files
# WRITE(ModelVerTxt,'(I2.2,I1)') Version%Major, Version%Minor
    ModelVerTxt = ' 1.0'

# Delete existing output files
    OPCLEAR()

#  Assigns unique output file unit numbers to input and output files
#  based on file variable name.  If valid file variable name is not
#  specified, unit numbers are assigned incrementally starting with
#  unit 500.
#  CALL GETLUN('FILEIO', LUNIO)

    FILEIO = 'DSSAT48.INP'

#-----------------------------------------------------------------------
#    Get argument from runtime module to determine path of the EXE files
#-----------------------------------------------------------------------
     #  CALL GETARG(0,PATHX)   !,IPX
     #  CALL GETARG(1,DUMMY)   !,IP
     #
     #  IF ((DUMMY(1:1) .NE. BLANK) .AND. (DUMMY(2:2) .EQ. BLANK))
     # &    THEN
     #    CALL GETARG(1,RNMODE)   !,IP
     #    NARG = 1
     #    CALL CheckRunMode(RNMODE)
     #  ELSE
     #    CALL GETARG(1,MODELARG)  !,IP
     #    CALL GETARG(2,RNMODE)    !,IP
     #    CALL CheckRunMode(RNMODE)
     #    NARG = 2
     #  ENDIF
    PATHX = 'C:/DSSAT48/DSCSM048.EXE'
    DUMMY = 'CRGRO048'
    MODELARG = 'CRGRO048'
    RNMODE = 'B'
    NARG = 2

#-----------------------------------------------------------------------
#     RNMODE:
#      A - Run all treatments.  User specifies fileX on the command
#          line and the model runs all treatments
#      B - Batch mode. User defines fileX and treatment numbers in
#          Batch file
#      C - Command line mode.  Use input from the command line.
#      D - Debug mode.  Model skips input module and reads temp
#          file from the command line
#      E - Sensitivity analysis.  User defines fileX and treatment
#          number in Batch file
#      F - Farm model.  Use Batch file to define experiment
#      G - Gencalc. Use Command line to define experiment and treatment
#      I - Interactive mode.  Use model interface for exp. & trtno.
#      L - Gene based model (Locus). Use Batch file to define experiment
#      N - Seasonal analysis. Use Batch file to define experiment and
#          treatments
#      Q - Sequence analysis. Use Batch file to define experiment
#      S - Spatial.  Use Batch file to define experiment
#      T - Gencalc. Use Batch file to define experiments and treatment
#      Y - Yield forecast mode. Use batch file.
#-----------------------------------------------------------------------

    RNMODE = RNMODE.upper()
    ROTNUM = 0
    TRTNUM = 0

#       SELECT CASE(RNMODE)
#
# !     Read experiment file from command line -- run all treatments
#       CASE('A')   !run All treatments
#         CALL GETARG(NARG+1,FILEX)   !,IP   !Experiment file
#         CALL GETARG(NARG+2,FILECTL) !,IP   !Simulation control file name
#
# !     Read experiment file and treatment number from command line
#       CASE('C','G')   !Command line, Gencalc
#         CALL GETARG(NARG+1,FILEX)   !,IP   !Experiment file
#         CALL GETARG(NARG+2,TRNARG)  !,IP   !Treatment number
#         CALL GETARG(NARG+3,FILECTL) !,IP   !Simulation control file name
#         READ(TRNARG,'(I6)') TRTNUM
#
# !     Get experiment and treatment from batch file
#       CASE('B','N','Q','S','F','T','E','L','Y')
# !           Batch, seasoNal, seQuence, Spatial,
# !           Farm, Gencalc(T), sEnsitivity, Locus, Yield forecast
#         CALL GETARG(NARG+1,FILEB)   !,IP   !Batch file name
#         CALL GETARG(NARG+2,FILECTL) !,IP   !Simulation control file name
#
# !     Debug mode -- bypass input module and read FILEIO
#       CASE ('D')  !Debug
#         CALL GETARG(NARG+1,FILEIO)  !,IP   !INP file
#         DO I = 1, LEN(FILEIO)
#           FILEIO(I:I) = UPCASE(FILEIO(I:I))
#           ROTNUM = 0
#           TRTNUM = 0
#         END DO
#
# !     Interactive mode, no command line arguments
#       CASE DEFAULT    !Interactive mode.
#         RNMODE = 'I'
#       END SELECT
    FILEB = 'C:\\DSSAT48\\DSSBATCH.V48'


#-----------------------------------------------------------------------
#    Delete previouse copies of temporary input *.inp file
#-----------------------------------------------------------------------
#     IF (RNMODE .NE. 'D') THEN
#       INQUIRE (FILE = FILEIO,EXIST = FEXIST)
#       IF (FEXIST) THEN
#         OPEN (LUNIO, FILE = FILEIO,STATUS = 'UNKNOWN',IOSTAT=ERRNUM)
#         CLOSE (LUNIO,STATUS = 'DELETE')
#       ENDIF
#       LN = LEN(TRIM(FILEIO))
#       FILEIOH = FILEIO
#       # delete previous INH file
#       WRITE(FILEIOH(LN:LN),'(A1)') 'H'
#       INQUIRE (FILE = FILEIOH,EXIST = FEXIST)
#       IF (FEXIST) THEN
#         OPEN (LUNIO, FILE = FILEIOH,STATUS = 'UNKNOWN',IOSTAT=ERRNUM)
#         CLOSE (LUNIO,STATUS = 'DELETE')
#       ENDIF

#-----------------------------------------------------------------------
#    Open BATCH file
#-----------------------------------------------------------------------
#       IF (INDEX('NQSFBETY',RNMODE) .GT. 0) THEN
#          CALL GETLUN('BATCH ', LUNBIO)
#          FINDCH='$BATCH'
#          OPEN (LUNBIO, FILE = FILEB,STATUS = 'UNKNOWN',IOSTAT=ERRNUM)
#          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,28,FILEB,LINBIO)
#          CALL FIND (LUNBIO,FINDCH,LINBIO,IFIND)
#          IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,26,FILEB,LINBIO)
#       ENDIF
#     ENDIF

#-----------------------------------------------------------------------
#    Set run number and replication number
#-----------------------------------------------------------------------
    RUN   = 0
    REPNO = 1
    CONTROL.REPNO = REPNO

#***********************************************************************
#     RUN INITIALIZATION
#***********************************************************************
#      RUN_LOOP: DO WHILE (.NOT. DONE)
    while not DONE:
        YREND = -99
        RUN = RUN + 1
        CONTROL.RUN = RUN
        CONTROL.YRDOY = 0
        Put_CONTROL(CONTROL)

#      IF ((INDEX('NSFBTY',RNMODE) .GT. 0) .OR.
#     &    (INDEX('E',RNMODE) .GT. 0 .AND. RUN .EQ. 1)) THEN
#        CALL IGNORE (LUNBIO,LINBIO,ISECT,CHARTEST) # returns uncommented line from DSSBatch.v48
#        IF (ISECT .EQ. 1) THEN
#          END_POS = LEN(TRIM(CHARTEST(1:92)))+1
#          FILEX = CHARTEST((END_POS-12):(END_POS-1))
#          PATHEX = CHARTEST(1:END_POS-13)
#          READ(CHARTEST(93:113),110,IOSTAT=ERRNUM) TRTNUM,TRTREP,ROTNUM
# 110      FORMAT(3(1X,I6))
#          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,26,FILEB,LINBIO)
#        ELSE
#          DONE = .TRUE.
#          GO TO 2000
#        ENDIF
#      ENDIF
        # from uncommented line from DSSBatch.v48
        FILEX = 'UFBA1701.SRX'
        PATHEX = 'C:\\DSSAT48\\Strawberry\\'
        TRTNUM, TRTREP, ROTNUM = 1,1,0

#     IF (INDEX('Q',RNMODE) .GT. 0) THEN
#       CALL IGNORE (LUNBIO,LINBIO,ISECT,CHARTEST)
#       IF (ISECT .EQ. 0 .OR. RUN .EQ. 1) THEN
#         REWIND(LUNBIO)
#         FINDCH='$BATCH'
#         CALL FIND (LUNBIO,FINDCH,LINBIO,IFIND)
#         CALL IGNORE (LUNBIO,LINBIO,ISECT,CHARTEST)
#       ENDIF
#       END_POS = INDEX(CHARTEST,BLANK)
#       FILEX = CHARTEST((END_POS-12):(END_POS-1))
#       PATHEX = CHARTEST(1:END_POS-13)
#       READ (CHARTEST(93:113),110,IOSTAT=ERRNUM) TRTNUM,TRTREP,ROTNUM
#       IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,26,FILEB,LINBIO)
#     ENDIF

        CONTROL.FILEIO  = FILEIO
        CONTROL.FILEX   = FILEX
        CONTROL.RNMODE  = RNMODE
        CONTROL.ROTNUM  = ROTNUM
        CONTROL.TRTNUM  = TRTNUM
        CONTROL.ERRCODE = 0
        CONTROL.CropStatus = -99
        Put_CONTROL(CONTROL)

#C-KRT**************************************************************
        if RNMODE == 'A': PATHEX = ' '
#C-KRT*************************************************************

#-----------------------------------------------------------------------
#    Run INPUT module
#-----------------------------------------------------------------------
#      IF (RNMODE .NE. 'D') THEN
#        CALL INPUT_SUB(
#     &    FILECTL, FILEIO, FILEX, MODELARG, PATHEX,       !Input
#     &    RNMODE, ROTNUM, RUN, TRTNUM,                    !Input
#     &    ISWITCH, CONTROL)                               !Output
#      ELSE
#        FILEX = '            '    !Debug mode - no FILEX
#        CALL PATHD  (DSSATP,PATHX,LEN_TRIM(PATHX))
#        CONTROL % DSSATP = DSSATP
#      ENDIF

        if RNMODE != 'D':
            ISWITCH, CONTROL = INPUT_SUB( FILEIO, FILEX, MODELARG, PATHEX,
                                          RNMODE, ROTNUM, RUN, TRTNUM, CONTROL)
#-----------------------------------------------------------------------
#    Check to see if the temporary file exists
#-----------------------------------------------------------------------
        FEXIST = INQUIRE (FILE = FILEIO)
        if not FEXIST:
            ERROR(ERRKEY,2,FILEIO,LUNIO)
#
#       OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERRNUM)
#       IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,0)
#       READ (LUNIO,300,IOSTAT=ERRNUM) EXPNO,TRTNUM,TRTALL
#  300  FORMAT(36X,3(1X,I5))
#       IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,1)
#       READ (LUNIO,'(//,15X,A12)',IOSTAT=ERRNUM) FILEX
#       IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,1)
#       IF (RUN .EQ. 1) THEN
#         READ(LUNIO,'(8(/),A6,9X,A8)',IOSTAT=ERRNUM) FINDCH, FNAME
#         IF (RNMODE .EQ. 'Y' .AND. FINDCH .NE. 'OUTPUT') THEN
# !       There might be 2 weather files listed for yield forecast mode
#           READ(LUNIO,'(A6,9X,A8)',IOSTAT=ERRNUM) FINDCH, FNAME
#         ENDIF
#         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,13)
#         READ(LUNIO,400,IOSTAT=ERRNUM) NYRS, NREPS, YRSIM
#         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,15)
#  400    FORMAT(/,15X,I5,1X,I5,7X,I7)
#       ELSE IF (RNMODE .NE. 'Q') THEN
#         REWIND (LUNIO)
#         FINDCH = '*SIMUL'
#         CALL FIND(LUNIO, FINDCH, LNUM, FOUND)
#         IF (FOUND .EQ. 0) THEN
#           CALL ERROR(FINDCH, 42, FILEIO, LNUM)
#         ELSE
#           READ(LUNIO,500,IOSTAT=ERRNUM) NYRS, NREPS, YRSIM
#           IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,15)
#  500      FORMAT(15X,I5,1X,I5,7X,I7)
#           LNUM = LNUM + 1
#           IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
#         ENDIF
#       ENDIF
#       CLOSE(LUNIO)

        EXPNO = 1
        TRTNUM = 1
        TRTALL = 999
        FILEX = 'UFBA1701.SRX'
        FINDCH = 'OUTPUT'
        FNAME = 'OVERVIEW'
        NYRS = 1
        NREPS = 1
        YRSIM = 2017274

        if NYRS > 1:
            YRSIM_SAVE = YRSIM
#
#       IF (INDEX('FQ',RNMODE) .GT. 0) THEN
#         IF (RUN .EQ. 1) THEN
#           CALL YR_DOY(YRSIM,YR,ISIM)
#           YRDOY_END = (YR + NYRS) * 1000 + ISIM
#           YRDOY_END = INCYD(YRDOY_END, -1)
#         ENDIF
#         NYRS  = 1
#       ENDIF
#
        if 'Y'.find(RNMODE) > 0:
            REPNO = 1

        if RNMODE != 'Q' or RUN == 1:
            YRDOY = YRSIM

        MULTI  = 0
        YRDIF  = 0
        ENDYRS = 0
#
#       IF (INDEX('FQ',RNMODE).GT. 0 .AND. RUN .GT. 1) THEN
#          YRSIM = INCYD(YRDOY,1)
#          CALL YR_DOY(YRSIM_SAVE, YR0, ISIM0)
#          CALL YR_DOY(YRSIM,      YR,  ISIM)
#          YRDIF = YR - YR0
#          CONTROL % YRDIF = YRDIF
#       ENDIF

        CONTROL.FILEX   = FILEX
        CONTROL.NYRS    = NYRS
        CONTROL.MULTI   = MULTI
        CONTROL.RUN     = RUN
        CONTROL.TRTNUM  = TRTNUM
        CONTROL.YRDIF   = YRDIF
        CONTROL.YRDOY   = YRDOY
        CONTROL.YRSIM   = YRSIM
        CONTROL.DYNAMIC = RunConstants.RUNINIT
        Put_CONTROL(CONTROL)

        RunList(CONTROL)

#       WRITE(MSG(1),'("RNMODE = ",A)')  RNMODE
#       WRITE(MSG(2),'("PATHEX = ",A)')  PATHEX(1:67)
#       WRITE(MSG(3),'("FILEX  = ",A)')  FILEX
#       WRITE(MSG(4),'("FILEB  = ",A)')  FILEB
#       WRITE(MSG(5),'("FILEIO = ",A)')  FILEIO
#       WRITE(MSG(6),'("MODEL  = ",A)')  CONTROL % MODEL
#       WRITE(MSG(7),'("TRTNUM = ",I5)') TRTNUM
#       WRITE(MSG(8),'("ROTNUM = ",I5)') ROTNUM
#       IF (INDEX('FQ',RNMODE) > 0) THEN
#         CALL INFO(8,ERRKEY,MSG)
#       ELSE
#         CALL INFO(7,ERRKEY,MSG)
#       ENDIF

        YRPLT, MDATE, YREND = LAND(CONTROL, ISWITCH, YRPLT,YREND)

#***********************************************************************
#***********************************************************************
#-----------------------------------------------------------------------
#     BEGINNING of SEASONAL SIMULATION loop
#-----------------------------------------------------------------------
#     SEASONAL INITIALIZATION
#***********************************************************************
        while ENDYRS != NYRS:
# C***********************************************************************
            if NYRS > 1:
                ENDYRS = ENDYRS + 1
                if RNMODE != 'Y':
                    MULTI = MULTI + 1
            else:
                MULTI = 1
                ENDYRS = 1
#
#       IF (MULTI .GT. 1) THEN
#         RUN   = RUN + 1
#         CALL MULTIRUN(RUN, 0)  !chp 3/17/2011
#         YRSIM = YRSIM_SAVE
#         CALL YR_DOY(YRSIM,YR,ISIM)
#         YRSIM = (YR + MULTI - 1) * 1000 + ISIM
#         YREND = -99
#         IF (CONTROL%ErrCode /= 0) THEN
#           CONTROL%ErrCode = 0
# !         EXIT SEAS_LOOP
#           IF (INDEX('QY',RNMODE) > 0) EXIT SEAS_LOOP
#         ENDIF
#       ENDIF
#
# !     Forecast mode
#       IF (RNMODE .EQ. 'Y') THEN
#         IF (ENDYRS .GT. 1) THEN
#           RUN = RUN + 1
#           REPNO = REPNO + 1
#           CALL MULTIRUN(RUN, 0)
#           YREND = -99
#         ENDIF
#       ENDIF
#
            if RNMODE != 'Q' or RUN > 1:
                YRDOY = YRSIM

            CONTROL.DAS     = 0
            CONTROL.RUN     = RUN
            CONTROL.YRSIM   = YRSIM
            CONTROL.YRDOY   = YRDOY
            CONTROL.MULTI   = MULTI
            CONTROL.DYNAMIC = RunConstants.SEASINIT
            CONTROL.ENDYRS  = ENDYRS
            CONTROL.REPNO   = REPNO
            Put_CONTROL(CONTROL)

            YRPLT, MDATE, YREND = LAND(CONTROL,ISWITCH, YRPLT,YREND)

            YRDOY = INCYD(YRDOY,-1)
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#     BEGINNING of DAILY SIMULATION loop
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
            while YRDOY > YREND:
                YRDOY = INCYD(YRDOY,1)
#-----------------------------------------------------------------------
#     Calculate days after simulation (DAS)
#-----------------------------------------------------------------------
                YEAR,DOY = YR_DOY(YRDOY)
                DAS   = max(0,TIMDIF(INCYD(YRSIM,-1),YRDOY))
                CONTROL.YRDOY   = YRDOY
                CONTROL.DAS     = DAS
#***********************************************************************
#     RATE CALCULATIONS
#***********************************************************************
                CONTROL.DYNAMIC = RunConstants.RATE
                Put_CONTROL(CONTROL)

                YRPLT, MDATE, YREND = LAND(CONTROL, ISWITCH, YRPLT, YREND)

#***********************************************************************
#     INTEGRATION
#***********************************************************************
                CONTROL.DYNAMIC = RunConstants.INTEGR
                Put_CONTROL(CONTROL)

                YRPLT, MDATE, YREND = LAND(CONTROL, ISWITCH, YRPLT, YREND)
#***********************************************************************
#     OUTPUT
#***********************************************************************
                CONTROL.DYNAMIC = OUTPUT
                Put_CONTROL(CONTROL)

                YRPLT, MDATE, YREND = LAND(CONTROL, ISWITCH, YRPLT, YREND)

#***********************************************************************
#             ENDDO DAY_LOOP   !End of daily loop
#-----------------------------------------------------------------------
#             END of DAILY SIMULATION loop
#----------------------------------------------------------------------
#***********************************************************************
#     End of Season
# ***********************************************************************
            CONTROL.DYNAMIC = RunConstants.SEASEND
            Put_CONTROL(CONTROL)

            YRPLT, MDATE, YREND = LAND(CONTROL, ISWITCH, YRPLT, YREND)

# -----------------------------------------------------------------------
#      ENDDO SEAS_LOOP
# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
#      END of SEASONAL SIMULATION loop
# -----------------------------------------------------------------------
#
# -----------------------------------------------------------------------
#     Check to see if all treatments have been run for RNMODE = 'A'
# -----------------------------------------------------------------------
        I = 'A'.find(RNMODE)
        if 'A'.find(RNMODE) >0  and TRTNUM > TRTALL:
            DONE = True

# C-----------------------------------------------------------------------
# C
# C-----------------------------------------------------------------------
        elif 'GDC'.find(RNMODE) > 0:
            DONE = True
        elif 'FQ'.find(RNMODE) >0 and YRDOY > YRDOY_END:
            REPNO = REPNO + 1
            CONTROL.REPNO = REPNO
            if REPNO > NREPS:
                DONE = True
            else:
                RUN = 0
#
#       ELSE IF (INDEX('IE',RNMODE) .GT. 0) THEN
#         WRITE(*,1700)
#  1700   FORMAT(/,1X,'Do you want to run more simulations ? ',
#      &         /,1X,'Y or N ? [Default = "N"] ===> ',$)
#         READ (5,1800) ANS
#  1800   FORMAT(A1)
#         ANS = UPCASE(ANS)
#         IF (ANS .NE. 'Y') DONE = .TRUE.
#       ENDIF
#
#  2000 CONTINUE
#       ENDDO RUN_LOOP
#
# !     Final end-of-run call to land unit module
    CONTROL.DYNAMIC = RunConstants.ENDRUN
    Put_CONTROL(CONTROL)
    YRPLT, MDATE, YREND = LAND(CONTROL, ISWITCH, YRPLT, YREND)

      # Change output file names if FNAME set
    # OPNAMES(FNAME)

    RunList(CONTROL)

      # END PROGRAM CSM
#-----------------------------------------------------------------------

def main():
    CSM()

if __name__ == '__main__':
    main()

#===========================================================================
# Variable listing for main program
# ---------------------------------
# BLANK   Blank character
# CONTROL Composite variable containing variables related to control and/or
#           timing of simulation.  The structure of the variable
#           (ControlType) is defined in ModuleDefs.for.
# DAS     Days after start of simulation (d)
# DONE    Logical variable. TRUE if all runs have been completed. FALSE
#           otherwise.
# DOY     Current day of simulation (d)
# DSCSM   Name of CSM model executable (i.e., DSCSM040.EXE)
# ERRKEY  Subroutine name for error file
# ERRNUM  Error number for input
# EXPNO   Experiment number
# FEXIST  Logical variable
# FILEARG Run-time argument which contains name of input file (either
#           FILEIO, FILEB or FILEX depending on run mode).
# FILEB   Name of batch file (i.e., D4batch.dv4)
# FILEIO  Filename for input file (e.g., IBSNAT35.INP)
# FILEX   Experiment file, e.g., UFGA7801.SBX
# FNAME   Output file name, usually 'OVERVIEW'
# I       Loop counter
# INPUT   Name of input module executable (i.e., MINPT040.EXE)
# INPUTX  Command line for system call to run input module.
# IP      Return status of GETARG command
# IPX     Length of path plus filename for CSM executable
# ISECT   Indicator of completion of IGNORE routine: 0 - End of file
#           encountered, 1 - Found a good line to read, 2 - End of Section
#           in file encountered denoted by * in column 1.
# ISIM    Day portion of Julian date
# ISWITCH Composite variable containing switches which control flow of
#           execution for model.  The structure of the variable
#           (SwitchType) is defined in ModuleDefs.for.
# LN      Pest number
# LUNIO   Logical unit number for FILEIO
# MDATE   Harvest maturity date (YYYYDDD)
# MULTI   Current simulation year (=1 for first or single simulation, =NYRS
#           for last seasonal simulation)
# NREPS   Number of replications for sequenced simulation
# NYRS    Number of years of simulations to perform for multi-season run
#           (each with identical intitial conditions, but different weather
#           years)
# REPNO   Replication number for current simulation
# RNMODE    Simulation run mode (I=Interactive, A=All treatments,
#             B=Batch mode, E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence)
# RUN     Change in date between two observations for linear interpolation
# TRTNUM   Treatment number being simulated (from FILEX)
# YEAR    Year of current date of simulation
# YR      Year portion of date
# YRDIF   Increment in years which must be added to operations dates for
#           seasonal or sequenced simulations (yr)
# YRDOY   Current day of simulation (YYYYDDD)
# YREND   Date for end of season (usually harvest date) (YYYYDDD)
# YRPLT   Planting date (YYYYDDD)
# YRSIM   Start of simulation date (YYYYDDD)
#===========================================================================