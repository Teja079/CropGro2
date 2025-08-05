# C=======================================================================
#
# C=======================================================================
# C  HEADER, Subroutine, C. H. Porter
# C-----------------------------------------------------------------------
# C  Writes simulation header info to file.
#
# !     Currently, the routine reads header info from a file (written
# !     previously by input module or elsewhere in the code).  Probably
# !     should generate the header info here.
#
# C-----------------------------------------------------------------------
# C  REVISION HISTORY
# C  10/18/2001 CHP Written.
# C  08/12/2003 CHP Added I/O error checking
# !  05/09/2007 CHP Can get CONTROL variable from here, don't need to send
# !                 name of FILEIO from suborutine.  Keep sending DYNAMIC
# !                 and RUN, because these do not always have the same
# !                 values as that in the CONTROL variable.
# C-----------------------------------------------------------------------
# ! Called by: IRRIG, OPWBAL, OPGROW, . . .
# ! Calls: None
# C========================================================================
#
def HEADER(DYNAMIC, LUNDES, RUN):
    from datetime import datetime
    from ModuleDefs import RunConstants as RC,Headers,MonthTxt,Version,VBranch
#
# !     Reads up to 100 lines of header information and prints to output
# !       files as needed.
# !     DYNAMIC = 0 : Prints version number and date/time only
# !     DYNAMIC = 1 (RUNINIT) : Complete header is printed
# !     DYNAMIC = 2 (SEASINIT): Short header is printed
# !-----------------------------------------------------------------------
#       USE ModuleDefs
#       USE ModuleData
#       USE HeaderMod
#       IMPLICIT NONE
#       INCLUDE 'COMIBS.blk'
#       INCLUDE 'COMSWI.blk'
#       SAVE
#
    ERRKEY = 'HEADER'
#
#       INTEGER DATE_TIME(8)
#       INTEGER DYNAMIC, I, LUNDES
#       INTEGER ICOUNT, RUN, PREV_RUN
#       INTEGER ShortCount
#
#       DATA PREV_RUN /0/
    PREV_RUN = 0
    MAXLUN = 200
    NOHEADER = {}
#       logical NOHEADER(MAXLUN)
#
#       TYPE (ControlType) CONTROL
#       CALL GET(CONTROL)
#
    ICOUNT = Headers.ICOUNT
    ShortCount = Headers.ShortCount
#
# !-----------------------------------------------------------------------
    if RUN != PREV_RUN:
        NOHEADER[LUNDES] = True
        PREV_RUN = RUN

# !        IF (CONTROL % MULTI > 1 .OR. CONTROL % RNMODE == 'Q') THEN
# !          CALL MULTIRUN(RUN)
# !        ENDIF
#
# !     Write Model info and date-time stamp to all headers
    if ICOUNT > 0:
        LUNDES.write("\n" + Headers.Header(1).strip() + "\n")
    else:
        now = datetime.now()
        LUNDES.write(
            f"*DSSAT Cropping System Model Ver. {Version}.{VBranch}. "
            f"{MonthTxt[now.month]} {now.day:02d}, {now.year}; "
            f"{now.hour:02d}:{now.minute:02d}:{now.second:02d}\n"
        )
        LUNDES.write(f"\n*RUN {RUN % 1000:3d}\n")
        return
#
# !***********************************************************************
# !***********************************************************************
# !     Run Initialization
# !***********************************************************************
    if DYNAMIC == RC.RUNINIT:
# !-----------------------------------------------------------------------
# !     Write OVERVIEW header info to destination file.
        for I in range(2, ICOUNT + 1):
            try:
                LUNDES.write(Headers.Header(I).strip())
            except Exception:
                pass
#
# !***********************************************************************
# !***********************************************************************
# !     Seasonal initialization
# !***********************************************************************
    elif DYNAMIC == RC.SEASINIT:
# !-----------------------------------------------------------------------
        if LUNDES > MAXLUN:
            return
        if NOHEADER[LUNDES] and RUN > 0 and ICOUNT > 0:
# !       Header exists, write it
            for i in range(2, ShortCount + 1):
                LUNDES.write(Headers.Header(i).strip() + "\n")
            LUNDES.write("\n")
            NOHEADER[LUNDES] = False
#
# !      ELSE
# !!       Header already written for this run, or header not avail yet.
# !        CALL DATE_AND_TIME (VALUES=DATE_TIME)
# !        WRITE (LUNDES,500)Version,MonthTxt(DATE_TIME(2)),DATE_TIME(3),
# !     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
# !        WRITE(LUNDES,1200) MOD(RUN,1000)
#       ENDIF
#
# !***********************************************************************
#       ENDIF
# !***********************************************************************
# !***********************************************************************
# !     END OF DYNAMIC IF CONSTRUCT
# !***********************************************************************
    return
#
    now = datetime.now()
    LUNDES.write(
        "*DSSAT Cropping System Model Ver. "
        f"{Version}.{VBranch}.{SubVersion}.{Build:03d} "         
        f"{MonthTxt[now.month]}{' ' * 4}"                        
        f"{CropCode3} {now.day:02d}, {now.year}; "               
        f"{now.hour:02d}:{now.minute:02d}:{now.second:02d}\n"
    )
    LUNDES.write(f"\n*RUN {RUN:3d}\n")
#
#       END SUBROUTINE HEADER

# Test Drive for Header
DYNAMIC=2
LUNDES=122
RUN=1
print(HEADER(DYNAMIC, LUNDES, RUN))