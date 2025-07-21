import os
from DATES import *
#=======================================================================
#  WARNING, Subroutine
#  Writes warning messages to Warning.OUT file
# Inputs
#   ICOUNT - integer, meaning (?), 1
#   ERRKEY  'SOILDYN' List
#   MESSAGE 'Soil Photosynthesis Factor   ' List
# Output
#   Text to WARNING.OUT file
#-----------------------------------------------------------------------
def WARNING(ICOUNT, ERRKEY, MESSAGE):
    from OPHEAD import MULTIRUN, HEADER
    from ModuleDefs import RunConstants, ControlType, GET_CONTROL

    w = WARNING
    if not all(hasattr(w, attr) for attr in ["FIRST", "OLDRUN"]):
        w.FIRST = True
        w.OLDRUN = 0

    WarnOut = "WARNING.OUT"

    FIRST = True
    OLDRUN = 0

    CONTROL = ControlType()
    CONTROL = GET_CONTROL()
    FILEIO = CONTROL.FILEIO
    RUN = CONTROL.RUN
    YRDOY = CONTROL.YRDOY
    ErrCode = CONTROL.ERRCODE

    ISWITCH = GET_ISWITCH()
    IDETL = ISWITCH.IDETL

    try: #Always Open File
        if os.path.exists(WarnOut):
            LUN = open(WarnOut, 'a')
        else:
            LUN = open(WarnOut, 'w')
            LUN.write("*WARNING DETAIL FILE*")
    except Exception as e:
        print(f"Error opening file {WarnOut}: {e}")
        return

    if "ENDRUN" not in ERRKEY :
        # First-time routine is called, open file
        if FIRST:
            #ISWITCH = GET_ISWITCH()
            if IDETL == "0" and ErrCode <= 0:
                return
            LUN.write(f"\n{"*"*78}\n")
            if CONTROL.MULTI > 1:
                MULTIRUN(RUN, 0) #From Header Module
            if Headers.RUN == RUN:
                HEADER(RunConstants.SEASINIT, LUN, RUN)
                w.FIRST = False
                w.OLDRUN = RUN

        if ICOUNT > 0:
            # Print header if this is a new run
            if OLDRUN != RUN and RUN != 0 and FILEIO != "":
                if Headers.RUN == RUN:
                    HEADER(RunConstants.SEASINIT, LUN, RUN)
                    w.OLDRUN = RUN

            # Print the warning message
            YEAR, DOY = YR_DOY(YRDOY)
            LUN.write(f"\n {ERRKEY}  YEAR DOY = {YEAR:4d} {DOY:3d}\n")
            for i in range(ICOUNT):
                LUN.write(f" {MESSAGE[i]:78s}\n")

            # Check if end of season
        if "ENDRUN" in ERRKEY :
            FIRST = True
            LUN.close() #Always Close file

# Test driver for WARNING
# ICOUNT = 1
# ERRKEY = ['SOILDYN']
# MESSAGE = ['Soil Photosynthesis Factor   ']
# WARNING(ICOUNT, ERRKEY, MESSAGE)