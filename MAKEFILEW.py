# =======================================================================
#  MAKEFILEW, Subroutine
#
# This subroutine open the FILEX and read the weather station characters (WSTA).
# Then create the weather filename and check if FILEW exists.
# -----------------------------------------------------------------------
#  INPUT
#     LUNEXP = full path to File X instead of unit number 16
#     DSSATP = 'C:\\DSSAT48\\DSSATPRO.v48'
#     PATHEX = 'C:\\DSSAT48\\Strawberry\\'
#     FILEX = 'UFBA1701'
#     SimLevel = True
#     LNSIM =  1
#     LNPLT = 1
#     LNFLD = 1
#
#  OUTPUT : YEAR
# -----------------------------------------------------------------------
# =======================================================================
def MAKEFILEW(LUNEXP, DSSATP, PATHEX, FILEX,
              SimLevel, LNSIM, LNPLT, LNFLD):  # Input: Factor Levels

    # USE ModuleDefs
    from ERROR import ERROR
    from WARNING import WARNING
    from READS import IGNORE2, FIND, HFIND
    from PATH import PATH
    from UTILS import INQUIRE
    from fortranformat import FortranRecordReader

    # BLANK, WTHER = ' ' * 2
    # PROCOD = ' '
    # ERRKEY, SECTION = ' ' * 2
    # FINDH = ' '
    # WSTA, FILEW4, FILEX = ' ' * 3
    # FILEW, NAMEF, LastFileW = ' ' * 3
    MSG = [''] * 4
    # PATHEX, CHARTEST = ' ' * 2
    # FILEWW, FILETMP = ' ' * 2
    # DSSATP = ' '
    # LINE = ' '
    #
    # FEXIST = False
    #
    # LUNEXP, LINEXP, LNUM, ISECT, ERRNUM, PATHL, IFIND = [0] * 7
    # ICASAF, UNIT = [0] * 2
    # SDATE, PDATE, YRDOY = [0] * 3
    # LNFLD, TLNFLD = [0] * 2
    # LNSIM, TLNSIM = [0] * 2
    # LNPLT, TLNPLT = [0] * 2
    # YEARDOY, YEAR, YR, DOY = [0] * 4

    # DATA LastFileW /" "/
    LastFileW = ' '

    ERRKEY = 'MAKEFW'
    BLANK = ' '

    SDATE = -99
    PDATE = -99
    WTHER = 'M'

    # -----------------------------------------------------------------------
    #    Open FILEX and read Weather station characters (WSTA)
    # -----------------------------------------------------------------------

    LNUM = 0
    LINEXP = 0

    # FO - Rewind because ipexp.for call FileX LUNEXP as a constant
    # REWIND(LUNEXP)

    with (open(LUNEXP, 'r') as file):
        if LNFLD > 0:
            # FO - Find FIELD section to search for weather station
            SECTION = '*FIELD'
            LINEXP,IFIND = FIND (LUNEXP,SECTION)
            if IFIND != 0:
                # FO - Find the specific section header
                FINDH = 'L ID_'
                LINEXP, IFIND = HFIND(LUNEXP, FINDH, LINEXP)
                if IFIND == 1:
                    # FO - Loop through good lines to find and read the weather station
                    while True:
                        LINEXP, ISECT, CHARTEST = IGNORE2(LUNEXP, LINEXP)

                        if ISECT == 1:
                            f10 = FortranRecordReader('I3,9X,A8')
                            try:
                                TLNFLD, WSTA = f10.read(CHARTEST)
                            except ValueError as e:
                            # TLNFLD = int(CHARTEST[0:3].strip())
                            # WSTA = CHARTEST[11:19]
                                ERROR(ERRKEY, 4, FILEX, LINEXP)
                            if WSTA[0] == BLANK: ERROR(ERRKEY, 3, FILEX, LINEXP)
                        else:
                            ERROR(ERRKEY, 3, FILEX, LINEXP)

                        if TLNFLD == LNFLD and LNFLD > 0:
                            break
                else:
                    # Error - If FINDH ='L ID_' not found
                    ERROR(ERRKEY, 2, FILEX, LINEXP)
            else:
                ERROR(ERRKEY, 2, FILEX, LINEXP)
        else:
            ERROR(ERRKEY, 1, FILEX, LINEXP)

    # -----------------------------------------------------------------------
    #    Read FILEX YRSIM and weather method from *SIMUALTION CONTROLS
    #    Otherwise, read planting date for experiments (LNSIM = 0)
    # -----------------------------------------------------------------------

    LNUM = 0
    LINEXP = 0

    # FO - Rewind FileX, because find always set LINEXP to 0
    # REWIND(LUNEXP)

    # FO - Search for *SIMUL CONTROLS if SM factor level is gt 0
    if LNSIM > 0 and SimLevel:
        # FO - Find *SIMULATION CONTROLS section to search for start simulation date
        # FO - DO/UNTIL loop is used to iterate until find the right section
        SECTION = '*SIMUL'
        LINEXP, IFIND = FIND(LUNEXP, SECTION)
        LNUM = LNUM + LINEXP

        if IFIND != 0:
            #while TLNSIM == LNSIM and LNSIM > 0:
            while True:
                # Find the specific header to search for SDATE
                FINDH = 'N GEN'
                LINEXP, IFIND = HFIND(LUNEXP, FINDH, LINEXP)
                if IFIND == 1:
                    LINEXP, ISECT, CHARTEST = IGNORE2(LUNEXP, LINEXP)
                    if ISECT == 1:
                        f20 = FortranRecordReader('I2,30X,I6')
                        try:
                            TLNSIM, SDATE = f20.read(CHARTEST)
                        except ValueError as e:
                            ERROR(ERRKEY, 8, FILEX, LNUM)
                    else:
                        ERROR(ERRKEY, 7, FILEX, LNUM)
                else:
                    ERROR(ERRKEY, 6, FILEX, LNUM)

                # Find the specific header to search for WTHER
                FINDH = 'N MET'
                LINEXP, IFIND = HFIND(LUNEXP, FINDH, LINEXP)
                if IFIND == 1:
                    LINEXP, ISECT, CHARTEST = IGNORE2(LUNEXP, LINEXP)
                    if ISECT == 1:
                        f30 = FortranRecordReader('I2,17X,A1')
                        try:
                            TLNSIM, WTHER = f30.read(CHARTEST)
                        except ValueError as e:
                            ERROR(ERRKEY, 11, FILEX, LNUM)
                        if WTHER == BLANK: ERROR(ERRKEY, 10, FILEX, LINEXP)
                    else:
                        ERROR(ERRKEY, 10, FILEX, LNUM)
                else:
                    ERROR(ERRKEY, 9, FILEX, LNUM)

                if TLNSIM == LNSIM and LNSIM > 0:
                    break
        else:
            ERROR(ERRKEY, 6, FILEX, LNUM)

    # FO - Search for *PLANTING DETAILS if MP factor level is gt 0
    elif LNPLT > 0:
        SECTION = '*PLANT'
        LINEXP, IFIND = FIND(LUNEXP, SECTION)
        LNUM = LNUM + LINEXP

        if IFIND != 0:
            FINDH = 'P PDA'
            LINEXP, IFIND = HFIND(LUNEXP, FINDH, LINEXP)
            if IFIND == 1:
                while True:
                    # Until SM factor lvl equals to the current section.
                    LINEXP, ISECT, CHARTEST = IGNORE2(LUNEXP, LINEXP)
                    if ISECT == 1:
                        f40 = FortranRecordReader('I2,I6')
                        try:
                            TLNPLT, PDATE = f40.read(CHARTEST)
                        except ValueError as e:
                            ERROR(ERRKEY, 14, FILEX, LNUM)
                    else:
                        ERROR(ERRKEY, 13, FILEX, LNUM)

                    if TLNPLT == LNPLT and LNPLT > 0:
                        break
            else:
                ERROR(ERRKEY, 12, FILEX, LNUM)
        else:
            # Error - If !SECTION ='*PLANT'
            ERROR(ERRKEY, 12, FILEX, LNUM)
    else:
        ERROR(ERRKEY, 5, FILEX, LNUM)

    # -----------------------------------------------------------------------
    #    Converts SDATE or PDATE to YR and DOY
    # -----------------------------------------------------------------------
    if SDATE > 0:
        YRDOY = SDATE
    elif PDATE > 0:
        YRDOY = PDATE
    else:
        ERROR(ERRKEY, 15, FILEX, LNUM)

    # Converts YRDOY to YR and DOY
    YR = int(YRDOY / 1000)
    DOY = YRDOY - YR * 1000

    # -----------------------------------------------------------------------
    #    Create WTH file name and path
    # -----------------------------------------------------------------------
    #    Establish the weather file FILEW as WSTA + .WT?  where ? :
    #          M = observed data
    #          G = generated data
    #          S = interactively generated
    # -----------------------------------------------------------------------
    if WTHER == 'M':
        if WSTA[4:5] == BLANK:
            FILEW = WSTA[:4] + str(YR) + "01.WTH"
        else:
            FILEW = WSTA[:8] + "WTH"
        PROCOD = 'WED'
    elif WTHER == 'G':
        if WSTA[4:5] == BLANK:
            FILEW = WSTA[:4] + str(YR) + "01.WTG"
        else:
            FILEW = WSTA[:8] + ".WTG"
        PROCOD = 'WGD'
    elif WTHER == 'S' or WTHER == 'W':
        FILEW = WSTA [:4] + ".CLI"
        PROCOD = 'CLD'
    else:
        ERROR(ERRKEY, 16, FILEX, LINEXP)

    #     Check weather filename in current directory
    FEXIST = INQUIRE(FILEW)
    if FEXIST:
        PATHWT = BLANK
    #     Check weather filename in data directory
    else:
        FILETMP = PATHEX.strip() + FILEW
        FEXIST = INQUIRE(FILETMP)
        if FEXIST:
            PATHWT = PATHEX.strip()
        #       Check weather filename in default DSSAT directory
        else:
            PATHWT, NAMEF = PATH(PROCOD, DSSATP, 1)
            FILETMP = PATHWT.strip() + FILEW
            FEXIST = INQUIRE(FILETMP)
            if FEXIST:
                PATHWT = PATHWT
            #         Check 4-character file name in data directory
            else:
                FILEW4 = FILEW[0:4] + ".WTH"
                FILETMP = PATHEX.strip() + FILEW4
                FEXIST = INQUIRE(FILETMP)
                if FEXIST:
                    PATHWT = PATHEX.strip()
                    FILEW = FILEW4
                #           Check 4-character filename in default DSSAT directory
                else:
                    FILEW4 = FILEW[0:4] + ".WTH"
                    FILETMP = PATHWT.strip() + FILEW4
                    FEXIST = INQUIRE(FILETMP)
                    if FEXIST:
                        PATHWT = PATHWT.strip()
                        FILEW = FILEW4
                    else:
                        MSG[0] = "Weather file not found."
                        MSG[1] = "  Neither " + FILEW + " nor " + FILEW4
                        MSG[2] = "  were found in weather or experiment directories."
                        MSG[3] = "Simulation will end."
                        WARNING(4, ERRKEY, MSG)
                        ERROR(ERRKEY, 18, FILEW, 0)

    # -----------------------------------------------------------------------
    #    Open and Read Weather file data
    # -----------------------------------------------------------------------
    PATHL = PATHWT.index(BLANK)
    if PATHL <= 1:
        FILEWW = FILEW
    else:
        FILEWW = PATHWT[0:PATHL] + FILEW

    # FO - Process FILEW if last FILEW is different.
    if LastFileW != FILEW:
        LastFileW = FILEW
        ICASAF = 0
        YEAR = -99
        DOY = -99
        YEARDOY = -99

        # CALL GETLUN('LUNMFW', UNIT) HZ
        FEXIST = INQUIRE(FILEWW)
        if not FEXIST:
            ERROR(ERRKEY, 18, FILEW, 0)
        else:
            try:
                with open(FILEWW) as f:
                    for LINE in f:
                        if LINE.find('$WEATHER') > -1 and ICASAF == 0:
                            ICASAF = 1
                        elif ICASAF == 1:
                            if LINE[0:0] == '@' and LINE.find('DATE') > 0:
                                # Read 7-digit First Weather YEAR
                                next(f)
                                YEARDOY = LINE[0:7]
                                # if ERRNUM != 0: ERROR(ERRKEY, 17, FILEW, 0)
                                break
                            elif LINE[0:0] == '@' and LINE.find('YEAR') > 0:
                                # Read ICASA format
                                # READ (UNIT,'(I4,1X,I3)', IOSTAT=ERRNUM)
                                YEAR, DOY = LINE[0:4], LINE[5:8]
                                # if ERRNUM != 0: ERROR(ERRKEY, 17, FILEW, 0)
                                YEARDOY = YEAR * 1000 + DOY
            except ValueError as e:
                ERROR(ERRKEY, 18, FILEW, 0)
    # -----------------------------------------------------------------------
    #    Convert SDATE to 7-digits and check if is in the date range
    # -----------------------------------------------------------------------
    if YEARDOY > 0 and ICASAF == 1:
        FirstWeatherDate = YEARDOY
        YEARDOY = int(FirstWeatherDate / 100000) * 100000 + YRDOY

        if YEARDOY >= FirstWeatherDate:
            NEWSDATE = YEARDOY
        else:
            NEWSDATE = int((FirstWeatherDate + 99000) / 100000) * 100000 + YRDOY

        # Error Checking
        if NEWSDATE < FirstWeatherDate or NEWSDATE > FirstWeatherDate + 99000:
            ERROR(ERRKEY, 19, FILEX, 0)
    else:
        MSG[1] = 'Y4KDOY - Weather file'
        MSG[2] = 'Not able to find 7-digits first weather date'
        MSG[3] = 'Set simulation to 5-digits first weather date'
        FirstWeatherDate = -99

        # INFO(3,ERRKEY,MSG)

# Test Drive for MAKEFILEW
# # LUNEXP = 16
# LUNEXP = 'C:/DSSAT48/Strawberry/UFBA1701.SRX'
# DSSATP = 'C:/DSSAT48/DSSATPRO.v48'
# PATHEX = 'C:/DSSAT48/Strawberry/'
# FILEX = 'UFBA1701'
# SimLevel = True
# LNSIM =  1
# LNPLT = 1
# LNFLD = 1
#
# MAKEFILEW(LUNEXP, DSSATP, PATHEX, FILEX, SimLevel, LNSIM, LNPLT, LNFLD)


