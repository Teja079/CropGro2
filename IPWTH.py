# =======================================================================
#   IPWTH, Subroutine
#   Input weather data and check for errors.
# -----------------------------------------------------------------------
#   Called by: WEATHR
#   Calls:     None
# =======================================================================
import os
import numpy as np

def IPWTH(CONTROL, SOURCE, DYNAMIC):

    from ModuleDefs import RunConstants as RC, Put_WEATHER, WEATHER
    import fortranformat as ff
    from SumModule import SUMDAT
    #USE ModuleData
    #USE Forecast
    #USE SumModule
    from DATES import YR_DOY, INCYD
    from ERROR import ERROR
    from READS import find, ignore
    from WARNING import WARNING
    #from OPSUM import SUMVALS TODO commented out for testing since OPSUM has errors
     #  EXTERNAL YR_DOY, INCYD, ERROR, FIND, GETLUN, WARNING, UPCASE,
     # &  IGNORE, PARSE_HEADERS, WEATHERERROR, IGNORE2,
     # &  CHECK_WEATHER_HEADERS, SUMVALS, IPWREC, DAILYWEATHERCHECK
     #  SAVE
    MSG = [''] * 8
    MaxRecords = 1000
    ERRKEY = 'IPWTH '

#   ============================================================
#   New stuff
#   Arrays of weather data stored.
    YRDOY_A, LineNumber = ([-99] * MaxRecords for _ in range(2))

    MAXCOL = 25
    HEADER = [""] * MAXCOL
    COL = [[-99]*2] * MAXCOL

    SUMNUM = 3
    LABEL = [''] * SUMNUM
    VALUE = [-99.0] * SUMNUM
    FILEWC = '' #TODO these FILE and PATH variables are not relevant for us?
    FILEWG = ''
    PATHWTG = ''
    PATHWTC = ''

    FILEIO = CONTROL.FILEIO
    MULTI  = CONTROL.MULTI
    RUN    = CONTROL.RUN
    RNMODE = CONTROL.RNMODE
    YRDOY  = CONTROL.YRDOY
    YRSIM  = CONTROL.YRSIM
    IPWTH.LastFileW = ' '
    LongFile = False
# !***********************************************************************
# !***********************************************************************
# !     Run Initialization - Called once per simulation
# !***********************************************************************
    if DYNAMIC == RC.RUNINIT:
        IPWTH.FirstWeatherDay = 0
        if 'FQ'.find(RNMODE) < 0 or RUN == 1:
            LNUM = -99
            try:
                with open(FILEIO, 'r') as f:
                    lines = f.readlines()
                    SECTION = '*SIMUL'
                    LNUM, FOUND = find(lines, SECTION)
                    if not FOUND:
                        ERROR(SECTION, 42, FILEIO,LNUM)
                        return
                    LNUM += 1
                    fmt = ff.FortranRecordReader('41X,I5')
                    IPWTH.RSEED1 = fmt.read(lines[LNUM])[0]

                    LNUM += 2
                    fmt = ff.FortranRecordReader('19X,A1')
                    IPWTH.MEWTH = fmt.read(lines[LNUM])[0]

                    LNUM = 11
                    fmt = ff.FortranRecordReader('15X,A12,1X,A80')
                    FILEW, IPWTH.PATHWTW = fmt.read(lines[LNUM])
            except Exception:
                ERROR(ERRKEY,-99,FILEIO,LNUM + 1)
                return
            if FILEW != IPWTH.LastFileW:
                IPWTH.LastWeatherDay = 0

# !       With forecast mode, there is a possibility of two weather files, one for
# !         forecast season observed data (WTH) and one for either historical ensemble
# !         climate data (CLI) or WTG generated data. It is also possible to have
# !         only one weather file if historical ensemble is also in the WTH file.
# !       NOTE for future enhancement: May also need a third weather file for short
# !         term forecast, but this would be expected to be in the experiment directory
# !         and the name is read from Simulation Options.
#         IF (RNMODE .EQ. 'Y') THEN #Yield forecast mode
#           SELECT CASE (MEWTH)
#             CASE ('M')
#             CASE ('G')
#               READ (LUNIO,'(15X,A12,1X,A80)',IOSTAT=ERR)
#      &          FILEWG, PATHWTG
#               CALL GETLUN('FILEWG', LUNWTHG)
#             CASE ('S','W')
#               READ (LUNIO,'(15X,A12,1X,A80)',IOSTAT=ERR)
#      &          FILEWC, PATHWTC
#               CALL GETLUN('FILEWC', LUNWTHC)
#           END SELECT
#         ELSE
# !         In any case, need to keep separate weather file names
#           SELECT CASE (MEWTH)
#             CASE ('M')
#             CASE ('G')
#               FILEWG = FILEW
#               PATHWTG = IPWTH.)PATHWTW
#             CASE ('S','W')
#               FILEWC = FILEW
#               PATHWTC = IPWTH.)PATHWTW
#           END SELECT
#         ENDIF
#
#         CLOSE (LUNIO)
#
#         IF (FILEW /= LastFileW) THEN
# !          NRecords = 0
#           LastWeatherDay  = 0
#         ENDIF
#       ENDIF

        DCO2 = OZON7 = PAR = RAIN = IPWTH.REFHT = RHUM = SRAD = IPWTH.TAMP = IPWTH.TAV = TDEW = TMAX = TMIN = VAPR = IPWTH.WINDHT = WINDSP = 0.0
        IPWTH.CCO2 = IPWTH.XELEV = IPWTH.XLAT = IPWTH.XLONG = 0.0
        IPWTH.FILEWW = ''

        YREND = -99
        return (IPWTH.CCO2, DCO2, FILEW, FILEWC, FILEWG, IPWTH.FILEWW, IPWTH.MEWTH, OZON7, PAR,
        PATHWTC, PATHWTG, IPWTH.PATHWTW, RAIN, IPWTH.REFHT, RHUM, IPWTH.RSEED1, SRAD, IPWTH.TAMP, IPWTH.TAV, TDEW,
        TMAX, TMIN, VAPR, IPWTH.WINDHT, WINDSP, IPWTH.XELEV, IPWTH.XLAT, IPWTH.XLONG, YREND)

# ***********************************************************************
# ***********************************************************************
#      Seasonal initialization - run once per season
# ***********************************************************************
    elif DYNAMIC == RC.SEASINIT:
        ErrCode = 0
        (IPWTH.SRAD_A, IPWTH.TMAX_A, IPWTH.TMIN_A, IPWTH.RAIN_A, IPWTH.TDEW_A, IPWTH.WINDSP_A, IPWTH.PAR_A,
         IPWTH.RHUM_A, IPWTH.VAPR_A, IPWTH.DCO2_A, IPWTH.OZON7_A) = \
            ([-99.0] * MaxRecords for _ in range(11))
# -----------------------------------------------------------------------
#       Don't re-initialize for sequence and seasonal runs
        if 'FQ'.find(RNMODE) >= 0 and RUN > 1: return
        try:
            with open(FILEIO, 'r') as f:
                lines = f.readlines()
                LNUM = 11
                fmt = ff.FortranRecordReader('15X,A12,1X,A80')
                FILEW, IPWTH.PATHWTW = fmt.read(lines[LNUM])
        except Exception:
            ERROR(ERRKEY, -99, FILEIO, LNUM + 1)
            return
        if IPWTH.MEWTH == 'M':
            pass
        elif IPWTH.MEWTH == 'G':
            FILEWG = FILEW
            PATHWTG = IPWTH.PATHWTW
        elif IPWTH.MEWTH in ('S', 'W'):
            FILEWC = FILEW
            PATHWTC = IPWTH.PATHWTW

        IPWTH.WYEAR = int(FILEW[4]) * 10 + int(FILEW[5])
        IPWTH.NYEAR = int(FILEW[6]) * 10 + int(FILEW[7])
        if IPWTH.LastWeatherDay > IPWTH.FirstWeatherDay:
            IPWTH.NYEAR = int(int(IPWTH.LastWeatherDay)/1000 - int(IPWTH.FirstWeatherDay)/1000 + 1)
            if LongFile:
                IPWTH.NYEAR = max(2, IPWTH.NYEAR)
            else:
                IPWTH.NYEAR = max(1, IPWTH.NYEAR)

        if 'M' in IPWTH.MEWTH or SOURCE == 'FORCST':
            IPWTH.WFile = FILEW
            IPWTH.WPath = IPWTH.PATHWTW
        elif 'G' in IPWTH.MEWTH and SOURCE == 'WEATHR':
            IPWTH. WFile = FILEWG
            IPWTH.WPath = PATHWTG
        elif any(c in IPWTH.MEWTH for c in 'SW') and SOURCE == 'WEATHR':
            IPWTH.WFile = FILEWC
            IPWTH.WPath = PATHWTC

        #       IF (RNMODE .EQ. 'Y') THEN
#         SELECT CASE (MEWTH)
#           CASE ('M')
#           CASE ('G')
#             READ (LUNIO,'(15X,A12,1X,A80)',IOSTAT=ERR)
#      &        FILEWG, PATHWTG
#             CALL GETLUN('FILEWG', LUNWTHG)
#           CASE ('S','W')
#             READ (LUNIO,'(15X,A12,1X,A80)',IOSTAT=ERR)
#      &        FILEWC, PATHWTC
#             CALL GETLUN('FILEWC', LUNWTHC)
#         END SELECT
#
#       ELSE
# !       In any case, need to keep separate weather file names
#         SELECT CASE (MEWTH)
#           CASE ('M')
#           CASE ('G')
#             FILEWG = FILEW
#             PATHWTG = IPWTH.)PATHWTW
#           CASE ('S','W')
#             FILEWC = FILEW
#             PATHWTC = IPWTH.)PATHWTW
#         END SELECT
#       ENDIF

      # CLOSE (LUNIO)
      #
      # WYEAR = (ICHAR(FILEW(5:5)) - 48)*10 + (ICHAR(FILEW(6:6)) - 48)
      # NYEAR = (ICHAR(FILEW(7:7)) - 48)*10 + (ICHAR(FILEW(8:8)) - 48)
      #

#     Detect name of weather file based on MEWTH
        if 'M' in IPWTH.MEWTH or SOURCE == 'FORCST':
            IPWTH.WFile = FILEW
            IPWTH.WPath = IPWTH.PATHWTW
    # elif 'G' in MEWTH and SOURCE == 'WEATHR':
    #     WFile = FILEWG
    #     WPath = PATHWTG
    # elif 'SW' in MEWTH and SOURCE == 'WEATHR':
    #     WFile = FILEWC
    #     WPath = PATHWTC


# !     Multi-year runs, update file names for single season weather files
#       IF (NYEAR == 1 .AND. MULTI > 1) THEN
#         PATHL  = INDEX(WPath,BLANK)
#         WYEAR = MOD((WYEAR + MULTI - 1),100)
#         WRITE(WFile(5:6),'(I2.2)') WYEAR
#         IF (PATHL <= 1) THEN
#           FileWW = WFile
#         ELSE
#           FileWW = WPath(1:(PATHL-1)) // WFile
#         ENDIF
#         INQUIRE (FILE = FileWW, EXIST = FEXIST)
#         IF (.NOT. FEXIST) THEN
#           ErrCode = 29
#           CALL WeatherError(CONTROL, ErrCode, FileWW, 0, YRDOYWY, YREND)
#           RETURN
#         ENDIF
#       ENDIF

# !     Forecast mode: Set weather file name for historical weather data for forecast
# !     - when IPWTH is called from the forecast module, don't change weather file name
# !     If it's a multi-year weather file, no need to change the name.
#       IF (RNMODE .EQ. 'Y' .AND.      !Yield forecast mode
#      &    SOURCE .EQ. "WEATHR" .AND. !Using historical ensemble
#      &    NYEAR .EQ. 1) THEN         !Single-season weather file
#         PATHL  = INDEX(WPath,BLANK)
#         CALL YR_DOY(CONTROL % YRDOY, WYEAR, WDOY)
#         WYEAR = MOD(WYEAR,100)
#         YRSIM = CONTROL % YRDOY
#
#         WRITE(WFile(5:6),'(I2.2)') WYEAR
#         IF (PATHL <= 1) THEN
#           IPWTH.FILEWW = WFile
#         ELSE
#           IPWTH.FILEWW = WPath(1:(PATHL-1)) // WFile
#         ENDIF
#         INQUIRE (FILE = IPWTH.FILEWW,EXIST = FEXIST)
#         IF (.NOT. FEXIST) THEN
#           ErrCode = 29
#           CALL WeatherError(CONTROL, ErrCode, IPWTH.FILEWW, 0,YRDOYWY,YREND)
#           RETURN
#         ENDIF
#       ENDIF
#-----------------------------------------------------------------------
        YR, ISIM = YR_DOY(YRSIM)
        IPWTH.FILEWW = os.path.join(IPWTH.PATHWTW.strip(), FILEW.strip())

        if YRDOY == YRSIM:
            YRDOY_WY = INCYD(YRSIM,-1)

# !     Forecast mode - check bounds of weather file. Needed to determine
# !     that correct century is read for files with 2-digit years
#       IF (RNMODE .EQ. 'Y' .AND.           !Yield forecast mode
#      &    SOURCE .EQ. "FORCST" .AND.      !Getting in-season data
# !    &    INDEX('MG',MEWTH) .GT. 0 .AND.  !Measured or generated data
# !    &    NYEAR .GT. 1 .AND.              !Multi-year weather file
#      &    CONTROL % ENDYRS .EQ. 1) THEN   !First year simulation
#         WFPASS = 0
#         CenturyWRecord = -99
#         CALL FCAST_ScanWeathData(CONTROL, FileWW, LunWth,CenturyWRecord)
#       ENDIF

        WSTAT = IPWTH.WFile[0:8]
        WEATHER.WSTAT = WSTAT #PUT('WEATHER','WSTA',WSTAT)
        #Put_WEATHER(WEATHER)

# !     If this is the first of the forecast simulations, need to
# !     re-read weather data so that metadata are available in WEATHR
#       IF (RNMODE .EQ. 'Y' .AND. CONTROL % ENDYRS .EQ. 1) THEN
#         LastFileW = ''
#         CLOSE (LUNWTH)
#       ENDIF

        if IPWTH.WFile != IPWTH.LastFileW:
            YRDOY_WY = 0
            IPWTH.LastWeatherDay  = 0
            LINWTH = 0
            LongFile = False
            try:
                with open(IPWTH.FILEWW, 'r') as f: #Open WTH file and get lines as list
                    lines = f.readlines()
            except Exception as e:
                ErrCode = 29
                #WeatherError(CONTROL, ErrCode, IPWTH.FILEWW, 0, YRDOYWY, YREND)
                return

            INSI  = '-99 '
            IPWTH.XLAT  = -99.
            IPWTH.XLONG = -99.
            IPWTH.XELEV = -99.
            IPWTH.TAV   = -99.
            IPWTH.TAMP  = -99.
            IPWTH.REFHT = -99.
            IPWTH.WINDHT= -99.
            IPWTH.CCO2  = -99.

            LNUM, FOUND = find(lines, "@") #Find 1st header
            if not FOUND:
                ERROR(ERRKEY,10, IPWTH.WFile,LINWTH)
                return
            data_dict = PARSE_COL(lines[LNUM:LNUM+2])
            HEADER = data_dict.keys()
            ICOUNT = len(HEADER)
            if ICOUNT < 1: ERROR(ERRKEY,10, IPWTH.WFile,LINWTH)

            for key, val in data_dict.items():
                key = key.strip().upper()
                if key == 'INSI':
                    INSI = val
                elif key in ('LAT', 'WTHLAT'):
                    try:
                        IPWTH.XLAT = float(val)
                        IPWTH.CYCRD = val
                    except:
                        IPWTH.XLAT = 0.0
                        MSG[0] = 'Error reading latitude, value of zero will be used.'
                        WARNING(1, ERRKEY, MSG)

                elif key in ('LONG', 'WTHLONG'):
                    try:
                        IPWTH.XLONG = float(val)
                        IPWTH.CXCRD = val
                    except:
                        IPWTH.XLONG = -99.0

                elif key in ('ELEV', 'WELEV'):
                    try:
                        IPWTH.XELEV = float(val)
                        IPWTH.CELEV = val
                    except:
                        IPWTH.XELEV = -99.0

                elif key == 'TAV':
                    try:
                        IPWTH.TAV = float(val)
                    except:
                        IPWTH.TAV = -99.0

                elif key == 'AMP':
                    try:
                        IPWTH.TAMP = float(val)
                    except:
                        IPWTH.TAMP = -99.0

                elif key == 'REFHT':
                    try:
                        IPWTH.REFHT = float(val)
                    except:
                        IPWTH.REFHT = 1.5

                elif key == 'WNDHT':
                    try:
                        IPWTH.WINDHT = float(val)
                    except:
                        IPWTH.WINDHT = 2.0

                elif key in ('CCO2.', 'CO2'):
                    try:
                        IPWTH.CCO2 = float(val)
                    except:
                        IPWTH.CCO2 = -99.0

            if IPWTH.REFHT <= 0.0:
                IPWTH.REFHT = 1.5
            if IPWTH.WINDHT <= 0.0:
                IPWTH.WINDHT = 2.0
            IPWTH.LastFileW = IPWTH.WFile
            if IPWTH.TAV <= 0.0:
                IPWTH.TAV = 20.0
                MSG[0] = 'Value of IPWTH.TAV, average annual soil temperature is missing.'
                MSG[1] = f'A default value of {IPWTH.TAV}°C is being used for this simulation,'
                MSG[2] = 'which may produce inaccurate results.'
                WARNING(3, ERRKEY, MSG) #TODO Warning function not working
            if IPWTH.TAMP <= 0.0:
                IPWTH.TAMP = 20.0
                MSG[0] = 'Value of IPWTH.TAMP, amplitude of soil temperature function is missing.'
                MSG[1] = f'A default value of {IPWTH.TAMP}°C is being used for this simulation,'
                MSG[2] = 'which may produce inaccurate results.'
                WARNING(3, ERRKEY, MSG) #TODO Warning function not working
# !       -------------------------------------------------------------
# !         Look for second header line beginning with '@' in column 1 (ISECT = 3)
            LNUM, FOUND = find(lines, "@", 2)  # Find 2nd header
            if not FOUND:
                ERROR(ERRKEY, 10, IPWTH.WFile, LINWTH)
                return

#        -------------------------------------------------------------
#           Get daily weather data under second header
#             time_series_dict = PARSE_COL(lines[LNUM:])
#             HEADER = time_series_dict.keys()
#             ICOUNT = len(HEADER)
#             if ICOUNT < 1:
#                 ERROR(ERRKEY, 10, IPWTH.WFile, LINWTH)
#                 return
# !       ------------------------------------------------------------
#

#
#       ELSEIF ((LongFile .AND. YRDOY < FirstWeatherDay)
#      &   .OR. (RNMODE .EQ. 'Y' .AND. CONTROL % ENDYRS. EQ. 1)) THEN
# !       Starting over with long file -- same file, but need to read from top
# !       or starting a historical ensemble for forecast mode.
#         REWIND(LUNWTH)
#
#   200   CONTINUE
#         CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
#         IF (FOUND == 2) GO TO 200
#         IF (FOUND == 0) CALL ERROR(ERRKEY,-1,WFile,LINWTH)
#         NRecords = 0
#
#       ELSEIF (LongFile .AND. YRDOY > LastWeatherDay) THEN
# !       Need to get next batch of records from long file
#         NRecords = 0
#
# !       Starting over, need to rewind
#         IF (MULTI == 1) THEN
#           REWIND (LUNWTH)
#   210     CONTINUE
#           CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
#           IF (FOUND == 2) GO TO 210
#           IF (FOUND == 0) CALL ERROR(ERRKEY,-1,WFile,LINWTH)
#         ENDIF
#       ENDIF

#-----------------------------------------------------------------------
#     Check if LAT and LONG are correct in FileX
        if SUMDAT.YCRD <= -99.0 or SUMDAT.XCRD <= -999.0:
            if (IPWTH.XLAT >= -90.0 and IPWTH.XLAT <= 90.0 and IPWTH.XLONG >= -180.0 and IPWTH.XLONG <= 180.0 and
                    len(IPWTH.CYCRD.strip()) > 0.0 and len(IPWTH.CXCRD.strip()) > 0.0 and
                    (abs(IPWTH.XLAT) > 1.0e-15 or abs(IPWTH.XLONG) > 1.0e-15)):
                # PUT('FIELD', 'IPWTH.CYCRD', IPWTH.CYCRD) #TODO Put_Field not implemented
                # PUT('FIELD', 'IPWTH.CXCRD', IPWTH.CXCRD)
                LABEL[0] = 'YCRD'
                VALUE[0] = IPWTH.XLAT
                LABEL[1] = 'XCRD'
                VALUE[1] = IPWTH.XLONG
            else:
                # PUT('FIELD', 'IPWTH.CYCRD', '            -99')
                # PUT('FIELD', 'IPWTH.CXCRD', '            -99')
                LABEL[0] = 'YCRD'
                VALUE[0] = -99.0
                LABEL[1] = 'XCRD'
                VALUE[1] = -999.0

        if SUMDAT.ELEV <= -99.0:
            if IPWTH.XELEV > -99.0 and len(IPWTH.CELEV.strip()) > 0.0:
                #PUT('FIELD', 'IPWTH.CELEV', IPWTH.CELEV)
                LABEL[2] = 'ELEV'
                VALUE[2] = IPWTH.XELEV
            else:
                #PUT('FIELD', 'IPWTH.CELEV', '      -99')
                LABEL[2] = 'ELEV'
                VALUE[2] = -99.0
        #SUMVALS (SUMNUM, LABEL, VALUE) #TODO commented out for testing
#-----------------------------------------------------------------------
        YRDOYWY = INCYD(YRSIM,-1)
          # IF (MULTI > 1) THEN
          #   YRDOY_WY = YRDOYWY
          # ELSE
          #   YRDOY_WY = 0
          # ENDIF
        IPWTH.NRecords = 0
        if IPWTH.NRecords == 0:
            time_series_dict = PARSE_COL(lines[LNUM:])
            HEADER = time_series_dict.keys()
            ICOUNT = len(HEADER)
            if ICOUNT < 1:
                ERROR(ERRKEY, 10, IPWTH.WFile, LINWTH)
                return
            century = int(YRDOY/100000)
            (IPWTH.FirstWeatherDate, IPWTH.LastWeatherDate, IPWTH.NRecords,
             IPWTH.YRDOY_A, IPWTH.SRAD_A, IPWTH.TMAX_A, IPWTH.TMIN_A, IPWTH.RAIN_A, IPWTH.TDEW_A, IPWTH.WINDSP_A,
             IPWTH.PAR_A, IPWTH.RHUM_A, IPWTH.VAPR_A, IPWTH.DCO2_A, IPWTH.OZON7_A,
             ) = (
                IpWRec(time_series_dict, MaxRecords, century)
            )

        IPWTH.LastRec = 0
        IPWTH.YRDOYWY = YRSIM - 1 # Day before simulation starts
        if IPWTH.YRDOY_A[0] == YRDOY:
            IPWTH.LastRec = 0
        else:
            for i in range(IPWTH.NRecords):
                if IPWTH.YRDOY_A[i] == YRDOYWY:
                    IPWTH.LastRec = i
                    break
        SRAD = IPWTH.SRAD_A[IPWTH.LastRec]
        TMAX = IPWTH.TMAX_A[IPWTH.LastRec]
        TMIN = IPWTH.TMIN_A[IPWTH.LastRec]
        RAIN = IPWTH.RAIN_A[IPWTH.LastRec]
        TDEW = IPWTH.TDEW_A[IPWTH.LastRec]
        WINDSP = IPWTH.WINDSP_A[IPWTH.LastRec]
        PAR = IPWTH.PAR_A[IPWTH.LastRec]
        RHUM = IPWTH.RHUM_A[IPWTH.LastRec]
        VAPR = IPWTH.VAPR_A[IPWTH.LastRec]
        DCO2 = IPWTH.DCO2_A[IPWTH.LastRec]
        OZON7 = IPWTH.OZON7_A[IPWTH.LastRec]

     #          CALL DailyWeatherCheck(CONTROL,
     # &    "WTHINIT", IPWTH.FILEWW, RAIN, RecNum,                !Input
     # &    SRAD, TMAX, TMIN, YRDOY,                        !Input
     # &    YREND)
#           IF (YREND > 0) THEN
# !       Try again with next weather day (only for initialization)
#         SRAD  = SRAD_A(I+1)
#         TMAX  = TMAX_A(I+1)
#         TMIN  = TMIN_A(I+1)
#         RAIN  = RAIN_A(I+1)
#         TDEW  = TDEW_A(I+1)
#         WINDSP= WINDSP_A(I+1)
#         PAR   = PAR_A(I+1)
#         RHUM  = RHUM_A(I+1)
#         VAPR  = VAPR_A(I+1)
#         DCO2  = DCO2_A(I+1)
#         OZON7 = OZON7_A(I+1)
#         YREND = -99
#
# !       Error checking
#         CALL DailyWeatherCheck(CONTROL,
#      &    ERRKEY, IPWTH.FILEWW, RAIN, RecNum,                   !Input
#      &    SRAD, TMAX, TMIN, YRDOY,                        !Input
#      &    YREND)                                          !Output
#
#       ENDIF

    elif DYNAMIC == RC.RATE:
        # if YRDOY > IPWTH.LastWeatherDay: # TODO this Section opens the next file if end of first file is reached
        #     IPWTH.YRSIMMY = INCYD(YRDOY,-1)
        #     YEAR, DOY = YR_DOY(YRDOY)
        #     IPWTH.CurrentWeatherYear = YEAR
        #     IF (NYEAR <= 1 .AND. .NOT. LongFile) THEN
        #      Open new weather file
        #       CLOSE(LUNWTH)
        #       WRITE(WFile(5:6),'(I2.2)') MOD(CurrentWeatherYear,100)
        #       LINWTH = 0
        #       PATHL  = INDEX(WPath,BLANK)
        #       IF (PATHL <= 1) THEN
        #          IPWTH.FILEWW = WFile
        #       ELSE
        #          IPWTH.FILEWW = WPath(1:(PATHL-1)) // WFile
        #       ENDIF
        #
        #       INQUIRE(FILE=IPWTH.FILEWW,EXIST=FEXIST)
        #       IF (.NOT. FEXIST) THEN
        #         ErrCode = 30
        #         CALL WeatherError(CONTROL, ErrCode, IPWTH.FILEWW, 0,YRDOYWY,YREND)
        #         RETURN
        #       ENDIF
#
#           OPEN (LUNWTH,FILE=IPWTH.FILEWW,STATUS='OLD',IOSTAT=ERR)
#           IF (ERR /= 0) THEN
#             ErrCode = 29
#             CALL WeatherError(CONTROL, ErrCode, IPWTH.FILEWW, 0,YRDOYWY,YREND)
#             RETURN
#           ENDIF
#           WSTAT = WFile(1:8)
#           CALL PUT('WEATHER','WSTA',WSTAT)
#
# C         Read in weather file header.
#   500     CONTINUE
#           CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
#           IF (FOUND == 2) GO TO 500
#           IF (FOUND == 0) CALL ERROR(ERRKEY,-1,WFile,LINWTH)
#
#           IF (WFile == LastFILEW) THEN
#             IF(YRDOY > YRDOY_A(NRecords)) THEN
#               ErrCode = 10
#               CALL WeatherError(CONTROL, ErrCode, IPWTH.FILEWW,
#      &                          LINWTH, YRDOY, YREND)
#               RETURN
#             ENDIF
#           ENDIF
#           LastFileW = WFile
#         ELSEIF (NYEAR > 1 .AND. .NOT. LongFile) THEN
# !         Simulation goes beyond weather file data
#           ErrCode = 10
#           CALL WeatherError(CONTROL, ErrCode, IPWTH.FILEWW, LINWTH,
#      &      YRDOY, YREND)
#           RETURN
#         ELSEIF (LongFile) THEN
#           YRDOYWY = LastWeatherDay
#         ENDIF
#
#         LastRec = 0
#
# !     ---------------------------------------------------------
#         IF (YRDOY == YRSIM) THEN
#           YRDOYWY = INCYD(YRSIM,-1)
#         ENDIF
#
# !       Read in another batch of data
# !       Use YRDOYWY here (different argument than previous call)
#         CALL IpWRec(CONTROL, MaxRecords,
#      &    COL, ICOUNT, IPWTH.FILEWW, HEADER, LINWTH,            !Input
#      &    LUNWTH, YRDOYWY, CenturyWRecord,                  !Input
#      &    ErrCode, FirstWeatherDay, LastWeatherDay,       !Output
#      &    LineNumber, LongFile, NRecords, DCO2_A,         !Output
#      &    OZON7_A, PAR_A, WFPASS,                         !Output
#      &    RAIN_A, RHUM_A, SRAD_A, TDEW_A, TMAX_A,         !Output
#      &    TMIN_A, VAPR_A, WINDSP_A, YRDOY_A, YREND)       !Output
#         IF (ErrCode > 0) RETURN
#       ENDIF

        IPWTH.YRDOYWY = INCYD(YRDOY,-1) #Set YRDOYWY to one day before current simulation date
        if IPWTH.LastRec > 0:
            if IPWTH.YRDOYWY < IPWTH.YRDOY_A[IPWTH.LastRec]:
                IPWTH.LastRec = 0
#     ---------------------------------------------------------
#     Retreive daily weather data from stored arrays
        for I in range(IPWTH.LastRec + 1, IPWTH.NRecords):
            #Recnum = LineNumber(I) !Line number in weather file
            if IPWTH.YRDOY_A[I] != YRDOY:
                if YRDOY_A[I] < YRSIM:
                    continue
                YRDOYW = YRDOY_A[I]
                YEARW, DOYW = YR_DOY(YRDOYW)
                if YRDOYW <= IPWTH.YRDOYWY:
                    MSG[0] = "Duplicate record found in weather file."
                    MSG[1] = f"Year {YEARW} DOY {DOYW}"
                    MSG[2] = f"File: {IPWTH.WFile.strip()}"
                    MSG[3] = "Record will be ignored."
                    WARNING(4, ERRKEY, MSG)
                    continue
                elif YRDOYW > INCYD(IPWTH.YRDOYWY,1): #Date skipped
                    ErrCode = 10
                    # CALL WeatherError(CONTROL, ErrCode, IPWTH.FILEWW, RecNum,
                    # & INCYD(YRDOYWY, 1), YREND)
                    return
#           Date OK, continue
            YRDOYWY = YRDOY
            SRAD   = IPWTH.SRAD_A[I]
            TMAX   = IPWTH.TMAX_A[I]
            TMIN   = IPWTH.TMIN_A[I]
            RAIN   = IPWTH.RAIN_A[I]
            TDEW   = IPWTH.TDEW_A[I]
            WINDSP = IPWTH.WINDSP_A[I]
            PAR    = IPWTH.PAR_A[I]
            RHUM   = IPWTH.RHUM_A[I]
            VAPR   = IPWTH.VAPR_A[I]
            DCO2   = IPWTH.DCO2_A[I]
            OZON7  = IPWTH.OZON7_A[I]

            IPWTH.LastRec = I
            break

# !     Error checking
#       CALL DailyWeatherCheck(CONTROL,
#      &    ERRKEY, IPWTH.FILEWW, RAIN, RecNum,                   !Input
#      &    SRAD, TMAX, TMIN, YRDOY,                        !Input
#      &    YREND)                                          !Output
            # (ErrCode, FirstWeatherDay, LastWeatherDay,
            # LineNumber, LongFile, NRecords, DCO2_A,
            # OZON7_A, PAR_A, WFPASS,
            # RAIN_A, RHUM_A, SRAD_A, TDEW_A, TMAX_A,
            # TMIN_A, VAPR_A, WINDSP_A, YRDOY_A, YREND) =(
            #     IpWRex(CONTROL, MaxRecords, COL, ICOUNT, IPWTH.FILEWW, HEADER, LINWTH))
    YREND = -99
    return (IPWTH.CCO2, DCO2, IPWTH.WFile, FILEWC, FILEWG, IPWTH.FILEWW, IPWTH.MEWTH, OZON7, PAR,
            PATHWTC, PATHWTG, IPWTH.PATHWTW, RAIN, IPWTH.REFHT, RHUM, IPWTH.RSEED1, SRAD, IPWTH.TAMP, IPWTH.TAV, TDEW,
            TMAX, TMIN, VAPR, IPWTH.WINDHT, WINDSP, IPWTH.XELEV, IPWTH.XLAT, IPWTH.XLONG, YREND)


def PARSE_COL(lines):
    """
    Parses Headers, assuming that it is the first element of lines, and determines column widths.
    For columns widths, assumes that it includes all leading spaces until end of header.
    For Example, 800.0 will belong to COL2:
    "COL1  COL2"
    " 9.0 800.0"

    :param lines: Array containing txt file data
    :return: Dictionary where key are headers, and values are column data
    """
    import numpy as np

    header_line = lines[0]
    headers = []
    positions = [] #

    pos = 0
    start = 0
    while pos < len(header_line):
        if header_line[pos].isspace():
            start = pos
            while pos < len(header_line) and header_line[pos].isspace(): #Skip spaces to next header
                pos += 1
        else:
            while pos < len(header_line) and not header_line[pos].isspace(): #Skip over header to end of column pos
                pos += 1
            headers.append(header_line[start:pos])
            positions.append((start, pos))


    data = {h: [] for h in headers}

    for line in lines[1:]:
        for (h, (start, end)) in zip(headers, positions):
            field = line[start:end].strip()
            val = field if field else np.nan
            data[h].append(val)

    for h, c in data.items(): #If there is only one element under headers, return a single element instead of a list
        if len(c) == 1:
            data[h] = c[0]

    return {h: column for h, column in data.items()}

def IpWRec(data_dict, MaxRecords, century):
    import math
    ERRKEY = "IPWTH"
    YRDOY_A = [-99] * MaxRecords
    SRAD_A = [-99.0] * MaxRecords
    TMAX_A = [-99.0] * MaxRecords
    TMIN_A = [-99.0] * MaxRecords
    RAIN_A = [-99.0] * MaxRecords
    TDEW_A = [-99.0] * MaxRecords
    WINDSP_A = [-99.0] * MaxRecords
    PAR_A = [-99.0] * MaxRecords
    RHUM_A = [-99.0] * MaxRecords
    VAPR_A = [-99.0] * MaxRecords
    DCO2_A = [-99.0] * MaxRecords
    OZON7_A = [-99.0] * MaxRecords
    for key, val in data_dict.items():
        key = key.strip().upper()
        if key == '@DATE':
            for i in range(len(val)):
                try:
                    YRDOY_A[i] = int(val[i]) + century * 100000
                except:
                    YRDOY_A[i] = -99
        elif key == 'SRAD':
            for i in range(len(val)):
                try:
                    val[i] = float(val[i])
                    if not math.isnan(val[i]): SRAD_A[i] = val[i]
                except:
                    SRAD_A[i] = -99.0
        elif key == 'TMAX':
            for i in range(len(val)):
                try:
                    val[i] = float(val[i])
                    if not math.isnan(val[i]): TMAX_A[i] = float(val[i])
                except:
                    TMAX_A[i] = -99.0

        elif key == 'TMIN':
            for i in range(len(val)):
                try:
                    val[i] = float(val[i])
                    if not math.isnan(val[i]): TMIN_A[i] = float(val[i])
                except:
                    TMIN_A[i] = -99.0

        elif key == 'RAIN':
            for i in range(len(val)):
                try:
                    val[i] = float(val[i])
                    if not math.isnan(val[i]): RAIN_A[i] = float(val[i])
                except:
                    RAIN_A[i] = -99.0

        elif key in ('DEWP', 'TDEW'):
            for i in range(len(val)):
                try:
                    val[i] = float(val[i])
                    if not math.isnan(val[i]): TDEW_A[i] = float(val[i])
                except:
                    TDEW_A[i] = -99.0

        elif key == 'WIND':
            for i in range(len(val)):
                try:
                    val[i] = float(val[i])
                    if not math.isnan(val[i]): WINDSP_A[i] = float(val[i])
                except:
                    WINDSP_A[i] = -99.0

        elif key == 'PAR':
            for i in range(len(val)):
                try:
                    val[i] = float(val[i])
                    if not math.isnan(val[i]): PAR_A[i] = float(val[i])
                except:
                    PAR_A[i] = -99.0

        elif key == 'RHUM':
            for i in range(len(val)):
                try:
                    val[i] = float(val[i])
                    if not math.isnan(val[i]): RHUM_A[i] = float(val[i])
                except:
                    RHUM_A[i] = -99.0

        elif key in ('VAPR', 'VPRS'):
            for i in range(len(val)):
                try:
                    val[i] = float(val[i])
                    if not math.isnan(val[i]): VAPR_A[i] = float(val[i])
                except:
                    VAPR_A[i] = -99.0
        elif key in ('DCO2', 'CO2'):
            for i in range(len(val)):
                try:
                    val[i] = float(val[i])
                    if not math.isnan(val[i]): DCO2_A[i] = float(val[i])
                except:
                    DCO2_A[i] = -99.0

        elif key == 'OZON7':
            for i in range(len(val)):
                try:
                    val[i] = float(val[i])
                    if not math.isnan(val[i]): OZON7_A[i] = float(val[i])
                except:
                    OZON7_A[i] = -99.0

    first_date_ind = 0
    for i in range(len(YRDOY_A)):
        if YRDOY_A[i] != -99 and YRDOY_A[i] != np.nan:
            first_date_ind = 0
            break
    FirstWeatherDate = YRDOY_A[first_date_ind]

    NRecords = 0
    last_date_ind = 0
    for i in range(first_date_ind,len(YRDOY_A)):
        if i == len(YRDOY_A)-1 or YRDOY_A[i] == -99 and YRDOY_A[i] == np.nan:
            last_date_ind = i
        NRecords += 1
    LastWeatherDate = YRDOY_A[last_date_ind]



    return (FirstWeatherDate, LastWeatherDate, NRecords,
            YRDOY_A, SRAD_A, TMAX_A, TMIN_A, RAIN_A, TDEW_A, WINDSP_A, PAR_A, RHUM_A, VAPR_A, DCO2_A, OZON7_A)

#ErrCode, FirstWeatherDay, LastWeatherDay,       !Output
#     &    LineNumber, LongFile, NRecords, DCO2_A
#YREND

#Testing weather data reading
# IPWTH.FILEWW = "C:/DSSAT48/Weather/UFBA1701.WTH"
# with open(IPWTH.FILEWW, 'r') as f:
#     lines = f.readlines()
#
# data_dict = PARSE_COL(lines[4:])
# MaxRecords = 400
# for key, val in data_dict.items():
#     print(f"{key}: {val}")
#
# arrs = IpWRec(data_dict, MaxRecords)
# print("\n\n")
# for a in arrs:
#     print(a)

#Testing IPWTH
# from ModuleDefs import ControlType
# CONTROL = ControlType()
# CONTROL.RNMODE = 'B'
# CONTROL.FILEIO = 'DSSAT48.INP'
# CONTROL.RUN = 1
# SOURCE = 'WEATHR'
# CONTROL.YRDOY = 2017274
# CONTROL.YRSIM = 2017274
#
# DYNAMIC = 1
# print(IPWTH(CONTROL, SOURCE, DYNAMIC))
# DYNAMIC = 2
# print(IPWTH(CONTROL, SOURCE, DYNAMIC), '\n')
#
# DYNAMIC = 3 # Rate
# CONTROL.YRDOY = 2017274
# CONTROL.YRSIM = 2017274
# print("------RATE--------")
# for date in range (2017274,2017280):
#     CONTROL.YRDOY = date
#     print(date, ": ", IPWTH(CONTROL, SOURCE, DYNAMIC), '\n')



