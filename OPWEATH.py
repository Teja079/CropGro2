# =======================================================================
#   OpWeath, Subroutine
#   Generates output for daily weather data.
# -----------------------------------------------------------------------
#   Called from:   WEATHR
#   Calls:         None
# =======================================================================
from ModuleDefs import NOHEADER


def OpWeath(CONTROL, ISWITCH,
            CLOUDS, CO2, DAYL, FYRDOY, OZON7, PAR, RAIN,    #Daily values
            SRAD, TAVG, TDAY, TDEW, TGROAV, TGRODY,         #Daily values
            TMAX, TMIN, TWILEN, WINDSP, WEATHER):
    from ModuleDefs import RunConstants as RC
    from DATES import YR_DOY
    import fortranformat as ff
    import os
    from HEADER import HEADER

    ERRKEY = 'OPWTHR'
    OUTWTH = 'Weather.OUT'

    IDETG = ISWITCH.IDETG
    IDETL = ISWITCH.IDETL
    if IDETG != 'Y' or IDETL == '0': return

    DAS = CONTROL.DAS
    DYNAMIC = CONTROL.DYNAMIC
    FROP = CONTROL.FROP
    RNMODE = CONTROL.RNMODE
    REPNO = CONTROL.REPNO
    RUN = CONTROL.RUN
    YRDOY = CONTROL.YRDOY

    FMOPT = ISWITCH.FMOPT

    if DYNAMIC == RC.SEASINIT:
        if FMOPT == 'A' or FMOPT == ' ':
            write_mode = "w"
            if os.path.exists(OUTWTH):
                write_mode = "a"
            with open(OUTWTH, write_mode) as f:
                if write_mode == "w":
                    f.write("*WEATHER MODULE DAILY OUTPUT FILE\n")

                if RNMODE != 'Q' or RUN == 1:
                    # if RNMODE == 'Q':
                    #     HEADER(RC.SEASINIT, LUN, REPNO)
                    # else:
                    #     HEADER(RC.SEASINIT, LUN, RUN)
                    HEADER(RC.SEASINIT, f, RUN)
                    f.write('@YEAR DOY   DAS'
                            '   PRED  DAYLD   TWLD   SRAD   PARD   CLDD   TMXD   TMND   TAVD'
                            '   TDYD   TDWD   TGAD   TGRD   WDSD   CO2D   VPDF    VPD  OZON7'
                            '   WDATE')
    elif DYNAMIC == RC.OUTPUT or DYNAMIC == RC.SEASEND:
        if ((DYNAMIC == RC.OUTPUT and DAS % FROP == 0) or
                (DYNAMIC == RC.SEASEND and DAS % FROP != 0) or
                DAS == 1):

            VPDF = WEATHER.VPDF
            vpd_transp = WEATHER.VPD_TRANSP
            # These can be modified by ETPHOT during hourly energy balance
            TGROAV = WEATHER.TGROAV
            TGRODY = WEATHER.TGRODY

            YEAR, DOY = YR_DOY(YRDOY)

            if FMOPT == 'A' or FMOPT == ' ':  # VSH
                # Daily printout
                if FYRDOY > 0:
                    WDATE = FYRDOY  # Date for weather forecasting ensembles
                else:
                    WDATE = YRDOY
                f300 = ff.FortranRecordWriter("1X,I4,1X,I3.3,1X,I5, 5(1X,F6.1),1X,F6.2,"
                                              "8(1X,F6.1),F7.1, 1x, F6.2, 1X, F6.2, F7.2, I8")
                with open(OUTWTH, "a") as f:
                    f.write(f300.write([YEAR, DOY, DAS, RAIN, DAYL, TWILEN, SRAD, PAR, CLOUDS, TMAX,
                                        TMIN, TAVG, TDAY, TDEW, TGROAV, TGRODY, WINDSP, CO2, VPDF,
                                        vpd_transp, WDATE]))

