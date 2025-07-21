#=======================================================================
#  OPHEAD
#  Creates List of Strings for Overview.OUT file
#-----------------------------------------------------------------------
#  INPUT  : DYNAMIC, LUNOV,CUMDEP,TPESW,VRNAME,AINO3,AINH4,
#           ECONO,RUN,MODEL,TITLET,WTHSTR, RNMODE,
#           CONTROL, ISWITCH, UseSimCtr, PATHEX
#
#           WSTA,PEDON,TRTNO,CUMDEP,NFERT,TOTNAP,NARES,RESAMT,NAPW,TOTAPW,
#           SLDESC,TPESW,ROWSPC,PLTPOP,VRNAME,AINO3,AINH4,YEAR,SLTX,LUNOV,
#           YRSIM,YRPLT,SOILNX,DSOILN,SOILNC,ECONAM,RUN,MODEL,
#           EXPER,CROP,CROPD,DSOIL,THETAC,TITLET
#
#  OUTPUT : HEADER (List)
#-----------------------------------------------------------------------
#  Called : OPDAY
#=======================================================================
def OPHEAD (DYNAMIC, LUNOV,CUMDEP,TPESW,VRNAME,AINO3,AINH4,
            ECONO,RUN,MODEL,TITLET,WTHSTR, RNMODE,
            CONTROL, ISWITCH, UseSimCtr, PATHEX):

    from COMSWI import SWITCH as sw
    from COMIBS import IBS01 as ib1, IBS02 as ib2, IBS03 as ib3, IBS04 as ib4
    from COMSOI import SOI01 as soi1
    from DATES import YR_DOY, NAILUJ
    from ModuleDefs import RunConstants, Headers, Version, VBranch
    from UTILS import date_and_time, NINT
    import fortranformat as ff
    import calendar

    fmt100 = ff.FortranRecordWriter(
        '"*DSSAT Cropping System Model Ver. ",I6, 1X,A,2X,A3," ",I2.2,", ",I4,I3.2,":",I2.2,":",I2.2')

    fmt200 = ff.FortranRecordWriter("'*RUN ',I3,8X,': ',A25")
    fmt201 = ff.FortranRecordWriter("'*RUN ',I3,8X,': ',A25,1X,A8,1X,A8,I5")

    fmt300 = ff.FortranRecordWriter("1X,'MODEL',10X,':',1X,A8,' - ',A16")
    fmt310 = ff.FortranRecordWriter("1X,'EXPERIMENT',5X,':',1X,A8,1X,A2,1X,A50")
    fmt312 = ff.FortranRecordWriter("1X,'DATA PATH ',5X,':',1X,A")
    fmt320 = ff.FortranRecordWriter("1X,'TREATMENT',I3,3X,':',1X,A25,1X,A8")
    fmt330 = ff.FortranRecordWriter("1X,'CROP',11X,':',1X,A16,1X,'CULTIVAR :',A17,1X,'ECOTYPE :',A6")

    fmt400 = ff.FortranRecordWriter("1X,'STARTING DATE  :',1X,A3,1X,I2,1X,I4")
    fmt425 = ff.FortranRecordWriter("1X,'HARVEST DATE   :',1X,A3,1X,I2,1X,I4")
    fmt450 = ff.FortranRecordWriter(
        "1X,'PLANTING DATE  :',1X,A3,1X,I2,1X,I4,6X,'PLANTS/m2 :',F8.1,4X,'ROW SPACING :',F5.0,'cm '")
    fmt475 = ff.FortranRecordWriter(
        "1X,'PLANTING DATE  :',1X,'AUTOMATIC PLANTING',1X,'PLANTS/m2 :',F5.1,5X,'ROW SPACING :',F5.0,'cm '")

    fmt500 = ff.FortranRecordWriter("1X,'WEATHER',8X,':',1X,A4,3X,I4")
    fmt600 = ff.FortranRecordWriter("1X,'SOIL',11X,':',1X,A10,5X,'TEXTURE : ',A5,' - ',A25")
    fmt625 = ff.FortranRecordWriter(
        "1X,'SOIL INIT COND ',':',1X,'DEPTH:',I3,'cm',1X,'EXTR. H2O:',F5.1,'mm  NO3:',F5.1,'kg/ha  NH4:',F5.1,'kg/ha'")

    fmt650 = ff.FortranRecordWriter("1X,'WATER BALANCE',2X,':',1X,'IRRIGATE ON',' REPORTED DATE(S)'")
    fmt655 = ff.FortranRecordWriter("1X,'WATER BALANCE',2X,':',1X,'IRRIGATE ON REPORTED',' DAP'")
    fmt660 = ff.FortranRecordWriter("' IRRIGATION     : ',I8,' mm IN ',I5,' APPLICATIONS'")
    fmt665 = ff.FortranRecordWriter("' WATER BALANCE  : AUTOMATIC IRRIGATION - REFILL PROFILE'")
    fmt666 = ff.FortranRecordWriter("' IRRIGATION     : AUTOMATIC [SOIL DEPTH:',F5.2,' m',1X,F3.0,'%]'")
    fmt670 = ff.FortranRecordWriter("' WATER BALANCE  : AUTOMATIC IRRIGATION - FIXED AMOUNT'")
    fmt675 = ff.FortranRecordWriter("' WATER BALANCE  : ET AUTO IRRIGATION - REFILL PROFILE'")
    fmt676 = ff.FortranRecordWriter("' IRRIGATION     : AUTOMATIC [ET ACCUM:',F5.2,' mm ]'")
    fmt680 = ff.FortranRecordWriter("' WATER BALANCE  : ET AUTOMATIC IRRIGATION - FIXED AMOUNT'")
    fmt690 = ff.FortranRecordWriter("' WATER BALANCE  : RAINFED'")
    fmt691 = ff.FortranRecordWriter("' IRRIGATION     : NOT IRRIGATED'")

    fmt705 = ff.FortranRecordWriter("1X,'WATER BALANCE',2X,':',1X,'NOT SIMULATED ;',' NO H2O-STRESS'")
    fmt710 = ff.FortranRecordWriter("1X,'IRRIGATION',5X,':'")

    fmt720 = ff.FortranRecordWriter("1X,'NITROGEN BAL.',2X,':',1X,'SOIL-N, N-UPTAKE & DYNAMIC N-FIXATION SIMULATION'")
    fmt730 = ff.FortranRecordWriter("1X,'NITROGEN BAL.',2X,':',1X,'SOIL-N, N-UPTAKE & UNLIMITED N-FIXATION SIMULATION'")
    fmt740 = ff.FortranRecordWriter("1X,'NITROGEN BAL.',2X,':',1X,'SOIL-N & N-UPTAKE SIMULATION; NO N-FIXATION'")
    fmt750 = ff.FortranRecordWriter("1X,'NITROGEN BAL.',2X,':',1X,'NOT SIMULATED ; NO N-STRESS'")
    fmt751 = ff.FortranRecordWriter("1X,'N-FERTILIZER',3X,':'")
    fmt752 = ff.FortranRecordWriter("1X,'RESIDUE/MANURE',1X,':'")

    fmt800 = ff.FortranRecordWriter("1X,'N-FERTILIZER',3X,':',1X,I8,' kg/ha IN ',I5,' APPLICATIONS'")
    fmt810 = ff.FortranRecordWriter(
        "1X,'N-FERTILIZER',3X,':',1X,'AUTO APPLICATIONS ',F5.0,' kg/ha AT ',F6.2,' cm AND',F5.2,' % STRESS'")
    fmt820 = ff.FortranRecordWriter("1X,'N-FERTILIZER',3X,':',1X,'NO N-FERTILIZER APPLIED'")

    fmt1000 = ff.FortranRecordWriter(
        "1X,'RESIDUE/MANURE',1X,':',1X,'INITIAL : ',I5,' kg/ha ;',I8,' kg/ha IN ',I5,' APPLICATIONS'")

    fmt1200 = ff.FortranRecordWriter("1X,'ENVIRONM. OPT. :',1X,A60")
    fmt1210 = ff.FortranRecordWriter("18X,A60")

    fmt1300 = ff.FortranRecordWriter(
        "1X,'SIMULATION OPT : WATER',3X,':',A1,2X,'NITROGEN:',A1,2X,'N-FIX:',A1,2X,'PHOSPH :',A1,2X,'PESTS  :',A1")
    fmt1350 = ff.FortranRecordWriter(
        "18X,'PHOTO',3X,':',A1,2X,'ET      :',A1,2X,'INFIL:',A1,2X,'HYDROL :',A1,2X,'SOM    :',A1")
    fmt1355 = ff.FortranRecordWriter(
        "18X,'CO2  ',3X,':',A1,2X,'NSWIT   :',I1,2X,'EVAP :',A1,2X,'SOIL   :',A1,2X,'STEMP  :',A1")

    fmt1400 = ff.FortranRecordWriter(
        "1X,'MANAGEMENT OPT : PLANTING:',A1,2X,'IRRIG',3X,':',A1,2X,'FERT :',A1,2X,'RESIDUE:',A1,2X,'HARVEST:',A1,2X")
    fmt1405 = ff.FortranRecordWriter("18X,'WEATHER :',A1,2X,'TILLAGE :',A1")

    if LUNOV == 6: #Print HEADER list to console
        for i in range(Headers.ICOUNT):
            print(Headers.Header[i])

    if sw.TITLER[0:5] == '     ': TITLER = TITLET

#     ******************************************************************
#     ******************************************************************
    HEADER = [' '] * 100
#     Generate short header
    if DYNAMIC == RunConstants.RUNINIT:
#     ------------------------------------------------------------------
#         HEADER = create_str_array(8) #Simulation header
        date_time = date_and_time()[3]
#       date_time(1)  The 4-digit year
#       date_time(2)  The month of the year
#       date_time(3)  The day of the month
#       date_time(4)  The time difference with respect to Coordinated Universal Time (UTC) in minutes
#       date_time(5)  The hour of the day (range 0 to 23) - local time
#       date_time(6)  The minutes of the hour (range 0 to 59) - local time
#       date_time(7)  The seconds of the minute (range 0 to 59) - local time
#       date_time(8)  The milliseconds of the second (range 0 to 999) - local time

#       Version information stored in ModuleDefs.for
        HEADER[0] = (fmt100.write([Version,VBranch, calendar.month_name[date_time[1]],
                                  date_time[2], date_time[0], date_time[4], date_time[5],
                                  date_time[6]]))
        HEADER[1] = ' '
        HEADER[2] = fmt201.write([RUN%1000, TITLET, MODEL, ib1.EXPER, ib2.TRTNO])

        I = MODEL.find(" ")
        if I == -1: #Index not found
            HEADER[3] = fmt300.write([MODEL[0:8], ib1.CROPD])
        else:
            HEADER[3] = " "

        HEADER[4] = fmt310.write([ib1.EXPER, sw.CG, sw.ENAME])

        HEADER[5] = fmt312.write([PATHEX])

        if 'FQ'.find(RNMODE) > -1 :
            HEADER[6] = fmt320.write([CONTROL.ROTNUM%1000, TITLET, MODEL])
        else:
            HEADER[6] = fmt320.write([ib2.TRTNO%1000, TITLET, MODEL])

        HEADER[7] = ' '

        I = 8
        Headers.shortCount  = I-2
        Headers.ICOUNT      = I-1
        Headers.Header      = HEADER
        Headers.RUN         = RUN
#   ******************************************************************
#   ******************************************************************
#   Continue with long header
#    ------------------------------------------------------------------
    else:
        HEADER = Headers.Header # Retrieve HEADER from Headers object in ModuleDefs

        ICO2   = ISWITCH.ICO2 #TODO Most of these not implemented in switchtype -HZ
        IFERI  = ISWITCH.IFERI
        IHARI  = ISWITCH.IHARI
        IIRRI  = ISWITCH.IIRRI
        IPLTI  = ISWITCH.IPLTI
        IRESI  = ISWITCH.IRESI
        ISWNIT = ISWITCH.ISWNIT
        ISWPHO = ISWITCH.ISWPHO
        ISWSYM = ISWITCH.ISWSYM
        ISWTIL = ISWITCH.ISWTIL
        ISWWAT = ISWITCH.ISWWAT
        MEEVP  = ISWITCH.MEEVP
        MEHYD  = ISWITCH.MEHYD
        MEINF  = ISWITCH.MEINF
        MEPHO  = ISWITCH.MEPHO
        MESEV  = ISWITCH.MESEV
        MESOL  = ISWITCH.MESOL
        METMP  = ISWITCH.METMP
        MEWTH  = ISWITCH.MEWTH
        NSWITCH= ISWITCH.NSWI

        if UseSimCtr:
            I = 8
            HEADER[I] = "!Simulation control file used for this simulation."
            SimLen = len(CONTROL.SimControl)
            I += 1
            HEADER[I] = f"!File: {CONTROL.SimControl[0:SimLen]}"
            I += 1
            HEADER[I] = "!See top of WARNING.OUT file for specific controls."
            Headers.shortCount = len(HEADER) - 1

        I = 8 # VSH added
        HEADER[I] = " "
        I += 1
        HEADER[I] = fmt330.write([ib1.CROPD, VRNAME, ECONO])
#
        IPYRS, ISIM = YR_DOY(CONTROL.YRSIM)
        RMS, IDYS = NAILUJ(ISIM, IPYRS)
        I += 1
        HEADER[I] = fmt400.write([RMS,IDYS,IPYRS])
        IPYRP, IPLT = YR_DOY (ib2.YRPLT)
#
#       Output the line with planting date, density and rowspacing
        I += 1
        if IPLT <= 366 and IPLTI == 'R':
            RMP, IDYP = NAILUJ(IPLT, IPYRP)
            HEADER[I] = fmt450.write([RMP, IDYP, IPYRP, ib3.PLTPOP, ib3.ROWSPC])
        else:
            HEADER[I] = fmt475.write([ib3.PLTPOP, ib3.ROWSPC])

        I += 1
        if IHARI == 'R' and 'FQNY'.find(RNMODE) < 0:
            HDATE_YR, HDATE_DOY = YR_DOY(ib2.HDATE[0])
            RMS, IDYS = NAILUJ(HDATE_DOY,HDATE_YR)
            HEADER[I] = fmt425.write([RMS,IDYS,HDATE_YR])
# c     ::::::::::::::::::::::::::::::::::
#
# c     Weather station and year:
        I += 1
        HEADER[I] = fmt500.write([ib1.WSTA, ib2.YEAR])
# c     MJ, Mar 2008: Soil information
        I += 1
        HEADER[I] = fmt600.write([soi1.PEDON, ib1.SLTX , soi1.SLDESC])

        if ISWWAT != 'N':
            if 'FQ'.find(RNMODE) < 0 or RUN == 1:
                I += 1
                HEADER[I] = fmt625.write([NINT(CUMDEP), TPESW*10, AINO3, AINH4])
            if IIRRI == 'R' or 'D':
                I += 1
                if IIRRI == 'R':
                    HEADER[I] = fmt650.write([])
                else:
                    HEADER[I] = fmt655.write([])
                #if ib4.TOTAPW == 0 and ib2.NAPW >= 1:
                NNAPW = ib2.NAPW
                I += 1
                HEADER[I] = fmt660.write([NINT(ib4.TOTAPW), NNAPW])
            elif IIRRI == 'A':
                I += 1
                HEADER[I] = fmt665.write([])
                I += 1
                HEADER[I] = fmt666.write([ib3.DSOIL/100, ib3.THETAC])
            elif IIRRI == 'F':
                I += 1
                HEADER[I] = fmt670.write([])
                I += 1
                HEADER[I] = fmt666.write([ib3.DSOIL / 100, ib3.THETAC])
            elif IIRRI == 'E':
                I += 1
                HEADER[I] = fmt675.write([])
                I += 1
                HEADER[I] = fmt676.write([ib3.DSOIL])
            elif IIRRI == 'T':
                I += 1
                HEADER[I] = fmt680.write([])
                I += 1
                HEADER[I] = fmt676.write([ib3.DSOIL])
            elif IIRRI == 'T':
                I += 1
                HEADER[I] = fmt690.write([])
                I += 1
                HEADER[I] = fmt691.write([])

        elif ISWWAT == 'N':
            I += 1
            HEADER[I] = fmt705.write([])
            I += 1
            HEADER[I] = fmt710.write([])

        if ISWNIT == 'Y':
            if ISWSYM == 'Y':
                I += 1
                HEADER[I] = fmt720.write([])
            elif ISWSYM == 'U':
                I += 1
                HEADER[I] = fmt730.write([])
            elif ISWSYM == 'N':
                I += 1
                HEADER[I] = fmt740.write([])

            if IFERI == 'R' or 'D':
                if ib4.TOTNAP == 0 and ib2.NFERT >= 1:
                    NNFERT = 0
                else:
                    NNFERT = ib2.NFERT
                I += 1
                HEADER[I] = fmt800.write([NINT(ib4.TOTNAP), NNFERT])
            elif IFERI == 'A' or 'F':
                I += 1
                HEADER[I] = fmt810.write([ib3.SOILNX, ib3.DSOILN, ib3.SOILNC])
            elif IFERI == 'N':
                I += 1
                HEADER[I] = fmt820.write([])
            I += 1
            HEADER[I] = fmt1000.write([NINT(ib4.ICRES), NINT(ib4.RESAMT), ib2.NARES])
        else:
            I += 1
            HEADER[I] = fmt750.write([])
            I += 1
            HEADER[I] = fmt751.write([])
            I += 1
            HEADER[I] = fmt752.write([])

        if len(WTHSTR) > 1:
            I += 1
            HEADER[I] = fmt1200.write([WTHSTR[0:60]])
            I += 1
            HEADER[I] = fmt1210.write([WTHSTR[61:120]])

        HEADER[I] = fmt1300.write([ISWWAT, ISWNIT, ISWSYM, ISWPHO, sw.ISWDIS]); I += 1
        HEADER[I] = fmt1350.write([MEPHO, MEEVP, MEINF, MEHYD, sw.MESOM]); I += 1
        HEADER[I] = fmt1355.write([ICO2, NSWITCH, MESEV, MESOL, METMP]); I += 1
        HEADER[I] = fmt1400.write([IPLTI, IIRRI, IFERI, IRESI, IHARI]); I += 1
        HEADER[I] = fmt1405.write([MEWTH, ISWTIL])

        ICOUNT = I
        Headers.ICOUNT = ICOUNT
        Headers.Header = HEADER
        Headers.RUN = RUN

        # return Headers
#=======================================================================

#=======================================================================
#  HEADER, Subroutine
#-----------------------------------------------------------------------
#  Writes simulation header info to file.
#     Currently, the routine reads header info from a file (written
#     previously by input module or elsewhere in the code).  Probably
#     should generate the header info here.
#-----------------------------------------------------------------------
# Called by: IRRIG, OPWBAL, OPGROW, . . .
#========================================================================
def HEADER(DYNAMIC, LUNDES, RUN):
    """
    Reads up to 100 lines of header information and prints to output files as needed.
    DYNAMIC = 0 : Prints version number and date/time only
    DYNAMIC = 1 (RUNINIT) : Complete header is printed
    DYNAMIC = 2 (SEASINIT): Short header is printed
    """
    import CSMVersion
    from ModuleDefs import GET_CONTROL, Headers, RunConstants, NOHEADER
    from UTILS import date_and_time
    # COMIBS
    # COMSWI

    ERRKEY = 'HEADER'
    MAXLUN = 200
    PREV_RUN = 0

    CONTROL = GET_CONTROL()

    ICOUNT = Headers.ICOUNT
    ShortCount = Headers.ShortCount

    # # import Headers TODO connects Headers instance from that module
    # h = HEADER
    # if not all (hasattr(h, attr) for attr in ['PREV_RUN', 'NOHEADER']):
    #     h.PREV_RUN = 0
    #     h.NOHEADER = []
    #
    # # Reset NOHEADER if RUN has changed
    # if RUN != h.PREV_RUN:
    #     h.NOHEADER = [True] * MAXLUN
    #     h.PREV_RUN = RUN

    if LUNDES not in NOHEADER:
        NOHEADER[LUNDES] = True

    if RUN != PREV_RUN:
        NOHEADER[LUNDES] = True
        PREV_RUN = RUN

    # Print model info and date-time stamp if no headers exist
    FLUNDES = open(LUNDES, 'a')
    if ICOUNT > 0:
        FLUNDES.write(f"\n{Headers.Header[0].rstrip()}")  # LUNDES must be closed in the modules that call HEADER
    else:
        DATE_TIME = date_and_time()[3]
        version_info = (f"*CropGro Template Model Ver. {CSMVersion.Version.Major:1d}.{CSMVersion.VBranch:1d}.{DATE_TIME[1]:1d}."
                        f"{DATE_TIME[2]:03d} {DATE_TIME[0]} {DATE_TIME[4]:3d} "
                        f"{DATE_TIME[5]:02d},{DATE_TIME[6]:4d}; \n")
                        # {hour:02d}:{minute:02d}:{second:02d}\n")
        # TODO Version and VBranch are global? Arrays? Need to be added
        FLUNDES.write(version_info)
        FLUNDES.write(f"\n*RUN {RUN % 1000:3d}\n")
        FLUNDES.close()
        return

    # Run Initialization
    if DYNAMIC == RunConstants.RUNINIT:
        # Write OVERVIEW header info to destination file.
        for I in range(1, ICOUNT):  # Skip the first line
            FLUNDES.write(Headers.Header[I].rstrip())
    # Seasonal Initialization
    elif DYNAMIC == RunConstants.SEASINIT:
        # if LUNDES > MAXLUN:  ##TODO unclear about how this part works
        #     return

        if NOHEADER[LUNDES] and RUN > 0 and ICOUNT > 0:  #
            # Header exists, write it
            for I in range(1, ShortCount):
                FLUNDES.write(Headers.Header[I].strip() + "\n")
            NOHEADER[LUNDES] = False
    FLUNDES.close()
    return
#========================================================================

def MULTIRUN(RUN, YRPLT):
    """
    Updates header for multi-year runs.
    """
    from DATES import YR_DOY, NAILUJ
    from ModuleDefs import GET_CONTROL, Headers, GET_WEATHER

    CONTROL = GET_CONTROL()

    # Update RUN number in header
    for I in range(1, Headers.ShortCount):
        if Headers.Header[I][:4] == "*RUN":
            HEADER2 = Headers.Header[I]
            Headers.Header[I] = f"{HEADER2[:5]:5s}{RUN%1000:3d}{HEADER2[8:80]}"
            Headers.RUN = RUN
            break

    # Update simulation start date
    for I in range(1, Headers.ICOUNT):
        if Headers.Header[I][:6] == " START":
            IPYRS, ISIM = YR_DOY(CONTROL.YRSIM)
            RMS, IDYS = NAILUJ(ISIM, IPYRS)
            Headers.Header[I] = f" STARTING DATE  : {RMS:3s} {IDYS:2d} {IPYRS:4d}"
            break

    # Update planting date if applicable
    if YRPLT > 0:
        for I in range(1, Headers.ICOUNT):
            if Headers.Header[I][:6] == " PLANT":
                IPYRS, IPLT = YR_DOY(YRPLT)
                RMS, IDYS = NAILUJ(IPLT, IPYRS)
                TEXT = f"{RMS:3s} {IDYS:2d} {IPYRS:4d}"
                Headers.Header[I] = Headers.Header[I][:18] + TEXT + "       "
                break

    # Update WEATHER file
    for I in range(1, Headers.ICOUNT):
        if Headers.Header[I][:6] == " WEATH":
            WSTAT = GET_WEATHER("WEATHER", "WSTA")  #TODO Needs to be implemented (Return WSTA from weather module)
            if len(WSTAT) > 0:
                Headers.Header[I] = f" WEATHER        : {WSTAT}"
            break

    #return
#========================================================================

#======================================================================
# OPSOIL, Subroutine
# Generates output for soil data
#----------------------------------------------------------------------
# INPUT  : IDETO,NOUTDO,NYRS,LL,DUL,SAT,DLAYR,SWINIT,DS,NLAYR,ESW
#          SHF,BD,PH,INO3,INH4,OC,TLL,TDUL,TSAT,TPESW,TSWINI,AINO3,AINH4
#          TSOC,SWCON,U,SALB,CN2,CROPD,VRNAME,VARTY,SLPF,ECONO
#          SLNF,LUNOV,CROP
#
#  OUTPUT :
#-----------------------------------------------------------------------
#  Called : OPIBS3
#  Calls  : CLEAR
#=======================================================================
def OPSOIL (LL,DUL,SAT,DLAYR,SWINIT,DS,NLAYR,ESW,SHF,BD,PH,INO3,INH4,OC,
            TLL,TDUL,TSAT,TPESW,TSWINI,AINO3,AINH4,TSOC,SWCON,U,SALB,CN2,
            CROPD,VRNAME,VARTY,SLPF,ECONO,SLNF,CROP, RNMODE, RUN, MODEL, ISWITCH, ATLINE):

    import fortranformat as ff
    from ModuleDefs import  NL, Headers
    from UTILS import NINT
    from COMGEN import GEN03 as GEN3

#
#       INCLUDE 'COMGEN.blk'
#
    formats = {
        360: ff.FortranRecordWriter('3X,A48,5X,A23'),
        361: ff.FortranRecordWriter('2X,A49,26X,A1'),
        362: ff.FortranRecordWriter('3X,A2,3X,3(A7,4X),5X,A5,9X,A11,5X,A1'),
        362: ff.FortranRecordWriter('79A1'),
        410: ff.FortranRecordWriter("I3,'-',I3,5(1X,F5.3),7(1X,F6.2)"),
        610: ff.FortranRecordWriter("'TOT-',I3,5F6.1,2X,'<--cm   -','  kg/ha-->',2F7.1,I7"),
        710: ff.FortranRecordWriter("'SOIL ALBEDO    :',F5.2,6X,'EVAPORATION LIMIT :',F5.2,9X,'MIN. FACTOR  :',F5.2"),
        711: ff.FortranRecordWriter("'RUNOFF CURVE # :',F5.2,6X,'DRAINAGE RATE     :',F5.2,9X,'FERT. FACTOR :',F5.2"),
        800: ff.FortranRecordWriter("1X,A16,1X,'CULTIVAR: ',A6,'-',A16,3X,'ECOTYPE: ',A6"),
        850: ff.FortranRecordWriter("1X,'CSDVAR :',F5.2,'  PPSEN  :',F5.2,'  EMG-FLW:',F5.2,'  FLW-FSD:',F5.2,"
                                    "'  FSD-PHM :',F6.2"),
        851: ff.FortranRecordWriter("1X,'WTPSD  :',F5.3,'  SDPDVR :',F5.2,'  SDFDUR :',F5.2,"
                                    "'  PODDUR :',F5.2,'  XFRUIT  :',F6.2"),
        853: ff.FortranRecordWriter("1X,'THRESH :',F5.1,'  SDPRO  :',F5.3,'  SDLIP   :',F6.3"),
        852: ff.FortranRecordWriter("1X,'WTPSD  :',F5.3,'  SDPDVR :',F5.1,'  SDFDUR :',F5.2,"
                                    "'  PODDUR :',F5.2,'  XFRUIT  :',F6.2")
    }

    I = Headers.ICOUNT + 1
    HEADER = Headers.Header

    ISWWAT = ISWITCH.ISWWAT
#
# !=======================================================================
# !     SOILS
# !-----------------------------------------------------------------------
#  Don't report initial conditions for sequenced runs.
    if 'FQ'.find(RNMODE) <= 0 or RUN == 1:
        HEADER[I] = " "; I += 1
        HEADER[I] = "*SUMMARY OF SOIL AND GENETIC INPUT PARAMETERS"; I += 1
        HEADER[I] = " "; I += 1
#-----------------------------------------------------------------------
#       Write soils info
        if ISWWAT != 'N':
            HEADER[I] = formats[360].write(['SOIL LOWER UPPER   SAT  EXTR  INIT   ROOT   BULK',
                                              'pH    NO3    NH4    ORG']); I += 1
            HEADER[I] = formats[361].write(['DEPTH LIMIT LIMIT    SW    SW    SW   DIST   DENS','C']); I += 1
            HEADER[I] = formats[362].write(['cm', 'cm3/cm3', 'g/cm3', 'ugN/g  ugN/g', '%']); I += 1
            HEADER[I] = formats[363].write(['-']); I += 1

            for L in range(1, NLAYR+1):
                HEADER[I] = formats[410].write([NINT(DS[L]-DLAYR[L]),NINT(DS[L]),LL[L],
                                                DUL[L],SAT[L],ESW[L],SWINIT[L],SHF[L],BD[L],
                                                PH[L],INO3[L],INH4[L],OC[L]]); I += 1
                HEADER[I] = " "
                HEADER[I] = formats[610].write([NINT(DS[NLAYR]),TLL,TDUL,TSAT,TPESW,TSWINI,
                                                AINO3,AINH4,NINT(TSOC)]); I += 1
                HEADER[I] = formats[710].write([SALB,U,SLNF]); I += 1
                HEADER[I] = formats[711].write([CN2,SWCON,SLPF]); I += 1
                HEADER[I] = " "; I += 1

    else:
        HEADER[I] = '*SUMMARY OF GENETIC INPUT PARAMETERS'; I += 1
        HEADER[I] = " "; I += 1

#=======================================================================
#     GENOTYPE
#-----------------------------------------------------------------------
    # Write genetic coefficients
    HEADER[I] = formats[800].write([CROPD[:16],VARTY,VRNAME,ECONO]); I += 1

    if MODEL[1:5] == 'CRGRO':
# !-----------------------------------------------------------------------
# !     CROPGRO
#       CASE ('CRGRO','PRFRM')
        if 'BG,BN,CH,CP,FB,GB,LT,PE,PN,PP,SB,VB'.find(CROP) > 0:
            # TO DO: indexes 8 and 10 to be checked
            HEADER[I] = formats[850].write([GEN3.CSDVAR,GEN3.PPSEN,GEN3.PH2T5,GEN3.PHTHRS[8],GEN3.PHTHRS[10]]); I += 1
            HEADER[I] = formats[851].write([GEN3.WTPSD,GEN3.SDPDVR,GEN3.SFDUR,GEN3.PODUR,GEN3.XFRUIT]); I += 1
        elif 'AM,BC,BH,BM,BR,CB,CI,CN,CO,CU,GY,NP,PR,QU,SF,SR,SU,TM'.find(CROP) > 0:
            HEADER[I] = formats[850].write([GEN3.CSDVAR,GEN3.PPSEN,GEN3.PH2T5,GEN3.PHTHRS[8],GEN3.PHTHRS[10]]); I += 1
            HEADER[I] = formats[852].write([GEN3.WTPSD,GEN3.SDPDVR,GEN3.SFDUR,GEN3.PODUR,GEN3.XFRUIT]); I += 1

        HEADER[I] = formats[853].write([GEN3.THRESH, GEN3.SDPRO, GEN3.SDLIP])


    Headers.ICOUNT = I
    Headers.Header = HEADER
    
# !-----------------------------------------------------------------------
# !     CSCER - Wheat, barley
#       CASE ('CSCER')
#         WRITE (HEADER(I),'(A)')
#      &    "   P1V   P1D    P5    G1    G2    G3 PHINT"
#         I=I+1
#         WRITE (HEADER(I),'(5(1X,F5.1),1X,F5.2,1X,F5.1)')
#      &    P1V,P1D,P5,G1,G2,G3,PHINT
#         I=I+1
#
# !       Print optional extra stuff from ecotype file
#         LENGTH = LenString(PLAINTXT)
#         IF (LENGTH > 0) THEN
#           DO J=1,5
#             J1 = (J-1)*78+1
#             J2 = J1 + 77
#             WRITE( HEADER(I),'(A)') TRIM(ATLINE(J1+29:J2+29))
#             WRITE( HEADER(I+1),'(A)') TRIM(PLAINTXT(J1:J2))
#             I = I + 2
#             IF (J2 > LENGTH) EXIT
#           ENDDO
#         ENDIF
#
# !-----------------------------------------------------------------------
# !     CSCRP - Wheat, barley, cassava
#       CASE ('CSCRP')
# !       -----------------------------------
# !       wheat, barley
#         IF (INDEX('WH,BA',CROP) > 0) THEN
#           WRITE (HEADER(I),'(A,A)')
#      &      "  VREQ VBASE  VEFF  PPS1  PPS2",
#      &      "    P1    P2    P3    P4    P5    P6    P7    P8"
#           I=I+1
#           WRITE (HEADER(I),'(2F6.1,2F6.2,9F6.1)')
#      &      VREQ, VBASE, VEFF, PPS1, PPS2,
#      &      P1, P2, P3, P4, P5, P6, P7, P8
#           I=I+1
#           WRITE (HEADER(I),'(A)')
#      &      " GNOWT  GWTS SHWTS PHINT  LA1S  LAFV  LAFR"
#           I=I+1
#           WRITE (HEADER(I),'(2F6.1, F6.2,F6.1, 3F6.2)')
#      &      GNOWT, GWTS, SHWTS, PHINT, LA1S, LAFV, LAFR
#           I=I+1
# !       -----------------------------------
# !       cassava
#         ELSEIF (INDEX('CS',CROP) > 0) THEN
#           WRITE (HEADER(I),'(A,A)')
#      &      "  PPS1   P1L   P2L   P4L   P5L SRNOW  SRFR"
# C    &      "  LA1S  LAXS"
# C    &      " LAXNO  LAFS LAFNO  LAWS"
#           I=I+1
# c          WRITE (HEADER(I),'(4F6.1,F6.0,2F6.2,6F6.0)')
# c     &      PPS1, P1L, P2L, P4L, P5L, SRNOW, SRFR, LA1S, LAXS,
# c     &      LAXNO, LAFS, LAFNO, LAWS
#           I=I+1
#           WRITE (HEADER(I),'(A)') " PHINT LLIFA  STFR"
#           I=I+1
# c          WRITE (HEADER(I),'(2F6.0,F6.2)') PHINT, LLIFA, STFR
#           I=I+1
#         ENDIF
#
# !       Print optional extra stuff from ecotype file
#         LENGTH = LenString(PLAINTXT)
#         IF (LENGTH > 0) THEN
#           DO J=1,3
#             J1 = (J-1)*78+1
#             J2 = J1 + 77
#             WRITE( HEADER(I),'(A)') TRIM(ATLINE(J1+149:J2+149))
#             WRITE( HEADER(I+1),'(A)') TRIM(PLAINTXT(J1:J2))
#             I = I + 2
#             IF (J2 > LENGTH) EXIT
#           ENDDO
#         ENDIF
# !-----------------------------------------------------------------------
# !     Cassava
#       CASE ('CSCAS')
#          WRITE (HEADER(I),'(A,A)')
#      &     "  PPS1 B01ND B12ND B23ND B34ND B45ND B56ND",
#      &     " SR#WT  SRFR  HMPC PHINT"
#          I=I+1
#           WRITE (HEADER(I),'(F6.2,6F6.1,4F6.2)')
#      &     PPS1, B01ND, B12ND, B23ND, B34ND, B45ND, B56ND,
#      &     SRNWT, SRFR, HMPC, PHINT
#          I=I+1
#          WRITE (HEADER(I),'(A,A)')
#      &     "  LA1S  LAXS LAXND LAXN2  LAFS LAFND  SLAS",
#      &     " LLIFA LPEFR  STFR"
#         I=I+1
#          WRITE (HEADER(I),'(F6.1,F6.0,6F6.1,2F6.2)')
#      &    LA1S, LAXS, LAXND, LAXN2, LAFS, LAFND, SLASS, LLIFA,
#      &    LPEFR, STFR
#        I=I+1
# !       Print optional extra stuff from ecotype file
#         LENGTH = LenString(PLAINTXT)
#         IF (LENGTH > 0) THEN
#           DO J=1,3
#             J1 = (J-1)*78+1
#             J2 = J1 + 77
#             WRITE( HEADER(I),'(A)') TRIM(ATLINE(J1+149:J2+149))
#             WRITE( HEADER(I+1),'(A)') TRIM(PLAINTXT(J1:J2))
#             I = I + 2
#             IF (J2 > LENGTH) EXIT
#           ENDDO
#         ENDIF
# !-----------------------------------------------------------------------
# !     Cassava CIAT
#       CASE ('CSYCA')
#           WRITE (HEADER(I),'(A,A)')
#      &     "  B01ND B12ND B23ND B34ND BR1FX BR2FX BR3FX BR4FX "
#           I=I+1
#           WRITE (HEADER(I),'(4F6.0,4F6.2)')
#      &     B01ND, B12ND, B23ND, B34ND, BR1FX, BR2FX, BR3FX, BR4FX
#          I=I+1
#          WRITE (HEADER(I),'(A,A)')
#      &     "  LAXS  SLAS",
#      &     " LLIFA LPEFR LNSLP NODWT NODLT"
#         I=I+1
#          WRITE (HEADER(I),'(F6.1,F6.0,2F6.1,3F6.2, 1F6.1)')
#      &    LAXS, SLASS, LLIFA,
#      &    LPEFR, LNSLP, NODWT, NODLT
#        I=I+1
# !       Print optional extra stuff from ecotype file
#         LENGTH = LenString(PLAINTXT)
#         IF (LENGTH > 0) THEN
#           DO J=1,3
#             J1 = (J-1)*78+1
#             J2 = J1 + 77
#             WRITE( HEADER(I),'(A)') TRIM(ATLINE(J1+149:J2+149))
#             WRITE( HEADER(I+1),'(A)') TRIM(PLAINTXT(J1:J2))
#             I = I + 2
#             IF (J2 > LENGTH) EXIT
#           ENDDO
#         ENDIF
#
# !-----------------------------------------------------------------------
# !     CERES-Maize
#       CASE ('MZCER')
#             WRITE (HEADER(I),900) P1,P2,P5; I=I+1
#             WRITE (HEADER(I),901) G2,G3,PHINT; I=I+1
#
# !-----------------------------------------------------------------------
# !     IXIM-Maize
#       CASE ('MZIXM')
#              WRITE (HEADER(I),915) P1,P2,P5,AX; I=I+1
#              WRITE (HEADER(I),916) G2,G3,PHINT,LX; I=I+1
#
# !-----------------------------------------------------------------------
# !     CERES-Sugarbeet
#       CASE ('BSCER')
#             WRITE (HEADER(I),900) P1,P2,P5; I=I+1
#             WRITE (HEADER(I),901) G2,G3,PHINT; I=I+1
#
# !-----------------------------------------------------------------------
# !     Sorghum
#       CASE ('SGCER')
#             WRITE (HEADER(I), 902) P1,P2O,P2R,P5; I=I+1
#             WRITE (HEADER(I),1002) G1,G2,PHINT,P3,P4; I=I+1
#             !Print optional parameters if used.
#             IF (PBASE > 1.E-2 .AND. PSAT > 1.E-2) THEN
#               WRITE(HEADER(I), 1010) PBASE, PSAT; I=I+1
#             ENDIF
#
# !-----------------------------------------------------------------------
# !     Millet
#       CASE ('MLCER')
#             WRITE (HEADER(I), 903) P1,P2O,P2R,P5; I=I+1
#             WRITE (HEADER(I),1003) G1,G4,PHINT; I=I+1
#
# !-----------------------------------------------------------------------
# !     Potato
#       CASE ('PTSUB')
# !      ELSEIF (INDEX ('PT',CROP) .GT. 0) THEN
#          WRITE (HEADER(I), 905) G2,G3; I=I+1
# !         WRITE (HEADER(I), 905) G2,G3,G4; I=I+1
#          WRITE (HEADER(I),1005) PD,P2,TC; I=I+1
#
# !-----------------------------------------------------------------------
# !     Rice
#       CASE ('RICER')
# !      ELSEIF (INDEX ('RI',CROP) .GT. 0) THEN
#          WRITE (HEADER(I), 906) P1,P2R,P5,P2O; I=I+1
#          WRITE (HEADER(I),1006) G1,G2,G3,THOT,TCLDP,TCLDF; I=I+1
#
# !-----------------------------------------------------------------------
# !     Aroids
#       CASE ('TNARO','TRARO')
# !      ELSEIF (INDEX ('TNTR',CROP) .GT. 0) THEN
#          WRITE (HEADER(I), 911) P1,P3,P4,P5; I=I+1
#          WRITE (HEADER(I),1011) G3,G4,PHINT,PCINT,PCGRD; I=I+1
#
# !-----------------------------------------------------------------------
# !     Pineapple **
#       CASE ('PIALO')
#          WRITE (HEADER(I),2010) P1,P2,P3,P4,P5,P6; I=I+1
#          WRITE (HEADER(I),2011) G2,G3,PHINT; I=I+1
#  2010 FORMAT (1X,'    P1:',F6.1,'    P2:',F6.1,
#      &           '    P3:',F6.1,'    P4:',F6.0,
#      &           '    P5:',F6.1,'    P6:',F6.1)
#  2011 FORMAT (1X,'    G2:',F6.1,'    G3:',F6.2,
#      &           ' PHINT:',F6.1)
#
# !-----------------------------------------------------------------------
# !     Sugarcane - Canegro
#       CASE ('SCCAN')
#
# !-----------------------------------------------------------------------
# !     Sugarcane - CASUPRO
#       CASE ('SCCSP')
# !     Not currently correct for either CaneGro or Casupro
# !     CHP removed 8/3/07
# !      ELSEIF (INDEX ('SC',CROP) .GT. 0) THEN
# !        IF (INDEX(MODEL,'SCCAN') > 0) THEN
# !          WRITE (HEADER(I), 907) P1,RATPOT,LFMAX; I=I+1
# !          WRITE (HEADER(I),1007) G1,PI1,PI2,DTTPI; I=I+1
# !        ELSEIF (INDEX(MODEL,'SCCSP') > 0) THEN
# !          WRITE (HEADER(I),957) PI1, PI2, DTPI, SMAX, SO, GMAX, GO; I=I+1
# !          WRITE (HEADER(I),1057) M1, M2, LA, WTPSD, SLAVAR, SIZLF; I=I+1
# !        ENDIF
#
# !-----------------------------------------------------------------------
# !!     Sunflower
# !      ELSEIF (INDEX ('SU',CROP) .GT. 0) THEN
# !         WRITE (HEADER(I), 908) P1,P2,P5; I=I+1
# !         WRITE (HEADER(I),1008) G2,G3,O1; I=I+1
# !!     Pineapple
# !      ELSEIF (INDEX ('PI',CROP) .GT. 0) THEN
# !         WRITE (HEADER(I), 909) P2,P3,P4; I=I+1
# !         WRITE (HEADER(I),1009) G2,G3,PHINT; I=I+1
#
# !!     ?? old cotton model??
# !      ELSEIF (INDEX ('CO',CROP) .GT. 0) THEN
# !         WRITE (HEADER(I), 912) SCPB,RESPC,SQCON; I=I+1
# !         WRITE (HEADER(I),1012) FCUT,FLAI,DDISQ
# !      ENDIF
#
#       END SELECT
#


#-----------------------------------------------------------------------
#     FORMAT Strings
#-----------------------------------------------------------------------
#   870 FORMAT (1X,'VREQ   :',F6.1,'  VBASE  :',F6.1,'  VEFF   :',F6.2,
#      &         '  PPS1   :',F6.3,'  PPS2   :',F6.1)
#   871 FORMAT (1X,'P1     :',F6.1,'  P2     :',F6.1,'  P3     :',F6.1,
#      &         '  P4     :',F6.1)
#   872 FORMAT (1X,'P5     :',F6.1,'  P6     :',F6.1,'  P7     :',F6.1,
#      &         '  P8     :',F6.1)
#   873 FORMAT (1X,'GRNOW  :',F6.1,'  GRWTX  :',F6.1,'  SHWMS  :',F6.2,
#      &         '  PHINT  :',F6.1)
#
#   880 FORMAT (1X,'PPS1   :',F6.1,'  P1L    :',F6.1,'  P2L    :',F6.1,
#      &         '  P4L    :',F6.1)
#   881 FORMAT (1X,'P5L    :',F6.0,'  SRNOW  :',F6.2,'  SRFR   :',F6.2,
#      &         '  LA1S   :',F6.1)
#   882 FORMAT (1X,'LAXS   :',F6.1,'  LAXNO  :',F6.1,'  LAFS   :',F6.1,
#      &         '  LAFNO  :',F6.1)
#   883 FORMAT (1X,'LAWS   :',F6.1,'  PHINT  :',F6.1,'  LLIFA  :',F6.1,
#      &         '  STFR   :',F6.2)
#
#   900 FORMAT (1X,'P1     :',F7.2,'  P2     :',F7.4,
#      &         '  P5     :',F7.2)
#   915 FORMAT (1X,'P1     :',F7.2,'  P2     :',F7.4,
#      &         '  P5     :',F7.2,'  AX     :',F6.1)
#
#   902 FORMAT (1X,'P1     :',F5.1,'  P2O    :',F5.2,
#      &         '  P2R    :',F6.1,'  P5     :',F6.2)
#   903 FORMAT (1X,'P1     :',F6.2,'  P2O    :',F6.3,
#      &         '  P2R    :',F6.1,'  P5     :',F6.2)
#
#   904 FORMAT (1X,'P1V    :',F8.3,'  P1D    :',F8.3,
#      &         '  P5     :',F8.2)
#   954 FORMAT (1X,'P1V    :',F8.3,'  P1D    :',F8.4,
#      &         '  P5     :',F8.2)      !,'  LT50H  :',F8.2)
#   905 FORMAT (1X,'G2     :',F7.2,'  G3     :',F7.4)
# !  905 FORMAT (1X,'G2     :',F7.2,'  G3     :',F7.4,'  G4     :',F7.2)
#   906 FORMAT (1X,'P1     :',F6.1,'  P2R    :',F6.1,
#      &         '  P5     :',F6.1,'  P2O    :',F6.1)
#   907 FORMAT (1X,'P1     :',F6.1,'  RATPOT :',F6.1,
#      &         '  LFMAX  :',F6.1)
#   957 FORMAT (1X,' PI1   :',F6.1,'   PI2  :',F6.1,
#      &         '   DTPI  :',F6.1,'   SMAX :',F6.1,
#      &         '   SO    :',F6.1,'   GMAX :',F6.1,
#      &         '   GO    :',F6.1)
#   908 FORMAT (1X,'P1     :',F7.2,'  P2     :',F7.4,
#      &         '  P5     :',F7.2)
#   909 FORMAT (1X,'P2     :',F6.1,'  P3     :',F6.1,
#      &         '  P4     :',F6.0)
#   911 FORMAT (1X,'P1     :',F7.1,' P3     :',F7.2,
#      &          ' P4     :',F7.1,' P5     :',F7.2)
#   912 FORMAT (1X,'SCPB   :',F7.1,' RESPC  :',F7.3,
#      &          ' SQCON  :',F7.3)
#   901 FORMAT (1X,'G2     :',F7.2,'  G3     :',F7.3,'  PHINT  :',F7.3)
#   916 FORMAT (1X,'G2     :',F7.2,'  G3     :',F7.3,
#      &         '  PHINT  :',F7.3,'  LX     :',F6.1)
#  1002 FORMAT (1X,'G1     :',F5.1,'  G2     :',F5.2,'  PHINT  :',F6.2,
#      &           '  P3     :',F6.1,'  P4    :',F6.1)
#  1003 FORMAT (1X,'G1     :',F6.2,'  G4     :',F6.2,'  PHINT  :',F6.2)
#  1004 FORMAT (1X,'G1     :',F8.3,'  G2     :',F8.3,'  G3     :',F8.3,
#      &         '  PHINT  :',F8.3)
#  1054 FORMAT (1X,'G1     :',F8.3,'  G2     :',F8.3,'  G3     :',F8.3,
#      &         '  PHINT  :',F8.3)
#  1005 FORMAT (1X,'PD     :',F7.2,'  P2     :',F7.3,'  TC     :',F7.3)
#  1006 FORMAT (1X,'G1     :',F6.1,'  G2     :',F6.4,
#      &         '  G3     :',F6.2,'  THOT   :',F6.1,'  TCLDP  :',F6.1,
#      &         '  TCLDF  :',F6.1)
#  1007 FORMAT (1X,'G1     :',F6.1,'  PI1    :',F6.1,
#      &         '  PI2    :',F6.1,'  DTTPI  :',F6.1)
#  1057 FORMAT (1X,' M1    :',F6.1,'   M2   :',F6.1,
#      &         '   LA    :',I6  ,'   WTPSD: ',F6.2,
#      &         '  SLAVAR:',F6.1,'   SIZLF:',F6.1)
#
#  1008 FORMAT (1X,'G2     :',F7.2,'  G3     :',F7.3,'  O1     :',F4.0)
#  1009 FORMAT (1X,'G2     :',F6.1,'  G3     :',F6.2,'  PHINT  :',F6.1)
#  1011 FORMAT (1X,'G3     :',F7.1,' G4     :',F7.1,
#      &          ' PHINT  :',F7.1,' PCINT  :',F7.1,' PCGRD  :',F7.1)
#  1012 FORMAT (1X,'FCUT   :',F7.3,' FLAI   :',F7.2,
#      &          ' DDISQ  :',F7.1)
#
#  2000 FORMAT (1X,'DUB1   :',F6.1,'  DUBR   :',F6.1,'  DESP   :',F6.2,
#      &         '  PHCX   :',F6.2,'  S#PE   :',F6.1)
#  2001 FORMAT (1X,'S#FX   :',F6.1,'  S#PX   :',F6.1,'  SWNX   :',F6.1,
#      &         '  L#IS   :',F6.2,'  L#IP   :',F6.2)
#  2002 FORMAT (1X,'LALX   :',F6.0,'  LAXA   :',F6.2,'  LAL3   :',F6.0,
#      &         '  LAWS   :',F6.0,'  LFLI   :',F6.0)
#
#  1010 FORMAT (1X,'PBASE  :',F6.2,'  PSAT   :',F6.2)
#=======================================================================

# Test driver for OPHEAD
# from ModuleDefs import ControlType, ISWITCH, Headers
#
# CONTROL = ControlType()
#
# CONTROL.RNMODE = 'B'
# CONTROL.ENAME = 'STRAWBER'
# CONTROL.FILEX = 'UFBA1701.SRX'
# CONTROL.FILEIO = 'DSSAT48.INP'
# CONTROL.DSSATP = 'C:/DSSAT48/DSSATPRO.V48'
# CONTROL.TRTNUM = 1
#
# DYNAMIC = 1
# LUNOV = 99
# CUMDEP = 0.0
# TPESW = 0.0
# VRNAME = '                '
# AINO3 = 0.0
# AINH4 = 0.0
# ECONO = '     '
# RUN = 1
# MODEL = '   '
# TITLET = 'Radiance     '
# WTHSTR = '                                             '
# RNMODE = 'B'
# UseSimCtr = False
# PATHEX = 'C:/DSSAT48/Strawberry/'
#
# OPHEAD(DYNAMIC, LUNOV, CUMDEP, TPESW, VRNAME, AINO3, AINH4, ECONO, RUN, MODEL, TITLET, WTHSTR,
#        RNMODE, CONTROL, UseSimCtr, PATHEX)
#
# print(Headers.ICOUNT,Headers.ShortCount, Headers.RUN)

# Test driver for HEADER
# DYNAMIC = 2
# LUNDES = 'ERROR.OUT' # 118
# RUN = 1
# HEADER(DYNAMIC, LUNDES, RUN)
