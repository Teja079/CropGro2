#=======================================================================
#  IPSIM, Subroutine
#       Reads parameters related to field operation from FILEX file
#-----------------------------------------------------------------------
#  INPUT
#       LUNEXP - Full file path to file-X (String)
#       LNSIM -
#       SimLevel -
#       RUN
#       LINEXP - Selected trt line number
#       CROP - Two char indicating species type (String)
#       RNMODE
#       FILEX - File-X name (String)
#       CONTROL
#       UseSimCtr
#       MODELARG -  Crop module name with version number
#       YRPLT - YYYYDOY format
#  OUTPUT
#       NYRS,NREPSQ,sw.ISWWAT,ISWNIT,ISWSYM,ISWPHO,ISWPOT,ISWDIS,MEWTH,
#       MESIC,MELI,MEEVP,MEINF,MEPHO,ISIMI,ISIM,IPLTI,IIRRI,IFERI,
#       IRESI,IHARI,IOX,IDETO,IDETS,IDETG,IDETC,IDETW,IDETN,IDETP,IDETD,
#       PWDINF,PWDINL,SWPLTL,SWPLTH,SWPLTD,PTX,PTTN,DSOILX,THETACX,
#       IEPTX,IOFFX,IAMEX,DSOILN,SOILNC,SOILNX,NEND,RIP,NRESDL,
#       DRESMG,HDLAY,HLATE
#       MESOM, METMP, MESOL, MESEV, MEGHG
#=======================================================================
from ModuleDefs import ISWITCH, SAVE_data, Put_CONTROL, Put_ISWITCH

def IPSIM(LUNEXP, LNSIM, SimLevel, RUN, LINEXP, CROP, RNMODE, FILEX, CONTROL, UseSimCtr, MODELARG, YRPLT):
    from UTILS import GET_CROPD
    from ERROR import ERROR
    from WARNING import WARNING
    from DATES import YR_DOY, Y4K_DOY
    from fortranformat import FortranRecordReader
    from READS import FIND, IGNORE, IGNORE2
    from COMSWI import SWITCH as sw, SWITCH1 as sw1
    from COMIBS import IBS01 as ibs1, IBS02 as ibs2

    LN = 0

    f55 = FortranRecordReader('I3,11X,2(1X,I5),5X,A1,1X,I5,1X,I5,1X,A25,1X,A8')
    f60 = FortranRecordReader('I3,11X,9(5X,A1)')
    f61 = FortranRecordReader('I3,11X,7(5X,A1),5X,I1,5(5X,A1)')
    f65 = FortranRecordReader('I3,11X,3(5X,A1),4X,I2,9(5X,A1),5X, A1')
    f66 = FortranRecordReader('I3,11X,2(1X,I5),5(1X,F5.0)')
    f67 = FortranRecordReader('I3,11X,3(1X,F5.0),2(1X,A5),1X,F5.0,1X,F5.0')
    f68 = FortranRecordReader('I3,11X,1X,F5.0,1X,I5,1X,F5.0')
    f69 = FortranRecordReader('I3,11X,3(1X,F5.0),2(1X,A5),1X,F5.0,1X,F5.0,1X,F5.0,1X,F6.0')
    f70 = FortranRecordReader('3X,I2')

    ERRKEY = 'IPSIM '
    FINDCH = '*SIMUL'



    MulchWarn = False

    # FO - IF SimLevel is not present set defaults
    if LNSIM == 0 or not SimLevel:
        LNSIM = 0
        NYRS = 1
        NREPSQ = 1
        ibs1.ISIMI = 'S'
        # FO - YRPLT was already read and can be updated.
        ibs2.YRSIM = YRPLT
        RSEED1 = 2150
        sw.ISWWAT = 'Y'
        sw.ISWNIT = 'Y'
        sw.ISWSYM = 'Y'
        sw.ISWPHO = 'N'
        sw.ISWPOT = 'N'
        sw.ISWDIS = 'N'
        sw.ISWCHE = 'N'
        sw.ISWTIL = 'Y'

        if 'FNQS'.find(RNMODE) >= 0:
            sw.ICO2 = 'D'  # Default CO2 from CO2???.WDA file
        else:
            sw.ICO2 = 'M'  # Measured CO2 from CO2???.WDA file

        sw.MEWTH = 'M'
        sw.MESIC = 'M'
        sw.MELI = 'E'
        sw.MEEVP = 'R'
        sw.MEINF = 'S'
        sw.MEPHO = 'L'
        sw.MEHYD = 'R'
        sw1.NSWITCH = 1
        sw.MESOM = 'G'
        sw1.MESOL = '2'  # was '1'
        sw1.MESEV = 'R'  # old Ritchie two-stage method
        sw.METMP = 'D'  # DSSAT original soil temperature
        # METMP = 'E'  # EPIC soil temp routine.
        sw.MEGHG = '0'
        # 0  => DSSAT original denitrification routine
        # 1  => DayCent N2O calculation

        sw.IPLTI = 'R'
        sw.IIRRI = 'R'
        sw.IFERI = 'R'
        sw.IRESI = 'R'
        sw.IHARI = 'M'
        sw.IOX = 'N'
        FROP = 3
        sw.IDETO = 'Y'
        sw.IDETS = 'Y'
        sw.IDETG = 'Y'
        sw.IDETN = 'N'
        sw.IDETC = 'N'
        sw.IDETW = 'N'
        sw.IDETP = 'N'
        sw.IDETD = 'N'
        sw.IDETL = 'N'
        sw.IDETH = 'N'
        sw.IDETR = 'Y'
        EFFIRR = 1.00
        AVWAT = -99.
        THETAC = 75.0
        IEPT = 100.0
        DSOIL = 30.0
        DSOILN = 30.0
        AIRAMT = 10.0
        IOFF = 'GS000'
        IAME = 'IR001'
        CRMODEL = '        '
        NCODE = "-99  "
        NEND = "-99  "

    else:

        # =============================================================
        # Read first line of simulation controls - GENERAL
        LINEXP, IFIND = FIND(LUNEXP, FINDCH)
        if IFIND == 0: ERROR(ERRKEY, 1, FILEX, LINEXP)

        while True:
            LINEXP, ISECT, CHARTEST = IGNORE(LUNEXP, LINEXP)
            if ISECT == 1:
                try:
                    LN = int(CHARTEST[:3])  # Simulating read with format 55
                    if LN != LNSIM:
                        continue
                    LN, NYRS, NREPSQ, ibs1.ISIMI, ibs2.YRSIM, RRSEED1, TITSIM, CRMODEL= f55.read(CHARTEST)
                except Exception as e:
                    ERROR(ERRKEY, 1, FILEX, LINEXP)

                if 'G' in RNMODE:
                    NYRS = 1

                if RNMODE != 'Q' or (RNMODE == 'Q' and RUN == 1):
                    RSEED1 = RRSEED1
                    if RSEED1 <= 0:
                        RSEED1 = 2150

                ibs2.YRSIM = Y4K_DOY(ibs2.YRSIM, FILEX, LINEXP, ERRKEY, 8)
                # Call Error before first weather day (RANGELH(1))
                YEAR,ISIM = YR_DOY(ibs2.YRSIM)
                break
            else:
                LUNEXP.seek(LUNEXP.tell() - 1)
        # =============================================================
        # Read SECOND line of simulation control - OPTIONS
        LINEXP, ISECT, CHARTEST = IGNORE(LUNEXP, LINEXP)
        try:
            (LN, sw.ISWWAT, sw.ISWNIT, sw.ISWSYM, sw.ISWPHO, sw.ISWPOT, sw.ISWDIS, sw.ISWCHE,
             sw.ISWTIL, sw.ICO2) = f60.read(CHARTEST)
        except Exception as e:
            print(e)
            ERROR(ERRKEY, -99, FILEX, LINEXP)

        sw.ISWWAT = sw.ISWWAT.upper()
        sw.ISWNIT = sw.ISWNIT.upper()
        sw.ISWSYM = sw.ISWSYM.upper()
        sw.ISWPHO = sw.ISWPHO.upper()
        sw.ISWPOT = sw.ISWPOT.upper()
        sw.ISWDIS = sw.ISWDIS.upper()
        sw.ISWCHE = sw.ISWCHE.upper()
        sw.ISWTIL = sw.ISWTIL.upper()
        sw.ICO2 = sw.ICO2.upper()

        if CROP not in ['BN', 'SB', 'PN', 'PE', 'CH', 'PP', 'VB', 'CP', 'CB', 'FB', 'GB', 'LT', 'AL', 'BG']:
            sw.ISWSYM = 'N'  # other crops don't have a choice

        if sw.ISWCHE == ' ': ISWCHE = 'N'
        if sw.ISWTIL == ' ': ISWTIL = 'N'
        if sw.ISWWAT == 'N':
            sw.ISWNIT = 'N'
            sw.ISWPHO = 'N'

        if 'FNQS'.find(RNMODE) >= 0:
            if 'WMD'.find(sw.ICO2) < 0:
                sw.ICO2 = 'D'
        else:
            if 'WMD'.find(sw.ICO2) < 0:
                sw.ICO2 = 'M'

        # =============================================================
        # Read THIRD line of simulation control - METHODS
        LINEXP, ISECT, CHARTEST = IGNORE(LUNEXP, LINEXP)
        try:
            (LN, sw.MEWTH, sw.MESIC, sw.MELI, sw.MEEVP, sw.MEINF, sw.MEPHO, sw.MEHYD,
             sw1.NSWITCH, sw.MESOM, sw1.MESEV, sw1.MESOL, sw.METMP, sw.MEGHG) = f61.read(CHARTEST)
        except Exception as ERRNUM:
            pass  # intentionally left as is, per original code

        sw.MEWTH = sw.MEWTH.upper()
        sw.MESIC = sw.MESIC.upper()
        sw.MELI  = sw.MELI.upper()
        sw.MEEVP = sw.MEEVP.upper()
        sw.MEINF = sw.MEINF.upper()
        sw.MEPHO = sw.MEPHO.upper()
        sw.MEHYD = sw.MEHYD.upper()
        sw.MESOM = sw.MESOM.upper()
        sw1.MESEV = sw1.MESEV.upper()
        sw1.MESOL = sw1.MESOL.upper()
        sw.METMP = sw.METMP.upper()
        sw.MEGHG = sw.MEGHG.upper()

        if 'PG'.find(sw.MESOM) < 0:
            sw.MESOM = 'G'

        if 'G' in sw.MESOM and 'FQ'.find(RNMODE) >= 0 and 'N' not in sw.MEINF:
            sw.MEINF = 'N'
            if not MulchWarn:
                MSG = [
                    "Long-term simulation of surface residues may not be accurate",
                    "when using Godwin soil organic matter module.  The effects of",
                    "a surface mulch layer on runoff and evaporation will not be modeled.",
                    "Simulation Options/Methods/Infiltration = 'No mulch effects'",
                    "You may want to consider using the Parton (CENTURY) method of",
                    "modeling soil organic matter.",
                ]
                WARNING(6, ERRKEY, MSG)
                MulchWarn = True

        if '123'.find(sw1.MESOL) < 0: sw1.MESOL = '2'
        if 'ED'.find(sw.METMP) < 0: sw.METMP = 'D'
        if '01'.find(sw.MEGHG) < 0: sw.MEGHG = '0'
        if sw1.MESEV in ['r', 'R']:
            sw1.MESEV = 'R'
        elif sw1.MESEV in ['s', 'S']:
            sw1.MESEV = 'S'
        else:
            sw1.MESEV = 'R'  # Default method Ritchie

        if sw.MEEVP == 'Z' and sw.MEPHO != 'L': ERROR(ERRKEY, 3, ' ', 0)
        if sw.MEHYD == ' ': sw.MEHYD = 'R'
        if int(sw1.NSWITCH) <= 0 and sw.ISWNIT == 'Y': sw1.NSWITCH = 1

        # =============================================================
        # Read FOURTH line of simulation control - MANAGEMENT
        LINEXP, ISECT, CHARTEST = IGNORE(LUNEXP, LINEXP)
        try:
            LN, sw.IPLTI, sw.IIRRI, sw.IFERI, sw.IRESI, sw.IHARI = f60.read(CHARTEST)[:6]
        except Exception as e:
            print(e)
            ERROR(ERRKEY, -99, FILEX, LINEXP)

        sw.IPLTI = sw.IPLTI.upper()
        sw.IIRRI = sw.IIRRI.upper()
        sw.IFERI = sw.IFERI.upper()
        sw.IRESI = sw.IRESI.upper()
        sw.IHARI = sw.IHARI.upper()

        if any(c in CROP for c in 'CSPT'):
            if sw.IHARI == 'A':
                MSG = [f"Automatic harvest option is not valid for crop type: {CROP}"]
                WARNING(1, ERRKEY, MSG)
                ERROR(ERRKEY, 4, FILEX, LINEXP)

        if any(c in CROP for c in 'CS'):
            if sw.IHARI == 'M':
                MSG = [f"Harvest at maturity option is not valid for crop type: {CROP}"]
                WARNING(1, ERRKEY, MSG)
                ERROR('IPSIM', 11, FILEX, LINEXP)

        if any(c in CROP for c in 'PT'):
            if sw.IPLTI == 'A':
                MSG = [f"Automatic planting option is not valid for crop type: {CROP}"]
                WARNING(1, ERRKEY, MSG)
                ERROR(ERRKEY, 5, FILEX, LINEXP)

        # =============================================================
        # Read FIFTH line of simulation control - OUTPUTS
        LINEXP, ISECT, CHARTEST = IGNORE(LUNEXP, LINEXP)
        if 'FQ'.find(RNMODE) < 0 or RUN == 1:
            try:
                values = CHARTEST.split()
                (LN, sw.IOX, sw.IDETO, sw.IDETS, FROP, sw.IDETG, sw.IDETC, sw.IDETW, sw.IDETN, sw.IDETP,
                 sw.IDETD, sw.IDETL, sw.IDETH, sw.IDETR, FMOPT) =(
                    f65.read(CHARTEST))
            except Exception as ERRNUM:
                ERROR(ERRKEY, ERRNUM, FILEX, LINEXP)

            sw.IOX = sw.IOX.upper()
            sw.IDETO = sw.IDETO.upper()
            sw.IDETS = sw.IDETS.upper()
            sw.IDETG = sw.IDETG.upper()
            sw.IDETC = sw.IDETC.upper()
            sw.IDETW = sw.IDETW.upper()
            sw.IDETN = sw.IDETN.upper()
            sw.IDETP = sw.IDETP.upper()
            sw.IDETD = sw.IDETD.upper()

            FMOPT = FMOPT.upper()
            if 'CA'.find(FMOPT) < 0: FMOPT = 'A'
            if sw.IDETL == ' ': sw.IDETL = 'N'
            sw.IDETL = sw.IDETL.upper()
            if sw.IDETH == ' ': sw.IDETH = 'N'
            sw.IDETH = sw.IDETH.upper()
            if sw.IDETR == ' ': sw.IDETR = 'Y'
            sw.IDETR = sw.IDETR.upper()

            if sw.IDETL == '0':
                sw.IDETS = 'Y'
                sw.IDETG = 'N'
                sw.IDETC = 'N'
                sw.IDETW = 'N'
                sw.IDETN = 'N'
                sw.IDETP = 'N'
                sw.IDETD = 'N'
                sw.IDETH = 'N'
                sw.IDETR = 'N'
                sw.IDETO = 'E'
                if any(c in RNMODE for c in 'SNY'):
                    sw.IDETO = 'N'
            elif sw.IDETL in ['A', 'D']:
                sw.IDETS = 'A'
                sw.IDETO = 'Y'
                sw.IDETG = 'Y'
                sw.IDETC = 'Y'
                sw.IDETW = 'Y'
                sw.IDETN = 'Y'
                sw.IDETP = 'Y'
                sw.IDETD = 'Y'
                sw.IDETH = 'Y'
                sw.IDETR = 'Y'
                FROP = 1

            if int(FROP) <= 0:
                FROP = 10

        # =============================================================
        # Read SIXTH line of simulation control - AUTOMATIC PLANTING
        LINEXP, ISECT, CHARTEST = IGNORE(LUNEXP, LINEXP)
        if ISECT == 1:
            try:
                LN, PWDINF, PWDINL, SWPLTL, SWPLTH, SWPLTD, PTX, PTTN = f66.read(CHARTEST)
            except Exception as ERRNUM:
                ERROR(ERRKEY, ERRNUM, FILEX, LINEXP)

            if sw.IPLTI in ['A', 'F']:
                Y4K_DOY(PWDINF, FILEX, LINEXP, ERRKEY, 9)
                Y4K_DOY(PWDINL, FILEX, LINEXP, ERRKEY, 9)
            else:
                PWDINF = -99
                PWDINL = -99

            # =============================================================
            # Read SEVENTH line of simulation control - AUTOMATIC IRRIGATION:
            V_IMDEP = [-99]*21
            V_ITHRL = [-99]*21
            V_ITHRU = [-99]*21
            V_IRON = [-99]*21
            V_IRAMT = [-99]*21
            V_IREFF = [-99]*21
            V_AVWAT = [-99]*21
            V_IRONC = [-99]*21
            V_IMETH = [-99]*21
            V_IFREQ = [-99]*21

            GSIRRIG = 1  # Start Growth Stage index
            LINEXP, ISECT, CHARTEST = IGNORE(LUNEXP, LINEXP)

            while ISECT != 3:
                try:
                    values = CHARTEST.split()
                    LN, DSOIL, THETAC, IEPT, IOFF, IAME, AIRAMT, EFFIRR, AVWAT, IFREQ = f69.read(CHARTEST)
                except Exception as ERRNUM:
                    ERROR(ERRKEY, ERRNUM, FILEX, LINEXP)

                TEXT = CHARTEST[57:62].strip()
                if len(TEXT) == 0: AVWAT = -99

                TEXT = CHARTEST[63:68].strip()
                if len(TEXT) == 0: IFREQ = 0.0

                V_IMDEP[GSIRRIG] = float(DSOIL)
                V_ITHRL[GSIRRIG] = float(THETAC)
                V_ITHRU[GSIRRIG] = float(IEPT)
                try:
                    V_IRON[GSIRRIG] = int(IOFF[3:5])
                except:
                    V_IRON[GSIRRIG] = -99
                V_IRONC[GSIRRIG] = IOFF
                V_IMETH[GSIRRIG] = IAME
                V_IRAMT[GSIRRIG] = float(AIRAMT)
                V_IREFF[GSIRRIG] = float(EFFIRR)
                V_IFREQ[GSIRRIG] = round(float(IFREQ))
                V_AVWAT[GSIRRIG] = float(AVWAT)

                LINEXP, ISECT, CHARTEST = IGNORE2(LUNEXP, LINEXP)
                if ISECT != 3:
                    GSIRRIG += 1

            DSOIL = V_IMDEP[1]
            THETAC = V_ITHRL[1]
            IEPT = V_ITHRU[1]
            IOFF = V_IRONC[1]
            IAME = V_IMETH[1]
            AIRAMT = V_IRAMT[1]
            EFFIRR = V_IREFF[1]
            AVWAT = V_AVWAT[1]
            IFREQ = float(V_IFREQ[1])

            SAVE_data.MGMT.V_IMDEP = V_IMDEP
            SAVE_data.MGMT.V_ITHRL = V_ITHRL
            SAVE_data.MGMT.V_ITHRU = V_ITHRU
            SAVE_data.MGMT.V_IRONC = V_IRONC
            SAVE_data.MGMT.V_IRON = V_IRON
            SAVE_data.MGMT.V_IRAMT = V_IRAMT
            SAVE_data.MGMT.V_IREFF = V_IREFF
            SAVE_data.MGMT.V_AVWAT = V_AVWAT
            SAVE_data.MGMT.V_IFREQ = V_IFREQ
            SAVE_data.MGMT.GSIRRIG = GSIRRIG

            # =============================================================
            # Read EIGHTH line of simulation control - AUTOMATIC N APPLICTION (not currently used)
            LINEXP, ISECT, CHARTEST = IGNORE(LUNEXP, LINEXP)
            try:
                LN, DSOILN, SOILNC, SOILNX, NCODE, NEND = f67.read(CHARTEST)[:6]
            except Exception as ERRNUM:
                ERROR(ERRKEY, ERRNUM, FILEX, LINEXP)

            try:
                FTYPEN = f70.read(NCODE)[0]
            except Exception as ERRNUM:
                ERROR(ERRKEY, ERRNUM, FILEX, LINEXP)

            # =============================================================
            # Read NINTH line of simulation control - AUTOMATIC RESIDUE APPLICATION (not currently used)
            LINEXP, ISECT, CHARTEST = IGNORE(LUNEXP, LINEXP)
            try:
                LN, RIP, NRESDL, DRESMG = f68.read(CHARTEST)
            except Exception as ERRNUM:
                ERROR(ERRKEY, ERRNUM, FILEX, LINEXP)

            # =============================================================
            # Read TENTH line of simulation control - AUTOMATIC HARVEST
            LINEXP, ISECT, CHARTEST = IGNORE(LUNEXP, LINEXP)
            try:
                LN, HDLAY, HLATE, HPP, HRP = f66.read(CHARTEST)[:5]
            except Exception as ERRNUM:
                ERROR(ERRKEY, ERRNUM, FILEX, LINEXP)

            # =============================================================
            # Read ELEVENTH line of simulation control - SIMULATION DATES
            ENDAT = -99
            SeasDur = -99
            FODAT = -99
            FStartYear = -99
            FEndYear = -99
            FWFILE = "-99"

            LINEXP, ISECT, CHARTEST = IGNORE(LUNEXP, LINEXP)

            try:
                LN = int(CHARTEST[:3])
                FODAT = int(CHARTEST[28:36])
            except Exception as ERRNUM:
                FODAT = -99

            if LN != LNSIM or ISECT == 0:   #HZ: removed from if statement (ERRNUM != 0 or)
                FODAT = -99

            CONTROL.FODAT = FODAT

            if sw.IHARI in ['A', 'F']:
                Y4K_DOY(HLATE, FILEX, LINEXP, ERRKEY, 10)
            else:
                HLATE = -99

            if HRP == None or float(HPP) < 0.0: #Added checking for None
                HPP = 100.0
            if HRP == None or float(HRP) < 0.0:
                HRP = 0.0
        else:
            PWDINF = 1
            PWDINL = 366
            SWPLTL = 1.0
            SWPLTH = 100.0
            SWPLTD = 200.0
            PTX = 50.0
            PTTN = 1.0
            DSOIL = 200.0
            THETAC = 10.0
            IEPT = 100.0
            IOFF = ' '
            HPP = 100.0
            HRP = 0.0

    #REWIND(LUNEXP)

    # -----------------------------------------------------------------------
    # Select Model Name and Path -- order of priority:
    # CTRMODEL is value from control file override -- this is used
    #     over all other values if valid. (Done in Default_SimControls)
    # CRMODEL is read from FILEX.  Use this if no control file.
    # MODELARG is from command line argument list. Third priority.
    # Last, use value from DSSATPRO.v??.
    # -----------------------------------------------------------------------

    # First check model name from FILEX
    TRY_MODEL = CRMODEL

    #MODEL_NAME(CROP, sw.DSSATP, TRY_MODEL, MODEL)
    MODEL = 'CRGRO048'

    # If FILEX model name was not acceptable, then try the
    # model name read from command line. If this is not OK,
    # MODEL contains value from DSSATPRO file
    if TRY_MODEL != MODEL:
        # Fallow must be associated with CRGRO model (for now)
        if CROP == 'FA':
            Try_MODELARG = "CRGRO"
        else:
            Try_MODELARG = MODELARG
        #MODEL_NAME(CROP, sw.DSSATP, Try_MODELARG, MODEL)
        Try_MODELARG = 'CRGRO048'

    if sw.MEPHO == 'L' and MODEL[:5] != 'CRGRO' and MODEL[:5] != 'PRFRM':
        sw.MEPHO = 'C'
        MSG = [
            "Photosynthesis method (PHOTO in FILEX) has been changed",
            f'from "L" to "C" for compatibility with crop model, {MODEL[:5]}.'
        ]
        WARNING(2, "IPEXP ", MSG)

    CONTROL, ISWITCH = FILL_ISWITCH(CONTROL, FROP, MODEL, NYRS, RNMODE)

    # Planting date needed for generic start of simulation
    if sw.IPLTI == 'R':
        PLDATE = YRPLT
    elif sw.IPLTI == 'A':
        PLDATE = PWDINF

    # Check Simulation control file for control overrides
    # Default_SimControls(
    #     CONTROL, CRMODEL, sw.DSSATP, FILECTL, ISWITCH,  # Input
    #     MODELARG, PLDATE,                           # Input
    #     UseSimCtr, MODEL                            # Output
    # )

    if UseSimCtr:
        sw.IOX     = ISWITCH.FNAME
        ibs1.ISIMI   = ISWITCH.ISIMI
        sw.ISWWAT  = ISWITCH.ISWWAT
        sw.ISWNIT  = ISWITCH.ISWNIT
        sw.ISWSYM  = ISWITCH.ISWSYM
        sw.ISWPHO  = ISWITCH.ISWPHO
        sw.ISWPOT  = ISWITCH.ISWPOT
        sw.ISWDIS  = ISWITCH.ISWDIS
        sw.ISWCHE  = ISWITCH.ISWCHE
        sw.ISWTIL  = ISWITCH.ISWTIL
        sw.ICO2    = ISWITCH.ICO2
        sw.MEWTH   = ISWITCH.MEWTH
        sw.MESOM   = ISWITCH.MESOM
        sw.MELI    = ISWITCH.MELI
        sw.MEEVP   = ISWITCH.MEEVP
        sw.MEINF   = ISWITCH.MEINF
        sw.MEPHO   = ISWITCH.MEPHO
        sw.MEHYD   = ISWITCH.MEHYD
        sw1.MESEV   = ISWITCH.MESEV
        sw1.MESOL   = ISWITCH.MESOL
        sw.METMP   = ISWITCH.METMP
        sw.MEGHG   = ISWITCH.MEGHG
        sw.IPLTI   = ISWITCH.IPLTI
        sw.IIRRI   = ISWITCH.IIRRI
        sw.IFERI   = ISWITCH.IFERI
        sw.IRESI   = ISWITCH.IRESI
        sw.IHARI   = ISWITCH.IHARI
        sw.IDETO   = ISWITCH.IDETO
        sw.IDETS   = ISWITCH.IDETS
        sw.IDETG   = ISWITCH.IDETG
        sw.IDETC   = ISWITCH.IDETC
        sw.IDETW   = ISWITCH.IDETW
        sw.IDETN   = ISWITCH.IDETN
        sw.IDETP   = ISWITCH.IDETP
        sw.IDETD   = ISWITCH.IDETD
        sw.IDETL   = ISWITCH.IDETL
        sw.IDETH   = ISWITCH.IDETH
        sw.IDETR   = ISWITCH.IDETR
        sw1.NSWITCH = ISWITCH.NSWI
        FMOPT   = ISWITCH.FMOPT   # VSH

        NYRS    = CONTROL.NYRS
        ibs2.YRSIM   = CONTROL.YRSIM
        MODEL   = CONTROL.MODEL
        FROP    = CONTROL.FROP

    Put_CONTROL(CONTROL)
    Put_ISWITCH(ISWITCH)
    # --------------------------------------------------------------------
    # Check for N model compatible with crop model
    if sw.ISWNIT != 'N':
        model_prefix = MODEL[:5]
        if model_prefix in ['SALUS', 'SCCAN', 'SCCSP', 'SCSAM']:
            CROPD = GET_CROPD(CROP)
            CROPD = CROPD.lstrip()

            MSG = [
                f"Nitrogen dynamics model has not been developed for {model_prefix} {CROPD}.",
                "Please contact the CSM development team if you wish to ",
                f"contribute to development of a N model for {model_prefix} {CROPD}.",
                "N simulation will be switched off."
            ]
            WARNING(4, ERRKEY, MSG)
            sw.ISWNIT = 'N'
            sw.ISWPHO = 'N'
            sw.ISWPOT = 'N'
            # ERROR('IPSIM', 6, "", 0)

    # --------------------------------------------------------------------
    # Check for phosphorus model compatible with crop model
    if sw.ISWPHO != 'N':
        model_prefix = MODEL[:5]
        if model_prefix in ['CRGRO', 'MZCER', 'SGCER']:
            crop_code = CONTROL.CROP
            if crop_code not in ['SB', 'FA', 'MZ', 'PN', 'SG']:
                CROPD = GET_CROPD(CROP)
                CROPD = CROPD.lstrip()

                MSG = [
                    f"Phosphorus model has not been tested for {model_prefix} {CROPD}.",
                    "Please contact the CSM development team if you wish to ",
                    f"contribute to development of a P model for {model_prefix} {CROPD}."
                ]
                WARNING(3, ERRKEY, MSG)
                ERROR('IPSIM', 6, "", 0)
        else:
            MSG = [
                f"Phosphorus model has not been enabled for {model_prefix} model.",
                "Please contact the CSM development team if you wish to contribute to ",
                f"development of a P model for {model_prefix}."
            ]
            WARNING(3, ERRKEY, MSG)
            ERROR('IPSIM', 6, "", 0)

    # --------------------------------------------------------------------
    # Check for potassium model compatible with crop model
    if sw.ISWPOT != 'N':
        model_prefix = MODEL[:5]
        if model_prefix in ['MZCER', 'RICER']:
            CROPD = GET_CROPD(CROP)
            CROPD = CROPD.lstrip()

            MSG = [
                f"Potassium model has not been tested for {model_prefix} {CROPD}.",
                "Please contact the CSM development team if you wish to ",
                f"contribute to development of a K model for {model_prefix} {CROPD}."
            ]
            WARNING(3, ERRKEY, MSG)
            ERROR('IPSIM', 7, "", 0)
        else:
            MSG = [
                f"Potassium model has not been enabled for {model_prefix} model.",
                "Please contact the CSM development team if you wish to contribute to ",
                f"development of a K model for {model_prefix}."
            ]
            WARNING(3, ERRKEY, MSG)
            ERROR('IPSIM', 7, "", 0)

    return (TITSIM, NYRS, NREPSQ, ibs1.ISIMI, PWDINF, PWDINL, SWPLTL, NCODE, SWPLTH, SWPLTD, YEAR, PTX, PTTN,
            DSOIL, THETAC, IEPT, IOFF, IAME, DSOILN, SOILNC, ibs2.YRSIM, SOILNX, NEND, RIP, NRESDL, DRESMG, HDLAY,
            HLATE, HPP, HRP, FTYPEN, RSEED1, AIRAMT, EFFIRR, FROP, MODEL, CONTROL, ISWITCH)


# !=======================================================================
# !  FILL_ISWITCH, Subroutine
# !
# !  Copies values to ISWITCH variable, determines what values are carried
# !     over in sequence runs.
# !-----------------------------------------------------------------------
# !  Called : IPSIM
# !  Calls  : none
# !=======================================================================
def FILL_ISWITCH(CONTROL, FROP, MODEL, NYRS, RNMODE):
    # Skip some variables for sequenced runs -- need to keep values
    # from first run
    from COMSWI import SWITCH as sw, SWITCH1 as sw1
    from COMIBS import  IBS01 as ibs1, IBS02 as ibs2
    if ('F' not in RNMODE and 'Q' not in RNMODE) or CONTROL.RUN == 1:
        ISWITCH.ISWWAT = sw.ISWWAT  # water simulation
        ISWITCH.ISWNIT = sw.ISWNIT  # N simulation
        ISWITCH.ISWPHO = sw.ISWPHO  # P simulation
        ISWITCH.ISWPOT = sw.ISWPOT  # K simulation
        ISWITCH.ISIMI  = ibs1.ISIMI   # start of simulation switch
        ISWITCH.ICO2   = sw.ICO2    # atmospheric CO2 data source
        ISWITCH.MEWTH  = sw.MEWTH   # weather data source
        ISWITCH.MESOM  = sw.MESOM   # SOM method
        ISWITCH.MEINF  = sw.MEINF   # infiltration method (mulch effects)
        ISWITCH.MEHYD  = sw.MEHYD   # hydrology
        ISWITCH.MESEV  = sw1.MESEV   # soil evaporation
        ISWITCH.MESOL  = sw1.MESOL   # soil layer distribution
        ISWITCH.METMP  = sw.METMP   # soil temperature method
        ISWITCH.MEGHG  = sw.MEGHG   # greenhouse gas calculations
        ISWITCH.IDETO  = sw.IDETO   # overview file
        ISWITCH.IDETS  = sw.IDETS   # summary file
        ISWITCH.IDETG  = sw.IDETG   # growth output files
        ISWITCH.IDETC  = sw.IDETC   # carbon output
        ISWITCH.IDETW  = sw.IDETW   # water output
        ISWITCH.IDETN  = sw.IDETN   # N output
        ISWITCH.IDETP  = sw.IDETP   # P output
        ISWITCH.IDETD  = sw.IDETD   # disease and pest output
        ISWITCH.IDETL  = sw.IDETL   # detail output (verbosity)
        ISWITCH.IDETH  = sw.IDETH   # chemical output
        ISWITCH.IDETR  = sw.IDETR   # management operations output
        ISWITCH.NSWI   = sw1.NSWITCH # N computations switch
        CONTROL.NYRS   = NYRS    # number of years simulated
        CONTROL.FROP   = FROP    # frequency of output
        ISWITCH.MEEVP  = sw.MEEVP   # potential ET method
        ISWITCH.FNAME  = sw.IOX     # output file name
        #ISWITCH.FMOPT  = FMOPT     From CSVoutput module?

    # Use these values for all runs
    ISWITCH.ISWDIS = sw.ISWDIS    # pests and disease
    ISWITCH.ISWCHE = sw.ISWCHE    # chemical application
    ISWITCH.ISWTIL = sw.ISWTIL    # tillage
    ISWITCH.MELI   = sw.MELI      # light interception
    ISWITCH.IPLTI  = sw.IPLTI     # planting switch
    ISWITCH.IIRRI  = sw.IIRRI     # irrigation switch
    ISWITCH.IFERI  = sw.IFERI     # fertilizer switch
    ISWITCH.IRESI  = sw.IRESI     # residue addition switch
    ISWITCH.IHARI  = sw.IHARI     # harvest switch

    CONTROL.YRSIM  = ibs2.YRSIM     # from csv output module?
    CONTROL.MODEL  = MODEL     # crop growth model

    ISWITCH.ISWSYM = sw.ISWSYM    # symbiosis (N-fixation)
    ISWITCH.MEPHO  = sw.MEPHO     # photosynthesis method

    Put_ISWITCH(ISWITCH)
    Put_CONTROL(CONTROL)

    return CONTROL, ISWITCH

# LUNEXP = "C:/DSSAT48/Strawberry/UFBA1701.SRX"
# LNSIM = 1
# SimLevel = True
# RUN = 1
# LINEXP = 17
# CROP = 'SR'
# RNMODE = 'B'
# FILEX = "UFBA1701.SRX"
# MODELARG = 'CRGRO048'
# UseSimCtr = False
# YRPLT = 2017282
#
# print(IPSIM(LUNEXP, LNSIM, SimLevel, RUN, LINEXP, CROP, RNMODE, FILEX, CONTROL, UseSimCtr, MODELARG, YRPLT))