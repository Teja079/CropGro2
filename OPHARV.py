# C=======================================================================
# C  OPHARV, Subroutine G. Hoogenboom, J. W. Jones
# C  Generates output for seasonal data.
# C=======================================================================
from ModuleDefs import *
import numpy as np
def OPHARV(CONTROL, ISWITCH,
          AGEFAC, CANHT, CANNAA, CANWAA, CROP,            #Input
          HARVFRAC, LAIMX, MDATE, NSTRES, PCLSD, PCNSD,   #Input
          PODNO, PODWT, PStres1, PStres2, SDRATE, SDWT,   #Input
          SEEDNO, STGDOY, SWFAC, TOPWT, TURFAC,           #Input
          VSTAGE, WTNCAN, WTNFX, WTNSD, WTNST, WTNUP,     #Input
          XLAI, RSTAGE, YREMRG, YRNR1, YRNR3, YRNR5,      #Input
          YRNR7, YRPLT,                                   #Input
          SDWTAH):                                        #Output

        RNMODE,IDETO,IPLTI, PLME = " "*4
        CROP =  '  '
        SECTION = ' '*6
        ERRKEY = 'OPHARV'
        STNAME = np.empty(20, dtype='s10')
        FILEA = ' '*12
        FILEIO = ' '*30
        PATHEX = ' '*80

        ACOUNT, DFLR, DEMRG, DFPD, DFSD, DHRV = [0]*6
        DNR8,DMAT,DNR0, DNR1,DNR3,DNR5,DNR7 = [0]*7
        DYNAMIC, ERRNUM, FOUND = [0]*3
        IFLR, IEMRG, IFPD, IFSD, IHRV, IMAT, ISENS = [0]*7
        LINC, LNUM, LUNIO, RUN, TIMDIF, TRTNUM, YIELD, YREMRG = [0]*8
        YRNR1,YRNR3,YRNR5,YRNR7,MDATE,YRDOY, YRPLT,YRSIM = [0]*8
        RSTAGE = 0
        TRT_ROT = 0
        STGDOY = np.zeros(20, dtype=int)

        BIOMAS, BWAH, CANHT, CANNAA, CANWAA, HI, HWAH, HWAM = [0.0]*8
        LAIMX, PCLSD, PCNSD, PODWT, PODNO, PSDWT, PSPP = [0.0]*7
        SDRATE, SDWT, SDWTAH, SEEDNO = [0.0]*4
        THRES, TOPWT, VSTAGE = [0.0]*3
        WTNCAN, WTNFX, WTNSD, WTNST, WTNUP, XLAI = [0.0]*6
        HARVFRAC = np.zeros(2)


        SUMNUM = 18
        LABEL = '    '
        VALUE = np.zeros(SUMNUM)

        OLAB, OLAP = (np.empty(EvaluateNum, dtype='s6'), np.empty(EvaluateNum, dtype='s6'))
        X = np.empty(EvaluateNum, dtype='s6')
        Simulated, Measured= np.empty(EvaluateNum, dtype='s8'),np.empty(EvaluateNum, dtype='s8')
        DESCRIP = np.empty(EvaluateNum, dtype='s50')

    #   P module
        PStres1, PStres2 = [0.0]*2

    #   Variables added for environmental and stress factors output
    #   AGEFAC, NSTRES, SWFAC, TURFAC = [0.0]*4
        PlantStres = PlStresType()

        CONTROL = ControlType()
        ISWITCH = SwitchType()

        DYNAMIC= CONTROL.DYNAMIC
        FILEIO = CONTROL.FILEIO
        LUNIO  = CONTROL.LUNIO
        RUN    = CONTROL.RUN
        RNMODE = CONTROL.RNMODE
        YRDOY  = CONTROL.YRDOY
        YRSIM  = CONTROL.YRSIM

        IDETO = ISWITCH.IDETO
        IPLTI = ISWITCH.IPLTI

        ACOUNT = 25  #Number of possible FILEA headers for this crop

        OLAB = [ #Pred.                 Obs.   Definition
                                #------------          -----  -----------
                                #new!old!
                'EDAT  ',                     # 1 !25 EDAT                  Emergence date
                'ADAT  ',                     # 2 ! 1 DNR1           DFLR   Anthesis date
                'PD1T  ',                     # 3 ! 2 DNR3           DFPD   First Pod
                'PDFT  ',                     # 4 ! 3 DNR5           DFSD   First Seed
                'MDAT  ',                     # 5 ! 4 DNR7           DMAT   Physiological Maturity
                'R8AT  ',                     # 6 !24 DNR8           DHRV   Harvest Maturity (dap)
                'HWAM  ',                     # 7 ! 5 NINT(SDWT*10)  XGWT   Seed Yield (kg/ha;dry)
                'PWAM  ',                     # 8 ! 6 NINT(PODWT*10) XPDW   Pod Yield (kg/ha;dry)
                'CWAA  ',                     # 9 !19 NINT(CANWAA*10)XCWAA  Biomass (kg/ha) at Anth
                'CWAM  ',                     #10 !10 NINT(TOPWT*10) XCWT   Biomass (kg/ha) Harv Mat
                'BWAM  ',                     #11 !11 (TOPWT-PODWT)*10 XSWT Tops - seed (kg/ha) @Mat
                'H#AM  ',                     #12 ! 7 NINT(SEEDNO)   XNOGR  Seed Number (Seed/m2)
                'HWUM  ',                     #13 ! 8 PSDWT          XGWU   Weight Per Seed (g;dry)
                'H#UM  ',                     #14 ! 9 PSPP           XNOGU  Seeds/Pod
                'HIAM  ',                     #15 !13 HI             XHIN   Harvest Index (kg/kg)
                'THAM  ',                     #16 !14 THRES          XTHR   Shelling Percentage (%)
                'LAIX  ',                     #17 !12 LAIMX          XLAM   Maximum LAI (m2/m2)
                'L#SM  ',                     #18 !21 VSTAGE         XLFNO  Final Leaf # Main Stem
                'CHTA  ',                     #19 !23 CANHT          XCNHT  Canopy Height (m)
                'CNAA  ',                     #20 !20                XCNAA  Biomass N @ anth (kg/ha)
                'CNAM  ',                     #21 !16 NINT(WTNCAN*10)XNTP   Biomass N (kg N/ha)
                'SNAM  ',                     #22 !17 NINT(WTNST*10) XNST   Stalk N (kg N/ha)
                'GNAM  ',                     #23 !15 NINT(WTNSD*10) XNGR   Seed N (kg N/ha)
                'GN%M  ',                     #24 !18 PCNSD          XNPS   Seed N (%)
                'GL%M  ',                     #25 !22 PCLSD          XLPS   Seed Lipid (%)
                15*'      ']

        # !***********************************************************************
        # !***********************************************************************
        # !     RUN INITIALIZATION
        # !***********************************************************************
        if DYNAMIC == RUNINIT:
                try:
                        with open(FILEIO, "r") as LUNIO:
                                lines = LUNIO.readlines()
                except FileNotFoundError:
                        ERROR(ERRKEY, 0, FILEIO, 0)

                try:
                        ISENS = int(lines[0][55:60].strip())
                except ValueError:
                        ERROR(ERRKEY, 0, FILEIO, LNUM)

                try:
                        FILEA = lines[4][15:27].strip()
                        PATHEX = lines[4][28:108].strip()
                        LNUM += 4
                except IndexError:
                        ERROR(ERRKEY, 0, FILEIO, 0)

                SECTION = "*TREAT"
                LINC, FOUND = FIND(LUNIO, SECTION)
                LNUM += LINC

                if FOUND == 0:
                        ERROR(SECTION, 42, FILEIO, LNUM)
                else:
                        try:
                                TRTNUM = int(lines[LNUM].strip())
                                LNUM += 1
                        except ValueError:
                                ERROR(ERRKEY, 0, FILEIO, 0)

                # Find and Read Planting Details Section
                SECTION = "*PLANT"
                LINC, FOUND = FIND(LUNIO, SECTION)
                LNUM += LINC

                if FOUND == 0:
                        ERROR(SECTION, 42, FILEIO, LNUM)

                try:
                        PLME = lines[LNUM][35:36].strip()
                        LNUM += 1
                except IndexError:
                        ERROR(ERRKEY, 0, FILEIO, 0)

                STNAMES(CROP, PLME, STNAME)

                GETDESC(ACOUNT, OLAB, DESCRIP)
                OLAP = OLAB

                BIOMAS = -99.0
                VSTAGE = -99.0
                WTNCAN = -99.0
                XLAI = -99.0
                YIELD = -99.0
                RSTAGE = -99

                OPVIEW(CONTROL, BIOMAS, ACOUNT, DESCRIP, IDETO, VSTAGE,
                       Measured, PlantStres, Simulated, STGDOY,
                       STNAME, WTNCAN, XLAI, YIELD, YRPLT, RSTAGE)

        # C***********************************************************************
        # C     SEASONAL INITIALIZATION
        # C***********************************************************************
        elif DYNAMIC == SEASINIT:
                Simulated = " "
                Measured = " "
                YIELD = 0
                BIOMAS = 0.0

                PlantStres.ACTIVE = False
                PlantStres.StageName = "                       "

                if CROP in ['BG', 'BN', 'CH', 'CI', 'CN', 'CO', 'CP', 'CU', 'FB', 'GB',
                            'LT', 'PE', 'PN', 'PP', 'PR', 'QU', 'SB', 'SF', 'SU',
                            'TM', 'VB']:
                        PlantStres.NSTAGES = 4
                        PlantStres.StageName[1:4] = [
                                "Emergence -First Flower",
                                "First Flower-First Seed",
                                "First Seed - Phys. Mat.",
                                "Emergence  - Phys. Mat."
                        ]
                elif CROP == 'CB':
                        PlantStres.NSTAGES = 0
                elif CROP in ['BM', 'BH', 'BR', 'C3', 'C4', 'NP']:
                        PlantStres.NSTAGES = 1
                        PlantStres.StageName[1] = "Emergence  - Phys. Mat."
                else:
                        PlantStres.NSTAGES = 0

                PlantStres.StageName.insert(1, "Planting to Harvest")

                OPVIEW(CONTROL, BIOMAS, ACOUNT, DESCRIP, IDETO, VSTAGE,
                       Measured, PlantStres, Simulated, STGDOY,
                       STNAME, WTNCAN, XLAI, YIELD, YRPLT, RSTAGE)

        # !***********************************************************************
        # !***********************************************************************
        # !     DAILY OUTPUT
        # !***********************************************************************
        elif DYNAMIC == OUTPUT:
                BIOMAS = TOPWT * 10.0

                PlantStres.W_grow = TURFAC
                PlantStres.W_phot = SWFAC
                PlantStres.N_grow = NSTRES
                PlantStres.N_phot = AGEFAC
                PlantStres.P_grow = PSTRES2
                PlantStres.P_phot = PSTRES1
                PlantStres.ACTIVE = False

                # Set ACTIVE variable to indicate that current phase is active
                if CROP in ['BG', 'BN', 'CH', 'CI', 'CN', 'CO', 'CP', 'CU', 'FB', 'GB',
                            'LT', 'PE', 'PN', 'PP', 'PR', 'QU', 'SB', 'SF', 'SU',
                            'TM', 'VB']:
                        if STGDOY[1] < YRDOY <= STGDOY[5]:
                                PlantStres.ACTIVE[1] = True
                        if STGDOY[5] < YRDOY <= STGDOY[8]:
                                PlantStres.ACTIVE[2] = True
                        if STGDOY[8] < YRDOY <= STGDOY[10]:
                                PlantStres.ACTIVE[3] = True
                        if STGDOY[1] < YRDOY <= STGDOY[10]:
                                PlantStres.ACTIVE[4] = True

                elif CROP == 'CB':
                        if STGDOY[15] < YRDOY <= STGDOY[16]:
                                PlantStres.ACTIVE[1] = True

                elif CROP in ['BM', 'BH', 'BR', 'NP']:
                        if STGDOY[1] < YRDOY <= STGDOY[16]:
                                PlantStres.ACTIVE[1] = True

                if STGDOY[15] < YRDOY <= STGDOY[16] and STGDOY[15] > -99:
                        PlantStres.ACTIVE[0] = True

                # Send data to Overview.out on days where stages occur
                OPVIEW(CONTROL, BIOMAS, ACOUNT, DESCRIP, IDETO, VSTAGE,
                       Measured, PlantStres, Simulated, STGDOY,
                       STNAME, WTNCAN, XLAI, YIELD, YRPLT, RSTAGE)

        # !***********************************************************************
        # !***********************************************************************
        # !     Seasonal Output
        # !***********************************************************************
        if DYNAMIC == SEASEND:

                if SEEDNO > 1.0e-4:
                        PSDWT = SDWT / SEEDNO
                else:
                        PSDWT = 0.0

                if PODNO > 1.0e-4:
                        PSPP = SEEDNO / PODNO
                else:
                        PSPP = 0.0

                if PODWT > 0.1:
                        THRES = (SDWT * 100.0) / PODWT
                else:
                        THRES = 0.0

                THRES = min(THRES, 99.99)

                if TOPWT > 1.0e-4 and SDWT >= 1.0e-4:
                        HI = SDWT / TOPWT
                else:
                        HI = 0.0

                if CROP == "FA":
                        YRPLT = -99

        #!-----------------------------------------------------------------------
        #!     Read Measured (measured) data from FILEA
        #!-----------------------------------------------------------------------
        if (
                ("YE" in IDETO or any(char in "IAEBCGDT" for char in RNMODE))
                or ("AY" in ISWITCH.IDETS and CROP != "FA")
        ):
                if "FQ" in RNMODE:
                        TRT_ROT = CONTROL.ROTNUM
                else:
                        TRT_ROT = TRTNUM

                READA(FILEA, PATHEX, OLAB, TRT_ROT, YRSIM, X)

                READA_Dates(X[2], YRSIM, IFLR)
                if IFLR > 0 and IPLTI == "R" and ISENS == 0:
                        DFLR = TIMDIF(YRPLT, IFLR)
                else:
                        DFLR = -99
                OLAP[2] = "ADAP  "
                GetDesc(1, OLAP[2], DESCRIP[2])

                READA_Dates(X[3], YRSIM, IFPD)
                if IFPD > 0 and IPLTI == "R" and ISENS == 0:
                        DFPD = TIMDIF(YRPLT, IFPD)
                else:
                        DFPD = -99
                OLAP[3] = "PD1P  "
                GetDesc(1, OLAP[3], DESCRIP[3])

                READA_Dates(X[4], YRSIM, IFSD)
                if IFSD > 0 and IPLTI == "R" and ISENS == 0:
                        DFSD = TIMDIF(YRPLT, IFSD)
                else:
                        DFSD = -99
                OLAP[4] = "PDFP  "
                GetDesc(1, OLAP[4], DESCRIP[4])

                READA_Dates(X[5], YRSIM, IMAT)
                if IMAT > 0 and IPLTI == "R" and ISENS == 0:
                        DMAT = TIMDIF(YRPLT, IMAT)
                else:
                        DMAT = -99
                OLAP[5] = "MDAP  "
                GetDesc(1, OLAP[5], DESCRIP[5])

                READA_Dates(X[6], YRSIM, IHRV)
                if IHRV > 0 and IPLTI == "R" and ISENS == 0:
                        DHRV = TIMDIF(YRPLT, IHRV)
                else:
                        DHRV = -99
                OLAP[6] = "R8AP  "
                GetDesc(1, OLAP[6], DESCRIP[6])

                READA_Dates(X[1], YRSIM, IEMRG)
                if IEMRG > 0 and IPLTI == "R" and ISENS == 0:
                        DEMRG = TIMDIF(YRPLT, IEMRG)
                else:
                        DEMRG = -99
                OLAP[1] = "EDAP  "
                GetDesc(1, OLAP[1], DESCRIP[1])

                DNR1 = TIMDIF(YRPLT, YRNR1)
                if DNR1 <= 0:
                        DNR1 = -99
                        YRNR1 = -99

                DNR3 = TIMDIF(YRPLT, YRNR3)
                if DNR3 <= 0:
                        DNR3 = -99
                        YRNR3 = -99

                DNR5 = TIMDIF(YRPLT, YRNR5)
                if DNR5 <= 0:
                        DNR5 = -99
                        YRNR5 = -99

                DNR7 = TIMDIF(YRPLT, YRNR7)
                if DNR7 <= 0:
                        DNR7 = -99
                        YRNR7 = -99

                DNR8 = TIMDIF(YRPLT, MDATE)
                if DNR8 <= 0 or YRPLT <= 0:
                        DNR8 = -99
                        MDATE = -99

                DNR0 = TIMDIF(YRPLT, YREMRG)
                if DNR0 <= 0 or YRPLT <= 0:
                        DNR0 = -99
                        YREMRG = -99


        Simulated[1] = f"{DNR0:8d}"
        Measured[1] = f"{DEMRG:8d}"

        Simulated[2] = f"{DNR1:8d}"
        Measured[2] = f"{DFLR:8d}"

        Simulated[3] = f"{DNR3:8d}"
        Measured[3] = f"{DFPD:8d}"

        Simulated[4] = f"{DNR5:8d}"
        Measured[4] = f"{DFSD:8d}"

        Simulated[5] = f"{DNR7:8d}"
        Measured[5] = f"{DMAT:8d}"

        Simulated[6] = f"{DNR8:8d}"
        Measured[6] = f"{DHRV:8d}"

        Simulated[7] = f"{round(SDWT * 10):8d}"
        Measured[7] = f"{X[7]:8s}"

        Simulated[8] = f"{round(PODWT * 10):8d}"
        Measured[8] = f"{X[8]:8s}"

        Simulated[9] = f"{round(CANWAA * 10):8d}"
        Measured[9] = f"{X[9]:8s}"

        Simulated[10] = f"{round(TOPWT * 10):8d}"
        Measured[10] = f"{X[10]:8s}"

        Simulated[11] = f"{round((TOPWT - SDWT) * 10):8d}"
        Measured[11] = f"{X[11]:8s}"

        Simulated[12] = f"{round(SEEDNO):8d}"
        Measured[12] = f"{X[12]:8s}"

        Simulated[13] = f"{PSDWT:8.4f}"
        Measured[13] = f"{X[13]:8s}"

        Simulated[14] = f"{PSPP:8.2f}"
        Measured[14] = f"{X[14]:8s}"

        Simulated[15] = f"{HI:8.3f}"
        Measured[15] = f"{X[15]:8s}"

        Simulated[16] = f"{THRES:8.2f}"
        Measured[16] = f"{X[16]:8s}"

        Simulated[17] = f"{LAIMX:8.2f}"
        Measured[17] = f"{X[17]:8s}"

        Simulated[18] = f"{VSTAGE:8.2f}"
        Measured[18] = f"{X[18]:8s}"

        Simulated[19] = f"{CANHT:8.2f}"
        Measured[19] = f"{X[19]:8s}"

        Simulated[20] = f"{round(CANNAA * 10):8d}"
        Measured[20] = f"{X[20]:8s}"

        Simulated[21] = f"{round(WTNCAN * 10):8d}"
        Measured[21] = f"{X[21]:8s}"

        Simulated[22] = f"{round(WTNST * 10):8d}"
        Measured[22] = f"{X[22]:8s}"

        Simulated[23] = f"{round(WTNSD * 10):8d}"
        Measured[23] = f"{X[23]:8s}"

        Simulated[24] = f"{PCNSD:8.2f}"
        Measured[24] = f"{X[24]:8s}"

        Simulated[25] = f"{PCLSD:8.2f}"
        Measured[25] = f"{X[25]:8s}"

        if CONTROL.ERRCODE > 0:
                Simulated = ["     -99"] * len(Simulated)
        SDWTAH = SDWT * HARVFRAC[1]

        BWAH = (TOPWT - PODWT) * HARVFRAC[2]

        PSDWT = SDWT / SEEDNO if SEEDNO > 1.0e-4 else 0.0
        PSPP = SEEDNO / PODNO if PODNO > 1.0e-4 else 0.0

        if CROP in ('TM', 'PR', 'CU'):
                HWAM = PODWT * 10.0
                HWAH = PODWT * 10.0
        else:
                HWAM = SDWT * 10.0
                HWAH = SDWTAH * 10.0

        LABEL = {}
        VALUE = {}

        LABEL[1], VALUE[1] = 'ADAT', float(YRNR1)
        LABEL[2], VALUE[2] = 'MDAT', float(YRNR7)
        LABEL[3], VALUE[3] = 'DWAP', SDRATE
        LABEL[4], VALUE[4] = 'CWAM', TOPWT * 10.0
        LABEL[5], VALUE[5] = 'HWAM', HWAM
        LABEL[6], VALUE[6] = 'HWAH', HWAH
        LABEL[7], VALUE[7] = 'BWAH', BWAH
        LABEL[8], VALUE[8] = 'HWUM', PSDWT
        LABEL[9], VALUE[9] = 'H#AM', SEEDNO
        LABEL[10], VALUE[10] = 'H#UM', PSPP
        LABEL[11], VALUE[11] = 'NFXM', WTNFX * 10.0
        LABEL[12], VALUE[12] = 'NUCM', WTNUP * 10.0
        LABEL[13], VALUE[13] = 'CNAM', WTNCAN * 10.0
        LABEL[14], VALUE[14] = 'GNAM', WTNSD * 10.0
        LABEL[15], VALUE[15] = 'PWAM', PODWT * 10.0
        LABEL[16], VALUE[16] = 'LAIX', LAIMX
        LABEL[17], VALUE[17] = 'HIAM', HI
        LABEL[18], VALUE[18] = 'EDAT', float(YREMRG)

        SUMVALS(SUMNUM, LABEL, VALUE)

        BIOMAS = TOPWT * 10.0
        YIELD = round(HWAH)

        OPVIEW(CONTROL, BIOMAS, ACOUNT, DESCRIP, IDETO, VSTAGE,
               Measured, PlantStres, Simulated, STGDOY,
               STNAME, WTNCAN, XLAI, YIELD, YRPLT, RSTAGE)

        if ('YE' in IDETO) or any(char in 'IAEBCGD' for char in RNMODE):
                EvaluateDat(ACOUNT, Measured, Simulated, DESCRIP, OLAP)

# C=======================================================================
# C  STNAMES, Subroutine C.H.Porter
# C  Assigns STNAME for various CROPGRO crops.
# C=======================================================================
def STNAMES(CROP, PLME, STNAME):
    for i in range(20):
        STNAME[i] = "          "

    if CROP in ('BN', 'CH', 'CI', 'CN', 'CP', 'CU', 'FB', 'GB', 'PE',
                'PP', 'PR', 'SB', 'TM', 'VB', 'LT'):
        STNAME[0] = 'Emergence '
        STNAME[1] = 'Unifoliate'
        STNAME[2] = 'End Juven.'
        STNAME[3] = 'Flower Ind'
        STNAME[4] = 'First Flwr'
        STNAME[5] = 'First Pod '
        STNAME[6] = 'First Pod '
        STNAME[7] = 'First Seed'
        STNAME[8] = 'End Pod   '
        STNAME[9] = 'Phys. Mat '
        STNAME[10] = 'Harv. Mat '
        STNAME[11] = 'End Msnode'
        STNAME[12] = 'End Leaf  '
        STNAME[13] = 'Start Sim '
        STNAME[14] = 'Sowing    '
        STNAME[15] = 'Harvest   '

    elif CROP in ('SU', 'SF'):
        STNAME[0] = 'Emergence '
        STNAME[1] = 'Unifoliate'
        STNAME[2] = 'End Juven.'
        STNAME[3] = 'Flower Ind'
        STNAME[4] = 'Star burst'
        STNAME[5] = '1st thalam'
        STNAME[6] = '1st thalam'
        STNAME[7] = '1st Flw/Sd'
        STNAME[8] = 'End Sd-add'
        STNAME[9] = 'Phys. Mat '
        STNAME[10] = 'Harv. Mat '
        STNAME[11] = 'End Msnode'
        STNAME[12] = 'End Leaf  '
        STNAME[13] = 'Start Sim '
        STNAME[14] = 'Sowing    '
        STNAME[15] = 'Harvest   '

    elif CROP == 'QU':
        STNAME[0] = 'Emergence '
        STNAME[1] = 'Unifoliate'
        STNAME[2] = 'End Juven.'
        STNAME[3] = 'Flower Ind'
        STNAME[4] = 'Repr. bud'
        STNAME[5] = '1st inflor'
        STNAME[6] = '1st inflor'
        STNAME[7] = '1st Flw/Sd'
        STNAME[8] = 'End Sd-add'
        STNAME[9] = 'Phys. Mat '
        STNAME[10] = 'Harv. Mat '
        STNAME[11] = 'End Msnode'
        STNAME[12] = 'End Leaf  '
        STNAME[13] = 'Start Sim '
        STNAME[14] = 'Sowing    '
        STNAME[15] = 'Harvest   '

    elif CROP in ('BG', 'PN'):
        STNAME[0] = 'Emergence '
        STNAME[1] = 'Unifoliate'
        STNAME[2] = 'End Juven.'
        STNAME[3] = 'Flower Ind'
        STNAME[4] = 'First Flwr'
        STNAME[5] = 'First Peg '
        STNAME[6] = 'First Pod '
        STNAME[7] = 'First Seed'
        STNAME[8] = 'End Pod   '
        STNAME[9] = 'Phys. Mat '
        STNAME[10] = 'Harv. Mat '
        STNAME[11] = 'End Msnode'
        STNAME[12] = 'End Leaf  '
        STNAME[13] = 'Start Sim '
        STNAME[14] = 'Sowing    '
        STNAME[15] = 'Harvest   '

    elif CROP == 'CB':
        STNAME[0] = 'Emergence '
        STNAME[1] = 'Unifoliate'
        STNAME[2] = 'End Juven.'
        STNAME[5] = 'First Head'
        STNAME[6] = 'Full Head '
        STNAME[9] = 'Phys. Mat '
        STNAME[10] = 'Harv. Mat '
        STNAME[11] = 'End Msnode'
        STNAME[12] = 'End Leaf  '
        STNAME[13] = 'Start Sim '
        STNAME[14] = 'Sowing    '
        STNAME[15] = 'Harvest   '

    elif CROP in ('BM', 'BH', 'BR', 'NP'):
        STNAME[0] = 'Emergence '
        STNAME[1] = 'First Leaf'
        STNAME[2] = 'End Juven.'
        STNAME[3] = 'Flower Ind'
        STNAME[4] = 'Flowering '
        STNAME[9] = 'Phys. Mat '
        STNAME[10] = 'Harv. Mat '
        STNAME[11] = 'End Msnode'
        STNAME[12] = 'End Leaf  '
        STNAME[13] = 'Start Sim '
        STNAME[14] = 'Sowing    '
        STNAME[15] = 'Harvest   '

    elif CROP == 'CO':
        STNAME[0] = 'Emergence '
        STNAME[1] = 'First Leaf'
        STNAME[2] = 'End Juven.'
        STNAME[3] = 'Flower Ind'
        STNAME[4] = 'Flowering '
        STNAME[5] = 'Boll > 6mm'
        STNAME[6] = 'End Flower'
        STNAME[7] = 'First Seed'
        STNAME[8] = 'Bolls>.5sz'
        STNAME[9] = 'Cracked Bl'
        STNAME[10] = '90%Open Bl'
        STNAME[11] = 'End Msnode'
        STNAME[12] = 'End Leaf  '
        STNAME[13] = 'Start Sim '
        STNAME[14] = 'Sowing    '
        STNAME[15] = 'Harvest   '

    elif CROP == 'FA':
        STNAME[13] = 'Start Sim '
        STNAME[15] = 'End Sim   '

    if PLME == 'T':
        STNAME[14] = 'Transplant'




