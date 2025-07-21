# =======================================================================
#   IPSOIL_Inp, Subroutine
#   Soil selection
# -----------------------------------------------------------------------
#   INPUT  : RNMODE,FILES,PATHSL,ISWWAT
#   GLOBAL : SLNO is used
#   OUTPUT : NSENS
# -----------------------------------------------------------------------
#   Called : SESOIL INPUT
#   Calls  : FIND IGNORE ERROR
# -----------------------------------------------------------------------
#                          DEFINITIONS
#   RNMOSE : Runmode (1 char)
#   FILES  : .SOL soil file name
#   PATHSL : Soil file dir
#   ISWITCH: SwitchType
# =======================================================================
#from OPHEAD import HEADER

def IPSOIL_Inp (RNMODE,FILES,PATHSL,NSENS,ISWITCH):
    from ModuleDefs import NL
    from ERROR import ERROR
    from WARNING import WARNING
    from READS import IGNORE, IGNORE2
    from COMSOI import SOI01 as S1, SOI02 as S2, SOI03 as S3
    from LMATCH import LMATCH, LYRSET, LYRSET2 # LYRSET3
    import fortranformat as ff
    import numpy as np
    import os
    #from UTILS import create_float_array

    f950  = ff.FortranRecordWriter("/,5X,' SOIL ',A10,' is not in the SOIL file:',/,A92")
    f5000 = ff.FortranRecordWriter("/, '  More.... press < ENTER > key '")
    f5101 = ff.FortranRecordWriter("10X,'ERROR! Soil Selection must be between 1 and ',I3,/")
    f5102 = ff.FortranRecordWriter("10X,'ERROR! Soil Selection must be an INTEGER value',/")
    f5130 = ff.FortranRecordWriter("T25, 'SOILS IN THE DATA BASE', /,T3, 'REF', T25,"
                                   "22('='),/,T3, 'NO.', T7, 'TAXONOMY NAME', T67,"
                                   "'PEDON NUMBER', /T2, 4('-'), 1X, 50('-'), T67, 12('-')")
    f5140 = ff.FortranRecordWriter("I4,') ',A50, T67, A10")
    f5160 = ff.FortranRecordWriter("/,6X,'SELECTED SOIL TYPE ===>',1X,I3,/,6X,'NEW SELECTION ?    --->',2X,' '")

    f5030 = ff.FortranRecordReader("1X,A10, 2X, A11,1X,A5,1X,F5.0,1X,A50")
    f5035 = ff.FortranRecordReader("2(1X,A11),2(1X,F8.3),1X,A50")

    MAXCOL = 50
    HEADER = []
    MSG = ['']*25
    COL = np.zeros((MAXCOL,2), dtype = int) #COL keeps beginning and ending column for each header
    ERRKEY = 'IPSOIL'
    LUNSL = 12
    BLANK = ' '
    ZLYR = [-99.0]*NL
    MH = [''] * NL

    NLSOIL = 0
    # -----------------------------------------------------------------------
    #     No soil file read - default conditions
    if S1.SLNO.strip() == '' or  S1.SLNO.strip() == '-99':
        S1.SLPF = 1.0
        S1.NLAYR = 1

    else: # Read soil file
        LINSOL = 0
        PATHSL = PATHSL.strip()
        FILESS = os.path.join(PATHSL,FILES)
# -----------------------------------------------------------------------
#      Sensitivity Analysis - soil selection
        if NSENS == 1:
            NLOOP = 0
            S1.NLSOIL = 0
            LINE = [' '] * 80
            I = 0

            if 'I' in RNMODE or 'E' in RNMODE:
                print('\n'*20) #CLEAR
                print(f5130.write(''))
            while True:
                LINSOL, ISECT, C255 = IGNORE(FILESS, LINSOL)
                if ISECT == 0:
                    break
                if not C255.startswith('*') or C255[1:5] == 'SOIL':
                    continue

                try:
                    S1.PEDON, S1.SLSOUR, S1.SLTXS, S1.SLDP, S1.SLDESC = f5030.read(C255)
                except Exception as e:
                    ERR = getattr(e, 'errno', 1) #Try to get errno from e. Default ERR = 1
                    ERROR(ERRKEY, ERR, FILES, LINSOL)

                S1.PEDON = S1.PEDON.upper()
                S1.SLNO =  S1.SLNO.upper()

                if 'I' in RNMODE or 'E' in RNMODE:
                    print(f5140.write([I,S1.SLDESC,S1.PEDON]))
                if S1.PEDON == S1.SLNO:
                    S1.NLSOIL = I + 1

                I += 1
                # Write out pause statement every 15 lines
                if I % 15 == 0 and ('I' in RNMODE or 'E' in RNMODE):
                    print(f5000.write(''))
                    # READ  (5,'(A1)') ANS
                    ANS = str(input().ljust(1))

            NLOOP += 1
            if NLOOP >= 25:
                ERROR(ERRKEY, 1, FILES, LINSOL)

            LINE[0] = ' '

            if ('I' in RNMODE or 'E' in RNMODE) and S1.NLSOIL == 0:
                print(f950.write([S1.SLNO,FILESS]))
            if 'I' in RNMODE or 'E' in RNMODE:
                print(f5160.write([S1.NLSOIL]))
            LINE = list(input().ljust(80))  # Simulates reading 80A1
            # SL, FLAG = VERIFY(LINE)
            #
            # if SL <= 0:
            #     SL = S1.NLSOIL
            # elif FLAG > 0 or SL > (I - 1):
            #     pass
            # elif SL != int(SL):
            #     pass
            # elif SL > 0:
            #     NLSOIL = int(round(SL))
            # else:
            #     ERROR(ERRKEY, 3, FILES, LINSOL)

#-----------------------------------------------------------------------
#       Set default values for soil parameters
        S1.SLTXS, S1.TAXON, S1.SLSOUR, S1.SSITE, S1.SCOUNT, S1.SLDESC = ['-99'] * 6
        S1.SMHB, S1.SMPX, S1.SMKE, S1.SGRP, S1.SCOM = ['-99'] * 5

        S3.U, S3.SWCON, S3.CN2, S3.SALB = [-99.0] * 4
        S3.LL, S3.DUL, S3.SAT = ([-99.0] * NL for _ in range(3))
        S3.SHF, S3.SWCN, S3.BD, S3.OC, S3.PH, S3.EXTP, S3.TOTP, S3.ORGP = ([-99.0] * NL for _ in range(8))
        S3.SLNF, S3.SLPF = [-99.0] * 2
        S3.CEC, S3.ADCOEF, S3.STONES, S3.CLAY, S3.SILT, S3.PHKCL = ([-99.0] * NL for _ in range(6))
        S3.SLAT, S3.SLONG = [-99.0] * 2
        S3.CACO, S3.EXTAL, S3.EXTFE, S3.EXTMN, S3.TOTBAS, S3.TOTN = ([-99.0] * NL for _ in range(6))
        S3.PTERMA, S3.PTERMB, S3.EXK, S3.EXMG, S3.EXNA, S3.EXTS, S3.SLEC, S3.EXCA, S3.SASC = \
            ([-99.0] * NL for _ in range(9))
        S3.SAEA = [-99.0] * NL
        S3.alphaVG, S3.mVG, S3.nVG, S3.WCR = ([-99.0] * NL for _ in range(4))

        SLDP = -99.




# -----------------------------------------------------------------------
#       Find correct soil within soil file
        I = 0
        while True:
            I += 1

            # CALL IGNORE (LUNSL,LINSOL,ISECT,C255)
            LINSOL, ISECT, C255 = IGNORE(FILESS, LINSOL)

            if ISECT == 0:  # end of file
                if FILES[:4] == 'SOIL':
                    FILES = S1.SLNO[:2] + '.SOL  '
                    FILESS = os.path.join(PATHSL, FILES)

                    try:
                        LUNSL = open(FILESS, 'r')
                    except Exception as e:
                        ERR = getattr(e, 'errno', 1)
                        ERROR(ERRKEY, ERR, FILES, 0)

                    LINSOL = 0
                    continue

                # IF (ISWITCH%ISWWAT .EQ. 'N') THEN
                #   SLPF  = 1.0
                #   NLAYR = 1
                #   RETURN
                # ELSE
                MSG[0] = "Soil profile missing from file."
                MSG[1] = f"Soil ID: {S1.SLNO}"
                MSG[2] = f"File: {FILESS[:72]}"
                WARNING(3, ERRKEY, MSG)
                ERROR(ERRKEY, 4, FILES, LINSOL)
                # ENDIF

            if not C255.startswith('*') or C255[1:5] == 'SOIL':
                continue

            try:
                S1.PEDON, S1.SLSOUR, S1.SLTXS, SLDP, S1.SLDESC = f5030.read(C255)
                ERR = 0
            except Exception as e:
                ERR = getattr(e, 'errno', 1)
                MSG[0] = f"Error in soil file on line {LINSOL:6d}"
                MSG[1] = "Model will continue to search for a valid soil profile."
                WARNING(2, ERRKEY, MSG)

            if S1.PEDON != S1.SLNO and NSENS == 0:
                continue
            if I < NLSOIL and NSENS == 1:
                continue
            break
            # If there was an error reading the profile and this is the profile of interest, then
            # call an error and stop model run.
        if ERR != 0:
            ERROR(ERRKEY, ERR, FILES, LINSOL)
        S1.PEDON = S1.PEDON.upper()
        S1.SLNO = S1.SLNO.upper()
# -----------------------------------------------------------------------
#         Found correct soil
# -----------------------------------------------------------------------
        LINSOL, ISECT, C255 = IGNORE2 (FILESS, LINSOL)
        if ISECT == 3:
            LINSOL, ISECT, C255 = IGNORE(FILESS, LINSOL)
            if ISECT != 1:
                ERROR(ERRKEY, 4, FILES, LINSOL)
            S1.SSITE, S1.SCOUNT, S3.SLAT, S3.SLONG, S1.TAXON = f5035.read(C255)
           #IF (ERR .NE. 0) CALL ERROR (ERRKEY,ERR,FILES,LINSOL)

        HEADER = ['-99'] * NL
        S2.NLAYR = 0

        while True: #This loops parses the .SOL file 'goodsoil'
            LINSOL, ISECT, C255 = IGNORE2(FILESS, LINSOL)
            if ISECT in [0, 2]:  #!End of file, End of Section
                if NLAYRI == 0: #TODO NLAYRI is defined later
                    if ISECT == 0:
                        ERROR(ERRKEY, 4, FILES, LINSOL)
                    if ISECT == 2:
                        ERROR(ERRKEY, 3, FILES, LINSOL)
                break

            elif ISECT == 3:  #!Header line found ('@' in column 1),
                # This condition needs to be met before ISECT == 1
                header_vals = C255.split()[1:] #Header names excluding @
                #HEADER.append(header_vals)
                HEADER = header_vals
                if len(header_vals) < 1:
                    ERROR(ERRKEY, 10, FILES, LINSOL)
                L = 0

            elif ISECT == 1:  # CASE(1)  !Data line found
                if '-99' in HEADER[0]:
                    ERROR(ERRKEY, 3, FILES, LINSOL)

                if HEADER[0].strip() != 'SLB':
                    data = C255.split()  #Split data in current row
                    for I,H in enumerate(HEADER): #Assig
                        try:
                            if H == 'SCOM':   S1.SCOM = data[I]
                            elif H == 'SALB': S3.SALB = float(data[I])
                            elif H == 'SLU1': S3.U = float(data[I])
                            elif H == 'SLDR': S3.SWCON = float(data[I])
                            elif H == 'SLRO': S3.CN2 = float(data[I])
                            elif H == 'SLNF': S3.SLNF = float(data[I])
                            elif H == 'SLPF': S3.SLPF = float(data[I])
                            elif H == 'SMHB': S1.SMHB = data[I]
                            elif H == 'SMPX': S1.SMPX = data[I]
                            elif H == 'SMKE': S1.SMKE = data[I]
                            elif H == 'SGRP': S1.SGRP = data[I]
                        except Exception as e:
                            ERR = getattr(e, 'errno', 1)
                            ERROR(ERRKEY, ERR, FILES, LINSOL)

                else: #First column is 'SLB'
                    L += 1
                    if L > NL: ERROR(ERRKEY, 2, FILES, LINSOL)
                    if L == 1: LINSOL_1 = LINSOL
                    data = C255.split()
                    try:
                        ZLYR[L - 1] = float(data[0])
                    except Exception as e:
                        ERROR(ERRKEY, getattr(e, 'errno', 1), FILES, LINSOL)

                    for I in range(1, len(HEADER)):
                        key = HEADER[I]
                        try:
                            if key == 'SLMH': MH[L - 1] = data[I]
                            elif key == 'SLLL': S3.LL[L - 1] = float(data[I])
                            elif key == 'SDUL': S3.DUL[L - 1] = float(data[I])
                            elif key == 'SSAT': S3.SAT[L - 1] = float(data[I])
                            elif key == 'SRGF': S3.SHF[L - 1] = float(data[I])
                            elif key == 'SSKS': S3.SWCN[L - 1] = float(data[I])
                            elif key == 'SBDM': S3.BD[L - 1] = float(data[I])
                            elif key == 'SLOC': S3.OC[L - 1] = float(data[I])
                            elif key == 'SLCL': S3.CLAY[L - 1] = float(data[I])
                            elif key == 'SLSI': S3.SILT[L - 1] = float(data[I])
                            elif key == 'SLCF': S3.STONES[L - 1] = float(data[I])
                            elif key == 'SLNI': S3.TOTN[L - 1] = float(data[I])
                            elif key == 'SLHW': S3.PH[L - 1] = float(data[I])
                            elif key == 'SLHB': S3.PHKCL[L - 1] = float(data[I])
                            elif key == 'SCEC': S3.CEC[L - 1] = float(data[I])
                            elif key == 'SADC':
                                try:
                                    S3.ADCOEF[L - 1] = float(data[I])
                                except:
                                    S3.ADCOEF[L - 1] = -99.0
                            elif key == 'SLPX': S3.EXTP[L - 1] = float(data[I])
                            elif key == 'SLPT': S3.TOTP[L - 1] = float(data[I])
                            elif key == 'SLPO': S3.ORGP[L - 1] = float(data[I])
                            elif key == 'CACO3': S3.CACO[L - 1] = float(data[I])
                            elif key == 'SLAL': S3.EXTAL[L - 1] = float(data[I])
                            elif key == 'SLFE': S3.EXTFE[L - 1] = float(data[I])
                            elif key == 'SLMN': S3.EXTMN[L - 1] = float(data[I])
                            elif key == 'SLBS': S3.TOTBAS[L - 1] = float(data[I])
                            elif key == 'SLPA': S3.PTERMA[L - 1] = float(data[I])
                            elif key == 'SLPB': S3.PTERMB[L - 1] = float(data[I])
                            elif key == 'SLKE': S3.EXK[L - 1] = float(data[I])
                            elif key == 'SLMG': S3.EXMG[L - 1] = float(data[I])
                            elif key == 'SLNA': S3.EXNA[L - 1] = float(data[I])
                            elif key == 'SLSU': S3.EXTS[L - 1] = float(data[I])
                            elif key == 'SLEC': S3.SLEC[L - 1] = float(data[I])
                            elif key == 'SLCA': S3.EXCA[L - 1] = float(data[I])
                            elif key == 'ALFVG': S3.alphaVG[L - 1] = float(data[I])
                            elif key == 'MVG': S3.mVG[L - 1] = float(data[I])
                            elif key == 'NVG': S3.nVG[L - 1] = float(data[I])
                            elif key == 'WCRES': S3.WCR[L - 1] = float(data[I])
                        except Exception as e:
                            ERR = getattr(e, 'errno', 1)
                            ERROR(ERRKEY, ERR, FILES, LINSOL)

                    NLAYRI = L
                    if NLAYRI == NL:
                        S3.DS[NL-1] = max(S3.DS[NL-1], ZLYR[NLAYRI-1])

        S1.SLNO = S1.PEDON

        if S3.SWCON < 0.0: ERROR(ERRKEY, 10, FILES, LINSOL)
        if S3.CN2 <= 0.0: ERROR(ERRKEY, 11, FILES, LINSOL)
        if S3.SALB <= 0.0: ERROR(ERRKEY, 12, FILES, LINSOL)

        if S3.SLNF > 1.0 or S3.SLNF < 0.0:
            S3.SLNF = 1.0
            if ISWITCH.ISWNIT != 'N':
                MSG[0] = "Invalid value for SLNF in soil file."
                MSG[1] = "Value changed to 1.0."
                WARNING(2, ERRKEY, MSG)

        if ISWITCH.ISWWAT != 'N':
            ERR = 0
            for J in range(1, NLAYRI + 1):
                if (S3.DUL[J - 1] - S3.SAT[J - 1]) > 0.0:
                    ERROR(ERRKEY, 7, FILES, S1.LINSOL_1 + J - 1)
                if (S3.LL[J - 1] - S3.DUL[J - 1]) > 0.0:
                    ERROR(ERRKEY, 8, FILES, S1.LINSOL_1 + J - 1)
                if S3.DUL[J - 1] < 0.0:
                    ERROR(ERRKEY, 13, FILES, S1.LINSOL_1 + J - 1)
                if abs(S3.SAT[J - 1] - S3.DUL[J - 1]) <= 0.0:
                    S3.SAT[J - 1] = S3.DUL[J - 1] + 0.01
                if abs(S3.DUL[J - 1] - S3.LL[J - 1]) <= 0.0:
                    S3.LL[J - 1] = S3.DUL[J - 1] - 0.01
                if S3.SHF[J - 1] < 0.0:
                    MSG[0] = f'File: {FILESS:<72}'
                    MSG[1] = f'Line number: {S1.LINSOL_1 + J - 1:>4}  Soil layer: {J:>2}'
                    MSG[2] = 'Root growth factor is missing.  '
                    MSG[3] = 'Model requires value between 0 and 1.'
                    MSG[4] = 'Program will stop.'
                    WARNING(5, ERRKEY, MSG)
                    ERROR(ERRKEY, 14, FILES, S1.LINSOL_1 + J - 1)

                if S3.SWCN[J - 1] < 0.0:
                    S3.SWCN[J - 1] = -99.
                    ERR += 1

            if ERR > 0:
                MSG[0] = "Saturated hydraulic conductivity equal to zero for one or more soil layers."
                MSG[1] = "Data will be treated as missing."
                WARNING(2, ERRKEY, MSG)

        if ISWITCH.MESOL == '1':
            S2.NLAYR, S3.DS, S3.DLAYR, S3.DEPMAX = LYRSET(NLAYRI, ZLYR) #Not tested
        elif ISWITCH.MESOL == '3':
            pass
            #LYRSET3(NLAYRI, ZLYR, S3.DS, S2.NLAYR, S3.DLAYR, S3.DEPMAX)
        else: #DEFAULT
            S2.NLAYR, S3.DS, S3.DLAYR, S3.DEPMAX = LYRSET2(NLAYRI, ZLYR)

        # Check for top layer too thick (could happen for MESOL='3')
        if S3.DLAYR[0] > 5:
            MSG[0] = "Soil layer 1 thicker than 5 cm."
            MSG[1] = "Soil water algorithms may become unstable."
            WARNING(2, ERRKEY, MSG)

        S3.LL = LMATCH(NLAYRI, ZLYR, S3.LL, S2.NLAYR, S3.DS)
        S3.DUL = LMATCH(NLAYRI, ZLYR, S3.DUL, S2.NLAYR, S3.DS)
        S3.SAT = LMATCH(NLAYRI, ZLYR, S3.SAT, S2.NLAYR, S3.DS)
        S3.SHF = LMATCH(NLAYRI, ZLYR, S3.SHF, S2.NLAYR, S3.DS)
        S3.SWCN = LMATCH(NLAYRI, ZLYR, S3.SWCN, S2.NLAYR, S3.DS)
        S3.BD = LMATCH(NLAYRI, ZLYR, S3.BD, S2.NLAYR, S3.DS)
        S3.OC = LMATCH(NLAYRI, ZLYR, S3.OC, S2.NLAYR, S3.DS)
        S3.CLAY = LMATCH(NLAYRI, ZLYR, S3.CLAY, S2.NLAYR, S3.DS)
        S3.SILT = LMATCH(NLAYRI, ZLYR, S3.SILT, S2.NLAYR, S3.DS)
        S3.STONES = LMATCH(NLAYRI, ZLYR, S3.STONES, S2.NLAYR, S3.DS)
        S3.TOTN = LMATCH(NLAYRI, ZLYR, S3.TOTN, S2.NLAYR, S3.DS)
        S3.PH = LMATCH(NLAYRI, ZLYR, S3.PH, S2.NLAYR, S3.DS)
        S3.PHKCL = LMATCH(NLAYRI, ZLYR, S3.PHKCL, S2.NLAYR, S3.DS)
        S3.CEC = LMATCH(NLAYRI, ZLYR, S3.CEC, S2.NLAYR, S3.DS)
        S3.ADCOEF = LMATCH(NLAYRI, ZLYR, S3.ADCOEF, S2.NLAYR, S3.DS)

        # Second tier soils data
        S3.EXTP = LMATCH(NLAYRI, ZLYR, S3.EXTP, S2.NLAYR, S3.DS)
        S3.TOTP = LMATCH(NLAYRI, ZLYR, S3.TOTP, S2.NLAYR, S3.DS)
        S3.ORGP = LMATCH(NLAYRI, ZLYR, S3.ORGP, S2.NLAYR, S3.DS)
        S3.CACO = LMATCH(NLAYRI, ZLYR, S3.CACO, S2.NLAYR, S3.DS)
        S3.EXTAL = LMATCH(NLAYRI, ZLYR, S3.EXTAL, S2.NLAYR, S3.DS)
        S3.EXTFE = LMATCH(NLAYRI, ZLYR, S3.EXTFE, S2.NLAYR, S3.DS)
        S3.EXTMN = LMATCH(NLAYRI, ZLYR, S3.EXTMN, S2.NLAYR, S3.DS)
        S3.TOTBAS = LMATCH(NLAYRI, ZLYR, S3.TOTBAS, S2.NLAYR, S3.DS)
        S3.PTERMA = LMATCH(NLAYRI, ZLYR, S3.PTERMA, S2.NLAYR, S3.DS)
        S3.PTERMB = LMATCH(NLAYRI, ZLYR, S3.PTERMB, S2.NLAYR, S3.DS)
        S3.EXK = LMATCH(NLAYRI, ZLYR, S3.EXK, S2.NLAYR, S3.DS)
        S3.EXMG = LMATCH(NLAYRI, ZLYR, S3.EXMG, S2.NLAYR, S3.DS)
        S3.EXNA = LMATCH(NLAYRI, ZLYR, S3.EXNA, S2.NLAYR, S3.DS)
        S3.EXTS = LMATCH(NLAYRI, ZLYR, S3.EXTS, S2.NLAYR, S3.DS)
        S3.SLEC = LMATCH(NLAYRI, ZLYR, S3.SLEC, S2.NLAYR, S3.DS)
        S3.EXCA = LMATCH(NLAYRI, ZLYR, S3.EXCA, S2.NLAYR, S3.DS)

        S3.alphaVG = LMATCH(NLAYRI, ZLYR, S3.alphaVG, S2.NLAYR, S3.DS)
        S3.mVG = LMATCH(NLAYRI, ZLYR, S3.mVG, S2.NLAYR, S3.DS)
        S3.nVG = LMATCH(NLAYRI, ZLYR, S3.nVG, S2.NLAYR, S3.DS)
        S3.WCR = LMATCH(NLAYRI, ZLYR, S3.WCR, S2.NLAYR, S3.DS)
# =======================================================================

# Test Case
# from ModuleDefs import SwitchType
# from COMSOI import SOI01 as S1
# S1.SLNO = 'UFGA010700'
# RNMODE = 'B'
# FILES = 'SOIL.SOL'
# PATHSL = 'C://DSSAT48//SOIL//'
# NSENS = 0
# ISWITCH = SwitchType()
# ISWITCH.MESOL = 2
# ISWITCH.ISWWAT = 'N'
#
# IPSOIL_Inp(RNMODE,FILES,PATHSL,NSENS,ISWITCH)