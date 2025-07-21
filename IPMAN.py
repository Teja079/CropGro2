from READS import *
from ModuleDefs import *
import numpy as np

#=======================================================================
#  IPFERT, Subroutine
#  Subroutine to read in fertilizer applications during season
#  To read *FERTILIZER section in the V3.5 input files
#-----------------------------------------------------------------------
#  INPUT  : LUNEXP,FILEX,LNFER,YRSIM,ISWNIT
#  OUTPUT : NFERT,FDAY,IFTYPE,FERCOD,DFERT,ANFER,APFER,AKFER
#           ACFER,AOFER,FOCOD,ANFER
#-----------------------------------------------------------------------
#  Called : IPEXP
#  Calls  : FIND,IGNORE
#=======================================================================
def IPFERT(LUNEXP, FILEX, LNFER, YRSIM, ISWNIT, ISWPHO, ISWPOT, ISWWAT, LNSIM):
    from ERROR import ERROR
    from DATES import Y4K_DOY
    ERRKEY = 'IPFERT'
    FINDCH = '*FERTI'
    NAPPL = 20

    NFERT = 0
    TOTNAP = 0.0

    FDAY = [0] * NAPPL
    IFTYPE = [''] * NAPPL
    FERCOD = [''] * NAPPL
    DFERT = [0.0] * NAPPL
    ANFER = [0.0] * NAPPL
    APFER = [0.0] * NAPPL
    AKFER = [0.0] * NAPPL
    ACFER = [0.0] * NAPPL
    AOFER = [0.0] * NAPPL
    FOCOD = [''] * NAPPL

    if LNFER > 0:
        if ISWNIT == 'N' and LNSIM == 0:
            ISWNIT = 'Y'
            IFERI = 'R'
        else:
            IFERI = 'R'

        if ISWWAT == 'N' and LNSIM == 0:
            ISWWAT = 'Y'

        LINEXP = 0
        NFERT = 1
        LINEXP, IFIND = FIND(LUNEXP, FINDCH, 0)
        if IFIND == 0:
            ERROR(ERRKEY, 1, FILEX, LINEXP)

        while True:
            ISECT, CHARTEST = IGNORE(LUNEXP, LINEXP)

            if ISECT == 1:
                parts = CHARTEST.split()
                try:
                    LN = int(parts[0])
                except:
                    ERROR(ERRKEY, 99, FILEX, LINEXP)

                if LN != LNFER:
                    continue

                try:

                    FDAY[NFERT - 1] = int(parts[1])
                    IFTYPE[NFERT - 1] = parts[2]
                    FERCOD[NFERT - 1] = parts[3]
                    DFERT[NFERT - 1] = float(parts[4])
                    ANFER[NFERT - 1] = float(parts[5])
                    APFER[NFERT - 1] = float(parts[6])
                    AKFER[NFERT - 1] = float(parts[7])
                    ACFER[NFERT - 1] = float(parts[8])
                    AOFER[NFERT - 1] = float(parts[9])
                    FOCOD[NFERT - 1] = parts[10]
                except:
                    ERROR(ERRKEY, 99, FILEX, LINEXP)

                if FDAY[NFERT - 1] < 0 or (IFERI == 'R' and FDAY[NFERT - 1] % 1000 > 366):
                    ERROR(ERRKEY, 10, FILEX, LINEXP)

                if IFERI == 'R':
                    FDAY[NFERT - 1] = Y4K_DOY(FDAY[NFERT - 1], FILEX, LINEXP, ERRKEY, 3)

                if IFERI == 'R' and FDAY[NFERT - 1] < YRSIM:
                    ERROR(ERRKEY, 3, FILEX, LINEXP)

                if DFERT[NFERT - 1] < 0 or DFERT[NFERT - 1] > 99999.:
                    ERROR(ERRKEY, 11, FILEX, LINEXP)

                if ANFER[NFERT - 1] < 0 or ANFER[NFERT - 1] > 99999.:
                    ERROR(ERRKEY, 12, FILEX, LINEXP)

                try:
                    IFFTYP = int(IFTYPE[NFERT - 1][2:5])
                    if IFFTYP < 1 or IFFTYP >= 999:
                        raise ValueError
                except:
                    ERROR(ERRKEY, 14, FILEX, LINEXP)

                try:
                    IFFCOD = int(FERCOD[NFERT - 1][2:5])
                    if IFFCOD < 1 or IFFCOD > 20:
                        raise ValueError
                except:
                    FERCOD[NFERT - 1] = FERCOD[NFERT - 1][:2] + '001'

                if ISWPHO in ('Y', 'H'):
                    if APFER[NFERT - 1] < 0 or APFER[NFERT - 1] > 99999.:
                        APFER[NFERT - 1] = 0.0

                if ISWPOT == 'Y':
                    if AKFER[NFERT - 1] < 0 or AKFER[NFERT - 1] > 99999.:
                        AKFER[NFERT - 1] = 0.0

                TOTNAP += ANFER[NFERT - 1]
                NFERT += 1

                if NFERT > NAPPL:
                    break
            else:
                if NFERT == 1:
                    ERROR(ERRKEY, 2, FILEX, LINEXP)
                break

    NFERT = max((NFERT - 1), 0)
    #LUNEXP.seek(0)

    return NFERT, FDAY, IFTYPE, FERCOD, DFERT, ANFER, APFER, AKFER, ACFER, AOFER, FOCOD, TOTNAP

#=======================================================================
#  IPIRR, Subroutine
#  Determines irrigation application for a simulation
#-----------------------------------------------------------------------
#  INPUT  : LUNEXP,FILEX,LNIR,YRSIM,ISWWAT,NIRR,EFFIRX,DSOILX,THETCX
#           IEPTX,IOFFX,IAMEX,NAPW,TOTAPW,AIRAMX,IDLAPL,IRRCOD,AMT
#
#  OUTPUT :
#-----------------------------------------------------------------------
#  Called : IPEXP
#
#  Calls  : FIND ERROR IGNORE
#-----------------------------------------------------------------------
#=======================================================================
def IPIRR(LUNEXP, FILEX, LNIR, YRSIM, ISWWAT,
          NIRR, EFFIRX, DSOILX, THETCX, IEPTX, IOFFX, IAMEX, LNSIM,
          NAPW, TOTAPW, AIRAMX, IDLAPL, IRRCOD, AMT, IIRV, IIRRI):
    from ERROR import ERROR
    from DATES import Y4K_DOY

    ERRKEY = 'IPIRR '
    FINDCH = '*IRRIG'

    # Set default values
    EFFIRX = 1.00
    NIRR = 0
    NAPW = 0
    TOTAPW = 0.0
    THETCX = 0.0
    DSOILX = 0.0
    AIRAMX = 0.0
    IOFFX = 'GS000'
    IAMEX = 'IR001'

    for j in range(len(IDLAPL)):
        IDLAPL[j] = 0
        AMT[j] = 0.0
        IRRCOD[j] = '     '

    if LNIR > 0:
        if ISWWAT == 'N' and LNSIM == 0:
            IIRRI = 'R'
            ISWWAT = 'Y'
        NIRR = 1

        LINEXP, IFIND = FIND(LUNEXP, FINDCH)
        if IFIND == 0:
            ERROR(ERRKEY, 1, FILEX, LINEXP)

        while True:
            ISECT, CHARTEST = IGNORE(LUNEXP, LINEXP)
            if ISECT == 1:
                try:
                    parts = CHARTEST.split()
                    LN = int(parts[0])
                    if LN != LNIR:
                        continue
                    EFFIRX = float(parts[1])
                    DSOILX = float(parts[2])
                    THETCX = float(parts[3])
                    IEPTX = float(parts[4])
                    IOFFX = parts[5]
                    IAMEX = parts[6]
                    AIRAMX = float(parts[7])
                except Exception:
                    ERROR(ERRKEY, 1, FILEX, LINEXP)

                if AIRAMX < 0.0:
                    AIRAMX = 0.0
                if EFFIRX <= 0.0:
                    EFFIRX = 1.0
                if DSOILX <= 0.0:
                    DSOILX = 30.0
                if THETCX <= 0.0:
                    THETCX = 75.0
                if IOFFX[:3] == '-99' or IOFFX[2:5] == '-99':
                    IOFFX = 'GS000'
                if IAMEX[:3] == '-99' or IAMEX[2:5] == '-99':
                    IAMEX = 'IR001'
                break
            else:
                ERROR(ERRKEY, 2, FILEX, LINEXP)

        while True:
            PERM = 0
            ISECT, CHARTEST = IGNORE(LUNEXP, LINEXP)
            if ISECT != 1:
                break

            try:
                parts = CHARTEST.split()
                LN = int(parts[0])
                if LN > LNIR:
                    break

                IDLAPL[NIRR] = int(parts[1])
                IRRCOD[NIRR] = parts[2]
                AMT[NIRR] = float(parts[3])
                IIRV[NIRR] = int(parts[4])
            except Exception:
                ERROR(ERRKEY, 1, FILEX, LINEXP)

            if IDLAPL[NIRR] < 0 or (IIRRI == 'R' and IDLAPL[NIRR] % 1000 > 366):
                ERROR(ERRKEY, 10, FILEX, LINEXP)

            if IIRRI != 'D':
                IDLAPL[NIRR] = Y4K_DOY(IDLAPL[NIRR], FILEX, LINEXP, ERRKEY, 3)

            if IIRRI == 'R' and IDLAPL[NIRR] < YRSIM:
                ERROR(ERRKEY, 3, FILEX, LINEXP)

            if IIRRI == 'D' and IDLAPL[NIRR] < 0:
                continue

            try:
                IRRCD = int(IRRCOD[NIRR][2:5])
            except:
                ERROR(ERRKEY, 12, FILEX, LINEXP)

            if IRRCD < 1 or IRRCD > 11:
                ERROR(ERRKEY, 12, FILEX, LINEXP)

            if IRRCD in [3, 9] and IIRV[NIRR] > 0:
                if IIRV[NIRR] == 6:
                    PERM = 1
                elif IIRV[NIRR] == 7:
                    PERM = 2
                elif IIRV[NIRR] == 8:
                    PERM = 3

                if PERM > 0 and NIRR > 1:
                    PERMDOY = IDLAPL[NIRR]
                    for i in range(NIRR - 1, -1, -1):
                        if (IDLAPL[i] == PERMDOY and
                            IRRCOD[i][2:5] not in ['007', '008', '009', '010']):
                            if PERM == 1:
                                IRRCOD[i] = IRRCOD[i][:2] + '006'
                            elif PERM in [2, 3]:
                                IRRCOD[i] = IRRCOD[i][:2] + '011'

            if IRRCOD[NIRR][2:5] not in ['007', '008', '009', '010']:
                NAPW += 1
                if AMT[NAPW] > 0.0:
                    TOTAPW += AMT[NAPW]
                if PERM > 0:
                    if PERM == 1:
                        IRRCOD[NIRR] = IRRCOD[NIRR][:2] + '006'
                    elif PERM in [2, 3]:
                        IRRCOD[NIRR] = IRRCOD[NIRR][:2] + '011'

            NIRR += 1
            if NIRR >= len(IDLAPL):
                break

    #LUNEXP.seek(0)
    NIRR = max(NIRR - 1, 0)

    return (ISWWAT, NIRR, EFFIRX, DSOILX, THETCX, IEPTX,
            IOFFX, IAMEX, NAPW, TOTAPW, AIRAMX,
            IDLAPL, IRRCOD, AMT, IIRV, IIRRI)
#=======================================================================
#  IPHAR, Subroutine
#  Subroutine to read in harvest management
#-----------------------------------------------------------------------
#  INPUT  : LUNEXP,FILEX,LNHAR,YEAR
#  OUTPUT : HDATE,HSTG,HCOM,HSIZ,HPC,NHAR
#-----------------------------------------------------------------------
#  Called : IPEXP
#=======================================================================
def IPHAR (LUNEXP,FILEX,LNHAR,IHARI,YRSIM,CROP):
    import fortranformat as ff
    from ERROR import ERROR
    from READS import IGNORE, FIND
    from DATES import  Y4K_DOY

    HSTG = np.full(NAPPL, '', dtype='U5')
    HCOM = np.full(NAPPL, '', dtype='U5')
    HSIZ = np.full(NAPPL, '', dtype='U5')

    HDATE = np.zeros(NAPPL, dtype=int)
    HPC = np.zeros(NAPPL, dtype=float)
    HBPC = np.zeros(NAPPL, dtype=float)

    ERRKEY='IPHAR '
    FINDCH='*HARVE'

    f60 = ff.FortranRecordReader("I3,I5,3(1X,A5),2(1X,F5.0)")

    NHAR  = 0

      # DO J = 1, NHAR
      #    HSTG(J)  = '     '
      #    HCOM(J)  = '     '
      #    HSIZ(J)  = '     '
      #    HPC(J)   = 100.0
      #    HDATE(J) = -99
      # END DO

    if LNHAR != 0 :
        NHAR = 0
        LINEXP,IFIND = FIND (LUNEXP,FINDCH)
        if IFIND == 0 : ERROR (ERRKEY,1,FILEX,LINEXP)

        while True :
            LINEXP,ISECT,CHARTEST = IGNORE (LUNEXP,LINEXP)

            if ISECT == 1 :
                # LN = CHARTEST[:2]
                LN = f60.read(CHARTEST)[0]
                # if ERRNUM == 0 : ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
                if LN != LNHAR : continue
#        Read several lines of harvest details
#                 values = CHARTEST.split()
#                 (LN, HDATE[NHAR], HSTG[NHAR], HCOM[NHAR], HSIZ[NHAR], HPC[NHAR],
#                  HBPC[NHAR]) =  (strToType(x) for x in values)

                (LN, HDATE[NHAR], HSTG[NHAR], HCOM[NHAR],
                 HSIZ[NHAR], HPC[NHAR], HBPC[NHAR]) = f60.read(CHARTEST)

                # if ERRNUM != 0 : ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
                if (HDATE[NHAR] <  0 or
                    (IHARI == 'R' and HDATE[NHAR]%1000 > 366) or
                    (IHARI == 'W' and HDATE[NHAR]%1000 > 366) or
                    (IHARI == 'X' and HDATE[NHAR]%1000 > 366) or
                    (IHARI == 'Y' and HDATE[NHAR]%1000 > 366) or
                    (IHARI == 'Z' and HDATE[NHAR]%1000 > 366)) :
                        ERROR (ERRKEY,10,FILEX,LINEXP)
                if (IHARI == 'R' or IHARI == 'W' or
                    IHARI == 'X' or IHARI == 'Y' or IHARI == 'Z') :
                    HDATE[NHAR] = Y4K_DOY(HDATE[NHAR],FILEX,LINEXP,ERRKEY,6)
                if IHARI == 'R' and HDATE[NHAR] < YRSIM : continue
                if IHARI == 'W' and HDATE[NHAR] < YRSIM : continue
                if IHARI == 'X' and HDATE[NHAR] < YRSIM : continue
                if IHARI == 'Y' and HDATE[NHAR] < YRSIM : continue
                if IHARI == 'Z' and HDATE[NHAR] < YRSIM : continue

#        Harvested product defaults to 100%
                if HPC[NHAR] < -1.E-4 : HPC[NHAR] = 100.0
#        Harvested by-product defaults to 0%
                if HBPC[NHAR] < 1.E-4 : HBPC[NHAR] = 0.0
                if HSTG[NHAR] == '     ' : HSTG[NHAR] = '  -99'
                if HCOM[NHAR] == '     ' : HCOM[NHAR] = '  -99'
                if HSIZ[NHAR] == '     ' : HSIZ[NHAR] = '  -99'
                NHAR = NHAR + 1
                if NHAR >= NAPPL: break

            else:
                if NHAR == 1 :
                    ERROR (ERRKEY,2,FILEX,LINEXP)
                break
                 # GO TO 120
        NHAR = NHAR
      # GO TO 50
 # 120  REWIND (LUNEXP)

    if 'CSPT'.find(CROP) > 0 :
        if HDATE[1] < 0 : ERROR (ERRKEY,13,FILEX,LINEXP)
        if IHARI == 'A' : ERROR (ERRKEY,14,FILEX,LINEXP)

    NHAR = max(0,NHAR-1)
    if LNHAR == 0 and IHARI != 'M' and IHARI != 'A' :
        ERROR (ERRKEY,1,FILEX,LINEXP)
    if IHARI == 'G' and HSTG[1] == '     ' :
        ERROR (ERRKEY,3,FILEX,LINEXP)
    if IHARI == 'R' and HDATE[1] == 0 :
        ERROR (ERRKEY,4,FILEX,LINEXP)
    if IHARI == 'W' and HDATE[1] == 0 :
        ERROR (ERRKEY,4,FILEX,LINEXP)
    if IHARI == 'X' and HDATE[1] == 0 :
        ERROR (ERRKEY,4,FILEX,LINEXP)
    if IHARI == 'Y' and HDATE[1] == 0 :
        ERROR (ERRKEY,4,FILEX,LINEXP)
    if IHARI == 'Z' and HDATE[1] == 0 :
        ERROR (ERRKEY,4,FILEX,LINEXP)
    if IHARI == 'D' and HDATE[1] == 0 :
        ERROR (ERRKEY,5,FILEX,LINEXP)

    return HDATE, HSTG,HCOM, HSIZ, HPC, HBPC, NHAR

#=======================================================================
#  IPCUL, Subroutine
#  Reads parameters related cultivar selection from FILEX file
#-----------------------------------------------------------------------
#  INPUT
#       LUNEXP - Full file path to file-X (String)
#       FILEX - File-X name (String)
#       LNCU - Cultivar level number in cultivar section
#  OUTPUT
#       CROP - Two char indicating species type (String)
#       VARNO - Cultivar ID
#-----------------------------------------------------------------------
def IPCUL (LUNEXP,FILEX,LNCU):
    from ERROR import ERROR
    from READS import FIND, IGNORE
    import fortranformat as ff

    ERRKEY = 'IPCUL '
    FINDCH = '*CULTI'
    f55 = ff.FortranRecordReader("I3,A2,1X,A6,1X,A16")

    if LNCU > 0:
        LINEXP,IFIND = FIND (LUNEXP,FINDCH)
        if IFIND == 0: ERROR (ERRKEY,1,FILEX,LINEXP)

        LN = 0
        while LN < LNCU:
            LINEXP, ISECT, CHARTEST = IGNORE(LUNEXP, LINEXP)
            if ISECT == 1:
                try:
                    LN = f55.read(CHARTEST)[0]
                except Exception as e:
                    ERROR (ERRKEY,1,FILEX,LINEXP)
                if LN == LNCU:
                    try:
                        LNCU, CROP, VARNO, CNAME = f55.read(CHARTEST)
                    except Exception as e:
                        ERROR(ERRKEY, 1, FILEX, LINEXP)
            else:
                ERROR(ERRKEY,2,FILEX,LINEXP)

    return CROP,VARNO
#=======================================================================

# Test driver for IPCUL
# LUNEXP = "C:/DSSAT48/Strawberry/UFBA1701.SRX"
# FILEX = "UFBA1701.SRX"
# LNCU = 1
# print(IPCUL(LUNEXP,FILEX,LNCU))

# Test driver for IPHAR
# LUNEXP = 'C:/DSSAT48/Strawberry/UFBA1701.SRX'
# FILEX = 'UFBA1701.SRX'
# LNHAR = 1
# IHARI = 'R'
# YRSIM = 2017264
# CROP = 'SR'
#
# HDATE,HSTG,HCOM,HSIZ,HPC,HBPC, NHAR = IPHAR (LUNEXP,FILEX,LNHAR,IHARI,YRSIM,CROP)