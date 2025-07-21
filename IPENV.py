#=======================================================================
#  IPENV, Subroutine
#  Input environmental data and check for errors.
#-----------------------------------------------------------------------
#  INPUT  : FILEX,LNENV,LUNEXP
#  OUTPUT : CO2ADJ,CO2FAC,DAYADJ,DAYFAC,DPTADJ,DPTFAC,NEV,PRCADJ,PRCFAC,
#           RADADJ,RADFAC,TMADJ,TMFAC,TXADJ,TXFAC,WMDATE,WNDADJ,WNDFAC
#           (Updates these variables in COMIBS IBS01 and IBS03)
#
#  Fn/Sub : EOSECT,ERROR,FIND,IGNORE,VALSTR
#
#-----------------------------------------------------------------------
#  Called : IPEXP
#  Calls  : FIND IGNORE ERROR
#=======================================================================

def IPENV(FILEX, LNENV, LUNEXP):
    from COMIBS import IBS01 as ib1, IBS02 as ib2, IBS03 as ib3, IBS04 as ib4
    import numpy as np
    from ModuleDefs import NAPPL
    from READS import FIND, IGNORE
    from ERROR import ERROR
    from DATES import Y4K_DOY
    import fortranformat as ff

    FINDCH = "*ENVIR"
    ERRKEY = 'IPENV '

    ib1.DAYFAC = np.full(NAPPL, '', dtype='U1')
    ib1.RADFAC = np.full(NAPPL, '', dtype='U1')
    ib1.TXFAC = np.full(NAPPL, '', dtype='U1')
    ib1.TMFAC = np.full(NAPPL, '', dtype='U1')
    ib1.WNDFAC = np.full(NAPPL, '', dtype='U1')
    ib1.PRCFAC = np.full(NAPPL, '', dtype='U1')
    ib1.CO2FAC = np.full(NAPPL, '', dtype='U1')
    ib1.DPTFAC = np.full(NAPPL, '', dtype='U1')

    ib4.WTHADJ = np.zeros((2,8))
    ib4.WTHADJ[1] = [1.0] * 8
    ib2.WMDATE  = np.zeros(NAPPL,dtype=int)
    ib3.DAYADJ  = np.zeros(NAPPL)
    ib3.RADADJ  = np.zeros(NAPPL)
    ib3.TXADJ   = np.zeros(NAPPL)
    ib3.TMADJ   = np.zeros(NAPPL)
    ib3.PRCADJ  = np.zeros(NAPPL)
    ib3.CO2ADJ  = np.zeros(NAPPL)
    ib3.DPTADJ  = np.zeros(NAPPL)
    ib3.WNDADJ  = np.zeros(NAPPL)

    WMODI = 'N'
    ib2.NEV = 0
    LINEXP = 0

    if LNENV == 0:
#        rewind(LUNEXP)
        ib2.NEV = max((ib2.NEV - 1), 0)
        return (ib3.CO2ADJ, ib1.CO2FAC, ib3.DAYADJ, ib1.DAYFAC, ib3.DPTADJ, ib1.DPTFAC,
            ib2.NEV, ib3.PRCADJ, ib1.PRCFAC, ib3.RADADJ, ib1.RADFAC, ib3.TMADJ, ib1.TMFAC, ib3.TXADJ, ib1.TXFAC,
            ib2.WMDATE, WMODI, ib3.WNDADJ, ib1.WNDFAC, ib4.WTHADJ)

    LINEXP,FOUND = FIND(LUNEXP, FINDCH)
    if FOUND == 0:
        ERROR(ERRKEY, 1, FILEX, LINEXP)

    if FOUND == 1:
        ib3.NEV = 0
        while True:
            FOUND, LINE = IGNORE(LUNEXP, LINEXP)
            if FOUND == 1:
                try:
                    LN = int(LINE[:2])
                except ValueError as ERRNUM:
                    ERROR(ERRKEY, ERRNUM, FILEX, LINEXP)

                if LN == LNENV:
                    NEV = ib3.NEV
                    try:
                        f1000 = ff.FortranRecordReader("I3,I5,5(1X,A1,F4.0),1X,A1,F4.0,2(1X,A1,F4.0)")
                        (LN, ib2.WMDATE[NEV], ib1.DAYFAC[NEV], ib3.DAYADJ[NEV], ib1.RADFAC[NEV], ib3.RADADJ[NEV],
                         ib1.TXFAC[NEV], ib3.TXADJ[NEV], ib1.TMFAC[NEV], ib3.TMADJ[NEV], ib1.PRCFAC[NEV],
                         ib3.PRCADJ[NEV],ib1.CO2FAC[NEV], ib3.CO2ADJ[NEV], ib1.DPTFAC[NEV], ib3.DPTADJ[NEV],
                         ib1.WNDFAC[NEV], ib3.WNDADJ[NEV]) = (
                            f1000.read(LINE)
                        )
                    except ValueError:
                        ERROR(ERRKEY, 1, FILEX, LINEXP)
                    Y4K_DOY(ib2.WMDATE[NEV], FILEX, LINEXP, ERRKEY, 3)
                    if ib3.DAYADJ[NEV] <= -90.0: ib3.DAYADJ[NEV] = 0.0
                    if ib3.RADADJ[NEV] <= -90.0: ib3.RADADJ[NEV] = 0.0
                    if ib3.TXADJ[NEV] <= -90.0: ib3.TXADJ[NEV] = 0.0
                    if ib3.TMADJ[NEV] <= -90.0: ib3.TMADJ[NEV] = 0.0
                    if ib3.PRCADJ[NEV] <= -90.0: ib3.PRCADJ[NEV] = 0.0
                    if ib3.CO2ADJ[NEV] <= -90.0: ib3.CO2ADJ[NEV] = 0.0
                    if ib3.DPTADJ[NEV] <= -90.0: ib3.DPTADJ[NEV] = 0.0
                    if ib3.WNDADJ[NEV] <= -90.0: ib3.WNDADJ[NEV] = 0.0

                    ib2.NEV += 1
                continue
            else:
                if ib2.NEV == 1:
                    ERROR(ERRKEY, 2, FILEX, LINEXP)
                break

 #   rewind(LUNEXP)
    ib2.NEV = max((ib2.NEV - 1), 0)

    return (ib3.CO2ADJ, ib1.CO2FAC, ib3.DAYADJ, ib1.DAYFAC, ib3.DPTADJ, ib1.DPTFAC,
            ib2.NEV, ib3.PRCADJ, ib1.PRCFAC, ib3.RADADJ, ib1.RADFAC, ib3.TMADJ, ib1.TMFAC, ib3.TXADJ, ib1.TXFAC,
            ib2.WMDATE, WMODI, ib3.WNDADJ, ib1.WNDFAC, ib4.WTHADJ)
