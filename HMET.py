import math

import numpy as np


#=================================================================
#  HMET.for contains:
#     HMET  - Calculates hourly values of temperature, PAR, solar
#             radiation, fraction diffuse radiation, and relative
#             humidity.
#     HANG  - Computes solar elevation and azimuth
#     HTEMP - Computes temperature for hour-of-day
#     HRAD  - Computes total hourly solar radiation
#     FRACD - Computes hourly fraction diffuse total radiation and PAR.
#     HPAR  - Computes hourly PAR
#     VPSAT - Calculates saturated vapor pressure of air
#     VPSLOP- Calculates slope of saturated vapor pressure versus
#             temperature curve
#=================================================================
#  HMET, Subroutine
#  Calculates hourly values of temperature, PAR, solar radiation,
#  fraction diffuse radiation, and relative humidity,
#-----------------------------------------------------------------------
#  Called by: WEATHR
#  Calls:     HANG, HTEMP, HRAD, FRACD, HPAR
#=======================================================================
#
def HMET(CLOUDS, DAYL, DEC, ISINB, PAR, REFHT,SNDN, SNUP, S0N, SRAD, TDEW, TMAX,TMIN, WINDHT, WINDSP, XLAT):
#
# !-----------------------------------------------------------------------
    from ModuleDefs import TS
    import numpy as np
#       IMPLICIT NONE
#       EXTERNAL HANG, HTEMP, VPSAT, , HRAD, FRACD, HPAR
#       INTEGER H,NDAY
#
    AZZON = np.zeros(TS, dtype=float)
    BETA = np.zeros(TS, dtype=float)
    TAIRHR = np.zeros(TS, dtype=float)
    RHUMHR = np.zeros(TS, dtype=float)
    WINDHR = np.zeros(TS, dtype=float)
    RADHR = np.zeros(TS, dtype=float)
    AMTRH = np.zeros(TS, dtype=float)
    FRDIFP = np.zeros(TS, dtype=float)
    FRDIFR = np.zeros(TS, dtype=float)
    PARHR = np.zeros(TS, dtype=float)
    TGRO = np.zeros(TS, dtype=float)

#       REAL CLOUDS, DAYL, DEC,
#      &  HS,ISINB,PAR,REFHT,S0N,SRAD,SNDN,SNUP,
#      &  TAVG,TDAY,TDEW,TGROAV,TGRODY,TINCR,TMAX,TMIN,
#      &  RH,VPSAT,WINDAV,WINDHT,WINDSP,
#      &  XLAT

    TINCR=24./TS
#
#-----------------------------------------------------------------------
#     Initialize
    TAVG = 0.0
    TDAY = 0.0
    NDAY = 0
    WINDAV = WINDSP / 86.4 * (REFHT/WINDHT)**0.2

#   Loop to compute hourly weather data.
    for H in range (1,TS +1):

#   Calculate real and solar time.
        HS = float(H) * TINCR

#   Calculate sun angles and hourly weather variables.
        AZZON[H], BETA[H] = HANG(DEC, HS, XLAT)

        TAIRHR[H] = HTEMP(DAYL, HS, SNDN, SNUP, TMAX, TMIN)

        RH = VPSAT(TDEW) / VPSAT(TAIRHR[H]) * 100.0
        RHUMHR[H] = min(RH,100.0)

        WINDHR[H] = HWIND(DAYL, HS, SNDN, SNUP, WINDAV)

        RADHR[H] = HRAD(BETA[H], HS, ISINB, SNDN, SNUP, SRAD)

        AMTRH[H], FRDIFP[H], FRDIFR[H] = FRACD(BETA[H], CLOUDS, HS, RADHR[H], S0N, SNDN, SNUP)

        PARHR[H] = HPAR(HS, PAR, RADHR[H], SNDN, SNUP, SRAD)

        TAVG = TAVG + TAIRHR(H)

        if H > SNUP and H <= SNDN:
          TDAY = TDAY + TAIRHR(H)
          NDAY = NDAY + 1


    TAVG = TAVG / float(TS)
    TDAY = TDAY / float(NDAY)

    TGRODY = TDAY
    TGROAV = TAVG
    for H in range(1,TS+1):
        TGRO[H] = TAIRHR[H]

    if PAR <= 0.:
        PAR = 2.0 * SRAD

    return  (AMTRH, AZZON, BETA, FRDIFP, FRDIFR, PARHR,RADHR, RHUMHR, TAIRHR,
             TAVG, TDAY, TGRO,TGROAV, TGRODY, WINDHR)
# 
#=======================================================================
#     HMET variable definitions
#-----------------------------------------------------------------------
# AZZON(TS)  Hourly solar azimuth (+/- from South) (deg.)
# BETA(TS)   Hourly solar elevation (+/- from horizontal) (deg.)
# CLOUDS     Relative cloudiness factor (0-1)
# DAYL       Day length on day of simulation (from sunrise to sunset) (hr)
# DEC        Solar declination or (90o - solar elevation at noon).
#              Amplitude = +/- 23.45. Min = Dec. 21, Max = Jun 21/22 (deg.)
# FRDIFP(TS) Hourly fraction diffuse photon flux density after correcting
#              for circumsolar radiation (Spitters, 1986)
# FRDIFR(TS) Hourly fraction diffuse solar radiation after correcting for
#              circumsolar radiation (Spitters, 1986)
# H          Internal hourly counter (hr)
# HS         Internal hourly counter (hour)
# ISINB      Integral in Spitter's Equation 6
# NDAY       Number of daylight hours (hr)
# PAR        Daily photosynthetically active radiation or photon flux
#              density (moles[quanta]/m2-d)
# PARHR(TS)  hourly PAR (J / m2 - s)
# RADHR(TS)  Total hourly solar radiation (J/m2-s)
# REFHT      Reference height for wind speed (m)
# RH         Relative humidity (%)
# RHUMHR(TS) Relative humidity hourly value (%)
# S0N        Normal extraterrestrial radiation (set equal to average solar
#              constant; elliptical orbit ignored) (J/m2-s)
# SNDN       Time of sunset (hr)
# SNUP       Time of sunrise (hr)
# SRAD       Solar radiation (MJ/m2-d)
# TAIRHR(TS) Hourly air temperature (in some routines called TGRO) (°C)
# TAVG       Average daily temperature (°C)
# TDAY       Average temperature during daylight hours (°C)
# TDEW       Dewpoint temperature (°C)
# TGRO(I)    Hourly air temperature (°C)
# TGROAV     Average daily canopy temperature (°C)
# TGRODY     Average temperature during daylight hours (°C)
# TINCR      Time increment (hr)
# TMAX       Maximum daily temperature (°C)
# TMIN       Minimum daily temperature (°C)
# TS         Number of intermediate time steps (=24)
# VPSAT      Saturated vapor pressure of air (Pa)
# WINDAV     Average wind speed (m/s)
# WINDHR(TS) Hourly wind speed (m/s)
# WINDHT     Reference height for wind speed (m)
# WINDSP     Wind speed (km/d)
# XLAT       Latitude (deg.)
#=======================================================================

def HTEMP(DAYL, HS, SNDN, SNUP, TMAX, TMIN):
    """
    Calculate hourly air temperature.

    Args:
    DAYL : float
        Day length in hours.
    HS : float
        Hour of the day for which temperature is calculated.
    SNDN : float
        Sunset time in hours.
    SNUP : float
        Sunrise time in hours.
    TMAX : float
        Maximum temperature of the day.
    TMIN : float
        Minimum temperature of the day.

    Returns:
    float
        Hourly air temperature (TAIHR) at the specified hour.
    """
    # Constants based on Parton and Logan
    a = 2.0
    b = 2.2
    c = 1.0
    pi = 3.14159

    # Calculate minimum temperature time and maximum temperature time
    min_time = SNUP + c
    max_time = min_time + DAYL / 2.0 + a

    # Temperature at sunset and minimum temperature at infinite time
    t = 0.5 * pi * (SNDN - min_time) / (max_time - min_time)
    tsndn = TMIN + (TMAX - TMIN) * math.sin(t)
    tmini = (TMIN - tsndn * math.exp(-b)) / (1.0 - math.exp(-b))
    hdecay = 24.0 + c - DAYL

    # Calculate temperature based on daylight or nighttime hours
    if SNUP + c <= HS <= SNDN:
        # Daylight hours, sine curve calculation
        t = 0.5 * pi * (HS - min_time) / (max_time - min_time)
        TAIRHR = TMIN + (TMAX - TMIN) * math.sin(t)
    else:
        # Nighttime, exponential decrease
        if HS < SNUP + c:
            t = 24.0 + HS - SNDN
        else:
            t = HS - SNDN
        arg = -b * t / hdecay
        TAIRHR = tmini + (tsndn - tmini) * math.exp(arg)
    return TAIRHR

def tgro_gen(dayl, sndn, snup, tmax, tmin):
    tgro = 24*[None]
    for i in range(0,24):
        hs = i+1 # Hour of day
        tgro[i] = HTEMP(dayl, hs, sndn, snup, tmax, tmin)
    return tgro

#=======================================================================
#  HANG, Subroutine,
#  Computes solar elevation and azimuth for time-of-day (deg).
#-----------------------------------------------------------------------
#  Called by: HMET
#  Calls:     None
#-----------------------------------------------------------------------
#  Input : HS,DEC,XLAT
#  Output: AZZON,BETA
#  Local : PI,RAD,SINAS,SINB,SOLTIM
#=======================================================================
def HANG(DEC, HS, XLAT):
# !-----------------------------------------------------------------------
#       IMPLICIT NONE
#
#       REAL AZZON,BETA,CCOS,HANGL,HS,DEC,PI,RAD,SINAS,SSIN,XLAT,BETAS
    PI=3.14159
    RAD=PI/180.
#
# !-----------------------------------------------------------------------
#      Sines and cosines and their products.
#
    SSIN = math.sin(RAD*DEC) * math.sin(RAD*XLAT)
    CCOS = math.cos(RAD*DEC) * math.cos(RAD*XLAT)
    HANGL = (HS-12.0)*PI/12.0

# Calculate solar elevation and azimuth.
    BETAS = min(max(SSIN + CCOS*math.cos(HANGL),-1.0),1.0)
    BETA  = math.asin(BETAS)
    SINAS = math.cos(RAD*DEC) * math.sin(HANGL) / math.cos(BETA)
    SINAS = min(max(SINAS,-1.0),1.0)
    AZZON = math.asin(SINAS)
    AZZON = AZZON / RAD
    BETA = BETA / RAD

    if BETA > 1.E-4:
        BETA = max(BETA, 1.0)

    return AZZON, BETA

#=======================================================================
#  VPSAT, Real Function,
#  Calculates saturated vapor pressure of air (Tetens, 1930).
#-----------------------------------------------------------------------
#  Called by: CANPET, HMET, VPSLOP, PETPEN
#  Calls:     None
#-----------------------------------------------------------------------
#  Input : T (C)
#  Output: VPSAT (Pa)
#=======================================================================
#
def VPSAT(T):
#
#       IMPLICIT NONE
#       REAL T
#
    VPSAT = 610.78 * math.exp(17.269*T/(T+237.30))
#
    return VPSAT
#=======================================================================
# VPSAT Variables
#-----------------------------------------------------------------------
# T     Air temperature (oC)
# VPSAT Saturated vapor pressure of air (Pa)
#=======================================================================

#=======================================================================
#  HWIND, Subroutine,
#  Computes wind speed for hour-of-day using speed at night
#  (10% of max.) plus full sine in the day (90% of max.).
#-----------------------------------------------------------------------
#  Called by: HMET
#  Calls:     None
#=======================================================================
def HWIND(DAYL, HS, SNDN, SNUP, WINDAV):

#-----------------------------------------------------------------------
      # IMPLICIT NONE
      # REAL DAYL,HS,PI,SNDN,SNUP,T,WNIGHT,WINDAV,WINDHR,WMAX
    PI = 3.14159
#-----------------------------------------------------------------------

#     Set base wind speed as proportion of maximum daily wind speed then
#     compute the maximum wind speed from the average daily wind speed.

    WNIGHT = 0.3
    WMAX = WINDAV / (WNIGHT+(1.0-WNIGHT)*DAYL/48.0)

#     Day time.
    if HS > SNUP and HS < SNDN:
        T = 2.0*PI*(HS-SNUP)/(SNDN-SNUP) + 1.5*PI
        WINDHR = WMAX*WNIGHT + (1.0-WNIGHT) * WMAX/2.0 * (1.0+math.sin(T))

#     Night time.
    else:
        WINDHR = WMAX*WNIGHT

    return WINDHR
#=======================================================================
# HWIND Variables
#-----------------------------------------------------------------------
# DAYL   Day length on day of simulation (from sunrise to sunset) (hr)
# HS     Internal hourly counter (hour)
# SNDN   Time of sunset (hr)
# SNUP   Time of sunrise  (hr)
# T      Time factor for hourly calculations
# WNIGHT Night time wind speed set at 10% of WMAX (m/s)
# WMAX   Maximum wind speed (m/s)
# WINDAV Average wind speed (m/s)
# WINDHR Hourly wind speed (m/s)
# WINDHT Reference height for windspeed (m)
# WINDSP Windspeed (km/d)
#=======================================================================

#=======================================================================
#  HRAD, Subroutine
#  Computes total hourly solar radiation (Spitters, 1986).
#-----------------------------------------------------------------------
#  Called by: HMET
#  Calls:     None
#=======================================================================

def HRAD(BETA, HS, ISINB, SNDN, SNUP, SRAD):

#-----------------------------------------------------------------------
      # IMPLICIT NONE
      # REAL BETA,HS,ISINB,PI,RAD,RADHR,SINB,SNDN,SNUP,SRAD
    PI=3.14159
    RAD=PI/180.0

#-----------------------------------------------------------------------
#     Daylight hour calculations
    if HS > SNUP and HS < SNDN:

#       Initialization
        SINB = math.sin(RAD*BETA)

#       Compute instantaneous SRAD values with Spitters' Eqn. 6.
        RADHR = SINB * (1.0+0.4*SINB) * SRAD*1.0E6 / ISINB

    else:
        RADHR = 0.0

    return RADHR

#=======================================================================
# HRAD Variables
#-----------------------------------------------------------------------
# BETA(TS)  Hourly solar elevation (+/- from horizontal) (deg.)
# HS        Internal hourly counter (hour)
# ISINB     Integral in Spitter's Equation 6
# PI        PI=3.14159 (rad)
# RAD       RAD=PI/180. (rad./deg.)
# RADHR(TS) Total hourly solar radiation (J/m2-s)
# SINB      Sine of Beta (hourly solar elevation)
# SNDN      Time of sunset (hr)
# SNUP      Time of sunrise (hr)
# SRAD      Solar radiation (MJ/m2-d)
#=======================================================================

#=======================================================================
#  FRACD, Subroutine
#  Computes hourly fraction diffuse total radiation and PAR.
#-----------------------------------------------------------------------
#  Called by: HMET
#  Calls:     None
#=======================================================================
def FRACD(BETA, CLOUDS, HS, RADHR, S0N, SNDN, SNUP):

# !-----------------------------------------------------------------------
#       IMPLICIT NONE
#       REAL AMTRH,BETA,CLOUDS,CORR,COS90B,COSB,FRDFH,FRDIFP,
#      &  FRDIFR,HS,PI,RAD,RADHR,S0,S0N,SDF,SINB,SNDN,SNUP
#
    PI = 3.14159
    RAD = PI/180.0
#
#-----------------------------------------------------------------------
#     Daylight hour calculations
    if HS > SNUP and HS < SNDN:
#       Initialization
        SINB = math.sin(RAD*BETA)
        COSB = math.cos(RAD*BETA)
        COS90B = math.cos(RAD*(90.0-BETA))

#       Calculate instantaneous atmospheric transmission from global
#       and extra-terrestial radiation.

        S0 = S0N * SINB
        AMTRH = RADHR / S0

# C       Calculate hourly fraction diffuse from hourly atmospheric
# C       transmission (after Erbs, Klein and Duffie, 1982; De Jong,
# C       1980).  Lower end adjusted to give smooth transition.
#
# !        IF (AMTRH .LE. 0.22) THEN
# !          FRDFH = 1.0
# !        ELSE IF (AMTRH .GT. 0.22 .AND. AMTRH .LE. 0.5) THEN
# !          FRDFH = 1.0 - 4.3*(AMTRH-0.22)**2
# !        ELSE
# !          FRDFH = 0.1 + 0.563*EXP(-5.5*(AMTRH-0.5))
# !        ENDIF
#
# ! JIL 09/11/2007 Using single logistic equation instead of discontinous function
# ! FO  06/05/2022 Added protection for overflow in the EXP() function.
# !                Overflow happens when AMTRH is greater than 64.
# !                This is possible for extreme latitudes that has
# !                low day length periods. IF AMTRH is greater than 5.0
# !                FRDFH will be always equal to 0.156. AMTRH lower than
# !                5.0, it will reach a maximum of 1.016.
        if AMTRH < 5.0:
            FRDFH = 0.156 + (0.86/(1.0+math.exp(11.1*(AMTRH - 0.53))))
            FRDFH = min(FRDFH,1.0)
        else:
            FRDFH = 0.156

#        Calculate instantaneous diffuse global irradiance.
        SDF = RADHR * FRDFH

#        Decrease sdf by circumsolar part of diffuse radiation.
#          Spitters Eqn. 9
        CORR =  (1.0-CLOUDS) * COS90B**2 * COSB**3
        SDF = SDF / (1.0 + CORR)

#        Calculate intantaneous fraction diffuse of global and par.
        if RADHR > 0.0:
            FRDIFR = SDF / RADHR
        else:
            FRDIFR = 0.0

#      Spitters Eqn. 10.
        FRDIFP = (1.0+0.3*(1.0-CLOUDS)) * FRDIFR
        FRDIFR = min(max(FRDIFR,0.0),1.0)
        FRDIFP = min(max(FRDIFP,0.0),1.0)

    else:
        FRDIFP = 1.0
        FRDIFR = 1.0

    return AMTRH, FRDIFP, FRDIFR
#=======================================================================
# FRACD Variables
#-----------------------------------------------------------------------
# AMTRH      Hourly atmospheric transmission coefficient or ratio of solar:
#              extraterrestrial radiation
# BETA(TS)   Hourly solar elevation (+/- from horizontal) (deg.)
# CLOUDS     Relative cloudiness factor (0-1)
# CORR       Correction to decrease diffuse solar radiation by the
#              circumsolar component, since circumsolar diffuse radiation
#              acts like direct radiation (Spitters, 1986)
# COS90B     Cosine of (90° - BETA)
# COSB       Cosine of BETA
# FRDFH      Hourly fraction diffuse radiation before correcting for
#              circumsolar radiation (Spitters, 1986)
# FRDIFP(TS) Hourly fraction diffuse photon flux density after correcting
#              for circumsolar radiation (Spitters, 1986)
# FRDIFR(TS) Hourly fraction diffuse solar radiation after correcting for
#              circumsolar radiation (Spitters, 1986)
# HS         Internal hourly counter (hour)
# PI         PI=3.14159 (rad)
# RAD        RAD=PI/180. (rad./deg.)
# RADHR(TS)  Total hourly solar radiation (J/m2-s)
# S0         Extraterrestrial irradiance on a plant parallel to earth
#              surface (J / m2-s)
# S0N        Normal extraterrestrial radiation (set equal to average solar
#              constant; elliptical orbit ignored) (J/m2-s)
# SDF        Instantaneous diffuse global irradiance (J / m2 - s)
# SINB       Sine of Beta (hourly solar elevation)
# SNDN       Time of sunset (hr)
# SNUP       Time of sunrise (hr)
#=======================================================================

#=======================================================================
#  HPAR, Subroutine
#  Computes hourly PAR using Spitters (1986) or Efimova (1967)
#  in Pearcy et al (1991).
#-----------------------------------------------------------------------
#  Called by: HMET
#  Calls:     None
#-----------------------------------------------------------------------
#  Input : BETA,FRDIFR,HS,ISINB,PAR,SNDN,SNUP,RADHR
#  Output: PARHR
#  Local : CONST,DAYL,PARFAC,PARQC,PI,RAD,SINB,SOLTIM
#=======================================================================
def HPAR(HS, PAR, RADHR, SNDN, SNUP, SRAD):
# !-----------------------------------------------------------------------
#       IMPLICIT NONE
#       REAL HS,PAR,PARFAC,PARHR,PARQC,RADHR,SNDN,SNUP,SRAD
    PARQC=4.6
#-----------------------------------------------------------------------
#     Daylight hour calculations
    if HS > SNUP and HS < SNDN:

#        Compute instantaneous PAR using same equation as HRAD.  If PAR not
#        available, compute from RADHR and FRDIFR.  Uses PARQC=4.6 photon/J
#        and Eqn. 6.2 in Pearcy et al (1991).  PARFAC=2.1-2.6 (sunny-cloudy).
#   10/02/2007 JIL     For this to work units should be:
#              SRAD (MJ/m2 d)
#              PAR (mol/m2 d)
#              RADHR (J/m2 s)
#              PARHR (umol/m2 s)
#              PARQC = 4.6 umol/J

        if PAR > 1.E-4:
            PARHR = RADHR * PAR/SRAD
        else:
#           PARFAC = (0.43*(1.0-FRDIFR)+0.57*FRDIFR) * PARQC
#           PARFAC = 2.0
#
#   10/02/2007 JIL   According to Lizaso et al. (2003)
            PARFAC = (0.43 + 0.12*math.exp(-SRAD/2.8)) * PARQC
            PARHR = RADHR * PARFAC

#      Night time.
    else:
        PARHR = 0.0

    return PARHR
#
# C=======================================================================
# ! HPAR Variables
# !-----------------------------------------------------------------------
# ! HS        Internal hourly counter (hour)
# ! PAR       Daily photosynthetically active radiation or photon flux
# !           density, calculated as half the solar radiation
# !           (moles[quanta]/m2-d)
# ! PARFAC    Factor used to compute hourly PAR from hourly solar radiation
# ! PARHR(TS) hourly PAR (J / m2 - s) Units should be umol/m2 s ....JIL
# ! RADHR(TS) Total hourly solar radiation (J/m2-s)
# ! SNDN      Time of sunset (hr)
# ! SNUP      Time of sunrise  (hr)
# ! SRAD      Solar Radiation  (MJ/m2-d)
# C=======================================================================

# =======================================================================
#   TDEW, Real Function
#   Calculates dew point temperature
# -----------------------------------------------------------------------
#   Called by: IPWEATHR
# -----------------------------------------------------------------------
#   Input : Tmin, Tmax (C), RHUM (%)
#   Output: TDEW
# =======================================================================
def CALC_TDEW(TMIN, RHUM):
    import math
    a = 0.61078
    b = 17.269
    c = 237.3

    es = a * math.exp(b * TMIN / (TMIN + c))
    ea = RHUM/100. * es
    Td = (c * math.log(ea/a)) / (b - math.log(ea/a))
    return Td
# =======================================================================
#  TDEW variables
# -----------------------------------------------------------------------
#  TMIN        Min daily temperature (oC)
#  TMAX        Max daily temperature (oC)
#  CALC_TDEW   Dew point temperature (oC)
#  RHUM        Relative humidity (%)
#  ea          Ambient vapour pressure (kPa)
#  es          Saturated vapour pressure (kPa)
#  a           Air pressure (kPa)
#  b           ??? some sort of empirical constant
#  c           ??? (oC)
# =======================================================================