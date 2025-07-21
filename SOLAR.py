import math

#=======================================================================
#  SOLAR.for contains:
#     SOLAR  - Computes declination, solar time of sunrise and sunset,
#                daily solar constant and cloudiness factor.
#     DAYLEN - Computes solar day length
#     DECL   - Computes solar declination from day length
#=======================================================================
#  SOLAR, Subroutine
#  Computes daily solar constant (Spitters, 1986) and cloudiness factor.
#-----------------------------------------------------------------------
#  Called by: WEATHR
#  Calls:     None
#=======================================================================
#
def SOLAR(DAYL, DEC, SRAD, XLAT):
# !-----------------------------------------------------------------------
#       IMPLICIT NONE
#       REAL AMTRCS,AMTRD,CCOS,CLOUDS,DAYL,DEC,DSINB,ISINB,
#      &  PI,RAD,SCLEAR,S0D,S0N,SC,SOC,SRAD,SRADJ,SSIN,XLAT

    AMTRCS=0.77
    PI = 3.14159
    RAD = PI/180.0
    SC=1368.0

# -----------------------------------------------------------------------
#      Initialization
    SRADJ = SRAD * 1.0E6
#
#      Sun angles.  SOC limited for latitudes above polar circles.
    SSIN = math.sin(RAD*DEC) * math.sin(RAD*XLAT)
    CCOS = math.cos(RAD*DEC) * math.cos(RAD*XLAT)
    SOC = SSIN / CCOS
    SOC = min(max(SOC,-1.0),1.0)
#
#      Integral of SINB over day (sec) (Eqn. 18).
    DSINB = 3600.0 * (DAYL*SSIN + 24.0/PI*CCOS*math.sqrt(1.0-SOC**2))
#
#   Set normal (perp.) extra-terrestial radiation equal to
#   average solar constant (elliptical orbit ignored).
    S0N = SC
#
#   Calculate daily atmospheric transmission from global
#   and normal extra-terrestial radiation.
    S0D = S0N * DSINB
#   Added protection in S0D for division by zero.
    if S0D > 0.0:
        AMTRD = SRADJ / S0D
    else:
        AMTRD = 0.0

#      Calculate clear sky radiation (0.77*S0D) in MJ.
    SCLEAR = AMTRCS * S0D * 1.E-6
#
#   Calculate daily cloudiness factor (range = 0-1).
#   Added protection in SCLEAR for division by zero.
    if SCLEAR > 0:
        CLOUDS = min(max(1.0-SRAD/SCLEAR,0.0),1.0)
    else:
        CLOUDS = 0.0

#   Integral in Eqn. 6 (used in HMET)
    ISINB = 3600.0 * (DAYL*(SSIN+0.4*(SSIN**2+0.5*CCOS**2)) +
        24.0/PI*CCOS*(1.0+1.5*0.4*SSIN)*math.sqrt(1.0-SOC**2))

    return CLOUDS, ISINB, S0N
# C=======================================================================
# !   SOLAR Variables
# !-----------------------------------------------------------------------
# ! AMTRCS Fraction of radiation which reaches surface under clear sky
# !          conditions
# ! AMTRD  Actual fraction of radiation which reaches surface
# ! CCOS   Product of cosines of declination and latitude
# ! CLOUDS Relative cloudiness factor (0-1)
# ! DAYL   Day length on day of simulation (from sunrise to sunset) (hr)
# ! DEC    Solar declination or (90o - solar elevation at noon). Amplitude =
# !          +/- 23.45. Min = Dec. 21, Max = Jun 21/22 (deg.)
# ! DSINB  Integral of SINB over day (s)
# ! ISINB  Integral in Spitter's Equation 6
# ! PI     PI=3.14159 (rad)
# ! RAD    RAD=PI/180. (rad./deg.)
# ! S0D    Daily extraterrestrial irradiance (MJ/m2-d)
# ! S0N    Normal extraterrestrial radiation (set equal to average solar
# !          constant; elliptical orbit ignored) (J/m2-s)
# ! SC     Solar constant (J / m2 - s)
# ! SCLEAR Clear sky radiation (MJ / m2 - d)
# ! SOC    Sine over cosine (intermediate calculation)
# ! SRAD   Solar radiation (MJ/m2-d)
# ! SRADJ  Solar Radiation (J/m2-d)
# ! SSIN   Product of sines of declination and latitude
# ! XLAT   Latitude (deg.)
# C=======================================================================

def DAYLEN(DOY, XLAT):
    """
    Calculate the day length, sunrise time, and sunset time.

    Args:
    DOY : int
        Day of the year (between 1 to 365 or 366).
    XLAT : float
        Latitude in degrees.

    Returns:
    tuple : (DAYL, SNDN, SNUP)
        DAYL: Day length in hours.
        SNDN: Sunset time in hours.
        SNUP: Sunrise time in hours.
    """
    pi = 3.14159
    rad = pi / 180.0

    # Calculate solar declination (dec) in degrees
    dec = -23.45 * math.cos(2.0 * pi * (DOY + 10.0) / 365.0)

    # Calculate the solar angle (soc)
    soc = math.tan(rad * dec) * math.tan(rad * XLAT)
    soc = max(min(soc, 1.0), -1.0)  # Clamp soc between -1 and 1

    # Calculate day length, sunrise, and sunset times
    dayl = 12.0 + 24.0 * math.asin(soc) / pi

    # Ensure day length is between 0 and 24 hours
    dayl = max(min(dayl, 24.0), 0.0)

    # Calculate sunrise and sunset times
    snup = 12.0 - dayl / 2.0
    sndn = 12.0 + dayl / 2.0

    return dayl, sndn, snup


