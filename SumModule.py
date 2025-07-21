from dataclasses import dataclass, field
from typing import List
from ModuleDefs import EvaluateNum

@dataclass
class SummaryType:
    # Summary.OUT data
    ADAT: int = 0; EDAT: int = 0; MDAT: int = 0; DWAP: int = 0; CWAM: int = 0
    HWAM: int = 0
    HNUMAM: int = 0; NFXM: int = 0; NUCM: int = 0; CNAM: int = 0; GNAM: int = 0
    IRNUM: int = 0; IRCM: int = 0; ETCM: int = 0
    PRCM: int = 0; ROCM: int = 0; DRCM: int = 0; SWXM: int = 0
    NINUMM: int = 0; NICM: int = 0; NLCM: int = 0; NIAM: int = 0; RECM: int = 0; ONAM: int = 0; OCAM: int = 0
    PINUMM: int = 0; PICM: int = 0; PUPC: int = 0; SPAM: int = 0
    KINUMM: int = 0; KICM: int = 0; KUPC: int = 0; SKAM: int = 0
    HWAH: float = 0.0; HWUM: float = 0.0; BWAH: float = 0.0; HNUMUM: float = 0.0

    # Added 2/6/2005 for v4.0.2.0
    LAIX: float = 0.0; HIAM: float = 0.0
    PWAM: int = 0; EPCM: int = 0; ESCM: int = 0

    # Added 12/12/2005 Organic Matter
    OCTAM: int = 0; ONTAM: int = 0; OPAM: int = 0; OPTAM: int = 0

    # Added 06/27/2007 Water productivity
    DMPEM: float = 0.0; DMPPM: float = 0.0; DMPTM: float = 0.0
    YPEM: float = 0.0; YPPM: float = 0.0; YPTM: float = 0.0

    # Added 01/07/2010 Irrigation productivity
    DMPIM: float = 0.0; YPIM: float = 0.0

    # Added 01/27/2010 N productivity
    DPNAM: float = 0.0; DPNUM: float = 0.0; YPNAM: float = 0.0; YPNUM: float = 0.0

    # Added 02/23/2011 Seasonal average environmental data
    NDCH: int = 0
    TMINA: float = 0.0; TMAXA: float = 0.0; SRADA: float = 0.0; DAYLA: float = 0.0
    CO2A: float = 0.0; PRCP: float = 0.0; ETCP: float = 0.0; ESCP: float = 0.0; EPCP: float = 0.0

    # Added 7/19/2016 N2O emissions
    N2OEM: float = 0.0  # kg/ha
    CO2EM: float = 0.0
    CH4EM: float = 0.0  # kg[C]/ha chp 2021-07-28

    # Added 2019-19-17 CHP Cumulative net mineralization
    NMINC: float = 0.0

    # Added 05/28/2021 Latitude, Longitude and elevation
    XCRD: float = 0.0; YCRD: float = 0.0; ELEV: float = 0.0

    # Added 2021-04-14 CHP End of season crop status
    CRST: int = 0

    # Added 2021-20-04 LPM Fresh weight variables
    FCWAM: int = 0; FHWAM: int = 0; FPWAM: int = 0
    HWAHF: float = 0.0; FBWAH: float = 0.0

    # Added 2024-06-20 FO Economic Yield
    EYLDH: float = 0.0


@dataclass
class EvaluateType:
    ICOUNT: int = 0
    OLAP: List[str] = field(default_factory=lambda: [''] * EvaluateNum)
    Simulated: List[str] = field(default_factory=lambda: [''] * EvaluateNum)
    Measured: List[str] = field(default_factory=lambda: [''] * EvaluateNum)
    DESCRIP: List[str] = field(default_factory=lambda: [''] * EvaluateNum)


# Module-level equivalents
SUMDAT = SummaryType()
EvaluateData = EvaluateType()