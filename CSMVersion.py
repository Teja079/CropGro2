from dataclasses import dataclass

# Global CSM Version Number
@dataclass
class VersionType:
    Major : int = 1
    Minor : int = 0
    Model : int = 0
    Build : int = 0

Version = VersionType()

VBranch = 1
