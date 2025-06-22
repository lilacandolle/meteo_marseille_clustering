@echo on
CALL conda activate base
setlocal enabledelayedexpansion

set "files="

cd totalprecipitation

cd tmp
for %%f in (rr*.nc) do (
    set "files=!files! %%f"
)

echo Fichiers à concaténer : %files%

ncrcat %files% totalprecipitation_20162022.nc

pause
