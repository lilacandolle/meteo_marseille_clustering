@echo on
CALL conda activate base

:: Aller dans le dossier contenant les fichiers
cd totalprecipitation

:: Créer un dossier temporaire pour les fichiers avec dimension record
mkdir tmp

:: Ajouter la dimension record sur tous les fichiers
for %%f in (*.nc) do (
    echo Traitement de %%f...
    ncks --mk_rec_dmn valid_time "%%f" "tmp\%%f"
)


echo Fini !
pause
