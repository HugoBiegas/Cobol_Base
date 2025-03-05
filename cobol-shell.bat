@echo off
setlocal EnableDelayedExpansion

REM Appel du script de verification d'environnement
call cobol-verify-env.bat
IF ERRORLEVEL 1 (
    echo [ERREUR] La verification de l'environnement a echoue.
    exit /b %errorlevel%
)

echo ================================================
echo Demarrage du shell COBOL...
echo ================================================
echo Tapez 'exit' pour quitter le shell.
echo.
echo Commandes utiles:
echo - cobc -x fichier.cbl     : Compiler un programme
echo - cobol fichier.cbl       : Executer directement un programme
echo - ls /code                : Lister les fichiers de votre dossier
echo ================================================
echo.

docker run -it --rm -v "%cd%:/code" esolang/cobol sh