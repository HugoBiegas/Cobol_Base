@echo off
setlocal EnableDelayedExpansion

REM Verification des arguments
IF "%~1"=="" (
    echo Usage: %0 fichier.cbl
    echo Le fichier .exe sera cree dans le meme repertoire que le fichier source
    exit /b 1
)

REM Verification du fichier source
IF NOT EXIST "%~1" (
    echo [ERREUR] Le fichier %~1 n'existe pas.
    exit /b 1
)

REM Appel du script de verification d'environnement
call cobol-verify-env.bat
IF ERRORLEVEL 1 (
    echo [ERREUR] La verification de l'environnement a echoue.
    exit /b %errorlevel%
)

echo ================================================
echo Configuration de la compilation...
echo ================================================

REM Construction du chemin de sortie en remplacant l'extension
SET "OUTPUT_FILE=%~1"
SET "OUTPUT_FILE=%OUTPUT_FILE:.cbl=.exe%"

echo Fichier source: %~1
echo Executable de sortie: %OUTPUT_FILE%

echo ================================================
echo Compilation en cours...
echo ================================================

REM Compiler
docker run --rm -v "%cd%:/code" esolang/cobol sh -c "cobc -x -o /code/%OUTPUT_FILE% /code/%~1"
IF ERRORLEVEL 1 (
    echo [ERREUR] Echec de compilation!
    exit /b 1
)

echo ================================================
echo [SUCCES] Programme compile avec succes!
echo Executable cree: %OUTPUT_FILE%
echo ================================================