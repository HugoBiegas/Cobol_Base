@echo off
setlocal EnableDelayedExpansion

REM Gestion des arguments
IF "%~1"=="" (
    echo [ERREUR] Aucun fichier specifie.
    echo Usage: %0 fichier.cbl ou fichier.exe
    exit /b 1
)

REM Appel du script de verification d'environnement
call cobol-verify-env.bat
IF ERRORLEVEL 1 (
    echo [ERREUR] La verification de l'environnement a echoue.
    exit /b %errorlevel%
)

echo ================================================
echo Preparation de l'execution...
echo ================================================

REM Traitement du fichier à exécuter
SET "EXE_PATH=%~1"

REM Si c'est un fichier CBL, remplacer l'extension
SET "FILE_EXT=%~x1"
IF /I "!FILE_EXT!"==".cbl" (
    SET "EXE_PATH=!EXE_PATH:.cbl=.exe!"
)

echo Fichier a executer: !EXE_PATH!

IF NOT EXIST "!EXE_PATH!" (
    echo [ERREUR] Le fichier "!EXE_PATH!" n'existe pas.
    exit /b 1
)

echo ================================================
echo Execution du programme...
echo ================================================
docker run -i --rm -v "%cd%:/code" esolang/cobol /code/!EXE_PATH!
echo ================================================
echo Execution terminee.
echo ================================================