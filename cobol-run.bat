@echo off
setlocal EnableDelayedExpansion

REM Verification si un fichier a ete specifie
if "%~1"=="" (
    echo [ERREUR] Aucun fichier COBOL specifie.
    echo Usage: %0 fichier.cbl
    exit /b 1
)

REM Verification si le fichier existe
if not exist "%~1" (
    echo [ERREUR] Le fichier "%~1" n'existe pas.
    exit /b 1
)

REM Appel du script de verification d'environnement
call cobol-verify-env.bat
IF ERRORLEVEL 1 (
    echo [ERREUR] La verification de l'environnement a echoue.
    exit /b %errorlevel%
)

REM Construction du chemin de sortie en remplacant l'extension
SET "OUTPUT_FILE=%~1"
SET "OUTPUT_FILE=%OUTPUT_FILE:.cbl=.exe%"

echo ================================================
echo Compilation de %1...
echo ================================================
echo Fichier source: %~1
echo Executable de sortie: %OUTPUT_FILE%

docker run --rm -v "%cd%:/code" esolang/cobol sh -c "cobc -x -o /code/%OUTPUT_FILE% /code/%~1"
if %errorlevel% neq 0 (
    echo [ERREUR] La compilation a echoue!
    exit /b 1
)
echo [SUCCES] Compilation reussie!

echo ================================================
echo Execution du programme...
echo ================================================
docker run -i --rm -v "%cd%:/code" esolang/cobol /code/%OUTPUT_FILE%
echo ================================================
echo Execution terminee.
echo ================================================