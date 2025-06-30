clear 
@echo off
rem del dsp1.o
rem del sample1.o

rem rmac -fb -u dsp1.s
rem rln -o dsp1.abs -w -rq -a 4000 x x dsp1.o



rem rmac -fb -u "dsp1 - replay sample au timer 1.s"
rem rln -o sample1.abs -w -rq -a 4000 x x "dsp1 - replay sample au timer 1.o"

@echo off
rem del console1.o
rem rmac -fb -s -u console1.s 
rem rln -o console1.abs -w -rq -a 4000 x x console1.o -m 


@echo off
rem del ym1.o
rem rmac -fb -s -u ym1.s 
rem rln -o ym1.abs -w -rq -a 4000 x x ym1.o -m 
rem C:\Jaguar\Emulateur\jiffi_1.4\JiFFI.Exe -i c:\jaguar\ym1.abs -o c:\jaguar -rom -j64 -overwrite

@echo off
rem del ym2.o
rem rmac -fb -s -u ym2.s 
rem rln -o ym2.abs -w -rq -a 4000 x x ym2.o -m 

@echo off
rem del lsp.o
rem copy lsp.s backups\lsptmp.s
rem cd backups
rem ren "lsptmp.s" "lsp_ %date:/=-% %time::=-%.s"
rem cd ..
rem rmac -fb -s -u lsp.s 
rem rln -o lsp.abs -w -rq -a 4000 x x lsp.o -m 
rem C:\Jaguar\Emulateur\jiffi_1.4\JiFFI.Exe -i c:\jaguar\lsp.abs -o c:\jaguar -rom -j64 -overwrite

@echo on
del %1.o
copy %1.s backups\%1.s
cd backups
ren %1.s "%1_ %date:/=-% %time::=-%.s"
cd ..
rmac -fb -s -u %1.s 
if %ERRORLEVEL% NEQ 0 exit 0
rln   -o %1.abs -w -rq -a 4000 x x %1.o -m -z
if %ERRORLEVEL% NEQ 0 exit 0
rln   -o %1.bin -w -rq -a 4000 x x %1.o -m -n -z
if %ERRORLEVEL% NEQ 0 exit 0

copy %1.bin edz.bin /y

rem loader
rmac -fb -s -u loader_rom_%1.s
if %ERRORLEVEL% NEQ 0 exit 0
rln  -o %1_edz.rom -w -rq -a 800000 x x loader_rom_%1.o -m -n -z
if %ERRORLEVEL% NEQ 0 exit 0

@echo off
rem C:\Jaguar\Emulateur\jiffi_1.4\JiFFI.Exe -i c:\jaguar\%1.abs -o c:\jaguar -rom -j64 -overwrite

echo -------------------- ALL OK -------------------- 

rem cp "c:\jaguar\hively_v1_edz.rom" "c:\jaguar\_____\hively1.rom"

