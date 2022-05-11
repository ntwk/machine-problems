ALL: MP1.EXE

MP1.EXE: MP1.OBJ LIBMP1.LIB LIB291.LIB
        Link /CO MP1,,,LIB291/map+LIBMP1.LIB/map;


MP1.OBJ: MP1.ASM TIME.DTA
        Ml /I. /c /Zi /Fl MP1.asm

CLEAN:
        DEL MP1.OBJ
        DEL MP1.LST
        DEL MP1.EXE
        DEL MP1.MAP
