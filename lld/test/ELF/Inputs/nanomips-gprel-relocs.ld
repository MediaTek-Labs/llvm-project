SECTIONS
{   
    . = 0x1000;
    .text : {
        *(.text)
    }

    . = 0x2000;
    _gp = .;
    .sdata : {
        *(.sdata)
    }

    .nanoMIPS.abiflags : {
        *(.nanoMIPS.abiflags)
    }
}