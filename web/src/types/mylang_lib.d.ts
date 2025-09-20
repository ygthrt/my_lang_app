interface MylangLib {
    parse: (code: string) => string;
    tcheck: (code: string) => string;
    cast: (code: string) => string;
    eval: (code: string) => string;
}

declare const mylang_lib: MylangLib;
