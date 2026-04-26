(*
===============================================================================
 ZSTD STREAMING DECOMPRESSION + SHA256 VERIFICATION (PRODUCTION UNIT)
===============================================================================

 This unit provides:
   - Streaming decompression using libzstd (no full RAM buffering)
   - Cross-platform dynamic loading (Windows / Linux / macOS)
   - SHA-256 post-decompression verification using OpenSSL (libcrypto)
   - Safe initialization / cleanup lifecycle

===============================================================================
 REQUIRED NATIVE LIBRARIES
===============================================================================

 1) ZSTD (compression engine)
    Official project: https://github.com/facebook/zstd

    Windows:
      - Download prebuilt DLL from Zstd GitHub releases
      - Place: libzstd.dll next to your executable (recommended)

    Linux (Debian/Ubuntu):
      sudo apt install libzstd-dev

    Linux (Fedora):
      sudo dnf install libzstd-devel

    macOS:
      brew install zstd
      (provides libzstd.dylib via Homebrew paths)

-------------------------------------------------------------------------------

 2) OpenSSL (SHA-256 hashing)
    Used for integrity verification of decompressed output.

    Windows:
      - Install OpenSSL 3.x
      - Ensure libcrypto DLL is available:
        e.g. libcrypto-3-x64.dll next to executable

    Linux:
      sudo apt install libssl-dev

    macOS:
      brew install openssl

===============================================================================
 USAGE FLOW
===============================================================================

   1. InitZstd();
   2. InitCrypto();
   3. DecompressZstd(file, outputFolder, expectedHash);
   4. FreeZstd();
   5. FreeCrypto();

===============================================================================
*)

unit utils.zstd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function InitZstd(const ZstdPath: string = ''): Boolean;
procedure FreeZstd;

function DecompressZstd(const ZstdFile, DestFolder: string;
  const ExpectedSHA256: string = ''): Boolean;

implementation

{$IFDEF Windows}
uses Windows,
{$ENDIF}

{$IFDEF Unix}
uses DynLibs,
{$ENDIF}
  utils.crypto;

type
  size_t = NativeUInt;

  ZSTD_inBuffer = record
    src: Pointer;
    size: size_t;
    pos: size_t;
  end;

  ZSTD_outBuffer = record
    dst: Pointer;
    size: size_t;
    pos: size_t;
  end;

  ZSTD_DCtx = Pointer;

  SHA256_CTX = record
    data: array[0..255] of Byte;
  end;

var
  { ================= ZSTD ================= }
  LibZstd: TLibHandle = 0;

  ZSTD_createDCtx: function: ZSTD_DCtx; cdecl;
  ZSTD_freeDCtx: function(dctx: ZSTD_DCtx): size_t; cdecl;
  ZSTD_decompressStream: function(dctx: ZSTD_DCtx;
    output: PZSTD_outBuffer;
    input: PZSTD_inBuffer): size_t; cdecl;
  ZSTD_isError: function(code: size_t): LongBool; cdecl;
  ZSTD_getErrorName: function(code: size_t): PChar; cdecl;
  ZSTD_DStreamInSize: function: size_t; cdecl;
  ZSTD_DStreamOutSize: function: size_t; cdecl;


{ =============================================================================
  ZSTD LOADING
============================================================================= }

function LoadZstdSymbol(const Name: PChar): Pointer;
begin
  Result := GetProcAddress(LibZstd, Name);
  if Result = nil then
    raise Exception.Create('Missing ZSTD symbol: ' + Name);
end;

function InitZstd(const ZstdPath: string = ''): Boolean;
var
  LibName: string;
begin
  Result := False;
  if LibZstd <> 0 then Exit(True);

  if ZstdPath <> '' then
    LibName := ZstdPath
  else
  begin
    {$IFDEF Windows}
    LibName := 'libzstd.dll';
    {$ENDIF}
    {$IFDEF Linux}
    LibName := 'libzstd.so';
    {$ENDIF}
    {$IFDEF Darwin}
    LibName := 'libzstd.dylib';
    {$ENDIF}
  end;

  LibZstd := LoadLibrary(PChar(LibName));
  if LibZstd = 0 then
    raise Exception.Create('Failed to load ZSTD: ' + LibName);

  Pointer(ZSTD_createDCtx) := LoadZstdSymbol('ZSTD_createDCtx');
  Pointer(ZSTD_freeDCtx) := LoadZstdSymbol('ZSTD_freeDCtx');
  Pointer(ZSTD_decompressStream) := LoadZstdSymbol('ZSTD_decompressStream');
  Pointer(ZSTD_isError) := LoadZstdSymbol('ZSTD_isError');
  Pointer(ZSTD_getErrorName) := LoadZstdSymbol('ZSTD_getErrorName');
  Pointer(ZSTD_DStreamInSize) := LoadZstdSymbol('ZSTD_DStreamInSize');
  Pointer(ZSTD_DStreamOutSize) := LoadZstdSymbol('ZSTD_DStreamOutSize');

  Result := True;
end;

procedure FreeZstd;
begin
  if LibZstd <> 0 then
  begin
    UnloadLibrary(LibZstd);
    LibZstd := 0;
  end;
end;


{ =============================================================================
  STREAMING DECOMPRESSION + VERIFICATION
============================================================================= }

function DecompressZstd(const ZstdFile, DestFolder: string;
  const ExpectedSHA256: string = ''): Boolean;
var
  InStream, OutStream: TFileStream;
  InBuf, OutBuf: Pointer;
  InBufSize, OutBufSize: size_t;
  InBuffer: ZSTD_inBuffer;
  OutBuffer: ZSTD_outBuffer;
  Dctx: ZSTD_DCtx;
  Remaining: size_t;
  OutFile, ActualHash: string;
begin
  if LibZstd = 0 then
    raise Exception.Create('ZSTD not initialized');
  if LibCrypto = 0 then
    raise Exception.Create('Crypto not initialized');

  Result := False;

  if not FileExists(ZstdFile) then
    raise Exception.Create('Input file not found');

  if not DirectoryExists(DestFolder) then
    if not CreateDir(DestFolder) then
      raise Exception.Create('Cannot create output folder');

  OutFile := IncludeTrailingPathDelimiter(DestFolder) +
    ChangeFileExt(ExtractFileName(ZstdFile), '');

  InStream := TFileStream.Create(ZstdFile, fmOpenRead);
  OutStream := TFileStream.Create(OutFile, fmCreate);

  Dctx := ZSTD_createDCtx;
  if Dctx = nil then
    raise Exception.Create('Failed to create ZSTD context');

  InBufSize := ZSTD_DStreamInSize;
  OutBufSize := ZSTD_DStreamOutSize;

  GetMem(InBuf, InBufSize);
  GetMem(OutBuf, OutBufSize);

  try
    Remaining := 1;

    while Remaining <> 0 do
    begin
      InBuffer.src := InBuf;
      InBuffer.size := InStream.Read(InBuf^, InBufSize);
      InBuffer.pos := 0;

      if InBuffer.size = 0 then Break;

      while InBuffer.pos < InBuffer.size do
      begin
        OutBuffer.dst := OutBuf;
        OutBuffer.size := OutBufSize;
        OutBuffer.pos := 0;

        Remaining := ZSTD_decompressStream(Dctx, @OutBuffer, @InBuffer);

        if ZSTD_isError(Remaining) then
          raise Exception.Create('ZSTD error: ' +
            StrPas(ZSTD_getErrorName(Remaining)));

        if OutBuffer.pos > 0 then
          OutStream.WriteBuffer(OutBuf^, OutBuffer.pos);
      end;
    end;

    Result := True;

  finally
    FreeMem(InBuf);
    FreeMem(OutBuf);
    ZSTD_freeDCtx(Dctx);
    InStream.Free;
    OutStream.Free;
  end;

  { VERIFY HASH }
  if ExpectedSHA256 <> '' then
  begin
    ActualHash := SHA256File(OutFile);

    if not SameText(ActualHash, ExpectedSHA256) then
      raise Exception.Create(
        'SHA256 mismatch!' + LineEnding +
        'Expected: ' + ExpectedSHA256 + LineEnding +
        'Actual:   ' + ActualHash
      );
  end;
end;

end.
