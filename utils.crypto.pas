unit utils.crypto;

interface

uses
  Classes, SysUtils;

function InitCrypto(const CryptoPath: string = ''): Boolean;
procedure FreeCrypto;
function SHA256File(const FileName: string): string;

implementation

var
  { ================= CRYPTO ================= }
  LibCrypto: TLibHandle = 0;

  SHA256_Init: function(ctx: PSHA256_CTX): Integer; cdecl;
  SHA256_Update: function(ctx: PSHA256_CTX; data: Pointer; len: size_t): Integer; cdecl;
  SHA256_Final: function(md: PByte; ctx: PSHA256_CTX): Integer; cdecl;


{ =============================================================================
  CRYPTO LOADING
============================================================================= }

function LoadCryptoSymbol(const Name: PChar): Pointer;
begin
  Result := GetProcAddress(LibCrypto, Name);
  if Result = nil then
    raise Exception.Create('Missing CRYPTO symbol: ' + Name);
end;


function InitCrypto(const CryptoPath: string = ''): Boolean;
var
  LibName: string;
begin
  Result := False;
  if LibCrypto <> 0 then Exit(True);

  if CryptoPath <> '' then
    LibName := CryptoPath
  else
  begin
    {$IFDEF Windows}
    LibName := 'libcrypto-3-x64.dll';
    {$ENDIF}
    {$IFDEF Linux}
    LibName := 'libcrypto.so';
    {$ENDIF}
    {$IFDEF Darwin}
    LibName := 'libcrypto.dylib';
    {$ENDIF}
  end;

  LibCrypto := LoadLibrary(PChar(LibName));
  if LibCrypto = 0 then
    raise Exception.Create('Failed to load crypto: ' + LibName);

  Pointer(SHA256_Init) := LoadCryptoSymbol('SHA256_Init');
  Pointer(SHA256_Update) := LoadCryptoSymbol('SHA256_Update');
  Pointer(SHA256_Final) := LoadCryptoSymbol('SHA256_Final');

  Result := True;
end;

procedure FreeCrypto;
begin
  if LibCrypto <> 0 then
  begin
    UnloadLibrary(LibCrypto);
    LibCrypto := 0;
  end;
end;

{ =============================================================================
  SHA256 FILE HASH
============================================================================= }

function SHA256File(const FileName: string): string;
var
  FS: TFileStream;
  Buffer: array[0..8191] of Byte;
  ReadBytes: Integer;
  Ctx: SHA256_CTX;
  Digest: array[0..31] of Byte;
  i: Integer;
begin
  if LibCrypto = 0 then
    raise Exception.Create('Crypto not initialized');

  FS := TFileStream.Create(FileName, fmOpenRead);
  try
    SHA256_Init(@Ctx);

    repeat
      ReadBytes := FS.Read(Buffer, SizeOf(Buffer));
      if ReadBytes > 0 then
        SHA256_Update(@Ctx, @Buffer, ReadBytes);
    until ReadBytes = 0;

    SHA256_Final(@Digest, @Ctx);

    Result := '';
    for i := 0 to 31 do
      Result := Result + LowerCase(IntToHex(Digest[i], 2));

  finally
    FS.Free;
  end;
end;

end.