unit utils.cryptstream;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils;

function EncryptBlock(sIn, sOut : TStream; const aEncryptKey : string):int64;
procedure DecryptBlock(sIn, sOut : TStream; BlkSz : int64; const aEncryptKey : string);


implementation

uses
   BlowFish;

function EncryptBlock(sIn, sOut: TStream; const aEncryptKey: string): int64;
var
  EncrytpStream : TBlowFishEncryptStream;
begin
  // crypt
  sIn.Position := 0;
  EncrytpStream := TBlowFishEncryptStream.Create(aEncryptKey, sOut);
  try
    result := EncrytpStream.CopyFrom(sIn, sIn.Size);
  finally
    EncrytpStream.Free;
  end;
end;


procedure DecryptBlock(sIn, sOut: TStream; BlkSz: int64;
  const aEncryptKey: string);
var
  DecrytpStream: TBlowFishDecryptStream;
begin
  DecrytpStream := TBlowFishDecryptStream.Create(aEncryptKey, sIn);
  try
    sOut.CopyFrom(DecrytpStream, BlkSz);
  finally
    DecrytpStream.Free;
  end;
end;

end.

