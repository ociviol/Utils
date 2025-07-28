unit utils.cryptstream;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils;

function EncryptStream(sIn, sOut : TStream; const aEncryptKey : string):int64;
procedure DecryptStream(sIn, sOut : TStream; BlkSz : int64; const aEncryptKey : string);


implementation

uses
   BlowFish;

function EncryptStream(sIn, sOut: TStream; const aEncryptKey: string): int64;
var
  EncrytpStream : TBlowFishEncryptStream;
begin
  sIn.Position := 0;
  EncrytpStream := TBlowFishEncryptStream.Create(aEncryptKey, sOut);
  try
    result := EncrytpStream.CopyFrom(sIn, sIn.Size);
  finally
    EncrytpStream.Free;
  end;
end;


procedure DecryptStream(sIn, sOut: TStream; BlkSz: int64;
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

