unit utils.compressstream;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils;

function CompressBlock(sIn, sOut: TStream): int64;
function DecompressBlock(sIn, sOut: TStream; DataSize: int64): int64;

implementation

uses
  zstream;

function CompressBlock(sIn, sOut: TStream): int64;
var
  cs : Tcompressionstream;
begin
  cs := Tcompressionstream.create(clMax, sOut, True);
  try
    result := cs.CopyFrom(sIn, sIn.Size);
  finally
    cs.free;
  end;
end;

function DecompressBlock(sIn, sOut: TStream; DataSize: int64): int64;
var
  ds : TDecompressionstream;
begin
  ds := TDecompressionstream.create(sIn, True);
  try
    result := sOut.CopyFrom(ds, DataSize) ;
  finally
    ds.Free;
  end;
end;

end.

