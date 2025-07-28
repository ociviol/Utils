unit utils.crcstream;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

 type
  { TCrcStream }

  TCRCStream = class(TStream)
  private
    FCrc : dword;
    FDest,
    FSource : TStream;
    FCRCTable : array[0..255] of dword;

    function GetCrc: dword;
    procedure UpdateCrc32(const Buffer; aCount : integer); inline;
  public
    constructor Create(aSource, aDest : TStream);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    property Crc : dword read GetCrc;
  end;

implementation

{ TCrcStream }

const
  POLY = $EDB88320;

constructor TCRCStream.Create(aSource, aDest: TStream);
  procedure InitCRCTable;
  var
    c : dword;
    i, j: byte;
  begin
    for i := 0 to 255 do
    begin
      c := i;
      for j := 0 to 7 do
      begin
        if (c and 1) <> 0 then
          c := (c shr 1) xor POLY
        else
          c := c shr 1;
      end;
      FCRCTable[i] := c;
    end;
  end;
begin
  inherited Create;
  FCrc := $FFFFFFFF;
  FSource := aSource;
  FDest := aDest;
  InitCRCTable;
end;

function TCRCStream.GetCrc: dword;
begin
  result := FCRC xor $FFFFFFFF;
end;

procedure TCRCStream.UpdateCrc32(const Buffer; aCount: integer);
var
  i: Integer;
begin
  for i := 0 to aCount - 1 do
    FCRC := (FCRC shr 8) xor FCRCTable[(FCRC and $FF) xor Byte(PByte(@Buffer)[i])];
end;

function TCRCStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Assigned(FSource) then
  begin
    Result:=FSource.Read(Buffer, Count);
    UpdateCrc32(Buffer, Count);
  end;
end;

function TCRCStream.Write(const Buffer; Count: Longint): Longint;
begin
  if Assigned(FDest) then
    Result:=FDest.Write(Buffer, Count)
  else
    result := Count;

  UpdateCrc32(Buffer, Count);
end;

function TCRCStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  FSource.Seek(Offset, Origin);
end;

end.

