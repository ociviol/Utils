unit Utils.Base64;

{$mode delphi}

interface

uses
  Classes, SysUtils, Base64;

function EncodeStringBase64(const s: string): string;
function DecodeStringBase64(const s: string): string;

implementation

function DecodeStringBase64(const s: string): string;
var
  instream, outstream: TStringStream;
  decoder: TBase64DecodingStream;
begin
  instream := TStringStream.Create(s);
  try
    outstream := TStringStream.Create('');
    try
      decoder := TBase64DecodingStream.Create(instream, bdmmime);
      try
        outstream.copyfrom(decoder, decoder.size);
        outstream.position := 0;
        Result := outstream.readstring(outstream.size);
      finally
        decoder.Free;
      end;
    finally
      outstream.Free;
    end;
  finally
    instream.Free;
  end;
end;


function EncodeStringBase64(const s: string): string;
var
  outstream: TStringStream;
  encoder: TBase64EncodingStream;
begin
  outstream := TStringStream.Create('');
  try
    encoder := TBase64EncodingStream.Create(outstream);
    try
      encoder.Write(s[1], length(s));
    finally
      encoder.Free;
    end;
    outstream.position := 0;
    Result := outstream.readstring(outstream.size);
  finally
    outstream.Free;
  end;
end;

end.
