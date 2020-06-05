unit utils.loggerServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$if defined(Darwin) or defined(Linux)}
  cthreads,
  {$endif}
  blcksock;

type
  TTCPOnReceive = procedure(const Data : String; Socket : TUDPBlockSocket) of object;

  { TTCPServer }

  TTCPServer = Class
  private
  public
    constructor Create(aOnReceive : TTCPOnReceive = nil);
    destructor Destroy; override;
  end;

  { TTCPClient }

  TTCPClient = Class
  private
  public
    constructor Create(aOnReceive : TTCPOnReceive = nil);
    destructor Destroy; override;
  end;

implementation

{ TTCPClient }

constructor TTCPClient.Create(aOnReceive: TTCPOnReceive);
begin

end;

destructor TTCPClient.Destroy;
begin
  inherited Destroy;
end;

{ TTCPServer }

constructor TTCPServer.Create(aUEOnReceive: TUEOnReceive);
begin
  inherited Create;
end;

destructor TTCPServer.Destroy;
begin
  inherited Destroy;
end;


end.

