unit utils.udp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$if defined(Darwin) or defined(Linux)}
  cthreads,
  {$endif}
  UE_Server, UE_Client, blcksock;

type

  { TUDP }

  TUDP = Class
  private
    FConnected : Boolean;
    FUEServer : TUEServer;
    FUEClient : TUEClient;
    FUEOnReceive : TUEOnReceive;
  public
    constructor Create(aUEOnReceive : TUEOnReceive = nil);
    destructor Destroy; override;
    procedure StartServer;
    procedure StopServer;
    function ConnectToServer(const aIP : String):boolean;
    function Send(const aStr : String):String;
    procedure Disconnect;
    property Connected : Boolean read FConnected;
  end;

implementation

{ TUDP }

constructor TUDP.Create(aUEOnReceive: TUEOnReceive);
begin
  FConnected := False;
  FUEServer := TUEServer.Create(aUEOnReceive);
  FUEClient := TUEClient.Create;
  inherited Create;
end;

destructor TUDP.Destroy;
begin
  if Assigned(FUEServer) then
    FreeAndNil(FUEServer);
  if Assigned(FUEClient) then
    FreeAndNil(FUEClient);
  inherited Destroy;
end;

procedure TUDP.StartServer;
begin
  FUEServer.Start;
end;

procedure TUDP.StopServer;
begin
  FUEServer.Stop;
end;

function TUDP.ConnectToServer(const aIP: String): boolean;
begin
  FConnected := FUEClient.Connect(aIP);
end;

function TUDP.Send(const aStr: String): String;
begin
  result := FUEClient.Send(aStr);
end;

procedure TUDP.Disconnect;
begin
  if FUEClient.Connected then
    FUEClient.Disconnect;
end;

end.

