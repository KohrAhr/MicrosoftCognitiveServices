unit uFaceApi.ServersAccess.Types;

interface

uses
  { TFaceApiServer }
  uFaceApi.Servers.Types;

type
  TAccessServer = record
    AccessKey: String;
    AccessServer: TFaceApiServer;
  end;

  function AccessServer(AAccessKey: String; AAccessServer: TFaceApiServer): TAccessServer;

implementation

function AccessServer(AAccessKey: String; AAccessServer: TFaceApiServer): TAccessServer;
begin
  Result.AccessKey := AAccessKey;
  Result.AccessServer := AAccessServer;
end;

end.
