unit uFaceApi.ServersAccess.Types;

interface

uses
  { TFaceApiServer }
  uFaceApi.Servers.Types;

type
  TAccess = record
    AccessKey: String;
    AccessServer: TFaceApiServer;
  end;

  function Access(AAccessKey: String; AAccessServer: TFaceApiServer): TAccess;

implementation

function Access(AAccessKey: String; AAccessServer: TFaceApiServer): TAccess;
begin
  Result.AccessKey := AAccessKey;
  Result.AccessServer := AAccessServer;
end;

end.
