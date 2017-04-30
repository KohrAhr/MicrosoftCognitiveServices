unit uFaceApi.Base;

interface

uses
  { TFaceApiServer }
  uFaceApi.Servers.Types,
  { TInterfacedPersistent }
  System.Classes,
  { TNetHeader }
  System.Net.URLClient,
  { TAccess }
  uFaceApi.ServersAccess.Types;

type
  TFaceApiBase = class(TInterfacedObject)
  private
    const
      CONST_FACEAPI_ACCESS_KEY_NAME = 'Ocp-Apim-Subscription-Key';

    var
      FAccess: TAccess;
  protected
    function ServerBaseUrl: String;
  public
    property Access: TAccess write FAccess;

    function GetAccessKey: TNetHeader;

    procedure SetAccessKey(const AAccess: TAccess);
  end;

implementation

uses
  { Format }
  System.SysUtils;

procedure TFaceApiBase.SetAccessKey(const AAccess: TAccess);
begin
  Access := AAccess;
end;

function TFaceApiBase.ServerBaseUrl: String;
begin
  Result := Format('https://%s/face/v1.0', [CONST_FACE_API_SERVER_URLS[FAccess.AccessServer]]);
end;

function TFaceApiBase.GetAccessKey: TNetHeader;
begin
  Result.Name := CONST_FACEAPI_ACCESS_KEY_NAME;
  Result.Value := FAccess.AccessKey;
end;

end.
