unit uFaceApi.Base;

interface

uses
  { TFaceApiServer }
  uFaceApi.Servers.Types,
  { TInterfacedPersistent }
  System.Classes,
  { TNetHeader }
  System.Net.URLClient;

type
  TFaceApiBase = class(TInterfacedObject)
  private
    const
      CONST_FACEAPI_ACCESS_KEY_NAME = 'Ocp-Apim-Subscription-Key';

    var
      FAccessKey: String;
      FAccessServer: TFaceApiServer;
  protected
    function ServerBaseUrl(AServer: TFaceApiServer): String;
  public
    property AccessKey: String read FAccessKey write FAccessKey;
    property AccessServer: TFaceApiServer read FAccessServer write FAccessServer;

    function GetAccessKey: TNetHeader;
  end;

implementation

uses
  { Format }
  System.SysUtils;

function TFaceApiBase.ServerBaseUrl(AServer: TFaceApiServer): String;
begin
  Result := Format('https://%s/face/v1.0', [CONST_FACE_API_SERVER_URLS[AServer]]);
end;

function TFaceApiBase.GetAccessKey: TNetHeader;
begin
  Result.Name := CONST_FACEAPI_ACCESS_KEY_NAME;
  Result.Value := AccessKey;
end;

end.
