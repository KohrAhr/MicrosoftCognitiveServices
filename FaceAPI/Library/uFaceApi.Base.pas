unit uFaceApi.Base;

interface

uses
  { TFaceApiServer }
  uFaceApi.Servers.Types,
  { TInterfacedPersistent }
  System.Classes,
  { THTTPClient }
  System.Net.HttpClient,
  { TNetHeaders }
  System.Net.URLClient;

type
  TFaceApiBase = class(TInterfacedPersistent)
  private
    const
      CONST_FACEAPI_ACCESS_KEY_NAME = 'Ocp-Apim-Subscription-Key';

    var
      FAccessKey: String;
      FAccessServer: TFaceApiServer;

  protected
    function PrepareHttpClient(var AHeaders: TNetHeaders; const AContentType: String): THTTPClient;
    function ProceedHttpClientData(AClient: THTTPClient; AData: TStream): String;

    function ServerBaseUrl(AServer: TFaceApiServer): String;
  public
    property AccessKey: String read FAccessKey write FAccessKey;
    property AccessServer: TFaceApiServer read FAccessServer write FAccessServer;
  end;

implementation

uses
  { StringHelper }
  uFunctions.StringHelper,
  { Format }
  System.SysUtils,
  System.NetConsts;

function TFaceApiBase.PrepareHttpClient(var AHeaders: TNetHeaders; const AContentType: String): THTTPClient;
var
  LNameValuePair: TNameValuePair;
begin
  Result := THTTPClient.Create;
  try
    SetLength(AHeaders, 1);

    LNameValuePair.Name := CONST_FACEAPI_ACCESS_KEY_NAME;
    LNameValuePair.Value := AccessKey;
    AHeaders[0] := LNameValuePair;

    Result.ContentType := AContentType;
  except
    Result.Free;
    raise;
  end;
end;

function TFaceApiBase.ProceedHttpClientData(AClient: THTTPClient; AData: TStream): String;
var
	LResponse: TMemoryStream;
begin
  LResponse := TMemoryStream.Create;
  try
    AData.Position := 0;
    LResponse.CopyFrom(AData, AData.Size);

    Result := StringHelper.MemoryStreamToString(LResponse);
  finally
    LResponse.Free;
  end;

  AClient.Free;
end;

function TFaceApiBase.ServerBaseUrl(AServer: TFaceApiServer): String;
begin
  Result := Format('https://%s/face/v1.0', [CONST_FACE_API_SERVER_URL[AServer]]);
end;

end.
