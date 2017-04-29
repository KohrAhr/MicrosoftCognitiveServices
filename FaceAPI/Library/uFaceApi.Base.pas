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
  System.Net.URLClient,
  { CONST_CONTENT_TYPE_JSON }
  uFaceApi.Content.Types;

type
  TFaceApiBase = class(TInterfacedObject)
  private
    const
      CONST_FACEAPI_ACCESS_KEY_NAME = 'Ocp-Apim-Subscription-Key';

    var
      FAccessKey: String;
      FAccessServer: TFaceApiServer;

  private
    function PostRequestBase(const AURL, AData: String; ARequestContent: TBytesStream; const AContentType: String): String;
  protected
    function PrepareHttpClient(var AHeaders: TNetHeaders; const AContentType: String): THTTPClient;
    function ProceedHttpClientData(AClient: THTTPClient; AData: TStream): String;

    function GetRequest(const AURL: String): String;
    function PostRequest(const AURL: String; ARequestContent: TBytesStream; const AContentType: String): String; overload;
    function PostRequest(const AURL: String; const AData: string; const AContentType: String): String; overload;

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

function TFaceApiBase.GetRequest(const AURL: String): String;
var
  LHTTPClient: THTTPClient;
  LStream: TStream;
  LHeaders: TNetHeaders;
begin
  LHTTPClient := PrepareHTTPClient(LHeaders, CONST_CONTENT_TYPE_JSON);
  try
    LStream := LHTTPClient.Get(AURL, nil, LHeaders).ContentStream;

    Result := ProceedHttpClientData(LHTTPClient, LStream);
  finally
    LHTTPClient.Free;
  end;
end;

function TFaceApiBase.PostRequestBase(const AURL, AData: String; ARequestContent: TBytesStream; const AContentType: String): String;
var
  LHTTPClient: THTTPClient;
	LStream: TStream;
  LHeaders: TNetHeaders;
begin
  LHTTPClient := PrepareHTTPClient(LHeaders, AContentType);
  try
    if AData <> '' then
      LStream := LHTTPClient.Post(AURL, AData, nil, LHeaders).ContentStream
    else
      LStream := LHTTPClient.Post(AURL, ARequestContent, nil, LHeaders).ContentStream;

    Result := ProceedHttpClientData(LHTTPClient, LStream);
  finally
    LHTTPClient.Free;
  end;
end;

function TFaceApiBase.PostRequest(const AURL: String; ARequestContent: TBytesStream; const AContentType: String): String;
begin
  Result := PostRequestBase(AURL, '', ARequestContent, AContentType);
end;

function TFaceApiBase.PostRequest(const AURL, AData, AContentType: String): String;
begin
  Result := PostRequestBase(AURL, AData, nil, AContentType);
end;

function TFaceApiBase.PrepareHttpClient(var AHeaders: TNetHeaders; const AContentType: String): THTTPClient;
var
  LNameValuePair: TNameValuePair;
begin
  Result := THTTPClient.Create;
  try
    if AccessKey = '' then
      raise Exception.Create('License Key is required');

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
end;

function TFaceApiBase.ServerBaseUrl(AServer: TFaceApiServer): String;
begin
  Result := Format('https://%s/face/v1.0', [CONST_FACE_API_SERVER_URLS[AServer]]);
end;

end.
