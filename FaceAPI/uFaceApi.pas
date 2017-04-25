unit uFaceApi;

interface

uses
  { TStream }
  System.Classes,
  { TFaceApiServer }
  uFaceApi.Servers.Types,
  { TContentType }
  uFaceApi.Content.Types;

type
  TFaceApiBase = class
  private
    FAccessKey: String;
    FAccessServer: TFaceApiServer;
  public
    property AccessKey: String read FAccessKey write FAccessKey;
    property AccessServer: TFaceApiServer read FAccessServer write FAccessServer;
  end;

  TDetectOptions = record
    ReturnFaceId: Boolean;
    ReturnFaceLandmarks: Boolean;
    ReturnFaceAttributes: String;

    constructor Create(AReturnFaceId: Boolean; AReturnFaceLandmarks: Boolean = False; AReturnFaceAttributes: String = '');
  end;

  function Detect(AReturnFaceId: Boolean; AReturnFaceLandmarks: Boolean = False; AReturnFaceAttributes: String = ''): TDetectOptions;

type
  TFaceApi = class(TFaceApiBase)
    function Detect(ARequestType: TContentType; AData: String; ADetectOptions: TDetectOptions): String;
  public
    function DetectURL(AURL: String; ADetectOptions: TDetectOptions): String;
    function DetectFile(AFileName: String; ADetectOptions: TDetectOptions): String;

    constructor Create(const AAccessKey: String; const AAccessServer: TFaceApiServer = fasGeneral);
  end;

implementation

uses
  { THTTPClient }
  System.Net.HttpClient,
  { TNetHeaders }
  System.Net.URLClient,
  { ContentType }
  System.NetConsts,
  { Format }
  System.SysUtils,
  { StringHelper }
  uFunctions.StringHelper;

{ TFaceApi }

constructor TFaceApi.Create(const AAccessKey: String; const AAccessServer: TFaceApiServer = fasGeneral);
begin
  inherited Create;

  AccessKey := AAccessKey;

  FAccessServer := AAccessServer;
end;

function TFaceApi.Detect(ARequestType: TContentType; AData: String; ADetectOptions: TDetectOptions): String;
var
  LNameValuePair: TNameValuePair;
  LHTTPClient: THTTPClient;
	LStream: TStream;
  LURL: String;
  LHeaders: TNetHeaders;
	LResponse: TMemoryStream;
  LRequestContent: TStringStream;
begin
  if ARequestType = rtFile then
    if not FileExists(AData) then
      Exit;

  LRequestContent := nil;
  LHTTPClient := THTTPClient.Create;
  try
    SetLength(LHeaders, 1);

    LNameValuePair.Name := 'Ocp-Apim-Subscription-Key';
    LNameValuePair.Value := AccessKey;
    LHeaders[0] := LNameValuePair;

    LURL := Format(
      'https://%s/face/v1.0/detect?returnFaceId=%s&returnFaceLandmarks=%s&returnFaceAttributes=%s',
      [
        CONST_FACE_API_SERVER_URL[AccessServer],
        BoolToStr(ADetectOptions.ReturnFaceId, True).ToLower,
        BoolToStr(ADetectOptions.ReturnFaceLandmarks, True).ToLower,
        ADetectOptions.ReturnFaceAttributes.ToLower
      ]
    );

    LHTTPClient.ContentType := CONST_CONTENT_TYPE[ARequestType];

    if ARequestType = rtFile then
      LStream := LHTTPClient.Post(LURL, AData, nil, LHeaders).ContentStream
    else
      begin
        LRequestContent := TStringStream.Create(Format('{ "url":"%s" }', [AData]));
        LStream := LHTTPClient.Post(LURL, LRequestContent, nil, LHeaders).ContentStream;
      end;

    LResponse := TMemoryStream.Create;
    try
      LResponse.CopyFrom(LStream, LStream.Size);

      Result := StringHelper.MemoryStreamToString(LResponse);
    finally
      LResponse.Free;
    end;
  finally
    LRequestContent.Free;
    LHTTPClient.Free;
  end;
end;

function TFaceApi.DetectFile(AFileName: String; ADetectOptions: TDetectOptions): String;
begin
  Result := Detect(rtFile, AFileName, ADetectOptions);
end;

function TFaceApi.DetectURL(AURL: String; ADetectOptions: TDetectOptions): String;
begin
  Result := Detect(rtUrl, AURL, ADetectOptions);
end;

{ TDetect }

constructor TDetectOptions.Create(AReturnFaceId: Boolean; AReturnFaceLandmarks: Boolean = False; AReturnFaceAttributes: String = '');
begin
  ReturnFaceId := AReturnFaceId;
  ReturnFaceLandmarks := AReturnFaceLandmarks;
  ReturnFaceAttributes := AReturnFaceAttributes;
end;

function Detect(AReturnFaceId: Boolean; AReturnFaceLandmarks: Boolean = False; AReturnFaceAttributes: String = ''): TDetectOptions;
begin
  Result := TDetectOptions.Create(AReturnFaceId, AReturnFaceLandmarks, AReturnFaceAttributes);
end;

end.
