unit uFaceApi;

interface

uses
  { TStream }
  System.Classes,
  { TFaceApiServer }
  uFaceApi.Servers.Types,
  { TContentType }
  uFaceApi.Content.Types,
  { TFaceApiBase }
  uFaceApi.Base,
  { TFaceAttributes }
  uFaceApi.FaceAttributes,
  { IFaceApi }
  uIFaceApi,
  { TDetectOptions }
  uFaceApi.FaceDetectOptions;

type
  TFaceApi = class(TFaceApiBase, IFaceApi)
    function Detect(ARequestType: TContentType; AData: String; AStreamData: TBytesStream; ADetectOptions: TDetectOptions): String;
  public
    function DetectURL(AURL: String; ADetectOptions: TDetectOptions): String;
    function DetectFile(AFileName: String; ADetectOptions: TDetectOptions): String;
    function DetectStream(AStream: TBytesStream; ADetectOptions: TDetectOptions): String;

    function ListPersonGroups(AStart: String = ''; ATop: Integer = 1000): String;

    function ListPersonsInPersonGroup(APersonGroup: String): String;

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

const
  CONST_FACEAPI_ACCESS_KEY_NAME = 'Ocp-Apim-Subscription-Key';

constructor TFaceApi.Create(const AAccessKey: String; const AAccessServer: TFaceApiServer = fasGeneral);
begin
  inherited Create;

  AccessKey := AAccessKey;

  AccessServer := AAccessServer;
end;

function TFaceApi.Detect(ARequestType: TContentType; AData: String; AStreamData: TBytesStream; ADetectOptions: TDetectOptions): String;
var
  LNameValuePair: TNameValuePair;
  LHTTPClient: THTTPClient;
	LStream: TStream;
  LURL: String;
  LHeaders: TNetHeaders;
	LResponse: TMemoryStream;
  LRequestContent: TBytesStream;
begin
  if ARequestType = rtFile then
    if not FileExists(AData) then
      Exit;

  LRequestContent := nil;
  LHTTPClient := THTTPClient.Create;
  try
    SetLength(LHeaders, 1);

    LNameValuePair.Name := CONST_FACEAPI_ACCESS_KEY_NAME;
    LNameValuePair.Value := AccessKey;
    LHeaders[0] := LNameValuePair;

    LURL := Format(
      'https://%s/face/v1.0/detect?returnFaceId=%s&returnFaceLandmarks=%s&returnFaceAttributes=%s',
      [
        CONST_FACE_API_SERVER_URL[AccessServer],
        BoolToStr(ADetectOptions.FaceId, True).ToLower,
        BoolToStr(ADetectOptions.FaceLandmarks, True).ToLower,
        ADetectOptions.FaceAttributesToString
      ]
    );

    LHTTPClient.ContentType := CONST_CONTENT_TYPE[ARequestType];

    if ARequestType = rtFile then
      LStream := LHTTPClient.Post(LURL, AData, nil, LHeaders).ContentStream
    else
      begin
        if ARequestType = rtStream then
          LRequestContent := AStreamData
        else
          LRequestContent := TBytesStream.Create(TEncoding.UTF8.GetBytes(Format('{ "url":"%s" }', [AData])));

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
  Result := Detect(rtFile, AFileName, nil, ADetectOptions);
end;

function TFaceApi.DetectStream(AStream: TBytesStream; ADetectOptions: TDetectOptions): String;
begin
  Result := Detect(rtStream, '', AStream, ADetectOptions);
end;

function TFaceApi.DetectURL(AURL: String; ADetectOptions: TDetectOptions): String;
begin
  Result := Detect(rtUrl, AURL, nil, ADetectOptions);
end;

function TFaceApi.ListPersonGroups(AStart: String; ATop: Integer): String;
var
  LNameValuePair: TNameValuePair;
  LHTTPClient: THTTPClient;
	LStream: TStream;
  LURL: String;
  LHeaders: TNetHeaders;
	LResponse: TMemoryStream;
begin
  LHTTPClient := THTTPClient.Create;
  try
    SetLength(LHeaders, 1);

    LNameValuePair.Name := CONST_FACEAPI_ACCESS_KEY_NAME;
    LNameValuePair.Value := AccessKey;
    LHeaders[0] := LNameValuePair;

    LURL := Format(
      'https://%s/face/v1.0/persongroups?start=%s&top=%s',
      [
        CONST_FACE_API_SERVER_URL[AccessServer],
        AStart,
        ATop.ToString
      ]
    );

    LHTTPClient.ContentType := CONST_CONTENT_TYPE[rtUrl];

    LStream := LHTTPClient.Get(LURL, nil, LHeaders).ContentStream;

    LResponse := TMemoryStream.Create;
    try
      LResponse.CopyFrom(LStream, LStream.Size);

      Result := StringHelper.MemoryStreamToString(LResponse);
    finally
      LResponse.Free;
    end;
  finally
    LHTTPClient.Free;
  end;
end;

function TFaceApi.ListPersonsInPersonGroup(APersonGroup: String): String;
var
  LNameValuePair: TNameValuePair;
  LHTTPClient: THTTPClient;
	LStream: TStream;
  LURL: String;
  LHeaders: TNetHeaders;
	LResponse: TMemoryStream;
begin
  LHTTPClient := THTTPClient.Create;
  try
    SetLength(LHeaders, 1);

    LNameValuePair.Name := CONST_FACEAPI_ACCESS_KEY_NAME;
    LNameValuePair.Value := AccessKey;
    LHeaders[0] := LNameValuePair;

    LURL := Format(
      'https://%s/face/v1.0/persongroups/%s/persons',
      [
        CONST_FACE_API_SERVER_URL[AccessServer],
        APersonGroup
      ]
    );

    LHTTPClient.ContentType := CONST_CONTENT_TYPE[rtUrl];

    LStream := LHTTPClient.Get(LURL, nil, LHeaders).ContentStream;

    LResponse := TMemoryStream.Create;
    try
      LResponse.CopyFrom(LStream, LStream.Size);

      Result := StringHelper.MemoryStreamToString(LResponse);
    finally
      LResponse.Free;
    end;
  finally
    LHTTPClient.Free;
  end;
end;

end.
