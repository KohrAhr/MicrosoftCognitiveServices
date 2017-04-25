unit uFaceApi;

interface

uses
  System.Classes,
  { TFaceApiServer }
  uFaceApi.Servers.Types;

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
  public
    function DetectURL(AURL: String; ADetectOptions: TDetectOptions): String; overload;
    function DetectFile(AFileName: String; ADetectOptions: TDetectOptions): String; overload;

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


function TFaceApi.DetectFile(AFileName: String; ADetectOptions: TDetectOptions): String;
var
  LNameValuePair: TNameValuePair;
  LHTTPClient: THTTPClient;
	LStream: TStream;
  LURL: String;
  LHeaders: TNetHeaders;
	LResponse: TMemoryStream;
begin
  if not FileExists(AFileName) then
    Exit;

  LHTTPClient := THTTPClient.Create;
  try
    SetLength(LHeaders, 1);

    LNameValuePair.Name := 'Ocp-Apim-Subscription-Key';
    LNameValuePair.Value := AccessKey;
    LHeaders[0] := LNameValuePair;

    LHTTPClient.ContentType := 'application/octet-stream';
    LURL := Format(
      'https://%s/face/v1.0/detect?returnFaceId=%s&returnFaceLandmarks=%s&returnFaceAttributes=%s',
      [
        CONST_FACE_API_SERVER_URL[AccessServer],
        BoolToStr(ADetectOptions.ReturnFaceId, True).ToLower,
        BoolToStr(ADetectOptions.ReturnFaceLandmarks, True).ToLower,
        ADetectOptions.ReturnFaceAttributes.ToLower
      ]
    );
    LStream := LHTTPClient.Post(LURL, AFileName, nil, LHeaders).ContentStream;

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

function TFaceApi.DetectURL(AURL: String; ADetectOptions: TDetectOptions): String;
var
  LNameValuePair: TNameValuePair;
  LHTTPClient: THTTPClient;
	LStream: TStream;
  LURL: String;
  LHeaders: TNetHeaders;
	LResponse: TMemoryStream;
  LRequestContent: TStringStream;
begin
  LRequestContent := nil;
  LHTTPClient := THTTPClient.Create;
  try
    SetLength(LHeaders, 1);

    LNameValuePair.Name := 'Ocp-Apim-Subscription-Key';
    LNameValuePair.Value := AccessKey;
    LHeaders[0] := LNameValuePair;

    LHTTPClient.ContentType := 'application/json';
    LURL := Format(
      'https://%s/face/v1.0/detect?returnFaceId=%s&returnFaceLandmarks=%s&returnFaceAttributes=%s',
      [
        CONST_FACE_API_SERVER_URL[AccessServer],
        BoolToStr(ADetectOptions.ReturnFaceId, True).ToLower,
        BoolToStr(ADetectOptions.ReturnFaceLandmarks, True).ToLower,
        ADetectOptions.ReturnFaceAttributes.ToLower
      ]
    );

    LRequestContent := TStringStream.Create('{ "url":"http://1click.lv/index.jpg" }');

    LStream := LHTTPClient.Post(LURL, LRequestContent, nil, LHeaders).ContentStream;

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
